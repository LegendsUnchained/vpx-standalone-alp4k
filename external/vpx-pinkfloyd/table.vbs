Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="esha_l4c",UseSolenoids=2,UseLamps=0,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff", SCoin="coin"

Const BallSize = 50
Const Ballmass = 1.7

LoadVPM "01120100", "S11.VBS", 3.22

NoUpperLeftFlipper
NoUpperRightFlipper
Dim DesktopMode: DesktopMode = Table1.ShowDT

If DesktopMode = True Then 'Show Desktop components
Ramp16.visible=1
Ramp15.visible=1
Primitive13.visible=1
Else
Ramp16.visible=0
Ramp15.visible=0
Primitive13.visible=0
End if


Dim SideWalls,Speakers,Spinner,Scream,SpeakerLights,Lightning,flames
'************************************************************************
'OPTIONS ANIMATIONS
'*************************************************************************

SideWalls = 1   'sidewall and backwall animation 1=on 0=off
Speakers = 1   'amp and speaker PINK FLOYD animation 1=on 0=off
Spinner = 1   'spinner animation 1=on 0=off
Scream = 1   'scream animation 1=on 0=off
SpeakerLights = 1   'speaker lights colour change animation 1=on 0=off
Lightning = 1  'sling shot lightning animation 1=on 0=off
flames = 1    'flame animation 1=on 0 =off

'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************
SolCallback(01) = "bsTrough.SolIn"
SolCallback(02) = "bsTrough.SolOut"
SolCallback(03) = "SolDropReset"
SolCallback(04) = "SolFault"
SolCallback(05) = "bsLSaucer.SolOut"
SolCallback(06) = "BotPop"
SolCallback(07) = "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
SolCallback(09) = "InstituteDrop"
SolCallback(10) = "PFGI2"
'SolCallBack(11) = ""
SolCallback(13) = "VukTopPop"
SolCallBack(14) = "Setlamp 114,"
SolCallBack(15) = "PFGI"
SolCallBack(16) = "Setlamp 116,"
SolCallback(22) = "ShakerMotor"
SolCallBack(25) = "Setlamp 125,"
SolCallBack(26) = "Setlamp 126,"
SolCallBack(27) = "Setlamp 127,"
SolCallBack(28) = "Setlamp 128,"
SolCallBack(29) = "Setlamp 129,"
SolCallBack(30) = "Setlamp 130,"
SolCallBack(31) = "Setlamp 131,"
SolCallBack(32) = "Setlamp 132,"

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySoundAt SoundFX("fx_FlipL",DOFContactors), LeftFlipper:LeftFlipper.RotateToEnd:LeftFlipper1.RotateToEnd
     Else
         PlaySoundAt SoundFX("Flipperdown",DOFContactors), LeftFlipper:LeftFlipper.RotateToStart:LeftFlipper1.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySoundAt SoundFX("fx_FlipR",DOFContactors), RightFlipper:RightFlipper.RotateToEnd
     Else
         PlaySoundAt SoundFX("Flipperdown",DOFContactors), RightFlipper:RightFlipper.RotateToStart
     End If
End Sub


'**********************************************************************************************************

'Solenoid Controlled toys
'**********************************************************************************************************

Sub SolDropReset(Enabled)
	dtL.SolDropUp Enabled
	UpdateTargetShadows
end sub

Sub ShakerMotor(enabled)
	If enabled Then 
	    ShakeTimer.Enabled = 1
		playsound SoundFX("Motor",DOFContactors)
	Else
    	ShakeTimer.Enabled = 0
	End If
End Sub

Sub ShakeTimer_Timer()

	Nudge 0,1
	Nudge 90,1
	Nudge 180,1
	Nudge 270,1
End Sub   

'Playfield GI
Sub PFGI(Enabled)
	If Enabled Then
		dim xx
		For each xx in GI:xx.State = 0: Next
		SetLamp 101, 0
        PlaySound "RelayOff"
	Else
		For each xx in GI:xx.State = 1: Next
		SetLamp 101, 1
        PlaySound "RelayOn"
	End If
End Sub

' Upper Playfield GI
Sub PFGI2(Enabled)
	If Enabled Then
		dim xxxx
		For each xxxx in GIU:xxxx.State = 0: Next
		SetLamp 102, 0
        PlaySound "RelayOff"
	Else
		For each xxxx in GIU:xxxx.State = 1: Next
		SetLamp 102, 1
        PlaySound "RelayOn"
	End If
	UpdateTargetShadows
End Sub


'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************

Dim bsTrough, bsLSaucer, dtL

Sub Table1_Init
	PFGI False: PFGI2 False

	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "EARTHSHAKER - WILLIAMS 1989"&chr(13)&"SHAKE N BAKE"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
        .hidden = 1
        .Games(cGameName).Settings.Value("sound")=0
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With
     On Error Goto 0
 
	PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled=1

	vpmNudge.TiltSwitch  = 9
	vpmNudge.Sensitivity = 2
	vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot)

	Set bsTrough = New cvpmBallStack
		bsTrough.Initsw 10,11,12,13,0,0,0,0
		bsTrough.InitKick BallRelease,150,7
        bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("SolOn",DOFContactors)
		bsTrough.Balls = 3 

	Set bsLSaucer = New cvpmBallStack
		bsLSaucer.InitSaucer sw20, 20, 250, 18
		bsLSaucer.InitExitSnd SoundFX("",DOFContactors), SoundFX("SolOn",DOFContactors)

	Set dtL=New cvpmDropTarget
		dtL.InitDrop Array(sw27,sw28,sw29),Array(27,28,29)
		dtL.InitSnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)

	HRampDX.Collidable=0
 
  ' Create Captive Ball
	CapKicker.CreateSizedBallWithMass BSize,BMass
	CapKicker.Kick 0,5

 End Sub


'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)

	If keycode = PlungerKey Then Plunger.Pullback::PlaySoundAt "plungerpull", Plunger
	If keycode = LeftFlipperKey Then Controller.Switch(58)  = 1
	If keycode = RightFlipperKey Then Controller.Switch(57) = 1
	If KeyDownHandler(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal KeyCode)

	If keycode = PlungerKey Then Plunger.Fire:PlaySoundAt "plunger", Plunger
	If keycode = LeftFlipperKey Then Controller.Switch(58)  = 0
	If keycode = RightFlipperKey Then Controller.Switch(57) = 0
	If keycode = LeftTiltKey Then Nudge 90, 1 : SoundNudgeLeft
	If keycode = RightTiltKey Then Nudge 270, 1 : SoundNudgeRight
	If KeyUpHandler(keycode) Then Exit Sub
End Sub

Sub UpdateTargetShadows
	' If the upper GI is off, shadows should be off.
	if LampState(102) = 0 then
		SetLamp 105, 0
		SetLamp 106, 0
		SetLamp 107, 0
	Else
		SetLamp 105, 1+Controller.Switch(27)
		SetLamp 106, 1+Controller.Switch(28)
		SetLamp 107, 1+Controller.Switch(29)
	end if
end sub 
	
			

'**********************************************************************************************************

 ' Drain hole and kickers
Sub Drain_Hit:bsTrough.addball me 
playSound("Drain_" & Int(Rnd*11)+1)
playsound "bonusmult"
playsound "fx_Crowd3"


End Sub

Sub sw20_Hit:bsLSaucer.addball 0 : PlaySoundAtVol "KickerEnter", sw20, 0.25 :PlaySound"fx_BikeStart": End Sub


'Stand Up Targets
Sub sw19_hit:vpmTimer.pulseSw 19 : PlaySound"targget":End Sub
Sub sw21_hit:vpmTimer.pulseSw 21 : PlaySound"targget":End Sub
Sub sw22_hit:vpmTimer.pulseSw 22 : PlaySound"targget":End Sub
Sub sw23_hit:vpmTimer.pulseSw 23 : Playsound"q_lockislit":End Sub
Sub sw24_hit:vpmTimer.pulseSw 24 : PlaySound"targget":End Sub
Sub sw30_hit:vpmTimer.pulseSw 30 : PlaySound"targget":End Sub

'Drop Targets
 Sub Sw27_Dropped:dtL.Hit 1 :UpdateTargetShadows:End Sub  
 Sub Sw28_Dropped:dtL.Hit 2 :UpdateTargetShadows:End Sub  
 Sub Sw29_Dropped:dtL.Hit 3 :UpdateTargetShadows:End Sub

'Wire Triggers
Sub sw14_Hit:Controller.Switch(14)=1 : PlaySoundAtBall "Sensor" :RandomSoundRollover:PlaySound "fx_BikeStart":WireRampOff: End Sub
Sub sw14_UnHit:Controller.Switch(14)=0:End Sub
Sub sw15_Hit:Controller.Switch(15)=1 : PlaySoundAtBall "Sensor" :RandomSoundRollover:PlaySound"rightin2": End Sub
Sub sw15_UnHit:Controller.Switch(15)=0:End Sub
Sub sw16_Hit:Controller.Switch(16)=1 : PlaySoundAtBall "Sensor" :RandomSoundRollover:PlaySound"outlanes": End Sub
Sub sw16_UnHit:Controller.Switch(16)=0:End Sub
Sub sw17_Hit:Controller.Switch(17)=1 : PlaySoundAtBall "Sensor" :RandomSoundRollover:PlaySound"outlanes": End Sub
Sub sw17_UnHit:Controller.Switch(17)=0:End Sub
Sub sw18_Hit:Controller.Switch(18)=1 : PlaySoundAtBall "Sensor" :RandomSoundRollover:PlaySound "fx_BikeStart":WireRampOff: End Sub
Sub sw18_UnHit:Controller.Switch(18)=0:End Sub
Sub sw33_Hit:controller.switch (33)=1 : PlaySoundAtBall "Sensor" :RandomSoundRollover:WireRampOff: End Sub  
Sub sw33_unHit:controller.switch (33)=0:End Sub
Sub sw34_Hit:controller.switch (34)=1 : PlaySoundAtBall "Sensor" :RandomSoundRollover: End Sub 
Sub sw34_unHit:controller.switch (34)=0:End Sub
Sub sw35_Hit:controller.switch (35)=1 : PlaySoundAtBall "Sensor" :RandomSoundRollover: End Sub 
Sub sw35_unHit:controller.switch (35)=0:End Sub
Sub sw36_Hit:controller.switch (36)=1 : PlaySoundAtBall "Sensor" :RandomSoundRollover:WireRampOff: End Sub 
Sub sw36_unHit:controller.switch (36)=0:End Sub

'Gate Trigger
Sub sw31_hit:vpmTimer.pulseSw 31 : End Sub
Sub sw32_hit:vpmTimer.pulseSw 32 : End Sub
Sub sw43_hit:vpmTimer.pulseSw 43 :WireRampOn True: End Sub
Sub sw44_hit:vpmTimer.pulseSw 44 :WireRampOn True: End Sub

'Subway
Sub sw38_Hit:Controller.Switch(38)=1 : playsound"Subway" : End Sub
Sub sw38_UnHit:Controller.Switch(38)=0:End Sub
Sub sw39_Hit:Controller.Switch(39)=1 : End Sub 
Sub sw39_unHit:Controller.Switch(39)=0:End Sub


'Spinners
Sub sw41_Spin:vpmTimer.PulseSw 41 : playsound"fx_spinner" :PlaySound"spinner" End Sub

'Left Ramp Wire Triggers
Sub sw45_Hit: controller.switch (45)=1 : PlaySoundAtBall "Sensor" :RandomSoundRollover: End Sub 
Sub sw45_unHit: controller.switch (45)=0:End Sub
Sub sw46_Hit: controller.switch (46)=1 : PlaySoundAtBall "Sensor" :RandomSoundRollover: End Sub 
Sub sw46_unHit: controller.switch (46)=0:End Sub

' Shooter lane
Sub sw50_Hit: controller.switch (50)=1 :RandomSoundRollover:End Sub 
Sub sw50_unHit: controller.switch (50)=0:End Sub

'Bumpers
Sub Bumper1_Hit : vpmTimer.PulseSw(52) : PlaySoundAt SoundFX("",DOFContactors), Bumper1:RandomSoundBumper1:End Sub
Sub Bumper2_Hit : vpmTimer.PulseSw(53) : PlaySoundAt SoundFX("",DOFContactors), Bumper2:RandomSoundBumper2:End Sub
Sub Bumper3_Hit : vpmTimer.PulseSw(54) : PlaySoundAt SoundFX("",DOFContactors), Bumper3:RandomSoundBumper3:End Sub

'Generic Sounds
Sub Trigger1_Hit : PlaySound "fx_ballrampdrop": End Sub
Sub Trigger2_Hit : StopSound "Wire Ramp" : PlaySound "fx_ballrampdrop"
End Sub
Sub Trigger3_Hit : StopSound "Wire Ramp" :WireRampOff: End Sub
Sub Trigger4_Hit : PlaySound "Wire Ramp": End Sub
Sub Trigger5_Hit : StopSound "Wire Ramp" : PlaySound "popper_ball":End Sub
Sub Trigger6_Hit : PlaySound "popper_ball":PlaySound "kfaultrel":PlaySound "earthsk3000":PlaySound "matchstart" End Sub

Sub PlungerCradle_Hit
	'Reset ball spin parameters, they don't "dissipate" at rest and they causes inconsistent shooting behavior
	ActiveBall.AngMomX = 0
	ActiveBall.AngMomY = 0
	ActiveBall.AngMomZ = 0
	ActiveBall.AngVelX = 0
	ActiveBall.AngVelY = 0
	ActiveBall.AngVelZ = 0
end sub

 '***********************************
 'Top Raising VUK 
 '***********************************
 'Variables used for VUK 
 Dim raiseballsw, raiseball 

 Sub TopVUK_Hit() 
PlaySound"lockedbacksd"
PlaySound"terremoto"
	WireRampOff
 	TopVUK.Enabled=FALSE
	Controller.switch (37) = True
	PlaySoundAtVol "popper_ball", TopVUK, 0.25
 End Sub
 
 Sub VukTopPop(enabled)
	if(enabled and Controller.switch (37)) then
		PlaySoundAt SoundFX("Popper_ball",DOFContactors), TopVUK
		TopVUK.DestroyBall
 		Set raiseball = TopVUK.CreateBall
 		raiseballsw = True
 		TopVukraiseballtimer.Enabled = True 'Added by Rascal
		TopVUK.Enabled=TRUE	
 		Controller.switch (37) = False
	else
		'PlaySound "Popper"
	end if
End Sub


 Sub TopVukraiseballtimer_Timer()
 	If raiseballsw = True then
 		raiseball.z = raiseball.z + 10
 		If raiseball.z > 190 then
 			'msgbox ("Over")
 			TopVUK.Kick 270, 10 
 			Set raiseball = Nothing
 			TopVukraiseballtimer.Enabled = False
 			raiseballsw = False
			PlaySound "Wire Ramp"
If InstZone = 0 Then 
StopMusic
PlaySound"multiball"
End If
PlaySound"kfaultrel"
 		End If
 	End If
 End Sub
 
 '********************
 ' Bottom raising VUK 
 '********************
 Dim braiseballsw, braiseball 

 Sub BottomVuk_Hit() 
	playsound"popper_ball"
 	BottomVUK.Enabled=FALSE
	Controller.switch (40) = True
 End Sub

 Sub BotPop(enabled)
	if(enabled and Controller.switch (40)) then
		playsound SoundFX("Popper",DOFContactors)
		BottomVUK.DestroyBall
 		Set braiseball = BottomVUK.CreateBall
 		braiseballsw = True
 		BottomVukraiseballtimer.Enabled = True 'Added by Rascal
		BottomVUK.Enabled=TRUE	
 		Controller.switch (40) = False
	else
		'PlaySound "Popper"
	end if
End Sub

 Sub BottomVukraiseballtimer_Timer()
 	If braiseballsw = True then
 		braiseball.z = braiseball.z + 10
 		If braiseball.z > 90 then
 			'msgbox ("Over")
 			BottomVUK.Kick 210, 9
 			Set braiseball = Nothing
 			BottomVukraiseballtimer.Enabled = False
 			braiseballsw = False
			PlaySound "Wire Ramp"
 		End If
 	End If
 End Sub


 '*************************************************
 ' California Nevada DIVERTER
 '*************************************************
dim FaultOpen, FaultChange : FaultOpen = False : FaultChange = False

Sub SolFault(enabled) 
	If enabled Then
		FaultOpen = Not FaultOpen
		If FaultOpen Then 
 			OpenFaultTimer.enabled=1
			HRampSX.Collidable= 0
			HRampDX.Collidable= 1
			FaultChange = 1
		Else
			CloseFaultTimer.enabled=1
 			HRampSX.Collidable= 1
			HRampDX.Collidable= 0 
			FaultChange = 1
		End If
	End If
End Sub

Dim BridgeStep: BridgeStep = 0

Sub OpenFaultTimer_Timer
    Select Case BridgeStep
		Case 0: CalPrim.TransX  =0 : NevadaPrim.TransX  =0   : BridgeStep =1
		Case 1: CalPrim.TransX  =-3: NevadaPrim.TransX  =3   : BridgeStep =2
		Case 2: CalPrim.TransX  =-6: NevadaPrim.TransX  =6   : BridgeStep =3
		Case 3: CalPrim.TransX  =-11: NevadaPrim.TransX  =11 : BridgeStep =4
        Case 4: CalPrim.TransX  =-16: NevadaPrim.TransX  =16 : BridgeStep =5
        Case 5: CalPrim.TransX  =-23: NevadaPrim.TransX  =26 : BridgeStep =0 : controller.switch(42) = 1 : OpenFaultTimer.enabled = 0
    End Select
End Sub

Sub CloseFaultTimer_Timer
    Select Case BridgeStep
		Case 0: CalPrim.TransX  =-23: NevadaPrim.TransX  =26 : BridgeStep =1
        Case 1: CalPrim.TransX  =-17: NevadaPrim.TransX  =17 : BridgeStep =2
		Case 2: CalPrim.TransX  =-11: NevadaPrim.TransX  =11 : BridgeStep =3
		Case 3: CalPrim.TransX  =-6:  NevadaPrim.TransX  =6  : BridgeStep =4
        Case 4: CalPrim.TransX  =-3:  NevadaPrim.TransX  =3  : BridgeStep =5
        Case 5: CalPrim.TransX  =0:   NevadaPrim.TransX  =-0 : BridgeStep =0 : controller.switch(42) = 0 : CloseFaultTimer.enabled = 0
    End Select
End Sub


'**************************************
'
' Earthquake Institute Animation
'
'**************************************

' In the real production Earthshakers, the Institute building is just a
' static toy.  All it does is flash its lights.  But in the pre-production 
' prototypes, the building had a sliding mechanism and a motor that made it 
' sink into the playfield and rise back up on certain game events.  The
' ROM code that controls the building movement is still present in the
' production ROMs, so some Earthshaker owners have modded their tables to
' restore the moving building.  This code implements the building animation
' in our simulated version.
'
' The Institute building is implemented using a bunch of little primitive 
' objects: one for each light window, and one for the outer frame with the 
' back wall and top sign.  This array is a list of all of these primitve 
' components.  To animate the building, we simply move all of the
' primitives in unison.  The animation is extremely simple in that all the
' building does is move straight up and down.  
'
' Windows are at the start of the array to make it easy to find the object
' for a given window number.  Note that array indices start' at 0, but window 
' numbers start at 1 - so InstPrim(0) is window 1, etc.
Dim InstPrim : InstPrim = Array( _
	InstituteWindow1, _
	InstituteWindow2, _
	InstituteWindow3, _
	InstituteWindow4, _
	InstituteWindow5, _
	InstituteWindow6, _
	InstituteWindow7, _
	InstituteWindow8, _
	InstituteWindow9, _
    Primitive034, _
	InstituteBackWall)

' Building travel parameters.  The range of motion is about 3/4 of the building
' height, and it takes about 3 seconds to cover that distance.
dim InstZ, InstDZ, InstDZUp, InstDZDown, InstZMin, InstZMax, InstZRange, InstTravelTime, InstMotorOn, InstZone
InstZMin = -190 : InstZMax = 0   ' travel limits, as Z Translations for the building primitives
InstTravelTime = 3000            ' one-way travel time in milliseconds
InstMotorOn = 1                  ' start with motor turned off

' figure the distance per timer events (total distance divided by total time,
' multiplied by the time interval per event)
InstZRange = InstZMax - InstZMin
InstDZUp = InstZRange / InstTravelTime * InstituteTimer.Interval
InstDZDown = -InstDZUp           ' use the same speed up and down

' start fully up (set Z to max, zone to top (zone 0=bottom, 1=between top and bottom, 2=top))
InstZ = InstZMax
InstZone = 2

' Institute light states.  Each array entry corresponds to the window
' primitive at the same index in InstPrim().
'
' InstLamp(i) is the ROM lamp number for the nth building light
' InstLampBri is the current brightness: 0=off, 1=33%, 2=66%, 3=100%.
' On each timer event, we increase or decrease the brightness of a light
' according to its current on/off state.
Dim InstLamp : InstLamp = Array(23, 24, 25, 20, 21, 22, 17, 18, 19)
Dim InstLampBri : InstLampBri = Array(0, 0, 0, 0, 0, 0, 0, 0, 0)

Sub InstituteTimer_Timer
	' Handle the motor
	if InstMotorOn then
		' move up or down in the current direction
		InstZ = InstZ + InstDZ

		' check for limits
		if InstZ >= InstZMax then 
			' all the the way up - stop here and reverse directions
			InstZ = InstZMax
			InstDZ = InstDZDown

			' trip switches to tell the ROM we're in the UP position
			controller.switch(25) = false
			controller.switch(26) = true
			InstZone = 3
		elseif InstZ <= InstZMin then
			' all the way down - stop here and reverse directions
			InstZ = InstZMin
			InstDZ = InstDZUp

			' trip switches to tell the ROM we're in the DOWN position
			controller.switch(25) = true
			controller.switch(26) = true
			InstZone = 0
		elseif InstZone = 3 and InstZ < InstZMax - InstZRange/3 then
			' we were at the top, and now we're 1/3 of the way down - the
			' original script set both switches off at this position
			controller.switch(25) = false
			controller.switch(26) = false
			InstZone = 2
		elseif InstZone = 2 and InstZ < InstZMin + InstZRange/3 then
			' crossing 2/3 of the way down - the original script set 25 ON
			' and 26 OFF at this position
			controller.switch(25) = true
			controller.switch(26) = false
		end if
		For i = 0 to UBound(InstPrim)
			InstPrim(i).TransZ = InstZ
		next
	end if

    ' Handle the lights
	Dim i, b
	for i = 0 to UBound(InstLamp)
		b = InstLampBri(i)
		if LampState(InstLamp(i)) = 5 then  ' controller turned lamp ON
			if b < 3 then
				b = b + 1
				InstLampBri(i) = b 
				InstPrim(i).Image = "Institute Map Brightness " & b
			end if
		else
			if b > 0 then
				b = b - 1
				InstLampBri(i) = b
				InstPrim(i).Image = "Institute Map Brightness " & b
			end if
		end if
	next
End Sub


' ROM motor control interface 
sub InstituteDrop(enabled)
	InstMotorOn = enabled
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
LampTimer.Interval = 17 'lamp fading speed
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
 	NFadeL 7, l7
 	NFadeL 8, l8
 	NFadeL 9, l9
 	NFadeL 10, l10
 	NFadeL 11, l11
 	NFadeL 12, l12
 	NFadeL 13, l13
 	NFadeL 14, l14
 	NFadeL 15, l15
 	NFadeL 16, l16
	FadeDisableLighting 17, InstituteWindow7, 1.2
	FadeDisableLighting 18, InstituteWindow8, 1.2
	FadeDisableLighting 19, InstituteWindow9, 1.2
	FadeDisableLighting 20, InstituteWindow4, 1.2
	FadeDisableLighting 21, InstituteWindow5, 1.2
	FadeDisableLighting 22, InstituteWindow6, 1.2
	FadeDisableLighting 23, InstituteWindow1, 1.2
	FadeDisableLighting 24, InstituteWindow2, 1.2
	FadeDisableLighting 25, InstituteWindow3, 1.2
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
    NFadeLm 41, l41 'Bumper 1
    NFadeL 41, l41a
    NFadeLm 42, l42 'Bumper 2
    NFadeL 42, l42a
    NFadeLm 43, l43 'Bumper 3
    NFadeL 43, l43a
    NFadeL 44, l44
 	NFadeL 45, l45
 	NFadeL 46, l46
 	NFadeL 47, l47
 	NFadeL 48, l48
    NFadeObjm 49, l49, "bulbcover1_redOn", "bulbcover1_red"       'Ramp Entrence Red    LED
 	NFadeL 49, l49a
 	NFadeL 50, l50
 	NFadeL 51, l51
 	NFadeL 52, l52	
 	NFadeL 53, l53
 	NFadeL 54, l54
 	NFadeL 55, l55
 	NFadeL 56, l56
    NFadeObjm 57, l57, "bulbcover1_redOn", "bulbcover1_red"       'Ramp Entrence Red    LED
    NFadeL 57, l57a

If DesktopMode = True Then 'Show Desktop components
	NFadeL 58, l58 'Backglass
	NFadeL 59, l59 'Backglass
	NFadeL 60, l60 'Backglass
	NFadeL 61, l61 'Backglass
	NFadeL 62, l62 'Backglass
	NFadeL 63, l63 'Backglass
	NFadeL 64, l64 'Backglass
End if

	' GI
	Flash 101, gion_flash1
	Flash 102, gion_flash2

	' Drop target shadows
	Flash 105, drop1
	Flash 106, drop2
	Flash 107, drop3

 'Solenoid Controlled Flashers

    NFadeLm 114, f114
    NFadeL 114, f114a
NFadeLm 131, f131

	FlupperFlash 129, Flasherflash1, Flasherlit1, Flasherbase1, Flasherlight1
	FlupperFlash 128, Flasherflash2, Flasherlit2, Flasherbase2, Flasherlight2
	FlupperFlash 127, Flasherflash3, Flasherlit3, Flasherbase3, Flasherlight3
	FlupperFlash 126, Flasherflash4, Flasherlit4, Flasherbase4, Flasherlight4
	FlupperFlash 132, Flasherflash5, Flasherlit5, Flasherbase5, Flasherlight5
	FlupperFlashm 132, Flasherflash9, Flasherlit9, Flasherbase9, Flasherlight9
	FlupperFlash 125, Flasherflash6, Flasherlit6, Flasherbase6, Flasherlight6
	FlupperFlash 130, Flasherflash7, Flasherlit7, Flasherbase7, Flasherlight7
	FlupperFlash 131, Flasherflash8, Flasherlit8, Flasherbase8, Flasherlight8
	FlupperFlash 116, Flasherflash10, Flasherlit10, Flasherbase10, Flasherlight10

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

'Lights, Ramps & Primitives used as 4 step fading lights
'a,b,c,d are the images used from on to off

Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.image = a:FadingLevel(nr) = 1                  'ON
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
				Object.visible = false
            End if
            Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
			Object.visible = true
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


 
'*****************FLUPPER FLASH****************************


Dim FlashLevel1, FlashLevel2, FlashLevel3, FlashLevel4, FlashLevel5, FlashLevel6, FlasherLevel7, FlasherLevel8, FlasherLevel9, FlasherLevel10
FlasherLight1.IntensityScale = 0
Flasherlight2.IntensityScale = 0
Flasherlight3.IntensityScale = 0
Flasherlight4.IntensityScale = 0
Flasherlight5.IntensityScale = 0
Flasherlight6.IntensityScale = 0
Flasherlight7.IntensityScale = 0
Flasherlight8.IntensityScale = 0
Flasherlight9.IntensityScale = 0
Flasherlight10.IntensityScale = 0

Sub FlupperFlash(nr, FlashObject, LitObject, BaseObject, LightObject)
	FadeEmpty nr
	FlupperFlashm nr, FlashObject, LitObject, BaseObject, LightObject
End Sub

Sub FlupperFlashm(nr, FlashObject, LitObject, BaseObject, LightObject)
	'exit sub
	dim flashx3
	Select Case FadingLevel(nr)
        Case 4, 5
			' This section adapted from Flupper's script
			flashx3 = FlashLevel(nr) * FlashLevel(nr) * FlashLevel(nr)
            FlashObject.IntensityScale = flashx3
			LitObject.BlendDisableLighting = 10 * flashx3
			BaseObject.BlendDisableLighting = (flashx3 * .6) + .4 
			LightObject.IntensityScale = flashx3 
			LitObject.material = "domelit" & Round(9 * FlashLevel(nr))
			LitObject.visible = 1
			FlashObject.visible = 1	
		case 3:
			LitObject.visible = 0
			FlashObject.visible = 0
	end select
End Sub
 
Sub FadeEmpty(nr)	'Fade a lamp number, no object updates
    Select Case FadingLevel(nr)
		Case 3
			FadingLevel(nr) = 0
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
               FadingLevel(nr) = 3 'completely off
            End if
            'Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 6 'completely on
            End if
            'Object.IntensityScale = FlashLevel(nr)
		Case 6
			FadingLevel(nr) = 1
    End Select
End Sub

'**********************************************************************************************************
'Digital Display
'**********************************************************************************************************
 Dim Digits(32)
 Digits(0)=Array(a00, a05, a0c, a0d, a08, a01, a06, a0f, a02, a03, a04, a07, a0b, a0a, a09, a0e)
 Digits(1)=Array(a10, a15, a1c, a1d, a18, a11, a16, a1f, a12, a13, a14, a17, a1b, a1a, a19, a1e)
 Digits(2)=Array(a20, a25, a2c, a2d, a28, a21, a26, a2f, a22, a23, a24, a27, a2b, a2a, a29, a2e)
 Digits(3)=Array(a30, a35, a3c, a3d, a38, a31, a36, a3f, a32, a33, a34, a37, a3b, a3a, a39, a3e)
 Digits(4)=Array(a40, a45, a4c, a4d, a48, a41, a46, a4f, a42, a43, a44, a47, a4b, a4a, a49, a4e)
 Digits(5)=Array(a50, a55, a5c, a5d, a58, a51, a56, a5f, a52, a53, a54, a57, a5b, a5a, a59, a5e)
 Digits(6)=Array(a60, a65, a6c, a6d, a68, a61, a66, a6f, a62, a63, a64, a67, a6b, a6a, a69, a6e)
 Digits(7)=Array(a70, a75, a7c, a7d, a78, a71, a76, a7f, a72, a73, a74, a77, a7b, a7a, a79, a7e)
 Digits(8)=Array(a80, a85, a8c, a8d, a88, a81, a86, a8f, a82, a83, a84, a87, a8b, a8a, a89, a8e)
 Digits(9)=Array(a90, a95, a9c, a9d, a98, a91, a96, a9f, a92, a93, a94, a97, a9b, a9a, a99, a9e)
 Digits(10)=Array(aa0, aa5, aac, aad, aa8, aa1, aa6, aaf, aa2, aa3, aa4, aa7, aab, aaa, aa9, aae)
 Digits(11)=Array(ab0, ab5, abc, abd, ab8, ab1, ab6, abf, ab2, ab3, ab4, ab7, abb, aba, ab9, abe)
 Digits(12)=Array(ac0, ac5, acc, acd, ac8, ac1, ac6, acf, ac2, ac3, ac4, ac7, acb, aca, ac9, ace)
 Digits(13)=Array(ad0, ad5, adc, add, ad8, ad1, ad6, adf, ad2, ad3, ad4, ad7, adb, ada, ad9, ade)
 Digits(14)=Array(ae0, ae5, aec, aed, ae8, ae1, ae6, aef, ae2, ae3, ae4, ae7, aeb, aea, ae9, aee)
 Digits(15)=Array(af0, af5, afc, afd, af8, af1, af6, aff, af2, af3, af4, af7, afb, afa, af9, afe)
 
 Digits(16)=Array(b00, b05, b0c, b0d, b08, b01, b06, b0f, b02, b03, b04, b07, b0b, b0a, b09, b0e)
 Digits(17)=Array(b10, b15, b1c, b1d, b18, b11, b16, b1f, b12, b13, b14, b17, b1b, b1a, b19, b1e)
 Digits(18)=Array(b20, b25, b2c, b2d, b28, b21, b26, b2f, b22, b23, b24, b27, b2b, b2a, b29, b2e)
 Digits(19)=Array(b30, b35, b3c, b3d, b38, b31, b36, b3f, b32, b33, b34, b37, b3b, b3a, b39, b3e)
 Digits(20)=Array(b40, b45, b4c, b4d, b48, b41, b46, b4f, b42, b43, b44, b47, b4b, b4a, b49, b4e)
 Digits(21)=Array(b50, b55, b5c, b5d, b58, b51, b56, b5f, b52, b53, b54, b57, b5b, b5a, b59, b5e)
 Digits(22)=Array(b60, b65, b6c, b6d, b68, b61, b66, b6f, b62, b63, b64, b67, b6b, b6a, b69, b6e)
 Digits(23)=Array(b70, b75, b7c, b7d, b78, b71, b76, b7f, b72, b73, b74, b77, b7b, b7a, b79, b7e)
 Digits(24)=Array(b80, b85, b8c, b8d, b88, b81, b86, b8f, b82, b83, b84, b87, b8b, b8a, b89, b8e)
 Digits(25)=Array(b90, b95, b9c, b9d, b98, b91, b96, b9f, b92, b93, b94, b97, b9b, b9a, b99, b9e)
 Digits(26)=Array(ba0, ba5, bac, bad, ba8, ba1, ba6, baf, ba2, ba3, ba4, ba7, bab, baa, ba9, bae)
 Digits(27)=Array(bb0, bb5, bbc, bbd, bb8, bb1, bb6, bbf, bb2, bb3, bb4, bb7, bbb, bba, bb9, bbe)
 Digits(28)=Array(bc0, bc5, bcc, bcd, bc8, bc1, bc6, bcf, bc2, bc3, bc4, bc7, bcb, bca, bc9, bce)
 Digits(29)=Array(bd0, bd5, bdc, bdd, bd8, bd1, bd6, bdf, bd2, bd3, bd4, bd7, bdb, bda, bd9, bde)
 Digits(30)=Array(be0, be5, bec, bed, be8, be1, be6, bef, be2, be3, be4, be7, beb, bea, be9, bee)
 Digits(31)=Array(bf0, bf5, bfc, bfd, bf8, bf1, bf6, bff, bf2, bf3, bf4, bf7, bfb, bfa, bf9, bfe)
 
 Sub DisplayTimer_Timer
    Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
    ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
    If Not IsEmpty(ChgLED)Then
		If DesktopMode = True Then
       For ii=0 To UBound(chgLED)
          num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
			if (num < 32) then
              For Each obj In Digits(num)
                   If chg And 1 Then obj.State=stat And 1
                   chg=chg\2 : stat=stat\2
                  Next
			Else
			       end if
        Next
	   end if
    End If
 End Sub


'**********************************************************************************************************
'**********************************************************************************************************
'	Start of VPX functions
'**********************************************************************************************************
'**********************************************************************************************************

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 56
    PlaySoundAt SoundFX("",DOFContactors), SLING1
	RandomSoundSlingshotRight
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
If Lightning = 1 then 
lightningsling
PlaySound "fx_LIGHTNING"
End if
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 55
    PlaySoundAt SoundFX("",DOFContactors), SLING2
	RandomSoundSlingshotLeft
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
If Lightning = 1 then 
lightningsling
PlaySound "fx_LIGHTNING"
End if
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
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

'Set position as table object and Vol manually.

Sub PlaySoundAtVol(sound, tableobj, Vol)
		PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set a Looping sound at object.
Sub PlayLoopSoundAtVol(sound, tableobj, Vol)
	PlaySound sound, -1, Vol, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
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
    Vol = Csng(BallVel(ball) ^2 / 900)
End Function

Function RollVol(ball) ' Calculates the Volume of the sound based on the ball speed
    RollVol = Csng(BallVel(ball) ^2 / 2000)
End Function


Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function RndNum(min,max)
 RndNum = Int(Rnd()*(max-min+1))+min     ' Sets a random number between min and max
End Function



'===========================
' Ramp Skill Shot
'===========================

Sub Skill33a_Hit() 
WireRampOff
Dim Velx,Vely
Velx = ActiveBall.Velx : Vely = ActiveBall.Vely
 'msgbox("33"  & Velx & "-" & Vely)
If Velx < 0 Then 
	Skill33a.DestroyBall : PlaySoundAt "BallBounce", Skill35b
playsound "outlanes"
	Skill33b.CreateBall : Skill33b.Kick 180,1
Else
	Skill33a.Kick 90,Velx
WireRampOn True
End If 
End Sub

' Handle Skill Shot Hole 25k
Sub Skill34a_Hit() 
WireRampOff
Dim Velx,Vely
Velx = ActiveBall.Velx : Vely = ActiveBall.Vely
 'msgbox("34" & Velx & "-" & Vely)
If Vely > 0 Then 
	Skill34a.DestroyBall : PlaySoundAt "BallBounce", Skill35b
	Skill34b.CreateBall : Skill34b.Kick 180,1
playsound "outlanes"
Else
	Skill34a.Kick 180,Vely
WireRampOn True
End If 
End Sub

' Handle Skill Shot Hole 100k
Sub Skill35a_Hit() 
WireRampOff
Dim Velx,Vely
Velx = ActiveBall.Velx : Vely = ActiveBall.Vely
 'msgbox("35" & Velx & "-" & Vely)

debug.print "VelX " & velx
' We can't rely on the ball rolling backwards into this kicker, so we need to trip if it's moving slow enough to the left 

If Velx >= -4 Then 
	Skill35a.DestroyBall : PlaySoundAt "BallBounce", Skill35b
	Skill35b.CreateBall : Skill35b.Kick 180,1
playsound "outlanes"
Else
	Skill35a.Kick 90,Velx
WireRampOn True
End If 
End Sub

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
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

    For b = 0 to UBound(BOT)
 
        ' play the rolling sound for each ball
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
			if BOT(b).z < 30 Then ' Ball on playfield
						PlaySound("fx_ballrolling" & b), -1, RollVol(BOT(b) ), audioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
			Else ' Ball on raised ramp
						PlaySound("fx_ballrolling" & b), -1, RollVol(BOT(b) )*.5, audioPan(BOT(b) ), 0, Pitch(BOT(b) )+30000, 1, 0, AudioFade(BOT(b) )
				End If
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' play ball drop sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_ballrampdrop", 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
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
	FlipperLSh1.RotZ = LeftFlipper1.currentangle
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
            BallShadow(b).visible = 0
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub


Sub Pins_Hit (idx)
	PlaySound "fx_PinHit", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
	Rubbers_Hit(idx)
End Sub

Sub Targets_Hit (idx)
	PlaySound "fx_target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Switches_Hit (idx)
	PlaySound "Sensor", 0, 1, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
	RandomSoundRollover
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall)/2, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	RandomSoundMetal
End Sub

Sub Gates_Hit (idx)
	PlaySound "fx_gate", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
End Sub

Sub Posts_Hit(idx)
Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Rubbers_Hit(idx)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
	Rubbers_Hit(idx)
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub RandomSoundHole()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "Hole1", 0, 1, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "Hole2", 0, 1, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "Hole4", 0, 1, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub LeftFlipper1_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub




'************************RAMP STUFF************************
'Set invisible Trigger on surface of ramp. Add these commands for each one.




Sub LRD_Hit()
PlaySoundAt "BallBounce", LRD
End Sub

Sub RRD_Hit()
PlaySoundAt "BallBounce", RRD
End Sub

Sub PRD_Hit: PlaySoundAt "RampDrop", PRD: End Sub
Sub PRD1_Hit: PlaySoundAt "RampDrop", PRD1:WireRampOn True: End Sub

'RAMP BUMPS

Sub RampBumps_Hit (idx)
	RandomSoundRamps
End Sub

Sub RandomSoundRamps()
	Select Case Int(Rnd*7)+1
		Case 1: Playsound "RB1", 0, Vol(ActiveBall)*2, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2: Playsound "RB2", 0, Vol(ActiveBall)*2, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3: Playsound "RB3", 0, Vol(ActiveBall)*2, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 4: Playsound "RB4", 0, Vol(ActiveBall)*2, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 5: Playsound "RB5", 0, Vol(ActiveBall)*2, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 6: Playsound "RB6", 0, Vol(ActiveBall)*2, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 7: Playsound "RB7", 0, Vol(ActiveBall)*2, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub



Dim SPGifCountr 
Sub SPGIF() 
If Spinner = 0 then SPGIFtimer.enabled = 0
If Spinner = 1 then 
SPGIFtimer.interval = 100
SPGIFtimer.enabled = 1
 SPGifCountr = SPGifCountr + 1
    If SPGifCountr > 7 then SPGifCountr = 0 end If
        select case SPGifCountr
            case 0:sw41.image = "sp_0000"
            case 1:sw41.image = "sp_0001"
            case 2:sw41.image = "sp_0002"
            case 3:sw41.image = "sp_0003"
			case 4:sw41.image = "sp_0004"
            case 5:sw41.image = "sp_0005"
            case 6:sw41.image = "sp_0006"
            case 7:sw41.image = "sp_0007"
        end Select
End If
End Sub

Sub SPGIFtimer_Timer ' add GIftimer to table
SPGIFtimer.enabled = 0
SPGIF
End Sub

Dim CLGifCountr 'static rededering on the primative must be set to false
Sub CLGIF() 
If Scream = 0 then CLGIFtimer.enabled = 0
If Scream = 1 then 
CLGIFtimer.interval = 100
CLGIFtimer.enabled = 1
 CLGifCountr = CLGifCountr + 1
    If CLGifCountr > 25 then CLGifCountr = 0 end If
        select case CLGifCountr
            case 0:CalPrim.image = "C1"
            case 1:CalPrim.image = "C2"
            case 2:CalPrim.image = "C3"
            case 3:CalPrim.image = "C4"
			case 4:CalPrim.image = "C5"
            case 5:CalPrim.image = "C6"
            case 6:CalPrim.image = "C7"
			case 7:CalPrim.image = "C8"
            case 8:CalPrim.image = "C9"
            case 9:CalPrim.image = "C10"
            case 10:CalPrim.image = "C9"
			case 11:CalPrim.image = "C8"
            case 12:CalPrim.image = "C7"
			case 13:CalPrim.image = "C6"
            case 14:CalPrim.image = "C5"
            case 15:CalPrim.image = "C4"
            case 16:CalPrim.image = "C3"
			case 17:CalPrim.image = "C2"
            case 18:CalPrim.image = "C0"
            case 19:CalPrim.image = "C0"
			case 20:CalPrim.image = "C0"
            case 21:CalPrim.image = "C0"
            case 22:CalPrim.image = "C0"
            case 23:CalPrim.image = "C0"
			case 24:CalPrim.image = "C0"
            case 25:CalPrim.image = "C0"
        end Select
End if
End Sub

Sub CLGIFtimer_Timer ' add GIftimer to table
CLGIFtimer.enabled = 0
CLGIF
End Sub

Dim SPTGifCountr 'static rededering on the primative must be set to false
Sub SPTGIF() 
If Speakers = 0 then SPTGIFtimer.enabled = 0
If Speakers= 1 then 
SPTGIFtimer.interval = 200
SPTGIFtimer.enabled = 1
SPTGifCountr = SPTGifCountr + 1
    If SPTGifCountr > 25 then SPTGifCountr = 0 end If
        select case SPTGifCountr
            case 0:InstituteBackWall.image = "spt0":Primitive040.image = "amp0"
            case 1:InstituteBackWall.image = "spt1":Primitive040.image = "amp1"
            case 2:InstituteBackWall.image = "spt2":Primitive040.image = "amp2"
            case 3:InstituteBackWall.image = "spt3":Primitive040.image = "amp3"
			case 4:InstituteBackWall.image = "spt4":Primitive040.image = "amp4"
            case 5:InstituteBackWall.image = "spt5":Primitive040.image = "amp5"
            case 6:InstituteBackWall.image = "spt6":Primitive040.image = "amp6"
			case 7:InstituteBackWall.image = "spt7":Primitive040.image = "amp7"
            case 8:InstituteBackWall.image = "spt8":Primitive040.image = "amp8"
            case 9:InstituteBackWall.image = "spt9":Primitive040.image = "amp9"
            case 10:InstituteBackWall.image = "spt9":Primitive040.image = "amp9"
			case 11:InstituteBackWall.image = "spt9":Primitive040.image = "amp9"
            case 12:InstituteBackWall.image = "spt0":Primitive040.image = "amp0"
			case 13:InstituteBackWall.image = "spt0":Primitive040.image = "amp0"
            case 14:InstituteBackWall.image = "spt0":Primitive040.image = "amp0"
            case 15:InstituteBackWall.image = "spt9":Primitive040.image = "amp9"
            case 16:InstituteBackWall.image = "spt9":Primitive040.image = "amp9"
			case 17:InstituteBackWall.image = "spt9":Primitive040.image = "amp9"
            case 18:InstituteBackWall.image = "spt0":Primitive040.image = "amp0"
            case 19:InstituteBackWall.image = "spt0":Primitive040.image = "amp0"
			case 20:InstituteBackWall.image = "spt0":Primitive040.image = "amp0"
            case 21:InstituteBackWall.image = "spt9":Primitive040.image = "amp9"
            case 22:InstituteBackWall.image = "spt9":Primitive040.image = "amp9"
            case 23:InstituteBackWall.image = "spt9":Primitive040.image = "amp9"
			case 24:InstituteBackWall.image = "spt0":Primitive040.image = "amp0"
            case 25:InstituteBackWall.image = "spt0":Primitive040.image = "amp0"
        end Select
End if
End Sub

Sub SPTGIFtimer_Timer ' add GIftimer to table
SPTGIFtimer.enabled = 0
SPTGIF
End Sub

Dim SLGifCountr 
Sub SLGIF() 
If SpeakerLights = 0 then SLGIFtimer.enabled = 0
If SpeakerLights = 1 then 
SLGIFtimer.interval = 1000
SLGIFtimer.enabled = 1
SLGifCountr=Int(13*Rnd(1))
        select case SLGifCountr
            case 0:
InstituteWindow1.image = "ins0"
InstituteWindow2.image = "ins0"
InstituteWindow3.image = "ins0"
InstituteWindow4.image = "ins0"
InstituteWindow5.image = "ins0"
InstituteWindow6.image = "ins0"
InstituteWindow7.image = "ins0"
InstituteWindow8.image = "ins0"
InstituteWindow9.image = "ins0"
            case 1:
InstituteWindow1.image = "ins1"
InstituteWindow2.image = "ins1"
InstituteWindow3.image = "ins1"
InstituteWindow4.image = "ins1"
InstituteWindow5.image = "ins1"
InstituteWindow6.image = "ins1"
InstituteWindow7.image = "ins1"
InstituteWindow8.image = "ins1"
InstituteWindow9.image = "ins1"
            case 2:
InstituteWindow1.image = "ins2"
InstituteWindow2.image = "ins2"
InstituteWindow3.image = "ins2"
InstituteWindow4.image = "ins2"
InstituteWindow5.image = "ins2"
InstituteWindow6.image = "ins2"
InstituteWindow7.image = "ins2"
InstituteWindow8.image = "ins2"
InstituteWindow9.image = "ins2"
            case 3:
InstituteWindow1.image = "ins3"
InstituteWindow2.image = "ins3"
InstituteWindow3.image = "ins3"
InstituteWindow4.image = "ins3"
InstituteWindow5.image = "ins3"
InstituteWindow6.image = "ins3"
InstituteWindow7.image = "ins3"
InstituteWindow8.image = "ins3"
InstituteWindow9.image = "ins3"
            case 4:
InstituteWindow1.image = "ins0"
InstituteWindow2.image = "ins1"
InstituteWindow3.image = "ins0"
InstituteWindow4.image = "ins1"
InstituteWindow5.image = "ins0"
InstituteWindow6.image = "ins1"
InstituteWindow7.image = "ins0"
InstituteWindow8.image = "ins1"
InstituteWindow9.image = "ins0"
            case 5:
InstituteWindow1.image = "ins0"
InstituteWindow2.image = "ins2"
InstituteWindow3.image = "ins0"
InstituteWindow4.image = "ins2"
InstituteWindow5.image = "ins0"
InstituteWindow6.image = "ins2"
InstituteWindow7.image = "ins0"
InstituteWindow8.image = "ins2"
InstituteWindow9.image = "ins0"
            case 6:
InstituteWindow1.image = "ins0"
InstituteWindow2.image = "ins3"
InstituteWindow3.image = "ins0"
InstituteWindow4.image = "ins3"
InstituteWindow5.image = "ins0"
InstituteWindow6.image = "ins3"
InstituteWindow7.image = "ins0"
InstituteWindow8.image = "ins3"
InstituteWindow9.image = "ins0"
            case 7:
InstituteWindow1.image = "ins1"
InstituteWindow2.image = "ins0"
InstituteWindow3.image = "ins1"
InstituteWindow4.image = "ins0"
InstituteWindow5.image = "ins1"
InstituteWindow6.image = "ins0"
InstituteWindow7.image = "ins1"
InstituteWindow8.image = "ins0"
InstituteWindow9.image = "ins1"
            case 8:
InstituteWindow1.image = "ins1"
InstituteWindow2.image = "ins2"
InstituteWindow3.image = "ins1"
InstituteWindow4.image = "ins2"
InstituteWindow5.image = "ins1"
InstituteWindow6.image = "ins2"
InstituteWindow7.image = "ins1"
InstituteWindow8.image = "ins2"
InstituteWindow9.image = "ins1"
            case 9:
InstituteWindow1.image = "ins1"
InstituteWindow2.image = "ins3"
InstituteWindow3.image = "ins1"
InstituteWindow4.image = "ins3"
InstituteWindow5.image = "ins1"
InstituteWindow6.image = "ins3"
InstituteWindow7.image = "ins1"
InstituteWindow8.image = "ins3"
InstituteWindow9.image = "ins1"
            case 10:
InstituteWindow1.image = "ins2"
InstituteWindow2.image = "ins0"
InstituteWindow3.image = "ins2"
InstituteWindow4.image = "ins0"
InstituteWindow5.image = "ins2"
InstituteWindow6.image = "ins0"
InstituteWindow7.image = "ins2"
InstituteWindow8.image = "ins0"
InstituteWindow9.image = "ins2"
            case 11:
InstituteWindow1.image = "ins2"
InstituteWindow2.image = "ins1"
InstituteWindow3.image = "ins2"
InstituteWindow4.image = "ins1"
InstituteWindow5.image = "ins2"
InstituteWindow6.image = "ins1"
InstituteWindow7.image = "ins2"
InstituteWindow8.image = "ins1"
InstituteWindow9.image = "ins2"
            case 12:
InstituteWindow1.image = "ins2"
InstituteWindow2.image = "ins3"
InstituteWindow3.image = "ins2"
InstituteWindow4.image = "ins3"
InstituteWindow5.image = "ins2"
InstituteWindow6.image = "ins3"
InstituteWindow7.image = "ins2"
InstituteWindow8.image = "ins3"
InstituteWindow9.image = "ins2"
end Select
End if
End Sub

Sub SLGIFtimer_Timer ' add GIftimer to table
SLGIFtimer.enabled = 0
SLGIF
End Sub


Dim BWGifCountr 'static rededering on the primative must be set to false
Sub BWGIF() 
If SideWalls = 0 then BWGIFtimer.enabled = 0
If SideWalls = 1 then 
BWGIFtimer.interval = 70
BWGIFtimer.enabled = 1
 BWGifCountr = BWGifCountr + 1
    If BWGifCountr > 11 then BWGifCountr = 0 end If
        select case BWGifCountr
            case 0:BACKWALL.image = "!bw (1)":SideL.image = "SPL":SideR.image = "SPR"
            case 1:BACKWALL.image = "!bw (2)"::SideL.image = "SP1L":SideR.image = "SP1R"
            case 2:BACKWALL.image = "!bw (3)":SideL.image = "SP2L":SideR.image = "SP2R"
            case 3:BACKWALL.image = "!bw (4)":SideL.image = "SP3L":SideR.image = "SP3R"
			case 4:BACKWALL.image = "!bw (5)":SideL.image = "SP4L":SideR.image = "SP4R"
            case 5:BACKWALL.image = "!bw (6)":SideL.image = "SP5L":SideR.image = "SP5R"
            case 6:BACKWALL.image = "!bw (7)":SideL.image = "SP6L":SideR.image = "SP6R"
			case 7:BACKWALL.image = "!bw (6)":SideL.image = "SP5L":SideR.image = "SP5R"
            case 8:BACKWALL.image = "!bw (5)":SideL.image = "SP4L":SideR.image = "SP4R"
            case 9:BACKWALL.image = "!bw (4)":SideL.image = "SP3L":SideR.image = "SP3R"
            case 10:BACKWALL.image = "!bw (3)":SideL.image = "SP2L":SideR.image = "SP2R"
			case 11:BACKWALL.image = "!bw (2)":SideL.image = "SP1L" :SideR.image = "SP1R"
        end Select
end if
End Sub

Sub BWGIFtimer_Timer ' add GIftimer to table
BWGIFtimer.enabled = 0
BWGIF
End Sub

Dim firelPos:firelPos = 1
Sub startfirel()
fireleft.enabled = 1
End Sub

Sub fireleft_Timer() ' add timer called fireleft
If flames = 0 then
fireleft.enabled = 0
fireleftf001.visible = 0
fireleftf002.visible = 0
end if
If flames = 1 then
firelPos = firelPos + 1
If firelpos = 29 then firelpos = 0 ' restart animation numer of frames
fireleftf001.ImageA = "flameB_0" & firelPos 'first image 
fireleftf001.ImageB = "flameB_01" & firelPos 'second image then all in order
fireleftf002.ImageA = "flameB_0" & firelPos 'first image 
fireleftf002.ImageB = "flameB_01" & firelPos 'second image then all in order
End If
End Sub

Dim lightninglfPos:lightninglfPos = 1
Sub lightningsling()
lightningl.visible = 1
lightningl001.visible = 1
lightningstop.Enabled = 1
End Sub

Sub lightningtimer_Timer() ' add timer called fireleft
'lightningl.visible = 1: lightningl001.visible = 1
lightninglfPos = lightninglfPos + 1
If lightninglfPos = 10 then lightninglfPos = 0 
lightningl.ImageA = "L_00" & lightninglfPos 'first image 
lightningl.ImageB = "L_01" & lightninglfPos 'second image then all in order
lightningl001.ImageA = "R_00" & lightninglfPos 'first image 
lightningl001.ImageB = "R_01" & lightninglfPos 'second image then all in order
End Sub

Sub lightningstop_timer()
lightningl.visible = 0: lightningl001.visible = 0
lightningstop.Enabled = 0
End Sub

Dim Lightningcounter
Sub lightningintrostart()
If lightning = 0 then 
lightningintro.Enabled = 0
lightningl.visible = 0: lightningl001.visible = 0
End if
If lightning = 1 then 
lightningintro.interval = 200
lightningintro.Enabled = 1
Lightningcounter = Lightningcounter + 1
    If Lightningcounter > 4 then Lightningcounter = 0 end If
        select case Lightningcounter
            case 0:lightningl.visible = 0: lightningl001.visible = 0:lightningintro.interval = 3000
            case 1:lightningl.visible = 1: lightningl001.visible = 1
            case 2:lightningl.visible = 0: lightningl001.visible = 0:lightningintro.interval = 3000
            case 3:lightningl.visible = 1: lightningl001.visible = 1
        end Select
end if
End Sub

Sub lightningintro_timer()
lightningintro.Enabled = 0
lightningl.visible = 0: lightningl001.visible = 0
lightningintrostart
End Sub


Sub WHR001_hit()
lightningintro.Enabled = 0
lightningl.visible = 0: lightningl001.visible = 0
End Sub


lightningintrostart






Sub WHR_hit()
	PlaySound "fx_wallhitright"
End Sub

Sub WHL_hit()
	PlaySound "fx_wallhitleft"
End Sub

Sub Gate2_hit()
	WireRampOn True
End Sub

Sub Trigger001_hit()
	RandomSoundRubberStrong
End Sub

Sub Trigger002_hit()
	RandomSoundRubberStrong
End Sub

Sub Trigger003_hit()
Stopmusic
Startmusic
End Sub

Sub Trigger004_hit()
	PlaySound"loops"
End Sub



Sub Trigger006_hit()
'pupevent 100
End Sub


stagelts
Dim stCountr 
Sub stagelts() 
slctimer.enabled = 1
stCountr = stCountr + 1
    If stCountr > 4 then stCountr = 0 end If
        select case stCountr
            case 0:Stagelights2
            case 1:Stagelights
			case 2:Stagelights2
            case 3:Stagelights
        end Select
End Sub

Sub slctimer_Timer()
slctimer.enabled = 0
stagelts
End Sub


Sub Stagelights()
sl1.State=2
sl2.State=2
sl3.State=2
sl4.State=2
sl5.State=2
sl1.BlinkInterval=175
sl2.BlinkInterval=175
sl3.BlinkInterval=175
sl4.BlinkInterval=175
sl5.BlinkInterval=175
sl1.BlinkPattern="10000010011001100000101001100110"
sl2.BlinkPattern="01000010000110000001001000011000"
sl3.BlinkPattern="00100010011001100010001001100110"
sl4.BlinkPattern="00010010000110000100001000011000"
sl5.BlinkPattern="00001010011001101000001001100110"
sl1a.State=0
sl2a.State=0
sl3a.State=0
sl4a.State=0
sl5a.State=0
End Sub


Sub Stagelights2()
sl1a.State=2
sl2a.State=2
sl3a.State=2
sl4a.State=2
sl5a.State=2
sl1a.BlinkInterval=175
sl2a.BlinkInterval=175
sl3a.BlinkInterval=175
sl4a.BlinkInterval=175
sl5a.BlinkInterval=175
sl1a.BlinkPattern="10000010011001100000101001100110"
sl2a.BlinkPattern="01000010000110000001001000011000"
sl3a.BlinkPattern="00100010011001100010001001100110"
sl4a.BlinkPattern="00010010000110000100001000011000"
sl5a.BlinkPattern="00001010011001101000001001100110"
sl1.State=0
sl2.State=0
sl3.State=0
sl4.State=0
sl5.State=0
End Sub


Sub Rubbers_Hit(idx)
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 5 then		
		RandomSoundRubberStrong 
	End if
	If finalspeed <= 5 then
		RandomSoundRubberWeak()
	End If	
End Sub

Sub RandomSoundRubberStrong()
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySound "Rubber_Strong_1", 0, Vol(ActiveBall)*50, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "Rubber_Strong_2", 0, Vol(ActiveBall)*50, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "Rubber_Strong_3", 0, Vol(ActiveBall)*50, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 4 : PlaySound "Rubber_Strong_4", 0, Vol(ActiveBall)*50, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 5 : PlaySound "Rubber_Strong_5", 0, Vol(ActiveBall)*50, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 6 : PlaySound "Rubber_Strong_6", 0, Vol(ActiveBall)*50, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 7 : PlaySound "Rubber_Strong_7", 0, Vol(ActiveBall)*50, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 8 : PlaySound "Rubber_Strong_8", 0, Vol(ActiveBall)*50, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 9 : PlaySound "Rubber_Strong_9", 0, Vol(ActiveBall)*50, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 10 : PlaySound "Rubber_1_Hard", 0, Vol(ActiveBall)*50, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub


Sub RandomSoundRubberWeak()
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySound "Rubber_1", 0, Vol(ActiveBall)*100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "Rubber_2", 0, Vol(ActiveBall)*100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "Rubber_3", 0, Vol(ActiveBall)*100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 4 : PlaySound "Rubber_4", 0, Vol(ActiveBall)*100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 5 : PlaySound "Rubber_5", 0, Vol(ActiveBall)*100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 6 : PlaySound "Rubber_6", 0, Vol(ActiveBall)*100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 7 : PlaySound "Rubber_7", 0, Vol(ActiveBall)*100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 8 : PlaySound "Rubber_8", 0, Vol(ActiveBall)*100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 9 : PlaySound "Rubber_9", 0, Vol(ActiveBall)*100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub RandomSoundDrain()
	PlaySound ("Drain_" & Int(Rnd*11)+1)
End Sub

Sub RandomSoundRollover() 'RandomSoundRollover add to all switches 
	PlaySound ("Rollover_" & Int(Rnd*4)+1)
End Sub

Sub Walls_Hit(idx)
	RandomSoundWall()      
End Sub

Sub RandomSoundWall()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		Select Case Int(Rnd*5)+1
			Case 1 : PlaySound "Wall_Hit_1", 0, Vol(ActiveBall) *100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
			Case 2 : PlaySound "Wall_Hit_2", 0, Vol(ActiveBall) *100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
			Case 3 : PlaySound "Wall_Hit_5", 0, Vol(ActiveBall) *100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
			Case 4 : PlaySound "Wall_Hit_7", 0, Vol(ActiveBall) *100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
			Case 5 : PlaySound "Wall_Hit_9", 0, Vol(ActiveBall) *100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		Select Case Int(Rnd*4)+1
			Case 1 : PlaySound "Wall_Hit_3", 0, Vol(ActiveBall) *100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
			Case 2 : PlaySound "Wall_Hit_4", 0, Vol(ActiveBall) *100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
			Case 3 : PlaySound "Wall_Hit_6", 0, Vol(ActiveBall) *100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
			Case 4 : PlaySound "Wall_Hit_8", 0, Vol(ActiveBall) *100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySound "Wall_Hit_4", 0, Vol(ActiveBall) *100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
			Case 2 : PlaySound "Wall_Hit_6", 0, Vol(ActiveBall) *100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
			Case 3 : PlaySound "Wall_Hit_8", 0, Vol(ActiveBall) *100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		End Select
	End if
End Sub


Sub RandomSoundMetal()
	PlaySound ("Metal_Touch_" & Int(Rnd*13)+1), 0, Vol(ActiveBall) * 100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlayTargetSound()
	PlaySound "fx_target"
PlaySound"targget"
End Sub

Sub RandomSoundBumper1()
	PlaySound("Bumpers_Top_" & Int(Rnd*5)+1), 0, Vol(ActiveBall) *100
End Sub

Sub RandomSoundBumper2()
	PlaySound("Bumpers_Middle_" & Int(Rnd*5)+1), 0, Vol(ActiveBall) *100
End Sub

Sub RandomSoundBumper3()
	PlaySound("Bumpers_Bottom_" & Int(Rnd*5)+1), 0, Vol(ActiveBall) *100
End Sub

Sub RandomSoundSlingshotLeft()
	PlaySound("Sling_L" & Int(Rnd*10)+1), 0, Vol(ActiveBall) *100, -1
End Sub

Sub RandomSoundSlingshotRight()
	 PlaySound("Sling_R" & Int(Rnd*8)+1), 0, Vol(ActiveBall) *100, 1
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySound "Flipper_Rubber_1", 0, Vol(ActiveBall) *100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "Flipper_Rubber_2", 0, Vol(ActiveBall) *100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "Flipper_Rubber_3", 0, Vol(ActiveBall) *100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 4 : PlaySound "Flipper_Rubber_4", 0, Vol(ActiveBall) *100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 5 : PlaySound "Flipper_Rubber_5", 0, Vol(ActiveBall) *100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 6 : PlaySound "Flipper_Rubber_6", 0, Vol(ActiveBall) *100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 7 : PlaySound "Flipper_Rubber_7", 0, Vol(ActiveBall) *100, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub


Const RampRollVolume = 2			'Level of ramp rolling volume. Value between 0 and 1
Const VolumeDial = 1.0

dim RampMinLoops : RampMinLoops = 4

' RampBalls
'      Setup:        Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RammBalls(6,2)
'      Description:  
dim RampBalls(6,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(6)	

Sub WireRampOn(input)  : Waddball ActiveBall, input : RampRollUpdate: End Sub
Sub WireRampOff() : WRemoveBall ActiveBall.ID	: End Sub


' WaddBall (Active Ball, Boolean)
'     Description: This subroutine is called from WireRampOn to Add Balls to the RampBalls Array
Sub Waddball(input, RampInput)	'Add ball
	' This will loop through the RampBalls array checking each element of the array x, position 1
	' To see if the the ball was already added to the array.
	' If the ball is found then exit the subroutine
	dim x : for x = 1 to uBound(RampBalls)	'Check, don't add balls twice
		if RampBalls(x, 1) = input.id then 
			if Not IsEmpty(RampBalls(x,1) ) then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next

	' This will itterate through the RampBalls Array.
	' The first time it comes to a element in the array where the Ball Id (Slot 1) is empty.  It will add the current ball to the array
	' The RampBalls assigns the ActiveBall to element x,0 and ball id of ActiveBall to 0,1
	' The RampType(BallId) is set to RampInput
	' RampBalls in 0,0 is set to True, this will enable the timer and the timer is also turned on
	For x = 1 to uBound(RampBalls)
		if IsEmpty(RampBalls(x, 1)) then 
			Set RampBalls(x, 0) = input
			RampBalls(x, 1)	= input.ID
			RampType(x) = RampInput
			RampBalls(x, 2)	= 0
			'exit For
			RampBalls(0,0) = True
			RampRoll.Enabled = 1	 'Turn on timer
			'RampRoll.Interval = RampRoll.Interval 'reset timer
			exit Sub
		End If
		if x = uBound(RampBalls) then 	'debug
			Debug.print "WireRampOn error, ball queue is full: " & vbnewline & _
			RampBalls(0, 0) & vbnewline & _
			Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & "type:" & RampType(1) & vbnewline & _
			Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & "type:" & RampType(2) & vbnewline & _
			Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & "type:" & RampType(3) & vbnewline & _
			Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & "type:" & RampType(4) & vbnewline & _
			Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & "type:" & RampType(5) & vbnewline & _
			" "
		End If
	next
End Sub


Sub WRemoveBall(ID)		'Remove ball
	dim ballcount : ballcount = 0
	dim x : for x = 1 to Ubound(RampBalls)
		if ID = RampBalls(x, 1) then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end If
		'if RampBalls(x,1) = Not IsEmpty(Rampballs(x,1) then ballcount = ballcount + 1
		if not IsEmpty(Rampballs(x,1)) then ballcount = ballcount + 1
	next
	if BallCount = 0 then RampBalls(0,0) = False	'if no balls in queue, disable timer update
End Sub

Sub RampRoll_Timer():RampRollUpdate:End Sub

Sub RampRollUpdate()		'Timer update
	dim x : for x = 1 to uBound(RampBalls)
		if Not IsEmpty(RampBalls(x,1) ) then 
			if BallVel(RampBalls(x,0) ) > 1 then ' if ball is moving, play rolling sound
				If RampType(x) then 
					PlaySound("RampLoop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))				
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				End If
				RampBalls(x, 2)	= RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
			end if
			if RampBalls(x,0).Z < 30 and RampBalls(x, 2) > RampMinLoops then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end if
	next
	if not RampBalls(0,0) then RampRoll.enabled = 0

End Sub

' This can be used to debug the Ramp Roll time.  You need to enable the tbWR timer on the TextBox
Sub tbWR_Timer()	'debug textbox
	me.text =	"on? " & RampBalls(0, 0) & " timer: " & RampRoll.Enabled & vbnewline & _
	"1 " & Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & " type:" & RampType(1) & " Loops:" & RampBalls(1, 2) & vbnewline & _
	"2 " & Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & " type:" & RampType(2) & " Loops:" & RampBalls(2, 2) & vbnewline & _
	"3 " & Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & " type:" & RampType(3) & " Loops:" & RampBalls(3, 2) & vbnewline & _
	"4 " & Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & " type:" & RampType(4) & " Loops:" & RampBalls(4, 2) & vbnewline & _
	"5 " & Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & " type:" & RampType(5) & " Loops:" & RampBalls(5, 2) & vbnewline & _
	"6 " & Typename(RampBalls(6, 0)) & " ID:" & RampBalls(6, 1) & " type:" & RampType(6) & " Loops:" & RampBalls(6, 2) & vbnewline & _
	" "
End Sub

Function BallPitch(ball) ' Calculates the pitch of the sound based on the ball speed
    BallPitch = pSlope(BallVel(ball), 1, -1000, 60, 10000)
End Function

Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
	BallPitchV = pSlope(BallVel(ball), 1, -4000, 60, 7000)
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
End Function

Dim RollingSoundFactor
RollingSoundFactor = 0.2

Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

		Controller.Games(cGameName).Settings.Value("dmd_red") = 255
        Controller.Games(cGameName).Settings.Value("dmd_green") = 255
        Controller.Games(cGameName).Settings.Value("dmd_blue") = 255
        Controller.Games(cGameName).Settings.Value("dmd_red66") = 255
        Controller.Games(cGameName).Settings.Value("dmd_green66") = 255
        Controller.Games(cGameName).Settings.Value("dmd_blue66") = 255
        Controller.Games(cGameName).Settings.Value("dmd_red33") = 5
        Controller.Games(cGameName).Settings.Value("dmd_green33") = 80
        Controller.Games(cGameName).Settings.Value("dmd_blue33") = 50
        Controller.Games(cGameName).Settings.Value("dmd_red0") = 0
        Controller.Games(cGameName).Settings.Value("dmd_green0") = 0
        Controller.Games(cGameName).Settings.Value("dmd_blue0") = 0
		
Sub intro_timer 
PlaySound"neverland",0,0.5
PlaySound "intro1",-1
intro.Enabled = 0
End sub



'*************************************************
' BALLS SAVE CHEAT
'**********************************************************
Dim tmp1  'add ballsavetimer not enabled add ballsavetrigger in shooter lane
Sub ballsavetrigger_Hit() 
If bslight.state = LightStateOFF Then
ballsavetimer.Interval = 15000
ballsavetimer.Enabled = true
bslight.state = LightStateOn
Else
bslight.state = LightStateOFF
end if
End Sub

Sub ballsavetimer_timer()
bslight.state = LightStateOff 
ballsavetimer.Enabled = False 
End Sub

Sub KickerIn_Hit()
If bslight.state = LightStateOn then
KickerIn.DestroyBall
PlaySound "plunger"
PlaySound "scream"
KickerOut.CreateBall
KickerOut.kick 0, 60
end if
If bslight.state = LightStateOff then
kickerin.Kick 180, 14
end if
End Sub

Dim PS1
Sub Startmusic()
PS1=Int(15*Rnd(1))
Select Case PS1
    Case 0 : PlaySound "Another Brick In The Wall1",-1
    Case 1 : PlaySound "Another Brick In The Wall" ,-1 
	Case 2 : PlaySound "Comfortably Numb",-1
    Case 3 : PlaySound "goodbye blue sky" ,-1 
	Case 4 : PlaySound "Have a Cigar",-1
    Case 5 : PlaySound "Money" ,-1 
	Case 6 : PlaySound "run",-1
    Case 7 : PlaySound "Shine On You Crazy Diamond" ,-1 
	Case 8 : PlaySound "Thin Ice",-1
    Case 9 : PlaySound "time" ,-1 
	Case 10 : PlaySound "Wall",-1
    Case 11 : PlaySound "welcome to machine" ,-1 
	Case 12 : PlaySound "wish",-1
    Case 13 : PlaySound "Worms" ,-1 
	Case 14 : PlaySound "Young Lust",-1
End Select
End Sub

Sub StopMusic()
StopSound "Another Brick In The Wall1"
StopSound "Another Brick In The Wall" 
StopSound "Comfortably Numb"
StopSound "goodbye blue sky" 
StopSound "Have a Cigar"
StopSound "Money" 
StopSound "run"
StopSound "Shine On You Crazy Diamond" 
StopSound "Thin Ice"
StopSound "time"  
StopSound "Wall"
StopSound "welcome to machine" 
StopSound "wish"
StopSound "Worms" 
StopSound "Young Lust"
StopSound "multiball" 
StopSound "intro1"
End SUb















