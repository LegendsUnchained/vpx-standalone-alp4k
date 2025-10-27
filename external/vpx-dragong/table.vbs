Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0


 
'**********************************************************
'********       OPTIONS     *******************************
'**********************************************************
 
Dim BallShadows: Ballshadows=1          '******************set to 1 to turn on Ball shadows
Dim FlipperShadows: FlipperShadows=1  '***********set to 1 to turn on Flipper shadows
Dim ROMSounds: ROMSounds=1				'**********set to 0 for no rom sounds, 1 to play rom sounds.. mostly used for testing


Const cGameName="dragon",UseSolenoids=2,UseLamps=1,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff",SFlipperOn="fx_Flipperup",SFlipperOff="fx_Flipperdown"
Const SCoin="coin",cCredits=""
Const VT_Delay_Factor = .80		'used to slow down the ball when hitting the vari targets, smaller number slows down faster

LoadVPM"01150000","GTS1.VBS",3.22


'Solenoid Call backs
'**********************************************************************************************************
SolCallback(1)="DrainKick"
SolCallback(2)="vpmSolSound SoundFX(""Knock"",DOFKnocker),"
SolCallback(3)="vpmSolSound SoundFX(""10pts"",DOFChimes),"
SolCallback(4)="vpmSolSound SoundFX(""100pts"",DOFChimes),"
SolCallback(5)="vpmSolSound SoundFX(""1000pts"",DOFChimes),"
	

SolCallback(6)="RVarireset"	
SolCallback(7)="LVarireset" 	
SolCallback(17)="vpmNudge.SolGameOn"
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub RVarireset(enabled)
	if enabled then Rreset.enabled=True
End Sub

dim rcount: rcount=0 
dim lcount: lcount=0

Sub Rreset_timer
	VariRTargetTimer.enabled=1
	playsoundat "Solenoid", PegPlasticT36
	me.enabled=false
End Sub

Sub LVarireset(enabled)
	if enabled then Lreset.enabled=True
End Sub

Sub Lreset_timer
	VariTargetTimer.enabled=1
	playsoundat "Solenoid", BulbTop15
	me.enabled=false
End Sub

Sub SolLFlipper(Enabled)
     If Enabled Then
        PlaySoundat SoundFx("fx_Flipperup",DOFContactors), Lflip
		LeftFlipper.RotateToEnd
       Else
         PlaySoundat SoundFx("fx_Flipperdown",DOFContactors), Lflip
		LeftFlipper.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
        PlaySoundat SoundFx("fx_Flipperup",DOFContactors), RFlip
		RightFlipper.RotateToEnd
       Else
        PlaySoundat SoundFx("fx_Flipperdown",DOFContactors), RFlip
		RightFlipper.RotateToStart
     End If
End Sub

'**********************************************************************************************************


'Primitive Flipper Code

Sub FlipperTimer_Timer
	dim PI:PI=3.1415926
	lflip.rotz = LeftFlipper.currentangle 
	rflip.rotz = RightFlipper.currentangle 
	Pgate1.rotz = (Gate1.currentangle*.75)+25

	if llane10.state=1 and l17.state=1 then metalguide_prim.image="metalGIONLIT":end If
	if llane10.state=1 and l17.state=0 then metalguide_prim.image="metalGION":end If
	if llane10.state=0 then metalguide_prim.image="metalGIOFF":end If
	if llane10.state=0 then metalguide_prim.blenddisablelighting=0:end If
	if llane10.state=1 then metalguide_prim.blenddisablelighting=0.4:end If
	if llane10.state=0 then outers_prim.image="outersGIOFF"
	if llane10.state=1 then outers_prim.image="outersGION"
	if llane10.state=0 then shadowsGIOFF.visible=1
	if llane10.state=0 then shadowsGION.visible=0
	if llane10.state=1 then shadowsGIOFF.visible=0
	if llane10.state=1 then shadowsGION.visible=1
	if llane10.state=0 then plastic1.image="plastic1off"
	if llane10.state=1 then plastic1.image="plastic1on"
	if llane10.state=0 then plastic2.image="plastic2off"
	if llane10.state=1 then plastic2.image="plastic2on"
	if llane10.state=0 then plastic3.image="plastic3off"
	if llane10.state=1 then plastic3.image="plastic3on"
	if llane10.state=0 then plastic4.image="plastic4off"
	if llane10.state=1 then plastic4.image="plastic4on"

	SpinnerP.Rotz = sw10.CurrentAngle
	SpinnerRod.TransZ = sin( (sw10.CurrentAngle+180) * (2*PI/360)) * 5
	SpinnerRod.TransX = -1*(sin( (sw10.CurrentAngle- 90) * (2*PI/360)) * 5)

	if FlipperShadows=1 then
		FlipperLSh.RotZ = LeftFlipper.currentangle
		FlipperRSh.RotZ = RightFlipper.currentangle
	end if

End Sub

'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************

Dim objekt
Dim BPG, hsaward, n, xx, Lstep

Sub Dragon_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine="Dragon (Gottlieb 1978)"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
        .hidden = 1
		If Err Then MsgBox Err.Description
	End With
	On Error Goto 0
		Controller.SolMask(0)=0
      vpmTimer.AddTimer 2000,"Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the timer to renable all the solenoids after 2 seconds
		Controller.Run
	If Err Then MsgBox Err.Description
	On Error Goto 0

	PinMAMETimer.Interval=PinMAMEInterval  
	PinMAMETimer.Enabled=1
	vpmNudge.TiltSwitch=4
	vpmNudge.Sensitivity=3
	vpmNudge.TiltObj=Array(BumperL,BumperR,SlingL)
 

 
 	vpmMapLights CPULights

	FindDips		'find balls per game and high score reward

	If B2SOn Then 'don't show Desktop components
		for each objekt in BackdropStuff: objekt.visible=0: next
	  Else
		for each objekt in BackdropStuff: objekt.visible=1: next
	End if

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

	drain.createball

	PRightSideRail.visible = 0
	PLeftSideRail.visible = 0
	PbackRail.visible = 0

	startGame.enabled=true

End Sub 

Sub DrainKick(enabled)
	If enabled Then
	Drain.kick 70, 12
	PlaySoundAt SoundFX("ballrelease",DOFContactors), Drain
	End If
end Sub

Sub Drain_Hit

	PlaySoundAt "drain", Drain
	controller.switch(66) = 1
End Sub

sub Drain_unhit
	controller.switch(66) = 0
end sub

Sub Dragon_Paused:Controller.Pause = 1:End Sub

Sub Dragon_unPaused:Controller.Pause = 0:End Sub 

Sub Dragon_Exit
	If b2son then controller.stop
End Sub

sub startGame_timer
	playsoundat "poweron", plunger
	For each xx in GILights:xx.State = 1: Next		'*****GI Lights On
	me.enabled=false
end sub


Sub Dragon_KeyDown(ByVal keycode)
	if keycode = 46 then' C Key
		If contball = 1 Then
			contball = 0
		  Else
			contball = 1
		End If
	End If

	if keycode = 48 then 'B Key
		If bcboost = 1 Then
			bcboost = bcboostmulti
		  Else
			bcboost = 1
		End If
	End If

	if keycode = 203 then Cleft = 1' Left Arrow

	if keycode = 200 then Cup = 1' Up Arrow

	if keycode = 208 then Cdown = 1' Down Arrow

	if keycode = 205 then Cright = 1' Right Arrow

	If vpmKeyDown(KeyCode) Then Exit Sub
	If keycode=AddCreditKey then playsoundat "coin", Drain: vpmTimer.pulseSW (swCoin1): end if
	If keycode=PlungerKey Then Plunger.Pullback:playsoundat "plungerpull", Plunger
End Sub

Sub Dragon_KeyUp(ByVal keycode)

	if keycode = 203 then Cleft = 0' Left Arrow

	if keycode = 200 then Cup = 0' Up Arrow

	if keycode = 208 then Cdown = 0' Down Arrow

	if keycode = 205 then Cright = 0' Right Arrow

	If keycode = LeftFlipperKey Then LeftFlipper.RotateToStart
   	If keycode = RightFlipperKey Then RightFlipper.RotateToStart
	
	If vpmKeyUp(KeyCode) Then Exit Sub
	If keycode=PlungerKey Then 
		Plunger.Fire
		if ballhome.ballcntover>0 then
			playsoundat "plungerreleaseball", Plunger
		  else
			playsoundat "plungerreleasefree", Plunger
		end if
	end if
End Sub

Sub StartControl_Hit()
	Set ControlBall = ActiveBall
	contballinplay = true
End Sub

Sub EndControl_Hit()
	contballinplay = false
End Sub

Dim Cup, Cdown, Cleft, Cright, contball, contballinplay, ControlBall, bcboost
Dim bcvel, bcyveloffset, bcboostmulti

bcboost = 1 'Do Not Change - default setting
bcvel = 4 'Controls the speed of the ball movement
bcyveloffset = -0.01 'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
bcboostmulti = 3 'Boost multiplier to ball veloctiy (toggled with the B key)

Sub BallControl_Timer()
	If Contball and ContBallInPlay then
		If Cright = 1 Then
			ControlBall.velx = bcvel*bcboost
		  ElseIf Cleft = 1 Then
			ControlBall.velx = - bcvel*bcboost
		  Else
			ControlBall.velx=0
		End If
		If Cup = 1 Then
			ControlBall.vely = -bcvel*bcboost
		  ElseIf Cdown = 1 Then
			ControlBall.vely = bcvel*bcboost
 		  Else
			ControlBall.vely= bcyveloffset
		End If
	End If
End Sub

Sub Ballhome_hit
	DOF 116, 1:
end sub

sub ballhome_unhit
	DOF 116, 0
end sub


'bumpers
Sub BumperL_Hit
	vpmTimer.PulseSw 50 
	DOF 103, 2
	playsoundat SoundFX("fx_bumper",DOFContactors), BumperL
End Sub

Sub BumperR_Hit
	vpmTimer.PulseSw 50 
	DOF 104, 2
	PlaySoundAt SoundFX("fx_bumper",DOFContactors), BumperR
End Sub

'Hit Target

	Sub sw12_Hit:vpmTimer.PulseSw 12:End Sub
	Sub sw40_Hit:vpmTimer.PulseSw 40:End Sub

'Rollover wire Tirggers

	  Sub sw14_Hit:Controller.Switch(14) = 1: End Sub
	  Sub sw14_UnHit:Controller.Switch(14) = 0: End Sub
	  Sub sw20_Hit:Controller.Switch(20) = 1: End Sub
	  Sub sw20_UnHit:Controller.Switch(20) = 0: End Sub
	  Sub sw21_Hit:Controller.Switch(21) = 1: End Sub
	  Sub sw21_UnHit:Controller.Switch(21) = 0:End Sub
	  Sub sw22_Hit:Controller.Switch(22) = 1:End Sub
	  Sub sw22_UnHit:Controller.Switch(22) = 0: End Sub
	  Sub sw24_Hit:Controller.Switch(24) = 1: End Sub
	  Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub
	  Sub sw20a_Hit:Controller.Switch(20) = 1: End Sub
	  Sub sw20a_UnHit:Controller.Switch(20) = 0: End Sub
	  Sub sw21a_Hit:Controller.Switch(21) = 1: End Sub
	  Sub sw21a_UnHit:Controller.Switch(21) = 0:End Sub
	  Sub sw22a_Hit:Controller.Switch(22) = 1: End Sub
	  Sub sw22a_UnHit:Controller.Switch(22) = 0: End Sub
	  Sub sw24a_Hit:Controller.Switch(24) = 1: End Sub
	  Sub sw24a_UnHit:Controller.Switch(24) = 0:End Sub


'Star Rollovers

	  Sub sw30_Hit:Controller.Switch(30) = 1: End Sub
	  Sub sw30_UnHit:Controller.Switch(30) = 0: End Sub
	  Sub sw31_Hit:Controller.Switch(31) = 1: End Sub
	  Sub sw31_UnHit:Controller.Switch(31) = 0:End Sub
	  Sub sw32_Hit:Controller.Switch(32) = 1: End Sub
	  Sub sw32_UnHit:Controller.Switch(32) = 0: End Sub
	  Sub sw34_Hit:Controller.Switch(34) = 1: End Sub
	  Sub sw34_UnHit:Controller.Switch(34) = 0:End Sub

'Spinner

Sub sw10_Spin
	vpmTimer.PulseSw 10
	PlaySound "fx_spinner", 0, .25, AudioPan(sw10), 0.25, 0, 0, 1, AudioFade(sw10)
End Sub

'Slingshot

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 44
    PlaySoundat SoundFXDOF("left_slingshot",101,DOFPulse,DOFContactors), slingL
    LSling.Visible = 0
    LSling1.Visible = 1
	slingL.objroty = 15
    LStep = 1
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:slingL.objroty = 7
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:slingL.objroty = 0:LeftSlingShot.TimerEnabled = 0:
    End Select
    LStep = LStep + 1
End Sub
		 
'Scoring rubbers - ANIMATED!!!!!

	Sub sw44a_hit()
		vpmTimer.PulseSw 44
		R44a.visible=0
		R44a1.visible=1
		me.uservalue=1
		me.timerenabled=1
	End Sub

	sub sw44a_timer
		select case sw44a.uservalue
			Case 1: r44a1.visible=0: r44a.visible=1
			Case 2: r44a.visible=0: r44a2.visible=1
			Case 3: r44a2.visible=0: r44a.visible=1: me.timerenabled=0
		end Select
		me.uservalue=me.uservalue+1
	end sub

	Sub sw44b_hit()
		vpmTimer.PulseSw 44
		R44b.visible=0
		R44b1.visible=1
		me.uservalue=1
		me.timerenabled=1
	End Sub

	sub sw44b_timer
		select case sw44b.uservalue
			Case 1: r44b1.visible=0: r44b.visible=1
			Case 2: r44b.visible=0: r44b2.visible=1
			Case 3: r44b2.visible=0: r44b.visible=1: me.timerenabled=0
		end Select
		me.uservalue=me.uservalue+1
	end sub

	Sub sw44c_hit()
		vpmTimer.PulseSw 44
		R44c.visible=0
		R44c1.visible=1
		me.uservalue=1
		me.timerenabled=1
	End Sub

	sub sw44c_timer
		select case sw44c.uservalue
			Case 1: r44c1.visible=0: r44c.visible=1
			Case 2: r44c.visible=0: r44c2.visible=1
			Case 3: r44c2.visible=0: r44c.visible=1: me.timerenabled=0
		end Select
		me.uservalue=me.uservalue+1
	end sub

	Sub sw44d_hit()
		vpmTimer.PulseSw 44
		R44d.visible=0
		R44d1.visible=1
		me.uservalue=1
		me.timerenabled=1
	End Sub

	sub sw44d_timer
		select case sw44d.uservalue
			Case 1: r44d1.visible=0: r44d.visible=1
			Case 2: r44d.visible=0: r44d2.visible=1
			Case 3: r44d2.visible=0: r44d.visible=1: me.timerenabled=0
		end Select
		me.uservalue=me.uservalue+1
	end sub

	Sub sw44e_hit()
'		vpmTimer.PulseSw 44			'this one is not actually a scoring rubber, just easier to copy to animate
		R44e.visible=0
		R44e1.visible=1
		me.uservalue=1
		me.timerenabled=1
	End Sub

	sub sw44e_timer
		select case sw44e.uservalue
			Case 1: r44e1.visible=0: r44e.visible=1
			Case 2: r44e.visible=0: r44e2.visible=1
			Case 3: r44e2.visible=0: r44e.visible=1: me.timerenabled=0
		end Select
		me.uservalue=me.uservalue+1
	end sub

''***********************************************************************************
''****				       		VariTarget Handling	     				    	****
''***********************************************************************************
Dim VariSwitch, i, VariActive
Dim VariSwitches: VariSwitches = Array(41, 42, 43)
Dim VariPositions: VariPositions = Array(2, 12, 18)
Dim Varinumpos: VariNumpos=3


Sub VariTrigger_Hit(vidx)
	If ((ActiveBall.VelY < 0)) Then
		ActiveBall.VelY = ActiveBall.VelY * VT_Delay_Factor
		For i = 0 to (VariNumpos-1)
			If vidx >= VariPositions(i) Then VariSwitch = VariSwitches(i)
		Next
		VariTargetP.RotX = -(vidx * 1.5)
	  else if VariActive=0 AND VariSwitch>-1 then
		Controller.Switch(VariSwitch) = 1
		VariActive=1
	  end if
	End If
End Sub

Sub VariTrigger_UnHit(idx)
	If ActiveBall.VelY > 0 Then DOF 119,0
End Sub

Sub VariTargetTimer_Timer
	If VariTargetP.RotX < 0 Then
		VariTargetP.RotX = VariTargetP.RotX + 1.5
	Else
		VariTargetP.RotX = 0
		For i = 0 to (VariNumpos-1)
			Controller.Switch(VariSwitches(i)) = 0
		Next
		VariSwitch = -1
		VariActive = 0
		Me.Enabled = 0
	End If
End Sub

Sub VariTarget_Init
	VariSwitch = -1
	VariActive = 0
End Sub

''************repeat for Right side VariTarget

Dim VariRSwitch, Ri, VariRActive
Dim VariRSwitches: VariRSwitches = Array(51, 52, 53)
Dim VariRPositions: VariRPositions = Array(2, 12, 18)
Dim VariRnumpos: VariRnumpos=3

Sub VariRTrigger_Hit(vidx)
	If ((ActiveBall.VelY < 0)) Then
		ActiveBall.VelY = ActiveBall.VelY * VT_Delay_Factor
		For Ri = 0 to (VariRnumpos-1)
			If vidx >= VariRPositions(Ri) Then VariRSwitch = VariRSwitches(Ri)
		Next
		VariTargetPR.RotX = -(vidx * 1.5)
	  else if VariRActive=0 AND VariRSwitch>-1 then
		Controller.Switch(VariRSwitch) = 1
		VariRActive=1
	  end if
	End If
End Sub

Sub VariRTrigger_UnHit(idx)
	If ActiveBall.VelY > 0 Then DOF 119,0
End Sub

Sub VariRTargetTimer_Timer
	If VariTargetPR.RotX < 0 Then
		VariTargetPR.RotX = VariTargetPR.RotX + 1.5
	Else
		VariTargetPR.RotX = 0
		For Ri = 0 to (VariRnumpos-1)
			Controller.Switch(VariRSwitches(Ri)) = 0
		Next
		VariRSwitch = -1
		VariRActive = 0
		Me.Enabled = 0
	End If
End Sub

Sub VariRTarget_Init
	VariRSwitch = -1
	VariRActive = 0
End Sub


'***************backbox Lights

 Dim N1,O1, Light
 N1=0:O1=0:
 Set LampCallback=GetRef("UpdateMultipleLamps")
 
Sub UpdateMultipleLamps
	If L17.state > 0 Then VariTargetTimer.Enabled = 1

	if l2.state=1 then 
		for each light in GILights: light.state=0: Next
	  Else
		for each light in GILights: light.state=1: Next
	end if

	if l1b.state=0 then
		l1.state = 1	'Game over Light
		l1a.state = 1	'number to match light
	  else
		l1.state = 0	'Game over Light
		l1a.state = 0	'number to match light
	end if

 End Sub
 


Dim Digits(32)
Digits(0)=Array(a00,a01,a02,a03,a04,a05,a06,n,a08)
Digits(1)=Array(a10,a11,a12,a13,a14,a15,a16,n,a18)
Digits(2)=Array(a20,a21,a22,a23,a24,a25,a26,n,a28)
Digits(3)=Array(a30,a31,a32,a33,a34,a35,a36,n,a38)
Digits(4)=Array(a40,a41,a42,a43,a44,a45,a46,n,a48)
Digits(5)=Array(a50,a51,a52,a53,a54,a55,a56,n,a58)
Digits(6)=Array(b00,b01,b02,b03,b04,b05,b06,n,b08)
Digits(7)=Array(b10,b11,b12,b13,b14,b15,b16,n,b18)
Digits(8)=Array(b20,b21,b22,b23,b24,b25,b26,n,b28)
Digits(9)=Array(b30,b31,b32,b33,b34,b35,b36,n,b38)
Digits(10)=Array(b40,b41,b42,b43,b44,b45,b46,n,b48)
Digits(11)=Array(b50,b51,b52,b53,b54,b55,b56,n,b58)
Digits(12)=Array(c00,c01,c02,c03,c04,c05,c06,n,c08)
Digits(13)=Array(c10,c11,c12,c13,c14,c15,c16,n,c18)
Digits(14)=Array(c20,c21,c22,c23,c24,c25,c26,n,c28)
Digits(15)=Array(c30,c31,c32,c33,c34,c35,c36,n,c38)
Digits(16)=Array(c40,c41,c42,c43,c44,c45,c46,n,c48)
Digits(17)=Array(c50,c51,c52,c53,c54,c55,c56,n,c58)
Digits(18)=Array(d00,d01,d02,d03,d04,d05,d06,n,d08)
Digits(19)=Array(d10,d11,d12,d13,d14,d15,d16,n,d18)
Digits(20)=Array(d20,d21,d22,d23,d24,d25,d26,n,d28)
Digits(21)=Array(d30,d31,d32,d33,d34,d35,d36,n,d38)
Digits(22)=Array(d40,d41,d42,d43,d44,d45,d46,n,d48)
Digits(23)=Array(d50,d51,d52,d53,d54,d55,d56,n,d58)
Digits(26)=Array(e00,e01,e02,e03,e04,e05,e06,n,e08)
Digits(27)=Array(e10,e11,e12,e13,e14,e15,e16,n,e18)
Digits(24)=Array(f00,f01,f02,f03,f04,f05,f06,n,f08)
Digits(25)=Array(f10,f11,f12,f13,f14,f15,f16,n,f18)



Sub DisplayTimer_Timer
	Dim ChgLED,ii,num,chg,stat,obj
	ChgLed = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
	If Not IsEmpty(ChgLED) Then
		If not b2son Then
		For ii = 0 To UBound(chgLED)
			num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
			if (num < 32) then
				For Each obj In Digits(num)
					If chg And 1 Then obj.State = stat And 1 
					chg = chg\2 : stat = stat\2
				Next
			else

			end if
		next
		end if
	end if
End Sub



'Finding an individual dip state based on scapino's Strikes and spares dip code - from unclewillys pinball pool
Dim TheDips(32)
  Sub FindDips
    Dim DipsNumber
	DipsNumber = Controller.Dip(1)
	TheDips(16) = Int(DipsNumber/128)
	If TheDips(16) = 1 then DipsNumber = DipsNumber - 128 end if
	TheDips(15) = Int(DipsNumber/64)
	If TheDips(15) = 1 then DipsNumber = DipsNumber - 64 end if
	TheDips(14) = Int(DipsNumber/32)
	If TheDips(14) = 1 then DipsNumber = DipsNumber - 32 end if
	TheDips(13) = Int(DipsNumber/16)
	If TheDips(13) = 1 then DipsNumber = DipsNumber - 16 end if
	TheDips(12) = Int(DipsNumber/8)
	If TheDips(12) = 1 then DipsNumber = DipsNumber - 8 end if
	TheDips(11) = Int(DipsNumber/4)
	If TheDips(11) = 1 then DipsNumber = DipsNumber - 4 end if
	TheDips(10) = Int(DipsNumber/2)
	If TheDips(10) = 1 then DipsNumber = DipsNumber - 2 end if
	TheDips(9) = Int(DipsNumber)
	DipsNumber = Controller.Dip(2)
	TheDips(24) = Int(DipsNumber/128)
	If TheDips(24) = 1 then DipsNumber = DipsNumber - 128 end if
	TheDips(23) = Int(DipsNumber/64)
	If TheDips(23) = 1 then DipsNumber = DipsNumber - 64 end if
	TheDips(22) = Int(DipsNumber/32)
	If TheDips(22) = 1 then DipsNumber = DipsNumber - 32 end if
	TheDips(21) = Int(DipsNumber/16)
	If TheDips(21) = 1 then DipsNumber = DipsNumber - 16 end if
	TheDips(20) = Int(DipsNumber/8)
	If TheDips(20) = 1 then DipsNumber = DipsNumber - 8 end if
	TheDips(19) = Int(DipsNumber/4)
	If TheDips(19) = 1 then DipsNumber = DipsNumber - 4 end if
	TheDips(18) = Int(DipsNumber/2)
	If TheDips(18) = 1 then DipsNumber = DipsNumber - 2 end if
	TheDips(17) = Int(DipsNumber)
	DipsTimer.Enabled=1
 End Sub


 Sub DipsTimer_Timer()
	hsaward = TheDips(22)
	BPG = TheDips(9)
	dim ebplay: ebplay= TheDips(11)
	If BPG = 1 then 
		if ebplay = 1 then
			instcard.image="InstCard3Balls"
		  else
			instcard.image="InstCard3BallsEB"
		end if
	  Else
		if ebplay = 1 then
			instcard.image="InstCard5Balls"
		  else
			instcard.image="InstCard5BallsEB"
		end if
	End if
	repcard.image="replaycard"&hsaward
	DipsTimer.enabled=0
 End Sub

 'Gottlieb System 1
 'added by Inkochnito
 Sub editDips
 	Dim vpmDips : Set vpmDips = New cvpmDips
 	With vpmDips
 		.AddForm 700,400,"System 1 - DIP switches"
 		.AddFrame 205,0,190,"Maximum credits",&H00030000,Array("5 credits",0,"8 credits",&H00020000,"10 credits",&H00010000,"15 credits",&H00030000)'dip 17&18
 		.AddFrame 0,0,190,"Coin chute control",&H00040000,Array("seperate",0,"same",&H00040000)'dip 19
 		.AddFrame 0,46,190,"Game mode",&H00000400,Array("extra ball",0,"replay",&H00000400)'dip 11
 		.AddFrame 0,92,190,"High game to date awards",&H00200000,Array("no award",0,"3 replays",&H00200000)'dip 22
 		.AddFrame 0,138,190,"Balls per game",&H00000100,Array("5 balls",0,"3 balls",&H00000100)'dip 9
 		.AddFrame 0,184,190,"Tilt effect",&H00000800,Array("game over",0,"ball in play only",&H00000800)'dip 12
 		.AddChk 205,80,190,Array("Match feature",&H00000200)'dip 10
 		.AddChk 205,95,190,Array("Credits displayed",&H00001000)'dip 13
 		.AddChk 205,110,190,Array("Play credit button tune",&H00002000)'dip 14
 		.AddChk 205,125,190,Array("Play tones when scoring",&H00080000)'dip 20
 		.AddChk 205,140,190,Array("Play coin switch tune",&H00400000)'dip 23
 		.AddChk 205,155,190,Array("High game to date displayed",&H00100000)'dip 21
 		.AddLabel 50,240,300,20,"After hitting OK, press F3 to reset game with new settings."
 		.ViewDips
 	End With
 End Sub
 Set vpmShowDips = GetRef("editDips")

'**********************************************************************************************************
'**********************************************************************************************************
'**********************************************************************************************************

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
    tmp = tableobj.y * 2 / Dragon.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / Dragon.width-1
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
        If BOT(b).X < Dragon.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/16) + ((BOT(b).X - (Dragon.Width/2))/17))' + 13
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/16) + ((BOT(b).X - (Dragon.Width/2))/17))' - 13
        End If
        ballShadow(b).Y = BOT(b).Y + 10
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

' The sound is played using the VOL, PAN and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the PAN function will change the stereo position according
' to the position of the ball on the table.


'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they 
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.

Sub a_Triggers_Hit (idx)
	playsound "sensor", 0,1,AudioPan(ActiveBall),0,0,0,1,AudioFade(ActiveBall)
End sub

Sub a_Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub a_Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub a_DropTargets_Hit (idx)
	PlaySound "DTDrop", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
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

