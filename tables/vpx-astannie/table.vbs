Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="astannie",UseSolenoids=1,UseLamps=1,UseGI=1,SSolenoidOn="SolOn",SSolenoidOff="SolOff",SFlipperOn="fx_Flipperup",SFlipperOff="fx_Flipperdown"
Const SCoin="coin",cCredits=""

LoadVPM"01150000","GTS1.VBS",3.22


'Solenoid Call backs
'**********************************************************************************************************
SolCallback(1)="bsTrough.SolOut"
SolCallback(2)="vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
'SolCallback(3)="vpmSolSound SoundFX(""10pts"",DOFChimes),"
'SolCallback(4)="vpmSolSound SoundFX(""100pts"",DOFChimes),"
'SolCallback(5)="vpmSolSound SoundFX(""1000pts"",DOFChimes),"
SolCallback(6)="sw41exit"		

SolCallback(7)="Lraised" 	'"dt5g.SolDropUp"
SolCallback(8)="Rraised"	'"dt5d.SolDropUp"
SolCallback(17)="vpmNudge.SolGameOn"
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub Rraised(enabled)
	if enabled then Rreset.enabled=True
End Sub

Sub Rreset_timer
	dt5d.DropSol_On	
	For each light in DTRightLights: light.state=0: Next
	Rreset.enabled=false
End Sub

Sub Lraised(enabled)
	if enabled then Lreset.enabled=True
End Sub

Sub Lreset_timer
	dt5g.DropSol_On	
	For each light in DTLeftLights: light.state=0: Next
	Lreset.enabled=false
End Sub

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFx("fx_Flipperup",DOFContactors)':LeftFlipper.RotateToEnd
     Else
         PlaySound SoundFx("fx_Flipperdown",DOFContactors)':LeftFlipper.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFx("fx_Flipperup",DOFContactors)':RightFlipper.RotateToEnd
     Else
         PlaySound SoundFx("fx_Flipperdown",DOFContactors)':RightFlipper.RotateToStart
     End If
End Sub

'**********************************************************************************************************


'Primitive Flipper Code
Sub FlipperTimer_Timer
	Dim PI: PI=3.1415926
	lflip.roty = LeftFlipper.currentangle 
	rflip.roty = RightFlipper.currentangle 
	SpinnerP.Rotz = Spinner.CurrentAngle
	SpinnerRod.TransZ = sin( (Spinner.CurrentAngle+180) * (2*PI/360)) * 5
	SpinnerRod.TransX = -1*(sin( (Spinner.CurrentAngle- 90) * (2*PI/360)) * 5)
End Sub

'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************

Dim bsTrough,dt5g,dt5d, bsSaucer1
Dim BPG, hsaward, n, plungerball, xx, spinselect

Sub AsteroidAnnie_Init
	vpmInit Me
	plungerball=0
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine="Asteroid Annie (Gottlieb 1980)"
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
	vpmNudge.TiltObj=Array(Bumper1,Bumper2,LeftSlingshot,RightSlingshot)
 

	Set bsTrough=New cvpmBallStack
	bsTrough.InitSw 0,66,0,0,0,0,0,0
	bsTrough.InitKick DRAIN,60,40
	bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors),SoundFX("drainkick",DOFContactors)
 	bsTrough.Balls=1
 
  	Set bsSaucer1=New cvpmBallstack
 	bsSaucer1.InitSaucer sw41,42,125,6
	bsSaucer1.InitExitSnd SoundFX("Popper",DOFContactors),SoundFX("holekick",DOFContactors)

     set dt5g = new cvpmdroptarget
     With dt5g
        .initdrop Array(sw30,sw31,sw32,sw33,sw34), array(30,31,32,33,34)
		.InitSnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)
     End With

     set dt5d = new cvpmdroptarget
     With dt5d
        .initdrop Array(sw20,sw21,sw22,sw23,sw24), array(20, 21, 22, 23, 24)
		.InitSnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)
     End With

	FindDips		'find balls per game and high score reward

	if b2son then: for each xx in backdropstuff: xx.visible=0: next
	startGame.enabled=true

End Sub 

Sub AsteroidAnnie_Paused:Controller.Pause = 1:End Sub

Sub AsteroidAnnie_unPaused:Controller.Pause = 0:End Sub 

Sub AsteroidAnnie_Exit
	If b2son then controller.stop
End Sub

sub startGame_timer
	dim xx
	playsound "poweron"
	For each xx in GILights:xx.State = 1: Next		'*****GI Lights On
	me.enabled=false
end sub


'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************
 Sub AsteroidAnnie_KeyDown(ByVal keycode)
	If keycode=rightmagnasave then
		spinselect=spinselect+1
		if spinselect>2 then spinselect=0
		SpinnerP.image="AnnieSpinner"&spinselect
	End if
	If GOBox.text="" and TILTBox.text="" then 
		If keycode = LeftFlipperKey Then LeftFlipper.RotateToEnd
		If keycode = RightFlipperKey Then RightFlipper.RotateToEnd
	End If
	If vpmKeyDown(KeyCode) Then Exit Sub
	If keycode=AddCreditKey then playsound "coin": vpmTimer.pulseSW (swCoin1): end if
	If keycode=PlungerKey Then Plunger.Pullback:playsound"plungerpull"

End Sub

Sub AsteroidAnnie_KeyUp(ByVal keycode)
	If keycode = LeftFlipperKey Then LeftFlipper.RotateToStart
   	If keycode = RightFlipperKey Then RightFlipper.RotateToStart
	
	If keycode = 61 then FindDips
	If vpmKeyUp(KeyCode) Then Exit Sub
	If keycode=PlungerKey Then 
		Plunger.Fire
		if plungerball=1 then
			playsound"plunger"
		  else
			playsound"plungerreleasefree"
		end if
	end if
End Sub

Sub Ballhome_hit
	plungerball=1
end sub

sub ballhome_unhit
	plungerball=0
end sub

Sub Drain_Hit:playsound"drain":bsTrough.addball me:End Sub


'bumpers
	Sub Bumper1_Hit:vpmTimer.PulseSw 40 : playsound SoundFXDOF("fx_bumper4",103,DOFPulse,DOFContactors): End Sub
	Sub Bumper2_Hit:vpmTimer.PulseSw 40 : playsound SoundFXDOF("fx_bumper4",104,DOFPulse,DOFContactors): End Sub

'Hit Target

	Sub sw44_Hit:vpmTimer.PulseSw 44:End Sub


'Rollover wire Tirggers

	  Sub sw11_Hit:Controller.Switch(11) = 1:PlaySound "sensor":End Sub
	  Sub sw11_UnHit:Controller.Switch(11) = 0:End Sub
	  Sub sw12_Hit:Controller.Switch(12) = 1:PlaySound "sensor":End Sub
	  Sub sw12_UnHit:Controller.Switch(12) = 0:End Sub
	  Sub sw42_Hit:Controller.Switch(42) = 1:PlaySound "sensor":End Sub
	  Sub sw42_UnHit:Controller.Switch(42) = 0:End Sub
	  Sub sw50_Hit:Controller.Switch(50) = 1:PlaySound "sensor":End Sub
	  Sub sw50_UnHit:Controller.Switch(50) = 0:End Sub
	  Sub sw14_Hit:Controller.Switch(14) = 1:PlaySound "sensor":End Sub
	  Sub sw14_UnHit:Controller.Switch(14) = 0:End Sub
	  Sub sw13_Hit:Controller.Switch(13) = 1:PlaySound "sensor":End Sub
	  Sub sw13_UnHit:Controller.Switch(13) = 0:End Sub
	  Sub sw51_Hit:Controller.Switch(51) = 1:DOF 105, 1:PlaySound "sensor":End Sub
	  Sub sw51_UnHit:DOF 105, 0:Controller.Switch(51) = 0:End Sub


		 
'Scoring rubbers

	Sub DingwallA_hit()
		vpmTimer.PulseSw 43
		SlingA.visible=0
		SlingA1.visible=1
		me.uservalue=1
		Me.timerenabled=1
	End Sub

	sub dingwalla_timer								
		select case dingwalla.uservalue
			Case 1: SlingA1.visible=0: SlingA.visible=1
			case 2:	SlingA.visible=0: SlingA2.visible=1
			Case 3: SlingA2.visible=0: SlingA.visible=1: Me.timerenabled=0
		end Select
		me.uservalue=me.uservalue+1
	end sub

	Sub DingwallB_hit()
		vpmTimer.PulseSw 43
		SlingB.visible=0
		SlingB1.visible=1
		me.uservalue=1
		Me.timerenabled=1
	End Sub

	sub dingwallb_timer									'default 50 timer
		select case DingwallB.uservalue
			Case 1: Slingb1.visible=0: SlingB.visible=1
			case 2:	SlingB.visible=0: Slingb2.visible=1
			Case 3: Slingb2.visible=0: SlingB.visible=1: Me.timerenabled=0
		end Select
		DingwallB.uservalue=DingwallB.uservalue+1
	end sub

'Kickers

	 Sub sw41_Hit
		controller.switch(41) = 1
	 End Sub

	Dim rkickstep

	 Sub sw41exit(enabled)
	  if enabled then
		controller.switch(41) = 0
		sw41.kick 250,10
		playsound SoundFX("holekick",DOFContactors)
		PkickarmR.rotz=15
		rkickstep=1
		sw41.timerenabled=1
	  end if
	end Sub

	sub sw41_timer
		Select Case rkickstep
		  Case 2:
			PkickarmR.rotz=0
			me.timerenabled=0
		End Select
		rkickstep=rkickstep+1
	end Sub

'Drop Targets

	  Sub sw30_dropped:dt5g.Hit 1: Lsw30.state=1:End Sub
	  Sub sw31_dropped:dt5g.Hit 2: Lsw31.state=1:End Sub
	  Sub sw32_dropped:dt5g.Hit 3: Lsw32.state=1:End Sub
	  Sub sw33_dropped:dt5g.Hit 4: Lsw33.state=1:End Sub
	  Sub sw34_dropped:dt5g.Hit 5:End Sub

	  Sub sw20_dropped:dt5d.Hit 1:Lsw20.state=1:End Sub
	  Sub sw21_dropped:dt5d.Hit 2:Lsw21.state=1:End Sub
	  Sub sw22_dropped:dt5d.Hit 3:Lsw22.state=1:End Sub
	  Sub sw23_dropped:dt5d.Hit 4:End Sub
	  Sub sw24_dropped:dt5d.Hit 5:End Sub


'Lamp driven drop target reset
 Dim N1,O1, Light
 N1=0:O1=0:
 Set LampCallback=GetRef("UpdateMultipleLamps")
 
Sub UpdateMultipleLamps

	N1=Controller.Lamp(1) 'Game Over triggers match and BIP
	If N1 Then
		BIPBox.text="BALL IN PLAY"
		NTMBox.text=""
		GOBox.text=""
	  Else
		BIPBox.text=""
		NTMBox.text="NUMBER TO MATCH"
		GOBox.text="GAME OVER"
	End If

	N1=Controller.Lamp(2) 'Tilt
	If N1 Then
		TILTBox.text="TILT"
	  Else
		TILTBox.text=""
	End If

	N1=Controller.Lamp(4) 'Shoot Again
	If N1 Then
		ShootAgainBox.text="SHOOT AGAIN"
	  Else
		ShootAgainBox.text=""
	End If

	N1=Controller.Lamp(3) 'HIGH SCORE TO DATE
	If N1 Then
		HStoDateBox.text="HIGH SCORE TO DATE"
	  Else
		HStoDateBox.text=""
	End If
 End Sub
 


'**********************************************************************************************************
 
'Map lights to an array
'**********************************************************************************************************
'Set Lights(1) = l1 'Game Over Backbox
'Set Lights(2) = l2 'Tilt Backbox
'Set Lights(3) = l3 'High Score Backbox
Set Lights(4) = l4 ' Shoot Again Playfied and backglass
Set Lights(5) = l5
Set Lights(6) = l6
Set Lights(7) = l7
Set Lights(8) = l8
Set Lights(9) = l9
Set Lights(10) = l10
Set Lights(11) = l11
Set Lights(12) = l12
Set Lights(13) = l13
Set Lights(14) = l14
Set Lights(15) = l15
Set Lights(16) = l16
Set Lights(17) = l17
Set Lights(18) = l18
Set Lights(19) = l19
Set Lights(20) = l20
Set Lights(21) = l21
Set Lights(22) = l22
Set Lights(23) = l23
Set Lights(24) = l24
Set Lights(25) = l25
Set Lights(26) = l26
Set Lights(27) = l27
Set Lights(28) = l28
Set Lights(29) = l29
Set Lights(30) = l30
Set Lights(31) = l31
Set Lights(32) = l32
Set Lights(33) = l33
Set Lights(34) = l34
Set Lights(35) = L35
Set Lights(36) = l36


Dim Digits(32)
Digits(0)=Array(a1,a2,a3,a4,a5,a6,a7,n,a8)
Digits(1)=Array(a9,a10,a11,a12,a13,a14,a15,n,a16)
Digits(2)=Array(a17,a18,a19,a20,a21,a22,a23,n,a24)
Digits(3)=Array(a25,a26,a27,a28,a29,a30,a31,n,a32)
Digits(4)=Array(a33,a34,a35,a36,a37,a38,a39,n,a40)
Digits(5)=Array(a41,a42,a43,a44,a45,a46,a47,n,a48)
Digits(6)=Array(b00,b01,b02,b03,b04,b05,b06,n,b08)
Digits(7)=Array(b10,b11,b12,b13,b14,b15,b16,n,b18)
Digits(8)=Array(b20,b21,b22,b23,b24,b25,b26,n,b28)
Digits(9)=Array(b30,b31,b32,b33,b34,b35,b36,n,b38)



Sub DisplayTimer_Timer
	Dim ChgLED,ii,num,chg,stat,obj
	ChgLed = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
	If Not IsEmpty(ChgLED) Then
		If NOT B2Son Then
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

'Finding an individual dip state based on scapino's Strikes and spares dip code - from unclewillys pinball pool, added another section to get high score award to set replay cards
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
	If BPG = 1 then 
		instcard.image="InstCard3Balls"
	  Else
		instcard.image="InstCard5Balls"
	End if
	replaycard.image="replaycard"&hsaward
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



'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 43
    PlaySound SoundFXDOF("right_slingshot",102,DOFPulse,DOFContactors), 0, 1, 0.05, 0.05
    RSling.Visible = 0
    RSling1.Visible = 1
    slingR.objroty = -15
    RStep = 1
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:slingR.objroty = -7
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:slingR.objroty = 0:RightSlingShot.TimerEnabled = 0:
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 43
    PlaySound SoundFXDOF("left_slingshot",101,DOFPulse,DOFContactors),0,1,-0.05,0.05
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



' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "AsteroidAnnie" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / AsteroidAnnie.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
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
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*10, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0
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
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0
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


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub a_Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub a_Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub a_Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Spinner_Spin
	vpmTimer.PulseSw 10
	PlaySound "fx_spinner",0,.25,0,0.25
End Sub

Sub a_Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub a_Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
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
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub