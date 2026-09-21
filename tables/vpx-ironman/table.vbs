
'                                                  IRON MAN     Stern 2010
'
'               :-+*#%%@@@@@@%%#*+-:               
'          :=*%#+-*@@@@@@@@@@@@@@*=*#%*=.          
'       -*@#=.     %@@@@@@@@@@@@%     :+%@*:       
'     -@@+.        =@@@@@@@@@@@@:        :*@%:     
'    .@@:           +++++++++++=           =@@     
'    +@@:                                  +@@-    
'    %@@-                                  +@@*    
'    @@*+                                  #*@@    
'   -@@-*                                  #=@@:   
'   +@@==                                  ==@@=   
'   #@@-                                    =@@*   
'   @@@.                                    :@@#   
'  .@@% .=-:.                           .-=. @@@   
'  -@@@.*@@@@@@%#**+++========++**##%%@@@@@+.@@@:  
'  *@@+#@@+====++*@@@#%%%%@%%%#@@%++=====*@@#*@@=  
'  %@%  =#%%###*****-          -+######%%%*-  @@+  
'  %@:                                        =@+  
'  %@                                         :@+  
'  *@#.                                      :%@+  
'  -@@@=   :                            :   +@@@:  
'   #@@@%: .*=                        ++. :%@@@*   
'   .@@@@@*. -%=                    +%:  +@@@@%    
'    .%@@@@@=  +%:                -@=  .%@@@@%     
'     .@@@@@@#  :%=              +#.  +@@@@@%      
'      .@@@@@@+  .@+            #*   #@@@@@@.      
'       =@@@@@%   *@*+++++++***#@-   %@@@@@-       
'        %@@@@%%*##-::::::......:##+%@@@@@#        
'        :@@@@. ::  +          =  -- :@@@@.        
'         -%@@#=.  =@**#@@@@*+#@-   =#@@#:         
'           .-#@@%@@@@@@@@@@@@@@@@%@@*-            
'               =##################-              





'* Full playfield - plastics redraw

'* Re-Build over: Iron Man (Stern 2010)

'		* fp to vp by freneticamnesic strip and rebuild on VP 10.5 by 32assassin

' Remodeled ramps, textured using flupper1 tutorial
' Remodeled monger, added hits events
' Remodeled Flashers
' Remodel bumpers to have flashers
' New plastics models
' Changed lights for lights primities
' Rearenge lots of possition and models according to IM Vault
' 	Uses:
'			Prerenderd Insert Images 1.0.0
'            By Schlabber34


' The LUT can be cycle change Byref use left control (Left magna save)
' Targets decals can be changed


Option Explicit
Randomize

Const UseVpmModSol = True

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="im_186",UseSolenoids=1,UseLamps=0,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff", SCoin="coin"

LoadVPM "01560000", "sam.VBS", 3.10

Dim DesktopMode: DesktopMode = Table1.ShowDT
If DesktopMode = True Then 'Show Desktop components
Ramp16.visible=1
Ramp15.visible=1
'Primitive13.visible=1
Else
Ramp16.visible=0
Ramp15.visible=0
'Primitive13.visible=0
End if

dim usedeftarget
dim usewhipdeftarget
dim temx

'************* OPTIONS **************
' The LUT  use left control to change
'	Table1.ColorGradeImage = "LUT_colorplus" 
'	Table1.ColorGradeImage = "lut_orangeplus" 
'	Table1.ColorGradeImage = "Lut_Color_off"   'Darker 
'	Table1.ColorGradeImage = "lutDEF" 'default
	Table1.ColorGradeImage = "Lut_Color_darkor" 'default

		 temx =5
' Change between default or custom mini Targets(drone targets) decals
usedeftarget=0                          '1 default             0 custom 

' Change between default or custom whiplash Targets decals
usewhipdeftarget=0                      '1 default             0 custom 

if usedeftarget=1 Then
sw44.Image="target-b"
sw45.Image="target-b"
sw46.Image="target-b"
sw50.Image="target-b"
end If

if usedeftarget=0 Then
sw44.Image="target_b_4"
sw45.Image="target_b_1"
sw46.Image="target_b_2"
sw50.Image="target_b_3"
end If

if usewhipdeftarget=1 Then
sw47.Image="target_Y"
sw48.Image="target_Y"
end If

if usewhipdeftarget=0 Then
sw47.Image="target_G"
sw48.Image="target_G"
end If

'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************

SolCallback(1) = "solTrough"
SolCallback(2) = "solAutofire"
'SolCallback(3) = "MongerMagnet"
'SolCallback(4) = whiplash magnet
SolCallback(5) = "bsSaucer.SolOut"
SolCallback(6) = "orbitpost"
SolCallback(12) = "ClanePost"
SolCallback(19) = "Solmonger"
SolModCallBack(20) = "SetLamp120" 'PF light
SolModCallback(21) = "SetLamp121"
SolModCallback(22) = "SetLamp122" 'PF light
SolModCallback(23) = "SetLamp123" 'PF light
SolModCallback(25) = "SetLamp125" 'monger toy
SolModCallback(26) = "SetLamp126"
SolModCallback(27) = "SetLamp127" 'warmachine toy
SolModCallback(29) = "SetLamp129" 'wiplash toy
'SolModCallback(30) = "SetLamp130"
SolModCallback(31) = "SetLamp131"
SolModCallback(32) = "SetLamp132"

SolCallback(15) = "SolLFlipper"
SolCallback(16) = "SolRFlipper"

'**********************************************************************************************************

'Solenoid Controlled Lamps
'**********************************************************************************************************

Sub SetLamp120(m):m = m/128:S120.state = m:S120a.state = m:End Sub
Sub SetLamp121(m):m = m/128:S121a.state = m:S121b.state = m:S121c.state = m:End Sub
Sub SetLamp122(m):m = m/128:S122.state = m:End Sub
Sub SetLamp123(m):m = m/128:S123.state = m:End Sub
Sub SetLamp125(m):m = m/128:S125.state = m:S125a.state = m:S125b.state = m:S125c.state = m:primitive31.blenddisablelighting = m +0.3:End Sub 
Sub SetLamp126(m):m = m/128:S126a.state = m:S126b.state = m:S126c.state = m:End Sub
Sub SetLamp127(m):m = m/128:S127.state = m:S127a.state = m:S127b.state = m:S127c.state = m:End Sub
Sub SetLamp129(m):m = m/128:S129.state = m:S129a.state = m:S129b.state = m:S129c.state = m:End Sub
Sub SetLamp131(m):m = m/128:S131a.state = m:S131b.state = m:S131c.state = m:S131d.state = m:End Sub
Sub SetLamp132(m):m = m/128:S132a.state = m:S132b.state = m:End Sub

'**********************************************************************************************************

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors):LeftFlipper.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):LeftFlipper.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors):RightFlipper.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):RightFlipper.RotateToStart
     End If
End Sub
'**********************************************************************************************************

'Solenoid Controlled toys
'**********************************************************************************************************

Sub solTrough(Enabled)
	If Enabled Then
		bsTrough.ExitSol_On
		vpmTimer.PulseSw 22
	End If
 End Sub

Sub solAutofire(Enabled)
	If Enabled Then
		PlungerIM.AutoFire
	End If
 End Sub

Sub ClanePost(Enabled)
	If Enabled Then
		ClaneUpPost.Isdropped=false
		playsound SoundFX("Diverter",DOFContactors)
	Else
		ClaneUpPost.Isdropped=true
	End If
 End Sub

Sub orbitpost(Enabled)
	If Enabled Then
		UpPost.Isdropped=false
		playsound SoundFX("Diverter",DOFContactors)
	Else
		UpPost.Isdropped=true
	End If
 End Sub


set GICallback = GetRef("UpdateGI")
Sub UpdateGI(no, Enabled)
	If Enabled Then
		dim xx
		For each xx in GI:xx.State = 1:	Next
        PlaySound "fx_relay"
Primitive026.Image="plasticos_ver_0001"
Primitive043.Image="plasticos_top"
Primitive6.Image="Metal guides_on"
Table1.colorgradeimage = "ColorGradeLUT256x16_ConSat"
	Else 
        For each xx in GI:xx.State = 0: Next
        PlaySound "fx_relay"
Primitive026.Image="plasticos_ver_0001_D"
Primitive043.Image="plasticos_top_D"
Primitive6.Image="Metal guides_off"
Table1.colorgradeimage = "ColorGrade_4"
	End If
End Sub

'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************

Dim bsTrough, bsSaucer, Mag1, Mag2

Sub Table1_Init
InitVpmFFlipsSam
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "IronMan (Stern 2010)"&chr(13)&"MOD by Francisco666"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
        .hidden = 0
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With
     On Error Goto 0

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1
    vpmNudge.TiltSwitch=-7
    vpmNudge.Sensitivity=2
    vpmNudge.TiltObj=Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot)

    Set bsTrough = New cvpmBallStack
		bsTrough.InitSw 0, 21, 20, 19, 18, 0, 0, 0
		bsTrough.InitKick BallRelease, 90, 8
		bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
		bsTrough.Balls = 4

	Set bsSaucer = New cvpmBallStack
		bsSaucer.InitSaucer sw10, 10, 180, 20
		bsSaucer.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
		bsSaucer.KickForceVar = 2.5        

	Set mag1= New cvpmMagnet
 	With mag1
		.InitMagnet Magnet1, 22  
		.GrabCenter = False 
 		.solenoid=3
		.CreateEvents "mag1"
	End With

	Set mag2= New cvpmMagnet
 	With mag2
		.InitMagnet Magnet2, 22  
		.GrabCenter = False 
 		.solenoid=4
		.CreateEvents "mag2"
	End With

 ClaneUpPost.Isdropped = 1
 UpPost.Isdropped = 1

sw3.IsDropped = 1
sw5.IsDropped = 1
sw6.IsDropped = 1
switchframe.IsDropped = 1
monl.State=0
fmong.Visible=False
fmong001.Visible=false

End Sub

 Sub Table_Paused:Controller.Pause = 1:End Sub
 Sub Table_unPaused:Controller.Pause = 0:End Sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)
	If KeyDownHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Pullback:playsound"plungerpull"':'starttoshake

End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If KeyUpHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Fire:PlaySound"plunger"
	If Keycode = StartGameKey Then Controller.Switch(16) = 0


	If Keycode = LeftMagnaSave Then
			temx=temx+1 
			if temx=>6 Then
				temx=1
			End If
		Select Case temx
			case 1
				Table1.ColorGradeImage = "LUT_colorplus"
			case 2
				Table1.ColorGradeImage = "lut_orangeplus"
			case 3
				Table1.ColorGradeImage = "Lut_Color_off"
			case 4
				Table1.ColorGradeImage = "lutDEF"
			case 5
				Table1.ColorGradeImage = "Lut_Color_darkor" 'default
		end Select
	end If
End Sub

	Dim PlungerIM
     ' Impulse Plunger
    Const IMPowerSetting = 50
    Const IMTime = 0.6
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 0.3
        .InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
        .CreateEvents "plungerIM"
    End With

'**********************************************************************************************************

 ' Drain hole and kickers
Sub Drain_Hit:bsTrough.addball me : playsound"drain" : End Sub
Sub sw10_Hit:vpmTimer.PulseSw 10:sw10.kick 165, 26: playsound "popper":End Sub

'Wire Triggers
Sub sw7_Hit:Controller.Switch(7) = 1:PlaySound "rollover":End Sub
Sub sw7_UnHit:Controller.Switch(7) = 0:End Sub 
Sub sw9_Hit:Controller.Switch(9) = 1:PlaySound "rollover":End Sub
Sub sw9_UnHit:Controller.Switch(9) = 0:End Sub
Sub sw23_Hit:Controller.Switch(23) = 1:PlaySound "rollover":End Sub
Sub sw23_UnHit:Controller.Switch(23) = 0:End Sub 
Sub sw24_Hit:Controller.Switch(24) = 1:PlaySound "rollover":End Sub
Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub
Sub sw25_Hit:Controller.Switch(25) = 1:PlaySound "rollover":End Sub
Sub sw25_UnHit:Controller.Switch(25) = 0:End Sub
Sub sw28_Hit:Controller.Switch(28) = 1:PlaySound "rollover":End Sub
Sub sw28_UnHit:Controller.Switch(28) = 0:End Sub
Sub sw29_Hit:Controller.Switch(29) = 1:PlaySound "rollover":End Sub
Sub sw29_UnHit:Controller.Switch(29) = 0:End Sub
Sub sw38_Hit:Controller.Switch(38) = 1:PlaySound "rollover":End Sub
Sub sw38_UnHit:Controller.Switch(38) = 0:End Sub 
Sub sw39_Hit:Controller.Switch(39) = 1:PlaySound "rollover":End Sub
Sub sw39_UnHit:Controller.Switch(39) = 0:End Sub 

'Spinners
Sub sw11_Spin:vpmTimer.PulseSw 11 : playsound"fx_spinner" : End Sub
Sub sw13_Spin:vpmTimer.PulseSw 13 : playsound"fx_spinner" : End Sub
Sub sw14_Spin:vpmTimer.PulseSw 14 : playsound"fx_spinner" : End Sub

'RAmp Gate Triggers
Sub sw12_Hit:vpmTimer.PulseSw 12:End Sub
Sub sw37_Hit:vpmTimer.PulseSw 37:End Sub
Sub sw43_Hit:vpmTimer.PulseSw 43:End Sub
Sub sw49_Hit:vpmTimer.PulseSw 49:End Sub

'Bumpers
Sub Bumper1_Hit : vpmTimer.PulseSw(31) : playsound SoundFX("fx_bumper1",DOFContactors): End Sub
Sub Bumper2_Hit : vpmTimer.PulseSw(30) : playsound SoundFX("fx_bumper1",DOFContactors): End Sub
Sub Bumper3_Hit : vpmTimer.PulseSw(32) : playsound SoundFX("fx_bumper1",DOFContactors): End Sub

'Stand Up Targets
Sub sw33_Hit:vpmTimer.PulseSw 33:End Sub
Sub sw34_Hit:vpmTimer.PulseSw 34:End Sub
Sub sw35_Hit:vpmTimer.PulseSw 35:End Sub
Sub sw36_Hit:vpmTimer.PulseSw 36:End Sub
Sub sw40_Hit:vpmTimer.PulseSw 40:End Sub
Sub sw41_Hit:vpmTimer.PulseSw 41:End Sub
Sub sw42_Hit:vpmTimer.PulseSw 42:End Sub
Sub sw44_Hit:vpmTimer.PulseSw 44:End Sub
Sub sw45_Hit:vpmTimer.PulseSw 45:End Sub
Sub sw46_Hit:vpmTimer.PulseSw 46:End Sub
Sub sw47_Hit:vpmTimer.PulseSw 47:extra.Enabled = 0:whiplash=10:extra.Enabled = 1:End Sub
Sub sw48_Hit:vpmTimer.PulseSw 48:extra.Enabled = 0:whiplash=10:extra.Enabled = 1:extra_timer:End Sub
Sub sw50_Hit:vpmTimer.PulseSw 50:End Sub

  '***************************************************************************
 '    monger Animation
 '***************************************************************************

'1 monger down
'3 monger up
'4 monger left shoulder
'5 monger legs
'Sub sw5_Hit:vpmTimer.PulseSw 5:PlaySound "target":End Sub
'6 monger rt shoulder

Dim monger, mongerPos, mongerDir, mongerFlash
mongerDir = 0:mongerPos = 0:mongerFlash = 0

 Sub Solmonger(Enabled)
     If Enabled Then
         If mongerDir = 0 Then
             Controller.Switch(3) = 0
             Controller.Switch(1) = 1
             mongerClose.Enabled = 0
             mongerOpen.Enabled = 1
             mongerOpen_Timer
             mongerDir = 1
switchframe.IsDropped = 0
         Else
             Controller.Switch(1) = 0
             Controller.Switch(3) = 1
             mongerOpen.Enabled = 0
             mongerClose.Enabled = 1
             mongerClose_Timer
             mongerDir = 0
switchframe.IsDropped = 1
         End If
     End If
 End Sub
 
 Sub mongerOpen_Timer()
     Updatemonger
     mongerPos = mongerPos + 1 
     If mongerPos> 41 Then
         mongerPos = 41
         mongerOpen.Enabled = 0
     End If
 End Sub
 
 Sub mongerClose_Timer()
     Updatemonger
     mongerPos = mongerPos - 1 
     If mongerPos <0 Then
         mongerPos = 0
         mongerClose.Enabled = 0
     End If
 End Sub
 
 Sub Updatemonger
   Primitive32.Z=(-mongerPos*200/40+205)
   Primitive31.Z=(-mongerPos*18/4+185)
   Primitive2.Z=(-mongerPos*18/4+185)
fmong.Visible=False
     Select Case mongerPos
			Case 0
				'Primitive32.Z=(mongerPos*15/4+150)
				'Primitive31.Z=300
				'Primitive2.Z=90
				mongerAnim.Enabled = 0:sw3.IsDropped = 0:switchframe.IsDropped = 0:sw6.IsDropped = 0:sw5.IsDropped = 0' monger1.IsDropped = 0:monger2.IsDropped = 1:
monl.State=1
fmong.Visible=True
			Case 40
				Primitive32.Z=0
				'Primitive31.Z=(mongerPos*15/4+150)
				'Primitive2.Z=(mongerPos*15/4+90)
				mongerAnim.Enabled = 0:sw3.IsDropped = 1:switchframe.IsDropped = 1:sw5.IsDropped = 1:sw6.IsDropped = 1
monl.State=0
fmong.Visible=False
     End Select
 End Sub

dim timestoshake

sub starttoshake
         SHAKEsantatarantula.Enabled = 0
		timestoshake=10
             SHAKEsantatarantula.Enabled = 1
end Sub

 Sub SHAKEsantatarantula_Timer()
         SHAKEsantatarantula.Enabled = 0
    timestoshake=timestoshake-1
		if timestoshake=<0 Then
			TEMPSHAKE=0
			   Primitive32.X=458.0141+TEMPSHAKE
			   Primitive31.X=437.274+TEMPSHAKE
			   Primitive2.X=435.2641+TEMPSHAKE
monl.State=1
fmong.Visible=True
			   Primitive32.Y=957.2623+TEMPSHAKE
			   Primitive31.Y=850.2624+TEMPSHAKE
		Primitive2.Y=848.003+TEMPSHAKE
			exit Sub
		end If
shakemonger
             SHAKEsantatarantula.Enabled = 1
 End Sub


DIM TEMPSHAKE
DIM TEMPSHAKE2

sub shakemonger
	TEMPSHAKE=Int(Rnd*4)+1
	TEMPSHAKE2=Int(Rnd*3)+1
   Primitive32.X=458.0141+TEMPSHAKE
   Primitive31.X=437.274+TEMPSHAKE
   Primitive2.X=435.2641+TEMPSHAKE2

select case TEMPSHAKE
	case 1, 2
		monl.State=0
fmong.Visible=false
	case Else
		monl.State=1
fmong.Visible=True
end Select
   Primitive32.Y=957.2623+TEMPSHAKE2
   Primitive31.Y=850.2624+TEMPSHAKE2
   Primitive2.Y=848.003+TEMPSHAKE
end sub

Sub sw5_Hit
vpmTimer.PulseSw 5
PlaySound "target"
starttoshake
End Sub

Sub sw6_Hit
vpmTimer.PulseSw 6
PlaySound "target"
starttoshake
End Sub

Sub sw3_Hit
vpmTimer.PulseSw 3
PlaySound "target"
starttoshake
End Sub

dim whiplash
 Sub extra_Timer()
	whiplash=whiplash-1
			if whiplash=<-1 Then
				extra.Enabled = 0
				fmong001.Visible=false
				exit Sub		
			end if
		select case whiplash
			case 0,2,4,8,10
				fmong001.Visible=True
			case Else
				fmong001.Visible=false
		end Select
end Sub

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
LampTimer.Interval = 30 'lamp fading speed
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
NFadeL 3, l3
'NFadeObjm 8, Primitive114, "il_On", "il_Off"
NFadeL 4, l4
NFadeL 5, l5
NFadeL 6, l6
NFadeL 7, l7
NFadeL 8, l8
'NFadeObjm 8, Primitive089, "R_red_on", "R_red_off"
NFadeL 9, l9
NFadeL 10, l10
'NFadeObjm 10, Primitive091, "RectangularStar_red_on", "RectangularStar_red_off"
NFadeL 11, l11
'NFadeObjm 11, Primitive086, "RoundLargeredOn", "RoundLargeredOff"
NFadeL 12, l12
'NFadeObjm 12, Primitive070, "ArrowM_on", "ArrowM_off"
NFadeL 13, l13
'NFadeObjm 13, Primitive071, "ArrowM_on", "ArrowM_off"
NFadeL 14, l14
NFadeL 15, l15
'NFadeObjm 15, Primitive090, "RectangularStar_red_on", "RectangularStar_red_off"
NFadeL 16, l16
'NFadeObjm 16, Primitive104, "Roundyellowon", "Roundyellowoff"
NFadeL 17, l17
NFadeL 18, l18
NFadeL 19, l19
NFadeL 20, l20
NFadeL 21, l21
'NFadeObjm 21, Primitive094, "RectangularStar_red_on", "RectangularStar_red_off"
NFadeL 22, l22
NFadeL 23, l23
NFadeL 24, l24
NFadeL 25, l25
NFadeL 26, l26
NFadeL 27, l27
NFadeL 28, l28
'NFadeObjm 28, Primitive101, "round_monger_tinny_on", "round_monger_tinny_off"
NFadeL 29, l29
'NFadeObjm 29, Primitive100, "round_monger_tinny_on", "round_monger_tinny_off"
NFadeL 30, l30
'NFadeObjm 30, Primitive099, "round_monger_tinny_on", "round_monger_tinny_off"
NFadeL 31, l31
'NFadeObjm 31, Primitive098, "round_monger_tinny_on", "round_monger_tinny_off"
NFadeL 32, l32
'NFadeObjm 32, Primitive097, "round_monger_tinny_on", "round_monger_tinny_off"
NFadeL 33, l33
'NFadeObjm 33, Primitive096, "round_monger_tinny_on", "round_monger_tinny_off"
NFadeL 34, l34
NFadeL 35, l35
'NFadeObjm 35, Primitive092, "RectangularStar_aqua_on", "RectangularStar_aqua_off"
NFadeL 36, l36
NFadeL 37, l37
NFadeL 38, l38
NFadeL 39, l39
NFadeL 40, l40
NFadeL 41, l41
NFadeL 42, l42
NFadeL 43, l43
'NFadeObjm 43, Primitive072, "ArrowM_on", "ArrowM_off"
NFadeL 44, l44
'NFadeObjm 44, Primitive085, "il_On", "il_Off"
NFadeL 45, l45
'NFadeObjm 45, Primitive093, "RoundLargeredOn", "RoundLargeredOff"
NFadeL 46, l46
'NFadeObjm 46, Primitive095, "ro_green_On", "ro_green_Off"
NFadeL 47, l47
'NFadeObjm 47, Primitive102, "round_monger_on", "round_monger_off"
NFadeL 48, l48
'NFadeObjm 48, Primitive103, "round_Blue_on", "round_Blue_off"
NFadeL 49, l49
'NFadeObjm 49, Primitive077, "il_On", "il_Off"
NFadeL 50, l50
'NFadeObjm 50, Primitive076, "il_On", "il_Off"
NFadeL 51, l51
'NFadeObjm 51, Primitive075, "il_On", "il_Off"
NFadeL 52, l52
'NFadeObjm 52, Primitive074, "il_On", "il_Off"
NFadeL 53, l53
'NFadeObjm 53, Primitive073, "il_On", "il_Off"
NFadeL 54, l54
NFadeL 55, l55
NFadeL 56, l56
'NFadeObjm 56, Primitive087, "RoundLargeredOn", "RoundLargeredOff"
NFadeL 57, l57
NFadeL 58, l58
NFadeL 59, l59
'NFadeObjm 59, Primitive069, "ArrowM_on", "ArrowM_off"
NFadeL 60, l60 'bumper2
NFadeL 61, l61 'bumper1
NFadeL 62, l62 'bumper3
NFadeL 63, l63
'NFadeObjm 63, Primitive105, "rorangon", "rorangoff"

End Sub

' div lamp subs

Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0        ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4      ' used to track the fading state
        FlashSpeedUp(x) = 0.25   ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.1 ' slower speed when turning off the flasher
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
			a.BlendDisableLighting = alvl * a.UserValue +0.5'brightness
		Case 5
			a.UserValue = a.UserValue + 0.50
			If a.UserValue > 1 Then 
				a.UserValue = 1
				FadingLevel(nr) = 1
			end If
			a.BlendDisableLighting = alvl * a.UserValue 'brightness
	End Select
End Sub

'*********************************************************************
'                 
'*********************************************************************

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 27
    PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0:
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 26
    PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0:
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

'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle

    LFLogo.RotY = LeftFlipper.CurrentAngle
    RFlogo.RotY = RightFlipper.CurrentAngle

    sw12p.RotZ = -(sw12.currentangle)
    sw37p.RotZ = -(sw37.currentangle)
    sw43p.RotZ = -(sw43.currentangle)
    sw49p.RotZ = -(sw49.currentangle)
End Sub

'*****************************************
'	ninuzzu's	BALL SHADOW
'*****************************************
Sub BallShadowUpdate_timer()
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
        If BOT(b).Z > 20 and BOT(b).Z < 140 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
if BOT(b).z > 30 Then 
ballShadow(b).height = BOT(b).Z - 20
ballShadow(b).opacity = 110
Else
ballShadow(b).height = BOT(b).Z - 24
ballShadow(b).opacity = 90
End If
    Next	
End Sub


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
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
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall)*4, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

'Generic Sounds
Sub Trigger1_hit:PlaySound "fx_ballrampdrop":End Sub
Sub Trigger2_hit:PlaySound "fx_ballrampdrop":End Sub
Sub Trigger3_hit:PlaySound "fx_ballrampdrop":End Sub


sub Trigger001_hit:PlaySound "fx_ballrampdrop":end Sub
