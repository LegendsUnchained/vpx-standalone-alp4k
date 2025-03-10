
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
                                                                                                                                                 
' ==================      =================         ============  ==========             =========+============    *=+++++++=    *=++++++++===    
' +=*#######**#####***+=  =+*##############**+=     +**#######*## #**#######+=         =+*######**##**######*==  ####**###*++ *=**##*****#####++  
'   =+####*######**#####**# =*####*#####**####*=     ##*####*#### ###*#######+=       =+*######+######*####*+   #######*#*+###*###*#######*##+=   
'   =+####*#######*+#####*###***##*######**####**#    =+####*##     **###*##*#*=     =+#####**#==  ###*##**++      ####+**###**##*+*########+     
'   =+****+#####   =*******##*****+###  +++*****###   =+****+##     ++**********=+  =**+**+****+=  ###+****+       ####+**###*******=+####        
'   =++++++#####  ==++++++###**++++### ++++++++*###   =+**+++##     +++++#*++++++=+=*+++##+++++==  ###++++++       ####+*+####++*+*++++==*        
'   =====+=####*-+=+====+####+=++==++==++=+==*####    ==++++=##     ==+=- ##++++====+=####==+++==  ###=+++==       ####=++*#####=++++++=+++==     
'   ===========-======######*=====-========+####      -======##     ====-  ##+=+==-==  ##*=======  ###======       ####====%#######=============  
'   ===-===###############  +=-=-==###=-----=##       --====-##     =-=--   ##+=--=*   ###=--=-==  ###-===-=       ####-==+   ########*=====--==+ 
'   ===---=#####            +=-----###-==-=--=##      -----=-##     +----     ##==#    ##*-----==  ###-=----       ###*--=+       #######*------= 
'   ==-----#####            =-----=### ==------*#     =------##     +----      ##      ##*-----==  ###-----==      ###=--== ###+==   #####*-----= 
'   =------*####            =------###  ==------=##   =------*#     +----              ##*-----=+  ###+------===**##*----=####=---==*####*=----=  
' ++==++++=-=*####        ==--------=###  =-------=+==---------=  =------==           +==--------== ####+=-------------=+####+--------=------==   
' ++**#####*#######       =+*############  ++*###**############## *******###         ###****##*****   #####*++=======    #########**++====+*      
                                                                                                                                                                                                                                                                                                             
                                                                                                                                                        
' Stern 2018

 
' This is a rework of Whoa Nellie - Hauntfreaks and allknowing and whoever else had their hands in it.
' 4 players added by jpsalas
' Dof by arngrim
' jpsalas:  4 players added
' arngrim:  DOF
' STAT:     4 Player directb2s Code Optimization
' MachWon:  Playfeild/inserts graphics cleanup,lighting corrected,fasteners added, sounds added/corrected, script corrections and nFozzy/Flipper Tricks added


' ZFLP: FLIPPERS
' ZTSS: TRIGGERS & SWITCHES
' ZKHS: KICKER HOLE SHOT
' ZSST: SLING SHOT TRIGGERS 
' ZULR: UPPER LEFT ROLLOVER SWITCHES 
' ZT13: SKILL SHOT SWITCH T13 
' ZSBS: SUPPORTING BALL & SOUND FUNCTIONS
' ZJPS: JP's VP10 COLLISION & ROLLING SOUNDS
' ZPSP: POSITIONAL SOUND PLAYBACK FUNCTIONS
' ZNEF: FLIPPER CORRECTION
' ZPHY: NFOZZY/ROTH PHYSICS
' ZCOR: FLIPPER CORRECTION FUNCTIONS
' ZPOL: FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS
' ZFLP: FLIPPER TRICKS
' ZDMP: RUBBER DAMPENERS
' ZTAC: TRACK ALL BALL VELOCITIES
' ZBOU: VPW TARGET BOUNCER FOR TARGET AND POSTS
' ZSSC: SLINGSHOT CORRECTION FUNCTIONS


Option Explicit
SetLocale 1033
Randomize
'///////////////////////----- Game Play Options -----///////////////////////
BallsPerGame = 5       ' Balls Per Game
StandardMode = False   ' True = Harder: True means you have to hit top 4 Gates twice for "Plus" award scoring..
SVol = .4    		   ' Callouts Volume (Max 1.0)
TVol = .8   		   ' Background Track Volume (Not Used)
CVol = .9   		   ' Bell Volume (Max 1.0)

Const cGameName = "primus"	'The unique alphanumeric name for this table
Const UsingROM = False		'The UsingROM flag is to indicate code that requires ROM usage. Mostly for instructional purposes only.
Const BallSize = 50			'Ball diameter in VPX units; must be 50
Const BallMass = 1			'Ball mass must be 1
Const tnob = 2				'Total number of balls the table can hold
Const lob = 0				'Locked balls
Const VolDiv = 250         'Lower number, louder ballrolling/collition sound

'///////////////////////----- Sound Volume -----///////////////////////

Const VolBump   = .5    ' Bumpers volume: Higher Number Higher Volume
Const VolGates  =  1    ' Gates volume.
Const VolFlip   = .3    ' Flipper volume.
Const VolTrig   = .5    ' Trigger volume.
Const VolRol    =  1    ' Rollover volume.
Const VolRH     = .3    ' Rubber volume

Const MaxPlayers = 4

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0
 
Dim KeyCodes
Dim HiScore
Dim GameOn
Dim Players
Dim UpBall
Dim Ball
Dim UpLight(4)
Dim Score(4)
Dim TempScore(4)
Dim Credit
Dim Credit1(4)
Dim Credit2(4)
Dim Tilted
Dim Tilts
Dim Up, pauseloop
Dim x, i, lno
Dim y, attractmodeflag, LFlag, RFlag
Dim BonusLights
Dim z
 
Dim BallsPerGame, StandardMode
 
Dim Controller
Dim DOFs
Dim B2SOn
Dim Req(220)
Dim rpos
Dim MusicFile, SVol,CVol,TVol  

Dim HiSc, Object
Dim Initial(4)
Dim HSInitial0, HSInitial1, HSInitial2
Dim EnableInitialEntry
Dim HSi,HSx, HSArray, HSiArray
Dim RoundHS, RoundHSPlayer
Dim ScoreMil, Score100K, Score10K, ScoreK, Score100, Score10, ScoreUnit
HSArray = Array("HS_0","HS_1","HS_2","HS_3","HS_4","HS_5","HS_6","HS_7","HS_8","HS_9","HS_Space","HS_Comma")
HSiArray = Array("HSi_0","HSi_1","HSi_2","HSi_3","HSi_4","HSi_5","HSi_6","HSi_7","HSi_8","HSi_9","HSi_10","HSi_11","HSi_12","HSi_13","HSi_14","HSi_15","HSi_16","HSi_17","HSi_18","HSi_19","HSi_20","HSi_21","HSi_22","HSi_23","HSi_24","HSi_25","HSi_26")
Dim MatchNumber, Balls, Shift

'*******************************************
'	         Callouts/Sounds
'*******************************************
 
Req(26)="" ' background music 
Req(28)="091 " ' intro
Req(30)="0bc" ' major award
Req(31)="0be" ' take a picture
Req(32)="05e" ' high score
Req(36)="0b706d065066067" 'ball 1 start
Req(38)="065060067100" ' song intro for ball 2
Req(39)="06c084085086" ' song introfor ball 3 or 4
Req(40)="0c5" ' song intro for ball 5
Req(41)="021" ' ball out
'54 bell
'55 cowbell
'56 high bell
Req(62)="0b70930cf0db06d06e06f095096097" ' game start
Req(78)="0b3" ' player not playing
Req(80)="0c0"
Req(81)="0c1"
Req(82)="0be"
Req(83)="0b4"
Req(84)="0be"
Req(85)="0b4"
Req(86)="0c8"
Req(87)="0fe"
Req(88)="0c8"
Req(90)="02b02c" ' ball 5 drain 
Req(91)="0da0ad0aa0f70ae" ' end of game
Req(93)="0ce0cf06c0fc0fb0fa0f90a60900a808f08b08a0910990a00a10a20a30a70aa0a40a80a90ab"  ' attract
Req(97)="0b30e60e50e40e8" ' tilt warn
Req(99)="02c" ' tilt sound
Req(100)="0fa0ae0ac" ' tilt
Req(104)="0c0"
Req(114)="08c08f" 
Req(115)="0960fd0690680cd"  ' in lane trigger
Req(116)="05c05d06a06b0870880890fb0e10e20e30e40e50e60e7"  ' center lane trigger
Req(117)="0800810820830ac0ae0af0b00b10b20b30b50b60bb0c40cb0dc0dd0de0df0f00f10f20f30f40f50f60f70f8" ' outside lanes
Req(119)="0e7"  ' Trigger 14
Req(120)="030" ' top lane triggers 
Req(121)="0a6"
Req(126)="098"
Req(127)="0b8"
Req(135)="038037" 'splat
Req(143)="0cc0bc0c20c30c70c90ca" ' target hits
Req(147)="0b408d0be09c" ' skill shot
Req(154)="038037" ' light triggers
Req(166)="405"
Req(195)="022024025" 'kicker
Req(197)="08e09c" ' inside lane
Req(198)="09a09b"
Req(199)="0a509f" 
Req(200)="09d"

'*******************************************

 
Set UpLight(1) = Up1
Set UpLight(2) = Up2
Set UpLight(3) = Up3
Set UpLight(4) = Up4

'*******************************************
'	            ZTIM: Timers
'*******************************************

'The FrameTimer interval should be -1, so executes at the display frame rate
'The frame timer should be used to update anything visual, like some animations, shadows, etc.
'However, a lot of animations will be handled in their respective _animate subroutines.

Dim FrameTime, InitFrameTime
InitFrameTime = 0

FrameTimer.Interval = -1
Sub FrameTimer_Timer() 'The frame timer interval should be -1, so executes at the display frame rate
	FrameTime = GameTime - InitFrameTime
	InitFrameTime = GameTime	'Count frametime
	'Add animation stuff here
	'RollingUpdate   		'update rolling sounds
	'DoDTAnim				'handle drop target animations
	'DoSTAnim				'handle stand up target animations
	'queue.Tick	      		'handle the queue system
	'BSUpdate
End Sub

'The CorTimer interval should be 10. It's sole purpose is to update the Cor calculations
CorTimer.Interval = 10
Sub CorTimer_Timer(): Cor.Update: End Sub

Sub table1_init()
  LoadEM


  Score(1) = 0
  Score(2) = 0
  Score(3) = 0
  Score(4) = 0
  Up = 1
  lno = 0 ' lamp seq flashing while in trough
   
  Hisc = 1000	
  Initial(1) = 1: Initial(2) = 1: Initial(3) = 1
  LoadHighScore
  UpdatePostIt
  CreditReel.SetValue Credit
 
    If ShowDT = True Then
        For each object in backdropstuff
        Object.visible = 1  
        Next
    End If
   
    If ShowDt = False Then
        For each object in backdropstuff
        Object.visible = 0  
        Next
    End If
 
  If B2SOn Then
    Controller.B2SSetCredits Credit
    Controller.B2SSetGameOver 1
  End If
 
  TriggerLightsOff()
 
  AttractMode.Interval=500
  AttractMode.Enabled = True
  attractmodeflag=0
  AttractModeSounds.Interval=7000
  AttractModeSounds.Enabled = True
 
  If Credit > 0 Then DOF 121,1
End Sub

'***************************************************
' 	ZMAT: General Math Functions - ROBBY K addition
'***************************************************
' These get used throughout the script. 

Dim PI
PI = 4 * Atn(1)

Function dSin(degrees)
	dsin = Sin(degrees * Pi / 180)
End Function

Function dCos(degrees)
	dcos = Cos(degrees * Pi / 180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		End If
	ElseIf dx = 0 Then
		If dy = 0 Then
			Atn2 = 0
		Else
			Atn2 = Sgn(dy) * pi / 2
		End If
	End If
End Function

Function ArcCos(x)
	If x = 1 Then
		ArcCos = 0/180*PI
	ElseIf x = -1 Then
		ArcCos = 180/180*PI
	Else
		ArcCos = Atn(-x/Sqr(-x * x + 1)) + 2 * Atn(1)
	End If
End Function

Function max(a,b)
	If a > b Then
		max = a
	Else
		max = b
	End If
End Function

Function min(a,b)
	If a > b Then
		min = b
	Else
		min = a
	End If
End Function

' Used for drop targets
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy) 'Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
	Dim AB, BC, CD, DA
	AB = (bx * py) - (by * px) - (ax * py) + (ay * px) + (ax * by) - (ay * bx)
	BC = (cx * py) - (cy * px) - (bx * py) + (by * px) + (bx * cy) - (by * cx)
	CD = (dx * py) - (dy * px) - (cx * py) + (cy * px) + (cx * dy) - (cy * dx)
	DA = (ax * py) - (ay * px) - (dx * py) + (dy * px) + (dx * ay) - (dy * ax)
	
	If (AB <= 0 And BC <= 0 And CD <= 0 And DA <= 0) Or (AB >= 0 And BC >= 0 And CD >= 0 And DA >= 0) Then
		InRect = True
	Else
		InRect = False
	End If
End Function

Function InRotRect(ballx,bally,px,py,angle,ax,ay,bx,by,cx,cy,dx,dy)
	Dim rax,ray,rbx,rby,rcx,rcy,rdx,rdy
	Dim rotxy
	rotxy = RotPoint(ax,ay,angle)
	rax = rotxy(0) + px
	ray = rotxy(1) + py
	rotxy = RotPoint(bx,by,angle)
	rbx = rotxy(0) + px
	rby = rotxy(1) + py
	rotxy = RotPoint(cx,cy,angle)
	rcx = rotxy(0) + px
	rcy = rotxy(1) + py
	rotxy = RotPoint(dx,dy,angle)
	rdx = rotxy(0) + px
	rdy = rotxy(1) + py
	
	InRotRect = InRect(ballx,bally,rax,ray,rbx,rby,rcx,rcy,rdx,rdy)
End Function

Function RotPoint(x,y,angle)
	Dim rx, ry
	rx = x * dCos(angle) - y * dSin(angle)
	ry = x * dSin(angle) + y * dCos(angle)
	RotPoint = Array(rx,ry)
End Function

'********************************************* 

Sub Table1_KeyDown(ByVal keycode)

    If EnableInitialEntry = True Then EnterIntitals(keycode)

    If keycode = PlungerKey Then
        Plunger.PullBack:PlaySoundAt "fx_plungerpull",Plunger
    End If
 
    If keycode = LeftFlipperKey Then
      If GameOn = TRUE then
        If Tilted = FALSE then
			SolLFlipper True	'This would be called by the solenoid callbacks if using a ROM
			'LeftFlipper.RotateToEnd
			'flipperLtimer.interval=10
			'flipperLtimer.Enabled = TRUE
			PlaySound SoundFX("FlipUpL", DOFFlippers), 0, VolFlip, Pan(LeftFlipper), 0, 2000, 0, 1, AudioFade(LeftFlipper)
			DOF 101, 1 			
				
        End If
      End If
    End If
  
    If keycode = RightFlipperKey Then
      If GameOn = TRUE then
        If Tilted = FALSE then
			SolRFlipper True	'This would be called by the solenoid callbacks if using a ROM
			'RightFlipper.RotateToEnd
			'flipperRtimer.interval=10
			'flipperRtimer.Enabled = TRUE
			PlaySound SoundFX("FlipUpR", DOFFlippers), 0, VolFlip, Pan(RightFlipper), 0, -2000, 0, 1, AudioFade(RightFlipper)
			DOF 102, 1			
			' PlayAllReq(117)
        End If
      End If
    End If

'********************************************
'            Music Control Keys
'********************************************

	If keycode = RightMagnaSave Then       
        MusicOn()
    End If
    If keycode = LeftMagnaSave  Then 
		EndMusic
End If

'*********************************************

    If KeyCode = "20" Then
		'Tilted = 'TRUE'
		TiltCheck
	End If


    If keycode = LeftTiltKey Then
        Nudge 90, 3
        TiltCheck
    End If
   
    If keycode = RightTiltKey Then
        Nudge 270, 3
        TiltCheck
    End If
   
    If keycode = CenterTiltKey Then
        Nudge 0, 3
        TiltCheck
    End If
 
   if keycode = 6 then   ' Add coin
          if Credit < 9 then
            Credit = Credit + 1
            DOF 121, 1
            PlaysoundAt "coin3",Drain
        end if
        CreditReel.SetValue Credit
        If B2SOn Then
            Controller.B2SSetCredits Credit
        End If
        savehs
    end if
 
   if keycode = 2 then
        If EnableInitialEntry = False Then
       PlaySound "buzz"  'Play a sound for no credits
' Do not add players if Credit = 0
        if Credit > 0 then
          AttractMode.Enabled = FALSE
          AttractModeSounds.Enabled=FALSE
          StopSound MusicFile
          MyLightSeq.StopPlay()
' Do Reset Sequence if Players = 0
           If GameOn = FALSE and Players = 0 then
              StartGame
              Exit Sub
           End If  
' Add Player if Game not started yet
             If Players < MaxPLayers and Score(1) = 0 AND Credit > 0 then
                Players = Players + 1
                Credit = Credit - 1
                If Credit < 1 Then DOF 121, 0
                CreditReel.SetValue Credit
                Playsound "newplayer"
                If B2SOn Then
                    Controller.B2SSetCredits Credit
					if Players > 1 Then
						Controller.B2SSetPlayerUp 1
					End If
                End If
                savehs
                End If
           End If  
        end if
        End If
End Sub
 
'*************************************************
 
Sub BallInLane_timer
    PlayReq(78)
    BallInLane.Interval=10000+INT(5000*RND)
End Sub

  
SUB Table1_KeyUp(ByVal keycode)

    If keycode = PlungerKey Then
        Plunger.Fire
    PlaySoundAt "fx_plunger",Plunger
    'PlayReq(41)
    End If
    If keycode = LeftFlipperKey Then
		If GameOn = TRUE then
			SolLFlipper False 
			PlaySoundAtVol SoundFX("FlipDownL", DOFFlippers),LeftFlipper,VolFlip
			StopSound "FlipUpL"
            DOF 101, 0		
		End If
    End If
   
    If keycode = RightFlipperKey Then
		If GameOn = TRUE then
			SolRFlipper False  
			PlaySoundAtVol SoundFX("FlipDownR", DOFFlippers),RightFlipper,VolFlip
			StopSound "FlipUpR"
			DOF 102, 0			
		End If
    End If

END SUB
 
Sub TriggerLightsOff
  For each i in BumperLights: i.State = LightStateOff: Next
  For each i in TableLights: i.State = LightStateOff: Next
End Sub
 
'GI Lights'
Sub GI_On
  For each i in GI: i.State = LightStateOn: Next
End Sub
 
Sub GI_Off
  For each i in GI: i.State = LightStateOff: Next
End Sub
 
Sub Bumper1_On
  b1=1:b1l1.state = LightStateOn
  b1l2.state = LightStateOn
  b1l3.state = LightStateOn
End Sub
Sub Bumper1_Off
  b1=0:b1l1.state = LightStateOff
  b1l2.state = LightStateOff
  b1l3.state = LightStateOff
End Sub
Sub Bumper2_On
  b2=1:b2l1.state = LightStateOn
  b2l2.state = LightStateOn
  b2l3.state = LightStateOn
End Sub
Sub Bumper2_Off
  b2=0:b2l1.state = LightStateOff
  b2l2.state = LightStateOff
  b2l3.state = LightStateOff
End Sub
Sub Bumper3_On
  b3=1:b3l1.state = LightStateOn
  b3l2.state = LightStateOn
  b3l3.state = LightStateOn
End Sub
Sub Bumper3_Off
  b3=0:b3l1.state = LightStateOff
  b3l2.state = LightStateOff
  b3l3.state = LightStateOff
End Sub
Sub Bumper4_On
  b4=1:b4l1.state = LightStateOn
  b4l2.state = LightStateOn
  b4l3.state = LightStateOn
End Sub
Sub Bumper4_Off
  b4=0:b4l1.state = LightStateOff
  b4l2.state = LightStateOff
  b4l3.state = LightStateOff
End Sub
Sub Bumper5_On
  b5=1:b5l1.state = LightStateOn
  b5l2.state = LightStateOn
  b5l3.state = LightStateOn
End Sub
Sub Bumper5_Off
  b5=0:b5l1.state = LightStateOff
  b5l2.state = LightStateOff
  b5l3.state = LightStateOff
End Sub


'******************************* 
'        Tilt Routine
'*******************************

Sub TiltCheck
   If Tilted = FALSE then
      If Tilts = 0 then
         Tilts = Tilts + int(rnd*100)
         TiltTimer.Enabled = TRUE
      Else  
         Tilts = Tilts + int(rnd*100)
      End If  
      If Tilts >= 225 and Tilted = FALSE then
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        Bumper1.Force=0
        Bumper2.Force=0
        Bumper3.Force=0
        Bumper4.Force=0
        Bumper5.Force=0
 
        SkillShotOff()
        If LampSplash.enabled=True then ' if you tilt while in plunger lane
          l1.state=s1:l2.state=s2:l3.state=s3:l4.state=s4
          LampSplash.enabled = False  'Turn off the 4 light attract mode
          BallInLane.Enabled=False
        End If
        Tilted = TRUE
        TiltTimer.Enabled = FALSE
        TiltBox.Text = "TILT"
        TiltBoxa.Text = "TILT"
          TiltGI.interval=7000
          TiltGI.enabled=TRUE
        GI_Off()
        EndMusic
        PlayReq(100)
        PlayReq(99)
        If B2SOn Then
            Controller.B2SSetTilt 33.1
            Controller.B2SSetdata 1,0
        End If
      Else
        if Tilts > 80 then   ' tilt warn
          PlayReq(97)
        End if
      End If
   End If

End Sub


'*********************************
'         Tilt Timers
'*********************************
 
Sub TiltGI_timer()
  TiltGI.enabled=FALSE
  GI_On()
  MusicOn()
End Sub
 
Sub TiltTimer_Timer()
    If Tilts > 0 then
       Tilts = Tilts - 1
    Else
       TiltTimer.Enabled = FALSE
    End If
End Sub

'*********************************
'         Music Routine
'*********************************

Sub MusicOn
Dim musicEnd
Dim z
z = INT(15 * RND(1) )
Select Case z
Case 0:PlayMusic "primus\primus1.wav"
Case 1:PlayMusic "primus\primus2.wav"
Case 2:PlayMusic "primus\primus3.wav"
Case 3:PlayMusic "primus\primus4.wav"
Case 4:PlayMusic "primus\primus5.wav"
Case 5:PlayMusic "primus\primus6.wav"
Case 6:PlayMusic "primus\primus7.wav"
Case 7:PlayMusic "primus\primus8.wav"
Case 8:PlayMusic "primus\primus9.wav"
Case 9:PlayMusic "primus\primus10.wav"
Case 10:PlayMusic "primus\primus11.wav"
Case 11:PlayMusic "primus\primus12.wav"
Case 12:PlayMusic "primus\primus13.wav"
Case 13:PlayMusic "primus\primus14.wav"
Case 14:PlayMusic "primus\primus15.wav"
End Select
End Sub

Sub Table1_MusicDone()
    MusicOn
End Sub

'********************************
'       Start Of Game 
'********************************

Sub StartGame
    PlayReq(62)
    MusicOn()
    Players = 1
    Up = 1
    UpLight(1).State = LightStateOn
    Credit = Credit - 1
    If Credit < 1 Then DOF 121, 0
    CreditReel.SetValue Credit
    If B2SOn Then
        Controller.B2SSetCredits Credit
    End If
    savehs

    Clear_Match

    Bumper1.Force=5  ' reset Bumpers after a tilt
    Bumper2.Force=5
    Bumper3.Force=5
    Bumper4.Force=5
    Bumper5.Force=0
 
    LFlag=0:RFlag=0
 
    GI_On
    Tilted = FALSE
    Tilts = 0
    'TiltBox.Text = ""
    If B2SOn Then
        Controller.B2SSetTilt 0
        Controller.B2SSetdata 1,1
    End If
    GameOn = TRUE
    BallReel.SetValue 1
    If B2SOn Then
        Controller.B2SSetBallInPlay Ball
    End If
    For x = 1 to 2
      Score(x) = 0
      Credit1(x) = FALSE
      Credit2(x) = FALSE
      If B2SOn Then
        Controller.B2SSetGameOver 0
      End If
    NEXT
    EMReel1.ResettoZero
    PlaySound "initialize"
    ResetTimer.Enabled = TRUE
    If B2SOn Then
        Controller.B2SSetScorePlayer 1,0
    End If
End Sub


Sub ResetTimer_Timer()
    ResetTimer.Enabled = FALSE
    Ball = 1
    Bumper1_Off:Bumper2_Off:Bumper3_Off:Bumper4_Off:Bumper5_On
 
      ' turn on target lights
      L5.State = LightStateOn   'Yellow Light
 
    L11.State=LightStateOn:L12.State=LightStateOn:L13.State=LightStateOn:L14.State=LightStateOn 'Upper Lane Lights
    If B2SOn Then
        Controller.B2SSetBallInPlay 32, Ball
    End If
 
    PlayReq(36) ' SOB
    BallReleaseTimer.Enabled = TRUE
End Sub  
 
Sub BallReleaseTimer_Timer()
  BallReleaseTimer.Enabled = FALSE
  BallRelease.createball
  PlaySoundAt SoundFX("ballrelease", DOFContactors),Ballrelease
  DOF 119, 2
  BallRelease.kick 135,4,0
	If B2SOn Then
        Controller.B2SSetScorePlayer 1,Score(Up)
		If Players > 1 Then
			Controller.B2SSetPlayerUp Up
		End If
    End If
End Sub
 
Sub Drain_Hit()
    Drain.DestroyBall
    DOF 133, 2
    TriggerLightsOff()
    MyLightSeq.StopPlay()  ' reset if Tilted lights
    Tilts = 0
    PlayerDelayTimer.Enabled = TRUE
    PlaySoundAt "drain", Drain
End Sub
 
Sub PlayerDelayTimer_Timer()
    Tilted = FALSE
    PlayerDelayTimer.Enabled = FALSE
 
    BonusLights = False
    BumperTimersOff()
    Bumper5_On()
    Bumper1.Force=8  ' reset Bumpers after a tilt
    Bumper2.Force=8
    Bumper3.Force=8
    Bumper4.Force=8
    Bumper5.Force=5
    StopSound MusicFile
 
    Tilts = 0
    If B2SOn Then
        Controller.B2SSetTilt 0
        Controller.B2SSetdata 1,1
    End If
 
    UpLight(Up).State = LightStateOff
    Up = Up + 1
    IF Up> Players then
        Ball = Ball + 1
        If B2SOn Then
            Controller.B2SSetBallInPlay Ball
        End If
        Up = 1
    End If  
    If Ball > BallsPerGame then
       GameOn = FALSE
       EndOfGame()
    Else
       UpLight(Up).State = LightStateOn
	   EmReel1.SetValue Score(up)
       BallReel.SetValue Ball
       Bumper1_Off:Bumper2_Off:Bumper3_Off:Bumper4_Off:Bumper5_On
       L11.State=LightStateOn:L12.State=LightStateOn:L13.State=LightStateOn:L14.State=LightStateOn 'Upper Lane Lights
       LFlag=0:RFlag=0   ' speech you hear on the inlanes
         ' turn on bullseye lights
         L5.State = LightStateOn   'Yellow Light
       if ball=2 then PlayReq(38) ' SOB
       if ball=3 then PlayReq(39)
       'if ball=4 then PlayReq(37)
       if ball=5 then PlayReq(40)
       BallReleaseTimer.Enabled = TRUE
       If B2SOn Then
         Controller.B2SSetBallInPlay Ball
       End If
    End If
End Sub

'***************************************
'              GAME OVER
'***************************************
 
Sub EndOfGame()
    LeftFlipper.RotateToStart
    RightFlipper.RotateToStart
    PlayReq(90)
    EndMusic
    
    If B2SOn Then
        Controller.B2SSetBallInPlay 0
    End If
    RoundHS = Score(1)
    If RoundHS > HiSc Then
        PlayReq(32)
        HiSc = RoundHS
        HSi = 1
        HSx = 1
        y = 1
        Initial(1) = 1
        For x = 2 to 3
            Initial(x) = 0
        Next
        UpdatePostIt
        InitialTimer1.enabled = 1
        EnableInitialEntry = True
    End If
    UpdatePostIt
    SaveHS
    Players = 0
    Bumper1_Off:Bumper2_Off:Bumper3_Off:Bumper4_Off:Bumper5_Off
    GI_Off
    If B2SOn Then
        Controller.B2SSetGameOver 1
		Controller.B2SSetPlayerUp 0
    End If
    EndOfGameMatchFeature
    EndOfGameTimer.Interval=1200
    EndOfGameTimer.Enabled=True
End Sub
 
Sub EndOfGameTimer_Timer()
    EndOfGameTimer.Enabled=False
    PlayReq(91) ' End of Game Talk
    AttractMode.Interval=500
    AttractMode.Enabled = True
    attractmodeflag=0
    AttractModeSounds.Interval= 15000
    AttractModeSounds.Enabled = True
End Sub
 


'*************** ABHCOIDE'S MATCH FEATURE ***************
' Generate and display match number at the end of the game

Dim Match
Sub EndOfGameMatchFeature()
    ' Generate a random match number (0-9)
     MatchNumber = Int(Rnd * 10 + 1)

    ' Update backglass with the match number if B2S is active
    If B2SOn Then
        Controller.B2SSetData 34, MatchNumber
    End If

    ' Check if the last digit of Player 1's score matches the generated number
    Dim PlayerLastDigit
    PlayerLastDigit = Score(1) Mod 10

    If MatchNumber = PlayerLastDigit Then
        ' Call the subroutine to award a free game or special
        Call AwardSpecial
    End If
End Sub

Sub Clear_Match()
    MatchNumber = 0
    If B2SOn then
        Controller.B2SSetData 34, MatchNumber
    end if
End Sub


'*****************************************
'	ZFLP:   Flippers Primitives
'*****************************************

Const ReflipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled) 'Left flipper solenoid callback
	If Enabled Then
		FlipperActivate LeftFlipper, LFPress
		LF.Fire  'leftflipper.rotatetoend
		
		'If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then
		'	RandomSoundReflipUpLeft LeftFlipper
		'Else
		'	SoundFlipperUpAttackLeft LeftFlipper
		'	RandomSoundFlipperUpLeft LeftFlipper
		'End If
	Else
		FlipperDeActivate LeftFlipper, LFPress
		LeftFlipper.RotateToStart
		'If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
		'	RandomSoundFlipperDownLeft LeftFlipper
		'End If
		'FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled) 'Right flipper solenoid callback
	If Enabled Then
		FlipperActivate RightFlipper, RFPress
		RF.Fire 'rightflipper.rotatetoend
		
		'If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
		'	RandomSoundReflipUpRight RightFlipper
		'Else
		'	SoundFlipperUpAttackRight RightFlipper
		'	RandomSoundFlipperUpRight RightFlipper
		'End If
	Else
		FlipperDeActivate RightFlipper, RFPress
		RightFlipper.RotateToStart
		'If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
		'	RandomSoundFlipperDownRight RightFlipper
		'End If
		'FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

' Flipper collide subs
Sub LeftFlipper_Collide(parm)
	CheckLiveCatch ActiveBall, LeftFlipper, LFCount, parm
	LF.ReProcessBalls ActiveBall
	RandomSoundFlipper()
	'LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch ActiveBall, RightFlipper, RFCount, parm
	RF.ReProcessBalls ActiveBall
	RandomSoundFlipper()
	'RightFlipperCollide parm
End Sub

'Sub LeftFlipper_Animate
'	dim a: a = LeftFlipper.CurrentAngle
'	FlipperLSh.RotZ = a
'	leftflipprim.RotZ = a
'	'Add any left flipper related animations here
'End Sub

'Sub RightFlipper_Animate
'	dim a: a = RightFlipper.CurrentAngle
'	FlipperRSh.RotZ = a
'	rightflipprim.RotZ = a
'	'Add any right flipper related animations here
'End Sub

' Old Flipper codes

Sub UpdateFlipperLogos
    leftflipprim.RotAndTra8 = LeftFlipper.CurrentAngle + 235
    rightflipprim.RotAndTra8 = RightFlipper.CurrentAngle + 126
    FlipperLSh.RotZ = LeftFlipper.currentangle
    FlipperRSh.RotZ = RightFlipper.currentangle
End Sub
 
Sub FlippersTimer_Timer()
    UpdateFlipperLogos
End Sub
 
' Game Logic
Sub TestHS
            HSi = 1
            HSx = 1
            y = 1
            Initial(1) = 1
            For x = 2 to 3
                Initial(x) = 0
            Next
            UpdatePostIt
            InitialTimer1.enabled = 1
            EnableInitialEntry = True
End Sub

'********************************************************
'              ZTSS: TRIGGERS & SWITCHES
'********************************************************

' Playfield Star Rollovers
 
'*** T1 = T R I G G E R   1
Sub Trigger1_Hit()
  
  DOF 127, 2
  If L1.State = LightStateOn and  L3.State = LightStateOn then
    AddScore (10)
  End if
  If L1.State = LightStateOn and L3.State = LightStateOn and L2.State = LightStateOn and L4.State = LightStateOn then
    AddScore (90)
  End if
  If L1.State = LightStateOn and  L3.State <> LightStateOn then
    AddScore (10)
  End if
  If L1.State = LightStateOff then
    'PlaysoundAtVol "trigger7",Trigger1, VolTrig
    FlashForMs L1, 500, 50, 0 
    AddScore (1)
  End If
  If L1.State = LightStateOn then
    'PlaysoundAtVol "trigger2",Trigger1, VolTrig
    FlashForMs L1, 500, 50, 1
  End if
 
End Sub

 
Sub Trigger2_Hit()
  
  DOF 128, 2
  If L2.State = LightStateOn and  L4.State = LightStateOn then
    AddScore (10)
  End if
  If L2.State = LightStateOn and L4.State = LightStateOn and L1.State = LightStateOn and L3.State = LightStateOn then
    AddScore (90)
  End if
  If L2.State = LightStateOn and L4.State <> LightStateOn then
    AddScore(10)
  End if
  If L2.State = LightStateOff then
    'PlaysoundAtVol "trigger7",Trigger2, VolTrig
    FlashForMs L2, 500, 50, 0
    AddScore(1)
  End If
  If L2.State = LightStateOn then
    'PlaysoundAtVol "trigger2",Trigger2, VolTrig
    FlashForMs L2, 500, 50, 1
  End if
End Sub
 
Sub Trigger3_Hit()
  
  DOF 129, 2
  If L3.State = LightStateOn and  L1.State = LightStateOn then
    AddScore (10)
  End if
  If L1.State = LightStateOn and L3.State = LightStateOn and L2.State = LightStateOn and L4.State = LightStateOn then
    AddScore (90)
  End if
  If L4.State = LightStateOn and L1.State <> LightStateOn then
    AddScore(10)
  End if
  If L3.State = LightStateOff then
    'PlaysoundAtVol "trigger7",Trigger3, VolTrig
    FlashForMs L3, 500, 50, 0
    AddScore(1)
  End If
  If L3.State = LightStateOn then
    'PlaysoundAtVol "trigger2",Trigger3, VolTrig
    FlashForMs L3, 500, 50, 1
  End if
End Sub
 
Sub Trigger4_Hit()
  
  DOF 130, 2
  If L4.State = LightStateOn and  L2.State = LightStateOn then
    AddScore (10)
  End if
  If L1.State = LightStateOn and L2.State = LightStateOn and L3.State = LightStateOn and L4.State = LightStateOn then
    AddScore (90)
  End if
  If L4.State = LightStateOn and L2.State <> LightStateOn then
    AddScore(10)
  End if
  If L4.State = LightStateOff then
    'PlaysoundAtVol "trigger7",Trigger4, VolTrig
    FlashForMs L4, 500, 50, 0
    AddScore(1)
  End If
  If L4.State = LightStateOn then
    'PlaysoundAtVol "trigger2",Trigger4, VolTrig
    FlashForMs L4, 500, 50, 1
  End if
End Sub

'*********************************
'       TRIGGER 5 VALUE 
'*********************************

Sub Trigger5_Hit() 
  DOF 126, 2
  If L8.State = LightStateOn then
     'PlaysoundAtVol "trigger2",Trigger5, VolTrig
     FlashForMs L8, 500, 50, 1
     AddScore(10)
	   End if
  If L8.State = LightStateOff then
    'PlaysoundAtVol "trigger7",Trigger5, VolTrig
    FlashForMs L8, 500, 50, 0
    AddScore(1)
  End if
End Sub
 
 
Sub Trigger6_Hit()
'L9 Appears to be the turkey light
  If L9.State = LightStateOff then
    'PlaysoundAtVol "trigger7",Trigger6, VolTrig
    FlashForMs L9, 500, 50, 0
    AddScore(1)
	  End If
  If L9.State = LightStateOn then
    'PlaysoundAtVol "trigger2",Trigger6, VolTrig
    FlashForMs L9, 500, 50, 1
    AddScore(10)
	  End if
  
End Sub
 
Sub Trigger7_Hit()
  
  
  DOF 131, 2
  If L10.State = LightStateOff then
    'PlaysoundAtVol "trigger7",Trigger7, VolTrig
    FlashForMs L10, 500, 50, 0
    AddScore(1)
    Exit Sub
  End if 
  If L10.State = LightStateOn then
    'PlaysoundAtVol "trigger2",Trigger7, VolTrig
    FlashForMs L10, 500, 50, 1
    AddScore(10)
  End if
End Sub

'*** T8 = T R I G G E R 8   =   C E N T E R    D R A I N   S W I T C H *** 
Sub Trigger8_Hit() ' Center Exit/Drain lane
If Tilted = FALSE then
    PlaySoundAtVol "rollover", Trigger8, VolTrig
    PlayReq(116)
    AddScore(10)
	   End if
End Sub
 
dim b1,b2,b3,b4,b5
 
'**************************************
'          POP BUMPER HITS 
'**************************************

Sub Bumper1_Hit() 
  PlaySoundAtVol SoundFX("fx_bumper", DOFContactors),Bumper1,VolBump
    DOF 105, 2
  if b1=0 then   
    AddScore 1
    FlashForMs b1l2, 500, 50, 0
    b1=0
  else
    AddScore 10
    FlashForMs b1l2, 500, 50, 1
    b1=1  
  end if
End Sub
 
Sub Bumper2_Hit()
  PlaySoundAtVol SoundFX("fx_bumper", DOFContactors),Bumper2,VolBump
  DOF 107, 2
  if b2=0 then
    AddScore 1
    FlashForMs b2l3, 500, 50, 0
    b2=0
  else
    AddScore 10
    FlashForMs b2l3, 500, 50, 1
    b2=1
  end if
End Sub
 
Sub Bumper3_Hit()
  PlaySoundAtVol SoundFX("fx_bumper", DOFContactors),Bumper3,VolBump
  DOF 106, 2
  if b3=0 then
    AddScore 1
    FlashForMs b3l3, 500, 50, 0
    b3=0
  else
    AddScore 10
    FlashForMs b3l3, 500, 50, 1
    b3=1
  end if
End Sub

Sub Bumper4_Hit()
  PlaySoundAtVol SoundFX("fx_bumper", DOFContactors),Bumper4,VolBump
  DOF 108, 2
  if b4=0 then
    AddScore 1
    FlashForMs b4l2, 500, 50, 0
    b4=0
  else
    AddScore 10
    FlashForMs b4l2, 500, 50, 1
    b4=1
  end if
End Sub
 

'*** POP BUMPER ***
Sub Bumper5_Hit()
  PlaySoundAtVol SoundFX("fx_bumper", DOFContactors),Bumper5,VolBump
  DOF 109, 2						
  RotateBonusLight()
  'PlayReq(166)
  if b5=0 then
      AddScore 1 
    FlashForMs b5l3, 500, 50, 0
    b5=0
  else
      AddScore 5 
    FlashForMs b5l3, 500, 50, 1
    b5=1
  end if 
End Sub
 
Sub t1_hit()
    t1p.transx = -10
    Me.TimerEnabled = 1
    addscore 50
	End sub
Sub t1_Timer
    t1p.transx = 0
    Me.TimerEnabled = 0
End Sub
 
'right target1
'*************************************************
'              RIGHT KICKER TARGET
'*************************************************

Sub target1L_hit()
  target1_hit(5)
End Sub
Sub target1R_hit()
  target1_hit(5)
End Sub
Sub target1C_hit()
  target1_hit(50)
End Sub
 
Sub target1_hit(pts)
    target1p.transx = -10
    target1.TimerEnabled = 1
    PlaysoundAt SoundFX("TargetR", DOFTargets),Light12
    DOF 124, 2
    ' add some points
    If L6.State = LightStateOff then
      AddScore(pts)
      if pts = 50 then
        L6.State=LightStateBlinking:L6.uservalue=0
        L6.timerinterval=700:L6.TimerEnabled=True
        PlayReq(143)
      end if      
    end if
    If L6.State = LightStateOn then
      AddScore(200)            
      L6.State=LightStateBlinking:L6.uservalue=0 ' Turn Off
      L6.timerinterval=700:L6.TimerEnabled=True
      L5.State=LightStateOn   ' Next Light
      PlayReq(143)
    End if
End sub
 
Sub L6_timer()
  L6.TimerEnabled=False
  if L6.uservalue = 0 then L6.State=LightStateOff
  if L6.uservalue = 1 then L6.State=LightStateOn
End Sub
 
Sub target1_Timer
    target1p.transx = 0
    Me.TimerEnabled = 0
End Sub

'**********************************************
'          LEFT KICKER TARGET
'**********************************************

Sub target2L_hit()
  target2_hit(5)
End Sub
Sub target2R_hit()
  target2_hit(5)
End Sub
Sub target2C_hit()
  target2_hit(50)
End Sub
 
Sub target2_hit(pts)
    target2p.transx = -10
    target2.TimerEnabled = 1
    PlaysoundAt SoundFX("TargetR", DOFTargets),Light11
    DOF 125, 2
    ' add some points
    If L5.State = LightStateOff then
      AddScore(pts)
      if pts = 50 then
        L5.State=LightStateBlinking:L5.uservalue=0
        L5.timerinterval=700:L5.TimerEnabled=True
        PlayReq(143)
      end if      
    end if
    If L5.State = LightStateOn then
      AddScore(200)	  
      L5.State=LightStateBlinking:L5.uservalue=0
      L5.timerinterval=700:L5.TimerEnabled=True
      L7.State=LightStateOn
      PlayReq(143)
    End if
End sub
 
Sub target2_Timer
    target2p.transx = 0
    Me.TimerEnabled = 0
End Sub
 
Sub L5_timer()
  L5.TimerEnabled=False
  if L5.uservalue = 0 then L5.State=LightStateOff
  if L5.uservalue = 1 then L5.State=LightStateOn
End Sub
 
'*********************************************
'   ZKHS       KICKER HOLE SHOT
'*********************************************

Sub Kicker1_Hit()
    PlaysoundAt "kicker_enter", Kicker1
  If L7.State = LightStateoff then 'light below cup
     L7.State=LightStateBlinking:L7.uservalue=0
     L7.timerinterval=700:L7.TimerEnabled=True
     AddScore (50)
  End if

  If L7.State = LightStateOn then
     L7.State=LightStateBlinking:L7.uservalue=0
     L7.timerinterval=700:L7.TimerEnabled=True
		AddScore (200)
    L6.State = LightStateOn ' Turn Next Light On
  End if

  pauseloop=0
  PauseTimer.Interval=250
  PauseTimer.Enabled=TRUE
End Sub
 
Sub PauseTimer_Timer()
  pauseloop=pauseloop+1
  if pauseloop=10 then
    PlayReq(195)
  end if
  if pauseloop=1 and (b1+b2+b3+b4)=4 then PlayReq(126)
  if pauseloop=1 and (b1+b2+b3+b4)=3 then PlayReq(127)
 
  if pauseloop=2 then
    if b1=1 then  ' flash lit bumper and award points
      Bumper1_Off:b1=1 ' flash - need to restore the value of b1 here
      Bumper1.PlayHit():DOF 105, 2:PlaySoundAt SoundFX("fx_bumper", DOFContactors),Bumper1
      'playsound "primbump14"
      Bumper1.TimerInterval=500:Bumper1.TimerEnabled=1
      AddScore (10)
    else
      pauseloop=pauseloop+1 ' skip forward
    end if
  end if
  if pauseloop=4 then
    if b2=1  then
      Bumper2_Off:b2=1 ' flash
      Bumper2.PlayHit():DOF 107, 2:PlaySoundAt SoundFX("fx_bumper", DOFContactors),Bumper2
      'playsound "primbump23"
      Bumper2.TimerInterval=500:Bumper2.TimerEnabled=1
      AddScore (10)
    else
      pauseloop=pauseloop+1
    end if
  end if
  if pauseloop=6 then
    if b3=1 then
      Bumper3_Off:b3=1 ' flash
      Bumper3.PlayHit():DOF 106, 2:PlaySoundAt SoundFX("fx_bumper", DOFContactors),Bumper3
      'playsound "primbump14"
      Bumper3.TimerInterval=500:Bumper3.TimerEnabled=1
      AddScore (10)
    else
      pauseloop=pauseloop+1
    end if
  end if
  if pauseloop=8 then
    if b4=1 then
      Bumper4_Off:b4=1 ' flash
      Bumper4.PlayHit():DOF 108, 2:PlaySoundAt SoundFX("fx_bumper", DOFContactors),Bumper4
      'playsound "primbump23"
      Bumper4.TimerInterval=500:Bumper4.TimerEnabled=1
      AddScore (10)
    else
      pauseloop=pauseloop+1
    end if
  end if
  if pauseloop > 12 then
    PauseTimer.Enabled=FALSE
    Kicker1.TimerInterval = 500
    Kicker1.TimerEnabled = TRUE
  End if
End Sub
 
Sub Kicker1_Timer()
    PlaySoundAt "kicker_release", Kicker1
	kicker1.TimerEnabled = FALSE
	Kicker1.kick 258+rnd*4,23+rnd*5, .1
	kicker1Rod.transy=15	
	kickrodtimer.uservalue=1
	kickrodtimer.enabled=1
	DOF 123, 2
End Sub

Sub kickrodtimer_timer
	if me.uservalue=2 then kicker1rod.transy=7.5
	if me.uservalue=3 then
		kicker1rod.transy=0
		me.enabled=0
	end if
	me.uservalue=me.uservalue+1
end sub
 
Sub L7_timer()
  L7.TimerEnabled=False
  if L7.uservalue = 0 then L7.State=LightStateOff
  if L7.uservalue = 1 then L7.State=LightStateOn
End Sub
 
Sub RotateBonusLight
  if L5.state = LightStateOn then
    ' Move to L7
      L5.state = LightStateOff
      L7.state=LightStateOn
  else
    if L7.state = LightStateOn then
      ' move to L6
        L7.state = LightStateOff
        L6.state=LightStateOn
    else
      if L6.state = LightStateOn then
        ' move to L5
          L6.state = LightStateOff
          L5.state=LightStateOn
      end if
    end if
  end if
End Sub

'****************************************
'      OUTLANE / INLANE  SWITCHES
'****************************************
 
Sub LeftInLaneTrigger_Hit()
 If Tilted = FALSE then
    PlaySoundAtVol "rollover", LeftInLaneTrigger, VolRol
    AddScore(5)
    DOF 116, 2
    LFlag=LFlag+1
    if LFlag>10 then LFlag=1
 
    if LFlag = 1 then
      if RFlag=0 or RFlag=3 then
        RFlag=1  ' skip over the intro speech
      end if
      PlayReq(197)
    end if
    if LFlag = 2 then PlayReq(198)
    if LFlag = 3 then PlayReq(199)
    if LFlag > 3 then PlayReq(115)
    RotateBonusLight()
    
End if
End Sub
 
Sub RightInLaneTrigger_Hit()
 If Tilted = FALSE then
    PlaySoundAtVol "rollover", RightInLaneTrigger, VolRol
    ' add some points
    AddScore(5)
     DOF 117, 2
    ' add some points
    RFlag=RFlag+1
    if RFlag>10 then RFlag=1
 
    if RFlag = 1 then
      if LFlag=0 or LFlag=3 then
        LFlag=1  ' skip over the intro speech
      end if
      PlayReq(197)
    end if
    if RFlag = 2 then PlayReq(198)
    if RFlag = 3 then PlayReq(199)
    if RFlag > 3 then PlayReq(115)
    RotateBonusLight()

End If
End Sub
 
Sub LeftOutLaneTrigger_Hit()
 If Tilted = FALSE then
    PlaySoundAtVol "rollover", LeftOutLaneTrigger, VolRol
    DOF 115, 2
    ' add some points
    AddScore(50)
    PlayReq(117)
End If
End Sub
 
Sub RightOutLaneTrigger_Hit()
 If Tilted = FALSE then
    PlaySoundAtVol "rollover", RightOutLaneTrigger, VolRol
    ' add some points
    DOF 118, 2
    ' add some points
    AddScore(50)
      PlayReq(117)
End If
End Sub

'************************************************
' ZSST:        SLINGSHOT TRIGGERS 
'************************************************ 

' RStep and LStep are the variables that increment the animation
Dim RStep, LStep

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
	RSling1.Visible = 1
	'Sling1.TransY =  - 20   'Sling Metal Bracket
	RStep = 0
	RightSlingShot.TimerEnabled = 1
	RightSlingShot.TimerInterval = 10
	PlaySoundAt SoundFX("fx_slingshot", DOFContactors),SLING9
    DOF 104, 2
    Addscore(1)
	'   vpmTimer.PulseSw 52	'Slingshot Rom Switch
	'RandomSoundSlingshotRight Sling1
End Sub

Sub RightSlingShot_Timer
	Select Case RStep
		Case 3
			RSLing1.Visible = 0
			RSLing2.Visible = 1
			SLING9.TransY =  - 10
		Case 4
			RSLing2.Visible = 0
			SLING9.TransY = 0
			RightSlingShot.TimerEnabled = 0
	End Select
	RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
	LSling1.Visible = 1
	SLING8.TransY =  - 20   'Sling Metal Bracket
	LStep = 0
	LeftSlingShot.TimerEnabled = 1
	LeftSlingShot.TimerInterval = 10
	PlaySoundAt SoundFX("fx_slingshot", DOFContactors),SLING8
    DOF 103, 2
    Addscore(1)
	'   vpmTimer.PulseSw 51	'Slingshot Rom Switch
	'RandomSoundSlingshotLeft Sling2
End Sub

Sub LeftSlingShot_Timer
	Select Case LStep
		Case 3
			LSLing1.Visible = 0
			LSLing2.Visible = 1
			SLING8.TransY =  - 10
		Case 4
			LSLing2.Visible = 0
			SLING8.TransY = 0
			LeftSlingShot.TimerEnabled = 0
	End Select
	LStep = LStep + 1
End Sub

Sub Blinkwallm1_hit()
    Playsoundat "fx_rubber2", activeball
    FlashforMs Light36, 500, 50, 1
    Addscore(1)  
End Sub

Sub BlinkwallR1_hit()
    Playsoundat "fx_rubber2", activeball
    FlashforMs Light36, 500, 50, 1
    Addscore(1)  
End Sub

Sub RSlingshot1R_hit()
    Addscore(1)
End Sub

Sub BlinkwallR2_hit()
    Playsoundat "fx_rubber2", activeball
    FlashforMs Light48, 500, 50, 1
    Addscore(1)
End Sub

Sub RSlingshot2R_hit()
    Addscore(1)
End Sub

Sub RSlingshot3R_hit()
    Addscore(1)
End Sub

Sub Blinkwall1_hit()
    Playsoundat "fx_rubber2", activeball
    FlashforMs Light44, 500, 50, 1
    Addscore(1)
End Sub

Sub LSlingshot1R_hit()
    Addscore(1)
End Sub


Sub LSlingshot3R_hit()
    Addscore(1)
End Sub


Sub LSlingshot2R_hit()
    Addscore(1)
End Sub

Sub Blinkwall2_hit()
    Playsoundat "fx_rubber2", activeball
    FlashforMs Light45, 500, 50, 1
    Addscore(1)
End Sub

Sub Blinkwall3_hit()
    Playsoundat "fx_rubber2", activeball
    FlashforMs Light51, 500, 50, 1
    Addscore(1)
End Sub

Sub LSlingshot4R_hit()
    Addscore(1)
End Sub
Sub LSlingshot5R_hit()
    Addscore(1)
End Sub

'*****************************************************************************************
'      ZULR:  UPPER LEFT ROLLOVER SWITCHES [T9 T10 T11 T12] aka STERN # 14 13 28 27
'***************************************************************************************** 

Sub t9_Hit()  'lane 1 turns on L8/trigger 5 (1st of 4 Upper Left Switches Left to RightFlipper)
 If Tilted = FALSE then
    bumper1_on
    PlaysoundAt "rollover",T9					
    PlayReq(120) ' top lanes : Sub
    DOF 110, 2
    If StandardMode = True then
        If L14.State = LightStateOn then
            AddScore(10) 
                 L14.State = LightStateBlinking
            Exit Sub
        End if
   
        If L14.State = LightStateBlinking then  ' Upper Left Light
            AddScore(10)
            L14.State = LightStateOff
            L1.State = LightStateOn   ' Bottom Rollover Lightfx_
            L8.State = LightStateOn   ' Right Red Rollover
        End if
    Else
        If L14.State = LightStateOn then ' Upper Left Light
            AddScore(10)
            L14.State = LightStateOff
            L1.State = LightStateOn   ' Bottom Rollover Light
            L8.State = LightStateOn   ' Right Red Rollover
        Else
           AddScore(1) ' Get 10pts when light is off
        End if
    End if
    If L14.State = LightStateOff and L13.State = LightStateOff and L12.State = LightStateOff and L11.State=LightStateOff  then ' all top lights
        AddScore(250)
         PlayReq(121)
    End if
End if
End Sub
 
Sub t10_Hit() ' lane2  - should light L15/trigger 14
 If Tilted = FALSE then
    bumper2_on
    PlaysoundAt "rollover",T10	
    PlayReq(120) ' top lanes
    DOF 111, 2
    If StandardMode = True then
        If L13.State = LightStateOn then
            AddScore(10) 
            L13.State = LightStateBlinking
            Exit Sub
        End if
   
        If L13.State = LightStateBlinking then  ' Upper Left Light
            AddScore(10)
            L13.State = LightStateOff
            L2.State = LightStateOn   ' Bottom Rollover Light
            L15.State = LightStateOn  ' Green Rolloever
        End if
    Else
        If L13.State = LightStateOn then ' Upper Left Light
            AddScore(10)
             L13.State = LightStateOff
            L2.State = LightStateOn   ' Bottom Rollover Light
            L15.State = LightStateOn  ' Green Rolloever
        Else
           AddScore(1) ' Get 10pts when light is off
        End if
    End if
 
 
    If L14.State = LightStateOff and L13.State = LightStateOff and L12.State = LightStateOff and L11.State=LightStateOff  then ' all top lights
        AddScore(250) 
        PlayReq(121)
    End if
End if
End Sub
 
Sub t11_Hit()
 If Tilted = FALSE then
    bumper4_on
    PlaysoundAt "rollover",T11
    PlayReq(120) ' top lanes
    DOF 112, 2
    If StandardMode = True then
        If L12.State = LightStateOn then
            AddScore(10)
             L12.State = LightStateBlinking
            Exit Sub
        End if
   
        If L12.State = LightStateBlinking then  '
            AddScore(10) 
             L12.State = LightStateOff
            L3.State = LightStateOn   '
            L9.State = LightStateOn  
        End if
    Else
        If L12.State = LightStateOn then '
            AddScore(10) 
            L12.State = LightStateOff
            L3.State = LightStateOn   ' Mid Playfield rollovers
            L9.State = LightStateOn   ' 2nd rollover
        Else
           AddScore(1) ' Get 10pts when light is off
         End if
    End if
    If L14.State = LightStateOff and L13.State = LightStateOff and L12.State = LightStateOff and L11.State=LightStateOff  then ' all top lights
        AddScore(250)
        PlayReq(121)
    End if
End If
End Sub
 
Sub T12_Hit()  ' should light  L10
 If Tilted = FALSE then
    bumper3_on
    PlaysoundAt "rollover",T12
    PlayReq(120) ' top lanes
    DOF 113, 2
    If StandardMode = True then
        If L11.State = LightStateOn then
            AddScore(10)
            L11.State = LightStateBlinking
            Exit Sub
        End if
   
        If L11.State = LightStateBlinking then  ' Upper Left Light
            AddScore(10)
            L11.State = LightStateOff
            L4.State = LightStateOn   ' Bottom Rollover Light
            L10.State = LightStateOn  ' 2nd rollover
        End if
    Else
        If L11.State = LightStateOn then ' Upper Left Light
            AddScore(10) 
             L11.State = LightStateOff
            L4.State = LightStateOn   ' Bottom Rollover Light
            L10.State = LightStateOn  ' 2nd rollover
        Else
           AddScore(1) ' Get 1pts when light is off
         End if
    End if
 
    If L14.State = LightStateOff and L13.State = LightStateOff and L12.State = LightStateOff and L11.State=LightStateOff  then ' all top lights
        AddScore(250) 
        PlayReq(121)
    End if
End If
End Sub


'****************************************************************
'         ZT13     SKILL SHOT SWITCH T13 VALUE 
'****************************************************************

Sub T13_Hit()					
 If Tilted = FALSE then
    PlaysoundAt "rollover",T13

    DOF 114, 2
 
    if LampSplash2.enabled then '200 pts for skill shot - gets turned off in addscore routine
      If L1.State = LightStateOff then
        FlashForMs L1, 500, 50, 0
      End If
      If L1.State = LightStateOn then
        FlashForMs L1, 500, 50, 1
      End if
      If L2.State = LightStateOff then
        FlashForMs L2, 500, 50, 0
      End If
      If L2.State = LightStateOn then
        FlashForMs L2, 500, 50, 1
      End if
      If L3.State = LightStateOff then
        FlashForMs L3, 500, 50, 0
      End If
      If L3.State = LightStateOn then
        FlashForMs L3, 500, 50, 1
      End if
      If L4.State = LightStateOff then
        FlashForMs L4, 500, 50, 0
      End If
      If L4.State = LightStateOn then
        FlashForMs L4, 500, 50, 1
      End if
      If L5.State = LightStateOff then
        FlashForMs L5, 500, 50, 0
      End If
      If L5.State = LightStateOn then
        FlashForMs L5, 500, 50, 1
      End if
      If L6.State = LightStateOff then
        FlashForMs L6, 500, 50, 0
      End If
      If L6.State = LightStateOn then
        FlashForMs L6, 500, 50, 1
      End if
      If L7.State = LightStateOff then
        FlashForMs L7, 500, 50, 0
      End If
      If L7.State = LightStateOn then
        FlashForMs L7, 500, 50, 1
      End if
      If L8.State = LightStateOff then
        FlashForMs L8, 500, 50, 0
      End If
      If L8.State = LightStateOn then
        FlashForMs L8, 500, 50, 1
      End if
      If L9.State = LightStateOff then
        FlashForMs L9, 500, 50, 0
      End If
      If L9.State = LightStateOn then
        FlashForMs L9, 500, 50, 1
      End if
      If L10.State = LightStateOff then
        FlashForMs L10, 500, 50, 0
      End If
      If L10.State = LightStateOn then
        FlashForMs L10, 500, 50, 1
      End if
      If L11.State = LightStateOff then
        FlashForMs L11, 500, 50, 0
      End If
      If L11.State = LightStateOn then
        FlashForMs L11, 500, 50, 1
      End if
      If L12.State = LightStateOff then
        FlashForMs L12, 500, 50, 0
      End If
      If L12.State = LightStateOn then
        FlashForMs L12, 500, 50, 1
      End if
      If L13.State = LightStateOff then
        FlashForMs L13, 500, 50, 0
      End If
      If L13.State = LightStateOn then
        FlashForMs L13, 500, 50, 1
      End if
      If L14.State = LightStateOff then
        FlashForMs L14, 500, 50, 0
      End If
      If L14.State = LightStateOn then
        FlashForMs L14, 500, 50, 1
      End if
      If L15.State = LightStateOff then
        FlashForMs L15, 500, 50, 0
      End If
      If L15.State = LightStateOn then
        'FlashForMs L15, 500, 50, 1
      End if
      If b1l2.State = LightStateOff then
        FlashForMs b1l2, 500, 50, 0
      End If
      If b1l2.State = LightStateOn then
        FlashForMs b1l2, 500, 50, 1
      End if
      If b2l3.State = LightStateOff then
        FlashForMs b2l3, 500, 50, 0
      End If
      If b2l3.State = LightStateOn then
        FlashForMs b2l3, 500, 50, 1
      End if
      If b3l3.State = LightStateOff then
        FlashForMs b3l3, 500, 50, 0
      End If
      If b3l3.State = LightStateOn then
        FlashForMs b3l3, 500, 50, 1
      End if
      If b4l2.State = LightStateOff then
        FlashForMs b4l2, 500, 50, 0
      End If
      If b4l2.State = LightStateOn then
        FlashForMs b4l2, 500, 50, 1
      End if
      AddScore(200)
      PlayReq(147) ' top lanes
	     
    else
      AddScore(50) 
      PlayReq(143)
      
   End If
End If
End Sub
 
Sub Trigger14_Hit()
 If Tilted = FALSE then
    DOF 134, 2
    PlayReq(119)
    If L15.State = LightStateOff then
      FlashForMs L15, 500, 50, 0
      AddScore(1)
    End If
      If L15.State = LightStateOn then
        FlashForMs L15, 500, 50, 1
        AddScore(10) 
      End if
   
End If
End Sub
 
 
Sub BumperTimersOff()
  L5.State = LightStateOff
  L6.State = LightStateOff
  L7.State = LightStateOff
End Sub
 
'====================
 
Sub Resetaftersuccess()
  BumperTimersOff()
  L5.State = LightStateOn  'reenable the lights
End Sub
 
'****** Score Handling ******
 
Sub AddScore(val)
  skillshotoff()
  If Tilted = FALSE then
    if int(Score(Up)/1000) <> int((Score(Up)+val)/1000) then
      PlaySound "Sounds-0x32C",0, CVol
    else
      if int(Score(Up)/100) <> int((Score(Up)+val)/100) then
        PlaySound "Sounds-0xE9",0, CVol
      else
        if int(Score(Up)/10) <> int((Score(Up)+val)/10) then
          PlaySound "Sounds-0x1F0",0, CVol
        end if
      end if
    end if
    if Score(Up)+val > 1000 and Score(Up)<1000 then
      PlayReq(88)
    end if
    if Score(Up)+val > 1500 and Score(Up)<1500 then
      PlayReq(87)
    end if
    if Score(Up)+val > 3500 and Score(Up)<3500 then
      PlayReq(86)
    end if
    if Score(Up)+val > 4000 and Score(Up)<4000 then
      PlayReq(85)
    end if
    if Score(Up)+val > 5000 and Score(Up)<5000 then
      PlayReq(84)
    end if
    if Score(Up)+val > 6000 and Score(Up)<6000 then
      PlayReq(83)
    end if
    if Score(Up)+val > 7000 and Score(Up)<7000 then
      PlayReq(82)
    end if
    if Score(Up)+val > 8000 and Score(Up)<8000 then
      PlayReq(80)
    end if
    if Score(Up)+val >10000 and Score(Up)<10000 then
      PlayReq(81)
    end if
 
    Score(Up) = Score(Up) + val
    DisplayScore
    AddReel val
 
    If B2SOn Then
        Controller.B2SSetScorePlayer 1,Score(Up)
    End If
  End If  
End Sub    
 
Sub AddReel(Pts) 'using one reel
   EmReel1.AddValue Pts
End Sub
 
 '****** Score Credit Award Added 12/27/24 ******

Sub DisplayScore()
            If Score(Up) >= 5000 And Credit1(Up)=false Then
            'PlayReq(30)
            AwardSpecial
            Credit1(Up)=true
        End If
         If Score(Up) >= 8000 And Credit2(Up)=false Then
            'PlayReq(31)
            AwardSpecial
            Credit2(Up)=true
        End If
End Sub
 
Sub AwardSpecial()
   
    PlaysoundAt SoundFX("fx_knocker", DOFKnocker),L11
    DOF 122, 2
    Credit = Credit + 1
    DOF 121, 1
    CreditReel.SetValue Credit
    If B2SOn Then
        Controller.B2SSetCredits Credit
    End If
    savehs
End Sub

'********** Save Scores *********

Sub SaveHS
    savevalue "primus", "Credit", Credit
    savevalue "primus", "HiScore", HiSc
    savevalue "primus", "Match", MatchNumber
    savevalue "primus", "Initial1", Initial(1)
    savevalue "primus", "Initial2", Initial(2)
    savevalue "primus", "Initial3", Initial(3)
    savevalue "primus", "Score4", Score(1)
    savevalue "primus", "Balls", Balls
End sub
 
'********** Load Scores **********

Sub LoadHighScore
    dim temp
    temp = LoadValue("primus", "credit")
    If (temp <> "") then Credit = CDbl(temp)
    temp = LoadValue("primus", "HiScore")
    If (temp <> "") then HiSc = CDbl(temp)
    temp = LoadValue("primus", "match")
    If (temp <> "") then MatchNumber = CDbl(temp)
    temp = LoadValue("primus", "Initial1")
    If (temp <> "") then Initial(1) = CDbl(temp)
    temp = LoadValue("primus", "Initial2")
    If (temp <> "") then Initial(2) = CDbl(temp)
    temp = LoadValue("primus", "Initial3")
    If (temp <> "") then Initial(3) = CDbl(temp)
    temp = LoadValue("primus", "score4")
    If (temp <> "") then score(1) = CDbl(temp)
    temp = LoadValue("primus", "balls")
    If (temp <> "") then Balls = CDbl(temp)
End Sub
 
 
Dim s26,s27,s1,s2,s3,s4,s6,s7
 
Sub LampSplashOn_Hit
  if Tilted = FALSE then
    PlaySoundAt "rollover", LampSplashOn
    s26=Light26.state:s27=Light27.state:s6=Light6.state:s7=Light7.state
    s1=l1.state:s2=l2.state:s3=l3.state:s4=l4.state
    l1.state=LightStateOff
    l2.state=LightStateOff
    l3.state=LightStateOff
    l4.state=LightStateOff
    LampSplash.interval = 75
    LampSplash2.interval = 75
    LampSplash.enabled = True
    LampSplash2.enabled = True
    BallInLane.Enabled=True
  End if
End Sub
 
Sub LampSplashOn_UnHit 
  if Tilted = FALSE then
    DOF 120, 2
    l1.state=s1:l2.state=s2:l3.state=s3:l4.state=s4
    LampSplash.enabled = False  'Turn off the 4 light attract mode
    BallInLane.Enabled=False
  End If
End Sub
 

Sub SkillshotOff   'turn off when points are scored
  if LampSplash2.enabled = True then
    LampSplash2.enabled = False
    Light26.state=s26:Light27.state=s27
    Light27.state = LightStateOff:Light26.state = LightStateOff
  End if
End Sub
 
Sub LampSplash_Timer() ' These lights no longer flash once the trigger is unhit
  Select Case lno
        Case 1
            l1.state = LightStateOn:l2.state = LightStateOff
        Case 2
            l2.state = LightStateOn:l1.state = LightStateOff
        Case 3
            l3.state = LightStateOn:l2.state = LightStateOff
        Case 4
            l4.state = LightStateOn:l3.state = LightStateOff
        Case 5
            l3.state = LightStateOn:l4.state = LightStateOff
        Case 6
            l2.state = LightStateOn:l3.state = LightStateOff
    End Select  
End Sub
 
Sub LampSplash2_Timer() ' flash the lights while ball is in the trough
  lno = lno + 1
  if lno > 6 then lno = 1
  Select Case lno
        Case 1
            Light27.state = LightStateOff:Light26.state = LightStateOff
        Case 2
            Light27.state = LightStateOn:Light26.state = LightStateOn
        Case 3
            Light27.state = LightStateOff:Light26.state = LightStateOff
        Case 4
            Light27.state = LightStateOn:Light26.state = LightStateOn
        Case 5
            Light27.state = LightStateOff:Light26.state = LightStateOff
        Case 6
            Light27.state = LightStateOn:Light26.state = LightStateOn
    End Select  
End Sub
 
' Base Vp10 Routines
' *********************************************************************
'  ZSBS:         Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng((BallVel(ball)*1 + 4)/ VolDiv)
End Function

Function VolMulti(ball,Multiplier) ' Calculates the Volume of the sound based on the ball speed
  VolMulti = Csng(BallVel(ball) ^2 / 150) * Multiplier
End Function
 
Function Vol2(ball1, ball2) ' Calculates the Volume of the sound based on the speed of two balls
    Vol2 = (Vol(ball1) + Vol(ball2) ) / 2
End Function
 
Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp> 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function
 
Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function
 
function AudioFade(ball)
    Dim tmp
    tmp = ball.y * 2 / Table1.height-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function
 
Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function
 
'**********************************************
' ZJPS   JP's VP10 Collision & Rolling Sounds
'**********************************************
 
ReDim rolling(tnob)
ReDim collision(tnob)
Initcollision
 
Sub Initcollision
    Dim i
    For i = 0 to tnob
        collision(i) = -1
        rolling(i) = False
    Next
End Sub
 
Sub CollisionTimer_Timer()
    Dim BOT, B, B1, B2, dx, dy, dz, distance, radii
    BOT = GetBalls
 
    ' rolling
   
    For B = UBound(BOT) +1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next
    If UBound(BOT) = -1 Then Exit Sub
    For b = 0 to UBound(BOT)
    If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
                    PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
            Else   
                    PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0
            End If
        Else
            If rolling(b) = True Then
                    StopSound("fx_ballrolling" & b)
                    rolling(b) = False
            End If
        End If

        '***Ball Drop Sounds***

		If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
			PlaySound "fx_ball_drop" & b, 0, ABS(BOT(b).velz)/17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
		End If

    Next
 
    'collision
 
    If UBound(BOT) < 1 Then Exit Sub
 
    For B1 = 0 to UBound(BOT)
        For B2 = B1 + 1 to UBound(BOT)
            dz = INT(ABS((BOT(b1).z - BOT(b2).z) ) )
            radii = BOT(b1).radius + BOT(b2).radius
            If dz <= radii Then
 
            dx = INT(ABS((BOT(b1).x - BOT(b2).x) ) )
            dy = INT(ABS((BOT(b1).y - BOT(b2).y) ) )
            distance = INT(SQR(dx ^2 + dy ^2) )
 
            If distance <= radii AND (collision(b1) = -1 OR collision(b2) = -1) Then
                collision(b1) = b2
                collision(b2) = b1
                PlaySound("fx_collide"), 0, Vol2(BOT(b1), BOT(b2)), Pan(BOT(b1)), 0, Pitch(BOT(b1)), 0, 0, AudioFade(BOT(b1))
            Else
                If distance > (radii + 10)  Then
                    If collision(b1) = b2 Then collision(b1) = -1
                    If collision(b2) = b1 Then collision(b2) = -1
                End If
            End If
            End If
        Next
    Next
End Sub
 
Sub Posts_Hit(idx)
    If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
        RandomSoundRubber()
    Else
        PlaySound "fx_rubber_post", 0, VolMulti(ActiveBall,VolRH), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
    End If
End Sub
 
 
Sub Rubbers_Hit(idx)
    If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
        RandomSoundRubber()
    Else
        PlaySound "fx_rubber_band", 0, VolMulti(ActiveBall,VolRH), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
    End If
End Sub
 
Sub RandomSoundRubber()
    Select Case Int(Rnd*3)+1
        Case 1 : PlaySoundAtBallVol "rubber_hit_1",VolRH
        Case 2 : PlaySoundAtBallVol "rubber_hit_2",VolRH
        Case 3 : PlaySoundAtBallVol "rubber_hit_3",VolRH
    End Select
End Sub
 
Sub RandomSoundFlipper()
    Select Case Int(Rnd*3)+1
        Case 1 : PlaySoundAtBall "flip_hit_1"
        Case 2 : PlaySoundAtBall "flip_hit_2"
        Case 3 : PlaySoundAtBall "flip_hit_3"
    End Select
End Sub
 
Sub Gates_Hit (idx)
    PlaySoundAtVol "gate4",ActiveBall, VolGates
End Sub
 
 
Sub AttractMode_Timer()   
    AttractMode.Enabled=False
    MyLightSeq.UpdateInterval=15
    MyLightSeq.Play SeqLeftOn, 30, 3
    MyLightSeq.Play SeqUpOn, 30, 3
    MyLightSeq.Play SeqDownOn, 30, 3
    MyLightSeq.Play SeqRightOn, 30, 3
    MyLightSeq.Play SeqLeftOn, 30, 3
    MyLightSeq.Play SeqUpOn, 30, 3
    MyLightSeq.Play SeqDownOn, 30, 3
    MyLightSeq.Play SeqRightOn, 30, 3
    MyLightSeq.Play SeqLeftOn, 30, 3
    MyLightSeq.Play SeqUpOn, 30, 3
    MyLightSeq.Play SeqDownOn, 30, 3
    MyLightSeq.Play SeqRightOn, 30, 3
    MyLightSeq.Play SeqLeftOn, 30, 3
    MyLightSeq.Play SeqUpOn, 30, 3
    MyLightSeq.Play SeqDownOn, 30, 3
    MyLightSeq.Play SeqRightOn, 30, 3
End Sub
 
Sub MyLightSeq_PlayDone() ' play the lights again until key is pressed
  AttractMode.Interval=100
  AttractMode.Enabled=True
End Sub
 
Sub AttractModeSounds_Timer() ' play random attract speech

  attractmodeflag=attractmodeflag+1
  if attractmodeflag = 1 then
    Playreq(28) 
  else
    if attractmodeflag > 7 then
      attractmodeflag=1
      Playreq(93) ' : msgbox "Attract Mode MusicFile=" &MusicFile
    else
      if attractmodeflag <> 6 then ' skip one round
        Playreq(93) ' : msgbox "Attract Mode MusicFile=" &MusicFile
      end if
    end if
  end if
  AttractModeSounds.Interval=7000+(INT(RND*5)*1000)
End Sub
 
Sub LastScoreReels
        HSScorex = LastScore
        HSScore100K=Int (HSScorex/100000)'Calculate the value for the 100,000's digit
        HSScore10K=Int ((HSScorex-(HSScore100k*100000))/10000) 'Calculate the value for the 10,000's digit
        HSScoreK=Int((HSScorex-(HSScore100k*100000)-(HSScore10K*10000))/1000) 'Calculate the value for the 1000's digit
        HSScore100=Int((HSScorex-(HSScore100k*100000)-(HSScore10K*10000)-(HSScoreK*1000))/100) 'Calculate the value for the 100's digit
        HSScore10=Int((HSScorex-(HSScore100k*100000)-(HSScore10K*10000)-(HSScoreK*1000)-(HSScore100*100))/10) 'Calculate the value for the 10's digit
        HSScore1=Int(HSScorex-(HSScore100k*100000)-(HSScore10K*10000)-(HSScoreK*1000)-(HSScore100*100)-(HSScore10*10)) 'Calculate the value for the 1's digit
 
End Sub
 
'********** Enter Initials **********

Sub EnterIntitals(keycode)
    If KeyCode = LeftFlipperKey Then
        HSx = HSx - 1
        if HSx < 0 Then HSx = 26
        If HSi < 4 Then EVAL("Initial" & HSi).image = HSiArray(HSx)
    End If
    If keycode = RightFlipperKey Then
        HSx = HSx + 1
        If HSx > 26 Then HSx = 0
        If HSi < 4 Then EVAL("Initial"& HSi).image = HSiArray(HSx)
    End If
        If keycode = StartGameKey Then
            If HSi < 3 Then
                EVAL("Initial" & HSi).image = HSiArray(HSx)
                Initial(HSi) = HSx
                EVAL("InitialTimer" & HSi).enabled = 0
                EVAL("Initial" & HSi).visible = 1
                Initial(HSi + 1) = HSx
                EVAL("Initial" & HSi +1).image = HSiArray(HSx)
                y = 1
                EVAL("InitialTimer" & HSi + 1).enabled = 1
                HSi = HSi + 1
            Else
                Initial3.visible = 1
                InitialTimer3.enabled = 0
                Initial(3) = HSx
                InitialEntry.enabled = 1
                HSi = HSi + 1
            End If
        End If
End Sub
 
Sub InitialEntry_timer
    SaveHS
    HSi = HSi + 1
    EnableInitialEntry = False
    InitialEntry.enabled = 0
    Players = 0
End Sub
 
'************ Flash Initials Timers **********

Sub InitialTimer1_Timer
    y = y + 1
    If y > 1 Then y = 0
    If y = 0 Then
        Initial1.visible = 1
    Else
        Initial1.visible = 0   
    End If
End Sub
 
Sub InitialTimer2_Timer
    y = y + 1
    If y > 1 Then y = 0
    If y = 0 Then
        Initial2.visible = 1
    Else
        Initial2.visible = 0   
    End If
End Sub
 
Sub InitialTimer3_Timer
    y = y + 1
    If y > 1 Then y = 0
    If y = 0 Then
        Initial3.visible = 1
    Else
        Initial3.visible = 0   
    End If
End Sub
 
'**************Update Desktop Text
Sub UpdateText
    BallReel.SetValue Ball
End Sub
 
 
'***************Post It Note Update
Sub UpdatePostIt
    ScoreMil = Int(HiSc/1000000)
    Score100K = Int( (HiSc - (ScoreMil*1000000) ) / 100000)
    Score10K = Int( (HiSc - (ScoreMil*1000000) - (Score100K*100000) ) / 10000)                                                     
    ScoreK = Int( (HiSc - (ScoreMil*1000000) - (Score100K*100000) - (Score10K*10000) ) / 1000)                                     
    Score100 = Int( (HiSc - (ScoreMil*1000000) - (Score100K*100000) - (Score10K*10000) - (ScoreK*1000) ) / 100)                    
    Score10 = Int( (HiSc - (ScoreMil*1000000) - (Score100K*100000) - (Score10K*10000) - (ScoreK*1000) - (Score100*100) ) / 10)         
    ScoreUnit = (HiSc - (ScoreMil*1000000) - (Score100K*100000) - (Score10K*10000) - (ScoreK*1000) - (Score100*100) - (Score10*10) )
 
    Pscore6.image = HSArray(ScoreMil):If HiSc < 1000000 Then PScore6.image = HSArray(10)
    Pscore5.image = HSArray(Score100K):If HiSc < 100000 Then PScore5.image = HSArray(10)
    PScore4.image = HSArray(Score10K):If HiSc < 10000 Then PScore4.image = HSArray(10)
    PScore3.image = HSArray(ScoreK):If HiSc < 1000 Then PScore3.image = HSArray(10)
    PScore2.image = HSArray(Score100):If HiSc < 100 Then PScore2.image = HSArray(10)
    PScore1.image = HSArray(Score10):If HiSc < 10 Then PScore1.image = HSArray(10)
    PScore0.image = HSArray(ScoreUnit):If HiSc < 1 Then PScore0.image = HSArray(10)
    If HiSc < 1000 Then
        PComma.image = HSArray(10)
    Else
        PComma.image = HSArray(11)
    End If
    If HiSc < 1000000 Then
        PComma1.image = HSArray(10)
    Else
        PComma1.image = HSArray(11)
    End If
    If HiSc < 1000000 Then Shift = 1:PComma.transx = -10
    If HiSc < 100000 Then Shift = 2:PComma.transx = -20
    If HiSc < 10000 Then Shift = 3:PComma.transx = -30
    For x = 0 to 6
        EVAL("Pscore" & x).transx = (-10 * Shift)
    Next
    Initial1.image = HSiArray(Initial(1))
    Initial2.image = HSiArray(Initial(2))
    Initial3.image = HSiArray(Initial(3))
End Sub
 
Sub table1_Exit
'   SaveLMEMConfig
    If B2SOn Then Controller.stop
End Sub
 
function PlayReq(num)
  rpos=INT(RND*len(Req(num))/3)
  PlaySound "Sound-0x" & rtrim(Ucase(mid(Req(num),3*rpos+1,3))),0, SVol
  PlayReq="Sound-0x" & Ucase(mid(Req(num),3*rpos+1,3))
End function
 
function PlayTrack(num)  ' softer and looping
  rpos=INT(RND*len(Req(num))/3)
  PlaySound "Sounds-0x" & rtrim(Ucase(mid(Req(num),3*rpos+1,3))), 5, TVol
  PlayTrack = "Sounds-0x" & rtrim(Ucase(mid(Req(num),3*rpos+1,3)))
End function
 
' Just used for testing
Sub PlayallReq(num)
  for i = 1 to len(Req(num))/3
    PlaySound "Sounds-0x" & Ucase(mid(Req(num),3*(i-1)+1,3))
    msgbox num & " playing Sounds-0x" & Ucase(mid(Req(num),3*(i-1)+1,3))
  Next
End Sub
 
 
'**************************************************************************
' ZPSP:  Positional Sound Playback Functions by DJRobX
' VPX version check only required for 10.3 backwards compatibility.
' Version check and Else statement may be removed if table is > 10.4 only.
'**************************************************************************
 
'Set position as table object (Use object or light but NOT wall) and Vol to 1
 
Sub PlaySoundAt(sound, tableobj)
    If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
        PlaySound sound, 1, 1, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
    Else
        PlaySound sound, 1, 1, Pan(tableobj)
    End If
End Sub
 
 
'Set all as per ball position & speed.
 
Sub PlaySoundAtBall(sound)
    If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
        PlaySound sound, 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
    Else
        PlaySound sound, 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1
    End If
End Sub
 
 
'Set position as table object and Vol manually.
 
Sub PlaySoundAtVol(sound, tableobj, Vol)
    If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
        PlaySound sound, 1, Vol, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
    Else
        PlaySound sound, 1, Vol, Pan(tableobj)
    End If
End Sub
 
 
'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3
 
Sub PlaySoundAtBallVol(sound, VolMult)
    If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
        PlaySound sound, 0, Vol(ActiveBall) * VolMult, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
    Else
        PlaySound sound, 0, Vol(ActiveBall) * VolMult, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1
    End If
End Sub
 
 
'*****************************************
'           BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)
 
Sub BallShadowUpdate_timer()
    Dim BOT, b
    BOT = GetBalls
    ' hide shadow of deleted balls
    If UBound(BOT)<(tnob-1) Then
        For b = (UBound(BOT) + 1) to (tnob-1)
            BallShadow(b).visible = 5
        Next
    End If
    ' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
    ' render the shadow for each ball
    For b = 0 to UBound(BOT)
        If BOT(b).X < Table1.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/16) + ((BOT(b).X - (Table1.Width/2))/7)) + 13
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/16) + ((BOT(b).X - (Table1.Width/2))/7)) - 13
        End If
        ballShadow(b).Y = BOT(b).Y + 10
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub


Sub FlashForMs (MyLight, TotalPeriod, BlinkPeriod, FinalState)
'===================================================================
' FlashForMs will blink light for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Return to previous State
'===================================================================
	If FinalState = 2 Then 
		MyLight.UserValue = MyLight.State		'Save current light state
	Else
		MyLight.UserValue = FinalState			'Save desired final state
	End If

	MyLight.State= 2    	
	MyLight.BlinkInterval= BlinkPeriod
	MyLight.TimerInterval= TotalPeriod
	MyLight.TimerEnabled=True

	'Creates the Timer subroutine.  Uncomment the MsgBox line to see the subroutine string it creates
	'MsgBox "Sub " & MyLight.Name & "_Timer : " & MyLight.name & ".State="& MyLight.name & ".UserValue: "& MyLight.name & ".TimerEnabled=False:  End Sub"
	ExecuteGlobal "Sub " & MyLight.Name & "_Timer : " & MyLight.name & ".State="& MyLight.name & ".UserValue: "& MyLight.name & ".TimerEnabled=False:  End Sub"
End Sub


'*** P H Y S I C S   &   N F O Z Z Y / R O T H *** PASTING IN CORE PHYSICS ROUTINTES FROM VPIN WORKSHOP BASE EXAMPLE TABLE V1.5.16
'*********************************************************************************************************************************
'
'******************************************************
' ZPHY     :  GENERAL ADVICE ON PHYSICS
'******************************************************
'
' It's advised that flipper corrections, dampeners, and general physics settings should all be updated per these
' examples as all of these improvements work together to provide a realistic physics simulation.
'
' Tutorial videos provided by Bord
' Adding nFozzy roth physics : pt1 rubber dampeners 				https://youtu.be/AXX3aen06FM?si=Xqd-rcaqTlgEd_wx
' Adding nFozzy roth physics : pt2 flipper physics 					https://youtu.be/VSBFuK2RCPE?si=i8ne8Ao2co8rt7fy
' Adding nFozzy roth physics : pt3 other elements 					https://youtu.be/JN8HEJapCvs?si=hvgMOk-ej1BEYjJv
'
' Note: BallMass must be set to 1. BallSize should be set to 50 (in other words the ball radius is 25)
'
' Recommended Table Physics Settings
' | Gravity Constant             | 0.97      |
' | Playfield Friction           | 0.15-0.25 |
' | Playfield Elasticity         | 0.25      |
' | Playfield Elasticity Falloff | 0         |
' | Playfield Scatter            | 0         |
' | Default Element Scatter      | 2         |
'
' Bumpers
' | Force         | 12-15    |
' | Hit Threshold | 1.6-2    |
' | Scatter Angle | 2        |
'
' Slingshots
' | Hit Threshold      | 2    |
' | Slingshot Force    | 3-5  |
' | Slingshot Theshold | 2-3  |
' | Elasticity         | 0.85 |
' | Friction           | 0.8  |
' | Scatter Angle      | 1    |

'******************************************************
'	ZNFF:  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
' There are several steps for taking advantage of nFozzy's flipper solution.  At a high level we'll need the following:
'	1. flippers with specific physics settings
'	2. custom triggers for each flipper (TriggerLF, TriggerRF)
'	3. and, special scripting
'
' TriggerLF and RF should now be 27 vp units from the flippers. In addition, 3 degrees should be added to the end angle
' when creating these triggers.
'
' RF.ReProcessBalls Activeball and LF.ReProcessBalls Activeball must be added the flipper_collide subs.
'
' A common mistake is incorrect flipper length.  A 3-inch flipper with rubbers will be about 3.125 inches long.
' This translates to about 147 vp units.  Therefore, the flipper start radius + the flipper length + the flipper end
' radius should  equal approximately 147 vp units. Another common mistake is is that sometimes the right flipper
' angle was set with a large postive value (like 238 or something). It should be using negative value (like -122).
'
' The following settings are a solid starting point for various eras of pinballs.
' |                    | EM's           | late 70's to mid 80's | mid 80's to early 90's | mid 90's and later |
' | ------------------ | -------------- | --------------------- | ---------------------- | ------------------ |
' | Mass               | 1              | 1                     | 1                      | 1                  |
' | Strength           | 500-1000 (750) | 1400-1600 (1500)      | 2000-2600              | 3200-3300 (3250)   |
' | Elasticity         | 0.88           | 0.88                  | 0.88                   | 0.88               |
' | Elasticity Falloff | 0.15           | 0.15                  | 0.15                   | 0.15               |
' | Fricition          | 0.8-0.9        | 0.9                   | 0.9                    | 0.9                |
' | Return Strength    | 0.11           | 0.09                  | 0.07                   | 0.055              |
' | Coil Ramp Up       | 2.5            | 2.5                   | 2.5                    | 2.5                |
' | Scatter Angle      | 0              | 0                     | 0                      | 0                  |
' | EOS Torque         | 0.4            | 0.4                   | 0.375                  | 0.375              |
' | EOS Torque Angle   | 4              | 4                     | 6                      | 6                  |
'

'******************************************************
' Flippers Polarity (Select appropriate sub based on era)
'******************************************************

Dim LF : Set LF = New FlipperPolarity
Dim RF : Set RF = New FlipperPolarity

InitPolarity

'
''*******************************************
'' Late 70's to early 80's
'
Sub InitPolarity()
   dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 80
		'x.DebugOn=False ' prints some info in debugger


        x.AddPt "Polarity", 0, 0, 0
        x.AddPt "Polarity", 1, 0.05, - 2.7
        x.AddPt "Polarity", 2, 0.16, - 2.7
        x.AddPt "Polarity", 3, 0.22, - 0
        x.AddPt "Polarity", 4, 0.25, - 0
        x.AddPt "Polarity", 5, 0.3, - 1
        x.AddPt "Polarity", 6, 0.4, - 2
        x.AddPt "Polarity", 7, 0.5, - 2.7
        x.AddPt "Polarity", 8, 0.65, - 1.8
        x.AddPt "Polarity", 9, 0.75, - 0.5
        x.AddPt "Polarity", 10, 0.81, - 0.5
        x.AddPt "Polarity", 11, 0.88, 0
        x.AddPt "Polarity", 12, 1.3, 0

		x.AddPt "Velocity", 0, 0, 0.85
		x.AddPt "Velocity", 1, 0.15, 0.85
		x.AddPt "Velocity", 2, 0.2, 0.9
		x.AddPt "Velocity", 3, 0.23, 0.95
		x.AddPt "Velocity", 4, 0.41, 0.95
		x.AddPt "Velocity", 5, 0.53, 0.95 '0.982
		x.AddPt "Velocity", 6, 0.62, 1.0
		x.AddPt "Velocity", 7, 0.702, 0.968
		x.AddPt "Velocity", 8, 0.95,  0.968
		x.AddPt "Velocity", 9, 1.03,  0.945
		x.AddPt "Velocity", 10, 1.5,  0.945

	Next

	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
	LF.SetObjects "LF", LeftFlipper, TriggerLF
	RF.SetObjects "RF", RightFlipper, TriggerRF
End Sub


'
'******************************************************
'  ZCOR:   FLIPPER CORRECTION FUNCTIONS
'******************************************************

' modified 2023 by nFozzy
' Removed need for 'endpoint' objects
' Added 'createvents' type thing for TriggerLF / TriggerRF triggers.
' Removed AddPt function which complicated setup imo
' made DebugOn do something (prints some stuff in debugger)
'   Otherwise it should function exactly the same as before\
' modified 2024 by rothbauerw
' Added Reprocessballs for flipper collisions (LF.Reprocessballs Activeball and RF.Reprocessballs Activeball must be added to the flipper collide subs
' Improved handling to remove correction for backhand shots when the flipper is raised

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt		'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay		'delay before trigger turns off and polarity is disabled
	Private Flipper, FlipperStart, FlipperEnd, FlipperEndY, LR, PartialFlipCoef, FlipStartAngle
	Private Balls(20), balldata(20)
	Private Name
	
	Dim PolarityIn, PolarityOut
	Dim VelocityIn, VelocityOut
	Dim YcoefIn, YcoefOut
	Public Sub Class_Initialize
		ReDim PolarityIn(0)
		ReDim PolarityOut(0)
		ReDim VelocityIn(0)
		ReDim VelocityOut(0)
		ReDim YcoefIn(0)
		ReDim YcoefOut(0)
		Enabled = True
		TimeDelay = 50
		LR = 1
		Dim x
		For x = 0 To UBound(balls)
			balls(x) = Empty
			Set Balldata(x) = new SpoofBall
		Next
	End Sub
	
	Public Sub SetObjects(aName, aFlipper, aTrigger)
		
		If TypeName(aName) <> "String" Then MsgBox "FlipperPolarity: .SetObjects error: first argument must be a String (And name of Object). Found:" & TypeName(aName) End If
		If TypeName(aFlipper) <> "Flipper" Then MsgBox "FlipperPolarity: .SetObjects error: Second argument must be a flipper. Found:" & TypeName(aFlipper) End If
		If TypeName(aTrigger) <> "Trigger" Then MsgBox "FlipperPolarity: .SetObjects error: third argument must be a trigger. Found:" & TypeName(aTrigger) End If
		If aFlipper.EndAngle > aFlipper.StartAngle Then LR = -1 Else LR = 1 End If
		Name = aName
		Set Flipper = aFlipper
		FlipperStart = aFlipper.x
		FlipperEnd = Flipper.Length * Sin((Flipper.StartAngle / 57.295779513082320876798154814105)) + Flipper.X ' big floats for degree to rad conversion
		FlipperEndY = Flipper.Length * Cos(Flipper.StartAngle / 57.295779513082320876798154814105)*-1 + Flipper.Y
		
		Dim str
		str = "Sub " & aTrigger.name & "_Hit() : " & aName & ".AddBall ActiveBall : End Sub'"
		ExecuteGlobal(str)
		str = "Sub " & aTrigger.name & "_UnHit() : " & aName & ".PolarityCorrect ActiveBall : End Sub'"
		ExecuteGlobal(str)
		
	End Sub
	
	' Legacy: just no op
	Public Property Let EndPoint(aInput)
		
	End Property
	
	Public Sub AddPt(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out)
		Select Case aChooseArray
			Case "Polarity"
				ShuffleArrays PolarityIn, PolarityOut, 1
				PolarityIn(aIDX) = aX
				PolarityOut(aIDX) = aY
				ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity"
				ShuffleArrays VelocityIn, VelocityOut, 1
				VelocityIn(aIDX) = aX
				VelocityOut(aIDX) = aY
				ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef"
				ShuffleArrays YcoefIn, YcoefOut, 1
				YcoefIn(aIDX) = aX
				YcoefOut(aIDX) = aY
				ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
	End Sub
	
	Public Sub AddBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If IsEmpty(balls(x)) Then
				Set balls(x) = aBall
				Exit Sub
			End If
		Next
	End Sub
	
	Private Sub RemoveBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If TypeName(balls(x) ) = "IBall" Then
				If aBall.ID = Balls(x).ID Then
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
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x)) Then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next
	End Property
	
	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x)) Then
				balldata(x).Data = balls(x)
			End If
		Next
		FlipStartAngle = Flipper.currentangle
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub

	Public Sub ReProcessBalls(aBall) 'save data of balls in flipper range
		If FlipperOn() Then
			Dim x
			For x = 0 To UBound(balls)
				If Not IsEmpty(balls(x)) Then
					if balls(x).ID = aBall.ID Then
						If isempty(balldata(x).ID) Then
							balldata(x).Data = balls(x)
						End If
					End If
				End If
			Next
		End If
	End Sub

	'Timer shutoff for polaritycorrect
	Private Function FlipperOn()
		If GameTime < FlipAt+TimeDelay Then
			FlipperOn = True
		End If
	End Function
	
	Public Sub PolarityCorrect(aBall)
		If FlipperOn() Then
			Dim tmp, BallPos, x, IDX, Ycoef, BalltoFlip, BalltoBase, NoCorrection, checkHit
			Ycoef = 1
			
			'y safety Exit
			If aBall.VelY > -8 Then 'ball going down
				RemoveBall aBall
				Exit Sub
			End If
			
			'Find balldata. BallPos = % on Flipper
			For x = 0 To UBound(Balls)
				If aBall.id = BallData(x).id And Not IsEmpty(BallData(x).id) Then
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					BalltoFlip = DistanceFromFlipperAngle(BallData(x).x, BallData(x).y, Flipper, FlipStartAngle)
					If ballpos > 0.65 Then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)								'find safety coefficient 'ycoef' data
				End If
			Next
			
			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				If ballpos > 0.65 Then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)												'find safety coefficient 'ycoef' data
				NoCorrection = 1
			Else
				checkHit = 50 + (20 * BallPos) 

				If BalltoFlip > checkHit or (PartialFlipCoef < 0.5 and BallPos > 0.22) Then
					NoCorrection = 1
				Else
					NoCorrection = 0
				End If
			End If
			
			'Velocity correction
			If Not IsEmpty(VelocityIn(0) ) Then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
				
				'If partialflipcoef < 1 Then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
				
				If Enabled Then aBall.Velx = aBall.Velx*VelCoef
				If Enabled Then aBall.Vely = aBall.Vely*VelCoef
			End If
			
			'Polarity Correction (optional now)
			If Not IsEmpty(PolarityIn(0) ) Then
				Dim AddX
				AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				
				If Enabled and NoCorrection = 0 Then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef*VelCoef)
			End If
			If DebugOn Then debug.print "PolarityCorrect" & " " & Name & " @ " & GameTime & " " & Round(BallPos*100) & "%" & " AddX:" & Round(AddX,2) & " Vel%:" & Round(VelCoef*100)
		End If
		RemoveBall aBall
	End Sub
End Class

'*********************************************************************
'  ZPOL:   FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS
'*********************************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	Dim x, aCount
	aCount = 0
	ReDim a(UBound(aArray) )
	For x = 0 To UBound(aArray)		'Shuffle objects in a temp array
		If Not IsEmpty(aArray(x) ) Then
			If IsObject(aArray(x)) Then
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	If offset < 0 Then offset = 0
	ReDim aArray(aCount-1+offset)		'Resize original array
	For x = 0 To aCount-1				'set objects back into original array
		If IsObject(a(x)) Then
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
	BallSpeed = Sqr(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)		'Set up line via two points, no clamping. Input X, output Y
	Dim x, y, b, m
	x = input
	m = (Y2 - Y1) / (X2 - X1)
	b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius
	Public Property Let Data(aBall)
		With aBall
			x = .x
			y = .y
			z = .z
			velx = .velx
			vely = .vely
			velz = .velz
			id = .ID
			mass = .mass
			radius = .radius
		End With
	End Property
	Public Sub Reset()
		x = Empty
		y = Empty
		z = Empty
		velx = Empty
		vely = Empty
		velz = Empty
		id = Empty
		mass = Empty
		radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	Dim y 'Y output
	Dim L 'Line
	'find active line
	Dim ii
	For ii = 1 To UBound(xKeyFrame)
		If xInput <= xKeyFrame(ii) Then
			L = ii
			Exit For
		End If
	Next
	If xInput > xKeyFrame(UBound(xKeyFrame) ) Then L = UBound(xKeyFrame)		'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )
	
	If xInput <= xKeyFrame(LBound(xKeyFrame) ) Then Y = yLvl(LBound(xKeyFrame) )		 'Clamp lower
	If xInput >= xKeyFrame(UBound(xKeyFrame) ) Then Y = yLvl(UBound(xKeyFrame) )		'Clamp upper
	
	LinearEnvelope = Y
End Function

'******************************************************
'  ZFLP:          FLIPPER TRICKS
'******************************************************
' To add the flipper tricks you must
'	 - Include a call to FlipperCradleCollision from within OnBallBallCollision subroutine
'	 - Include a call the CheckLiveCatch from the LeftFlipper_Collide and RightFlipper_Collide subroutines
'	 - Include FlipperActivate and FlipperDeactivate in the Flipper solenoid subs

RightFlipper.timerinterval = 1
Rightflipper.timerenabled = True

Sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
End Sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b
	Dim BOT 'Uncommented because this table does not use gBOT. All the gBOT calls have been altered to call for BOT instead
	BOT = GetBalls
	
	If Flipper1.currentangle = Endangle1 And EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'   debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					Exit Sub
				End If
			Next
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
				End If
			Next
		End If
	Else
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 Then EOSNudge1 = 0
	End If
End Sub

Dim FCCDamping: FCCDamping = 0.4

Sub FlipperCradleCollision(ball1, ball2, velocity)
	if velocity < 0.7 then exit sub		'filter out gentle collisions
    Dim DoDamping, coef
    DoDamping = false
    'Check left flipper
    If LeftFlipper.currentangle = LFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, LeftFlipper) OR FlipperTrigger(ball2.x, ball2.y, LeftFlipper) Then DoDamping = true
    End If
    'Check right flipper
    If RightFlipper.currentangle = RFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, RightFlipper) OR FlipperTrigger(ball2.x, ball2.y, RightFlipper) Then DoDamping = true
    End If
    If DoDamping Then
		coef = FCCDamping
        ball1.velx = ball1.velx * coef: ball1.vely = ball1.vely * coef: ball1.velz = ball1.velz * coef
        ball2.velx = ball2.velx * coef: ball2.vely = ball2.vely * coef: ball2.velz = ball2.velz * coef
    End If
End Sub

'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = Sqr((ax - bx) ^ 2 + (ay - by) ^ 2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) 'Distance between a point and a line where point Is px,py
	DistancePL = Abs((by - ay) * px - (bx - ax) * py + bx * ay - by * ax) / Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI / 180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax)) * 180 / PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle + 90)) + Flipper.x, Sin(Radians(Flipper.currentangle + 90)) + Flipper.y)
End Function

Function DistanceFromFlipperAngle(ballx, bally, Flipper, Angle)
	DistanceFromFlipperAngle = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Angle + 90)) + Flipper.x, Sin(Radians(angle + 90)) + Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle = Abs(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360
	
	If DistanceFromFlipper(ballx,bally,Flipper) < 48 And DiffAngle <= 90 And Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If
End Function

'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

Dim LFPress, RFPress, LFCount, RFCount
Dim LFState, RFState
Dim EOST, EOSA,Frampup, FElasticity,FReturn
Dim RFEndAngle, LFEndAngle

Const FlipperCoilRampupMode = 0 '0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
Const EOSTnew = 1.5 'EM's to late 80's - new recommendation by rothbauerw (previously 1)
'Const EOSTnew = 1.2 '90's and later - new recommendation by rothbauerw (previously 0.8)
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode
	Case 0
		SOSRampup = 2.5
	Case 1
		SOSRampup = 6
	Case 2
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.055  'EM's
'   Const EOSReturn = 0.045  'late 70's to mid 80's
'Const EOSReturn = 0.035  'mid 80's to early 90's
'   Const EOSReturn = 0.025  'mid 90's and later

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
	Flipper.eostorque = EOST * EOSReturn / FReturn
	
	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b, BOT 'Uncommented because this table does not use gBOT. All the gBOT calls have been altered to call for BOT instead
		BOT = GetBalls
		
		For b = 0 To UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= - 0.4 Then BOT(b).vely =  - 0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState)
	Dim Dir
	Dir = Flipper.startangle / Abs(Flipper.startangle) '-1 for Right Flipper
	
	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup
			Flipper.endangle = FEndAngle - 3 * Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) And FlipperPress = 1 Then
		If FCount = 0 Then FCount = GameTime
		
		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	ElseIf Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 And FlipperPress = 1 Then
		If FState <> 3 Then
			Flipper.eostorque = EOST
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If
	End If
End Sub

Const LiveDistanceMin = 5  'minimum distance In vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114 'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)
Const BaseDampen = 0.55

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
    Dim Dir, LiveDist
    Dir = Flipper.startangle / Abs(Flipper.startangle)    '-1 for Right Flipper
    Dim LiveCatchBounce   'If live catch is not perfect, it won't freeze ball totally
    Dim CatchTime
    CatchTime = GameTime - FCount
    LiveDist = Abs(Flipper.x - ball.x)

    If CatchTime <= LiveCatch And parm > 3 And LiveDist > LiveDistanceMin And LiveDist < LiveDistanceMax Then
        If CatchTime <= LiveCatch * 0.5 Then   'Perfect catch only when catch time happens in the beginning of the window
            LiveCatchBounce = 0
        Else
            LiveCatchBounce = Abs((LiveCatch / 2) - CatchTime)  'Partial catch when catch happens a bit late
        End If
        
        If LiveCatchBounce = 0 And ball.velx * Dir > 0 And LiveDist > 30 Then ball.velx = 0

        If ball.velx * Dir > 0 And LiveDist < 30 Then
            ball.velx = BaseDampen * ball.velx
            ball.vely = BaseDampen * ball.vely
            ball.angmomx = BaseDampen * ball.angmomx
            ball.angmomy = BaseDampen * ball.angmomy
            ball.angmomz = BaseDampen * ball.angmomz
        Elseif LiveDist > 30 Then
            ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
            ball.angmomx = 0
            ball.angmomy = 0
            ball.angmomz = 0
        End If
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf ActiveBall, parm
    End If
End Sub


'******************************************************
' 	ZDMP: RUBBER  DAMPENERS
'******************************************************
' These are data mined bounce curves,
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR

Sub dPosts_Hit(idx)
	RubbersD.dampen ActiveBall
	TargetBouncer ActiveBall, 1
End Sub

Sub dSleeves_Hit(idx)
	SleevesD.Dampen ActiveBall
	TargetBouncer ActiveBall, 0.7
End Sub

Dim RubbersD				'frubber
Set RubbersD = New Dampener
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	  'debug, reports In debugger (In vel, out cor); cor bounce curve (linear)

'for best results, try to match in-game velocity as closely as possible to the desired curve
'   RubbersD.addpoint 0, 0, 0.935   'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1		 'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	   'there's clamping so interpolate up to 56 at least

Dim SleevesD	'this is just rubber but cut down to 85%...
Set SleevesD = New Dampener
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	  'debug, reports In debugger (In vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'######################### Adjust these values to increase or lessen the elasticity

Dim FlippersD
Set FlippersD = New Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False
FlippersD.addpoint 0, 0, 1.1
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn   'tbpOut.text
	Public name, Threshold  'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
	End Sub
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If GameTime > 100 Then Report
	End Sub
	
	Public Sub Dampen(aBall)
		If threshold Then
			If BallSpeed(aBall) < threshold Then Exit Sub
		End If
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If debugOn Then str = name & " In vel:" & Round(cor.ballvel(aBall.id),2 ) & vbNewLine & "desired cor: " & Round(desiredcor,4) & vbNewLine & _
		"actual cor: " & Round(realCOR,4) & vbNewLine & "ballspeed coef: " & Round(coef, 3) & vbNewLine
		If Print Then Debug.print Round(cor.ballvel(aBall.id),2) & ", " & Round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef
		aBall.vely = aBall.vely * coef
		aBall.velz = aBall.velz * coef
		If debugOn Then TBPout.text = str
	End Sub
	
	Public Sub Dampenf(aBall, parm) 'Rubberizer is handle here
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If Abs(aball.velx) < 2 And aball.vely < 0 And aball.vely >  - 3.75 Then
			aBall.velx = aBall.velx * coef
			aBall.vely = aBall.vely * coef
			aBall.velz = aBall.velz * coef
		End If
	End Sub
	
	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		Dim x
		For x = 0 To UBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x) * aCoef
		Next
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
End Class

'***********************************************************************
'  ZTAC: TRACK ALL BALL VELOCITIES FOR RUBBER DAMPENER AND DROP TARGETS
'***********************************************************************

Dim cor
Set cor = New CoRTracker

Class CoRTracker
	Public ballvel, ballvelx, ballvely
	
	Private Sub Class_Initialize
		ReDim ballvel(0)
		ReDim ballvelx(0)
		ReDim ballvely(0)
	End Sub
	
	Public Sub Update()	'tracks in-ball-velocity
		Dim str, b, AllBalls, highestID
		allBalls = GetBalls
		
		For Each b In allballs
			If b.id >= HighestID Then highestID = b.id
		Next
		
		If UBound(ballvel) < highestID Then ReDim ballvel(highestID)	'set bounds
		If UBound(ballvelx) < highestID Then ReDim ballvelx(highestID)	'set bounds
		If UBound(ballvely) < highestID Then ReDim ballvely(highestID)	'set bounds
		
		For Each b In allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

' Note, cor.update must be called in a 10 ms timer. The example table uses the GameTimer for this purpose, but sometimes a dedicated timer call RDampen is used.
'
'Sub RDampen_Timer
'	Cor.Update
'End Sub


'*****************************************************************************
' ZBOU: VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'*****************************************************************************

Const TargetBouncerEnabled = 1	  '0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.9	 'Level of bounces. Recommmended value of 0.7-1

Sub TargetBouncer(aBall,defvalue)
	Dim zMultiplier, vel, vratio
	If TargetBouncerEnabled = 1 And aball.z < 30 Then
		'   debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		vel = BallSpeed(aBall)
		If aBall.velx = 0 Then vratio = 1 Else vratio = aBall.vely / aBall.velx
		Select Case Int(Rnd * 6) + 1
			Case 1
				zMultiplier = 0.2 * defvalue
			Case 2
				zMultiplier = 0.25 * defvalue
			Case 3
				zMultiplier = 0.3 * defvalue
			Case 4
				zMultiplier = 0.4 * defvalue
			Case 5
				zMultiplier = 0.45 * defvalue
			Case 6
				zMultiplier = 0.5 * defvalue
		End Select
		aBall.velz = Abs(vel * zMultiplier * TargetBouncerFactor)
		aBall.velx = Sgn(aBall.velx) * Sqr(Abs((vel ^ 2 - aBall.velz ^ 2) / (1 + vratio ^ 2)))
		aBall.vely = aBall.velx * vratio
		'   debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		'   debug.print "conservation check: " & BallSpeed(aBall)/vel
	End If
End Sub

'Add targets or posts to the TargetBounce collection if you want to activate the targetbouncer code from them
Sub TargetBounce_Hit(idx)
	TargetBouncer ActiveBall, 1
End Sub

'******************************************************
'	ZSSC: SLINGSHOT CORRECTION FUNCTIONS by apophis
'******************************************************
' To add these slingshot corrections:
'	 - On the table, add the endpoint primitives that define the two ends of the Slingshot
'	 - Initialize the SlingshotCorrection objects in InitSlingCorrection
'	 - Call the .VelocityCorrect methods from the respective _Slingshot event sub

Dim LS
Set LS = New SlingshotCorrection
Dim RS
Set RS = New SlingshotCorrection

InitSlingCorrection

Sub InitSlingCorrection
	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS
	
	RS.Object = RightSlingshot
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS
	
	'Slingshot angle corrections (pt, BallPos in %, Angle in deg)
	' These values are best guesses. Retune them if needed based on specific table research.
	AddSlingsPt 0, 0.00, - 4
	AddSlingsPt 1, 0.45, - 7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4
End Sub

Sub AddSlingsPt(idx, aX, aY)		'debugger wrapper for adjusting flipper script In-game
	Dim a
	a = Array(LS, RS)
	Dim x
	For Each x In a
		x.addpoint idx, aX, aY
	Next
End Sub


Class SlingshotCorrection
	Public DebugOn, Enabled
	Private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2
	
	Public ModIn, ModOut
	
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
		Enabled = True
	End Sub
	
	Public Property Let Object(aInput)
		Set Slingshot = aInput
	End Property
	
	Public Property Let EndPoint1(aInput)
		SlingX1 = aInput.x
		SlingY1 = aInput.y
	End Property
	
	Public Property Let EndPoint2(aInput)
		SlingX2 = aInput.x
		SlingY2 = aInput.y
	End Property
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If GameTime > 100 Then Report
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
	
	Public Sub VelocityCorrect(aBall)
		Dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then
			XL = SlingX1
			YL = SlingY1
			XR = SlingX2
			YR = SlingY2
		Else
			XL = SlingX2
			YL = SlingY2
			XR = SlingX1
			YR = SlingY1
		End If
		
		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then
			If Abs(XR - XL) > Abs(YR - YL) Then
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If
		
		'Velocity angle correction
		If Not IsEmpty(ModIn(0) ) Then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'   debug.print " BallPos=" & BallPos &" Angle=" & Angle
			'   debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled Then aBall.Velx = RotVxVy(0)
			If Enabled Then aBall.Vely = RotVxVy(1)
			'   debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely
			'   debug.print " "
		End If
	End Sub
End Class

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function


