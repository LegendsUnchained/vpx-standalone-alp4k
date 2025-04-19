Option Explicit       
'                                                                   
'     _,---.             .=-.-..-._        ,--.--------.   ,-,--.  ,--.--------.   _,.---._    .-._           ,----.    ,-,--.  
'  .-`.' ,  \   _.-.    /==/_ /==/ \  .-._/==/,  -   , -\,-.'-  _\/==/,  -   , -\,-.' , -  `. /==/ \  .-._ ,-.--` , \ ,-.'-  _\ 
' /==/_  _.-' .-,.'|   |==|, ||==|, \/ /, |==\.-.  - ,-./==/_ ,_.'\==\.-.  - ,-./==/_,  ,  - \|==|, \/ /, /==|-  _.-`/==/_ ,_.' '
'/==/-  '..-.|==|, |   |==|  ||==|-  \|  | `--`\==\- \  \==\  \    `--`\==\- \ |==|   .=.     |==|-  \|  ||==|   `.-.\==\  \    
'|==|_ ,    /|==|- |   |==|- ||==| ,  | -|      \==\_ \  \==\ -\        \==\_ \|==|_ : ;=:  - |==| ,  | -/==/_ ,    / \==\ -\   
'|==|   .--' |==|, |   |==| ,||==| -   _ |      |==|- |  _\==\ ,\       |==|- ||==| , '='     |==| -   _ |==|    .-'  _\==\ ,\  
'|==|-  |    |==|- `-._|==|- ||==|  /\ , |      |==|, | /==/\/ _ |      |==|, | \==\ -    ,_ /|==|  /\ , |==|_  ,`-._/==/\/ _ | 
'/==/   \    /==/ - , ,/==/. //==/, | |- |      /==/ -/ \==\ - , /      /==/ -/  '.='. -   .' /==/, | |- /==/ ,     /\==\ - , / 
'`--`---'    `--`-----'`--`-` `--`./  `--`      `--`--`  `--`---'       `--`--`    `--`--''   `--`./  `--`--`-----``  `--`---'  
'Williams 1994 
'                                     .:::::::::.
'                                    .::::::::::::::::,       .::
'                                  -'`;. ccccr -ccc,```'::,:::::::
'                                     `,z$$$$$$c $$$F.::::::::::::
'                                      'c`$'cc,?$$$$ :::::`:. ``':
'                                      $$$`4$$$,$$$$ :::',   `
'                                ..    F  .`$   $$"$L,`,d$c$
'                               d$$$$$cc,,d$c,ccP'J$$$$,,`"$F
'                               $$$$$$$$$$$$$$$$$$$$$$$$$",$F
'                               $$$$$$$$$$$ ccc,,"?$$$$$$c$$F
'                               `?$$$PFF",zd$P??$$$c?$$$$$$$F
'                              .,cccc=,z$$$$$b$ c$$$ $$$$$$$
'                           cd$$$F",c$$$$$$$$P'<$$$$ $$$$$$$
'                           $$$$$$$c,"?????""  $$$$$ $$$$$$F
'                       ::  $$$$L ""??"    .. d$$$$$ $$$$$P'..
'                       ::: ?$$$$J$$cc,,,,,,c$$$$$$PJ$P".::::
'                  .,,,. `:: $$$$$$$$$$$$$$$$$$$$$P".::::::'
'        ,,ccc$$$$$$$$$P" `::`$$$$$$$$$$$$$$$$P".::::::::' c$c.
'  .,cd$$PPFFF????????" .$$$$$b,
'z$$$$$$$$$$$$$$$$$$$$bc.`'!>` -:.""?$$P".:::'``. `',<'` $$$$$$$$$c
'$$$$$$$$$$$$$$$$$$$$$$$$$c,=$$ :::::  -`',;;!!!,,;!!>. J$$$$$$$$$$b,
'?$$$$$$$$$$$$$$$$$$$$$$$$$$$cc,,,.` ."?$$$$$$$$$$$$$$$$$$.
'     ""??"""   ;!!!.$$$ `?$$$$$$P'!!!!;     !!;.""?$$$$$$$$$$$$$$$r
'               !!!'<$$$ :::..  .;!!!!!!;   !!!!!!!!!!!!!>  "?$$$$$$$$$$$"
'              !!!!>`?$F::::::::`!!!!!!!!! ?"
'                  `!!!!>`::::: :: 
'               `    `!!! `:::: ,, ;!!!!!!!!!'`    ;!!!!!!!!!!!
'                \;;;;!!!! :::: !!!!!!!!!!!       ;!!!!!!!!!!!!>
'                `!!!!!!!!> ::: !!!!!!!!!!!      ;!!!!!!!!!!!!!!>
'                 !!!!!!!!!!.` !!!!!!!!!!!!!;. ;!!!!!!!!!!!!!!!!>
'                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
'                  `!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
'                   `!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
'                    `
'                       ?$$c``!!! d $$c,c$.`!',d$$$P
'                           `$$$$$c,,d$ 3$$$$$$cc$$$$$$F
'                            `$$$$$$$$$b`$$$$$$$$$$$$$$
'                             `$$$$$$$$$ ?$$$$$$$$$$$$$
'                              `$$$$$$$$$ $$$$$$$$$$$$F
'                               `$$$$$$$$,?$$$$$$$$$$$'
'                                `$$$$$$$$ $$$$$$$$$$P
'                                  ?$$$$$$b`$$$$$$$$$F
'                                ,c$$$$$$$$c`$$"$$$$$$$cc,
'                            ,z$$$$$$$$$$$$$ $L')$$$$$$$$$$b,,,,, ,
'                       ,,-=P???$$$$$$$$$$PF $$$$$$$$$$$$$Lz =zr4%'
'                      `?'d$ $b = $$$$$$           "???????$$$P
'                         `"-"$$$$P""""                     "

'*****************************************************************************************************
' CREDITS
' Authors: g5k, 3rdaxis, DJRobX
' Bronto, Dictabird models and 3d scan cleanup of building toys: Dark
' Color DMD : Slippifishi and Wob - To be available at vpuniverse.com 
' Legends: SlyDog43, Dave Conn
' Some stuff from the vp9 version, DOF code etc (thanks to JPSalas and those who helped make that original one)
' DOF Updates: Arngrim
' Shout out to the VPX and VPM dev teams!
' Big thanks to all those who pitched in to help make this happen
' Yabba Dabba Doo
'*****************************************************************************************************
       

'First, try to load the Controller.vbs (DOF), which helps controlling additional hardware like lights, gears, knockers, bells and chimes (to increase realism)
'This table uses DOF via the 'SoundFX' calls that are inserted in some of the PlaySound commands, which will then fire an additional event, instead of just playing a sample/sound effect

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the Controller.vbs file in order to run this table (installed with the VPX package in the scripts folder)"
On Error Goto 0
Const cGameName = "fs_lx5"



'*****************************************************************************************************
'SCRIPT OPTIONS
'*****************************************************************************************************

Dim LUTmeUP:LUTMeUp = 0 '0 = No LUT, will look nice and bright, 1 = 30% contrast and brightness adj, 2 = 50% contrast and brightness adj, 3 = 70% contrast and brightness adj, 4 = 100% contrast and brightness adj, 5 = 130% contrast and brightness
Dim DisableLUTSelector:DisableLUTSelector = 1  ' Disables the ability to change LUT option with magna saves in game when set to 1
Const UseSolenoids=2 'FastFlips
Const FlipperShadows = 0 ' change to 0 to turn off flipper shadows
Const OutlaneDifficulty = 2 ' 1 = EASY, 2 = MEDIUM (Factory), 3 = HARD 
Const BallShadowOn = 0  '0=Off 1=On (Off=Performance On=Quality)
Const GiMethod = 1 ' 1 = GI control by materials less overhead; 2 = GI Double Prims, this has more overhead and will not run on shite setups
Const PreloadMe = 1  ' To prevent in-game slowdowns
Const VolRoll = 70 ' 0..100.  Ball roll volume
Const FlasherIntensity = 200' (0-1000) 200 = Default. Can be higher or lower (i.e. 220 to make them brighter, 180 to make them more dull)
Const Flasher4k = 0 '0 Uses 1024x1024 flasher overlays will help performance on systems experience frame dips in lightshow moments, 1 = 4K Overlays for beasts
					'If you still have performance issues you will need to select all the primitives on layer 4,5 and 6 and deselect "Reflection Enabled". 
					'Disabling playfield reflections would be best but at time of release this was creating an error with the inserts. Future VPX updates may fix this.


Const UseLamps=0,UseGI=1,SSolenoidOn="SolOn",SSolenoidOff="SolOff", SCoin="coin",SKickerOn="RearScoop"
Dim UseVPMDMD:UseVPMDMD = True
Const UseVPMModSol=1
Const MaxLut = 4

If Table1.ShowDT = false then
    Scoretext.Visible = false
	UseVPMDMD = False
End If


LoadVPM "01560000", "WPC.VBS", 3.26

Dim bsTrough, bsKicker36, dtLeftDrop, dtRightDrop, ttMachine
Set GiCallback2 = GetRef("UpdateGI")

Dim EnableBallControl
EnableBallControl = false 'Change to true to enable manual ball control (or press C in-game) via the arrow keys and B (boost movement) keys

Dim NullFader : set NullFader = new NullFadingObject
Dim FadeLights : Set FadeLights = New LampFader
Dim GI_STATUS
Dim DesktopMode: DesktopMode = Table1.ShowDT
'Dim autoflip 'AXS
'autoflip=0'(For Stress Testing)>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

'Const BallSize = 25  'Ball radius



Sub Table1_Init

    if table1.VersionMinor < 6 AND table1.VersionMajor = 10 then MsgBox "This table requires VPX 10.6, you have " & table1.VersionMajor & "." & table.VersionMinor
	if VPinMAMEDriverVer < 3.57 then MsgBox "This table requires core.vbs 3.57 or higher, which is included with VPX 10.6.  You have " & VPinMAMEDriverVer & ". Be sure scripts folder is up to date, and that there are no old .vbs files in your table folder."

	vpmInit Me
    NoUpperLeftFlipper
    NoUpperRightFlipper

     With Controller
        .GameName = cGameName
          If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
         .SplashInfoLine = "The Flintstones - based on the table by Williams from 1994" & vbNewLine & "VPX table by g5k, 3rdAxis, DJRobX and Dark"
          'DMD position and size for 1400x1050
         '.Games(cGameName).Settings.Value("dmd_pos_x")=500
         '.Games(cGameName).Settings.Value("dmd_pos_y")=2
         '.Games(cGameName).Settings.Value("dmd_width")=400
         '.Games(cGameName).Settings.Value("dmd_height")=92
         .Games(cGameName).Settings.Value("rol") = 0
         .HandleKeyboard = 0
         .ShowTitle = 0
         .ShowDMDOnly = 1
         .ShowFrame = 0
         .HandleMechanics = 0
         .Hidden = 0
         '.SetDisplayPosition 0, 0, GetPlayerHWnd 'uncomment this line if you don't see the DMD
          On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
         .Switch(22) = 1 'close coin door
         .Switch(24) = 1 'and keep it close
     End With
 
     ' Nudging
     vpmNudge.TiltSwitch = 14
     vpmNudge.Sensitivity = 1
    ' vpmNudge.TiltObj = Array(bumper1, bumper2, bumper3, LeftSlingshot, RightSlingshot)
 		
     ' Trough
     Set bsTrough = New cvpmTrough
     With bsTrough
		 .Size = 4
         .InitSwitches Array(32, 33, 34, 35) 
         .InitExit BallRelease, 90, 4
         .InitExitSounds  SoundFX(SSolenoidOn,DOFContactors), SoundFX("BallRelease",DOFContactors)
         .Balls = 4
     End With

	' Bronto/Machine VUK
	Set bsKicker36 = New cvpmSaucer
	With bsKicker36
		.InitKicker Kicker36, 36, 0, 35, 1.56
		.InitSounds "kicker_enter_center", SoundFX(SKickerOn,DOFContactors), SoundFX(SKickerOn,DOFContactors)
		.CreateEvents "bsKicker36", Kicker36
	End With

     ' Left Drop Targets
     Set dtLeftDrop = New cvpmDropTarget
     With dtLeftDrop
         .InitDrop Array(sw45, sw46, sw47), Array(45, 46, 47)
         .initsnd "droptarget", SoundFX("DTReset", DOFContactors)
         .CreateEvents "dtLeftDrop"
     End With
 
     ' Right Drop Targets
     Set dtRightDrop = New cvpmDropTarget
     With dtRightDrop
         .InitDrop Array(sw41, sw42, sw43, sw44), Array(44, 43, 42, 41)
         .initsnd SoundFX("droptarget", DOFDropTargets), SoundFX("DTReset",DOFContactors)
         .CreateEvents "dtRightDrop"
     End With


     ' Machine Toy
     Set ttMachine = New cvpmTurnTable
     With ttMachine
         .InitTurnTable ttMachineTrigger, 32
         .SpinUp = 32
         .SpinDown = 25
         .SpinCW = True
         .CreateEvents "ttMachine"
     End With
 
	PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1
    MachineLock.Collidable = false

	AutoPlunger.Pullback

	LUTBox.Visible = 0
	SetLUT

	if Flasher4k = 1 Then
		FlPf17.ImageA = "fl17"
		FlPf17.ImageB = "fl17"
		FlPf18.ImageA = "fl18"
		FlPf18.ImageB = "fl18"
		FlPf19.ImageA = "fl19"
		FlPf19.ImageB = "fl19"
		FlPf20.ImageA = "fl20"
		FlPf20.ImageB = "fl20"
		FlPf21.ImageA = "fl21"
		FlPf21.ImageB = "fl21"
		FlPf22.ImageA = "fl22"
		FlPf22.ImageB = "fl22"
		FlPf24.ImageA = "fl24"
		FlPf24.ImageB = "fl24"
		FlPf25.ImageA = "fl25"
		FlPf25.ImageB = "fl25"
		FlPf28.ImageA = "fl28"
		FlPf28.ImageB = "fl28"
	end If

	If DesktopMode = True Then 
		Bar_Rails.visible=True
	  Else
		Bar_Rails.visible=False
		
	end If

If Table1.ShowDT = false then 'AXS
    Fl1.State = 1
else
    Fl1.State = 0
End If

End Sub

Sub SetLUT
	Select Case LUTmeUP
		Case 0:table1.ColorGradeImage = 0
		Case 1:table1.ColorGradeImage = "AA_FS_Lut30perc"
		Case 2:table1.ColorGradeImage = "AA_FS_Lut50perc"
		Case 3:table1.ColorGradeImage = "AA_FS_Lut70perc"
		Case 4:table1.ColorGradeImage = "AA_FS_Lut100perc"
	end Select
end sub 

Sub LUTBox_Timer
	LUTBox.TimerEnabled = 0 
	LUTBox.Visible = 0
End Sub

Sub ShowLUT
	LUTBox.visible = 1
	LUTBox.text = "LUTmeUP: " & CStr(LUTmeUP)
	LUTBox.TimerEnabled = 1
End Sub

sub Drain_Hit
	RandomSoundDrain
	bsTrough.AddBall me
end sub 

'***********
' Update GI
'***********

Dim LastGi0:LastGi0 = 7
Dim LastGi1:LastGi1 = 7

Dim LastGiDir:LastGiDir = 0

Sub UpdateGI(no, value)

	
	dim obj, GiDir
	Select Case no
		case 0:
				
				if value >= 7 Then 
					RG13_Plastics_Machine_GiOff.Visible=0
					TurnTable_giOFF.Visible=0
					RG_Bulbs_giOFF_Machine.Visible = 0
					RG4_giOFF_Machine.Visible = 0
					RG_Bulbs_Machine.visible = 1
					Fl001.IntensityScale = 1
					RearBulbsCard.image = "RearWall_GI8"
				    LastGi1 = value 

					LastGi0 = value
					'debug.print "GI: " & CStr(no) & " to " & CStr(value) & " lastgi0 " & LastGi0 & " lastgi1 " & LastGi1 & " LastGiDir = " & GiDir 

					LastGiDir = 0
				else	
					if value < LastGi0 then GiDir = -1 else GiDir = 1

					if LastGi0 >= 7 or LastGi0 <= 1 or ((LastGiDir = 0 or GiDir = LastGiDir) and abs(LastGi0 - value < 2) and value <> LastGi1) then  ' VPM output seems to be a little glitchy, throw out changes in the wrong direction if in the middle of a fade sequence
						RG13_Plastics_Machine_GiOff.Visible=1
						RG13_Plastics_Machine_GiOff.material = "GIShading_" & (value)
						TurnTable_giOFF.Visible=1
						TurnTable_giOFF.material = "GIShading_" & (value)
						RG_Bulbs_giOFF_Machine.Visible=1
						RG_Bulbs_giOFF_Machine.material = "GIShading_" & (value)
						if value <2 Then RG_Bulbs_Machine.Visible = 0 Else RG_Bulbs_Machine.visible = 1
						if GI_STATUS = 1 then 
							RG4_giOFF_Machine.Visible=1
							RG4_giOFF_Machine.material = "GIShading_" & (value)
						Else
							RG4_giOFF_Machine.Visible=0
						end if 
						Fl001.IntensityScale = (Value-1) / 7
						RearBulbsCard.image = "RearWall_GI" & (value)
						LastGi1 = LastGi0
						LastGi0 = value
						if LastGi0 < 2 or LastGi0 >= 6 then LastGiDir = 0 else LastGiDir = GiDir
						'debug.print "GI: " & CStr(no) & " to " & CStr(value) & " lastgi0 " & LastGi0 & " lastgi1 " & LastGi1 & " LastGiDir = " & GiDir 
					Else	
						'debug.print "RejectGI: " & CStr(no) & " to " & CStr(value) & "lastgi0 " & LastGi0
					end if
					
				end if 
				
	
			
		Case 2

			'Table1.ColorGradeImage = "LUT1_1_0" & (8-value)  '''''' GI Fading via LUT (removed)
			'PF_GiON_Flasher.IntensityScale = (value/8) '''''''''''''Additive GI playfield method (removed)


			'GiOFF Playfield fade up/down
			'if value <7 Then PF_GiOFF.Opacity = 100-(value*14.28) else PF_GiOFF.Opacity = 0 end If
			fl1.IntensityScale = (Value-1) / 7

if value <7 Then
	DOF 104, DOFOff
else
	DOF 104, DOFOn
end if

select case value
		case 0: 
			PF_GiOFF.Opacity = 100
			GI_STATUS=0
			'SpotlightBeam.image = "BulbAlpha_GI0"
			
		case 1:
			PF_GiOFF.Opacity = 100
			GI_STATUS=0
			'SpotlightBeam.image = "BulbAlpha_GI1"
			
		case 2: 
			PF_GiOFF.Opacity = 85
			GI_STATUS=1
			'SpotlightBeam.image = "BulbAlpha_GI2"
			
		case 3:
			PF_GiOFF.Opacity = 68
			GI_STATUS=1
			'SpotlightBeam.image = "BulbAlpha_GI3"

		case 4:
			PF_GiOFF.Opacity = 51
			GI_STATUS=1
			'SpotlightBeam.image = "BulbAlpha_GI4"
			
		case 5:
			PF_GiOFF.Opacity = 34
			GI_STATUS=1
			'SpotlightBeam.image = "BulbAlpha_GI5"
			
		case 6:
			PF_GiOFF.Opacity = 17
			GI_STATUS=1
			'SpotlightBeam.image = "BulbAlpha_GI6"
	
		case 7:
			PF_GiOFF.Opacity = 0
			GI_STATUS=1
			'SpotlightBeam.image = "BulbAlpha_GI7"
			
		case 8:
			PF_GiOFF.Opacity = 0
			GI_STATUS=1
			'SpotlightBeam.image = "BulbAlpha_GI8"
			

		end select 







if GiMethod = 2 then

			' This is the double prim method

			for each obj in GIOFF_Collection	
				obj.material = "GIShading_" & (value)
				if value >= 7 Then obj.Visible=0
				if value <7 Then obj.Visible=1
				if value <2 Then RG_Bulbs.Visible = 0 Else RG_Bulbs.visible = 1
			
			next

			for each obj in GION_DuplicateSet
				if value <2 Then obj.Visible=0 else obj.Visible=1
			next







			for each obj in GIOFF_MaterialShade	
				obj.material = "GIMATERIALShading" & (value)
			next

Elseif GiMethod = 1 then

			' This is the single prim method

			for each obj in GIOFF_Collection
				obj.Visible=0
			Next


			for each obj in GIOFF_SinglePrimMethod	
				obj.material = "GIMATERIALShading" & (value)
			next

End If


' For each lightobj in Lamps
	end select
end sub
             

' *****  Drop targets with ball hop  *************

Dim HopLeftBall:HopLeftBall = Empty
Dim HopRightBall:HopRightBall = Empty

Sub SolLeftDrop(Enabled)
	dtLeftDrop.SolDropUp Enabled
	If Not IsEmpty(HopLeftBall) Then
 	   HopLeftBall.velZ = 10
          SlingHopTimer.Enabled = 1
	End If
End sub

Sub SolRightDrop(Enabled)
	dtRightDrop.SolDropUp Enabled
	If Not IsEmpty(HopRightBall) Then
 	   HopRightBall.velZ = 10
	SlingHopTimer.Enabled = 1
	End If
End sub

Sub TargetResetHopLeft_Hit
	Set HopLeftBall = ActiveBall
End Sub

Sub TargetResetHopLeft_UnHit
    HopLeftBall = Empty
End Sub

Sub TargetResetHopRight_Hit
	Set HopRightBall = ActiveBall
End Sub

Sub TargetResetHopRight_UnHit
    HopRightBall = Empty
End Sub


' *** Nudge 

Dim mMagnet, cBall

Sub WobbleMagnet_Init
	 Set mMagnet = new cvpmMagnet
	 With mMagnet
		.InitMagnet WobbleMagnet, 1
		.Size = 100
		.CreateEvents mMagnet
		.MagnetOn = True
		.GrabCenter = False
	 End With
	Set cBall = ckicker.CreateBall
'	Set cBall = ckicker.CreateSizedBallWithMass(25, 1)
	ckicker.Kick 0,0:mMagnet.addball cball
End Sub


Sub ShakeTimer_Timer
	dim NudgeAmount:NudgeAmount = cball.y - ckicker.y
	if abs(NudgeAmount) > .3 then
		cball.x = ckicker.x
		cball.y = ckicker.y 
		cball.velx = 0
		cball.vely = 0
		NudgePins(NudgeAmount)
		if abs(NudgeAmount) > 4 then HTBronto.Start
	end if 	
	UpdatePins
End Sub

Dim PinAngleMax:PinAngleMax = 30
Dim PinAngleMin:PinAngleMin = -30
Dim PinAngle(5)
Dim PinSpeed(5)
Dim PinObjs:PinObjs = Array(BPin1, BPin2, BPin3, BPin4, BPin5)
Dim PinDamping:PinDamping = 0.985
Dim PinGravity:PinGravity = 1
InitPins

Sub InitPins
	dim i 
	for i=0 to 4
		PinAngle(i) = 0
		PinSpeed(i) = 0
	Next
end sub 

Sub UpdatePins
	dim i
	for i=0 to 4
		UpdatePin(i)
	Next
End Sub 

Sub NudgePins(NudgeAmount)
	dim i 
	for i=0 to 4
		PinSpeed(i) = PinSpeed(i) + (NudgeAmount + (-.5 + rnd(1)))  
	Next
End Sub

Sub UpdatePin(n)
	if abs(PinSpeed(n)) <> 0.0 Then
		PinSpeed(n) = PinSpeed(n) - sin(PinAngle(n) * 3.14159 / 180) * PinGravity
		PinSpeed(n) = PinSpeed(n) * PinDamping
	end if
	PinAngle(n) = PinAngle(n) + PinSpeed(n)
	if PinAngle(n) > PinAngleMax Then
		PinAngle(n) = PinAngleMax
		PinSpeed(n) = -PinSpeed(n)
		PinSpeed(n) = PinSpeed(n) * PinDamping * 0.8
	elseif PinAngle(n) < PinAngleMin Then
		PinAngle(n) = PinAngleMin
		PinSpeed(n) = -PinSpeed(n)
		PinSpeed(n) = PinSpeed(n) * PinDamping * 0.8
	end if
	'debug.print PinSpeed & "  " & PinAngle
	'BowlingPin1.ObjRotX = (cball.y - ckicker.y)*2
	PinObjs(n).ObjRotX = PinAngle(n)
'	PinAngle = PinAngle + PinVel
'	if abs(PinAngle) > PinMax then PinVel = -PinVel
End Sub

Sub BPTrigger_Hit
	RandomSoundFlipper
    NudgePins(10)
End Sub


Dim GIInit: GIInit=10 * 4

Sub PreloadImages
	If PreloadMe = 1 and GIInit > 0 Then
		GIInit = GIInit -1
		select case (GIInit \ 4) ' Divide by 4, this is not a frame timer, so we want to be sure frame is visible 
		case 0:
				FlipperL.image="leftflipper_giON_BLK"
				FlipperR.image="rightflipper_giON_BLK"
				FlipperR1.image="rightUPPERflipper_giON_BLK"

		case 1:
				FlipperL.image="leftflipper_giOFF_BLK"
				FlipperR.image="rightflipperUP_giOFF_BLK"
				FlipperR1.image="rightUPPERflipperUP_giOFF_BLK"
		case 2:
				FlipperL.image="leftflipperUP_giON_BLK"
				FlipperR.image="rightflipper_giOFF_BLK"
				FlipperR1.image="rightUPPERflipper_giOFF_BLK"
		case 3:
				FlipperL.image="leftflipperUP_giOFF_BLK"
				FlipperR.image="rightflipperUP_giON_BLK"
				FlipperR1.image="rightUPPERflipperUP_giON_BLK"
		end select 
	End If
End Sub 


Set MotorCallback = GetRef("RealTimeUpdates")

Sub RealTimeUpdates

	Dim chgLamp, num, chg, ii
	chgLamp = Controller.ChangedLamps
	If Not IsEmpty(chgLamp) Then
		For ii = 0 To UBound(chgLamp)
			 SetLamp ChgLamp(ii,0), ChgLamp(ii,1)
		Next
	End If
	FadeLights.Update
	UpdateTheMachine
	PreloadImages

	IF GI_STATUS = 0 Then
		If FlipperLeft.CurrentAngle < 80 Then
				FlipperL.image = "leftflipperUp_giOff_BLK"
		Else
			FlipperL.image = "leftflipper_giOFF_BLK"
		End If
		
		If FlipperRight.CurrentAngle < -80 Then
				FlipperR.image = "rightflipper_giOFF_BLK"
		Else
				FlipperR.image = "rightflipperUP_giOFF_BLK"
		End If

		If FlipperRight1.CurrentAngle < -125 Then
				FlipperR1.image = "rightUPPERflipper_giOFF_BLK"
		Else			
			FlipperR1.image = "rightUPPERflipperUp_giOFF_BLK"			
		End If

	

	Elseif GI_STATUS = 1 Then
		If FlipperLeft.CurrentAngle < 80 Then
				FlipperL.image = "leftflipperUP_giON_BLK"
		Else
			FlipperL.image = "leftflipper_giON_BLK"
		End If

		If FlipperRight.CurrentAngle < -80 Then
			FlipperR.image = "rightflipper_giON_BLK"
		Else
			FlipperR.image = "rightflipperUP_giON_BLK"
		End If

		If FlipperRight1.CurrentAngle < -125 Then
				FlipperR1.image = "rightUPPERFlipper_giON_BLK"
			Else
				FlipperR1.image = "rightUPPERflipperUP_giON_BLK"
		End If
	End if	



	WireGate.rotx= 0- Sw38.Currentangle / 1
	WireGate2.rotx= 0 - Gate5.Currentangle / 1
	GateFlapLeft.roty= 0- Gate4.Currentangle / 1
	GateFlapRight.roty= 150 - Gate2.Currentangle / -1

	flipperL.RotZ = FlipperLeft.CurrentAngle
	flipperR.RotZ = FlipperRight.CurrentAngle
	flipperR1.RotZ = FlipperRight1.CurrentAngle

	if BallShadowOn = 1 then BallShadowUpdate

	If FlipperShadows = 1 Then
		FlipperShadowL.RotZ = FlipperLeft.currentAngle
		FlipperShadowR.RotZ = FlipperRight.currentAngle
		FlipperShadowR1.RotZ = FlipperRight1.currentAngle
	End If

        If OutlaneDifficulty = 1 Then 'AXS
		OutlaneLeft1.Collidable = True
		OutlaneLeft2.Collidable = False
		OutlaneLeft3.Collidable = False

		OutlaneRight1.Collidable = True
		OutlaneRight2.Collidable = False
		OutlaneRight3.Collidable = False

'		InlaneLeft1.Collidable = True
'		InlaneLeft2.Collidable = False
'		InlaneLeft3.Collidable = False
'
'		InlaneRight1.Collidable = True
'		InlaneRight2.Collidable = False
'		InlaneRight3.Collidable = False
        End If

        If OutlaneDifficulty = 2 Then 
		OutlaneLeft1.Collidable = False
		OutlaneLeft2.Collidable = True
		OutlaneLeft3.Collidable = False

		OutlaneRight1.Collidable = False
		OutlaneRight2.Collidable = True
		OutlaneRight3.Collidable = False

'		InlaneLeft1.Collidable = False
'		InlaneLeft2.Collidable = True
'		InlaneLeft3.Collidable = False
'
'		InlaneRight1.Collidable = False
'		InlaneRight2.Collidable = True
'		InlaneRight3.Collidable = False
        End If

        If OutlaneDifficulty = 3 Then 
		OutlaneLeft1.Collidable = False
		OutlaneLeft2.Collidable = False
		OutlaneLeft3.Collidable = True

		OutlaneRight1.Collidable = False
		OutlaneRight2.Collidable = False
		OutlaneRight3.Collidable = True

'		InlaneLeft1.Collidable = False
'		InlaneLeft2.Collidable = False
'		InlaneLeft3.Collidable = True
'
'		InlaneRight1.Collidable = False
'		InlaneRight2.Collidable = False
'		InlaneRight3.Collidable = True
        End If

End Sub

 SolCallback(1) = "SolRelease"
 SolCallback(2) = "vpmSolAutoPlungeS AutoPlunger, SoundFX(SSolenoidOn, DOFContactors), 8,"
 SolCallback(3) = "SolTopDiverter"
 SolCallback(4) = "bsKicker36.SolOut"
 SolCallback(5) = "SolLeftDrop" 
 SolCallback(6) = "SolRightDrop"
 SolCallback(7) = "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
 SolCallback(8) = "SolBrontoDiverter"
 SolCallback(9) = "sRightSlingshot"
 SolCallback(10) = "sLeftSlingshot"
 SolCallback(14) = "SetLamp 114," 
 SolCallBack(15) = "SolLeftApronDiverter"
 SolCallback(16) = "SolRightApronDiverter"
 SolModCallback(17) = "FadeLights.LampMod 117,"
 SolModCallback(18) = "FadeLights.LampMod 118,"
 SolModCallback(19) = "FadeLights.LampMod 119,"
 SolModCallBack(20) = "FadeLights.LampMod 120,"
 SolModCallback(21) = "FadeLights.LampMod 121,"
 SolModCallback(22) = "FadeLights.LampMod 122," 
 SolCallback(23) = "SolMachine"
 SolModCallback(24) = "FadeLights.LampMod 124,"
 SolModCallback(25) = "FadeLights.LampMod 125,"
 SolCallback(26) = ""
 SolCallback(27) = ""
 SolModCallBack(28) =  "Flasher28" 
 SolCallback(35) = "SolGateRGate" '"vpmSolGate RGate,false,"
 SolCallback(36) = "SolGateLGate" '"vpmSolGate LGate,false,"
 SolCallback(sLRFlipper) = "SolRFlipper"
 SolCallback(sLLFlipper) = "SolLFlipper"

Sub  Flasher28(Intensity)
	FadeLights.LampMod 128,(Intensity / 5)
	RG5_PlasticsFlasher.BlendDisableLighting = Intensity / 255
	RG13_PlasticsFlasher.BlendDisableLighting = Intensity / 255
	if Intensity = 0 Then
		RG5_PlasticsFlasher.visible = false
		RG13_PlasticsFlasher.visible = False
	Fl002.State=0
	Fl003.State=0
	Fl004.State=0
	Else	
		RG5_PlasticsFlasher.visible = true
		RG13_PlasticsFlasher.visible = true
	
		RG5_PlasticsFlasher.material = "GIShading_" & ((255-Intensity) * 6 \ 255)
		RG13_PlasticsFlasher.material = "GIShading_" & ((255 - Intensity)  * 6 \ 255)
	Fl002.State=1
	Fl003.State=1
	Fl004.State=1
	end if 
'	if Enabled Then
'		RG5_Plastics.image = "Flasher28FredsChoiceB"
'		RG13_Plastics.image = "Flasher28FredsChoiceAC"
'	else
'		RG5_Plastics.image = "RG_15_plastics_giON_AXS"
'		RG13_Plastics.image = "RG_13_giON"
'	end if
end sub



' ************************************************
' The Machine
' ************************************************

dim MachineSpeedMax:MachineSpeedMax = 20.0
dim MachineSpeedCur:MachineSpeedCur = 0	
dim MachineRamp:MachineRamp = .5
dim MachinePos:MachinePos = 0 

Sub SolMachine(Enabled)
     If Enabled Then
         ttMachine.MotorOn = 1
     Else
         ttMachine.MotorOn = 0
     End If
End Sub

Sub UpdateTheMachine
	if ttMachine.MotorOn = 1 Then
		if MachineSpeedCur < MachineSpeedMax then MachineSpeedCur = MachineSpeedCur + MachineRamp
		if MachineSpeedCur > MachineSpeedMax then MachineSpeedCur = MachineSpeedMax
	Else
		if MachineSpeedCur > 0 then 
			MachineSpeedCur = MachineSpeedCur - MachineRamp
			if MachineSpeedCur <= 0 then 
				MachineSpeedCur = 0
				StopSound "machine"
			end if
		end if
	end if 
	
	' This is a bit of a dirty trick to help keep the balls spinning in the machine longer.   Blocks the exit at certain angles.
	if MachinePos < 90 and ttMachine.MotorOn then 
		MachineLock.Collidable = true
	Else		
		MachineLock.Collidable = False
	end if 
	if MachineSpeedCur > 0 then 
		PlaySound SoundFX("machine", DOFGear), -1, MachineSpeedCur / MachineSpeedMax / 400, AudioPan(ttMachineTrigger), 0, MachineSpeedCur * 100000/ MachineSpeedMax, 1, 0, AudioFade(ttMachineTrigger)
		MachinePos = MachinePos - MachineSpeedCur
		if MachinePos < 0 then MachinePos = MachinePos + 360

		TurnTable.rotz = MachinePos
		TurnTable.image = "Turntable_giON_" & Right("00" & CStr(Int(MachinePos / 2.5)), 3)
		TurnTable_giOFF.rotz = MachinePos
		'TurnTableNut.rotz = MachinePos 'AXS
		TurnTable_giOFF.image = "Turntable_giOFF_" & Right("00" & CStr(Int(MachinePos / 2.5)), 3)
		dim x: x= Int(MachinePos / 2.5)
		'if x < 0 or x > 143 then debug.print "X out of range " & CStr(x) & " for degree " & MachinePos
	end if 
End Sub

' ************************************************
' Dictabird
' ************************************************

Sub UpdateDictabird(Value)
	Dictabird.objrotx = -7 + (-Value) * 9 
End Sub


Sub SolBrontoDiverter(Enabled)
	if Enabled Then
		BrontoDiverter.RotateToEnd'BrontoDiverter.RotateToStart'
		Diverter.TransX = -40
	Else
		BrontoDiverter.RotateToStart'BrontoDiverter.RotateToEnd'
		Diverter.TransX = 0
	end if 
End Sub

Sub SolLeftApronDiverter(Enabled)
 If Enabled Then
	 'LeftApronGate.RotatetoStart
         DiverterLeft.TransZ  = -40
	 DiverterLeft.Collidable = 0
         PlaySoundAt SoundFX("DiverterLeftUp", DOFContactors), DiverterLeft
 Else
	 'LeftApronGate.RotatetoEnd
	 DiverterLeft.TransZ = -20
	 DiverterLeft.Collidable = 1	
	 'Playsound "DiverterLeftDown"
         LDTimer.Enabled = 1
 End If
End Sub

Sub SolRightApronDiverter(Enabled)
	If Enabled Then
		'RightApronGate.RotatetoStart
                DiverterRight.TransZ = -40
		DiverterRight.Collidable = 0
		PlaySoundAt SoundFX("DiverterRightUp", DOFContactors), DiverterRight
	Else
		'RightApronGate.RotatetoEnd
                DiverterRight.TransZ = -20
		DiverterRight.Collidable = 1
		'Playsound "DiverterRightDown"
		RDtimer.Enabled = 1
	End If
End Sub

Sub SolGateLGate(Enabled) 'AXS
	If Enabled Then
		Gate4.Collidable = False
	Else
		Gate4.Collidable = True
	End If
End Sub

Sub SolGateRGate(Enabled) 'AXS
	If Enabled Then
		Gate2.Collidable = False
	Else
		Gate2.Collidable = True
	End If
End Sub

Sub SolTopDiverter(Enabled) 'AXS
	If Enabled Then
		DiverterPost.Collidable = True
	Else
		DiverterPost.Collidable = False
	End If
End Sub

Sub LDTimer_Timer 'AXS
	DiverterLeft.TransZ  = 0
	'DiverterRight.TransZ  = 0
	PlaySoundAt SoundFX("DiverterLeftDown", DOFContactors), DiverterLeft
    LDTimer.Enabled = 0
End Sub

Sub RDTimer_Timer 'AXS
	'DiverterLeft.TransZ  = 0
	DiverterRight.TransZ  = 0
	PlaySoundAt SoundFX("DiverterRightDown", DOFContactors), DiverterRight
    RDTimer.Enabled = 0
End Sub

'************************************************** AXS AutoFlip (Testing)
'Sub TriggerAutoFlipLeft_Hit 'Axs
'if autoflip=1 Then
'                FlipperLeft.RotateToEnd
'		PlaySound SoundFX("FlipperUpLeft",DOFFlippers), 0, .67, AudioPan(FlipperLeft), 0.05,0,0,1,AudioFade(FlipperLeft)
'                TimerFlipperLeft.Enabled=1
'end if
'End Sub
'
'Sub TimerFlipperLeft_Timer
'		FlipperLeft.RotateToStart
'		TimerFlipperLeft.Enabled=0
'		PlaySound SoundFX("FlipperDown",DOFFlippers), 0, 1, AudioPan(FlipperLeft), 0.05,0,0,1,AudioFade(FlipperLeft)
'End Sub
'
'Sub TriggerAutoFlipRight_Hit 
'if autoflip=1 Then
'		PlaySound SoundFX("Flipper(s)UpRight",DOFFlippers), 0, .67, AudioPan(FlipperRight), 0.05,0,0,1,AudioFade(FlipperRight)
'                FlipperRight.RotateToEnd
'                TimerFlipperRight.Enabled=1
'end if
'End Sub
'
'Sub TimerFlipperRight_Timer
'		FlipperRight.RotateToStart
'		TimerFlipperRight.Enabled=0
'		PlaySound SoundFX("FlipperDown",DOFFlippers), 0, 1, AudioPan(FlipperRight), 0.05,0,0,1,AudioFade(FlipperRight)
'End Sub
'
'Sub TriggerAutoFlipRight1_Hit 
'if autoflip=1 Then
'		PlaySound SoundFX("Flipper(s)UpRight",DOFFlippers), 0, .67, AudioPan(FlipperRight), 0.05,0,0,1,AudioFade(FlipperRight)
'                FlipperRight1.RotateToEnd
'                TimerFlipperRight1.Enabled=1
'end if
'End Sub
'
'Sub TimerFlipperRight1_Timer
'		FlipperRight1.RotateToStart
'		TimerFlipperRight1.Enabled=0
'		PlaySound SoundFX("FlipperDown",DOFFlippers), 0, 1, AudioPan(FlipperRight), 0.05,0,0,1,AudioFade(FlipperRight)
'End Sub

'**************************************************

Sub SolLFlipper(Enabled)
    If Enabled Then
		FlipperLeft.RotateToEnd
		PlaySound SoundFX("FlipperUpLeft",DOFFlippers), 0, .67, AudioPan(LT41d), 0.05,0,0,1,AudioFade(FlipperLeft)
	Else
		FlipperLeft.RotateToStart
		PlaySound SoundFX("FlipperDown",DOFFlippers), 0, 1, AudioPan(LT41d), 0.05,0,0,1,AudioFade(FlipperLeft)
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		FlipperRight.RotateToEnd
		FlipperRight1.RotateToEnd
		PlaySound SoundFX("Flipper(s)UpRight",DOFFlippers), 0, .67, AudioPan(LT41c), 0.05,0,0,1,AudioFade(FlipperRight)
	Else
		FlipperRight.RotateToStart
		FlipperRight1.RotateToStart
		PlaySound SoundFX("FlipperDown",DOFFlippers), 0, 1, AudioPan(LT41c), 0.05,0,0,1,AudioFade(FlipperRight)
	End If
End Sub



Sub Table1_KeyDown(ByVal Keycode)
    If keycode = RightMagnaSave Then
		if DisableLUTSelector = 0 then
			LUTmeUP = LUTMeUp + 1
			if LutMeUp > MaxLut then LUTmeUP = 0
			SetLUT
			ShowLUT
		end if
	end if
	If keycode = LeftMagnaSave Then
          if DisableLUTSelector = 0 then
			LUTmeUP = LUTMeUp - 1
			if LutMeUp < 0 then LUTmeUP = MaxLut
			SetLUT
			ShowLUT
		end if
	end if
    If keycode = PlungerKey Then Controller.Switch(11) = 1
    If keycode = keyFront Then Controller.Switch(23) = 1
	If keycode = LeftTiltKey Then
		Nudge 90, 2
	End If

	If keycode = RightTiltKey Then
		Nudge 270, 2
	End If

	If keycode = CenterTiltKey Then
		Nudge 0, 2
	End If

    'If keycode = 31 then autoflip = 1 - autoflip: playsound "button-click" 'AXS
'Msgbox Keycode
	If vpmKeyDown(keycode) Then Exit Sub
End Sub
 
Sub table1_KeyUp(ByVal Keycode)
	If vpmKeyUp(keycode) Then Exit Sub
	If keycode = PlungerKey Then Controller.Switch(11) = 0
	If keycode = keyFront Then Controller.Switch(23) = 0
End Sub


Sub SolRelease(Enabled)
	If Enabled And bsTrough.Balls > 0 Then
		vpmTimer.PulseSw 31
		bsTrough.ExitSol_On
	End If
End Sub

' ************************************************
' Slingshots 
' ************************************************

Dim RStep, Lstep

Sub RightSlingShot_Slingshot()
	vpmTimer.PulseSW 62
End Sub

Sub LeftSlingShot_Slingshot()
	vpmTimer.PulseSW 61
End Sub


Sub sRightSlingShot(enabled)
	If enabled Then
    PlaySound SoundFX("slingshotRight", DOFContactors), 0, 1, 0.05, 0.05
    'RSling.Visible = 0
    RSling1.Visible = 1
    'sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	End If
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:SlingArmR.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSling1.Visible = 0:SlingArmR.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub sLeftSlingShot(enabled)
	If enabled Then
    PlaySound SoundFX("slingshotLeft", DOFContactors),0,1,-0.05,0.05
    'LSling.Visible = 0
    LSling1.Visible = 1
    'sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	End If
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:SlingArmL.TransZ = -10
        Case 4:LSLing2.Visible = 0:SlingArmL.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub SlingHopL_Hit
     'Msgbox Activeball.velx'Msgbox "left Hit"
     If Activeball.velX < -7 Then
		'Msgbox Activeball.velx
		SlingHopTimer.Enabled = 1
		Activeball.velZ = 10
	End If  
End Sub 

Sub SlingHopR_Hit
     'Msgbox Activeball.velx'Msgbox "Right hit"
	If Activeball.velX > 7 Then
		'Msgbox Activeball.velx
		SlingHopTimer.Enabled = 1
		Activeball.velZ = 10
	End If 
End Sub 

Sub SlingHopTimer_Timer
     Playsound "ball_bounce",0,.05,0,.1
     SlingHopTimer.Enabled = 0
End Sub


' ************************************************
' HitTarget and Bronto Crane animation 
' ************************************************



Class HTAnim
	Dim HTPrimObj, HTSwitchObj, HTOsc, HTOscIncrement, HTDist, HTSwitchNum
	Dim HTDistMax, HTStartOscDeg, HTDecay, HTOscInitialIncrement, HTTimerInterval, HTOscRampIncrement
	Dim HTAxis
	
	public default function Init(primobj, switchobj, switchnum)
		set HTPrimObj = primobj
		set HTSwitchObj = switchobj
		HTTimerInterval = 8
		HTSwitchObj.TimerEnabled = 0
		HTSwitchNum = switchnum
		HTDistMax = 6
		HTStartOscDeg = 0
		HTDecay = 0.6
		HTOscInitialIncrement = .474
		HTOscRampIncrement = .013
		
		set Init = Me
	end function

	sub SetAxis(axis)
		HTAxis = axis
	end sub

	sub Start
		HTOsc = HTStartOscDeg
	    HTDist = HTDistMax
		HTOscIncrement = HTOscInitialIncrement
		HTSwitchObj.TimerInterval = HTTimerInterval 
		HTSwitchObj.TimerEnabled = 1
		vpmTimer.PulseSw HTSwitchNum
	end sub 
	
	sub Update
		select case HTAxis
		case "Y+"
			HTPrimObj.RotY = HTDist * cos(HTOsc)
		case "Y-"
			HTPrimObj.RotY = -HTDist * cos(HTOsc)
		case else
			HTPrimObj.RotX = 180 + HTDist * cos(HTOsc)
		end select
		if HTDist > 0 Then
			HTDist = HTDist - HTDecay
			HTOsc = HTOsc + HTOscIncrement
			HTOscIncrement = HTOscIncrement + HTOscRampIncrement
			if HTOsc > 6.28 then HTOsc = HTOsc - 6.28
		Else
			HTSwitchObj.TimerEnabled = 0
		end if
	end sub 
End Class

Dim HTAnim26:Set HTAnim26 = (new HTAnim)(Rg9_T26, Sw26, 26)

Sub sw26_Hit:	HTAnim26.Start: PlaySoundAtBallVol SoundFX("target", DOFTargets): End Sub
Sub sw26_Timer:	HTAnim26.Update:End Sub

Dim HTAnim51a:Set HTAnim51a = (new HTAnim)(Rg9_T51a, Sw51a, 51):HTAnim51a.SetAxis("Y+")

Sub sw51a_Hit:	 HTAnim51a.Start: PlaySoundAtBallVol SoundFXDOF("target",101,DOFPulse,DOFTargets): End Sub
Sub sw51a_Timer: HTAnim51a.Update:End Sub

Dim HTAnim51b:Set HTAnim51b = (new HTAnim)(Rg9_T51b, Sw51b, 51):HTAnim51b.SetAxis("Y-")

Sub sw51b_Hit:	 HTAnim51b.Start: PlaySoundAtBallVol SoundFXDOF("target",102,DOFPulse,DOFTargets): End Sub
Sub sw51b_Timer: HTAnim51b.Update:End Sub

Dim HTAnim52a:Set HTAnim52a = (new HTAnim)(Rg9_T52a, Sw52a, 52):HTAnim52a.SetAxis("Y+")

Sub sw52a_Hit:	 HTAnim52a.Start: PlaySoundAtBallVol SoundFXDOF("target",101,DOFPulse,DOFTargets): End Sub
Sub sw52a_Timer: HTAnim52a.Update:End Sub

Dim HTAnim52b:Set HTAnim52b = (new HTAnim)(Rg9_T52b, Sw52b, 52):HTAnim52b.SetAxis("Y-")

Sub sw52b_Hit:	 HTAnim52b.Start: PlaySoundAtBallVol SoundFXDOF("target",102,DOFPulse,DOFTargets): End Sub
Sub sw52b_Timer: HTAnim52b.Update:End Sub

Dim HTAnim53a:Set HTAnim53a = (new HTAnim)(Rg9_T53a, Sw53a, 53):HTAnim53a.SetAxis("Y+")

Sub sw53a_Hit:	 HTAnim53a.Start: PlaySoundAtBallVol SoundFXDOF("target",101,DOFPulse,DOFTargets): End Sub
Sub sw53a_Timer: HTAnim53a.Update:End Sub

Dim HTAnim53b:Set HTAnim53b = (new HTAnim)(Rg9_T53b, Sw53b, 53):HTAnim53b.SetAxis("Y-")

Sub sw53b_Hit:	 HTAnim53b.Start: PlaySoundAtBallVol SoundFXDOF("target",102,DOFPulse,DOFTargets):End Sub
Sub sw53b_Timer: HTAnim53b.Update:End Sub

Dim HTAnim54:Set HTAnim54 = (new HTAnim)(Rg9_T54, Sw54, 54)

Sub sw54_Hit:	HTAnim54.Start: PlaySoundAtBallVol SoundFX("target", DOFTargets):End Sub
Sub sw54_Timer:	HTAnim54.Update:End Sub

Dim HTAnim55:Set HTAnim55 = (new HTAnim)(Rg9_T55, Sw55, 55)

Sub sw55_Hit:	HTAnim55.Start: PlaySoundAtBallVol SoundFX("target", DOFTargets):End Sub
Sub sw55_Timer:	HTAnim55.Update:End Sub

Dim HTAnim56:Set HTAnim56 = (new HTAnim)(Rg9_T56, Sw56, 56)

Sub sw56_Hit:	HTAnim56.Start: PlaySoundAtBallVol SoundFX("target", DOFTargets):End Sub
Sub sw56_Timer:	HTAnim56.Update:End Sub

Dim HTBronto:Set HTBronto = (new HTAnim)(BrontoCrane, BrontoTrigger1, 200):HTBronto.HTDistMax = .75:HTBronto.HTDecay = 0.01:HTBronto.HTOscInitialIncrement = .05:HTBronto.HTStartOscDeg = 2.14

Sub BrontoTrigger1_Hit: HTBronto.Start: DOF 103, DOFOn: End Sub
Sub BrontoTrigger2_Hit: HTBronto.Start: DOF 103, DOFOn: BrontoTrigger2.TimerInterval = 100:BrontoTrigger2.TimerEnabled = 1:End Sub

Sub BrontoTrigger1_Timer 
	HTBronto.Update
	if BrontoTrigger1.TimerEnabled = 0 then DOF 103, DOFOff
End Sub

Sub BrontoTrigger2_Timer: 
	PlaySound "ball_bounce", 1, .1, AudioPan(BrontoTrigger2), 0,0,0, 1, AudioFade(BrontoTrigger2)'PlaySoundAt "ball_bounce", BrontoTrigger2 
	BrontoTrigger2.TimerEnabled = 0
End Sub

' ************************************************
' Pop bumpers
' ************************************************


Sub Sw63_Hit:vpmTimer.PulseSw 63:PlaySoundAtBall SoundFX("BumperTop_Hit", DOFContactors):End Sub
Sub Sw64_Hit:vpmTimer.PulseSw 64:PlaySoundAtBall SoundFX("BumperRight_Hit", DOFContactors):End Sub
Sub Sw65_Hit:vpmTimer.PulseSw 65:PlaySoundAtBall SoundFX("BumperLeft_Hit", DOFContactors):End Sub


' ************************************************
' Drop targets 
' ************************************************

'Sub Sw41_Hit:vpmTimer.PulseSw 41:PlaySoundAtBallVol "target":End Sub
'Sub Sw42_Hit:vpmTimer.PulseSw 42:PlaySoundAtBallVol "target":End Sub
'Sub Sw43_Hit:vpmTimer.PulseSw 43:PlaySoundAtBallVol "target":End Sub
'Sub Sw44_Hit:vpmTimer.PulseSw 44:PlaySoundAtBallVol "target":End Sub
'Sub Sw45_Hit:vpmTimer.PulseSw 45:PlaySoundAtBallVol "target":End Sub
'Sub Sw46_Hit:vpmTimer.PulseSw 46:PlaySoundAtBallVol "target":End Sub
'Sub Sw47_Hit:vpmTimer.PulseSw 47:PlaySoundAtBallVol "target":End Sub

' ************************************************
' Other switches 
' ************************************************

Sub sw15_Hit:Controller.Switch(15) = 1:PlaySoundAtBallVol "sensor":End Sub
Sub sw15_Unhit:Controller.Switch(15) = 0:End Sub

Sub sw16_Hit:vpmTimer.PulseSw 16:PlaySoundAtBallVol "sensor":End Sub
'Sub sw16_Unhit:Controller.Switch(16) = 0:End Sub

Sub sw17_Hit:vpmTimer.PulseSw 17:PlaySoundAtBallVol "sensor":End Sub
'Sub sw17_Unhit:Controller.Switch(17) = 0:End Sub

Sub sw18_Hit:vpmTimer.PulseSw 18:PlaySoundAtBallVol "sensor":End Sub
'Sub sw18_Unhit:Controller.Switch(18) = 0:End Sub

Sub sw25_Hit:Controller.Switch(25) = 1:PlaySoundAtBallVol "sensor":End Sub
Sub sw25_Unhit:Controller.Switch(25) = 0:End Sub


Sub sw48_Hit:Controller.Switch(48) = 1:PlaySoundAtBallVol "sensor":End Sub
Sub sw48_UnHit:Controller.Switch(48) = 0:End Sub

Sub sw27_Hit:Controller.Switch(27) = 1:PlaySoundAtBallVol "sensor":End Sub
Sub sw27_UnHit:Controller.Switch(27) = 0:End Sub


Sub sw28_Hit:Controller.Switch(28) = 1:PlaySoundAtBallVol "sensor":End Sub
Sub sw28_UnHit:Controller.Switch(28) = 0:End Sub

Sub sw37_Hit:vpmTimer.PulseSw 37:PlaySoundAtBallVol "gate":End Sub

Sub sw38_Hit:vpmTimer.PulseSw 38:PlaySoundAtBallVol "gate":End Sub

Sub sw75_Hit:vpmTimer.PulseSw 75:PlaySoundAtBallVol "sensor":End Sub

Sub sw66_Hit:Controller.Switch(66) = 1:PlaySoundAtBallVol "sensor":End Sub
Sub sw66_UnHit:Controller.Switch(66) = 0:End Sub

Sub sw67_Hit:Controller.Switch(67) = 1:PlaySoundAtBallVol "sensor":End Sub
Sub sw67_UnHit:Controller.Switch(67) = 0:End Sub

Sub sw68_Hit:Controller.Switch(68) = 1:PlaySoundAtBallVol "sensor":End Sub
Sub sw68_UnHit:Controller.Switch(68) = 0:End Sub

Sub sw71_Hit:Controller.Switch(71) = 1:PlaySoundAt "sensor", Sw71:End Sub
Sub sw71_UnHit:Controller.Switch(71) = 0:End Sub

Sub sw72_Hit:Controller.Switch(72) = 1:PlaySoundAtBallVol "sensor":End Sub
Sub sw72_UnHit:Controller.Switch(72) = 0:End Sub

Sub sw73_Hit:Controller.Switch(73) = 1:PlaySoundAtBallVol "sensor":End Sub
Sub sw73_UnHit:Controller.Switch(73) = 0:End Sub

Sub sw74_Hit:Controller.Switch(74) = 1:PlaySoundAtBallVol "sensor":End Sub
Sub sw74_UnHit:Controller.Switch(74) = 0:End Sub

Sub sw76_Hit:Controller.Switch(76) = 1:PlaySoundAtBallVol "sensor":End Sub
Sub sw76_Unhit:Controller.Switch(76) = 0:End Sub

Sub sw77_Hit:Controller.Switch(77) = 1:PlaySoundAtBallVol "sensor":End Sub
Sub sw77_Unhit:Controller.Switch(77) = 0:End Sub

Sub sw78_Hit:Controller.Switch(78) = 1:PlaySoundAtBallVol "sensor":End Sub
Sub sw78_Unhit:Controller.Switch(78) = 0:End Sub



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

Sub PlaySoundAtBallVol(soundname)
	PlaySound soundname, 1, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub

Sub TriggerBallBounce1_UnHit:PlaySound "ball_bounce",0,.08,-0.05,.1:End Sub 'AXS
Sub TriggerBallBounce2_UnHit:PlaySound "ball_bounce" ,0, .08,0.05,.1:End Sub
Sub TriggerBallBounce3_UnHit:PlaySound "ball_bounce",0,.06,-0.05,.1:End Sub
Sub TriggerBallBounce4_UnHit:PlaySound "ball_bounce" ,0, .06,0.05,.1:End Sub

Sub LRHit1_Hit() : PlaySound "fx_lr1",0,.02,-0.05,.1: End Sub 'AXS
Sub LRHit2_Hit() : PlaySound "fx_lr2",0,.02,-0.05,.1: End Sub
Sub LRHit3_Hit() : PlaySound "fx_lr3",0,.02,-0.05,.1: End Sub
Sub LRHit4_Hit() : PlaySound "fx_lr4",0, .02,0.05,.1 : End Sub
Sub LRHit5_Hit() : PlaySound "fx_lr5",0, .02,0.05,.1: End Sub
Sub LRHit6_Hit() : PlaySound "fx_lr6",0, .02,0.05,.1: End Sub
Sub LRHit7_Hit() : PlaySound "fx_lr7",0,.01,-0.05,.1 : End Sub

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
    Vol = Csng(BallVel(ball) ^2 / 200)
End Function

Function RollVol(ball) ' Calculates the Volume of the sound based on the ball speed.   Targets 100-80000 when VolRoll is 0-100  
    RollVol = Csng(BallVel(ball) ^2 / (80000 - (79900 * Log(VolRoll) / Log(100))))
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Sub WireRampSFX_Hit
       Playsound "WireRolling"
End Sub


Sub RampDropSFX_Hit
       Playsound "RampDrop"
End Sub


'*****************************************
'   rothbauerw's Manual Ball Control
'*****************************************

Dim BCup, BCdown, BCleft, BCright
Dim ControlBallInPlay, ControlActiveBall
Dim BCvel, BCyveloffset, BCboostmulti, BCboost

BCboost = 1				'Do Not Change - default setting
BCvel = 4				'Controls the speed of the ball movement
BCyveloffset = -0.01 	'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
BCboostmulti = 3		'Boost multiplier to ball veloctiy (toggled with the B key) 

ControlBallInPlay = false

Sub StartBallControl_Hit()
	Set ControlActiveBall = ActiveBall
	ControlBallInPlay = true
End Sub

Sub StopBallControl_Hit()
	ControlBallInPlay = false
End Sub	

Sub BallControlTimer_Timer()
	If EnableBallControl and ControlBallInPlay then
		If BCright = 1 Then
			ControlActiveBall.velx =  BCvel*BCboost
		ElseIf BCleft = 1 Then
			ControlActiveBall.velx = -BCvel*BCboost
		Else
			ControlActiveBall.velx = 0
		End If

		If BCup = 1 Then
			ControlActiveBall.vely = -BCvel*BCboost
		ElseIf BCdown = 1 Then
			ControlActiveBall.vely =  BCvel*BCboost
		Else
			ControlActiveBall.vely = bcyveloffset
		End If
	End If
End Sub


'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 5 ' total number of balls
Const fakeballs = 1
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub


Sub RollingTimer_Timer()
	if VolRoll = 0 then exit sub
    Dim BOT, b
    BOT = GetBalls

	' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob-1
		If rolling(b-fakeballs) = True Then
			rolling(b-fakeballs) = False
			StopSound("fx_ballrolling" & b-fakeballs)
		end if 
    Next

	' exit the sub if no balls on the table
    If UBound(BOT) = fakeballs-1 Then Exit Sub

	' play the rolling sound for each ball
	For b = fakeballs to UBound(BOT)
        If BallVel(BOT(b) ) > 1 Then
            rolling(b-fakeballs) = True
			
			If BOT(b).z < 30 Then
				PlaySound("fx_ballrolling" & b-fakeballs), -1, RollVol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
            Else
				PlaySound("fx_ballrolling" & b-fakeballs), -1, RollVol(BOT(b) )*.2, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
            End If
        Else
            If rolling(b-fakeballs) = True Then
                StopSound("fx_ballrolling" & b-fakeballs)
                rolling(b-fakeballs) = False
            End If
        End If
    Next
End Sub



'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 200, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'*****************************************
'	ninuzzu's	BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)

Sub BallShadowUpdate()
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
    For b = fakeballs to UBound(BOT)
        If BOT(b).X < Table1.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 6
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 6
        End If
        ballShadow(b).Y = BOT(b).Y + 12
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub


InitLamps()             ' turn off the lights and flashers and reset them to the default parameters

' Called every 1ms.
Sub OneMsec_Timer()
	FadeLights.Update1
End Sub
 
' div lamp subs


Sub InitLamps()
	dim id, lightobj

	For each lightobj in Lamps
		' Asumptions: Light is named "LTxxy" where x is the lamp number, and y is optionally a,b,c for multiple on the same id
		dim arr
		id = cInt(mid(lightobj.Name,3, 2))
	    if TypeName(FadeLights.Obj(id)) = "NullFadingObject" then 
			arr = array(lightobj)
		Else
			arr = FadeLights.Obj(id)
			ReDim Preserve arr(UBound(arr) + 1)
			set arr(UBound(arr)) = lightobj
		end if
		FadeLights.Obj(id) = arr			
	next
	FadeLights.Callback(114) = "UpdateDictabird "
    FadeLights.FadeSpeedUp(114) = 1/50 : FadeLights.FadeSpeedDown(114) = 1/50

	FadeLights.Obj(117) = array(FlPf17,Fl17)
	FadeLights.Obj(118) = array(FlPf18,Fl18)
	FadeLights.Obj(119) = array(FlPf19,Fl19)
	FadeLights.Obj(120) = array(FlPf20,Fl20)
	FadeLights.Obj(121) = array(FlPf21,Fl21)
	FadeLights.Obj(122) = array(FlPf22,Fl22)
	FadeLights.Obj(124) = array(FlPf24,Fl24)
	FadeLights.Obj(125) = array(FlPf25,Fl25,DigMillions)
	FadeLights.Obj(128) = array(FlPf28,Fl28)
End Sub 


Sub AllLampsOff
    Dim x
    For x = 0 to 200
        SetLamp x, 0
    Next
End Sub

Sub SetLamp(nr, value)
	' If the lamp state is not changing, just exit. 
	if FadeLights.state(nr) = value then exit sub

    FadeLights.state(nr) = value
End Sub


' *** NFozzy's lamp fade routines *** 


Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class	'todo do better

Class LampFader
	Public FadeSpeedDown(140), FadeSpeedUp(140)
	Private Lock(140), Loaded(140), OnOff(140)
	Public UseFunction
	Private cFilter
	Private UseCallback(140), cCallback(140)
	Public Lvl(140), Obj(140)

	Sub Class_Initialize()
		dim x : for x = 0 to uBound(OnOff) 	'Set up fade speeds
			if FadeSpeedDown(x) <= 0 then FadeSpeedDown(x) = 1/100	'fade speed down
			if FadeSpeedUp(x) <= 0 then FadeSpeedUp(x) = 1/80'Fade speed up
			UseFunction = False
			lvl(x) = 0
			OnOff(x) = False
			Lock(x) = True : Loaded(x) = False
		Next

		for x = 0 to uBound(OnOff) 		'clear out empty obj
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		Next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property		'debug.print Lampz.Locked(100)	'debug
	Public Property Get state(idx) : state = OnOff(idx) : end Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property

	Public Property Let state(ByVal idx, input) 'Major update path
		input = cBool(input)
		if OnOff(idx) = Input then : Exit Property : End If	'discard redundant updates
		OnOff(idx) = input
		Lock(idx) = False 
		Loaded(idx) = False
	End Property

	Public sub LampMod(ByVal idx, input) 
		if Lvl(idx) = input then Exit Sub
		Lvl(idx) = (input * FlasherIntensity) / 25500
		Lock(idx) = True 
		Loaded(idx) = False
	End Sub

	Public Sub TurnOnStates()	'If obj contains any light objects, set their states to 1 (Fading is our job!)
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then 
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x) : debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					
				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx) : debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				
			end if
		Next
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Update1()	 'Handle all boolean numeric fading. If done fading, Lock(True). Update on a '1' interval Timer!
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) > 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					if Lvl(x) < 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
	End Sub


	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx : for x = 0 to uBound(OnOff)
			if not Loaded(x) then
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then 
						for each xx in obj(x) : xx.IntensityScale = cFilter(Lvl(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x) : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then 
						obj(x).Intensityscale = cFilter(Lvl(x))
					Else
						obj(x).Intensityscale = Lvl(x)
					End If
				end if
				' Sleazy hack for regional decimal point problem
				If UseCallBack(x) then execute cCallback(x) & " CSng(" & CInt(10000 * Lvl(x)) & " / 10000)"	'Callback
				If Lock(x) Then
					Loaded(x) = True	'finished fading
				end if
			end if
		Next
	End Sub
End Class



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

' The sound is played using the VOL, AUDIOPAN, AUDIOFADE and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the AUDIOPAN & AUDIOFADE functions will change the stereo position
' according to the position of the ball on the table.


'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they 
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.


Sub RandomSoundDrain
	dim DrainSnd:DrainSnd= "drain" & CStr(Int(Rnd*4)+1)
	PlaySound DrainSnd, 0, 1, 0, .2
End Sub

Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySoundAtBallVol "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
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

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub FlipperLeft_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub FlipperRight_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Dim NextOrbitHit:NextOrbitHit = 0 


Sub PlasticRampBump1_Hit
	RandomBump 1, Pitch(ActiveBall)
End Sub

Sub PlasticRampBump2_Hit
	RandomBump 1, Pitch(ActiveBall)
End Sub

Sub PlasticRampBumps_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
		RandomBump 2, Pitch(ActiveBall)
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .1 + (Rnd * .2)
	end if 
End Sub

Sub MetalWallBumps_Hit(idx)
	if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
		RandomBump 1, 20000 'Increased pitch to simulate metal wall
		' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
		' Lowering these numbers allow more closely-spaced clunks.
		NextOrbitHit = Timer + .2 + (Rnd * .2)
	end if 
End Sub


'' Requires rampbump1 to 7 in Sound Manager
Sub RandomBump(voladj, freq)
	dim BumpSnd:BumpSnd= "rampbump" & CStr(Int(Rnd*7)+1)
		PlaySound BumpSnd, 0, Vol(ActiveBall)+voladj, AudioPan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub
