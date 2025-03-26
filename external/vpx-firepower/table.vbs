'                                _      ________   __   _______   __  _______  ______  ___  ___ 
'                               | | /| / /  _/ /  / /  /  _/ _ | /  |/  / __/ <  / _ \( _ )/ _ \
'                               | |/ |/ // // /__/ /___/ // __ |/ /|_/ /\ \   / /\_, / _  / // /
'                               |__/|__/___/____/____/___/_/ |_/_/  /_/___/  /_//___/\___/\___/ 
'                                                                
'                                    ______________  __________  ____ _       ____________ 
'                  **************   / ____/  _/ __ \/ ____/ __ \/ __ \ |     / / ____/ __ \   ***************
'                 **************   / /_   / // /_/ / __/ / /_/ / / / / | /| / / __/ / /_/ /  ***************
'                **************   / __/ _/ // _, _/ /___/ ____/ /_/ /| |/ |/ / /___/ _, _/  ***************
'               **************   /_/   /___/_/ |_/_____/_/    \____/ |__/|__/_____/_/ |_|  ***************
'                                                         
'                                                     V1.0.5
' 
'                                       A 3rdaxis, Slydog43 & G5K Collaboration.
'
'Table Options Below
'
'Recommendations:
'In-game AO should be left "Off"
'SCSP Reflect should be left "On"
'Make sure your "Sound Effects Volume" is set to maximum under "Preferences/Audio Options..."

'//////////////////////////////////////////////////////////////////////
'// LAYER 01 = Timers, Flippers, Triggers, Rubbers, Spinner, Walls
'// LAYER 02 = Lamps B, Upper Walls
'// LAYER 03 = GI, VPX Pop Bumpers
'// LAYER 04 = Lamps
'// LAYER 05 = Flashers
'// LAYER 06 = Blank
'// LAYER 07 = Targets, Target Primitives  
'// LAYER 08 = Glass
'// LAYER 09 = G5K Primitives, RailsLockbar, Blades
'// LAYER 10 = G5K Primitives, GRP1
'// LAYER 11 = G5K Primitives, GRP2, Pop Bumper,Outlane Posts 
'//////////////////////////////////////////////////////////////////////

Option Explicit
Randomize
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="frpwr_b7"
'Const UseSolenoids=1
Const UseSolenoids=25 'FastFlips for system6
Const UseLamps = True 'VPM to control Lights
Const UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff", SCoin="coin"
Const BallSize = 50 
Const Ballmass = 1  

LoadVPM "01560000", "S6.VBS", 3.26

'***********************************************************************************************************************************

Dim T1Step,T2Step,T3Step,T4Step,T5Step,T6Step,topPWRStep,midPWRStep,BtmPWRStep,CTStep,DTMod,CenterPost
Dim trTrough

Dim myPrefs_OutlaneDifficulty, MyPrefs_Brightness
Dim myPrefs_PlayMusic, MyPrefs_DisableBrightness
Dim myPrefs_InstructionCardType, myPrefs_GlassHit
Dim myPrefs_Targets
Dim myPrefs_DynamicBackground
Dim myPrefs_PlayfieldLamps
Dim myPrefs_FlipperBatType

Dim myPlayingEndGameSnd 
myPlayingEndGameSnd = False 

' _________   ___  __   ____  ____  ___  ______________  _  ______
'/_  __/ _ | / _ )/ /  / __/ / __ \/ _ \/_  __/  _/ __ \/ |/ / __/
' / / / __ |/ _  / /__/ _/  / /_/ / ___/ / / _/ // /_/ /    /\ \  
'/_/ /_/ |_/____/____/___/  \____/_/    /_/ /___/\____/_/|_/___/  
'***********************************************************************************************************************************
 
myPrefs_OutlaneDifficulty = 1      '0=Factory 1=Easy 
myPrefs_PlayMusic = 1                      '0=Factory 1=On 
myPrefs_InstructionCardType = 0  '0=Factory 1=Modded Cards 2=THX Card  (See THX Setup for directions)
myPrefs_Targets = 0                          '0=Factory 1=Modded 
myPrefs_DynamicBackground = 1      '0=Off 1=On
BallShadowUpdate.Enabled = 1        '0=Off 1=On (Off=Performance On=Quality)
myPrefs_PlayfieldLamps  = 1           '0=Factory 1=LED
MyPrefs_Brightness = 4  		      '1= 30% contrast and brightness adj, 2= 50% contrast and brightness adj, 3= 70% contrast and brightness adj, 4= 100% contrast and brightness adj, 
MyPrefs_DisableBrightness = 0      'Disables the ability to change Brightness option with magna saves in game when set to 1
myPrefs_GlassHit = 1                        '0=Off, 1=Less often, 2=More often (This determines how often the ball will smack against the glass)
myPrefs_FlipperBatType = 3            'Flipper Bat Type

'0= White Bat Red Rubber  (Factory)
'1= White Bat Black Rubber 
'2= White Bat White Rubber 
'3= Yellow Bat Red Rubber 
'4= Yellow Bat Black Rubber
'5= White Bat Yellow Rubber 

'THX Setup. (Select Instruction Card 2 in the script options)
'<Basic adjustment>
'While in game use the Magnasave buttons to adjust the brightness. 
'During adjustment the THX Logo's shadow should just barely be visible on the right player card. This will usually get you close enough.
'<Advanced Adjustment>
'The right card is for adjusting yourTV's Brightness. 
'While adjusting your brightness the THX Logo's shadow should just barely be visible and all 9 squares should be a different shade. 
'The left card is for adjusting your TV's contrast.
'While adjusting your contrast all 8 squares should be a different shade. 
'You may have to go back and forth between brightness and contrast to get the right ballence and calibration.

'**********************************************************************************************************************************

'        ______   _______  ___  _______  ____
'*****  / __/ /  /  _/ _ \/ _ \/ __/ _ \/ __/*****
'***** / _// /___/ // ___/ ___/ _// , _/\ \  ***** 
'*****/_/ /____/___/_/  /_/  /___/_/|_/___/  *****
  

Sub GraphicsTimer_Timer()
		batleft.objrotz = FlipperLeft.CurrentAngle + 1
		batleftshadow.objrotz = batleft.objrotz
		batright.objrotz = FlipperRight.CurrentAngle - 1
		batrightshadow.objrotz  = batright.objrotz 		
End Sub

  Select Case myPrefs_FlipperBatType
    Case 0
		batleft.image = "_flipper_white_red"
		batright.image = "_flipper_white_red"
    Case 1
	        batleft.image = "_flipper_white_black"
		batright.image = "_flipper_white_black"
    Case 2
	        batleft.image = "_flipper_white_white"
		batright.image = "_flipper_white_white"
    Case 3
		batleft.image = "_flipper_yellow_red"
		batright.image = "_flipper_yellow_red"
    Case 4
                batleft.image = "_flipper_yellow_black"
		batright.image = "_flipper_yellow_black"
    Case 5
                batleft.image = "_flipper_white_yellow"
		batright.image = "_flipper_white_yellow"
  End Select

'       ____  __  __________   ___   _  ______  ___  _______________________  ____ ________  __
'***** / __ \/ / / /_  __/ /  / _ | / |/ / __/ / _ \/  _/ __/ __/  _/ ___/ / / / //_  __/\ \/ /*****
'*****/ /_/ / /_/ / / / / /__/ __ |/    / _/  / // // // _// _/_/ // /__/ /_/ / /__/ /    \  / ***** 
'*****\____/\____/ /_/ /____/_/ |_/_/|_/___/ /____/___/_/ /_/ /___/\___/\____/____/_/     /_/  *****
                                                                                        

  Select Case myPrefs_OutlaneDifficulty
    Case 0 'Default(0)
          REDPOST1.Visible = False
          REDPOST1.Collidable = False 
          REDPOST.Visible = True
          REDPOST.Collidable = True 
    Case 1 'Easy(1)
          REDPOST1.Visible = True
          REDPOST1.Collidable = True 
          REDPOST.Visible = False
          REDPOST.Collidable = False   
  End Select

'      _________   ___  ___________________
'*****/_  __/ _ | / _ \/ ___/ __/_  __/ __/*****              Y+
'***** / / / __ |/ , _/ (_ / _/  / / _\ \  *****              ^
'*****/_/ /_/ |_/_/|_|\___/___/ /_/ /___/  *****                 >X+
                                                    

Sub Target1T_Hit:Playsound "target":vpmTimer.PulseSwitch 17, 0, "":    'Target1
      Target1_10.playanim 0,.6	
      If Activeball.velY > 16 Then
      'Msgbox Activeball.velY 
      Activeball.velZ = 17
    End If
       End Sub
      
	 Sub ComboTrigger1_hit:Playsound "target":vpmTimer.PulseSwitch 17, 0, "":vpmTimer.PulseSwitch 18, 0, ""   'Target 1/2
				  Target1_10.playanim 0,.6:Target2_10.playanim 0,.6
										 'MsgBox "Combo!!!"
						End Sub

Sub Target2T_Hit:Playsound "target":vpmTimer.PulseSwitch 18, 0, "":     'Target2
      Target2_10.playanim 0,.6
      If Activeball.velY > 16 Then
      'Msgbox Activeball.velY 
      Activeball.velZ = 17
    End If
       End Sub

	 Sub ComboTrigger2_hit:Playsound "target":vpmTimer.PulseSwitch 18, 0, "":vpmTimer.PulseSwitch 19, 0, ""   'Target 2/3
				  Target2_10.playanim 0,.6:Target3_10.playanim 0,.6
										  'MsgBox "Combo!!!"
						 End Sub

Sub Target3T_Hit:Playsound "target":vpmTimer.PulseSwitch 19, 0, "":     'Target3
     Target3_10.playanim 0,.6
      If Activeball.velY > 16 Then
      'Msgbox Activeball.velY 
       Activeball.velZ = 17
    End If
       End Sub  

Sub Target4T_Hit:Playsound "target":vpmTimer.PulseSwitch 21, 0, "":     'Target4
     Target4_10.playanim 0,.6
      If Activeball.velY > 16 Then
      'Msgbox Activeball.velY
       Activeball.velZ = 17
    End If
       End Sub 

	 Sub ComboTrigger3_hit:Playsound "target":vpmTimer.PulseSwitch 21, 0, "":vpmTimer.PulseSwitch 22, 0, ""   'Target4/5
				  Target4_10.playanim 0,.6:Target5_10.playanim 0,.6
						 'MsgBox "Combo!!!"
								End Sub

Sub Target5T_Hit:Playsound "target":vpmTimer.PulseSwitch 22, 0, "":     'Target5
     Target5_10.playanim 0,.6
      If Activeball.velY > 16 Then
      'Msgbox Activeball.velY
       Activeball.velZ = 17
    End If
       End Sub 

	 Sub ComboTrigger4_hit:Playsound "target":vpmTimer.PulseSwitch 22, 0, "":vpmTimer.PulseSwitch 23, 0, ""   'Target 5/6
				  Target5_10.playanim 0,.6:Target6_10.playanim 0,.6
						 'MsgBox "Combo!!!"
								End Sub

Sub Target6T_Hit:Playsound "target":vpmTimer.PulseSwitch 23, 0, "":     'Target6
     Target6_10.playanim 0,.6
      If Activeball.velY > 16 Then
      'Msgbox Activeball.velY
       Activeball.velZ = 17
    End If
       End Sub 

Sub PTTarget_6_Hit:Playsound "target":vpmTimer.PulseSw cTopPOWERTargetSW
	PTTarget_6.playanim 0,.6
   End Sub

Sub PMTarget_6_Hit:Playsound "target":vpmTimer.PulseSw cMiddlePOWERTargetSW
	 PMTarget_6.playanim 0,.6
   End Sub

Sub PBTarget_6_Hit:Playsound "target":vpmTimer.PulseSw cBottomPOWERTargetSW
	 PBTarget_6.playanim 0,.6
   End Sub

Sub TopTarget_6_Hit:Playsound "target":vpmTimer.PulseSw  cTopCenterTargetSW
	 TopTarget_6.playanim 0,.6
   End Sub


'********************************
'***** 10 point switches ***** (Standup Switches) (8 total) (StandupTarget1-8) 
'********************************

Sub StandupTarget1_hit:vpmtimer.pulsesw cLowerRightStandupSW:Playsound "", 0, 1, -0.09, 0.09:End Sub
Sub StandupTarget1_unhit:Controller.Switch(cLowerRightStandupSW)=0:End Sub

Sub StandupTarget2_hit:vpmtimer.pulsesw cLowerRightStandupSW:Playsound "", 0, 1, -0.09, 0.09:End Sub
Sub StandupTarget2_unhit:Controller.Switch(cLowerRightStandupSW)=0:End Sub

Sub StandupTarget3_hit:vpmtimer.pulsesw cLowerRightStandupSW:Playsound "", 0, 1, -0.09, 0.09:End Sub
Sub StandupTarget3_unhit:Controller.Switch(cLowerRightStandupSW)=0:End Sub

Sub StandupTarget4_hit:vpmtimer.pulsesw cLowerRightStandupSW:Playsound "", 0, 1, -0.09, 0.09:End Sub
Sub StandupTarget4_unhit:Controller.Switch(cLowerRightStandupSW)=0:End Sub

Sub StandupTarget5_hit:vpmtimer.pulsesw cLowerRightStandupSW:Playsound "", 0, 1, -0.09, 0.09:End Sub
Sub StandupTarget5_unhit:Controller.Switch(cLowerRightStandupSW)=0:End Sub

Sub StandupTarget6_hit:vpmtimer.pulsesw cLowerRightStandupSW:Playsound "", 0, 1, -0.09, 0.09:End Sub
Sub StandupTarget6_unhit:Controller.Switch(cLowerRightStandupSW)=0:End Sub

Sub StandupTarget7_hit:vpmtimer.pulsesw cLowerRightStandupSW:Playsound "", 0, 1, -0.09, 0.09:End Sub
Sub StandupTarget7_unhit:Controller.Switch(cLowerRightStandupSW)=0:End Sub

Sub StandupTarget8_hit:vpmtimer.pulsesw cLowerRightStandupSW:Playsound "", 0, 1, -0.09, 0.09:End Sub
Sub StandupTarget8_unhit:Controller.Switch(cLowerRightStandupSW)=0:End Sub

'***********************
'***** Target Mods *****
'***********************

  Select Case myPrefs_Targets
    Case 0
      Target1_10.Image = "targetmidleft1_red"
      Target2_10.Image = "targetmidleft2_red"
      Target3_10.Image = "targetmidleft3_red"
      Target4_10.Image = "targetmidright1_red"
      Target5_10.Image = "targetmidright2_red"
      Target6_10.Image = "targetmidright3_red"
    Case 1
      Target1_10.Image = "targetmidleft1_fp"
      Target2_10.Image = "targetmidleft2_fp"
      Target3_10.Image = "targetmidleft3_fp"
      Target4_10.Image = "targetmidright1_fp"
      Target5_10.Image = "targetmidright2_fp"
      Target6_10.Image = "targetmidright3_fp"
    End Select

'         _____  _______________  __  _____________________  _  __  ________   ___  ___  ____
'*****   /  _/ |/ / __/_  __/ _ \/ / / / ___/_  __/  _/ __ \/ |/ / / ___/ _ | / _ \/ _ \/ __/*****
'***** _/ //    /\ \  / / / , _/ /_/ / /__  / / _/ // /_/ /    / / /__/ __ |/ , _/ // /\ \   ***** 
'*****/___/_/|_/___/ /_/ /_/|_|\____/\___/ /_/ /___/\____/_/|_/  \___/_/ |_/_/|_/____/___/   *****

  Select Case myPrefs_InstructionCardType
    Case 0
      CardLFactory.visible = 1
      CardRFactory.visible = 1
    Case 1
      CardLMod.visible = 1
      CardRMod.visible = 1    
    Case 2
      CardLTHX.visible = 1
      CardRTHX.visible = 1
    End Select

'        ___  ____  ___    ___  __  ____  ______  _______  ____
'*****  / _ \/ __ \/ _ \  / _ )/ / / /  |/  / _ \/ __/ _ \/ __/*****
'***** / ___/ /_/ / ___/ / _  / /_/ / /|_/ / ___/ _// , _/\ \  ***** Fix
'*****/_/   \____/_/    /____/\____/_/  /_/_/  /___/_/|_/___/  *****

Dim xx

Set Lights(44)=LBumperTopLeft   
Set Lights(45)=LBumperTopRight  
Set Lights(46)=LBumperBottomRight
Set Lights(47)=LBumperBottomLeft

Sub B1T1_Hit:BumpSkirt1.ROTX=6:                                        BR1Animation:End Sub
Sub B1T2_Hit:BumpSkirt1.ROTX=6:  BumpSkirt1.ROTY=6:  BR1Animation:End Sub
Sub B1T3_Hit:BumpSkirt1.ROTY=6:                                        BR1Animation:End Sub
Sub B1T4_Hit:BumpSkirt1.ROTX=-6:BumpSkirt1.ROTY=6:  BR1Animation:End Sub
Sub B1T5_Hit:BumpSkirt1.ROTY=-6:                                      BR1Animation:End Sub
Sub B1T6_Hit:BumpSkirt1.ROTX=-6:BumpSkirt1.ROTY=-6:BR1Animation:End Sub
Sub B1T7_Hit:BumpSkirt1.ROTY=-6:                                      BR1Animation:End Sub
Sub B1T8_Hit:BumpSkirt1.ROTX=6:  BumpSkirt1.ROTY=-6:BR1Animation:End Sub

Sub B2T1_Hit:BumpSkirt2.ROTX=6:                                        BR2Animation:End Sub
Sub B2T2_Hit:BumpSkirt2.ROTX=6:  BumpSkirt2.ROTY=6:  BR2Animation:End Sub
Sub B2T3_Hit:BumpSkirt2.ROTY=6:                                        BR2Animation:End Sub
Sub B2T4_Hit:BumpSkirt2.ROTX=-6:BumpSkirt2.ROTY=6:  BR2Animation:End Sub
Sub B2T5_Hit:BumpSkirt2.ROTY=-6:                                      BR2Animation:End Sub
Sub B2T6_Hit:BumpSkirt2.ROTX=-6:BumpSkirt2.ROTY=-6:BR2Animation:End Sub
Sub B2T7_Hit:BumpSkirt2.ROTY=-6:                                      BR2Animation:End Sub
Sub B2T8_Hit:BumpSkirt2.ROTX=6:  BumpSkirt2.ROTY=-6:BR2Animation:End Sub

Sub B3T1_Hit:BumpSkirt3.ROTX=6:                                        BR3Animation:End Sub
Sub B3T2_Hit:BumpSkirt3.ROTX=6:  BumpSkirt3.ROTY=6:  BR3Animation:End Sub
Sub B3T3_Hit:BumpSkirt3.ROTY=6:                                        BR3Animation:End Sub
Sub B3T4_Hit:BumpSkirt3.ROTX=-6:BumpSkirt3.ROTY=6:  BR3Animation:End Sub
Sub B3T5_Hit:BumpSkirt3.ROTY=-6:                                      BR3Animation:End Sub
Sub B3T6_Hit:BumpSkirt3.ROTX=-6:BumpSkirt3.ROTY=-6:BR3Animation:End Sub
Sub B3T7_Hit:BumpSkirt3.ROTY=-6:                                      BR3Animation:End Sub
Sub B3T8_Hit:BumpSkirt3.ROTX=6:  BumpSkirt3.ROTY=-6:BR3Animation:End Sub

Sub B4T1_Hit:BumpSkirt4.ROTX=6:                                        BR4Animation:End Sub
Sub B4T2_Hit:BumpSkirt4.ROTX=6:  BumpSkirt4.ROTY=6:  BR4Animation:End Sub
Sub B4T3_Hit:BumpSkirt4.ROTY=6:                                        BR4Animation:End Sub
Sub B4T4_Hit:BumpSkirt4.ROTX=-6:BumpSkirt4.ROTY=6:  BR4Animation:End Sub
Sub B4T5_Hit:BumpSkirt4.ROTY=-6:                                      BR4Animation:End Sub
Sub B4T6_Hit:BumpSkirt4.ROTX=-6:BumpSkirt4.ROTY=-6:BR4Animation:End Sub
Sub B4T7_Hit:BumpSkirt4.ROTY=-6:                                      BR4Animation:End Sub
Sub B4T8_Hit:BumpSkirt4.ROTX=6:  BumpSkirt4.ROTY=-6:BR4Animation:End Sub

  Sub BR1Animation 
      SkirtTimer.Enabled=1
      If BGLmatch.State = 0 Then  
	BR1_7.playanim 0,.6
      End If
      If BGLtilt.State = 1 Then  
	BR1_7.playanim 0,.6
      End If
    End Sub

 Sub BR2Animation
      SkirtTimer.Enabled=1
      If BGLmatch.State = 0 Then
	BR2_7.playanim 0,.6
      End If
      If BGLtilt.State = 1 Then
	BR2_7.playanim 0,.0
      End If
    End Sub

 Sub BR3Animation
      SkirtTimer.Enabled=1
     If BGLmatch.State = 0 Then
	BR3_7.playanim 0,.6
     End If
     If BGLtilt.State = 1 Then
	BR3_7.playanim 0,.0
     End If
    End Sub

 Sub BR4Animation
      SkirtTimer.Enabled=1
     If BGLmatch.State = 0 Then
	BR4_7.playanim 0,.6
     End If
     If BGLtilt.State = 1 Then
	BR4_7.playanim 0,.0
     End If
    End Sub

 Sub SkirtTimer_Timer
	BumpSkirt1.ROTX=0
	BumpSkirt1.ROTY=0
	BumpSkirt2.ROTX=0
	BumpSkirt2.ROTY=0
	BumpSkirt3.ROTX=0
	BumpSkirt3.ROTY=0
	BumpSkirt4.ROTX=0
	BumpSkirt4.ROTY=0
	SkirtTimer.Enabled=0
    End Sub

'        ______   ___   ______ _________  ____
'*****  / __/ /  / _ | / __/ // / __/ _ \/ __/*****
'***** / _// /__/ __ |_\ \/ _  / _// , _/\ \  ***** 
'*****/_/ /____/_/ |_/___/_//_/___/_/|_/___/  *****
                                        

  Sub Flashers(enabled) 'AXS
    If enabled Then
        FlasherLightFire.State = 1
        FlasherLightPower.State = 1        
        FlasherFire.Visible = True
        FlasherPower.Visible = True
        FlasherBig.Visible = True
        LFire1.State = 1
        LFire2.State = 1
	LPower1.State = 1
	LPower2.State = 1
    Else	
        FlasherLightFire.State = 0
        FlasherLightPower.State = 0
        FlasherFire.Visible = False
        FlasherPower.Visible = False
        FlasherBig.Visible = False
	LFire1.State = 0
	LFire2.State = 0
 	LPower1.State =0
	LPower2.State = 0
      End If
   End Sub

'***************************************************
'Target Lights
Set Lights(26)=LT1
Set Lights(27)=LT2
Set Lights(28)=LT3
Set Lights(29)=LT4
Set Lights(30)=LT5
Set Lights(31)=LT6

'---------------------------------------------------
' Set up consts with easy names for solenoid numbers
Const cBallRelease = 1
Const cLeftEjectHole = 4
Const cRightEjectHole = 5
Const cUpperEjectHole = 6
Const cBallSaveKick = 7
Const cBallRampThrower = 8
Const cCreditKnocker = 14
Const cFlashLamps = 15
Const cTopLeftBumper = 17
Const cBottomLeftBumper = 18
Const cTopRightBumper = 19
Const cBottomRightBumper = 20
Const cRightSlingShot = 21
Const cLeftSlingShot = 22
Const MaxLut = 4
'---------------------------------------------------

'---------------------------------------------------
' Setup Consts with easy names for switch numbers
Const cOutHoleSW = 9
Const cLeftOutsideRolloverSW = 10
Const cLeftInsideRolloverSW = 11
Const cLeftKickerSW = 12
Const cLeftEjectHoleSW = 13
Const cUpperMiddleLeftStandupSW = 14
Const cSpinnerSW = 15
Const cTopLeftStandupSW = 16
'Const cTarget1SW = 17
'Const cTarget2SW = 18
'Const cTarget3SW = 19
'Const cTarget4SW = 21
'Const cTarget5SW = 22
'Const cTarget6SW = 23
Const cBottomLeftBumperSW = 25
Const cTopLeftBumperSW = 26
Const cTopRightBumperSW = 27
Const cBottomRightBumperSW = 28
Const cTopCenterTargetSW = 29
Const cRightEjectHoleSW = 30
Const cUpperTopRightStandupSW = 31
Const cFRolloverSW = 32
Const cIRolloverSW = 33
Const cRRolloverSW = 34
Const cERolloverSW = 35
Const cUpperRightEjectHoleSW = 36
Const cLowerTopRightStnadupSW = 37
Const cMIddleRightStandupSW = 38
Const cTopPOWERTargetSW = 39
Const cMiddlePOWERTargetSW = 40
Const cBottomPOWERTargetSW = 41
Const cRightKickerSW = 42
Const cRightInsideRolloverSW = 43
Const cRightOutsideRolloverSW = 44
Const cFlipperRightSW = 45
Const cBallShooterSW = 46
Const cPlayfieldTiltSW = 47
Const cLowerRightStandupSW = 48
Const cCenterMiddleLeftStandupSW = 49
Const cLowerMiddleLeftStuandupSW = 50
Const cLeftBallRampSW = 51  
Const cLeftEjectRolloverSW = 53
Const cRightEjectRolloverSW = 54
Const cRightBallRampSW = 57 
Const cCenterBallRampSW = 58 
'--------------------------------------------------

'Solenoids Setup
SolCallback(cBallRelease) = "trTrough.SolIn"
SolCallback(cLeftEjectHole) = "LeftEjectHole"
SolCallback(cRightEjectHole) = "RightEjectHole"
SolCallback(cUpperEjectHole) = "UpperEjectHole"
SolCallback(cBallSaveKick) = "BallSaveKick"
SolCallback(cBallRampThrower) = "trTrough.SolOut"
SolCallback(cFlashLamps) = "Flashers"
SolCallback(cCreditKnocker) = "CreditKnocker"
SolCallback(cRightSlingShot) = "sRightSlingShot"
SolCallback(cLeftSlingShot) = "sLeftSlingShot"
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
If dtmod=1 then
     SolCallback(2) = "LBankReset"
     SolCallback(3) = "RBankReset"
End If

'------------------------------------------------------------------
' Setup Solenoid Subs
Sub LeftEjectHole(enabled)
	if enabled Then
		KLeftEjectHole.Kick 180, 15  
		controller.Switch(cLeftEjectHoleSW)=0
		Playsound "Popper_ball",0, 1*3, 0.1, 1
	end If
End Sub

Sub RightEjectHole(enabled)
	if enabled Then
		KRightEjectHole.Kick 110,25 'AXS  (180.15)
		Controller.Switch(cRightEjectHoleSW)=0
		Playsound "Popper_ball",0, 1*3, 0.1, 0.0
	end If
End Sub

Sub UpperEjectHole(enabled)
	if enabled Then
		KUpperEjectHole.Kick -90,15
		Controller.Switch(cUpperRightEjectHoleSW)=0
	Playsound "Popper_ball",0, 1*3, 0.1, 0.5
	end if
End Sub

Sub CreditKnocker(enabled)
	if enabled then
	Playsound "knocker",0, 1, 0.1, 0.5
	'DOF 14,2
	end if
end Sub

'        ______   _____  _______  ______ ______  __________
'*****  / __/ /  /  _/ |/ / ___/ / __/ // / __ \/_  __/ __/*****
'***** _\ \/ /___/ //    / (_ / _\ \/ _  / /_/ / / / _\ \  ***** 
'*****/___/____/___/_/|_/\___/ /___/_//_/\____/ /_/ /___/  *****
 
'***************************************************************                                                  
'Rstep and Lstep  are the variables that increment the animation
'***************************************************************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot()
	vpmTimer.PulseSW cRightKickerSW
End Sub

Sub LeftSlingShot_Slingshot()
	vpmTimer.PulseSW cLeftKickerSW
End Sub

Sub sRightSlingShot(enabled)
	If enabled Then
    PlaySound "slingshotRight", 0, 1, 0.05, 0.05
    'RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	End If
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSling1.Visible = 0:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub sLeftSlingShot(enabled)
	If enabled Then
    PlaySound "slingshotLeft",0,1,-0.05,0.05
    'LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	End If
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0

    End Select
    LStep = LStep + 1
End Sub

 Sub SlingHopL_Hit
     'Msgbox "left Hit"
     If Activeball.velX > 9 Then
     'Msgbox Activeball.velx
     SlingHopLTimer.Enabled = 1
     Activeball.velZ = 12
   End If  
 End Sub 

 Sub SlingHopLTimer_Timer
     Playsound "ball_bounce"
     SlingHopLTimer.Enabled = 0
   End Sub

 Sub SlingHopR_Hit
     'Msgbox "Right hit"
     If Activeball.velX > 7 Then
     'Msgbox Activeball.velx
     SlingHopRTimer.Enabled = 1
     Activeball.velZ = 12
   End If  
 End Sub 

 Sub SlingHopRTimer_Timer
     Playsound "ball_bounce"
     SlingHopRTimer.Enabled = 0
   End Sub

dim MBon
MBon=0

' Flipper Solenoid Handlers ********************************************************

sub SolRFlipper(enabled)
	if enabled Then
		PlaySound "Fx_FlipperUp", 0, 1, 0.05, 0.05
		FlipperRight.RotateToEnd

	Else
		PlaySound "Fx_FlipperDown", 0, 1, 0.05, 0.05
		FlipperRight.RotateToStart	
	end If
end Sub


sub SolLFlipper(enabled)
	if enabled Then
		PlaySound "Fx_FlipperUp", 0, 1, -0.05, 0.05
		FlipperLeft.RotateToEnd
	Else
		PlaySound "Fx_FlipperDown", 0, 1, -0.05, 0.05
		FlipperLeft.RotateToStart
	end If
end Sub

' Handle eject hole hit and unhit events and set corresponding switches *************

Sub KLeftEjectHole_Hit()
	controller.Switch(cLeftEjectHoleSW)=1
MBon=MBon-1
End Sub

Sub KLeftEjectHole_UnHit()
	controller.Switch(cLeftEjectHoleSW)=0
MBon=MBon+1
End Sub

Sub KRightEjectHole_Hit()
MBon=MBon-1
	controller.Switch(cRightEjectHoleSW)=1
End Sub

Sub KRightEjectHole_UnHit()
	controller.Switch(cRightEjectHoleSW)=0
MBon=MBon+1
End Sub

Sub KUpperEjectHole_Hit()
MBon=MBon-1
	controller.Switch(cUpperRightEjectHoleSW)=1
End Sub

Sub KUpperEjectHole_Unhit()
	controller.Switch(cUpperRightEjectHoleSW)=0
MBon=MBon+1
End Sub

Sub BallSaveKick(enabled)
	if enabled Then
		Playsound "slingshotleft", 0, .67, -0.05, 0.05
	End If

End Sub


'***************************************************************************************
'Initialize Table

Sub Table1_Init()
	vpmInit Me
	With Controller      
 		  .GameName = cGameName
          If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
          .SplashInfoLine = "Firepower Williams 1980" & vbNewLine & "Created for VPX by 3rdaxis, Slydog43 & G5K"
          .HandleKeyboard = 0
          .ShowTitle = 0
          .ShowFrame = 0
          .HandleMechanics = 0
		  .ShowDMDOnly = 0
		  .Hidden = 1
 		 .dip(0)=&h00  'Set to usa
          On Error Resume Next
         ' .Run GetPlayerHWnd
          If Err Then MsgBox Err.Description
          On Error Goto 0
      End With
		Controller.Run	
PinMameTimer.enabled = 1
vpmMapLights AllLights

vpmNudge.TiltSwitch=cPlayfieldTiltSW
    vpmNudge.Sensitivity=1

Call myAdjustTableToPrefs() 'SDS
Call myAdjustPlayfieldLamps()'AXS

'*********************************************
'Setup Ball Trough Object
Set trTrough = new cvpmTrough
trTrough.CreateEvents "trTrough", Array(Drain, BallRelease)
trTrough.balls = 3
trTrough.size = 3
trTrough.EntrySw = cOutHoleSW
trTrough.InitEntrySounds "DrainShort", "DrainShort", "DrainShort"
trTrough.InitExitSounds "BallRelease", "BallRelease"
trTrough.addsw 2,cLeftBallRampSW
trTrough.addsw 1, cCenterBallRampSW
trTrough.addsw 0, cRightBallRampSW
trTrough.initexit BallRelease, 90, 10 
trTrough.StackExitBalls = 1
trTrough.MaxBallsPerKick = 1
trTrough.Reset

'**********************************************

Dim DesktopMode: DesktopMode = Table1.ShowDT

If DesktopMode = True Then 'Show Desktop components

	DisplayTimer7.enabled = True
	P1D1.visible = 1
	P1D2.visible = 1
	P1D3.visible = 1
	P1D4.visible = 1
	P1D5.visible = 1
	P1D6.visible = 1
	P1D7.visible = 1
	P2D1.visible = 1
	P2D2.visible = 1
	P2D3.visible = 1
	P2D4.visible = 1
	P2D5.visible = 1
	P2D6.visible = 1
	P2D7.visible = 1
	P3D1.visible = 1
	P3D2.visible = 1
	P3D3.visible = 1
	P3D4.visible = 1
	P3D5.visible = 1
	P3D6.visible = 1
	P3D7.visible = 1
	P4D1.visible = 1
	P4D2.visible = 1
	P4D3.visible = 1
	P4D4.visible = 1
	P4D5.visible = 1
	P4D6.visible = 1
	P4D7.visible = 1
	BaD1.visible = 1
	BaD2.visible = 1
	CrD1.visible = 1
	CrD2.visible = 1
        RailsLockbar.visible=1
        BLADES.visible=0
        LockdownBar.visible=1 'AXS
        SideRails.visible=1     'AXS
	
Else

	DisplayTimer7.enabled = False
	P1D1.visible = 0
	P1D2.visible = 0
	P1D3.visible = 0
	P1D4.visible = 0
	P1D5.visible = 0
	P1D6.visible = 0
	P1D7.visible = 0
	P2D1.visible = 0
	P2D2.visible = 0
	P2D3.visible = 0
	P2D4.visible = 0
	P2D5.visible = 0
	P2D6.visible = 0
	P2D7.visible = 0
	P3D1.visible = 0
	P3D2.visible = 0
	P3D3.visible = 0
	P3D4.visible = 0
	P3D5.visible = 0
	P3D6.visible = 0
	P3D7.visible = 0
	P4D1.visible = 0
	P4D2.visible = 0
	P4D3.visible = 0
	P4D4.visible = 0
	P4D5.visible = 0
	P4D6.visible = 0
	P4D7.visible = 0
	BaD1.visible = 0
	BaD2.visible = 0
	CrD1.visible = 0
	CrD2.visible = 0
	BGR1up.visible=0
	BGR2up.visible=0
	BGR3up.visible=0
	BGR4up.visible=0
	BGR1cp.visible=0
	BGR2cp.visible=0
	BGR3cp.visible=0
	BGR4cp.visible=0
	BGRtilt.visible=0
	BGRgo.visible=0
	BGRsa.visible=0
	BGRmatch.visible=0
	BGRbip.visible=0
	BGRhs.visible=0
        RailsLockbar.visible=0
        BLADES.visible=1
        LockdownBar.visible=0 'AXS
	SideRails.visible=0     'AXS
      end If
   end Sub

Sub SetLUT
	Select Case MyPrefs_Brightness
		Case 4:table1.ColorGradeImage = 0
		Case 3:table1.ColorGradeImage = "AA_FS_Lut30perc"
		Case 2:table1.ColorGradeImage = "AA_FS_Lut50perc"
		Case 1:table1.ColorGradeImage = "AA_FS_Lut70perc"
		Case 0:table1.ColorGradeImage = "AA_FS_Lut100perc"
	end Select
end sub 

Sub LUTBox_Timer
	LUTBox.TimerEnabled = 0 
	LUTBox.Visible = 0
End Sub

Sub ShowLUT
	LUTBox.visible = 1
	LUTBox.text = "Brightness: " & CStr(MyPrefs_Brightness)
	LUTBox.TimerEnabled = 1
End Sub

'*************************************************************
' Setup Switch Subs

Sub LeftOutsideRollover_hit()
	vpmtimer.pulsesw cLeftOutsideRolloverSW
	if LShieldOn.state = 1 Then
		KBallSaveKicker.enabled = 1
	Else
		KballsaveKicker.enabled = 0	
	end if
End Sub

Sub LeftInsideRollover_hit:vpmtimer.pulsesw cLeftInsideRolloverSW:End Sub
Sub LeftInsideRollover_unhit:Controller.Switch(cLeftInsideRolloverSW)=0:End Sub


Sub UpperMiddleLeftStandup_hit:vpmtimer.pulsesw cUpperMiddleLeftStandupSW:Playsound "rubber_hit_3", 0, 1, -0.09, 0.09:End Sub
Sub UpperMiddleLeftStandup_unhit:Controller.Switch(cUpperMiddleLeftStandupSW)=0:End Sub

Sub Spinner_Spin:vpmTimer.PulseSw cSpinnerSW:PlaySound "fx_spinner",0,.5,0,0,25:End Sub

Sub TopLeftStandup_hit:vpmtimer.pulsesw cTopLeftStandupSW:Playsound "rubber_hit_3", 0, 1, -0.09, 0.09:End Sub
Sub TopLeftStandup_unhit:Controller.Switch(cTopLeftStandupSW)=0:End Sub


Sub Bumper4_hit:vpmTimer.PulseSw cBottomLeftBumperSW:Playsound "Fx_Bumper1",0, 1, 0.05, 0.05:End Sub

Sub Bumper1_hit:vpmTimer.PulseSw cTopLeftBumperSW:Playsound "Fx_Bumper2",0, 1, 0.05, 0.05:End Sub

Sub Bumper2_hit:vpmTimer.PulseSw cTopRightBumperSW:Playsound "Fx_Bumper3",0, 1, 0.05, 0.05:End Sub

Sub Bumper3_hit:vpmTimer.PulseSw cBottomRightBumperSW:Playsound "Fx_Bumper4",0, 1, 0.05, 0.05 End Sub


Sub FRollover_hit:vpmtimer.pulsesw cFRolloverSW:End Sub
Sub FRollover_unhit:Controller.Switch(cFRolloverSW)=0:End Sub

Sub IRollover_hit:vpmtimer.pulsesw cIRolloverSW:End Sub
Sub IRollover_unhit:Controller.Switch(cIRolloverSW)=0:End Sub

Sub RRollover_hit:vpmtimer.pulsesw cRRolloverSW:End Sub
Sub RRollover_unhit:Controller.Switch(cRRolloverSW)=0:End Sub

Sub ERollover_hit:vpmtimer.pulsesw cERolloverSW:End Sub
Sub ERollover_unhit:Controller.Switch(cERolloverSW)=0:End Sub

Sub RightInsideRollover_hit:vpmtimer.pulsesw cRightInsideRolloverSw:End Sub
Sub RightInsideRollover_unhit:Controller.Switch(cRightInsideRolloverSW)=0:End Sub

Sub RightOutsideRollover_hit:vpmtimer.pulsesw cRightOutsideRolloverSW:End Sub
Sub RightOutsideRollover_unhit:Controller.Switch(cRightOutsideRolloverSW)=0:End Sub

Sub BallShooter_hit:Controller.Switch(cBallShooterSW)=1:End Sub
Sub BallShooter_unhit:Controller.Switch(cBallShooterSW)=0:End Sub

Sub LeftEjectRollover_hit:vpmtimer.pulsesw cLeftEjectRolloverSW:End Sub
Sub LeftEjectRollover_unhit:Controller.Switch(cLeftEjectRolloverSW)=0:End Sub

Sub RightEjectRollover_hit:vpmtimer.pulsesw cRightEjectRolloverSW:End Sub
Sub RightEjectRollover_unhit:Controller.Switch(cRightEjectRolloverSW)=0:End Sub

Sub Table1_KeyDown(ByVal keycode)
	If keycode = PlungerKey Then
		Plunger.PullBack
		PlaySound "plungerpull",0,1,0.25,0.25
	End If

	If keycode = 4 or keycode = 5 or keycode = 6 Then
		Playsound "CoinIn3",0,1,0.25,0.25 
	End if

    If keycode = 2 Then            
        stopsound "_SpaceWaltz"
        stopsound "_2001"
    End If

    If keycode = RightMagnaSave  Then
		if MyPrefs_DisableBrightness = 0 then
			MyPrefs_Brightness = MyPrefs_Brightness + 1
			if MyPrefs_Brightness > MaxLut then MyPrefs_Brightness = 0
			SetLUT
			ShowLUT
			Playsound "button-click"
		end if
	end if
	If keycode = LeftMagnaSave  Then
          if MyPrefs_DisableBrightness = 0 then
			MyPrefs_Brightness= MyPrefs_Brightness - 1
			if MyPrefs_Brightness < 0 then MyPrefs_Brightness = MaxLut
			SetLUT
			ShowLUT
			Playsound "button-clickOff"
		end if
end if

   vpmKeyDown(keycode)
   'Msgbox Keycode
 End Sub

    Sub Table1_KeyUp(ByVal keycode)
      	 vpmKeyUp(keycode)
	  If keycode = PlungerKey Then        
	 	 Plunger.Fire
		 PlaySound "plunger",0,1,0.25,0.25
      End If
  End Sub

'****************************************************************

'Backglass

Sub Backglass_Timer()
If BGL1up.State=1 then BGR1up.SetValue(1) End If
If BGL1up.State=0 then BGR1up.SetValue(0) End If
If BGL2up.State=1 then BGR2up.SetValue(1) End If
If BGL2up.State=0 then BGR2up.SetValue(0) End If
If BGL3up.State=1 then BGR3up.SetValue(1) End If
If BGL3up.State=0 then BGR3up.SetValue(0) End If
If BGL4up.State=1 then BGR4up.SetValue(1) End If
If BGL4up.State=0 then BGR4up.SetValue(0) End If
If BGL1cp.State=1 then BGR1cp.SetValue(1) End If
If BGL1cp.State=0 then BGR1cp.SetValue(0) End If
If BGL2cp.State=1 then BGR2cp.SetValue(1) End If
If BGL2cp.State=0 then BGR2cp.SetValue(0) End If
If BGL3cp.State=1 then BGR3cp.SetValue(1) End If
If BGL3cp.State=0 then BGR3cp.SetValue(0) End If
If BGL4cp.State=1 then BGR4cp.SetValue(1) End If
If BGL4cp.State=0 then BGR4cp.SetValue(0) End If
If BGLbip.State=1 then BGRbip.SetValue(1) End If
If BGLbip.State=0 then BGRbip.SetValue(0) End If
If BGLmatch.State=1 then BGRmatch.SetValue(1) End If
If BGLmatch.State=0 then BGRmatch.SetValue(0) End If
If BGLgo.State=1 then BGRgo.SetValue(1) End If
If BGLgo.State=0 then BGRgo.SetValue(0) End If
If BGLsa.State=1 then BGRsa.SetValue(1) End If
If BGLsa.State=0 then BGRsa.SetValue(0) End If
If BGLhs.State=1 then BGRhs.SetValue(1) End If
If BGLhs.State=0 then BGRhs.SetValue(0) End If
If BGLtilt.State=1 then BGRtilt.SetValue(1) End If
If BGLtilt.State=0 then BGRtilt.SetValue(0) End If

End Sub

'--------------------------------------------------------------
'*********BALLKICKER************

Sub KBallSaveKicker_Hit()
	
		Me.kick 0, 25
		Playsound "SlingshotLeft"

End Sub

'--------------------------------------------------------------
' Play sounds when gates are Hit
Sub Gate3_Hit()
	Playsound "GateWire",0, Vol(ActiveBall), 0.05, 0.05
End Sub

Sub Gate1_Hit()
	Playsound "GateWire",0, Vol(ActiveBall), 0.05, 0.05:
End Sub

Sub Gate2_Hit()
	Playsound "GateWire",0, Vol(ActiveBall), 0.05, 0.05
End Sub

Sub BallReleaseGate_Hit()
	Playsound "GateWire",0, Vol(ActiveBall), 0.05, 0.05
End Sub

'=========================================================
'                    LED Handling
'=========================================================
'Modified version of Scapino's LED code for Fathom
'and borrowed from Uncle Willy's VP9 Firepower
'
Dim SixDigitOutput(32)
Dim SevenDigitOutput(32)
Dim DisplayPatterns(11)
Dim DigStorage(32)

'Binary/Hex Pattern Recognition Array
DisplayPatterns(0) = 0		'0000000 Blank
DisplayPatterns(1) = 63		'0111111 zero
DisplayPatterns(2) = 6		'0000110 one
DisplayPatterns(3) = 91		'1011011 two
DisplayPatterns(4) = 79		'1001111 three
DisplayPatterns(5) = 102	'1100110 four
DisplayPatterns(6) = 109	'1101101 five
DisplayPatterns(7) = 125	'1111101 six
DisplayPatterns(8) = 7		'0000111 seven
DisplayPatterns(9) = 127	'1111111 eight
DisplayPatterns(10)= 111	'1101111 nine

'Assign 7-digit output to reels
Set SevenDigitOutput(0)  = P1D7
Set SevenDigitOutput(1)  = P1D6
Set SevenDigitOutput(2)  = P1D5
Set SevenDigitOutput(3)  = P1D4
Set SevenDigitOutput(4)  = P1D3
Set SevenDigitOutput(5)  = P1D2
Set SevenDigitOutput(6)  = P1D1

Set SevenDigitOutput(7)  = P2D7
Set SevenDigitOutput(8)  = P2D6
Set SevenDigitOutput(9)  = P2D5
Set SevenDigitOutput(10) = P2D4
Set SevenDigitOutput(11) = P2D3
Set SevenDigitOutput(12) = P2D2
Set SevenDigitOutput(13) = P2D1

Set SevenDigitOutput(14) = P3D7
Set SevenDigitOutput(15) = P3D6
Set SevenDigitOutput(16) = P3D5
Set SevenDigitOutput(17) = P3D4
Set SevenDigitOutput(18) = P3D3
Set SevenDigitOutput(19) = P3D2
Set SevenDigitOutput(20) = P3D1

Set SevenDigitOutput(21) = P4D7
Set SevenDigitOutput(22) = P4D6
Set SevenDigitOutput(23) = P4D5
Set SevenDigitOutput(24) = P4D4
Set SevenDigitOutput(25) = P4D3
Set SevenDigitOutput(26) = P4D2
Set SevenDigitOutput(27) = P4D1

Set SevenDigitOutput(28) = CrD2
Set SevenDigitOutput(29) = CrD1
Set SevenDigitOutput(30) = BaD2
Set SevenDigitOutput(31) = BaD1

Sub DisplayTimer7_Timer ' 7-Digit output
	On Error Resume Next
	Dim ChgLED,ii,chg,stat,obj,TempCount,temptext,adj

	ChgLED = Controller.ChangedLEDs(&HFF, &HFFFF) 'hex of binary (display 111111, or first 6 digits)

	If Not IsEmpty(ChgLED) Then
		For ii = 0 To UBound(ChgLED)
			chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
			For TempCount = 0 to 10
				If stat = DisplayPatterns(TempCount) then
					If LedStatus = 2 Then SevenDigitOutput(chgLED(ii, 0)).SetValue(TempCount)
					DigStorage(chgLED(ii, 0)) = TempCount
				End If
				If stat = (DisplayPatterns(TempCount) + 128) then
					If LedStatus = 2 Then SevenDigitOutput(chgLED(ii, 0)).SetValue(TempCount)
					DigStorage(chgLED(ii, 0)) = TempCount
				End If
			Next
		Next
	End IF
End Sub


'*DOF method for non rom controller tables by Arngrim****************
'*******Use DOF 1**, 1 to activate a ledwiz output*******************
'*******Use DOF 1**, 0 to deactivate a ledwiz output*****************
'*******Use DOF 1**, 2 to pulse a ledwiz output**********************
'Sub DOF(dofevent, dofstate) 
'	If B2SOn=True Then
'		If dofstate = 2 Then
'			Controller.B2SSetData dofevent, 1:Controller.B2SSetData dofevent, 0
'		Else
'			Controller.B2SSetData dofevent, dofstate
'		End If
'	End If
'End Sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
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

Const tnob = 4 ' total number of balls
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
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0
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
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 500, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub

'*************SUPPORTING SOUNDS************************

Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Metal_Hit (idx)
	PlaySound "metalhit_thin", 0, 0.05, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RubbersLower_Hit'(idx)  
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

Sub FlipperLeft_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub FlipperRight_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

'        __   ___   __  ______  ____
'*****  / /  / _ | /  |/  / _ \/ __/***** 
'***** / /__/ __ |/ /|_/ / ___/\ \  ***** 
'*****/____/_/ |_/_/  /_/_/  /___/  *****
                             
Sub myAdjustPlayfieldLamps()

  Select Case myPrefs_PlayfieldLamps
    Case 0 'Factory
      LF.Intensity=2:LF.Color=RGB(0,128,0):LF.ColorFull=RGB(0,255,0):LF.Image= "g_g5kplayfield"
      LI.Intensity=2:LI.Color=RGB(0,128,0):LI.ColorFull=RGB(0,255,0):LI.Image= "g_g5kplayfield"
      LR.Intensity=4:LR.Color=RGB(0,128,0):LR.ColorFull=RGB(0,255,0):LR.Image= "g_g5kplayfield"
      LE.Intensity=2:LE.Color=RGB(0,111,0):LE.ColorFull=RGB(0,111,0):LE.Image= "g_g5kplayfield"
      L2X.Intensity=200:L2X.Color=RGB(0,128,0):L2X.ColorFull=RGB(0,255,0):L2X.Image= "g_g5kplayfield"
      L3X.Intensity=200:L3X.Color=RGB(0,128,0):L3X.ColorFull=RGB(0,255,0):L3X.Image= "g_g5kplayfield"
      L4X.Intensity=200:L4X.Color=RGB(0,128,0):L4X.ColorFull=RGB(0,255,0):L4X.Image= "g_g5kplayfield"
      L5X.Intensity=200:L5X.Color=RGB(0,128,0):L5X.ColorFull=RGB(0,255,0):L5X.Image= "g_g5kplayfield"
      L1.Intensity=20:L1.Color=RGB(177,170,82):L1.ColorFull=RGB(177,170,82):L1.Image= "g_g5kplayfield"
      L2.Intensity=20:L2.Color=RGB(177,170,82):L2.ColorFull=RGB(177,170,82):L2.Image= "g_g5kplayfield"
      L3.Intensity=20:L3.Color=RGB(177,170,82):L3.ColorFull=RGB(177,170,82):L3.Image= "g_g5kplayfield"
      L4.Intensity=20:L4.Color=RGB(177,170,82):L4.ColorFull=RGB(177,170,82):L4.Image= "g_g5kplayfield"
      L5.Intensity=20:L5.Color=RGB(177,170,82):L5.ColorFull=RGB(177,170,82):L5.Image= "g_g5kplayfield"
      L6.Intensity=20:L6.Color=RGB(177,170,82):L6.ColorFull=RGB(177,170,82):L6.Image= "g_g5kplayfield"
      L7.Intensity=20:L7.Color=RGB(177,170,82):L7.ColorFull=RGB(177,170,82):L7.Image= "g_g5kplayfield"
      L8.Intensity=20:L8.Color=RGB(177,170,82):L8.ColorFull=RGB(177,170,82):L8.Image= "g_g5kplayfield"
      L9.Intensity=20:L9.Color=RGB(177,170,82):L9.ColorFull=RGB(177,170,82):L9.Image= "g_g5kplayfield"
      LT1.Intensity=20:LT1.Color=RGB(223,80,0):LT1.ColorFull=RGB(255,226,128):LT1.Image= "g_g5kplayfield"
      LT2.Intensity=20:LT2.Color=RGB(223,80,0):LT2.ColorFull=RGB(255,226,128):LT2.Image= "g_g5kplayfield"
      LT3.Intensity=20:LT3.Color=RGB(223,80,0):LT3.ColorFull=RGB(255,226,128):LT3.Image= "g_g5kplayfield"
      LT4.Intensity=20:LT4.Color=RGB(223,80,0):LT4.ColorFull=RGB(255,226,128):LT4.Image= "g_g5kplayfield"
      LT5.Intensity=20:LT5.Color=RGB(223,80,0):LT5.ColorFull=RGB(255,226,128):LT5.Image= "g_g5kplayfield"
      LT6.Intensity=20:LT6.Color=RGB(223,80,0):LT6.ColorFull=RGB(255,226,128):LT6.Image= "g_g5kplayfield"
      L10.Intensity=20:L10.Color=RGB(177,170,82):L10.ColorFull=RGB(177,170,82):L10.Image= "g_g5kplayfield"
      L20.Intensity=20:L20.Color=RGB(177,170,82):L20.ColorFull=RGB(177,170,82):L20.Image= "g_g5kplayfield"
      L10K.Intensity=300:L10K.Color=RGB(255,128,0):L10K.ColorFull=RGB(255,183,111):L10K.Image= "g_g5kplayfield"
      L30K.Intensity=300:L30K.Color=RGB(255,128,0):L30K.ColorFull=RGB(255,183,111):L30K.Image= "g_g5kplayfield"
      L50K.Intensity=200:L50K.Color=RGB(255,128,0):L50K.ColorFull=RGB(255,183,111):L50K.Image= "g_g5kplayfield"
      LFire1.Intensity=5:LFire1.Color=RGB(210,0,0):LFire1.ColorFull=RGB(255,255,255):LFire1.Image= "g_g5kplayfield"
      LFire2.Intensity=5:LFire2.Color=RGB(210,0,0):LFire2.ColorFull=RGB(255,255,255):LFire2.Image= "g_g5kplayfield"
      LPower1.Intensity=5:LPower1.Color=RGB(0,0,255):LPower1.ColorFull=RGB(132,132,255):LPower1.Image= "g_g5kplayfield"
      LPower2.Intensity=5:LPower2.Color=RGB(0,0,255):LPower2.ColorFull=RGB(132,132,255):LPower2.Image= "g_g5kplayfield"
      LBlueTop.Intensity=5:LBlueTop.Color=RGB(0,0,255):LBlueTop.ColorFull=RGB(132,132,255):LBlueTop.Image="g_g5kplayfield"
      LSpinner.Intensity=200:LSpinner.Color=RGB(149,30,0):LSpinner.ColorFull=RGB(255,43,43):LSpinner.Image= "g_g5kplayfield"
      LLeftHole.Intensity=50:LLeftHole.Color=RGB(0,128,0):LLeftHole.ColorFull=RGB(0,255,0): LLeftHole.Image= "g_g5kplayfield"
      LRightHole.Intensity=100:LRightHole.Color=RGB(0,128,0):LRightHole.ColorFull=RGB(0,255,0): LRightHole.Image= "g_g5kplayfield"
      LBackHole.Intensity=100:LBackHole.Color=RGB(0,128,0):LBackHole.ColorFull=RGB(0,255,0): LBackHole.Image= "g_g5kplayfield"
      LBlueMiddle.Intensity=5:LBlueMiddle.Color=RGB(0,0,255):LBlueMiddle.ColorFull=RGB(132,132,255):LBlueMiddle.Image= "g_g5kplayfield"
      LBlueBottom.Intensity=5:LBlueBottom.Color=RGB(0,0,255):LBlueBottom.ColorFull=RGB(132,132,255):LBlueBottom.Image= "g_g5kplayfield"
      LExtraBall.Intensity=200:LExtraBall.Color=RGB(255,55,55):LExtraBall.ColorFull=RGB(255,88,17):LExtraBall.Image= "g_g5kplayfield"
      LLeftBlue1000.Intensity=50:LLeftBlue1000.Color=RGB(0,0,255):LLeftBlue1000.ColorFull=RGB(132,132,255):LLeftBlue1000.Image=" g_g5kplayfield"
      LRightBlue1000.Intensity=50:LRightBlue1000.Color=RGB(0,0,255):LRightBlue1000.ColorFull=RGB(132,132,255):LRightBlue1000.Image= "g_g5kplayfield"
      LShieldOn.Intensity=300:LShieldOn.Color=RGB(115,28,23):LShieldOn.ColorFull=RGB(255,156,128):LShieldOn.Image= "g_g5kplayfield"
      LLeftSpecial.Intensity=5:LLeftSpecial.Color=RGB(255,26,0):LLeftSpecial.ColorFull=RGB(255,254,253):LLeftSpecial.Image= "g_g5kplayfield"
      LRightSpecial.Intensity=5:LRightSpecial.Color=RGB(255,26,0):LRightSpecial.ColorFull=RGB(255,254,253):LRightSpecial.Image= "g_g5kplayfield"
      LShootAgain.Intensity=5:LShootAgain.Color=RGB(55,3,0):LShootAgain.ColorFull=RGB(255,198,198):LShootAgain.Image= "g_g5kplayfield"
      LShootAgainB.Intensity=50:LShootAgainB.Color=RGB(55,3,0):LShootAgainB.ColorFull=RGB(255,198,198)
  Case 1 'LED
      LF.Intensity=800:LF.Color=RGB(0,132,0):LF.ColorFull=RGB(94,255,94):LF.Image= "g_g5kplayfield3"
      LI.Intensity=800:LI.Color=RGB(0,132,0):LI.ColorFull=RGB(94,255,94):LI.Image= "g_g5kplayfield3"
      LR.Intensity=800:LR.Color=RGB(0,132,0):LR.ColorFull=RGB(94,255,94):LR.Image= "g_g5kplayfield3"
      LE.Intensity=800:LE.Color=RGB(0,132,0):LE.ColorFull=RGB(94,255,94):LE.Image= "g_g5kplayfield3"
      L2X.Intensity=1500:L2X.Color=RGB(0,79,0):L2X.ColorFull=RGB(104,255,83):L2X.Image= "g_g5kplayfield3"
      L3X.Intensity=1500:L3X.Color=RGB(0,79,0):L3X.ColorFull=RGB(104,255,83):L3X.Image= "g_g5kplayfield3"
      L4X.Intensity=1500:L4X.Color=RGB(0,79,0):L4X.ColorFull=RGB(104,255,83):L4X.Image= "g_g5kplayfield3"
      L5X.Intensity=1500:L5X.Color=RGB(0,79,0):L5X.ColorFull=RGB(104,255,83):L5X.Image= "g_g5kplayfield3"
      L1.Intensity=600:L1.Color=RGB(207,203,148):L1.ColorFull=RGB(255,255,128):L1.Image= "g_g5kplayfield3"
      L2.Intensity=600:L2.Color=RGB(207,203,148):L2.ColorFull=RGB(255,255,128):L2.Image= "g_g5kplayfield3"
      L3.Intensity=600:L3.Color=RGB(207,203,148):L3.ColorFull=RGB(255,255,128):L3.Image= "g_g5kplayfield3"
      L4.Intensity=600:L4.Color=RGB(207,203,148):L4.ColorFull=RGB(255,255,128):L4.Image= "g_g5kplayfield3"
      L5.Intensity=600:L5.Color=RGB(207,203,148):L5.ColorFull=RGB(255,255,128):L5.Image= "g_g5kplayfield3"
      L6.Intensity=600:L6.Color=RGB(207,203,148):L6.ColorFull=RGB(255,255,128):L6.Image= "g_g5kplayfield3"
      L7.Intensity=600:L7.Color=RGB(207,203,148):L7.ColorFull=RGB(255,255,128):L7.Image= "g_g5kplayfield3"
      L8.Intensity=600:L8.Color=RGB(207,203,148):L8.ColorFull=RGB(255,255,128):L8.Image= "g_g5kplayfield3"
      L9.Intensity=600:L9.Color=RGB(207,203,148):L9.ColorFull=RGB(255,255,128):L9.Image= "g_g5kplayfield3"
      LT1.Intensity=800:LT1.Color=RGB(223,84,0):LT1.ColorFull=RGB(255,255,128):LT1.Image= "g_g5kplayfield3"
      LT2.Intensity=800:LT2.Color=RGB(223,84,0):LT2.ColorFull=RGB(255,255,128):LT2.Image= "g_g5kplayfield3"
      LT3.Intensity=800:LT3.Color=RGB(223,84,0):LT3.ColorFull=RGB(255,255,128):LT3.Image= "g_g5kplayfield3"
      LT4.Intensity=800:LT4.Color=RGB(223,84,0):LT4.ColorFull=RGB(255,255,128):LT4.Image= "g_g5kplayfield3"
      LT5.Intensity=800:LT5.Color=RGB(223,84,0):LT5.ColorFull=RGB(255,255,128):LT5.Image= "g_g5kplayfield3"
      LT6.Intensity=800:LT6.Color=RGB(223,84,0):LT6.ColorFull=RGB(255,255,128):LT6.Image= "g_g5kplayfield3"
      L10.Intensity=600:L10.Color=RGB(207,203,148):L10.ColorFull=RGB(255,255,128):L10.Image= "g_g5kplayfield3"
      L20.Intensity=600:L20.Color=RGB(207,203,148):L20.ColorFull=RGB(255,255,128):L20.Image= "g_g5kplayfield3"
      L10K.Intensity=1000:L10K.Color=RGB(255,51,0):L10K.ColorFull=RGB(255,148,40):L10K.Image= "g_g5kplayfield3"
      L30K.Intensity=1000:L30K.Color=RGB(255,51,0):L30K.ColorFull=RGB(255,148,40):L30K.Image= "g_g5kplayfield3"
      L50K.Intensity=1000:L50K.Color=RGB(255,51,0):L50K.ColorFull=RGB(255,148,40):L50K.Image= "g_g5kplayfield3"
      LFire1.Intensity=1000:LFire1.Color=RGB(255,34,34):LFire1.ColorFull=RGB(255,164,72):LFire1.Image= "g_g5kplayfield3"
      LFire2.Intensity=3000:LFire2.Color=RGB(255,34,34):LFire2.ColorFull=RGB(255,164,72):LFire2.Image= "g_g5kplayfield3"
      LPower1.Intensity=1000:LPower1.Color=RGB(0,0,128):LPower1.ColorFull=RGB(202,228,255):LPower1.Image= "g_g5kplayfield3"
      LPower2.Intensity=800:LPower2.Color=RGB(0,0,128):LPower2.ColorFull=RGB(202,228,255):LPower2.Image= "g_g5kplayfield3"
      LBlueTop.Intensity=300:LBlueTop.Color=RGB(0,0,128):LBlueTop.ColorFull=RGB(202,228,255):LBlueTop.Image= "g_g5kplayfield3"
      LSpinner.Intensity=600:LSpinner.Color=RGB(166,0,0):LSpinner.ColorFull=RGB(249,37,0):LSpinner.Image= "g_g5kplayfield3"
      LLeftHole.Intensity=3000:LLeftHole.Color=RGB(0,128,0):LLeftHole.ColorFull=RGB(0,255,0):LLeftHole.Image= "g_g5kplayfield3"
      LRightHole.Intensity=800:LRightHole.Color=RGB(0,128,0):LRightHole.ColorFull=RGB(0,255,0):LRightHole.Image= "g_g5kplayfield3"
      LBackHole.Intensity=800:LBackHole.Color=RGB(0,128,0):LBackHole.ColorFull=RGB(0,255,0):LBackHole.Image= "g_g5kplayfield3"
      LBlueMiddle.Intensity=300:LBlueMiddle.Color=RGB(0,0,128):LBlueMiddle.ColorFull=RGB(202,228,255):LBlueMiddle.Image= "g_g5kplayfield3"
      LBlueBottom.Intensity=300:LBlueBottom.Color=RGB(0,0,128):LBlueBottom.ColorFull=RGB(202,228,255):LBlueBottom.Image= "g_g5kplayfield3"
      LExtraBall.Intensity=400:LExtraBall.Color=RGB(128,0,0):LExtraBall.ColorFull=RGB(251,32,0):LExtraBall.Image= "g_g5kplayfield3"
      LLeftBlue1000.Intensity=800:LLeftBlue1000.Color=RGB(0,0,128):LLeftBlue1000.ColorFull=RGB(0,128,255):LLeftBlue1000.Image= "g_g5kplayfield3"
      LRightBlue1000.Intensity=350:LRightBlue1000.Color=RGB(0,0,128):LRightBlue1000.ColorFull=RGB(0,128,255):LRightBlue1000.Image= "g_g5kplayfield3"
      LShieldOn.Intensity=3000:LShieldOn.Color=RGB(115,28,23):LShieldOn.ColorFull=RGB(255,156,128):LShieldOn.Image= "g_g5kplayfield3"
      LLeftSpecial.Intensity=800:LLeftSpecial.Color=RGB(128,64,0):LLeftSpecial.ColorFull=RGB(255,128,0):LLeftSpecial.Image= "g_g5kplayfield3"
      LRightSpecial.Intensity=300:LRightSpecial.Color=RGB(128,64,0):LRightSpecial.ColorFull=RGB(255,128,0):LRightSpecial.Image= "g_g5kplayfield3"
      LShootAgain.Intensity=50:LShootAgain.Color=RGB(113,0,0):LShootAgain.ColorFull=RGB(244,184,0):LShootAgain.Image= "g_g5kplayfield3"
      LShootAgainB.Intensity=100:LShootAgainB.Color=RGB(55,3,0):LShootAgainB.ColorFull=RGB(255,198,198)
  End Select
End Sub

 Sub LightCopy_TIMER

    LBackHole1.State = LBackHole.State
    LBlueBottomB.State = LBlueBottom.State 
    LBlueMiddleB.State = LBlueMiddle.State
    LBlueTopB.State = LBlueTop.State
    LShootAgainB.State = LShootAgain.State
    LBumperTopLeftB.State = LBumperTopLeft.State
    LBumperTopRightB.State = LBumperTopRight.State
    LBumperBottomRightB.State = LBumperBottomRight.State
    LBumperBottomLeftB.State = LBumperBottomLeft.State

  If LBumperTopLeft.State=1 Then
             BumpCap1.image ="g_BUMPERCAP1_LIGHTON"
             BR1_7.image = "g_BUMPERRING1_LIGHTON"
             BumpSkirt1.image =  "g_BUMPERSKIRT1_LIGHTON"
       Else
             BumpCap1.image = "g_BUMPERCAP1_LIGHTOFF"
             BR1_7.image = "g_BUMPERRING1_LIGHTOFF"
             BumpSkirt1.image =  "g_BUMPERSKIRT1_LIGHTOFF"
        End If

  If LBumperTopRight.State=1 Then
             BumpCap2.image ="g_BUMPERCAP2_LIGHTON"
	     BR2_7.image = "g_BUMPERRING2_LIGHTON"
       	     BumpSkirt2.image =  "g_BUMPERSKIRT2_LIGHTON"
       Else
	     BumpCap2.image = "g_BUMPERCAP2_LIGHTOFF"
	     BR2_7.image = "g_BUMPERRING2_LIGHTOFF"
	     BumpSkirt2.image =  "g_BUMPERSKIRT2_LIGHTOFF"
	  End If

  If LBumperBottomRight.State=1 Then
	      BumpCap3.image ="g_BUMPERCAP3_LIGHTON"
	      BR3_7.image = "g_BUMPERRING3_LIGHTON"
	      BumpSkirt3.image =  "g_BUMPERSKIRT3_LIGHTON"
	Else
	      BumpCap3.image = "g_BUMPERCAP3_LIGHTOFF"
	      BR3_7.image = "g_BUMPERRING3_LIGHTOFF"
	      BumpSkirt3.image =  "g_BUMPERSKIRT3_LIGHTOFF"
	  End If

  If LBumperBottomLeft.State=1 Then
	       BumpCap4.image ="g_BUMPERCAP4_LIGHTON"
	       BR4_7.image = "g_BUMPERRING4_LIGHTON"
	       BumpSkirt4.image =  "g_BUMPERSKIRT4_LIGHTON"
	 Else
		BumpCap4.image = "g_BUMPERCAP4_LIGHTOFF"
		BR4_7.image = "g_BUMPERRING4_LIGHTOFF"
		BumpSkirt4.image =  "g_BUMPERSKIRT4_LIGHTOFF"
	   End If

'        ________  __  ___  _____    ____      __  _______  ______________  _  __
'*****  / __/ __ \/ / / / |/ / _ \  / __/___  /  |/  / __ \/_  __/  _/ __ \/ |/ /*****
'***** _\ \/ /_/ / /_/ /    / // /  > _/_ _/ / /|_/ / /_/ / / / _/ // /_/ /    / ***** 
'*****/___/\____/\____/_/|_/____/  |_____/  /_/  /_/\____/ /_/ /___/\____/_/|_/  *****
                                                                    

 If BGLmatch.State = 1 and Not myPlayingEndGameSnd Then 
     If myPrefs_PlayMusic then	   
	    myPlayingEndGameSnd = True
                 GameOverTimer.enabled = 1
	 End If
     End If
       If BGLmatch.State = 0 Then
            myPlayingEndGameSnd = False
            Bumper1.HasHitEvent=1
            Bumper2.HasHitEvent=1
            Bumper3.HasHitEvent=1
            Bumper4.HasHitEvent=1
            GameOverTimer.Enabled = 0
            'KBallSaveKicker.Enabled=1
         Else
            Bumper1.HasHitEvent=0
            Bumper2.HasHitEvent=0
            Bumper3.HasHitEvent=0
            Bumper4.HasHitEvent=0
            KBallSaveKicker.Enabled=0
        End If
      If BGLtilt.State=1 Then
            Bumper1.HasHitEvent=0
            Bumper2.HasHitEvent=0
            Bumper3.HasHitEvent=0
            Bumper4.HasHitEvent=0
       Else
            Bumper1.HasHitEvent=1
            Bumper2.HasHitEvent=1
            Bumper3.HasHitEvent=1
            Bumper4.HasHitEvent=1
       End If
          End Sub

 Sub GameOverTimer_Timer 
    Select Case Int(Rnd()*2)
      Case 0       
        PlaySound "_SpaceWaltz" 
      Case 1 
        PlaySound "_2001"
    End Select
	BGLmatch.State = 0
	 myPlayingEndGameSnd = False
	  GameOverTimer.Enabled = 0
 End Sub

 Sub Trigger1_Hit      
        stopsound "_SpaceWaltz"
         stopsound "_2001"
          stopSound "_SpaceAmbient"
 End Sub

 Sub myAdjustTableToPrefs 
    If myPrefs_PlayMusic then                              
        PlaySound "_SpaceAmbient"
    End If
 End Sub 

'********************************************************************

 Sub Glass_Hit
   Select Case Int(Rnd()*4)
     Case 0
       Playsound "AXSGlassHit6"
       Playsound "AXSGlassHit6"
     Case 1
       Playsound "AXSGlassHit3"
       Playsound "AXSGlassHit3"
     Case 2
       Playsound "AXSGlassHit3"
     Case 3
       Playsound "AXSGlassHit6"
     End Select
       BB1Timer.Enabled = 1
   End Sub

   Sub BB1Timer_Timer
	   Playsound "ball_bounce"
	   BB1Timer.Enabled = 0
    End Sub 

'******************************************************************

 Sub Glass_Hit
   If myPrefs_GlassHit = 2 Then
    Select Case Int(Rnd()*4)
     Case 0
       Playsound "AXSGlassHit6"
       Playsound "AXSGlassHit6"
      Case 1
       Playsound "AXSGlassHit3"
       Playsound "AXSGlassHit3"
      Case 2
       Playsound "AXSGlassHit3"
      Case 3
       Playsound "AXSGlassHit6"
      End Select
       BB1Timer.Enabled = 1
     End If
   If myPrefs_GlassHit = 1 Then
    Select Case Int(Rnd()*6)
      Case 0
      Case 1
      Case 2
       Playsound "AXSGlassHit3"
      Case 3
       Playsound "AXSGlassHit6"
      Case 4
      Case 5
      End Select
       BB1Timer.Enabled = 1
     End If
   If myPrefs_GlassHit = 0 Then
       BB1Timer.Enabled = 1
     End If
   End Sub

'*********************************************************************

Set MotorCallback = GetRef("RealTimeUpdates")

Sub RealTimeUpdates
    GateFlap.roty= 0 - Gate3.Currentangle / 1
    SLWGate.rotx= -30 - Gate2.Currentangle / -1
    ULWGate.rotx= -30 - Gate1.Currentangle / -1
	Select case myPrefs_DynamicBackground
	   Case 0
		 StarsTimer.enabled = 0
	   Case 1
		 StarsTimer.enabled = 1
	End Select
   End Sub

Sub StarsTimer_Timer()                                
   ItsFullOfStars.RotZ = ItsFullOfStars.RotZ + .0055 
End Sub

   Sub BB1Timer_Timer
	   Playsound "_ball_bounce"
	   BB1Timer.Enabled = 0
    End Sub 

'**********************************************************************
'************************* G5K Ball Shadows ***************************
'**********************************************************************

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
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 5
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 5
        End If
        ballShadow(b).Y = BOT(b).Y + 10
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub

Sub L20_Init()
	
End Sub

Sub L20_Timer()
	
End Sub