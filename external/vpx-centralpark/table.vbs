'---------------------
'-  OriginalScript by ROSVE  -
'-  Porting for VPX
'-  during Covid 2021 by TGX
'---------------------

Const BallSize = 50 ' 50 is the normal size
Const BallMass = 1.8  ' 1 is the normal ball mass.

Dim Controller, B2SOn
Dim Score
B2SOn = true
If B2SOn Then Set Controller = CreateObject("B2S.Server"): Controller.Run

 Dim Renudge
 Dim roll
 Dim ball
 Dim RBonus
 Dim RBonusQueue
 Dim LBonus
 Dim LBonusQueue
 Dim GameSeq
 Dim MaxPlayers
 Dim ActivePlayer
 Dim Round
 Dim BallInPlay
 Dim Coins
 Dim Tilt
 Dim FreePlayNum,HiScore
 Dim AddBall(4)
 Dim I
 Dim ii
 Dim be,alt,Y15,G610,monkey,inpos, monkeypos
 Dim OldScore, NewScore, tempcounter

 'ExecuteGlobal GetTextFile("b2s.vbs")

Sub Table1_Exit
	If B2SOn Then Controller.Stop
End Sub

 Sub Table1_Init()
	If B2SOn Then Controller.B2SSetGameOver 1
    'SetB2SScoreSounds "bell", "bell", "bell", "bell", "bell", "bell", "buzzer" 'Configure score sounds
    'ResetB2SData 0,49,0 'Initialise the b2s data area
    'LaunchBackGlass "CentralPark_FS_B2S",true 'True=Launch bg ,  False=Don't launch bg.
    On Error Resume Next
	HiScore=Cdbl(LoadValue(CentralPark,"HiScore"))
	If HiScore="" Then HiScore=0
    DisplayHS
    Coins=0
    GameSeq=0  'Game Over
    'SetB2SData 34,1
    For Each I In LightControl 
      I.State =0
    Next
	KickerB1.CreateBall
	KickerB2.CreateBall
	KickerB3.CreateBall
	KickerB4.CreateBall
	KickerB5.CreateBall
	BumperOff 1
	BumperOff 4
	BumperOff 2
	BumperOff 3
	BstackStop.IsDropped=True
	inpos=0
	LightL5.timerinterval=20
End Sub
 
 Sub InitGame()
    If B2SOn Then
		Controller.B2SSetGameOver 0
		Controller.B2SSetScorePlayer 1, 0
		Controller.B2SSetData 98,0
		Controller.B2SSetMatch 0
	End If
    For Each I In LightControl 
      I.State =1 
    Next
    For Each I In GBonusLights
      I.State =0
	Next
	For Each I In YBonusLights
      I.State =0
    Next
    BumperOff 1
    BumperOff 4
    BumperOn 2
    BumperOn 3
    LBonus=1
    RBonus=1	'0=10
	SetBonusLights
    Y15=0
    G610=0
	Score=0
    monkey=0
	monkeypos=92
 End Sub

Sub InitNewBall()
    If B2SOn Then Controller.B2SSetBallInPlay (6-Round)
    R15.Disabled=0
    R20.Disabled=0
    R30.Disabled=0
	BstackStop.IsDropped=false
    R27.Disabled=0
    R25.Disabled=0
    R34.Disabled=0
    LightLS.TimerEnabled=true
End Sub
 
'******DefineKeysUsed
 
 Sub Table1_KeyDown(ByVal keycode)
     If keycode = 6 Then
         If Coins < 9 Then
		   Coins=Coins+1
           If B2SOn Then Controller.B2SSetCredits Coins
         End If
        PlaySound "Coin"
        ' SetCoinLights
	 End If

     If keycode = 5 Then
         If Coins < 9 Then
		   Coins=Coins+1
           If B2SOn Then Controller.B2SSetCredits Coins
         End If
        PlaySound "Coin"
        ' SetCoinLights
	 End If

     If keycode = StartGameKey Then
         If MaxPlayers < 1 And Coins>0 Then
		   MaxPlayers=MaxPlayers+1
            AddPlayer
        End If    
	 End If

	 If keycode = PlungerKey Then
		Plunger.PullBack
         PlaySound "PlungePull"
	 End If

	 If keycode = LeftFlipperKey  AND MaxPlayers>0 Then
         If Tilt<3 And Round>0 Then
			Flipper1.RotateToEnd:LFlip.objrotz = LFlip.objrotz -53
           PlaySound "flipper"
         End If
	 End If

	 If keycode = RightFlipperKey  AND MaxPlayers>0 Then
         If Tilt<3 And Round>0 Then
		    Flipper3.RotateToEnd:RFlip.objrotz = RFlip.objrotz +53
		   PlaySound "flipper"
         End If
	 End If

	 If keycode = LeftTiltKey Then
      PlaySound "nudge_left"
      If BallInPlay=1 Then
       If Tilt<3 Then
		Nudge 90, 1.4
          Tilt2Timer.Enabled=1
          Renudge=-90
         If Round>0 Then
           Tilt=Tilt+1
           If Tilt>2 Then
              TiltOn
           Else
             TiltTimer.Enabled=1
           End If
         End If
       End If
      End If
	 End If

	 If keycode = RightTiltKey Then
      PlaySound "nudge_right"
      If BallInPlay=1 Then
       If Tilt<3 Then
		Nudge 270, 1.4
          Tilt2Timer.Enabled=1
          Renudge=90
         If Round>0 Then
           Tilt=Tilt+1
           If Tilt>2 Then
              TiltOn
           Else
             TiltTimer.Enabled=1
           End If
         End If
       End If
      End If
	 End If  
 
 	 If keycode = CenterTiltKey Then
      PlaySound "nudge_forward"
      If BallInPlay=1 Then
       If Tilt<3 Then
		Renudge=160+(Rnd*40)
          Tilt2Timer.Enabled=1
          Nudge -Renudge,1.4
         If Round>0 Then
           Tilt=Tilt+1
           If Tilt>2 Then
              TiltOn
           Else
             TiltTimer.Enabled=1
           End If
         End If
       End If
      End If
	 End If
End Sub

Sub Table1_KeyUp(ByVal keycode)

	If keycode = PlungerKey Then
		Plunger.Fire
         PlaySound "PlungeRel"
        If Round>0 And Tilt<3 Then
           If inpos=1 then PlaySound "Shoot"
        End If
	End If
    
	If keycode = LeftFlipperKey  AND MaxPlayers>0 Then
		Flipper1.RotateToStart:LFlip.objrotz = LFlip.objrotz +53
       If Tilt<3 And Round>0 Then  
		PlaySound "flipper2"
       End If
	End If
    
	If keycode = RightFlipperKey  AND MaxPlayers>0 Then
		 Flipper3.RotateToStart:RFlip.objrotz = RFlip.objrotz -53 
		If Tilt<3 And Round>0 Then
		 PlaySound "flipper2"
       End If
	End If

End Sub

'***************End Key Definitions

'**Drains and Kickers

Sub Drain_Hit()
     PlaySound "drain"
     RollTimer.Enabled=0
     BallInPlay=0
	Drain.DestroyBall
     GameSeq=3 ' start new ball
     SeqTimer.Enabled=1
     SeqTimer.Interval=125
	If B2SOn Then Controller.B2SSetTilt 0
End Sub

Sub Kicker2_Hit()
	Kicker2.DestroyBall
End Sub

 
 '----------------------------------------------------------
 '     TILT
 '-----------------------------------------------------------
 
 Sub TiltTimer_Timer()
    TiltTimer.Enabled=0
    If Tilt<3 Then
      Tilt=0
    End If
 End Sub
 
  Sub Tilt2Timer_Timer()
     Tilt2Timer.Enabled=0
     Nudge renudge,1
  End Sub
 
 Sub TiltOn()
    If B2SOn Then Controller.B2SSetTilt 1
    PlaySound "RESET3"
    Bumper1.Disabled=1
    Bumper2.Disabled=1
    Bumper3.Disabled=1
    Bumper4.Disabled=1
    R15.Disabled=1
    R20.Disabled=1
    R30.Disabled=1
    R27.Disabled=1
    R25.Disabled=1
    R34.Disabled=1
    BumperOff 1
    BumperOff 4
    BumperOff 2
    BumperOff 3
 End Sub
 
 '----------------------------------------
  'Add Players and start game
 '----------------------------------------
 Sub AddPlayer()
   Dim sr
  If Round=0 Or Round=5 Or Round=3 Then
    If Coins>0 Then
     Coins=Coins-1
     If B2SOn Then Controller.B2SSetCredits Coins
    ' SetCoinLights 
    ' SetCoinLights 

         Round=5  
         ActivePlayer=0
         GameSeq=5
        ' InitGame
         SeqTimer.Interval=100
         SeqTimer.Enabled=1
         be=5
     End If 
   End If    
 End Sub
  
 '------------------------------------------------------------------
 '    Game Sequence
 '------------------------------------------------------------------
 Sub SeqTimer_Timer()
    Select Case GameSeq
       Case 0 'Game Over
       Case 1 'New Ball
              SeqTimer.Enabled=0
              InitNewBall
              Set ball = Kicker1.CreateBall
              BallInPlay=1
              RollTimer.Enabled=1
              Kicker1.Kick 45,9
       Case 2 'Drain
              'Prepare for next ball
              'SetB2SData 32,0
              Tilt=0
              If Light_ShootAgain.State=0 Then
                 If ActivePlayer=MaxPlayers Then
                    Round=Round-1
                    If Round<1 Then
                       ActivePlayer=0
                       'SetB2SData 31, 0 'BIP
                    Else
                       ActivePlayer=1
                    End If
                 Else
                    ActivePlayer=ActivePlayer+1
                 End If
              End If
              If Round <1 Then
                 'End of game
                   'SetB2SData 30, 0
                   FreePlayNum =Int(rnd*9)
					If B2SOn Then Controller.B2SSetMatch FreePlayNum * 10
					If B2SOn Then Controller.B2SSetBallInPlay 0
					If B2SOn Then Controller.B2SSetGameOver 1
                   ' Check Match Wheel ------
                   For i=1 to MaxPlayers
                     If FreePlayNum=(Score MOD 10) Then
                        ReplayAction
                     End If
                  Next
                   '--------------------------
                  MaxPlayers=0
                  PlaySound "endgame"
                  SeqTimer.Enabled=0
                  'SetB2SData 34,1
				  LBonus=0
				  RBonus=0
                  For ii=1 to 4
                     BumperOff ii
                  Next
                  For Each I In LightControl 
                     I.State =0
                  Next
                  For Each I In GBonusLights 
                     I.State =0
                  Next
                  For Each I In YBonusLights 
                     I.State =0
                  Next
                  'LightL5.TimerEnabled=false
                  'SetB2SData 29, 0
                  If GetScore>HiScore Then
                     HiScore=GetScore
                     SaveValue CentralPark,"HiScore",Hiscore
                     DisplayHS
                  End If
              Else
                 'Start new ball
                 GameSeq=1
                 SeqTimer.Enabled=1
                 SeqTimer.Interval=400
                 If ActivePlayer=1 and Round=5 Then
                    SeqTimer.Interval=900
                 Else
                    PlaySound "newball"
                 End If
              End If
       Case 3 
             be=be+1
             KickerB1.CreateBall
             Select Case be
             Case 1
                KickerB5.Enabled=1
                KickerB1.Kick 90,6
             Case 2
                KickerB4.Enabled=1
                KickerB1.Kick 90,6
             Case 3
                KickerB3.Enabled=1
                KickerB1.Kick 90,6
             Case 4
                KickerB2.Enabled=1
                KickerB1.Kick 90,6
             Case 5
                BStackStop.IsDropped=True
             End Select
             GameSeq=2 ' start new ball
             SeqTimer.Enabled=1
             SeqTimer.Interval=10
       Case 5 
             be=be-1
             Select Case be
             Case 4
             PlaySound "cpstartup"
             KickerB5.Kick 90,6
             KickerB5.Enabled=0
             Case 3
             KickerB4.Kick 90,6
             KickerB4.Enabled=0
             Case 2
             KickerB3.Kick 90,9
             KickerB3.Enabled=0
             Case 1
             KickerB2.Kick 90,9
             KickerB2.Enabled=0
             Case 0
             KickerB1.Kick 90,10
             InitGame
             SeqTimer.Interval=500
             GameSeq=2
             End Select
       Case 6
       End Select
 End Sub
 
 '--------------------------------------------------------------------
 '------- TARGETS 
 '--------------------------------------------------------------------
 '--------------------------------------------------------------------
Sub T11_Hit()
   If Tilt>2 Then Exit Sub
   PlaySound "TargetSound"
   AddScore 5,1,ActivePlayer '10pts
   If LightP1.State=1 Then
      LightP1.State=0
	  GI_Bulb009.State=0
      Y15=Y15+1
   End If
   LightLS.TimerEnabled=true
   CheckY15
End Sub

Sub T21_Hit()
   If Tilt>2 Then Exit Sub
   PlaySound "TargetSound"
   AddScore 5,1,ActivePlayer '10pts
   If LightP2.State=1 Then
      LightP2.State=0
	  GI_Bulb010.State=0
      Y15=Y15+1
   End If
   LightLS.TimerEnabled=true
   CheckY15
End Sub

Sub T31_Hit()
   If Tilt>2 Then Exit Sub
   PlaySound "TargetSound"
   AddScore 5,1,ActivePlayer '10pts
   If LightP3.State=1 Then
      LightP3.State=0
	  GI_Bulb011.State=0
      Y15=Y15+1
   End If
   LightLS.TimerEnabled=true
   CheckY15
End Sub

Sub T41_Hit()
   If Tilt>2 Then Exit Sub
   PlaySound "TargetSound"
   AddScore 5,1,ActivePlayer '10pts
   If LightP4.State=1 Then
      LightP4.State=0
	  GI_Bulb012.State=0
      Y15=Y15+1
   End If
   LightLS.TimerEnabled=true
   CheckY15
End Sub

Sub T51_Hit()
   If Tilt>2 Then Exit Sub
   PlaySound "TargetSound"
   AddScore 5,1,ActivePlayer '10pts
   If LightP5.state=1 then
		LightP5.State=0
		GI_Bulb013.State=0
		Y15=Y15+1
	end if
   LightLS.TimerEnabled=true
   CheckY15
End Sub

Sub T61_Hit()
   If Tilt>2 Then Exit Sub
   PlaySound "TargetSound"
   AddScore 5,1,ActivePlayer '10pts
	If LightP6.state=1 then
		LightP6.State=0
		GI_Bulb017.State=0
		G610=G610+1
	end if
   LightLS.TimerEnabled=true
   CheckG610
End Sub

Sub T71_Hit()
   If Tilt>2 Then Exit Sub
   PlaySound "TargetSound"
   AddScore 5,1,ActivePlayer '10pts
	if LightP7.state=1 then
		LightP7.State=0
		GI_Bulb016.State=0
		G610=G610+1
	end if
   LightLS.TimerEnabled=true
   CheckG610
End Sub

Sub T81_Hit()
   If Tilt>2 Then Exit Sub
   PlaySound "TargetSound"
   AddScore 5,1,ActivePlayer '10pts
	if LightP8.state=1 then
		LightP8.State=0
		GI_Bulb015.State=0
		G610=G610+1
	end if
   LightLS.TimerEnabled=true
   CheckG610
End Sub

Sub T91_Hit()
   If Tilt>2 Then Exit Sub
   PlaySound "TargetSound"
   AddScore 5,1,ActivePlayer '10pts
	if LightP9.state=1 then
		LightP9.State=0
		GI_Bulb018.State=0
		G610=G610+1
	end if
   LightLS.TimerEnabled=true
   CheckG610
End Sub

Sub T101_Hit()
   If Tilt>2 Then Exit Sub
   PlaySound "TargetSound"
   AddScore 5,1,ActivePlayer '10pts
	if LightP10.state=1 then
		LightP10.State=0
		GI_Bulb014.State=0
		G610=G610+1
	end if
   LightLS.TimerEnabled=true
   CheckG610
End Sub


Sub T111_Hit()
   If Tilt>2 Then Exit Sub
	PlaySound "TargetSound"

	If LBonus<6 And LBonus>0 Then
		AddScore 5,LBonus,ActivePlayer '10-50pts
	Elseif LBonus>5 then
		ReplayAction
	End If
End Sub

Sub T121_Hit()
   If Tilt>2 Then Exit Sub
	PlaySound "TargetSound"

	If RBonus<6 And RBonus>0 Then
		AddScore 5,RBonus,ActivePlayer '10-50pts
	Elseif RBonus>5 then 
		ReplayAction
	End If
End Sub

Sub CheckY15()
  If Y15=5 Then'Check if number of 1-5 targets hit =5
     LightP1.TimerEnabled=true  'Reset all 1-5 target lights to on and advance bonus
  End If
End Sub

Sub CheckG610()'Check if number of 6-10 targets hit =5
  If G610=5 Then
     LightP6.TimerEnabled=true 'Reset all 6-10 target lights to on and advance bonus
  End If
End Sub

Sub LightP1_Timer()
     LightP1.TimerEnabled=false
     Y15=0
	 For Each I In LeftBankLights 
		I.State =1
     Next
	 For Each I In Lights1to5 
		I.State =1
     Next
     PlaySound "TargetBankReset"
     If LBonus<5 Then
        LBonus=Lbonus+1
        SetBonusLights
	 Else
		LBonus=0
		SetBonusLights
     End If
End Sub

Sub LightP6_Timer()
     LightP6.TimerEnabled=false
     G610=0
	 For Each I In RightBankLights 
		I.State =1
     Next
	 For Each I In Lights6to10 
		I.State =1
     Next
     PlaySound "TargetBankReset"
     If RBonus<5 Then
        RBonus=Rbonus+1
        SetBonusLights
	 Else
		RBonus=0
		SetBonusLights
     End If
End Sub

 '----------------------------------------------------------------------
 '     BUMPER
 '----------------------------------------------------------------------
 Sub Bumper1_Hit()
    PlaySound "bumper1"
     If LightB1.State=0 Then
        AddScore 6,1,ActivePlayer '1pts
     Else
        AddScore 5,1,ActivePlayer '10pts
        LightLS.TimerEnabled=true
     End If
 End Sub
 
 Sub Bumper2_Hit()
    PlaySound "bumper2"
     If LightB2.State=0 Then
        AddScore 6,1,ActivePlayer '1pts
     Else
        AddScore 5,1,ActivePlayer '10pts
        LightLS.TimerEnabled=true
     End If
 End Sub

  Sub Bumper3_Hit()
    PlaySound "bumper2"
     If LightB3.State=0 Then
        AddScore 6,1,ActivePlayer '1pts
     Else
        AddScore 5,1,ActivePlayer '10pts
        LightLS.TimerEnabled=true
     End If
 End Sub

 Sub Bumper4_Hit()
    PlaySound "bumper1"
     If LightB4.State=0 Then
        AddScore 6,1,ActivePlayer '1pts
     Else
        AddScore 5,1,ActivePlayer '10pts
        LightLS.TimerEnabled=true
     End If
 End Sub
 
Sub BumperOn(Byval b)
   Select Case b
   Case 1:LightB1.State=1:LightB1a.State=1:LightB1b.State=1
   Case 2:LightB2.State=1:LightB2a.State=1:LightB2b.State=1
   Case 3:LightB3.State=1:LightB3a.State=1:LightB3b.State=1
   Case 4:LightB4.State=1:LightB4a.State=1:LightB4b.State=1
   Case 5:LightB5.State=1
   End Select
End Sub

Sub BumperOff(Byval b)
   Select Case b
   Case 1:LightB1.State=0:LightB1a.State=0:LightB1b.State=0
   Case 2:LightB2.State=0:LightB2a.State=0:LightB2b.State=0
   Case 3:LightB3.State=0:LightB3a.State=0:LightB3b.State=0
   Case 4:LightB4.State=0:LightB4a.State=0:LightB4b.State=0
   Case 5:LightB5.State=0
   End Select
End Sub

'------------------------------------------------------------------------
'           TRIGGERS
'------------------------------------------------------------------------
Sub Trigger1_Hit()
   If Tilt>2 Then Exit Sub
   AddScore 5,3,ActivePlayer '30pts
   PlaySound "TargetSound"
   If lightP1.State=1 Then
      LightP1.State=0
	  GI_Bulb009.State=0
      Y15=Y15+1
   End If
   If lightP3.State=1 Then
      LightP3.State=0
	  GI_Bulb011.State=0
      Y15=Y15+1
   End If
   If lightP5.State=1 Then
      LightP5.State=0
	  GI_Bulb013.State=0
      Y15=Y15+1
   End If
   CheckY15
End Sub

Sub Trigger2_Hit()
   If Tilt>2 Then Exit Sub
   AddScore 5,3,ActivePlayer '30pts
   PlaySound "TargetSound"
   If lightP6.State=1 Then
      LightP6.State=0
	  GI_Bulb017.State=0
      G610=G610+1
   End If
   If lightP8.State=1 Then
      LightP8.State=0
	  GI_Bulb015.State=0
      G610=G610+1
   End If
   If lightP10.State=1 Then
      LightP10.State=0
	  GI_Bulb014.State=0
      G610=G610+1
   End If
   CheckG610
End Sub

Sub Trigger3_Hit()
   If Tilt>2 Then Exit Sub
   AddScore 5,1,ActivePlayer '10pts
   PlaySound "TargetSound"
   If lightP2.State=1 Then
      LightP2.State=0
	  GI_Bulb010.State=0
      Y15=Y15+1
   End If
   If lightP4.State=1 Then
      LightP4.State=0
	  GI_Bulb012.State=0
      Y15=Y15+1
   End If
   CheckY15
End Sub

Sub Trigger4_Hit()
   If Tilt>2 Then Exit Sub
   AddScore 5,1,ActivePlayer '10pts
   PlaySound "TargetSound"
   If lightP7.State=1 Then
      LightP7.State=0
	  GI_Bulb016.State=0
      G610=G610+1
   End If
   If lightP9.State=1 Then
      LightP9.State=0
	  GI_Bulb018.State=0
      G610=G610+1
   End If
   CheckG610
End Sub

Sub Trigger5_Hit()
   If Tilt>2 Then Exit Sub
   AddScore 5,1,ActivePlayer '10pts
   PlaySound "TargetSound"
   If lightP2.State=1 Then
      LightP2.State=0
	  GI_Bulb010.State=0
      Y15=Y15+1
   End If
   If lightP4.State=1 Then
      LightP4.State=0
	  GI_Bulb012.State=0
      Y15=Y15+1
   End If
   CheckY15
End Sub

Sub Trigger6_Hit()
   If Tilt>2 Then Exit Sub
   AddScore 5,1,ActivePlayer '10pts
   PlaySound "TargetSound"
   If lightP7.State=1 Then
      LightP7.State=0
	  GI_Bulb016.State=0
      G610=G610+1
   End If
   If lightP9.State=1 Then
      LightP9.State=0
	  GI_Bulb018.State=0
      G610=G610+1
   End If
   CheckG610
End Sub

Sub Trigger7_Hit()
   If Tilt>2 Then Exit Sub
   AddScore 5,3,ActivePlayer '30pts
   PlaySound "TargetSound"
   If lightP1.State=1 Then
      LightP1.State=0
	  GI_Bulb009.State=0
      Y15=Y15+1
   End If
   If lightP3.State=1 Then
      LightP3.State=0
	  GI_Bulb011.State=0
      Y15=Y15+1
   End If
   If lightP5.State=1 Then
      LightP5.State=0
	  GI_Bulb013.State=0
      Y15=Y15+1
   End If
   CheckY15
End Sub

Sub Trigger8_Hit()
   If Tilt>2 Then Exit Sub
   AddScore 5,3,ActivePlayer '30pts
   PlaySound "TargetSound"
   If lightP6.State=1 Then
      LightP6.State=0
	  GI_Bulb017.State=0
      G610=G610+1
   End If
   If lightP8.State=1 Then
      LightP8.State=0
	  GI_Bulb015.State=0
      G610=G610+1
   End If
   If lightP10.State=1 Then
      LightP10.State=0
	  GI_Bulb014.State=0
      G610=G610+1
   End If
   CheckG610
End Sub

Sub Trigger9_Hit()
   If Tilt>2 Then Exit Sub
   AddScore 5,1,ActivePlayer '10pts
   PlaySound "TargetSound"
   If lightP2.State=1 Then
      LightP2.State=0
	  GI_Bulb010.State=0
      Y15=Y15+1
   End If
   If lightP4.State=1 Then
      LightP4.State=0
	  GI_Bulb012.State=0
      Y15=Y15+1
   End If
   CheckY15
End Sub

Sub Trigger10_Hit()
   If Tilt>2 Then Exit Sub
   AddScore 5,1,ActivePlayer '10pts
   PlaySound "TargetSound"
   If lightP7.State=1 Then
      LightP7.State=0
	  GI_Bulb016.State=0
      G610=G610+1
   End If
   If lightP9.State=1 Then
      LightP9.State=0
	  GI_Bulb018.State=0
      G610=G610+1
   End If
   CheckG610
End Sub

Sub Trigger11_Hit()
  inpos=1
End Sub
Sub Trigger11_UnHit()
  inpos=0
End Sub

'PlungerSoundTrigger
Sub Trigger11_Hit()
  inpos=1
End Sub

Sub Trigger11_UnHit()
  inpos=0
End Sub

'-----------------------------------------------------------
'     SLINGS and WALLS
'-----------------------------------------------------------
  Sub R15_Slingshot()
    PlaySound "slingshot"
    If LightLS.State=0 Then
       AddScore 6,1,ActivePlayer '1p
    Else
       AddScore 5,1,ActivePlayer '10p
       LightLS.TimerEnabled=true
    End If
 End Sub

  Sub R20_Slingshot()
    PlaySound "slingshot"
    If LightRS.State=0 Then
       AddScore 6,1,ActivePlayer '1p
    Else
       AddScore 5,1,ActivePlayer '10p
       LightLS.TimerEnabled=true
    End If
 End Sub

 Sub R30_Slingshot()
    PlaySound "rubberS"
    AddScore 6,1,ActivePlayer '1pts
 End Sub

 Sub R34_Slingshot()
    PlaySound "rubberS"
    AddScore 6,1,ActivePlayer '1pts
 End Sub

 Sub R25_Slingshot()
    PlaySound "rubberS"
    AddScore 6,1,ActivePlayer '1pts
 End Sub

 Sub R27_Slingshot()
    PlaySound "rubberS"
    AddScore 6,1,ActivePlayer '1pts
 End Sub

'---------------------------------------------Random Wall and Rubber Strikes

Sub R71_Hit()
	PlaySound "rubberQ"
End Sub

Sub Rubber027_Hit()
	PlaySound "rubberQ"
End Sub 

Sub Rubber028_Hit()
	PlaySound "rubberQ"
End Sub 

Sub Rubber005_Hit()
	PlaySound "rubberQ"
End Sub  

Sub Rubber006_Hit()
	PlaySound "rubberQ"
End Sub 

Sub Rubber007_Hit()
	PlaySound "rubberQ"
End Sub 

Sub Rubber008_Hit()
	PlaySound "rubberQ"
End Sub

Sub Rubber009_Hit()
	PlaySound "rubberQ"
End Sub 

Sub Rubber010_Hit()
	PlaySound "rubberQ"
End Sub 

Sub Rubber015_Hit()
	PlaySound "rubberQ"
End Sub 

Sub Rubber017_Hit()
	PlaySound "rubberQ"
End Sub 

Sub Rubber024_Hit()
	PlaySound "rubberS"
End Sub 

Sub Rubber025_Hit()
	PlaySound "rubberS"
End Sub 

Sub Rubber026_Hit()
	PlaySound "rubberQ"
End Sub

Sub Rubber029_Hit()
	PlaySound "rubberS"
End Sub  

Sub Rubber030_Hit()
	PlaySound "rubberQ"
End Sub 

Sub RubberRB0_Hit()
	PlaySound "rubberS"
End Sub 

'---------------

Sub RubberRB001_Hit()
	PlaySound "rubberQ"
End Sub  

Sub RubberRB002_Hit()
	PlaySound "rubberQ"
End Sub 

Sub RubberRB003_Hit()
	PlaySound "rubberQ"
End Sub 

Sub RubberRB004_Hit()
	PlaySound "rubberQ"
End Sub 

Sub RubberRB005_Hit()
	PlaySound "rubberQ"
End Sub 

Sub RubberRB006_Hit()
	PlaySound "rubberQ"
End Sub 

Sub RubberRB007_Hit()
	PlaySound "rubberS"
End Sub 

Sub RubberRB009_Hit()
	PlaySound "rubberS"
End Sub 

Sub RubberRB012_Hit()
	PlaySound "rubberS"
End Sub 



'---------------Misc Sounds

Sub Bump1()
	PlaySound "rubberS"
End Sub   

Sub RubberBand11_Hit()
	PlaySound "rubberS"
End Sub   

Sub Gate1_Hit()
	PlaySound "gate_tgx" 
End Sub   

Sub Gate2_Hit()
	PlaySound "gate" 
End Sub 

Sub RFlip001_Hit()
	PlaySound "rubberS" 
End Sub  

Sub RFlip002_Hit()
	PlaySound "rubberS" 
End Sub  

Sub Wall019_Hit()
	PlaySound "rubberS"
End Sub
 
'-------------------------------------------------------------------
'      Alternators
'-------------------------------------------------------------------

Sub LightLS_Timer()
   LightLS.TimerEnabled=False
   alt=alt+1
   If alt>1 then alt=0
   If alt=0 then
      BumperOn 1
      BumperOn 4
      BumperOff 2
      BumperOff 3
      LightLS.State=1
      LightRS.State=0
   Else
      BumperOn 2
      BumperOn 3
      BumperOff 1
      BumperOff 4
      LightLS.State=0
      LightRS.State=1
   End if
End Sub

 '------------------------------------------------------------------------
 '             Monkey
 '------------------------------------------------------------------------

Sub LightL5_Timer()
	If B2SOn then
		Controller.B2SSetData monkeypos,0
	end if
	Select Case monkey
	
	Case 0,1
	  monkeypos=monkeypos-1
      If B2SOn then
		 Controller.B2SSetData monkeypos,1
         
      End If
	case 2,11

	Case 3,4,5,6,7,8,9
      monkeypos=monkeypos+1
      If B2SOn then
		 Controller.B2SSetData monkeypos,1
         
      End If

	case 10
      'SetB2SData 36,monkey
      playSound "CFchime1"

	Case 12,13,14,15,16
      monkeypos=monkeypos-1
      If B2SOn then 
		 Controller.B2SSetData monkeypos,1
		
	  end if

	  
	End Select
	If monkey<16 then
		monkey=monkey+1
	else
		monkey=0
		LightL5.timerenabled=false
	end if
End Sub
 

 Sub RollTimer_Timer()
    ' ****************************************************
    ' * Thanks to RASCAL for the rolling ball sound code *
    ' ****************************************************
   If (Abs(int(ball.velx)) or Abs(int(ball.vely))) > 5 and roll = 0 then
 	  roll = 1
 	  StopSound "rollstop"
 	  PlaySound "roll1"
   ElseIf (Abs(int(ball.velx)) or Abs(int(ball.vely))) <= 6 and roll = 1 then
 	  roll = 0
 	  StopSound "roll1"
 	  PlaySound "rollstop"
   End If
 End Sub
 
'---------------------------------------------------------------------
'       Handle Bonus
'---------------------------------------------------------------------

 Sub AddLeftBonus( ByVal bns)
    LBonusQueue=LBonusQueue+bns
 End Sub

Sub SetBonusLights()
	If Lbonus=6 Then 
				YBonusLights(LBonus-2).State=0
				LightYS.State=1
	End If
	If Lbonus=1 Then
		LightY10.State=1
	End If
	If LBonus >1 And LBonus < 6 Then
		YBonusLights(LBonus-2).State=0
		YBonusLights(LBonus-1).State=1
	End If
	If Rbonus=6 Then 
				GBonusLights(RBonus-2).State=0
				LightGS.State=1
	End If
	If Rbonus=1 Then
		LightG10.State=1
	End If
	If RBonus >1 And RBonus < 6 Then
		GBonusLights(RBonus-2).State=0
		GBonusLights(RBonus-1).State=1
	End If
End Sub

 Sub ReplayAction()
    If Coins<9 Then Coins=Coins+1
   If B2SOn Then Controller.B2SSetCredits Coins
    PlaySound"knocker"
 End Sub

Sub AddScore (ByVal as1,ByVal as2, ByVal as3)
   'If BC=0 Then AltLights
	OldScore=Score

		If as1 = 6 Then Score = Score + as2
		If as1 = 5 Then Score = Score + as2 * 10
	tempcounter = as2
	LightL10.timerinterval=150
	LightL10.timerenabled=true
	NewScore=Score
	If B2SOn Then			' SCORE SYSTEM by STAT
		Controller.B2SSetScorePlayer 1, Score
		if Score > 999 Then Controller.B2SSetData 98,1
	End if
	OldScore=int(OldScore/100)
	NewScore=int(NewScore/100)
	If OldScore<>NewScore then LightL5.timerenabled=true
End Sub

Sub LightL10_timer
	playsound"bell"
	tempcounter=tempcounter-1
	if tempcounter<1 then LightL10.timerenabled=false
end sub

Sub DisplayHS()
   b2sSplitScore 4,HiScore
End Sub

Function GetScore()
   Dim temphi

End Function

Sub b2sSplitScore (Byval b2splayer, Byval b2sscorevalue)
  Dim B2Ssplit

End Sub
