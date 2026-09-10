' Humpty Dumpty (Gottlieb 1947)
' editred to remove the highscore posting. 

'DOF mapping




'E101 Left flipper
'E102 Right flipper
'E103 Slingshot left
'E104 Slingshot right
'E105 Bumper back left
'E106 Bumper back center
'E107 Bumper back right
'E108 Bumper Left
'E109 Bumper center
'E110 Bumper right
'E111 Knocker
'E112 Shaker
'E113 Gear motor
'E114 Red flashers
'E115 Green flashers
'E116 Blue flashers
'E117 Beacons
'E118 Fan
'E119 Strobes
'E120 Red undercab
'E122 Green undercab
'E123 Blue undercab
'E124 Bell
'E125 Hell ball motor
'E126 Hell ball Green 
'E127 Hell ball Red 
'E128 Hell ball Blue






'Dim Controller,Tilt,Ball,Balls,Credits,ShootBall,Obj,Bonus,XAdv,Bonuscounter,MultiplierCounter,Tens,Ones,Score,Scoretoadd,TimesDroppedA,TimesDroppedB
'*DOF method for non rom controller tables by Arngrim****************
'*******Use DOF 1**, 1 to activate a ledwiz output*******************
'*******Use DOF 1**, 0 to deactivate a ledwiz output*****************
'*******Use DOF 1**, 2 to pulse a ledwiz output**********************
Sub DOF(dofevent, dofstate)
  If B2SOn=True Then
If dofstate = 2 Then  
Controller.B2SSetData dofevent, 1:Controller.B2SSetData dofevent, 0 
Else  Controller.B2SSetData dofevent, dofstate 
End If
End If
End Sub
'********************************************************************
'Sub Table1_Init()
	B2SOn=True

if B2SOn then
		Set Controller = CreateObject("B2S.Server")
		Controller.B2SName = "humpty_dumpty"
		Controller.Run()
		If Err Then MsgBox "Can't Load B2S.Server."
	end if
	 Ball=0
     Balls=5
	 Controller.B2SSetBallInPlay ball 
     Controller.B2SSetGameOver 0
Controller.B2SSetmatch 0
     Controller.B2SSetShootAgain 0
 If  Credits > 0 Then
     Controller.B2SSetPlayerup 1
     Controller.B2SSetCanPlay 0
 End If
 If  Credits = 0 Then
     Controller.B2SSetCanPlay 1
 End If


LoadVPM "01560000", "Bally.VBS", 3.26
Sub LoadVPM(VPMver, VBSfile, VBSver)
End Sub
 
Const BallSize = 50

Dim VarHidden, UseVPMDMD
If Table1.ShowDT = true then
	'UseVPMDMD = True
	'VarHidden = 0
	'lockdown.visible = true
	Hunklight2.visible = true
	Hunklight1.visible = true
	Hunklight3.visible = true
	Hunklight4.visible = true
	Hunklight5.visible = true
	Hunklight6.visible = true
	Hunklight7.visible = true
	A10KL.visible = true
	A20KL.visible = true
	A30KL.visible = true
	A40KL.visible = true
	A50KL.visible = true
	A60KL.visible = true
	A70KL.visible = true
	A80KL.visible = true
	A90KL.visible = true
	CredBox.visible = true
	Light2.visible = true

	'leftrail.visible = true
	'rightrail.visible = true
else
	'UseVPMDMD = False
	'VarHidden = 1
	'lockdown.visible = false
	'leftrail.visible = false
	'rightrail.visible = false
	Hunklight1.visible = false
	Hunklight1.visible = false
	Hunklight2.visible = false
	Hunklight3.visible = false
	Hunklight4.visible = false
	Hunklight5.visible = false
	Hunklight6.visible = false
	Hunklight7.visible = false
	A10KL.visible = false
	A20KL.visible = false
	A30KL.visible = false
	A40KL.visible = false
	A50KL.visible = false
	A60KL.visible = false
	A70KL.visible = false
	CredBox.visible = false
	Light2.visible = false
	A80KL.visible = false
	A90KL.visible = false
	Light10k.visible = false

end if
Dim Score
Dim Ball
Dim Ballsout
Dim Credit
Dim GameOn
Dim Spec1Award
Dim Spec2Award
Dim Spec3Award
Dim Spec4Award
Dim HunKs
Dim TenKs
Dim KBox(9)
Dim HunKLight(7)
Dim BL(10)
Dim Tilted
Dim Tilts
Dim Bonus
Dim Scoring
Dim Hiscore
Dim F
Dim Q
Dim ln
Dim Moo
Dim Hold
Dim BH(5)

dim Ali
dim Baba
dim sinbad
dim patho
dim pathl(16)
dim randy
dim bobo
dim sb
dim bv
dim Amos
dim Otis
dim candy
dim seqo
dim seqi

dim bump1
dim bump2
dim bump3
dim bump4
dim bumpbb1
dim bumpbb2
dim bumpbb3
dim bumpbb4
dim bumptop
dim bumpcenter


Set KBox(1) = A10kL
Set KBox(2) = A20kL
Set KBox(3) = A30kL
Set KBox(4) = A40kL
Set KBox(5) = A50kL
Set KBox(6) = A60kL
Set KBox(7) = A70kL
Set KBox(8) = A80kL
Set KBox(9) = A90kL

Set HunkLight(1) = HunkLight1
Set HunkLight(2) = HunkLight2
Set HunkLight(3) = HunkLight3
Set HunkLight(4) = HunkLight4
Set HunkLight(5) = HunkLight5
Set HunkLight(6) = HunkLight6
Set HunkLight(7) = HunkLight7

Set BL(1) = Light10k
Set BL(2) = Light20k
Set BL(3) = Light40k
Set BL(4) = Light60k
Set BL(5) = Light80k
Set BL(6) = Light100k

Init

Sub Init()
   Score = 0
   Credit = 0
   Randomize
   'LoadHS
LoadTrough.Enabled=1
   
end sub
'Standard Sounds
   Const SSolenoidOn = "Solenoid"
   Const SSolenoidOff = ""



Sub LoadTrough_Timer()
DOF 103,2
 	trough.CreateBall:trough.Kick 0, 1
trough1.CreateBall:trough1.Kick 0, 1
trough2.CreateBall:trough2.Kick 0, 1
trough3.CreateBall:trough3.Kick 0, 1
trough4.CreateBall:trough4.Kick 0, 1
TroughCount=5
LoadTrough.Enabled=0
 	End Sub

' Key Map 
Sub Table1_KeyDown(ByVal keycode)
'If PostItHighScoreCheck(keycode) then Exit Sub
	if keycode = 6 then
        if Credit < 26 then
     		Credit = Credit + 1
     		Playsound "coin"
        end if
		CredBox.Text = Credit
	end if
	if keycode = 2 then
        if GameOn = 0 and Credit > 0 then
           StartGame
drop.Rotz = -70
        end if    
	end if
	If keycode = PlungerKey Then
		Plunger.PullBack
PlaySound "plungerpull",0,1,0.25,0.25
	End If
    if (keycode =3 or keycode =RightMagnaSave) and gameon = TRUE and BallsOut<= 5 and hold=0 then
	drop.Rotz = 0
DOF 104,2
       BallsOut=BallsOut+1
       PlaySound "balloutr"
       Plungekick.createball.image="JPBall-Dark2"
       Plungekick.kick 270,20
       hold=1

    end if
	If keycode = LeftFlipperKey and tilted = FALSE then
DOF 101,1
		Flipper1.RotateToEnd
		Flipper2.RotateToEnd
		Flipper3.RotateToEnd
		PlaySound "FlipperUp"
	End If
	If keycode = RightFlipperKey and tilted = FALSE then
DOF 102,1
		Flipper4.RotateToEnd
		Flipper5.RotateToEnd
		Flipper6.RotateToEnd
		PlaySound "FlipperUp"
	End If
	If keycode = LeftTiltKey Then
		Nudge 90, 2
		playsound "nudge"
		TiltCheck
	End If
	If keycode = RightTiltKey Then
		Nudge 270, 2
		playsound "nudge"
		TiltCheck
	End If
	If keycode = CenterTiltKey Then
		Nudge 0, 2
		playsound "nudge"
		TiltCheck
	End If
End Sub

Sub Table1_KeyUp(ByVal keycode)
	If keycode = PlungerKey Then
		Plunger.Fire
PlaySound "plunger",0,1,0.25,0.25
	End If
	If keycode = LeftFlipperKey Then
DOF 101,0
		Flipper1.RotateToStart
		Flipper2.RotateToStart
		Flipper3.RotateToStart
		PlaySound "FlipperDown"

	End If
	If keycode = RightFlipperKey Then
DOF 102,0
		Flipper4.RotateToStart
		Flipper5.RotateToStart
		Flipper6.RotateToStart
		PlaySound "FlipperDown"
	End If
End Sub

' Tilt Routine
Sub TiltCheck
   If Tilted = FALSE then
      If Tilts = 0 then
         Tilts = Tilts + int(rnd*100)
         TiltTimer.Enabled = TRUE
      Else   
         Tilts = Tilts + int(rnd*120)
      End If   
      If Tilts >= 350 and Tilted = FALSE then
         GameOn = FALSE
 		 Flipper1.RotateToStart
 		 Flipper2.RotateToStart
		 Flipper3.RotateToStart
		 Flipper4.RotateToStart
		 Flipper5.RotateToStart
		 Flipper6.RotateToStart
         PlaySound "click"
         Lightb1.State = 0: Primitive110.image = "basedark"
		 gi1.State = 0
         Lightb2.State=  0: Primitive109.image = "basedark"
		 gi2.State = 0
         Lightb3.State = 0: Primitive103.image = "basedark"
		 gi3.State = 0
         Lightb4.State = 0: Primitive105.image = "basedark"
		 gi4.State = 0
         Lightbtopleft.State = 0: Primitive107.image = "basedark"
		 giLightbtopleft.state = 0
         Lightbtopright.State = 0: Primitive108.image = "basedark"
		 giLightbtopright.state = 0
         Lightbbottomleft.State = 0: Primitive104.image = "basedark"
		 giLightbbottomleft.state = 0
         Lightbbottomright.State = 0: Primitive106.image = "basedark"
		 giLightbbottomright.state = 0
		 Light1.State = 0
         LightBottom.State = 0
         RSL1.State = 0: Primitive57.image = "redshootcovertexture"	
		 giLighttriangle1.state = 0
DOF 144,0
         RSL2.State = 0: Primitive74.image = "redshootcovertexture"
		 giLighttriangle2.state = 0
DOF 125,0
    	 For X = 1 to 6
       	     BL(x).State = 0
         Next
    	 Tilted = TRUE 
         TiltBox.Text = "TILT"

      End If   
   End If
End Sub

Sub StartGame()
    Tilted = FALSE
    moo=0
    hold=0
    seqi=0
    Tilts = 0
    PlaySound "reset"
    GameOn = TRUE
DrainWall.IsDropped = TRUE
    Scoring = FALSE
    BallsOut = 0
    If Credit > 0 then
       Credit = Credit - 1
    End if
    CredBox.Text = Credit
    Score = 0
    Hunks = 0
    Tenks = 0
        Spec1Award = FALSE
    Spec2Award = FALSE
    Spec3Award = FALSE
    Spec4Award = FALSE
    lightb1.State = 0: Primitive110.image = "basedark"
	gi1.State = 0
    lightb2.State = 0: Primitive109.image = "basedark"
	gi2.State = 0
    lightb3.State = 0: Primitive103.image = "basedark"
	gi3.State = 0
    lightb4.State = 0: Primitive105.image = "basedark"
	gi4.State = 0
    CRL.state= 1
    crownlight.state= 0
   lefto.state= 0
   righto.state= 0
       Ali=0
    Baba=0
    Amos=0
    Otis=0
    Randy = 1
    bv = 2
    Lightbtopleft.State = 1: Primitive107.image = "baseon"
	giLightbtopleft.State = 1
    Lightbtopright.State = 1: Primitive108.image = "baseon"
	giLightbtopright.state = 1
    Lightbbottomleft.State = 1: Primitive104.image = "baseon"
	giLightbbottomleft.state = 1
    Lightbbottomright.State = 1: Primitive106.image = "baseon"
	giLightbbottomright.state = 1
	'giLightbtop.state = 1: Primitive111.image = "baseon"
	'Lightbtop.state = 1
    Light1.State = 0
    LightBottom.State = 0
    RSL1.State = 0: Primitive57.image = "redshootcovertexture"	
	giLighttriangle1.state = 0
DOF 144,0
    RSL2.State = 0: Primitive74.image = "redshootcovertexture"
	giLighttriangle2.state = 0
DOF 125,0
    patho = 1  
    For X = 1 to 6
       BL(x).State = 0
    Next
        Ball = 1
    Bonus = 1
    BL(1).State = 1   
    For x = 1 to 9
       If x <= 7 then 
			Hunklight(x).State = 0
			DOF 200+x, 0
	   End If
       KBox(x).State = 0
	   DOF 10*x, 0
    Next
End Sub
  
'BallTracking       
Dim BallsLOadedForPlay
	BallsLoadedForPlay = 0
'BallTracking
sub drainer_hit()
    drainer.destroyBall
'BallTracking
	BallsLoadedForPlay = BallsLoadedForPlay + 1
	If BallsLoadedForPlay = 5 then
		BallsLoadedForPlay = 0
		DrainWall.IsDropped = 0
	End If
'Ball Tracking
    hold=0
    'BH(Ball).CreateBall
'    ball = Ball + 1
'    if ball = 5 then GameOver
end sub

Sub plungekick_hit()
    Plungekick.kick 270, 5
End Sub

sub GameOver()
msgbox "gameover"
    Ali=0
    Baba=0
    Amos=0
    Otis=0
	MatchTimer.Enabled=1:PlaySound "MotorLeer"
    CRL.state= 0
   crownlight.state= 0
   lefto.state= 0
   righto.state= 0
    If Tilted = FALSE then
If Score = 0 then DrainWall.IsDropped = FALSE
       playsound "motorleer"
       If Score > hiscore then
       hiscore = score
       savehs
       hsbox.text = FormatNumber(hiscore, 0, -1, 0, -1)
       end if
    End If
    GameOn = FALSE
 ' CheckNewHighScorePostIt1Player hiscore '1 player table
    'CheckNewHighScorePostIt2Player xxx1, xxx2 '2 player table
    'CheckNewHighScorePostIt3Player xxx1, xxx2, xxx3 '3 player table
    'CheckNewHighScorePostIt4Player xxx1, xxx2, xxx3, xxx4 '4 player table
 
End Sub

' Rotation Sequence Elements


sub tbump1_hit()

	bump1 = 1
	Me.TimerEnabled = 1
	PlaySound "fx_bumper" 

	If lightb1.state = 0 then
lightb1.state = 1: addscore (10000)
DOF 148,1
 Primitive110.image = "baseon"
gi1.State = 1
	Ali=Ali+1
	end if
advance()	
end sub

sub TBump1_Timer()

           Select Case bump1
               Case 1:skirt1.z = 5:bump1 = 2
               Case 2:skirt1.z = 2:bump1 = 3
               Case 3:skirt1.z = -2:bump1 = 4
               Case 4:skirt1.z = -2:bump1 = 5
               Case 5:skirt1.z = 2:bump1 = 6
               Case 6:skirt1.z = 5:bump1 = 7
               case 7:Me.TimerEnabled = 0
           End Select
DOF 148,0
end sub

sub r2_hit(): PlaySound "fx_bumper"
if rsl2.state= 1 then
Primitive74.image = "redshootcovertexture"
DOF 125,0
giLighttriangle2.state = 0
AwardSpecial
PlaySound "fx_bumper"
end if
if lefto.state= 0 then
addscore (10000)

end if
if lefto.state= 1 then
      if f = 0 then
         f = 5
         Add50KTimer.Enabled = TRUE
      end if 
end if
end sub

sub tbump2_hit()

	bump2 = 1
	Me.TimerEnabled = 1
	PlaySound "fx_bumper" 

If ali=1 and lightb2.state= 0 then
	lightb2.state = 1 
DOF 150,1
addscore (10000)
advance()
Primitive109.image = "baseon"
	gi2.State = 1
Ali=Ali+1	
	end if
end sub

sub TBump2_Timer()

           Select Case bump2
               Case 1:skirt2.z = 5:bump2 = 2
               Case 2:skirt2.z = 2:bump2 = 3
               Case 3:skirt2.z = -2:bump2 = 4
               Case 4:skirt2.z = -2:bump2 = 5
               Case 5:skirt2.z = 2:bump2 = 6
               Case 6:skirt2.z = 5:bump2 = 7
               case 7:Me.TimerEnabled = 0
           End Select
DOF 150,0
end sub

sub tbump3_hit()

	bump3 = 1
	Me.TimerEnabled = 1
	PlaySound "fx_bumper"
	If ali=2 and lightb3.state= 0 then
	lightb3.state = 1
DOF 149,1
	addscore (10000) 
    advance()
	Primitive103.image = "baseon"
	gi3.State = 1
	Ali=Ali+1		
	 
  
	end if
end sub

sub TBump3_Timer()

           Select Case bump3
               Case 1:skirt3.z = 5:bump3 = 2
               Case 2:skirt3.z = 2:bump3 = 3
               Case 3:skirt3.z = -2:bump3 = 4
               Case 4:skirt3.z = -2:bump3 = 5
               Case 5:skirt3.z = 2:bump3 = 6
               Case 6:skirt3.z = 5:bump3 = 7
               case 7:Me.TimerEnabled = 0
           End Select
DOF 149,0
end sub

sub tbump4_hit()

	bump4 = 1
	Me.TimerEnabled = 1
	PlaySound "fx_bumper" 
If ali=3 and lightb4.state = 0 and moo=0 then
	lightb4.state = 1: addscore (10000)
DOF 150,1
	Primitive105.image = "baseon"
	gi4.State = 1
	advance()
	Baba=Baba+1
   playsound "sequence"
   playsound "sequence"
   playsound "sequence"
	light1.state=lightstateon
   lightbottom.state=lightstateon
	moo=1	
end if
	
end sub

sub TBump4_Timer()

           Select Case bump4
               Case 1:skirt4.z = 5:bump4 = 2
               Case 2:skirt4.z = 2:bump4 = 3
               Case 3:skirt4.z = -2:bump4 = 4
               Case 4:skirt4.z = -2:bump4 = 5
               Case 5:skirt4.z = 2:bump4 = 6
               Case 6:skirt4.z = 5:bump4 = 7
               case 7:Me.TimerEnabled = 0
           End Select
DOF 150,0
end sub

sub r5_hit(): PlaySound "fx_bumper"
	if rsl1.state= 1 then
DOF 144,1
	Primitive57.image = "redshootcovertextureon"
	giLighttriangle1.state = 1
	AwardSpecial
	end if
	if righto.state= 0 then
	addscore (10000)
	end if
	if righto.state= 1 then

      if f = 0 then
         f = 5
         Add50KTimer.Enabled = TRUE
      end if 
end if
end sub

sub advance()
      If Bonus < 6 then
      If Bonus > 0 then BL(Bonus).State = 0
      Bonus = Bonus + 1
      BL(Bonus).State = 1
               End If  
end sub

sub TopBump_hit()

	bumptop = 1
	Me.TimerEnabled = 1
	PlaySound "fx_bumper" 
	If Lightbtop.state = 1 then
DOF 136,1
	addscore (10000)
End If
	end sub

sub TopBump_Timer()

           Select Case bumptop
               Case 1:skirttop.z = 5:bumptop = 2
               Case 2:skirttop.z = 2:bumptop = 3
               Case 3:skirttop.z = -2:bumptop = 4
               Case 4:skirttop.z = -2:bumptop = 5
               Case 5:skirttop.z = 2:bumptop = 6
               Case 6:skirttop.z = 5:bumptop = 7
               case 7:Me.TimerEnabled = 0
           End Select
DOF 136,0
end sub

sub CR_hit()
      if f = 0 then
         f = 5
         Add50KTimer.Enabled = TRUE
      end if   
   if crownlight.state= 1 then 
   AwardSpecial
   crownlight.state= 0
   end if
end sub


sub dbump2_hit()
	bumpcenter = 1
	Me.TimerEnabled = 1
	PlaySound "fx_bumper" 
	if dbl2.state= 1 then
DOF 149,1
DOF 151,1
DOF 152,1
DOF 149,1
DOF 151,1
DOF 119,2
DOF 119,2
DOF 119,2
DOF 119,2
pbasecenter.image = "centerbaseon"
giLightcenter.state = 1
      if f = 0 then
         f = 5
         Add50KTimer.Enabled = TRUE
end if   
   else
      addscore (10000)
   
End If
	
end sub

sub dbump2_Timer()

           Select Case bumpcenter
               Case 1:centerskirt.z = 5:bumpcenter = 2
               Case 2:centerskirt.z = 2:bumpcenter = 3
               Case 3:centerskirt.z = -2:bumpcenter = 4
               Case 4:centerskirt.z = -2:bumpcenter = 5
               Case 5:centerskirt.z = 2:bumpcenter = 6
               Case 6:centerskirt.z = 5:bumpcenter = 7
               case 7:Me.TimerEnabled = 0
           End Select
DOF 149,0
DOF 151,0
DOF 152,0
DOF 119,2
DOF 119,2
DOF 119,2
DOF 119,2
end sub

' Bonus Bumpers score up to 30,000


sub bb1_hit()

	bumpbb1 = 1
	Me.TimerEnabled = 1
	PlaySound "fx_bumper" 
	If Lightbtopleft.state = 1 then
DOF 129,1
	Primitive107.image = "basedark"
	giLightbtopleft.state = 0
	Lightbtopleft.state = 0
	sb = 1
    bonusbumptimer.enabled = true
	End If
end sub

sub bb1_Timer()

           Select Case bumpbb1
               Case 1:skirtbb1.z = 5:bumpbb1 = 2
               Case 2:skirtbb1.z = 2:bumpbb1 = 3
               Case 3:skirtbb1.z = -2:bumpbb1 = 4
               Case 4:skirtbb1.z = -2:bumpbb1 = 5
               Case 5:skirtbb1.z = 2:bumpbb1 = 6
               Case 6:skirtbb1.z = 5:bumpbb1 = 7
               case 7:Me.TimerEnabled = 0
           End Select
DOF 129,0
end sub


sub bb2_hit()

	bumpbb2 = 1
	Me.TimerEnabled = 1
	PlaySound "fx_bumper" 
	If Lightbtopright.state = 1 then
DOF 139,1
	Primitive108.image = "basedark"
	giLightbtopright.state = 0
	Lightbtopright.state = 0
	sb = 1
    bonusbumptimer.enabled = true
End If
end sub

sub bb2_Timer()

           Select Case bumpbb2
               Case 1:skirtbb2.z = 5:bumpbb2 = 2
               Case 2:skirtbb2.z = 2:bumpbb2 = 3
               Case 3:skirtbb2.z = -2:bumpbb2 = 4
               Case 4:skirtbb2.z = -2:bumpbb2 = 5
               Case 5:skirtbb2.z = 2:bumpbb2 = 6
               Case 6:skirtbb2.z = 5:bumpbb2 = 7
               case 7:Me.TimerEnabled = 0
           End Select
DOF 139,0
end sub

sub bb3_hit()

	bumpbb3 = 1
	Me.TimerEnabled = 1
	PlaySound "fx_bumper" 
	If Lightbbottomleft.state = 1 then
DOF 132,1
	Primitive104.image = "basedark"
	giLightbbottomleft.state = 0
	Lightbbottomleft.state = 0
	sb = 1
    bonusbumptimer.enabled = true
	End If
end sub

sub bb3_Timer()

           Select Case bumpbb3
               Case 1:skirtbb3.z = 5:bumpbb3 = 2
               Case 2:skirtbb3.z = 2:bumpbb3 = 3
               Case 3:skirtbb3.z = -2:bumpbb3 = 4
               Case 4:skirtbb3.z = -2:bumpbb3 = 5
               Case 5:skirtbb3.z = 2:bumpbb3 = 6
               Case 6:skirtbb3.z = 5:bumpbb3 = 7
               case 7:Me.TimerEnabled = 0
           End Select
DOF 132,0
end sub

sub bb4_hit()

	bumpbb4 = 1
	Me.TimerEnabled = 1
	PlaySound "fx_bumper" 
	If Lightbbottomright.state = 1 then
DOF 142,1
	Primitive106.image = "basedark"
	giLightbbottomright.state = 0
	Lightbbottomright.state = 0
	sb = 1
    bonusbumptimer.enabled = true

End If
end sub

sub bb4_Timer()

           Select Case bumpbb4
               Case 1:skirtbb4.z = 5:bumpbb4 = 2
               Case 2:skirtbb4.z = 2:bumpbb4 = 3
               Case 3:skirtbb4.z = -2:bumpbb4 = 4
               Case 4:skirtbb4.z = -2:bumpbb4 = 5
               Case 5:skirtbb4.z = 2:bumpbb4 = 6
               Case 6:skirtbb4.z = 5:bumpbb4 = 7
               case 7:Me.TimerEnabled = 0
           End Select
DOF 142,0
end sub

sub bonusbumptimer_timer()
    If sb > 0 then
       playsound "score"
       changescore
       sb = sb - 1  
    else
       bonusbumptimer.enabled = false
    end if
end sub

sub AdvButton_hit()
    playsound "metal"
    Addscore 10000
    wall7x.isdropped=True
	timer2.enabled=True
	timer2.interval=600
    advance()
end sub

sub Timer2_Timer()
timer2.enabled=False
wall7x.isdropped=False
End Sub

dim kick

sub kicker1_hit(): kick = 1: Me.TimerEnabled = 1

   if crownlight.state= 0 then
   playsound "light"
   playsound "light"
   crownlight.state= 1
   lefto.state= 1
   righto.state= 1
   end if
   if light1.state= 1 then
   seqi=6
   end if
   topbonustimer.enabled = TRUE 

end sub

sub kicker1_Timer()

           Select Case kick
               Case 1:kick1.z = -43:kick = 2
			   Case 2:kick1.objrotx = 2:kick = 3
               Case 3:kick1.z = -39:kick = 4
			   Case 4:kick1.objrotx = 4:kick = 5
               Case 5:kick1.z = -33:kick = 6
			   Case 6:kick1.objrotx = 6:kick = 7
               Case 7:kick1.z = -33:kick = 8
			   Case 8:kick1.objrotx = 4:kick = 9
               Case 9:kick1.z = -39:kick = 10
			   Case 10:kick1.objrotx = 2:kick = 11
               Case 11:kick1.z = -43:kick = 12
               case 12:Me.TimerEnabled = 0
           End Select
end sub

sub topbonustimer_timer()
       topbonustimer.enabled = false
       seqi=seqi-1
       if Seqi>0 then
       AwardSpecial
       topbonustimer.enabled=True
       end if
       if Seqi<1 then
       if moo=1 then
       light1.state= 0
       end if
       Randomize
       Candy = Int(Rnd(1)* 2) + 1
       If Candy=1 then
       kicker1.kick 123, 5
DOF 106,2
DOF 119,2
DOF 119,2

       end if
       if Candy=2 then
       kicker1.kick 233, 5
DOF 106,2
DOF 119,2
DOF 119,2
       end if
       playsound "saucer"
    end if         
end sub

dim kick3

sub kicker2_hit(): kick3 = 1: Me.TimerEnabled = 1
   if lightbottom.state= 1 then AwardSpecial
   bottombonustimer.enabled = TRUE 
end sub

sub kicker2_Timer()

           Select Case kick3
               Case 1:kick2.z = -43:kick3 = 2
			   Case 2:kick2.objrotx = 2:kick3 = 3
               Case 3:kick2.z = -39:kick3 = 4
			   Case 4:kick2.objrotx = 4:kick3 = 5
               Case 5:kick2.z = -33:kick3 = 6
			   Case 6:kick2.objrotx = 6:kick3 = 7
               Case 7:kick2.z = -33:kick3 = 8
			   Case 8:kick2.objrotx = 4:kick3 = 9
               Case 9:kick2.z = -39:kick3 = 10
			   Case 10:kick2.objrotx = 2:kick3 = 11
               Case 11:kick2.z = -43:kick3 = 12
               case 12:Me.TimerEnabled = 0
           End Select
end sub


sub bottombonustimer_timer()
    if bonus > 0 then
       PlaySound "bump"
       ChangeScore
    if bonus >1 then
       playsound "bump"
       ChangeScore
    end if
       bl(bonus).State = 0
       bonus = bonus - 1
       If Bonus > 0 then bl(bonus).State = 1
    else
       bottombonustimer.enabled = false
       Bonus = 1
       BL(1).State = 1
       kicker2.kick 180, 5
DOF 109,2
DOF 119,2
DOF 119,2
       playsound "saucer"
    end if         
end sub

Sub AddScore(points)
 If Tilted = TRUE then Exit Sub
 If Tilted = FALSE and Scoring = FALSE then
If Score = 0 then DrainWall.IsDropped = FALSE
    ChangeScore
  	Scoring = TRUE
    PlaySound "score"

	
	
	Lightbtopleft.state = 0: Primitive107.image = "basedark"
	giLightbtopleft.state = 0

	
	giLightbtopright.state = 0: Primitive108.image = "basedark"
	Lightbtopright.state = 0

	Lightbbottomleft.state = 0: Primitive104.image = "basedark"
	giLightbbottomleft.state = 0

	Lightbbottomright.state = 0: Primitive106.image = "basedark"
	giLightbbottomright.state = 0

  	
  	Lightbtop.State = 0: Primitive111.image = "basedark"
	giLightbtop.state = 0
  	ScoreTimer.Enabled = TRUE 
 End If 
End Sub

Sub Add50KTimer_Timer()  
    If f > 0 then  
       If f = 5 then 
          PlaySound "50k"


	Lightbtopleft.state = 0: Primitive107.image = "basedark"
	giLightbtopleft.state = 0

	
	giLightbtopright.state = 0: Primitive108.image = "basedark"
	Lightbtopright.state = 0

	Lightbbottomleft.state = 0: Primitive104.image = "basedark"
	giLightbbottomleft.state = 0

	Lightbbottomright.state = 0: Primitive106.image = "basedark"
	giLightbbottomright.state = 0


          
  	    Lightbtop.State = 0: Primitive111.image = "basedark"
		giLightbtop.state = 0

       End If
       ChangeScore	
       f = f - 1
    Else
       Add50kTimer.Enabled = FALSE
       Scoring = FALSE


	Lightbtopleft.state = 1: Primitive107.image = "baseon"
	giLightbtopleft.state = 1

	
	giLightbtopright.state = 1: Primitive108.image = "baseon"
	Lightbtopright.state = 1

	Lightbbottomleft.state = 1: Primitive104.image = "baseon"
	giLightbbottomleft.state = 1

	Lightbbottomright.state = 1: Primitive106.image = "baseon"
	giLightbbottomright.state = 1


      
  	 Lightbtop.State = 1: Primitive111.image = "baseon"
	 giLightbtop.state = 1
     DBL2.State = 0 
	 pbasecenter.image = "centerbasedark"
	 giLightcenter.state = 0
    End If 
End Sub


Sub Timer3_Timer()  
    If Q > 0 then  
       If Q = 30 then 
       End If
       ChangeScore	
       Q = Q - 1
    Else
       Timer3.Enabled = FALSE
       Scoring = FALSE 
    End If 
End Sub


Sub ScoreTimer_Timer()
    ScoreTimer.Enabled = FALSE
    Scoring = FALSE
    If Tilted = TRUE then Exit Sub


	Lightbtopleft.state = 1: Primitive107.image = "baseon"
	giLightbtopleft.state = 1

	
	giLightbtopright.state = 1: Primitive108.image = "baseon"
	Lightbtopright.state = 1

	Lightbbottomleft.state = 1: Primitive104.image = "baseon"
	giLightbbottomleft.state = 1

	Lightbbottomright.state = 1: Primitive106.image = "baseon"
	giLightbbottomright.state = 1


  	
  	Lightbtop.State = 1: Primitive111.image = "baseon"
	giLightbtop.state = 1
    Randomize
    Candy = Int(Rnd(1)* 12) + 1
       If candy<8 then 
       DBL2.State= 1
		pbasecenter.image = "centerbaseon"
		giLightcenter.state = 1
       rsl2.state= 0: Primitive74.image = "redshootcovertexture"
		giLighttriangle2.state = 0
DOF 125,0
       rsl1.state= 0: Primitive57.image = "redshootcovertexture"
		giLighttriangle1.state = 0
DOF 144,0
       end if
       If candy=8 then 
       DBL2.State= 0
		pbasecenter.image = "centerbasedark"
		giLightcenter.state = 0
       rsl2.state= 0: Primitive74.image = "redshootcovertexture"
		giLighttriangle2.state = 0
DOF 125,0
       rsl1.state= 0: Primitive57.image = "redshootcovertexture"
		giLighttriangle1.state = 0
DOF 144,0
       end if
              If candy=9 then 
       DBL2.State= 0
		pbasecenter.image = "centerbasedark"
		giLightcenter.state = 0
       rsl2.state= 0: Primitive74.image = "redshootcovertexture"
		giLighttriangle2.state = 0
DOF 125,0
       rsl1.state= 0: Primitive57.image = "redshootcovertexture"
		giLighttriangle1.state = 0
DOF 144,0
       end if
              If candy>9 then 
       DBL2.State= 0
		pbasecenter.image = "centerbasedark"
		giLightcenter.state = 0
       rsl2.state= 1: Primitive74.image = "redshootcovertextureon"
DOF 125,1
		giLighttriangle2.state = 1
       rsl1.state= 1: Primitive57.image = "redshootcovertextureon"
DOF 144,1
		giLighttriangle1.state = 1
       end if
End Sub

Sub ChangeScore()
    If Tilted = TRUE then Exit Sub
	Score = Score + 10000
	' Lite-Up Scoring 10k's
    If Score < 790000 then
       TenKs = (Score Mod 100000) / 10000
       For X = 1 to 9
         KBox(x).State = 0
		 DOF 10*x, 0
       Next
       If Tenks<>0 then 
			Kbox(Tenks).State = 1     
			DOF 10*Tenks, 1
	   End If
   ' Lite Up Scoring 100K's
       HunKs = Int(Score/100000)
       Hunks = Hunks Mod 10
       For X = 1 to 7
          HunkLight(x).State = 0
		  DOF 200+x, 0
       Next
       If Hunks<>0 then 
			HunkLight(Hunks).State = 1
			DOF 200+Hunks, 1
	   End IF
     'Else
       'AuxBox.Text = "Score"
       'AuxSc.Text = FormatNumber(score, 0, -1, 0, -1)
     end if
End Sub

Sub AwardSpecial
DOF 111,2
    If Credit < 26 then
       Credit = Credit + 1
       CredBox.Text = Credit
       PlaySound "knock"
  if f = 0 then
         f = 5
         Add50KTimer.Enabled = TRUE
      end if   

    end if
End Sub       


Sub OutGate_Hit()
	Hold = 0
    PlaySound "gater"
    If Tilted = TRUE  and Ball = Ballsout then
       GameOn = FALSE
       Exit Sub
    End If
    If Tilted = FALSE then
       Ball = Ball + 1
	'MSGBOX BALL & " SOCRE " & score

       If Ball > 5 then
          GameOn = FALSE
         ' CheckNewHighScorePostIt1Player score '1 player table

          Chubby=0
         If score > hiscore then
            hiscore = score

			'savehs
            'HSBox.text = FormatNumber(hiscore, 0, -1, 0, -1)
         End If    
	   end if
    end if
End Sub

Sub timergate_Timer
	primitive95.rotz=gate.CurrentAngle 
End Sub       

Sub Gate_hit : PlaySound "Gate" : End Sub


' High Score To Date Routines
sub savehs
	' Based on Black's Highscore routines
	Dim FileObj
	Dim ScoreFile
	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "hmpty.txt",True)
		ScoreFile.WriteLine hiscore
		ScoreFile.Close
	Set ScoreFile=Nothing
	Set FileObj=Nothing
end sub

sub loadhs
    ' Based on Black's Highscore routines
	Dim FileObj
	Dim ScoreFile
    dim temp1
    Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & "HumptyDumpty.txt") then
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "HumptyDumpty.txt")
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			Exit Sub
		End if
		temp1=TextStr.ReadLine
		TextStr.Close
	    hiscore = CDbl(temp1)
	    Set ScoreFile=Nothing
	    Set FileObj=Nothing
End Sub

Sub UpdateFlipperLogo_Timer
    Primitive1.objrotz = Flipper4.CurrentAngle + 299
    Primitive5.objrotz = Flipper1.CurrentAngle + 57 
    Primitive2.objrotz = Flipper5.CurrentAngle + 299
    Primitive4.objrotz = Flipper2.CurrentAngle + 57 
    Primitive3.objrotz = Flipper6.CurrentAngle + 299
    Primitive6.objrotz = Flipper3.CurrentAngle + 57
End Sub 

sub ln1_hit()
    ln = TRUE
end sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************


Function Vol2(ball1, ball2) ' Calculates the Volume of the sound based on the speed of two balls
    Vol2 = (Vol(ball1) + Vol(ball2) ) / 2
End Function


'***************************************************************
'             Supporting Ball & Sound Functions v4.0
'  includes random pitch in PlaySoundAt and PlaySoundAtBall
'***************************************************************

Dim TableWidth, TableHeight

TableWidth = Table1.width
TableHeight = Table1.height

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^20 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / TableWidth-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = (SQR((ball.VelX ^2) + (ball.VelY ^2)))
End Function

Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / TableHeight-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.1, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.2, Pitch(ActiveBall) * 10, 0, 0, AudioFade(ActiveBall)
End Sub

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function


'***********************************
'   JP's VP10 Rolling Sounds v4.0
'   JP's Ball Shadows
'   JP's Ball Speed Control
'   Rothbauer's dropping sounds
'***********************************

Const tnob = 5   'total number of balls
Const lob = 0     'number of locked balls
Const maxvel = 35 'max ball velocity
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Dim aBallShadow
aBallShadow = Array (BallShadow1)

Sub CollisionTimer_Timer() 'call this routine from any realtime timer you may have, running at an interval of 10 is good.

    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls and hide the shadow
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
		'Debug.Print b
		'BallShadow1.Y = 2100 'under the apron 'may differ from table to table
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' draw the ball shadow
    For b = lob to UBound(BOT)
        'BallShadow1.X = BOT(b).X
		'BallShadow1.Y = BOT(b).Y
        'BallShadow1.Height = BOT(b).Z - Ballsize / 2 + 1

    'play the rolling sound for each ball
        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 25000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b)) * 5
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b)), 0, ballpitch, 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' jps ball speed control
        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            speedfactorx = ABS(maxvel / BOT(b).VelX)
            speedfactory = ABS(maxvel / BOT(b).VelY)
            If speedfactorx < 1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactorx
                BOT(b).VelY = BOT(b).VelY * speedfactorx
            End If
            If speedfactory < 1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactory
                BOT(b).VelY = BOT(b).VelY * speedfactory
            End If
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub metal_Hit (idx)
	PlaySound "metal", 10, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner",0,.25,0,0.25
End Sub

Sub rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 1 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 1 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
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

Sub Table1_Exit
    Controller.Stop
End Sub