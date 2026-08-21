
' Splatterhouse


	Option Explicit
Dim Controller
Dim FlexDMD
Const UseFlexDMD = 1

Dim Score

Sub AddScore(points) 'we also need to Dim Score in the beginning of script. all Variables should go in the beginning of script.
Score = Score + points ' This adds your score + the points in the (Brackets) when something is hit & it contains AddScore(#)
ScoreText.Text = FormatNumber(Score, 0, -1, 0, -1) 'this Displays the points added in scoretext.Text box on the backdrop
ScoreR1.SetValue Score
End Sub



Sub AddPoints(Points)
    Score(CurrentPlayer) = Score(CurrentPlayer) + Points
	scorereel1.addvalue(points)
 
End Sub

Sub Timer12_Timer
ScoreText.Text = FormatNumber(Score, 0, -1, 0, -1) 'this Displays the points added in scoretext.Text box on the backdrop
ScoreR1.SetValue Score
Timer12.Enabled = True
	End Sub



'	Randomize




	turnoffrules = 0 ' change to 1 to take off the backglass helper rules text during a game
	turnonultradmd = 0 ' change to 1 to turn on ultradmd
	toppervideo = 0 'set to 1 to turn on the topper
	ballrolleron = 1 ' set to 0 to turn off the ball roller if you use the "c" key in your cabinet

	Const typefont = "Raleway Medium"
	Const numberfont = "Bebas Neue"
	Const zoomfont = "Fundamental  Brigade"
	Const zoombgfont = "Fundamental 3D  Brigade" ' needs to be an outline of the zoomfont
	Const cGameName = "demo"
	Const TableName = "demo"
	Const myVersion = "75"
	
	'Constructions
	Const BallSize = 50
	Const MaxPlayers = 1
	Const BallSaverTime = 15 
	Const MaxMultiplier = 6 
	Const MaxMultiballs = 5
	Const bpgcurrent = 3
    Const Balls = 5

	' Define Global Variables

	Dim toppervideo
	Dim ballrolleron
	Dim turnonultradmd
	Dim turnoffrules
	Dim PlayersPlayingGame
	Dim CurrentPlayer
	Dim Credits
	Dim BonusPoints(4)
	Dim BonusHeldPoints(4)
	Dim BonusMultiplier(4)
	Dim bBonusHeld
	Dim BallsRemaining(4)
	Dim ExtraBallsAwards(4)
	Dim HighScore(4)
	Dim HighScoreName(4)
	Dim WaffleScore(4)
	Dim WaffleScoreName(4)
	Dim Jackpot
	Dim SuperJackpot
	Dim Tilt
	Dim TiltSensitivity
	Dim Tilted
	Dim TotalGamesPlayed
	Dim mBalls2Eject
	Dim SkillshotValue(4)
	Dim bAutoPlunger
	Dim bInstantInfo
	Dim bromconfig
	Dim bAttractMode
	Dim LastSwitchHit
	Dim BallsOnPlayfield
	Dim BallsInHole
	Dim bFreePlay
	Dim bGameInPlay
	Dim bOnTheFirstBall
	Dim bBallInPlungerLane
	Dim bBallSaverActive
	Dim bBallSaverReady
	Dim bMultiBallMode
	Dim bMusicOn
	Dim bSkillshotReady
	Dim bExtraBallWonThisBall
	Dim bJustStarted
    Dim ResetTargetsDelay
    Dim NewBallDelay
    Dim MatchDelay
    Dim BonusCountDelay
Dim Match
Dim Game_Over

Dim Special
Dim Bonus






Dim Ball

'Dim Credits
'Dim Match
'Dim Game_Over
'Dim NewBallDelay
'Dim Special
'Const Special1 = 62000
'Const Special2 = 76000
'Const Special3 = 84000
'Dim Special1Awarded, Special2Awarded, Special3Awarded
'Dim Score, Add10, Add100, Add1000
'Dim HighScore
'Dim Bonus
'Dim BonusMultiplier
'Dim BonusCountDelay
'Dim ResetTargetsDelay
'Dim MatchDelay









'Sub Loadhs
 '   Dim x
 '   x = LoadValue(TableName, "HighScore")
 '   If(x <> "") Then HighScore = CDbl(x) Else HighScore = 10000 End If
'End Sub



'HologramAnim1.enabled = -1



PlaySound "splatter",-1


Sub table1_Init
If UseFlexDMD Then FlexDMD_Init
Set Controller = CreateObject("B2S.Server")
Controller.B2SName = "splatterhouse"
Controller.Run()
If Err Then MsgBox "Can't Load B2S.Server."
Dim ii
    Ball = 0:UpdateBallNumber
    Credits = 0
    Match = 0

   StartAttractMode

 
 ScoreReel1.AddValue Score

Timer14.enabled=1




'    NewBallDelay = 0
 '   Special = False
 '   Special1Awarded = False
  '  Special1Awarded = False
   ' Special1Awarded = False
    Score = 0
    BonusCountDelay = 0
	ResetTargetsDelay = 0
	MatchDelay = 0
    Game_Over = TRUE
    Clear_Match
    loadhs
    CreditReel.Setvalue Credits
    ScoreReel1.SetValue Score
    'StartShake
    GameTimer.Enabled = 1
    ' reset VP objects
' Setup the lightning according to the nightday slider
If table1.nightday < 50 Then
for each ii in aGiLights: ii.intensity = ii.intensity + (100 - table1.nightday)/10: next
'bumper1light.opacity=bumper1light.opacity + (100 - table1.nightday)^2
'bumper2light.opacity=bumper2light.opacity + (100 - table1.nightday)^2
End If
End Sub

Sub FlexDMD_Init() 'default/startup values

	DIm FlexDMDFont
	Dim FlexDMDScene
	

	Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
	If Not FlexDMD is Nothing Then
	
		FlexDMD.Show = False
		FlexDMD.LockRenderThread

		'FlexDMD.GameName = cGameName
		FlexDMD.RenderMode = 2
		FlexDMD.Width = 128
		FlexDMD.Height = 32
		FlexDMD.Clear = True
		FlexDMD.Run = True

		Set FlexDMDScene = FlexDMD.NewGroup("Scene")

		FlexDMDScene.AddActor FlexDMD.NewImage("Back","FlexDMD.Resources.dmds.black.png")

		FlexDMDFont = FlexDMD.NewFont("FlexDMD.Resources.udmd-f12by24.fnt", vbRed, vbGreen, 1)
	
		FlexDMDScene.AddActor(FlexDMD.NewLabel("Score", FlexDMDFont, "0,000,000"))
		FlexDMDScene.GetLabel("Score").SetAlignedPosition 4, 1, 0

		FlexDMDFont = FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt", vbGreen, vbBlue, 0)

		FlexDMDScene.AddActor(FlexDMD.NewLabel("Ball", FlexDMDFont,  "Ball 0"))
		FlexDMDScene.GetLabel("Ball").SetAlignedPosition 1, 27, 0
	
		FlexDMDScene.AddActor(FlexDMD.NewLabel("Credit", FlexDMDFont,  "Credits 0"))
		FlexDMDScene.GetLabel("Credit").SetAlignedPosition 90, 27, 0
	
		
		FlexDMD.Stage.AddActor FlexDMDScene
		
		FlexDMD.Show = True
		FlexDMD.UnlockRenderThread

		FlexTimer.Enabled = True
		
	End If

End Sub

Sub FlexTimer_Timer()
	FlexDMD.LockRenderThread
	With FlexDMD.Stage
		.GetLabel("Score").Text = Right(FormatNumber(Score + 10000000, 0, -1, 0, -1),9) 
		.GetLabel("Ball").Text = "Ball " & CStr(Ball)
		.GetLabel("Credit").Text = "Credits " & CStr(Credits)
	End With
	FlexDMD.UnlockRenderThread
End Sub

Sub Gate2_Timer:Gateheavy.RotX = Gate2.Currentangle:End Sub

Sub GameTimer_Timer

    ' check the delays

    If NewBallDelay > 0 Then
        NewBallDelay = NewBallDelay - 1
        If NewBallDelay = 0 Then NewBall:End If
    End If

    If ResetTargetsDelay > 0 Then
        ResetTargetsDelay = ResetTargetsDelay - 1
        If ResetTargetsDelay = 0 Then ResetDroptargets:End If
    End If

    If MatchDelay > 0 Then
        MatchDelay = MatchDelay - 1
        If MatchDelay = 0 Then Verification_Match:End If
    End If



    If BonusCountDelay > 0 Then
        BonusCountDelay = BonusCountDelay - 1
        If BonusCountDelay = 0 Then CountBonus:End If
    End If
End Sub






Sub Table1_KeyDown(ByVal keycode)

     	If keycode = LeftFlipperKey Then
		LeftFlipper.RotateToEnd
		PlaySound "fx_flipperup", 0, .67, -0.05, 0.05
	End If
    
	If keycode = RightFlipperKey Then
		RightFlipper.RotateToEnd
		PlaySound "fx_flipperup", 0, .67, 0.05, 0.05
	End If



    If keycode = PlungerKey Then Plunger.Pullback:PlaySound "fx_Plungerpull"



    If keycode = AddCreditKey And Credits < 8 Then
        PlaySound "fx_coin"
        Addcredits 1
    End If

    If keycode = AddCreditKey2 And Credits < 8 Then
        PlaySound "fx_coin"
        AddCredits 2
        Savehs
    End If

    If keycode = StartGameKey And Credits > 0 AND Game_Over = TRUE Then
        AddCredits -1
        'PlaySound "fx_Startup"
        Initialize
        Clear_Match
        Game_Over = False
        NextBall
    End If

    If keycode = LeftTiltKey Then Nudge 90, 6:PlaySound "fx_nudge"
    If keycode = RightTiltKey Then Nudge 270, 6:PlaySound "fx_nudge"
    If keycode = CenterTiltKey Then Nudge 0, 7:PlaySound "fx_nudge"
End Sub

Sub Table1_KeyUp(ByVal keycode)

    If keycode = PlungerKey Then Plunger.Fire:PlaySound "fx_Plunger"


    	If keycode = LeftFlipperKey Then
		LeftFlipper.RotateToStart
		PlaySound "fx_flipperdown", 0, 1, -0.05, 0.05
	End If
    
	If keycode = RightFlipperKey Then
		RightFlipper.RotateToStart
		PlaySound "fx_flipperdown", 0, 1, 0.05, 0.05
        RotateLaneLightsRight
	End If
   
End Sub

'***************************************
'     Ghost  flippers
'**************************************


Sub Timer10_Timer
RightFlipperP.Rotz = RightFlipper.CurrentAngle
LeftFlipperP.Rotz = LeftFlipper.CurrentAngle
	End Sub







Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 6, -0.05, 0.05
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 6, 0.05, 0.05
End Sub

'**********************
' Drain and Reset subs
'**********************

' end of ball flow
'1 Drain ball - remove ball
'1 Check Tilt
'2 Count bonus
'3 Check highscore
'4 Next Ball (back to 1 until all balls are played)
'5 Game Over

Sub Drain_Hit()
    Drain.DestroyBall
    PlaySound "fx_Drain"
    CountBonus
    
End Sub

Sub Initialize 'beginning of a new game
    Score = 0
'    Special = False
 '   Special1Awarded = False
  '  Special1Awarded = False
   ' Special1Awarded = False
    ScoreReel1.ResetToZero
'    Bonus = 0
 '   BonusMultiplier = 0
End Sub

Sub Reset_Table() 'after each new ball
    Special = False
    'Setup other lights & objects
    l16.State = 1 '5 trigger lights
    l17.State = 1
    ResetTargetsDelay = 10
    dtdropped = 0
End Sub

Sub NewBall
    PlaySound "fx_ballrel"
    BallRelease.CreateBall
    BallRelease.Kick 90, 5
    StopAttractMode()
    
End Sub

Sub NextBall()
    Ball = Ball + 1
    If Ball = Balls then l14.State = 1 'double bonus on the las ball
    If Ball > Balls Then               'this is the last ball
        GameOver
    Else
        UpdateBallNumber
        Reset_Table
        NewBallDelay = 10
    End If
End Sub

Sub GameOver
    Ball = 0:UpdateBallNumber 'display the game over sign
    Game_Over = True
     ScoreReel1.ResetToZero
     StartAttractMode
End Sub

Sub UpdateBallNumber 'if ball = 0 shows the Game Over
    Select Case Ball
        Case 0:Ball5.SetValue 0:GameoverR.SetValue 1
        Case 1:GameoverR.SetValue 0:Ball1.SetValue 1
        Case 2:Ball1.SetValue 0:Ball2.SetValue 1
        Case 3:Ball2.SetValue 0:Ball3.SetValue 1
        Case 4:Ball3.SetValue 0:Ball4.SetValue 1
        Case 5:Ball4.SetValue 0:Ball5.SetValue 1
    End Select
End Sub

'*****************
'      Tilt
'*****************




Sub IncreaseMatch
    Match = (Match + 10) MOD 100
End Sub

Sub Verification_Match()
    PlaySound "fx_match"
    Display_Match
    If(Score MOD 100) = Match Then
        ExtraGame
    End If
End Sub

Sub AddCredits(value)
    Credits = Credits + value
    CreditReel.SetValue Credits
End Sub

Sub Clear_Match()

End Sub



Sub Display_Match()

End Sub

Sub ExtraGame()
    If Credits < 8 Then
        AddCredits 1
    End If
    PlaySound "fx_knocker"
End Sub

'******************
'   GI effects
' independent routine
' it turns on the gi
' when there is a ball
' in play
'******************




'**************
'   Score
'**************

Sub AddScore(Points)
    Select Case Points
        Case 10, 100, 1000
            Score = Score + Points
            ScoreReel1.AddValue Points
            If Points = 100 AND(Score MOD 1000) \ 100 = 0 Then  'New 1000 reel
                PlaySound "fx_bell1000"
            ElseIf Points = 10 AND(Score MOD 100) \ 10 = 0 Then 'New 100 reel
                PlaySound "fx_bell100"
            Else
                PlaySound "fx_bell" &Points
            End If
        Case 50
            Add10 = Add10 + 5	
            AddScore10Timer.Enabled = TRUE
        Case 500
            Add100 = Add100 + 5	
            AddScore100Timer.Enabled = TRUE
        Case 2000, 3000, 4000, 5000
            Add1000 = Add1000 + Points \ 1000 
            AddScore1000Timer.Enabled = TRUE
    End Select

    ' check replays
    If Score >= Special1 AND Special1Awarded = False Then
        ExtraGame
        Special1Awarded = True
    End If
    If Score >= Special2 AND Special2Awarded = False Then
        ExtraGame
        Special2Awarded = True
    End If
    If Score >= Special3 AND Special3Awarded = False Then
        ExtraGame
        Special3Awarded = True
    End If
End Sub

'******************************
'TIMER DE 10, 100 y 1000 PUNTOS
'******************************

Sub AddScore10Timer_Timer()
    if Add10 > 0 then
        AddScore 10
        Add10 = Add10 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore100Timer_Timer()
    if Add100 > 0 then
        AddScore 100
        Add100 = Add100 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore1000Timer_Timer()
    if Add1000 > 0 then
        AddScore 1000
        Add1000 = Add1000 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

'***********
' Highscore
'***********



' Load & Save Highscore

Sub Loadhs
    Dim x
    x = LoadValue(TableName, "HighScore")
    If(x <> "") Then HighScore = CDbl(x) Else HighScore = 10000 End If

    x = LoadValue(TableName, "Credits")
    If(x <> "") then Credits = CInt(x) Else Credits = 0 End If
End Sub



'**************
'    Bonus
'**************

Sub AddBonus
    If Bonus < 10 Then
        Bonus = Bonus + 1
        UpdateBonusLights
    End If
End Sub

Sub AddBonusMultiplier 'simply turn on the lights
    If l14.State = 0 Then
        l14.State = 1
    ElseIf l15.State = 0 Then
        l15.State = 1
    End If
End Sub

Sub UpDateBonusLights
    Dim ii
    ResetBonusLights
    If Bonus > 0 Then
        For ii = 0 to Bonus-1
 '           aBonusLights(ii).State = 1
        Next
    End If
End Sub

Sub CountBonus
    If Bonus > 0 Then
        Bonus = Bonus -1
        AddScore 1000 + 1000 * l14.State + 1000 * l15.State
       UpdateBonusLights
        BonusCountDelay = 4 + l14.State * 3 + l15.State * 3
    Else
        BonusCountDelay = 0
       ResetBonusLights 'turn off the bonus lights in case of tilt
        'reset bonus multiplier lights
        l14.State = 0
        l15.State = 0
        ' continue the end of ball procedure
        NextBall
    End If
End Sub

Sub ResetBonusLights
    Dim lamp
    For each lamp in aBonusLights
        lamp.State = 0
    Next
End Sub

'********************
' Diverse Help/Sounds
'********************

Sub aRubbers_Hit(idx):PlaySound "fx_rubber", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aPostRubbers_Hit(idx):PlaySound "fx_rubber2", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aMetals_Hit(idx):PlaySound "fx_MetalHit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aPlastics_Hit(idx):PlaySound "fx_PlasticHit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aGates_Hit(idx):PlaySound "fx_Gate", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aWoods_Hit(idx):PlaySound "fx_Woodhit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub

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
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub


'**********************************************************************
'       HIT EVENTS: tables switches and events start here
' *********************************************************************
'                        Table Object Hit Events
'
' Any target hit sub will follow this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/modes this trigger is a member of
' *********************************************************************

'***********
' Bumpers
'***********

	Sub Bumper1_Hit
		LightEffect 5
		If NOT Tilted Then
			PlaySoundAt SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), ActiveBall
			DOF 110, DOFPulse
			DOF 302, DOFPulse   'DOF MX - Bumper 1
			AddScore 1000
            b1l2.State = 1:b1l1. State = 1
           
		End If
	End Sub



	Sub Bumper2_Hit
		LightEffect 5
		If NOT Tilted Then
			PlaySoundAt SoundFXDOF("fx_bumper", 109, DOFPulse, DOFContactors), ActiveBall
			DOF 111, DOFPulse
			DOF 303, DOFPulse   'DOF MX - Bumper 2
			AddScore 1000
              B2L1.State = 1:b2l2. State = 1
               
		End If
	End Sub

	

	Sub Bumper4_Hit
		LightEffect 5
		If NOT Tilted Then
			PlaySoundAt SoundFXDOF("fx_bumper", 108, DOFPulse, DOFContactors), ActiveBall
			DOF 112, DOFPulse
			DOF 304, DOFPulse   'DOF MX - Bumper 3
			AddScore 1000
            
            B4L1.State = 1:B4L2. State = 1
		End If
	End Sub

	Sub Bumper5_Hit
		LightEffect 5
		If NOT Tilted Then
			PlaySoundAt SoundFXDOF("fx_bumper", 108, DOFPulse, DOFContactors), ActiveBall
			DOF 112, DOFPulse
			DOF 304, DOFPulse   'DOF MX - Bumper 3
			AddScore 1000
            b5l2.State = 1:b5l1. State = 1
		End If
	End Sub

Sub Timer6_Timer
    B2L1.State = 0:b2l2. State = 0
    b1l2.State = 0:b1l1. State = 0
    b3l1.State = 0:b3l2. State = 0
      B4L1.State = 0:B4L2. State = 0
    b5l2.State = 0:b5l1. State = 0


    Timer6.Enabled = True
End Sub

Sub Lane1_Hit
    PlaySound "sensor"
    AddScore 10
End Sub

Sub Lane2_Hit
    PlaySound "sensor"
    AddScore 10
End Sub

'***************
'  Slingshots
'***************
Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    PlaySound "fx_slingshot", 0, 1, -0.05, 0.05
    'LeftSling1.Visible = 0
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
    ' add points
    AddScore 10
End Sub

Sub LeftSlingShot_Timer
      LeftSlingShot.TimerEnabled = 0
   

    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    PlaySound "fx_slingshot", 0, 1, 0.05, 0.05
    'RightSling1.Visible = 0
    RStep = 0
    RightSlingShot.TimerEnabled = 1
    ' add points
    AddScore 10
End Sub

Sub RightSlingShot_Timer
   
     RightSlingShot.TimerEnabled = 0
    RStep = RStep + 1
End Sub

'*********************
'  Targets & switches
'*********************

' Flipper Lanes

Sub sw1_Hit ' left outlane
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
        AddBonus
        'give extra ball if light is lit
        If l12.State = 1 Then
            PlaySound "fx_knocker"
            Ball = Ball - 1
            l1.State = 1
        End If
End Sub


Sub sw3_Hit ' right outlane
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
        AddBonus
          l3.State = 1
        'give extra game is light is lit
        If l3.State = 1 Then
            ExtraGame
        End If
End Sub

Sub sw2_Hit ' left inlane
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
       l2.State = 1
End Sub

Sub sw4_Hit ' right inlane
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
End Sub

' Top Lanes

Sub sw6_Hit
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
        AddBonus
        If l27.State = 1 Then
            AddBonus
        End If
End Sub

Sub sw7_Hit
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
        AddBonus
        If l26.State = 1 Then
            AddBonus
        End If
End Sub

Sub sw8_Hit
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
        AddBonus
        If l25.State = 1 Then
            AddBonus
        End If
End Sub

Sub sw9_Hit
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
        AddBonus
        If l24.State = 1 Then
            AddBonus
        End If
End Sub

Sub sw10_Hit
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
        AddBonus
        If l23.State = 1 Then
            AddBonus
        End If
End Sub

' Side Lanes

Sub sw5_Hit
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
        If l22.State = 1 Then
            AddBonusMultiplier
        End If
End Sub

Sub sw11_Hit
    PlaySound "fx_sensor"
        AddScore 500
     l11.State = 1
End Sub

Sub sw12_Hit
    PlaySound "fx_sensor"
        AddScore 500
     l12.State = 1
End Sub

Sub sw13_Hit
    PlaySound "fx_sensor"
        AddScore 500
     l13.State = 1
End Sub



' Triggers on top of the red lights


Sub kicker1_hit()
Timer9.enabled = True

end sub 


Sub Timer9_timer()
Kicker1.Kick 20,80
Timer9.enabled = False
End Sub








' Droptargets

Dim dt1, dt2, dt3, dt4, dt5, dtdropped

Sub ResetDroptargets
    PlaySound "flip_hit_3"
    dt1 = 0
    dt2 = 0
    dt3 = 0
    dt4 = 0
    dt5 = 0
 '   drop1.IsDropped = 0
  '  drop2.IsDropped = 0
   ' drop3.IsDropped = 0
 '   drop4.IsDropped = 0
 '   drop5.IsDropped = 0
End Sub

Sub CheckDroptargets
    If(dt1 + dt2 + dt3 + dt4 + dt5) = 5 Then
        dtdropped = 1
        ResetTargetsDelay = 30
    End If
    'turn on the Special and gives 5000 points but only once per ball
    If Special = False AND dtdropped AND(l16.State + l14.State + l15.State) = 0 Then
        Addscore 5000
        Special = True
        l16.State = 1
        l14.State = 1
    End If
End Sub

Sub drop1_Hit
        PlaySound "fx_droptarget", 0, 1, -0.05, 0.05
        drop1.IsDropped = 1
        dt1 = 1
        AddScore 500
        AddBonus
        CheckDroptargets
End Sub

Sub drop2_Hit
        PlaySound "fx_droptarget", 0, 1, -0.05, 0.05
        drop2.IsDropped = 1
        dt2 = 1
        AddScore 500
        AddBonus
        CheckDroptargets
End Sub

Sub drop3_Hit
        PlaySound "fx_droptarget", 0, 1, -0.05, 0.05
        drop3.IsDropped = 1
        dt3 = 1
        AddScore 500
        AddBonus
        CheckDroptargets
End Sub

Sub drop4_Hit
        PlaySound "fx_droptarget", 0, 1, -0.05, 0.05
        drop4.IsDropped = 1
        dt4 = 1
        AddScore 500
        AddBonus
        CheckDroptargets
End Sub

Sub drop5_Hit
        PlaySound "fx_droptarget", 0, 1, -0.05, 0.05
        drop5.IsDropped = 1
        dt5 = 1
        AddScore 500
        AddBonus
        CheckDroptargets
End Sub

' Holes

Dim HolePos
HolePos = 0

Sub Hole_Hit
    PlaySound "fx_kicker_enter", 0, 1, 0.15, 0.05
    HolePos = 0
    HoleScoreTimer.Enabled = 1
End Sub

Sub HoleScoreTimer_Timer
    Select Case HolePos
        Case 0:Addscore 100 + 900 * l33.State
        Case 1:Addscore 100 + 900 * l32.State
        Case 2:Addscore 100 + 900 * l31.State
        Case 3:Addscore 100 + 900 * l30.State
        Case 4:Addscore 100 + 900 * l29.State
        Case 5:PlaySound "fx_kicker", 0, 1, 0.15, 0.05:Hole.Kick 240, 7 + RND(1) * 3:holekicker.IsDropped = 0:holekicker.TimerEnabled = 1:Me.Enabled = 0
    End Select

    HolePos = HolePos + 1
End Sub

Sub holekicker_Timer
    holekicker.IsDropped = 1
    Me.TimerEnabled = 0
End Sub

' 10 point rebounds

Sub aRebounds_Hit(idx):PlaySound "fx_rubber", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:AddScore 10:End Sub

' Spinner

Sub Spinner_Spin
    AddScore 10 + 900 * l28.State
    TopLightsPos = TopLightsPos + 1:UpdateTopLights
End Sub






sub savehs
    savevalue "Highscore", "hiscore", hisc
End Sub

sub loadhs
    dim temp
    If (temp <> "") then hisc = CDbl(temp)
    temp = LoadValue("Highscore", "match")
End Sub




























'**************
'    Bonus
'**************


'********************
' Diverse Help/Sounds
'********************

Sub aRubbers_Hit(idx):PlaySound "fx_rubber", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aPostRubbers_Hit(idx):PlaySound "fx_rubber2", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aMetals_Hit(idx):PlaySound "fx_MetalHit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aPlastics_Hit(idx):PlaySound "fx_PlasticHit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aGates_Hit(idx):PlaySound "fx_Gate", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aWoods_Hit(idx):PlaySound "fx_Woodhit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub

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



'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub


'**********************************************************************

' *********************************************************************

'***********





Sub Lane1_Hit
    PlaySound "sensor"
    AddScore 10
End Sub

Sub Lane2_Hit
    PlaySound "sensor"
    AddScore 10
End Sub

'***************
'  Slingshots
'***************


'*********************
'  Targets & switches
'*********************

' Flipper Lanes

Sub sw9_Hit ' left outlane
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
        
        'give extra ball if light is lit
        If l9.State = 0 Then
        
            l9.State = 1
        End If
End Sub


Sub sw8_Hit ' right outlane
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
        
        'give extra game is light is lit
        If l8.State = 0 Then
        l8.State = 1
        End If
End Sub

Sub sw7_Hit 
        AddScore 500

        
        'give extra ball if light is lit
        If l7.State = 0 Then
        
            l7.State = 1
        End If
End Sub

Sub sw4_Hit ' right inlane
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
End Sub

' Top Lanes

Sub sw6_Hit
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
        
        If l6.State = 0 Then
          l6.State = 1
           
        End If
End Sub



' Triggers on top of the red lights









' Droptargets



Sub ResetDroptargets
    PlaySound "flip_hit_3"
    dt1 = 0
    dt2 = 0
    dt3 = 0
    dt4 = 0
    dt5 = 0
'    drop1.IsDropped = 0
'    drop2.IsDropped = 0
'    drop3.IsDropped = 0
'    drop4.IsDropped = 0
'    drop5.IsDropped = 0
End Sub

Sub CheckDroptargets
    If(dt1 + dt2 + dt3 + dt4 + dt5) = 5 Then
        dtdropped = 1
        ResetTargetsDelay = 30
    End If
    'turn on the Special and gives 5000 points but only once per ball
    If Special = False AND dtdropped AND(l16.State + l14.State + l15.State) = 0 Then
        Addscore 5000
        Special = True
        l16.State = 1
        l14.State = 1
    End If
End Sub

Sub drop1_Hit
        PlaySound "fx_droptarget", 0, 1, -0.05, 0.05
        drop1.IsDropped = 1
        dt1 = 1
        AddScore 500
        AddBonus
        CheckDroptargets
End Sub

Sub drop2_Hit
        PlaySound "fx_droptarget", 0, 1, -0.05, 0.05
        drop2.IsDropped = 1
        dt2 = 1
        AddScore 500
        AddBonus
        CheckDroptargets
End Sub

Sub drop3_Hit
        PlaySound "fx_droptarget", 0, 1, -0.05, 0.05
        drop3.IsDropped = 1
        dt3 = 1
        AddScore 500
        AddBonus
        CheckDroptargets
End Sub

Sub drop4_Hit
        PlaySound "fx_droptarget", 0, 1, -0.05, 0.05
        drop4.IsDropped = 1
        dt4 = 1
        AddScore 500
        AddBonus
        CheckDroptargets
End Sub

Sub drop5_Hit
        PlaySound "fx_droptarget", 0, 1, -0.05, 0.05
        drop5.IsDropped = 1
        dt5 = 1
        AddScore 500
        AddBonus
        CheckDroptargets
End Sub

' Holes

'Dim HolePos
'HolePos = 0

Sub Hole_Hit
    PlaySound "fx_kicker_enter", 0, 1, 0.15, 0.05
    HolePos = 0
    HoleScoreTimer.Enabled = 1
End Sub

Sub HoleScoreTimer_Timer
    Select Case HolePos
        Case 0:Addscore 100 + 900 * l33.State
        Case 1:Addscore 100 + 900 * l32.State
        Case 2:Addscore 100 + 900 * l31.State
        Case 3:Addscore 100 + 900 * l30.State
        Case 4:Addscore 100 + 900 * l29.State
        Case 5:PlaySound "fx_kicker", 0, 1, 0.15, 0.05:Hole.Kick 240, 7 + RND(1) * 3:holekicker.IsDropped = 0:holekicker.TimerEnabled = 1:Me.Enabled = 0
    End Select

    HolePos = HolePos + 1
End Sub

Sub holekicker_Timer
    holekicker.IsDropped = 1
    Me.TimerEnabled = 0
End Sub

' 10 point rebounds

Sub aRebounds_Hit(idx):PlaySound "fx_rubber", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:AddScore 10:End Sub

' Spinner

Sub Spinner_Spin
    AddScore 10 + 900 * l28.State
    TopLightsPos = TopLightsPos + 1:UpdateTopLights
End Sub






sub savehs
    savevalue "Highscore", "hiscore", hisc
End Sub

sub loadhs
    dim temp
    If (temp <> "") then hisc = CDbl(temp)
    temp = LoadValue("Highscore", "match")
End Sub
























	LoadCoreFiles
	Sub LoadCoreFiles
		On Error Resume Next
		ExecuteGlobal GetTextFile("core.vbs")
		If Err Then MsgBox "Can't open core.vbs"
		On Error Goto 0
	End Sub

	Dim EnableBallControl
	EnableBallControl = false 'Change to true to enable manual ball control (or press C in-game) via the arrow keys and B (boost movement) keys


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'

Const directory = "HKEY_CURRENT_USER\SOFTWARE\Visual Pinball\Controller\"
Dim objShell
Dim PopupMessage
Dim B2SController
Const DOFContactors = 1
Const DOFKnocker = 2
Const DOFChimes = 3
Const DOFBell = 4
Const DOFGear = 5
Const DOFShaker = 6
Const DOFFlippers = 7
Const DOFTargets = 8
Const DOFDropTargets = 9
Const DOFOff = 0
Const DOFOn = 1
Const DOFPulse = 2

Dim DOFeffects(9)
Dim B2SOn
Dim B2SOnALT

Sub LoadEM
	LoadController("EM")
End Sub

Sub LoadPROC(VPMver, VBSfile, VBSver)
	LoadVBSFiles VPMver, VBSfile, VBSver
	LoadController("PROC")
End Sub

Sub LoadVPM(VPMver, VBSfile, VBSver)
	LoadVBSFiles VPMver, VBSfile, VBSver
	LoadController("VPM")
End Sub

Sub LoadVPMALT(VPMver, VBSfile, VBSver)
	LoadVBSFiles VPMver, VBSfile, VBSver
	LoadController("VPMALT")
End Sub

Sub LoadVBSFiles(VPMver, VBSfile, VBSver)
	On Error Resume Next
	If ScriptEngineMajorVersion < 5 Then MsgBox "VB Script Engine 5.0 or higher required"
	ExecuteGlobal GetTextFile(VBSfile)
	If Err Then MsgBox "Unable to open " & VBSfile & ". Ensure that it is in the same folder as this table. " & vbNewLine & Err.Description	
	InitializeOptions
End Sub

Sub LoadVPinMAME
	Set Controller = CreateObject("VPinMAME.Controller")
	If Err Then MsgBox "Can't load VPinMAME." & vbNewLine & Err.Description
	If VPMver > "" Then If Controller.Version < VPMver Or Err Then MsgBox "VPinMAME ver " & VPMver & " required."
	If VPinMAMEDriverVer < VBSver Or Err Then MsgBox VBSFile & " ver " & VBSver & " or higher required."
	On Error Goto 0
End Sub

Sub LoadController(TableType)
	Dim FileObj
	Dim DOFConfig
	Dim TextStr2
	Dim tempC
	Dim count
	Dim ISDOF
	Dim Answer
	
	B2SOn = False
	B2SOnALT = False
	tempC = 0
	on error resume next
	Set objShell = CreateObject("WScript.Shell")
	objShell.RegRead(directory & "ForceDisableB2S")
	If Err.number <> 0 Then
		PopupMessage = "This latest version of Controller.vbs stores its settings in the registry. To adjust the values, you must use VP 10.2 (or newer) and setup your configuration in the DOF section of the -Keys, Nudge and DOF- dialog of Visual Pinball."
		objShell.RegWrite directory & "ForceDisableB2S",0, "REG_DWORD"
		objShell.RegWrite directory & "DOFContactors",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFKnocker",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFChimes",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFBell",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFGear",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFShaker",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFFlippers",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFTargets",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFDropTargets",2, "REG_DWORD"
		MsgBox PopupMessage
	End If
	tempC = objShell.RegRead(directory & "ForceDisableB2S")
	DOFeffects(1)=objShell.RegRead(directory & "DOFContactors")
	DOFeffects(2)=objShell.RegRead(directory & "DOFKnocker")
	DOFeffects(3)=objShell.RegRead(directory & "DOFChimes")
	DOFeffects(4)=objShell.RegRead(directory & "DOFBell")
	DOFeffects(5)=objShell.RegRead(directory & "DOFGear")
	DOFeffects(6)=objShell.RegRead(directory & "DOFShaker")
	DOFeffects(7)=objShell.RegRead(directory & "DOFFlippers")
	DOFeffects(8)=objShell.RegRead(directory & "DOFTargets")
	DOFeffects(9)=objShell.RegRead(directory & "DOFDropTargets")
	Set objShell = nothing

	If TableType = "PROC" or TableType = "VPMALT" Then
		If TableType = "PROC" Then
			Set Controller = CreateObject("VPROC.Controller")
			If Err Then MsgBox "Can't load PROC"
		Else
			LoadVPinMAME
		End If		
		If tempC = 0 Then
			On Error Resume Next
			If Controller is Nothing Then
				Err.Clear
			Else
				Set B2SController = CreateObject("B2S.Server")
				If B2SController is Nothing Then
					Err.Clear
				Else
					B2SController.B2SName = B2ScGameName
					B2SController.Run()
					On Error Goto 0
					B2SOn = True
					B2SOnALT = True
				End If
			End If
		End If
	Else
		If tempC = 0 Then
			On Error Resume Next
			Set Controller = CreateObject("B2S.Server")
			If Controller is Nothing Then
				Err.Clear
				If TableType = "VPM" Then 
					LoadVPinMAME
				End If
			Else
				Controller.B2SName = cGameName
				If TableType = "EM" Then
					Controller.Run()
				End If
				On Error Goto 0
				B2SOn = True
			End If
		Else
			If TableType = "VPM" Then 
				LoadVPinMAME
			End If
		End If
		Set DOFConfig=Nothing
		Set FileObj=Nothing
	End If
End sub

Function SoundFX (Sound, Effect)
	If ((Effect = 0 And B2SOn) Or DOFeffects(Effect)=1) Then
		SoundFX = ""
	Else
		SoundFX = Sound
	End If
End Function

Function SoundFXDOF (Sound, DOFevent, State, Effect)
	If DOFeffects(Effect)=1 Then
		SoundFXDOF = ""
		DOF DOFevent, State
	ElseIf DOFeffects(Effect)=2 Then
		SoundFXDOF = Sound
		DOF DOFevent, State
	Else
		SoundFXDOF = Sound
	End If
End Function

Function SoundFXDOFALT (Sound, DOFevent, State, Effect)
	If DOFeffects(Effect)=1 Then
		SoundFXDOFALT = ""
		DOFALT DOFevent, State
	ElseIf DOFeffects(Effect)=2 Then
		SoundFXDOFALT = Sound
		DOFALT DOFevent, State
	Else
		SoundFXDOFALT = Sound
	End If
End Function


Sub DOF(DOFevent, State)
	If B2SOn Then
		If State = 2 Then
			Controller.B2SSetData DOFevent, 1:Controller.B2SSetData DOFevent, 0
		Else
			Controller.B2SSetData DOFevent, State
		End If
	End If
End Sub

Sub DOFALT(DOFevent, State)
	If B2SOnALT Then
		If State = 2 Then
			B2SController.B2SSetData DOFevent, 1:B2SController.B2SSetData DOFevent, 0
		Else
			B2SController.B2SSetData DOFevent, State
		End If
	End If
End Sub




'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   TABLE INITS & MATHS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  




	'********************
	' MATHS
	'********************

	Function RndNum(min,max)
	 RndNum = Int(Rnd()*(max-min+1))+min     ' Sets a random number between min AND max
	End Function


		Dim BIP
		BIP = 0







'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



	


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


	Sub StartControl_Hit()
		 Set ControlBall = ActiveBall
		 contballinplay = true
	End Sub

	Sub StopControl_Hit()
		 contballinplay = false
	End Sub

	Dim bcup, bcdown, bcleft, bcright, contball, contballinplay, ControlBall, bcboost
	Dim bcvel, bcyveloffset, bcboostmulti

	bcboost = 1 'Do Not Change - default setting
	bcvel = 4 'Controls the speed of the ball movement
	bcyveloffset = -0.01 'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
	bcboostmulti = 3 'Boost multiplier to ball veloctiy (toggled with the B key)

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






'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



	'*****************************************
	'*****************************************

	sub FlipperTimer_Timer()
		FlipperLSh.RotZ = LeftFlipper.currentangle
		FlipperRSh.RotZ = RightFlipper.currentangle

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
				BallShadow(b).visible = 1
			Else
				BallShadow(b).visible = 0
			End If
		Next
	End Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



	'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

	Sub CheckTilt                                    'Called when table is nudged
		Tilt = Tilt + TiltSensitivity                'Add to tilt count
		TiltDecreaseTimer.Enabled = True
		If(Tilt> TiltSensitivity) AND(Tilt <15) Then 'show a warning
			pNote "CAREFUL!","TILT WARNING"
				'PlaySound "buzz"
			PuPlayer.playlistplayex pBackglass,"videotilt","",100,1
			DOF 131, DOFPulse
			DOF 311, DOFPulse  'DOF MX - Tilt Warning
		End if
		If Tilt> 15 Then 'If more that 15 then TILT the table
			Tilted = True
			pNote "TILT",""
				'PlaySound "powerdownn"
			PuPlayer.playlistplayex pBackglass,"videotilt","",100,4
			DOF 310, DOFPulse   'DOF MX - TILT
			DOF 127, DOFOff   'DOF - Beacon - OFF
			DisableTable True
			tilttableclear.enabled = true
			TiltRecoveryTimer.Enabled = True 'start the Tilt delay
		End If
	End Sub

	Dim tilttime:tilttime = 0

	sub tilttableclear_timer
		tilttime = tilttime + 1
		Select Case tilttime
			Case 10
				tableclearing
		End Select
	End Sub

	Sub tableclearing

	End Sub

	Sub posttiltreset

	End Sub

	Sub TiltDecreaseTimer_Timer
		' DecreaseTilt
		If Tilt> 0 Then
			Tilt = Tilt - 0.1
		Else
			TiltDecreaseTimer.Enabled = False
		End If
	End Sub

	Sub DisableTable(Enabled)
		If Enabled Then
			GiOff
			LightSeqTilt.Play SeqAllOff
			LeftFlipper.RotateToStart
			RightFlipper.RotateToStart
			LeftSlingshot.Disabled = 1
			RightSlingshot.Disabled = 1
			PuPlayer.playresume 4
			PuPlayer.playlistplayex pAudio,"audiomodes","clear.mp3",100,1
			PuPlayer.playlistplayex pMusic,"audioclear","clear.mp3",100, 1
		Else
			GiOn
			LightSeqTilt.StopPlay
			LeftSlingshot.Disabled = 0
			RightSlingshot.Disabled = 0
		End If
	End Sub

	Sub TiltRecoveryTimer_Timer()
		If(BallsOnPlayfield = 0) Then
			EndOfBall()
			TiltRecoveryTimer.Enabled = False
		End If
	End Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX 
	'*********************************************************************
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

	Function AudioFade(tableobj) ' Fades 
		Dim tmp
		tmp = tableobj.y * 2 / table1.height-1
		If tmp > 0 Then
			AudioFade = Csng(tmp ^10)
		Else
			AudioFade = Csng(-((- tmp) ^10) )
		End If
	End Function

	Function AudioPan(tableobj) ' Calculates 
		Dim tmp
		tmp = tableobj.x * 2 / table1.width-1
		If tmp > 0 Then
			AudioPan = Csng(tmp ^10)
		Else
			AudioPan = Csng(-((- tmp) ^10) )
		End If
	End Function

	Function Vol(ball) ' Calculates the
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

'	Const tnob = 5 ' total number of balls
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

	'**************************************
	' Explanation of the collision routine
	'**************************************

	'  collision.


	Sub Pins_Hit (idx)
		PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
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




'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

	Sub ResetForNewGame()
		Dim i
		bGameInPLay = True
		StopAttractMode
		GiOn
		TotalGamesPlayed = TotalGamesPlayed + 1
		savegp
		CurrentPlayer = 1
		PlayersPlayingGame = 1
		bOnTheFirstBall = True
		For i = 1 To MaxPlayers
'			Score(i) = 0
			BonusPoints(i) = 0
			BonusHeldPoints(i) = 0
			BonusMultiplier(i) = 1
			BallsRemaining(i) = 3
			ExtraBallsAwards(i) = 0
		Next
		Tilt = 0
		Game_Init()
		vpmtimer.addtimer 1500, "FirstBall '"



        
           

	End Sub









'   Score
'**************

Sub AddScore(Points)
    Select Case Points
        Case 10, 100, 1000
            Score = Score + Points
            ScoreReel2.AddValue Points
            If Points = 100 AND(Score MOD 1000) \ 100 = 0 Then  'New 1000 reel
                PlaySound "fx_bell1000"
            ElseIf Points = 10 AND(Score MOD 100) \ 10 = 0 Then 'New 100 reel
                PlaySound "fx_bell100"
            Else
                PlaySound "fx_bell" &Points
            End If
        Case 50
            Add10 = Add10 + 5	
            AddScore10Timer.Enabled = TRUE
        Case 500
            Add100 = Add100 + 5	
            AddScore100Timer.Enabled = TRUE
        Case 2000, 3000, 4000, 5000
            Add1000 = Add1000 + Points \ 1000 
            AddScore1000Timer.Enabled = TRUE
    End Select

    ' check replays
    If Score >= Special1 AND Special1Awarded = False Then
        ExtraGame
        Special1Awarded = True
    End If
    If Score >= Special2 AND Special2Awarded = False Then
        ExtraGame
        Special2Awarded = True
    End If
    If Score >= Special3 AND Special3Awarded = False Then
        ExtraGame
        Special3Awarded = True
    End If
End Sub

'******************************












	Sub EndOfGame()
		pNote "GAME OVER","PLAY AGAIN"
		StartAttractMode
		introposition = 0
		bGameInPLay = False
		bJustStarted = False
		Dim i
		GiOff
	End Sub





	Dim UltraDMD

	' DMD using UltraDMD calls



		Dim fso:Set fso = CreateObject("Scripting.FileSystemObject")
		Dim curDir:curDir = fso.GetAbsolutePathName(".")

		Dim DirName






'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

'**************************

	Dim HasPup:HasPuP = True

	Dim PuPlayer

	Const pTopper=0
	Const pDMD=1
	Const pBackglass=2
	Const pPlayfield=3
	Const pMusic=4
	Const pAudio=7
	Const pCallouts=8






	Sub chilloutthemusic

		vpmtimer.addtimer 2200, "turnitbackup'"
	End Sub

	Sub turnitbackup

	End Sub




	If toppervideo = 1 Then
 	PuPlayer.playlistadd pTopper,"topper", 1 , 0
	End If





	If toppervideo = 1 Then

	End If





	Sub resetbackglass
	Loadhs



	End Sub


	Dim titlepos
	titlepos = 0
	titletimer.enabled = 0
	dim title
	title = ""
	dim subtitle
	subtitle = ""

	Sub titletimer_timer
		titlepos = titlepos + 1
		Select Case titlepos
			Case 1
'				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':2565927, 'size': 0, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 0, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':2565927, 'size': 0, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 0, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 2
'				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':5066061, 'size': 0.6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 0.6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':5066061, 'size': 0.4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 0.4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 3
'				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':7960953, 'size': 1.2, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 1.2, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':7960953, 'size': 0.8, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 0.8, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 4
'				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':9671571, 'size': 1.8, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 1.8, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':9671571, 'size': 1.2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 1.2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 5
'				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':11842740, 'size': 2.4, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 2.4, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':11842740, 'size': 1.6, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 1.6, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 6
'				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':13224393, 'size': 3, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 3, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':13224393, 'size': 2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 7
'				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':14671839, 'size': 3.6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 3.6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':14671839, 'size': 2.4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 2.4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 8
'				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':15790320, 'size': 4.2, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 4.2, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':15790320, 'size': 2.8, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 2.8, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 9
'				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':16316664, 'size': 4.8, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 4.8, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':16316664, 'size': 3.2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 3.2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 10
'				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':16777215, 'size': 5.4, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 5.4, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':16777215, 'size': 3.6, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 3.6, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 11

			Case 150

				titlepos = 0
				titletimer.enabled = 0
		End Select
	End Sub

	
	Sub pNote(msgText,msg2text)
		title = msgText
		subtitle = msg2text
		If titlepos = 0 Then
			titletimer.enabled = 1
		Else
			titlepos = 0
			titletimer.enabled = 1
'			PuPlayer.LabelSet pBackglass,"title","",1,"{'mt':2,'color':16777215, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
'			PuPlayer.LabelSet pBackglass,"titlebg","",1,"{'mt':2,'color':0, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
'			PuPlayer.LabelSet pBackglass,"title2","",1,"{'mt':2,'color':16777215, 'size': 4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
'			PuPlayer.LabelSet pBackglass,"titlebg2","",1,"{'mt':2,'color':0, 'size': 4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
		End If
	End Sub




	Sub currentplayerbackglass

	End Sub


	Sub pUpdateScores
'		PuPlayer.LabelSet pBackglass,"curscore",FormatNumber(Score(CurrentPlayer),0),1,""
'		PuPlayer.LabelSet pBackglass,"curplayer","Player " & CurrentPlayer,1,""
		If CurrentPlayer = 1 Then
'			PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(CurrentPlayer),0),1,"{'mt':2,'color':16777215, 'size': 2.4 }"
'			PuPlayer.LabelSet pBackglass,"Play1","Player 1",1,"{'mt':2,'color':16777215 }"
			'make other scores red (inactive)
			If PlayersPlayingGame = 2 Then
'				PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':8421504, 'size': 2.4 }"
'				PuPlayer.LabelSet pBackglass,"Play2","Player 2",1,"{'mt':2,'color':8421504}"
			End If
			If PlayersPlayingGame = 3 Then
'				PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':8421504, 'size': 2.4 }"
'				PuPlayer.LabelSet pBackglass,"Play2","Player 2",1,"{'mt':2,'color':8421504}"
'				PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':8421504, 'size': 2.4 }"
'				PuPlayer.LabelSet pBackglass,"Play3","Player 3",1,"{'mt':2,'color':8421504}"
			End If
			If PlayersPlayingGame = 4 Then
'				PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':8421504, 'size': 2.4 }"
'				PuPlayer.LabelSet pBackglass,"Play2","Player 2",1,"{'mt':2,'color':8421504}"
'				PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':8421504, 'size': 2.4 }"
'				PuPlayer.LabelSet pBackglass,"Play3","Player 3",1,"{'mt':2,'color':8421504}"
'				PuPlayer.LabelSet pBackglass,"Play4score","" & FormatNumber(Score(4),0),1,"{'mt':2,'color':8421504, 'size': 2.4 }"
'				PuPlayer.LabelSet pBackglass,"Play4","Player 4",1,"{'mt':2,'color':8421504}"
			End If
		End If
		If CurrentPlayer = 2 Then
'			PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(Score(CurrentPlayer),0),1,"{'mt':2,'color':16777215, 'size': 2.4 }"
'			PuPlayer.LabelSet pBackglass,"Play2","Player 2",1,"{'mt':2,'color':16777215 }"
			If PlayersPlayingGame = 2 Then
'				PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':8421504, 'size': 2.4 }"
'				PuPlayer.LabelSet pBackglass,"Play1","Player 1",1,"{'mt':2,'color':8421504}"
			End If
			If PlayersPlayingGame = 3 Then
'				PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':8421504, 'size': 2.4 }"
'				PuPlayer.LabelSet pBackglass,"Play1","Player 1",1,"{'mt':2,'color':8421504}"
'				PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':8421504, 'size': 2.4 }"
'				PuPlayer.LabelSet pBackglass,"Play3","Player 3",1,"{'mt':2,'color':8421504}"
			End If
			If PlayersPlayingGame = 4 Then
'				PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':8421504, 'size': 2.4 }"
'				PuPlayer.LabelSet pBackglass,"Play1","Player 1",1,"{'mt':2,'color':8421504}"
'				PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':8421504, 'size': 2.4 }"
'				PuPlayer.LabelSet pBackglass,"Play3","Player 3",1,"{'mt':2,'color':8421504}"
'				PuPlayer.LabelSet pBackglass,"Play4score","" & FormatNumber(Score(4),0),1,"{'mt':2,'color':8421504, 'size': 2.4 }"
'				PuPlayer.LabelSet pBackglass,"Play4","Player 4",1,"{'mt':2,'color':8421504}"
			End If
		End If
		If CurrentPlayer = 3 Then
'			PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(Score(CurrentPlayer),0),1,"{'mt':2,'color':16777215, 'size': 2.4 }"
'			PuPlayer.LabelSet pBackglass,"Play3","Player 3",1,"{'mt':2,'color':16777215 }"
			If PlayersPlayingGame = 3 Then
'				PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':8421504, 'size': 2.4 }"
'				PuPlayer.LabelSet pBackglass,"Play1","Player 1",1,"{'mt':2,'color':8421504}"
'				PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':8421504, 'size': 2.4 }"
'				PuPlayer.LabelSet pBackglass,"Play2","Player 2",1,"{'mt':2,'color':8421504}"
			End If
			If PlayersPlayingGame = 4 Then
'				PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':8421504, 'size': 2.4 }"
'				PuPlayer.LabelSet pBackglass,"Play1","Player 1",1,"{'mt':2,'color':8421504}"
'				PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':8421504, 'size': 2.4 }"
'				PuPlayer.LabelSet pBackglass,"Play2","Player 2",1,"{'mt':2,'color':8421504}"
'				PuPlayer.LabelSet pBackglass,"Play4score","" & FormatNumber(Score(4),0),1,"{'mt':2,'color':8421504, 'size': 2.4 }"
'				PuPlayer.LabelSet pBackglass,"Play4","Player 4",1,"{'mt':2,'color':8421504}"
			End If
		End If
		If CurrentPlayer = 4 Then
'			PuPlayer.LabelSet pBackglass,"Play4score","" & FormatNumber(Score(CurrentPlayer),0),1,"{'mt':2,'color':16777215, 'size': 2.4 }"
'			PuPlayer.LabelSet pBackglass,"Play4","Player 4",1,"{'mt':2,'color':16777215 }"
			If PlayersPlayingGame = 4 Then
'				PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':8421504, 'size': 2.4 }"
'				PuPlayer.LabelSet pBackglass,"Play1","Player 1",1,"{'mt':2,'color':8421504}"
'				PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':8421504, 'size': 2.4 }"
'				PuPlayer.LabelSet pBackglass,"Play2","Player 2",1,"{'mt':2,'color':8421504}"
'				PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':8421504, 'size': 2.4 }"
'				PuPlayer.LabelSet pBackglass,"Play3","Player 3",1,"{'mt':2,'color':8421504}"
			End If
		End If

	end Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  High Scores
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

	' load em up


	Dim hschecker:hschecker = 0

	Sub Loadhs
		Dim x
		x = LoadValue(TableName, "HighScore1")
		If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 1300000 End If

		x = LoadValue(TableName, "HighScore1Name")
		If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "SBW" End If

		x = LoadValue(TableName, "HighScore2")
		If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 1200000 End If

		x = LoadValue(TableName, "HighScore2Name")
		If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "JPS" End If

		x = LoadValue(TableName, "HighScore3")
		If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 1100000 End If

		x = LoadValue(TableName, "HighScore3Name")
		If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "011" End If

		x = LoadValue(TableName, "HighScore4")
		If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 1000000 End If

		x = LoadValue(TableName, "Credits")
		If(x <> "") then Credits = CInt(x) Else Credits = 0 End If

		x = LoadValue(TableName, "TotalGamesPlayed")
		If(x <> "") then TotalGamesPlayed = CInt(x) Else TotalGamesPlayed = 0 End If

		If hschecker = 0 Then
		checkorder
		End If
	End Sub

	Dim hs3,hs2,hs1,hs0,hsn3,hsn2,hsn1,hsn0


	Sub checkorder
		hschecker = 1
		hs3 = HighScore(3)
		hs2 = HighScore(2)
		hs1 = HighScore(1)
		hs0 = HighScore(0)
		hsn3 = HighScoreName(3)
		hsn2 = HighScoreName(2)
		hsn1 = HighScoreName(1)
		hsn0 = HighScoreName(0)
		If hs3 > hs0 Then
			HighScore(0) = hs3
			HighScoreName(0) = hsn3	
			HighScore(1) = hs0
			HighScoreName(1) = hsn0	
			HighScore(2) = hs1
			HighScoreName(2) = hsn1	
			HighScore(3) = hs2
			HighScoreName(3) = hsn2

		ElseIf hs3 > hs1 Then
			HighScore(0) = hs0
			HighScoreName(0) = hsn0	
			HighScore(1) = hs3
			HighScoreName(1) = hsn3	
			HighScore(2) = hs1
			HighScoreName(2) = hsn1	
			HighScore(3) = hs2
			HighScoreName(3) = hsn2
		ElseIf hs3 > hs2 Then
			HighScore(0) = hs0
			HighScoreName(0) = hsn0	
			HighScore(1) = hs1
			HighScoreName(1) = hsn1	
			HighScore(2) = hs3
			HighScoreName(2) = hsn3	
			HighScore(3) = hs2
			HighScoreName(3) = hsn2
		ElseIf hs3 < hs2 Then
			HighScore(0) = hs0
			HighScoreName(0) = hsn0	
			HighScore(1) = hs1
			HighScoreName(1) = hsn1	
			HighScore(2) = hs2
			HighScoreName(2) = hsn2	
			HighScore(3) = hs3
			HighScoreName(3) = hsn3
		End If

		savehs
	End Sub


	Sub Savehs
		SaveValue TableName, "HighScore1", HighScore(0)
		SaveValue TableName, "HighScore1Name", HighScoreName(0)
		SaveValue TableName, "HighScore2", HighScore(1)
		SaveValue TableName, "HighScore2Name", HighScoreName(1)
		SaveValue TableName, "HighScore3", HighScore(2)
		SaveValue TableName, "HighScore3Name", HighScoreName(2)
		SaveValue TableName, "HighScore4", HighScore(3)
		SaveValue TableName, "HighScore4Name", HighScoreName(3)
	End Sub


	Sub Savegp
		SaveValue TableName, "TotalGamesPlayed", TotalGamesPlayed
		vpmtimer.addtimer 1000, "Loadhs'"
	End Sub


	' Initials

	Dim hsbModeActive:hsbModeActive = False
	Dim hsEnteredName
	Dim hsEnteredDigits(3)
	Dim hsCurrentDigit
	Dim hsValidLetters
	Dim hsCurrentLetter
	Dim hsLetterFlash

	' Check the scores to see if you got one

	Sub CheckHighscore()
		Dim tmp
		tmp = Score(CurrentPlayer)

		If tmp > HighScore(3) Then
			AwardSpecial
			vpmtimer.addtimer 2000, "PlaySound ""vo_contratulationsgreatscore"" '"
			HighScore(3) = tmp
			'enter player's name
			HighScoreEntryInit()
			DOF 403, DOFPulse   'DOF MX - Hi Score
		Else
			EndOfBallComplete
		End If
	End Sub








	Sub HighScoreCommitName()
		checkorder
		EndOfBallComplete()
	End Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



	Sub StartAttractMode()
		DOF 323, DOFOn   'DOF MX - Attract Mode ON
		bAttractMode = True
		
		StartLightSeq
		'ShowTableInfo
	
		StartRainbow alights
		StartRainbow2 GI

		intromover.enabled = true
		ruleshelperoff
	End Sub

	Sub StopAttractMode()
		DOF 323, DOFOff   'DOF MX - Attract Mode Off
		bAttractMode = False
		
		LightSeqAttract.StopPlay
		LightSeqAttract2.StopPlay
		StopRainbow alights
		StopRainbow2 GI
		ResetAllLightsColor
		DMDattract.Enabled = 0
		intromover.enabled = false
		
	'StopSong
	End Sub







	Dim ldown:ldown = 0
	Dim rdown:rdown = 0

	Sub checkdown
		If ldown + rdown = 2 Then
			skipscene
		End If
	End Sub

	Sub skipscene

	End Sub






'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



	'********************************************************************************************

	Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState) 'thanks gtxjoe for the first version

		If TypeName(MyLight) = "Light" Then

			If FinalState = 2 Then
				FinalState = MyLight.State 'Keep the current light state
			End If
			MyLight.BlinkInterval = BlinkPeriod
			MyLight.Duration 2, TotalPeriod, FinalState
		ElseIf TypeName(MyLight) = "Flasher" Then

			Dim steps

			' Store all blink information
			steps = Int(TotalPeriod / BlinkPeriod + .5) 'Number of ON/OFF steps to perform
			If FinalState = 2 Then                      'Keep the current flasher state
				FinalState = ABS(MyLight.Visible)
			End If
			MyLight.UserValue = steps * 10 + FinalState 'Store # of blinks, and final state

			' Start blink timer and create timer subroutine
			MyLight.TimerInterval = BlinkPeriod
			MyLight.TimerEnabled = 0
			MyLight.TimerEnabled = 1
			ExecuteGlobal "Sub " & MyLight.Name & "_Timer:" & "Dim tmp, steps, fstate:tmp=me.UserValue:fstate = tmp MOD 10:steps= tmp\10 -1:Me.Visible = steps MOD 2:me.UserValue = steps *10 + fstate:If Steps = 0 then Me.Visible = fstate:Me.TimerEnabled=0:End if:End Sub"
		End If
	End Sub

	'******************************************
	' Change light color - simulate color leds
	' changes the light color and state
	' 10 colors: red, orange, amber, yellow...
	'******************************************
	' in this table this colors are use to keep track of the progress during the acts and battles

	'colors
	Dim red, orange, amber, yellow, darkgreen, green, blue, darkblue, purple, white, base

	red = 10
	orange = 9
	amber = 8
	yellow = 7
	darkgreen = 6
	green = 5
	blue = 4
	darkblue = 3
	purple = 2
	white = 1
	base = 11

	Sub SetLightColor(n, col, stat)
		Select Case col
			Case red
				n.color = RGB(18, 0, 0)
				n.colorfull = RGB(255, 0, 0)
			Case orange
				n.color = RGB(18, 3, 0)
				n.colorfull = RGB(255, 64, 0)
			Case amber
				n.color = RGB(193, 49, 0)
				n.colorfull = RGB(255, 153, 0)
			Case yellow
				n.color = RGB(18, 18, 0)
				n.colorfull = RGB(255, 255, 0)
			Case darkgreen
				n.color = RGB(0, 8, 0)
				n.colorfull = RGB(0, 64, 0)
			Case green
				n.color = RGB(0, 18, 0)
				n.colorfull = RGB(0, 255, 0)
			Case blue
				n.color = RGB(0, 18, 18)
				n.colorfull = RGB(0, 255, 255)
			Case darkblue
				n.color = RGB(0, 8, 8)
				n.colorfull = RGB(0, 64, 64)
			Case purple
				n.color = RGB(128, 0, 128)
				n.colorfull = RGB(255, 0, 255)
			Case white
				n.color = RGB(255, 252, 224)
				n.colorfull = RGB(193, 91, 0)
			Case white
				n.color = RGB(255, 252, 224)
				n.colorfull = RGB(193, 91, 0)
			Case base
				n.color = RGB(255, 197, 143)
				n.colorfull = RGB(255, 255, 236)
		End Select
		If stat <> -1 Then
			n.State = 0
			n.State = stat
		End If
	End Sub

	Sub ResetAllLightsColor ' Called at a new game
		SetLightColor l13, red, -1		
	End Sub

	Sub UpdateBonusColors
	End Sub

	'*************************
	' Rainbow Changing Lights
	'*************************

	Dim RGBStep, RGBFactor, rRed, rGreen, rBlue, RainbowLights

	Sub StartRainbow(n)
		set RainbowLights = n
		RGBStep = 0
		RGBFactor = 5
		rRed = 255
		rGreen = 0
		rBlue = 0
		RainbowTimer.Enabled = 1
	End Sub

	Dim RGBStep2, RGBFactor2, rRed2, rGreen2, rBlue2, RainbowLights2
	Sub StartRainbow2(n)
		set RainbowLights2 = n
		RGBStep2 = 0
		RGBFactor2 = 5
		rRed2 = 255
		rGreen2 = 0
		rBlue2 = 0
		RainbowTimer1.Enabled = 1
	End Sub

	Sub StopRainbow(n)
		Dim obj
		RainbowTimer.Enabled = 0
		RainbowTimer.Enabled = 0
			For each obj in RainbowLights
				SetLightColor obj, "white", 0
			Next
	End Sub

	Sub StopRainbow2(n)
		Dim obj
		RainbowTimer1.Enabled = 0
			For each obj in RainbowLights2
				SetLightColor obj, "white", 0
				obj.state = 1
				obj.Intensity = 15
			Next
	End Sub

	Sub RainbowTimer_Timer 'rainbow led light color changing
		Dim obj
		Select Case RGBStep
			Case 0 'Green
				rGreen = rGreen + RGBFactor
				If rGreen > 255 then
					rGreen = 255
					RGBStep = 1
				End If
			Case 1 'Red
				rRed = rRed - RGBFactor
				If rRed < 0 then
					rRed = 0
					RGBStep = 2
				End If
			Case 2 'Blue
				rBlue = rBlue + RGBFactor
				If rBlue > 255 then
					rBlue = 255
					RGBStep = 3
				End If
			Case 3 'Green
				rGreen = rGreen - RGBFactor
				If rGreen < 0 then
					rGreen = 0
					RGBStep = 4
				End If
			Case 4 'Red
				rRed = rRed + RGBFactor
				If rRed > 255 then
					rRed = 255
					RGBStep = 5
				End If
			Case 5 'Blue
				rBlue = rBlue - RGBFactor
				If rBlue < 0 then
					rBlue = 0
					RGBStep = 0
				End If
		End Select
			For each obj in RainbowLights
				obj.color = RGB(rRed \ 10, rGreen \ 10, rBlue \ 10)
				obj.colorfull = RGB(rRed, rGreen, rBlue)
			Next
	End Sub

	Sub RainbowTimer1_Timer 'rainbow led light color changing
		Dim obj
		Select Case RGBStep2
			Case 0 'Green
				rGreen2 = rGreen2 + RGBFactor2
				If rGreen2 > 255 then
					rGreen2 = 255
					RGBStep2 = 1
				End If
			Case 1 'Red
				rRed2 = rRed2 - RGBFactor2
				If rRed2 < 0 then
					rRed2 = 0
					RGBStep2 = 2
				End If
			Case 2 'Blue
				rBlue2 = rBlue2 + RGBFactor2
				If rBlue2 > 255 then
					rBlue2 = 255
					RGBStep2 = 3
				End If
			Case 3 'Green
				rGreen2 = rGreen2 - RGBFactor2
				If rGreen2 < 0 then
					rGreen2 = 0
					RGBStep2 = 4
				End If
			Case 4 'Red
				rRed2 = rRed2 + RGBFactor2
				If rRed2 > 255 then
					rRed2 = 255
					RGBStep2 = 5
				End If
			Case 5 'Blue
				rBlue2 = rBlue2 - RGBFactor2
				If rBlue2 < 0 then
					rBlue2 = 0
					RGBStep2 = 0
				End If
		End Select
			For each obj in RainbowLights2
				obj.color = RGB(rRed2 \ 10, rGreen2 \ 10, rBlue2 \ 10)
				obj.colorfull = RGB(rRed2, rGreen2, rBlue2)
			Next
	End Sub


	Sub StartLightSeq()
		Dim a
		For each a in alights
			a.State = 1
		Next
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 50, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqCircleOutOn, 15, 2
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 10
		LightSeqAttract.Play SeqCircleOutOn, 15, 3
		LightSeqAttract.UpdateInterval = 5
		LightSeqAttract.Play SeqRightOn, 50, 1
		LightSeqAttract.UpdateInterval = 5
		LightSeqAttract.Play SeqLeftOn, 50, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 50, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 50, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 40, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 40, 1
		LightSeqAttract.UpdateInterval = 10
		LightSeqAttract.Play SeqRightOn, 30, 1
		LightSeqAttract.UpdateInterval = 10
		LightSeqAttract.Play SeqLeftOn, 30, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 15, 1
		LightSeqAttract.UpdateInterval = 10
		LightSeqAttract.Play SeqCircleOutOn, 15, 3
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 5
		LightSeqAttract.Play SeqStripe1VertOn, 50, 2
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqCircleOutOn, 15, 2
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqStripe1VertOn, 50, 3
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqCircleOutOn, 15, 2
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqStripe2VertOn, 50, 3
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqStripe1VertOn, 25, 3
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqStripe2VertOn, 25, 3
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 15, 1
		For each a in GI
			a.State = 1
			a.Intensity = 80
		Next
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqUpOn, 50, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqDownOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqCircleOutOn, 15, 2
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqUpOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqDownOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqUpOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqDownOn, 25, 1
		LightSeqAttract2.UpdateInterval = 10
		LightSeqAttract2.Play SeqCircleOutOn, 15, 3
		LightSeqAttract2.UpdateInterval = 5
		LightSeqAttract2.Play SeqRightOn, 50, 1
		LightSeqAttract2.UpdateInterval = 5
		LightSeqAttract2.Play SeqLeftOn, 50, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqRightOn, 50, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 50, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqRightOn, 40, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 40, 1
		LightSeqAttract2.UpdateInterval = 10
		LightSeqAttract2.Play SeqRightOn, 30, 1
		LightSeqAttract2.UpdateInterval = 10
		LightSeqAttract2.Play SeqLeftOn, 30, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqRightOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqRightOn, 15, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 15, 1
		LightSeqAttract2.UpdateInterval = 10
		LightSeqAttract2.Play SeqCircleOutOn, 15, 3
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqRightOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqUpOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqDownOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqUpOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqDownOn, 25, 1
		LightSeqAttract2.UpdateInterval = 5
		LightSeqAttract2.Play SeqStripe1VertOn, 50, 2
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqCircleOutOn, 15, 2
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqStripe1VertOn, 50, 3
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqRightOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqUpOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqDownOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqCircleOutOn, 15, 2
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqStripe2VertOn, 50, 3
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqRightOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqUpOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqDownOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqUpOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqDownOn, 25, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqStripe1VertOn, 25, 3
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqStripe2VertOn, 25, 3
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqUpOn, 15, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqDownOn, 15, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqUpOn, 15, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqDownOn, 15, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqUpOn, 15, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqDownOn, 15, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqRightOn, 15, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 15, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqRightOn, 15, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 15, 1
	End Sub

	Sub LightSeqAttract_PlayDone()
		StartLightSeq()
	End Sub

	Sub LightSeqTilt_PlayDone()
		LightSeqTilt.Play SeqAllOff
	End Sub

	Sub LightSeqSkillshot_PlayDone()
		LightSeqSkillshot.Play SeqAllOff
	End Sub



	'**********************
	'     GI effects
	' independent routine
	' it turns on the gi
	' when there is a ball
	' in play
	'**********************

	Dim OldGiState
	OldGiState = -1   'start witht the Gi off

	Sub ChangeGi(col) 'changes the gi color
		Dim bulb
		For each bulb in GI
			SetLightColor bulb, col, -1
		Next
	End Sub

	Sub GIUpdateTimer_Timer
		Dim tmp, obj
		tmp = Getballs
		If UBound(tmp) <> OldGiState Then
			OldGiState = Ubound(tmp)
			If UBound(tmp) = 3 Then 'we have 4 captive balls on the table (-1 means no balls, 0 is the first ball, 1 is the second..)
				'GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
			Else
				'Gion
			End If
		End If
	End Sub

	Sub GiOn
		DOF 126, DOFOn
		Dim bulb
		For each bulb in GI
			SetLightColor bulb, base, -1
			bulb.State = 1
		Next
	End Sub

	Sub GiOff
		DOF 126, DOFOff
		Dim bulb
		For each bulb in GI
			bulb.State = 0
		Next
	End Sub


	' GI & light sequence effects


	Sub GiEffect(n)
		Select Case n
			Case 0 'all off
				'LightSeqGi.Play SeqAlloff
			Case 1 'all blink
				'LightSeqGi.UpdateInterval = 4
				'LightSeqGi.Play SeqBlinking, , 5, 100
			Case 2 'random
				'LightSeqGi.UpdateInterval = 10
				'LightSeqGi.Play SeqRandom, 5, , 1000
			Case 3 'upon
				'LightSeqGi.UpdateInterval = 4
				'LightSeqGi.Play SeqUpOn, 5, 1
			Case 4 ' left-right-left
				'LightSeqGi.UpdateInterval = 5
				'LightSeqGi.Play SeqLeftOn, 10, 1
				'LightSeqGi.UpdateInterval = 5
				'LightSeqGi.Play SeqRightOn, 10, 1
		End Select
	End Sub

	Sub LightEffect(n)
		Select Case n
			Case 0 ' all off
				'LightSeqInserts.Play SeqAlloff
			Case 1 'all blink
				'LightSeqInserts.UpdateInterval = 4
				'LightSeqInserts.Play SeqBlinking, , 5, 100
			Case 2 'random
				'LightSeqInserts.UpdateInterval = 10
				'LightSeqInserts.Play SeqRandom, 5, , 1000
			Case 3 'upon
				'LightSeqInserts.UpdateInterval = 4
				'LightSeqInserts.Play SeqUpOn, 10, 1
			Case 4 ' left-right-left
				'LightSeqInserts.UpdateInterval = 5
				'LightSeqInserts.Play SeqLeftOn, 10, 1
				'LightSeqInserts.UpdateInterval = 5
				'LightSeqInserts.Play SeqRightOn, 10, 1
			Case 5 'random
				'LightSeqbumper.UpdateInterval = 4
				'LightSeqbumper.Play SeqBlinking, , 5, 10
			Case 6 'random
				'LightSeqRSling.UpdateInterval = 4
				'LightSeqRSling.Play SeqBlinking, , 5, 6
			Case 7 'random
				'LightSeqLSling.UpdateInterval = 4
				'LightSeqLSling.Play SeqBlinking, , 5, 6
			Case 8 'random
				'LightSeqBack.UpdateInterval = 4
				'LightSeqBack.Play SeqBlinking, , 5, 6
			Case 12 'random
				'LightSeqlr.UpdateInterval = 4
				'LightSeqlr.Play SeqBlinking, , 5, 10
		End Select
	End Sub

	' Flasher Effects using lights

	Dim FEStep, FEffect
	FEStep = 0
	FEffect = 0

	Sub FlashEffect(n)
		Select case n
			Case 0 ' all off
				LightSeqFlasher.Play SeqAlloff
			Case 1 'all blink
				LightSeqFlasher.UpdateInterval = 4
				LightSeqFlasher.Play SeqBlinking, , 5, 100
			Case 2 'random
				LightSeqFlasher.UpdateInterval = 10
				LightSeqFlasher.Play SeqRandom, 5, , 1000
			Case 3 'upon
				LightSeqFlasher.UpdateInterval = 4
				LightSeqFlasher.Play SeqUpOn, 10, 1
			Case 4 ' left-right-left
				LightSeqFlasher.UpdateInterval = 5
				LightSeqFlasher.Play SeqLeftOn, 10, 1
				LightSeqFlasher.UpdateInterval = 5
				LightSeqFlasher.Play SeqRightOn, 10, 1
			Case 5 ' top flashers blink fast
		End Select
	End Sub







	'****************************
	' Flashers - Thanks Flupper
	'****************************

	Dim FlashLevel1, FlashLevel2, FlashLevel3, FlashLevel4, FlashLevel5, FlashLevel6
	Flasherlight4.IntensityScale = 0

	'*** right red flasher ***
	Sub Flasherflash3_Timer()
		dim flashx3, matdim
		If not Flasherflash3.TimerEnabled Then 
			Flasherflash3.TimerEnabled = True
			Flasherflash3.visible = 1
			Flasherlit3.visible = 1
		End If
		flashx3 = FlashLevel3 * FlashLevel3 * FlashLevel3
		Flasherflash3.opacity = 1500 * flashx3
		Flasherlit3.BlendDisableLighting = 10 * flashx3
		Flasherbase3.BlendDisableLighting =  flashx3
		Flasherlight4.IntensityScale = flashx3
		matdim = Round(10 * FlashLevel3)
		Flasherlit3.material = "domelit" & matdim
		FlashLevel3 = FlashLevel3 * 0.9 - 0.01
		If FlashLevel3 < 0.15 Then
			Flasherlit3.visible = 0
		Else
			Flasherlit3.visible = 1
		end If
		If FlashLevel3 < 0 Then
			Flasherflash3.TimerEnabled = False
			Flasherflash3.visible = 0
		End If
	End Sub



Sub AddScore(points) 'we also need to Dim Score in the beginning of script. all Variables should go in the beginning of script.
Score = Score + points ' This adds your score + the points in the (Brackets) when something is hit & it contains AddScore(#)
ScoreText.Text = FormatNumber(Score, 0, -1, 0, -1) 'this Displays the points added in scoretext.Text box on the backdrop
End Sub



'	Sub AddScore(points)
'		If(Tilted = False) Then
			' add the points to the current players score variable
'			Score(CurrentPlayer) = Score(CurrentPlayer) + points
'		End if
'	End Sub

	Sub AwardExtraBall()
		If NOT bExtraBallWonThisBall Then
			LightShootAgain.State = 1
			flashflash.Enabled = True
			LightSeqFlasher.UpdateInterval = 150
			LightSeqFlasher.Play SeqRandom, 10, , 10000
			ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
			bExtraBallWonThisBall = True
			DOF 402, DOFPulse   'DOF MX - Extra Ball
			'extraballready(CurrentPlayer) = 0
			If bMultiBallMode = false Then
'			PuPlayer.playlistplayex pBackglass,"videoextraball","extraballsm.mov",100,1
				PuPlayer.playlistplayex pCallouts,"audiocallouts","extraball.wav",100,1
		chilloutthemusic
			End If
		Else
		
		END If
	End Sub

	Sub AwardSpecial()
		Credits = Credits + 1
		DOF 140, DOFOn
		PlaySound SoundFXDOF("knocker",136,DOFPulse,DOFKnocker)		
		DOF 115, DOFPulse
		GiEffect 1
		LightEffect 1
		DOF 400, DOFPulse   'DOF MX - Special
	End Sub


	Sub AwardSkillshot()
		Dim i
		DOF 125, DOFPulse
		ResetSkillShotTimer_Timer
		AddScore SkillshotValue(CurrentPLayer)
		GiEffect 1
		LightEffect 2
		LightEffect 9
		DOF 401, DOFPulse   'DOF MX - Skillshot

		chilloutthemusic
		PuPlayer.playlistplayex pBackglass,"videoskillshot","",100,3
		pNote "SKILLSHOT",SkillShotValue(CurrentPLayer)
		SkillShotValue(CurrentPLayer) = SkillShotValue(CurrentPLayer) + 1000000
	End Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



'	Function Balls
'		Dim tmp
'		tmp = bpgcurrent - BallsRemaining(CurrentPlayer) + 1
'		If tmp> bpgcurrent Then
'			Balls = bpgcurrent
'		Else
'			Balls = tmp
'		End If
'	End Function


	Sub FirstBall
		ResetForNewPlayerBall()
		CreateNewBall()
	End Sub


	Sub ResetForNewPlayerBall()
		If PlayersPlayingGame > 1 Then
			If CurrentPlayer = 1 Then
			Elseif currentplayer = 2 Then
			Elseif currentplayer = 3 Then
			Elseif currentplayer = 4 Then
			End If
		Else
			pNote "BALL " & Balls,"LAUNCH BALL"
		PlaySound "flute"
		AddScore 1
		BonusPoints(CurrentPlayer) = 0
		bBonusHeld = False
		bExtraBallWonThisBall = False
		ResetNewBallLights()
		ResetNewBallVariables
		bBallSaverReady = True
		bSkillShotReady = True
End If

	End Sub





	Sub AddMultiball(nballs)
		mBalls2Eject = mBalls2Eject + nballs
		CreateMultiballTimer.Enabled = True
	End Sub

	Sub CreateMultiballTimer_Timer()
		If bBallInPlungerLane Then
			Exit Sub
		Else
			If BallsOnPlayfield <MaxMultiballs Then
				CreateNewBall()
				mBalls2Eject = mBalls2Eject -1
				If mBalls2Eject = 0 Then 
					Me.Enabled = False
				End If
			Else 
				mBalls2Eject = 0
				Me.Enabled = False
			End If
		End If
	End Sub

	Sub EndOfBall()
'		PuPlayer.playlistplayex pAudio,"audiomultiballs","clear.mp3",100,1


		bMultiBallMode = False
		bOnTheFirstBall = False
		If NOT Tilted Then
			vpmtimer.addtimer 500, "EndOfBall2 '"
		Else 
			vpmtimer.addtimer 500, "EndOfBall2 '"
		End If
	End Sub

	
	Sub EndOfBall2()
		Tilted = False
		Tilt = 0
		DisableTable False
		tilttableclear.enabled = False
		tilttime = 0
		If(ExtraBallsAwards(CurrentPlayer) <> 0) Then
			ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1
			If(ExtraBallsAwards(CurrentPlayer) = 0) Then
				LightShootAgain.State = 0
			End If
			LightSeqFlasher.UpdateInterval = 150
			LightSeqFlasher.Play SeqRandom, 10, , 2000

			CreateNewBall()
			ResetForNewPlayerBall

			pNote "SHOOT AGAIN","SAME PLAYER"

		chilloutthemusic
		Else
			BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1
			If(BallsRemaining(CurrentPlayer) <= 0) Then
'				CheckHighScore()
			Else
				EndOfBallComplete()
			End If
		End If
	End Sub

	Sub EndOfBallComplete()
		ResetNewBallVariables
		Dim NextPlayer
		If(PlayersPlayingGame> 1) Then
			NextPlayer = CurrentPlayer + 1
			If(NextPlayer> PlayersPlayingGame) Then
				NextPlayer = 1
			End If
		Else
			NextPlayer = CurrentPlayer
		End If
		If((BallsRemaining(CurrentPlayer) <= 0) AND(BallsRemaining(NextPlayer) <= 0) ) Then
			EndOfGame()
		Else
			CurrentPlayer = NextPlayer
			AddScore 1
			ResetForNewPlayerBall()
			CreateNewBall()
			If PlayersPlayingGame> 1 Then
				PlaySound "vo_player" &CurrentPlayer
			End If
		End If
	End Sub

	


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' 

	Sub ballsavestarttrigger_hit
		If(bBallSaverReady = True) AND(15 <> 0) And(bBallSaverActive = False) Then
			EnableBallSaver 15
		End If
	End Sub

	Sub swPlungerRest_Hit()
		PlaySoundAt "fx_sensor", ActiveBall
		bBallInPlungerLane = True
		If bAutoPlunger Then
			DOF 114, DOFPulse		
			DOF 115, DOFPulse
			DOF 318, DOFPulse
			bAutoPlunger = False
		End If
		DOF 141, DOFOn
		DOF 317, DOFOn
		If bSkillShotReady Then
			swPlungerRest.TimerEnabled = 1
			UpdateSkillshot()
		End If
		LastSwitchHit = "swPlungerRest"
	End Sub


	Sub swPlungerRest_UnHit()
		bBallInPlungerLane = False
		swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active
		If bSkillShotReady Then
			ResetSkillShotTimer.Enabled = 1
		End If
		DOF 317, DOFOff   'DOF MX - Ball is ready to Launch - OFF
	End Sub


	Sub swPlungerRest_Timer
		'DMD "start-5.wmv", "", "",  6000
		swPlungerRest.TimerEnabled = 0
	End Sub

	Sub EnableBallSaver(seconds)
		bBallSaverActive = True
		bBallSaverReady = False
		BallSaverTimerExpired.Interval = 1000 * seconds
		BallSaverTimerExpired.Enabled = True
		BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
		BallSaverSpeedUpTimer.Enabled = True
		' if you have a ball saver light you might want to turn it on at this point (or make it flash)
		LightShootAgain.BlinkInterval = 160
		LightShootAgain.State = 2
	End Sub

	' The ball saver timer has expired.  Turn it off AND reset the game flag
	'
	Sub BallSaverTimerExpired_Timer()
		'debug.print "Ballsaver ended"
		BallSaverTimerExpired.Enabled = False
		' clear the flag
		Dim waittime
		waittime = 4000
		vpmtimer.addtimer waittime, "ballsavegrace'"
		' if you have a ball saver light then turn it off at this point
		If bExtraBallWonThisBall = True Then
			LightShootAgain.State = 1
		Else
			LightShootAgain.State = 0
		End If
	End Sub

	Sub ballsavegrace
		bBallSaverActive = False
	End Sub

	Sub BallSaverSpeedUpTimer_Timer()
		'debug.print "Ballsaver Speed Up Light"
		BallSaverSpeedUpTimer.Enabled = False
		' Speed up the blinking
		LightShootAgain.BlinkInterval = 80
		LightShootAgain.State = 2
	End Sub

'********************************************************************************************************************************************
'FRAMEWORK BASE CODE END HERE


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


' 


	Dim introposition
	introposition = 0

	


	Dim introtime
	introtime = 0

	Sub intromover_timer
		introtime = introtime + 1
		If introposition = 1 Then
			If introtime = 47 Then
				
			End If
		End If
		If introposition = 2 Then
			If introtime = 13 Then
				
			End If
		End If
		If introposition = 3 Then
			If introtime = 13 Then
				
			End If
		End If
		If introposition = 4 Then
			If introtime = 9 Then
				
			End If
		End If
		If introposition = 0 Then
			If introtime = 9 Then
				introposition = 0
				
			End If
		End If
	End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 


	' Game Variables
	Dim LaneBonus
	Dim TargetBonus
	Dim RampBonus
	Dim OrbitBonus
	Dim spinvalue
	Dim finalflips
	Dim Saves
	Dim Drains
	Dim inmode


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



	Sub startamultiball
		Dim waittime
		waittime = 1000
		vpmtimer.addtimer waittime, "closeupshop'"
	End Sub
	
	Sub closeupshop

	End Sub

	Sub endamultiball

	End Sub


	Sub UpdateSkillShot() 'Updates the skillshot light
		l7.State = 2
	End Sub

	Sub SkillshotOff_Hit 'trigger to stop the skillshot due to a weak plunger shot
		If bSkillShotReady Then

			ResetSkillShotTimer_Timer
		End If
	End Sub

	Sub ResetSkillShotTimer_Timer 'timer to reset the skillshot lights & variables
		ResetSkillShotTimer.Enabled = 0
		bSkillShotReady = False
			If l7.State = 2 then l7.State = 0
	End Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'  RULES HELPER FOR YOUR BACKGLASS





	Sub ruleshelperon
		rulestime.enabled = 1
	End Sub

	Sub ruleshelperoff
		rulestime.enabled = 0
	End Sub

	Dim rulesposition
	rulesposition = 0

	Sub rulestime_timer
		If turnoffrules = 1 then exit sub end if
		rulesposition = rulesposition + 1
		Select Case rulesposition
		Case 1
			
		Case 56
			rulesposition = 0
	End Select
	End Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX




	Sub RightSlingShot1_slingshot
		PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFContactors), RightInlane
	End Sub

	Sub LeftSlingShot1_slingshot
		PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFContactors), LeftInlane
	End Sub

	Sub RightSlingShot_Slingshot
		PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFContactors), RightInlane
		PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
	
		RightSlingShot.TimerEnabled = 1

	End Sub

	Sub RightSlingShot_Timer
	
	End Sub

	Sub LeftSlingShot_Slingshot
		PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFContactors), LeftInlane
		PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
		
		
		LeftSlingShot.TimerEnabled = 1

	End Sub

	Sub LeftSlingShot_Timer
		
	End Sub


	Sub RotateLaneLightsLeft
		Dim TempState
		TempState = l6.State
		l6.State = l7.State
		l7.State = l8.State
		l8.State = l9.State
		l9.State = TempState
	End Sub

	Sub RotateLaneLightsRight
		Dim TempState
		TempState = l9.State
		l9.State = l8.State
		l8.State = l7.State
		l7.State = l6.State
		l6.State = TempState
	End Sub





'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX









'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



	Sub LeftInlane_Hit
        Light13.State = 1:Light17. State = 2
		PlaySoundAt "fx_sensor", ActiveBall
		If bMultiBallMode = False Then
			PlaySound "lane"
			DOF 144, DOFPulse
			DOF 313, DOFPulse   'DOF MX - Left Outer Lane
		End If
		If Tilted Then Exit Sub
		LaneBonus = LaneBonus + 1
		AddScore 50050
		' Do some sound or light effect
		LastSwitchHit = "lane1"
	End Sub







'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' DMD Scores Scripts







'*********************
' Update DMD - reels
'*********************






'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'Ball Save Left Lane

Sub LeftOutLaneTrigger_Hit()
      LeftOutLaneTrigger.kick 0, 40
 LaserKickP1.TransY = 90
End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

 





Sub Trigger3_Hit
      Plunger1.PullBack
     Timer13.Enabled = 1
End Sub

Sub Timer13_Timer()
     Plunger1.Fire
     Timer13.Enabled = 0
End Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' Flashers



sub Trigger4_hit
       Timer14.enabled=True
       	Flasherflash2.visible = 1	
	Flasherflash2.opacity = 500
 
End Sub


Sub Timer14_Timer
'		Flasherflash2.visible = 1	
'	Flasherflash2.opacity = 500
Flasherflash2.visible = 0
Timer14.enabled=False
End Sub


function RandomNumber(ByVal max)
    RandomNumber = Int(max * Rnd+1)
end function



Sub Kicker3_hit()
    Timer15.Enabled = True
  End Sub


Sub Timer15_timer()
Kicker3.Kick 250,15

 Select Case RandomNumber(7)
	Case 1: PlaySound "bonus"
	Case 2: PlaySound "bumpi"
	Case 3: PlaySound "cabo"
   Case 4: PlaySound "ooh"
   Case 5: PlaySound "ooh2"
   Case 6: PlaySound "splat"
   Case 7: PlaySound "spookytune"
	End Select
Timer15.enabled = False
End Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' 




Sub LeftInlane1_Hit
        AddScore (500)
Light15.State = 1:Light11. State = 1
    PlaySound "clickover"
End Sub



Sub LeftInlane2_Hit
        AddScore (500)
Light21.State = 1:Light7. State = 1
    PlaySound "clickover"
End Sub


Sub RightInlane_Hit
        AddScore (500)
Light28.State = 1:Light22. State = 1
    PlaySound "clickover"
End Sub

Sub Trigger5_Hit
        AddScore (500)
Light8.State = 1:Light20. State = 2
End Sub

Sub Trigger6_Hit
        AddScore (500)
Light18.State = 1:Light30. State = 2
End Sub


Sub Trigger7_Hit
        AddScore (500)
Light12.State = 1:Light26. State = 2
End Sub


Sub Trigger8_Hit
        AddScore (500)
Light4.State = 1:Light29. State = 2
End Sub


Sub Trigger9_Hit
        AddScore (500)
Light14.State = 1:Light16. State = 2
End Sub

Sub Trigger10_Hit
        AddScore (500)
Light1.State = 1:Light5. State = 2
End Sub


Sub Trigger11_Hit
        AddScore (500)
Light9.State = 1:Light25. State = 2
End Sub

Sub Trigger12_Hit
        AddScore (500)
Light24.State = 1:Light27. State = 2
End Sub



'**********HOLOGRAMME*****************
'*************************************
dim imagename

Sub IntroAnim()
    HologramAnim1.enabled = True
End Sub

Const HolostepMax = 246  '246
Dim Holostep1:Holostep1 = 1


Sub HologramAnim1_Timer()
     
    HologramAnim1.interval = 220 '30
    Flasher002.visible = 0


    if Holostep1 < 10 then 
        imagename = "alien" & "00"   & Holostep1
    elseif Holostep1 < 246 then
        imagename = "alien" & "0" & Holostep1 
    else 
        imagename = "alien"  & Holostep1 
    end if



    Flasher002.imageA = imagename
    Holostep1 = Holostep1 + 1 

    If Holostep1 > HolostepMax Then
       Holostep1 = 1 :  
   End If
 

DMDmold.image = imagename
 
End Sub







'Sub Timer16_Timer()
'Timer17.Enabled = True
'HologramAnim1.enabled = 0
'End Sub

'Sub Timer17_Timer()
'  HologramAnim1.Enabled = 0
 '  Flasher002.Visible = 0
'End Sub





Sub Table1_Exit()
controller.stop
	If UseFlexDMD Then
		If Not FlexDMD is Nothing Then
			FlexDMD.Show = False
			FlexDMD.Run = False
			FlexDMD = NULL
		End If
	End If
End Sub


































