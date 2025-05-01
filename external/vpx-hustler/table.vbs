' ****************************************************************
'               VISUAL PINBALL X EM Script by JPSalas
'                 Basic EM script up to 4 players
'		         uses core.vbs for extra functions
'                  Hustler - LTD do Brasil 1980
'                      VPX8 version 5.5.0
' ****************************************************************

Option Explicit
Randomize
'
' DOF config - leeoneil
'
' Option for more lights effects with DOF (Undercab effects on bumpers and slingshots) 
' Replace "False" by "True" to activate (False by default)
Const Epileptikdof = False
'
' Flippers L/R - 101/102
' Slingshot L/R - 103/104
' Bumpers L/R - 105/106
' Targets 8 - 107
' Droptargets - 108/109
'
' LED backboard
' Flasher Outside Left - 203/212/213/217/219/240/250
' Flasher left - 207/215/218/220/222/240/250
' Flasher center - 221/222/240/250
' Flasher right - 208/210/216/222/240/250
' Flasher Outside - 205/209/211/214/217/218/223/240/250
' MAX Flasher L/R - 204/206
' Start Button - 200
' Undercab - 201/202
' Strobe - 230
' Knocker - 300

' Standard physic values for the ball
Const BallSize = 50 ' diameter of the ball
Const BallMass = 1  ' ball's mass

' Load some vbs files
LoadCoreFiles

Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    On Error Resume Next
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox "Can't open controller.vbs"
End Sub

' Constants
Const TableName = "hustler"
Const cGameName = "hustler"
Const MaxPlayers = 2    ' de 1 a 4
Const MaxMultiplier = 3 ' limit bonus multipliplier to 3
Const BallsPerGame = 5  ' from 3 ó 5
Const Special1 = 79000  ' highscore to get 1 extra credit
Const Special2 = 120000 ' highscore to get 1 extra credit
Const Special3 = 999999 ' highscore to get 1 extra credit

' Globales Variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim Bonus
Dim BonusMultiplier(4)
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Special1Awarded(4)
Dim Special2Awarded(4)
Dim Special3Awarded(4)
Dim Score(4)
Dim HighScore
Dim Match
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim Add10
Dim Add100
Dim Add1000
Dim Add10000

' Control variables
Dim BallsOnPlayfield

' Boolean variables
Dim bAttractMode
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bExtraBallWonThisBall
Dim bJustStarted
Dim bBallInPlungerLane
Dim bBallSaverActive

Dim x

' core.vbs variables, like magnets, impulse plunger

' *********************************************************************
'                Common rutines for all the tables
' *********************************************************************

Sub Table1_Init()

    ' Initialise objects
    VPObjects_Init
    LoadEM

    ' load saved values, highscore ++
    Credits = 1
    Loadhs
    UpdateCredits

    ' freeplay or coins
    bFreePlay = False

    ' table's global variables
    bAttractMode = False
    bOnTheFirstBall = False
    bGameInPlay = False
    bBallInPlungerLane = False
    BallsOnPlayfield = 0
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bJustStarted = True
    Add10 = 0
    Add100 = 0
    Add1000 = 0

    ' set the table to wait mode
    EndOfGame

    'turn on Gi lights after 1 second
    vpmtimer.addtimer 1000, "GiOn '"

    ' Remove desktop objects if running in FS mode
    If Table1.ShowDT = False then
        lrail.Visible = False
        rrail.Visible = False
        For each x in aReels
            x.Visible = 0
        Next
    End If
    ' Start animations, rolling sounds
    GameTimer.Enabled = 1
    LoadLUT
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)
    If keycode = LeftMagnaSave Then bLutActive = True: SetLUTLine "Color LUT image " & table1.ColorGradeImage
    If keycode = RightMagnaSave AND bLutActive Then NextLUT
    ' add coins
    If Keycode = AddCreditKey OR Keycode = AddCreditKey2 Then
        If(Tilted = False)Then
            AddCredits 1
            PlaySound "fx_coin"
            PlaySound "coin"
            DOF 200, DOFOn
        End If
    End If

    ' plunger
    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
    End If

    ' keys during play

    If bGameInPlay AND NOT Tilted Then
        If Credits = 0 Then DOF 200, DOFOff
        ' tilt keys
        If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
        If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
        If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt
        If keycode = MechanicalTilt Then CheckTilt

        ' flipper keys
        If keycode = LeftFlipperKey Then SolLFlipper 1
        If keycode = RightFlipperKey Then SolRFlipper 1

        ' start game
        If keycode = StartGameKey Then
            If((PlayersPlayingGame < MaxPlayers)AND(bOnTheFirstBall = True))Then

                If(bFreePlay = True)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    PlaySound "start"
                Else
                    If(Credits > 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        Credits = Credits - 1
                        UpdateCredits
                        UpdateBallInPlay
                        PlaySound "start"
                    Else
                        DOF 200, DOFOff
                        ' not enough credits
                        PlaySound "10000"
                    End If
                End If
            End If
        End If
        Else ' If (GameInPlay)

            If keycode = StartGameKey Then
                If(bFreePlay = True)Then
                    If(BallsOnPlayfield = 0)Then
                        ResetScores
                        ResetForNewGame()
                        PlaySound "start"
                    End If
                Else
                    If(Credits > 0)Then
                        If(BallsOnPlayfield = 0)Then
                            Credits = Credits - 1
                            UpdateCredits
                            ResetScores
                            ResetForNewGame()
                            PlaySound "start"
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                        PlaySound "10000"
                    End If
                End If
            End If
    End If ' If (GameInPlay)
End Sub

Sub Table1_KeyUp(ByVal keycode)
    If keycode = LeftMagnaSave Then bLutActive = False: HideLUT
    If bGameInPlay AND NOT Tilted Then
        ' flippers
        If keycode = LeftFlipperKey Then SolLFlipper 0
        If keycode = RightFlipperKey Then SolRFlipper 0
    End If

    If keycode = PlungerKey Then
        Plunger.Fire
        If bBallInPlungerLane Then
            PlaySoundAt "fx_plunger", plunger
        Else
            PlaySoundAt "fx_plunger_empty", plunger
        End If
    End If
End Sub

'*************
' Para la mesa
'*************

Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub table1_Exit
    Savehs
    If B2SOn Then Controller.Stop
End Sub

'************************************
'       LUT - Darkness control
' 10 normal level & 10 warmer levels 
'************************************

Dim bLutActive, LUTImage

Sub LoadLUT
    bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "")Then LUTImage = x Else LUTImage = 0
    UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT:LUTImage = (LUTImage + 1)MOD 22:UpdateLUT:SaveLUT:SetLUTLine "Color LUT image " & table1.ColorGradeImage:End Sub

Sub UpdateLUT
    Select Case LutImage
        Case 0:table1.ColorGradeImage = "LUT0"
        Case 1:table1.ColorGradeImage = "LUT1"
        Case 2:table1.ColorGradeImage = "LUT2"
        Case 3:table1.ColorGradeImage = "LUT3"
        Case 4:table1.ColorGradeImage = "LUT4"
        Case 5:table1.ColorGradeImage = "LUT5"
        Case 6:table1.ColorGradeImage = "LUT6"
        Case 7:table1.ColorGradeImage = "LUT7"
        Case 8:table1.ColorGradeImage = "LUT8"
        Case 9:table1.ColorGradeImage = "LUT9"
        Case 10:table1.ColorGradeImage = "LUT10"
        Case 11:table1.ColorGradeImage = "LUT Warm 0"
        Case 12:table1.ColorGradeImage = "LUT Warm 1"
        Case 13:table1.ColorGradeImage = "LUT Warm 2"
        Case 14:table1.ColorGradeImage = "LUT Warm 3"
        Case 15:table1.ColorGradeImage = "LUT Warm 4"
        Case 16:table1.ColorGradeImage = "LUT Warm 5"
        Case 17:table1.ColorGradeImage = "LUT Warm 6"
        Case 18:table1.ColorGradeImage = "LUT Warm 7"
        Case 19:table1.ColorGradeImage = "LUT Warm 8"
        Case 20:table1.ColorGradeImage = "LUT Warm 9"
        Case 21:table1.ColorGradeImage = "LUT Warm 10"
    End Select
End Sub

Dim GiIntensity
GiIntensity = 1   'can be used by the LUT changing to increase the GI lights when the table is darker

Sub ChangeGiIntensity(factor) 'changes the intensity scale
    Dim bulb
    For each bulb in aGiLights
        bulb.IntensityScale = GiIntensity * factor
    Next
End Sub

' New LUT postit
Function GetHSChar(String, Index)
    Dim ThisChar
    Dim FileName
    ThisChar = Mid(String, Index, 1)
    FileName = "PostIt"
    If ThisChar = " " or ThisChar = "" then
        FileName = FileName & "BL"
    ElseIf ThisChar = "<" then
        FileName = FileName & "LT"
    ElseIf ThisChar = "_" then
        FileName = FileName & "SP"
    Else
        FileName = FileName & ThisChar
    End If
    GetHSChar = FileName
End Function

Sub SetLUTLine(String)
    Dim Index
    Dim xFor
    Index = 1
    LUBack.imagea="PostItNote"
    For xFor = 1 to 40
        Eval("LU" &xFor).imageA = GetHSChar(String, Index)
        Index = Index + 1
    Next
End Sub

Sub HideLUT
    SetLUTLine ""
    LUBack.imagea="PostitBL"
End Sub

'********************
'     Flippers
'********************

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF ("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToEnd
        LeftFlipper1.RotateToEnd
        LeftFlipperOn = 1
    Else
        PlaySoundAt SoundFXDOF ("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
        LeftFlipper1.RotateToStart
        LeftFlipperOn = 0
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF ("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
        RightFlipper1.RotateToEnd
        RightFlipperOn = 1
    Else
        PlaySoundAt SoundFXDOF ("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
        RightFlipper1.RotateToStart
        RightFlipperOn = 0
    End If
End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub LeftFlipper1_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper1_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

'*********************************************************
' Real Time Flipper adjustments - by JLouLouLou & JPSalas
'        (to enable flipper tricks) 
'*********************************************************

Dim FlipperPower
Dim FlipperElasticity
Dim SOSTorque, SOSAngle
Dim FullStrokeEOS_Torque, LiveStrokeEOS_Torque
Dim LeftFlipperOn
Dim RightFlipperOn

Dim LLiveCatchTimer
Dim RLiveCatchTimer
Dim LiveCatchSensivity

FlipperPower = 5000
FlipperElasticity = 0.8
FullStrokeEOS_Torque = 0.3 	' EOS Torque when flipper hold up ( EOS Coil is fully charged. Ampere increase due to flipper can't move or when it pushed back when "On". EOS Coil have more power )
LiveStrokeEOS_Torque = 0.2	' EOS Torque when flipper rotate to end ( When flipper move, EOS coil have less Ampere due to flipper can freely move. EOS Coil have less power )

LeftFlipper.EOSTorqueAngle = 10
RightFlipper.EOSTorqueAngle = 10

SOSTorque = 0.1
SOSAngle = 6

LiveCatchSensivity = 10

LLiveCatchTimer = 0
RLiveCatchTimer = 0

LeftFlipper.TimerInterval = 1
LeftFlipper.TimerEnabled = 1

Sub LeftFlipper_Timer 'flipper's tricks timer
'Start Of Stroke Flipper Stroke Routine : Start of Stroke for Tap pass and Tap shoot
    If LeftFlipper.CurrentAngle >= LeftFlipper.StartAngle - SOSAngle Then LeftFlipper.Strength = FlipperPower * SOSTorque else LeftFlipper.Strength = FlipperPower : End If
 
'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
	If LeftFlipperOn = 1 Then
		If LeftFlipper.CurrentAngle = LeftFlipper.EndAngle then
			LeftFlipper.EOSTorque = FullStrokeEOS_Torque
			LLiveCatchTimer = LLiveCatchTimer + 1
			If LLiveCatchTimer < LiveCatchSensivity Then
				LeftFlipper.Elasticity = 0
			Else
				LeftFlipper.Elasticity = FlipperElasticity
				LLiveCatchTimer = LiveCatchSensivity
			End If
		End If
	Else
		LeftFlipper.Elasticity = FlipperElasticity
		LeftFlipper.EOSTorque = LiveStrokeEOS_Torque
		LLiveCatchTimer = 0
	End If
	

'Start Of Stroke Flipper Stroke Routine : Start of Stroke for Tap pass and Tap shoot
    If RightFlipper.CurrentAngle <= RightFlipper.StartAngle + SOSAngle Then RightFlipper.Strength = FlipperPower * SOSTorque else RightFlipper.Strength = FlipperPower : End If
 
'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
 	If RightFlipperOn = 1 Then
		If RightFlipper.CurrentAngle = RightFlipper.EndAngle Then
			RightFlipper.EOSTorque = FullStrokeEOS_Torque
			RLiveCatchTimer = RLiveCatchTimer + 1
			If RLiveCatchTimer < LiveCatchSensivity Then
				RightFlipper.Elasticity = 0
			Else
				RightFlipper.Elasticity = FlipperElasticity
				RLiveCatchTimer = LiveCatchSensivity
			End If
		End If
	Else
		RightFlipper.Elasticity = FlipperElasticity
		RightFlipper.EOSTorque = LiveStrokeEOS_Torque
		RLiveCatchTimer = 0
	End If
End Sub

'*******************
'    GI lights
'*******************

Sub GiOn
    Dim bulb
    DOF 201, DOFOn
    For each bulb in aGiLights
        bulb.State = 1
    Next
    If B2SOn Then Controller.B2SSetData 60, 1
End Sub

Sub GiOff
    Dim bulb
    DOF 201, DOFOff
    For each bulb in aGiLights
        bulb.State = 0
    Next
    If B2SOn Then Controller.B2SSetData 60, 0
End Sub

'**************
' TILT - Falta
'**************

Sub CheckTilt
    Tilt = Tilt + TiltSensitivity
    TiltDecreaseTimer.Enabled = True
    If Tilt > 15 Then
        Tilted = True
        TiltReel.SetValue 1
        If B2SOn then
            Controller.B2SSetTilt 1
        end if
        DisableTable True
        TiltRecoveryTimer.Enabled = True
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
    If Tilt > 0 Then
        Tilt = Tilt - 0.1
    Else
        TiltDecreaseTimer.Enabled = False
    End If
End Sub

Sub DisableTable(Enabled)
    If Enabled Then
        GiOff
        'Disable slings, bumpers
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        Bumper1.Threshold = 100
        Bumper2.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
        DOF 101, DOFOff
        DOF 102, DOFOff
    Else
        GiOn
        Bumper1.Threshold = 1
        Bumper2.Threshold = 1
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' no more balls so do the end of ball
    If(BallsOnPlayfield = 0)Then
        EndOfBall()
        TiltRecoveryTimer.Enabled = False
    End If
' otherwise keep waiting
End Sub

'***************************************************************
'             Supporting Ball & Sound Functions v4.0
'  includes random pitch in PlaySoundAt and PlaySoundAtBall
'***************************************************************

Dim TableWidth, TableHeight

TableWidth = Table1.width
TableHeight = Table1.height

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
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
    PlaySound soundname, 0, 1, Pan(tableobj), 0.2, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.2, Pitch(ActiveBall) * 10, 0, 0, AudioFade(ActiveBall)
End Sub

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function

'***********************************************
'   JP's VP10 Rolling Sounds + Ballshadow v4.0
'   uses a collection of shadows, aBallShadow
'***********************************************

Const tnob = 19   'total number of balls
Const lob = 0     'number of locked balls
Const maxvel = 36 'max ball velocity
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
        aBallShadow(b).Y = 3000
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y
        aBallShadow(b).Height = BOT(b).Z -Ballsize/2

        If BallVel(BOT(b))> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 50000 'increase the pitch on a ramp
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
        If BOT(b).VelZ <-1 and BOT(b).z <55 and BOT(b).z> 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' jps ball speed & spin control
            BOT(b).AngMomZ = BOT(b).AngMomZ * 0.95
        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            speedfactorx = ABS(maxvel / BOT(b).VelX)
            speedfactory = ABS(maxvel / BOT(b).VelY)
            If speedfactorx <1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactorx
                BOT(b).VelY = BOT(b).VelY * speedfactorx
            End If
            If speedfactory <1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactory
                BOT(b).VelY = BOT(b).VelY * speedfactory
            End If
        End If
    Next
End Sub

'*****************************
' Ball 2 Ball Collision Sound
'*****************************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'***************************************
' Hit Sound of the collection of objects
' mostly rubbers, metals, plastics.
'***************************************

Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub

'************************************************************************************************************************
' FlashForMs will blink a light or a flasher for a period of time
'************************************************************************************************************************

Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState)

    If TypeName(MyLight) = "Light" Then ' object is a light
        If FinalState = 2 Then
            FinalState = MyLight.State  'save the light state
        End If
        MyLight.BlinkInterval = BlinkPeriod
        MyLight.Duration 2, TotalPeriod, FinalState
    ElseIf TypeName(MyLight) = "Flasher" Then ' object is a flasher
        Dim steps
        ' Store all blink information
        steps = Int(TotalPeriod / BlinkPeriod + .5) 'number of blink times
        If FinalState = 2 Then                      'save the flash state
            FinalState = ABS(MyLight.Visible)
        End If
        MyLight.UserValue = steps * 10 + FinalState 'save the number of blinks

        ' starts the rutine which wil blink the light
        MyLight.TimerInterval = BlinkPeriod
        MyLight.TimerEnabled = 0
        MyLight.TimerEnabled = 1
        ExecuteGlobal "Sub " & MyLight.Name & "_Timer:" & "Dim tmp, steps, fstate:tmp=me.UserValue:fstate = tmp MOD 10:steps= tmp\10 -1:Me.Visible = steps MOD 2:me.UserValue = steps *10 + fstate:If Steps = 0 then Me.Visible = fstate:Me.TimerEnabled=0:End if:End Sub"
    End If
End Sub

'****************************************
' Inicializa la mesa para un juego nuevo
'****************************************

Sub ResetForNewGame()
    'debug.print "ResetForNewGame"
    Dim i

    bGameInPLay = True
    bBallSaverActive = False

    'turn off attract mode
    StopAttractMode
    If B2SOn then
        Controller.B2SSetGameOver 0
    end if
    ' turn on GI lights
    GiOn

    CurrentPlayer = 1
    PlayersPlayingGame = 1
    bOnTheFirstBall = True
    For i = 1 To MaxPlayers
        Score(i) = 0
        ExtraBallsAwards(i) = 0
        Special1Awarded(i) = False
        Special2Awarded(i) = False
        Special3Awarded(i) = False
        BallsRemaining(i) = BallsPerGame
        BonusMultiplier(i) = 1
    Next
    Bonus = 0
    UpdateBallInPlay

    Clear_Match

    ' initialize other variables
    Tilt = 0

    ' Initialize game variables
    Game_Init()

    ' Start some music?
    ' start the first ball
    vpmtimer.addtimer 2000, "FirstBall '"
    If Credits = 0 Then DOF 200, DOFOff
End Sub

Sub FirstBall
    'debug.print "FirstBall"
    ' set up the table for a new ball
    ResetForNewPlayerBall()
    AddScore 1
    AddScore 2
    ' create the ball in the plunger
    CreateNewBall()
    DOF 250, DOFPulse
End Sub

' Reset the table for a new ball or player

Sub ResetForNewPlayerBall()
    'debug.print "ResetForNewPlayerBall"
    ' make sure the score is on

    ' turn on lights and reset game variables
    bExtraBallWonThisBall = False
    ResetNewBallLights
    ResetNewBallVariables
End Sub

' Create a new ball on the table

Sub CreateNewBall()
    ' create ball
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass

    ' count the balls on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' update desktop lights
    UpdateBallInPlay
    AddScore 1
    AddScore 2

    ' eject the ball
    PlaySoundAt SoundFXDOF ("fx_Ballrel", 104, DOFPulse, DOFContactors), BallRelease
    If Epileptikdof = True Then DOF 223, DOFPulse End If
    BallRelease.Kick 90, 4
End Sub

' The player has lost the ball

Sub EndOfBall()
    'debug.print "EndOfBall"
    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 0
    ' The first ball is over, do not add more player
    bOnTheFirstBall = False

    ' count the bonus if there is no tilt

    If NOT Tilted Then
        If bBonusReady Then
            ResetAllTargets CurrentPlayer
            ResetTargets.Enabled = 1
            BonusCountTimer.Enabled = 1
            bBonusReady = False
        Else
            vpmtimer.addtimer 400, "EndOfBall2 '"
        End If
    Else 'tilted
        vpmtimer.addtimer 400, "EndOfBall2 '"
    End If
End Sub

Sub BonusCountTimer_Timer 'add bonus and update the bonus lights
    'debug.print "BonusCount_Timer"
    If Bonus > 0 Then
        Bonus = Bonus -1
        AddBonusScore 500 * BonusMultiplier(CurrentPlayer)
    'UpdateBonusLights
    Else
        ' continue with the end of ball
        BonusCountTimer.Enabled = 0
        vpmtimer.addtimer 1000, "EndOfBall2 '"
    End If
End Sub

Sub UpdateBonusLights 'turn onn and off the bonus lights (not used in this table)
End Sub

Sub EndOfBall2()
    'debug.print "EndOfBall2"
    ' reset tilt and move to the next ball or player
    Tilted = False
    Tilt = 0
    TiltReel.SetValue 0
    If B2SOn then
        Controller.B2SSetTilt 0
    end if
    DisableTable False 'activate

    ' won an extra ball?
    If(ExtraBallsAwards(CurrentPlayer) > 0)Then
        'debug.print "Extra Ball"

        ' yes? the give it
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer)- 1

        ' no more balls then turn off the light shoot again, if any
        If(ExtraBallsAwards(CurrentPlayer) = 0)Then
            'LightShootAgain.State = 0
            If B2SOn then
                Controller.B2SSetShootAgain 0
            end if
        End If

        ' play a sound, blink a light?

        ' in this table we reset the table as for a new ball
        ResetForNewPlayerBall()

        ' create ball
        CreateNewBall()
    Else ' no more extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer)- 1

        ' last ball?
        If(BallsRemaining(CurrentPlayer) <= 0)Then

            ' check if the score qualifies for a highscore
            CheckHighScore()
        End If

        EndOfBallComplete()
    End If
End Sub

'
Sub EndOfBallComplete()
    Dim NextPlayer

    'debug.print "EndOfBall - Complete"

    ' more players?
    If(PlayersPlayingGame > 1)Then
        ' move to the next player
        NextPlayer = CurrentPlayer + 1
        ' no more players? Move to the first player
        If(NextPlayer > PlayersPlayingGame)Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'debug.print "Next Player = " & NextPlayer

    ' is this the end of the game, no more balls or players?
    If((BallsRemaining(CurrentPlayer) <= 0)AND(BallsRemaining(NextPlayer) <= 0))Then

        ' start the match
        If bFreePlay = False Then
            Verification_Match
        End If

        ' put the table in the end of game mode
        EndOfGame()
    Else
        ' move to the next player
        CurrentPlayer = NextPlayer

        ' update the scores
        AddScore 0

        ' reset table for next player/ball
        ResetForNewPlayerBall()
        CreateNewBall()
    End If
End Sub

' the end of the game

Sub EndOfGame()
    'debug.print "EndOfGame"
    bGameInPLay = False
    bJustStarted = False
    If B2SOn then
        Controller.B2SSetGameOver 1
        Controller.B2SSetScorePlayer 4, 0
        Controller.B2SSetPlayerUp 0
        Controller.B2SSetCanPlay 0
    end if
    ' turn off flippers
    SolLFlipper 0
    SolRFlipper 0

    ' start the attrack mode
    StartAttractMode
    DOF 250, DOFPulse
    DOF 201, DOFOff
End Sub

' calculates the number of ball
Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp > BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
    End If
End Function

' check the highscore
Sub CheckHighscore
    For x = 1 to PlayersPlayingGame
        If Score(x) > Highscore Then
            Highscore = Score(x)
            PlaySound SoundFXDOF  ("fx_knocker", 300, DOFPulse, DOFKnocker)
            DOF 230, DOFPulse
            'HighscoreReel.SetValue Highscore
            AddCredits 1
            DOF 200, DOFOn
        End If
    Next
End Sub

'******************
'  Match - Loteria
'******************

Sub Verification_Match()
    'PlaySound "fx_match"
    Match = INT(RND(1) * 10)
    BallinplayR.SetValue Match
    If B2SOn then
        Controller.B2SSetScorePlayer 4, Match
    end if
    If(Score(CurrentPlayer)MOD 100) = Match *10 Then
        PlaySound SoundFXDOF  ("fx_knocker", 300, DOFPulse, DOFKnocker)
        DOF 230, DOFPulse
        AddCredits 1
        DOF 200, DOFOn
    End If
End Sub

Sub Clear_Match()
    BallinplayR.SetValue 0
    If B2SOn then
        Controller.B2SSetScorePlayer 4, 0
    end if
End Sub

' *********************************************************************
'                      Drain / Plunger Functions
' *********************************************************************

Sub Drain_Hit()

    Drain.DestroyBall
    BallsOnPlayfield = BallsOnPlayfield - 1
    PlaySoundAt "fx_drain", Drain
    DOF 222, DOFPulse

    'if Tilted the tilt sub will do the rest of the end of ball
    If Tilted Then
        Exit Sub
    End If

    ' you are playfield and not tilted
    If(bGameInPLay = True)AND(Tilted = False)Then

        ' is ball saver active?
        If(bBallSaverActive = True)Then

            ' yes? the create ball
            CreateNewBall()
        Else
            ' is this the last ball?
            If(BallsOnPlayfield = 0)Then
                vpmtimer.addtimer 500, "EndOfBall '"
                Exit Sub
            End If
        End If
    End If
End Sub

Sub swPlungerRest_Hit()
    bBallInPlungerLane = True
End Sub

' here you could start a ball saver

Sub swPlungerRest_UnHit()
    bBallInPlungerLane = False
End Sub

' ***********************************************
'               Score functions
' ***********************************************

Sub AddBonusScore(Points)
            Score(CurrentPlayer) = Score(CurrentPlayer) + points
            UpdateScore
            PlaySound SoundFXDOF("beep100", 132, DOFPulse, DOFChimes)
End Sub

Sub AddScore(Points)
    If Tilted Then Exit Sub
    Select Case Points
       Case 1
            Score(CurrentPlayer) = Score(CurrentPlayer) + 1
            UpdateScore
       Case 2
            Score(CurrentPlayer) = Score(CurrentPlayer) - 1
            UpdateScore
       Case 10
            Score(CurrentPlayer) = Score(CurrentPlayer) + points
            UpdateScore
            PlaySound SoundFXDOF("beep10", 131, DOFPulse, DOFChimes)
        Case 100
            Score(CurrentPlayer) = Score(CurrentPlayer) + points
            UpdateScore
            PlaySound SoundFXDOF("beep100", 132, DOFPulse, DOFChimes)
        Case 1000
            Score(CurrentPlayer) = Score(CurrentPlayer) + points
            UpdateScore
            PlaySound SoundFXDOF("beep1000", 132, DOFPulse, DOFChimes)
        Case 20, 30, 40, 50
            Add10 = Add10 + Points \ 10
            AddScore10Timer.Enabled = TRUE
        Case 200, 300, 400, 500
            Add100 = Add100 + Points \ 100
            AddScore100Timer.Enabled = TRUE
        Case 2000, 3000, 4000, 5000
            Add1000 = Add1000 + Points \ 1000
            AddScore1000Timer.Enabled = TRUE
    End Select

    ' turn on 100.000 lights
    If Score(1) > 99999 Then
        p100.SetValue 1
    Else
        p100.SetValue 0
    End If

    If Score(2) > 99999 Then
        p200.SetValue 1
    Else
        p200.SetValue 0
    End If

    ' check for awards for higher scores
    If Score(CurrentPlayer) >= Special1 AND Special1Awarded(CurrentPlayer) = False Then
        AwardSpecial
        Special1Awarded(CurrentPlayer) = True
    End If
    If Score(CurrentPlayer) >= Special2 AND Special2Awarded(CurrentPlayer) = False Then
        AwardSpecial
        Special2Awarded(CurrentPlayer) = True
    End If
    If Score(CurrentPlayer) >= Special3 AND Special3Awarded(CurrentPlayer) = False Then
        AwardSpecial
        Special3Awarded(CurrentPlayer) = True
    End If
End Sub

'************************************
'       Score sound Timers
'************************************

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

'*****************************
'     BONUS functions
'*****************************

' use: AddBonus 1

Sub AddBonus(bonuspoints)
    If(Tilted = False)Then

        Bonus = Bonus + bonuspoints
        If Bonus > 15 Then
            Bonus = 15
        End If
        UpdateBonusLights
    End if
End Sub

Sub AddBonusMultiplier(multi) 'not used in this table, we set the multiplier manually
    If(Tilted = False)Then

        BonusMultiplier(CurrentPlayer) = BonusMultiplier(CurrentPlayer) + multi
        If BonusMultiplier(CurrentPlayer) > MaxMultiplier Then
            BonusMultiplier(CurrentPlayer) = MaxMultiplier
        End If
        UpdateMultiplierLights
    End if
End Sub

Sub UpdateMultiplierLights
    Select Case BonusMultiplier(CurrentPlayer)
        Case 1:li2.State = 0:li1.State = 0
        Case 2:li2.State = 1:li1.State = 0
        Case 3:li2.State = 1:li1.State = 1
    End Select
End Sub

'******************************
'        Score reels
'******************************

Sub UpdateScore
    Select Case CurrentPlayer
        Case 1:ScoreReel1.Setvalue Score(1)
        Case 2:ScoreReel2.Setvalue Score(2)
    End Select
    If B2SOn then
        Controller.B2SSetScorePlayer CurrentPlayer, Score(CurrentPlayer)
        If Score(CurrentPlayer) >= 100000 then
            Controller.B2SSetScoreRollover 24 + CurrentPlayer, 1
        end if
    end if
End Sub

Sub ResetScores
    ScoreReel1.ResetToZero
    ScoreReel2.ResetToZero
    If B2SOn then
        Controller.B2SSetScorePlayer1 0
        Controller.B2SSetScorePlayer2 0
        Controller.B2SSetScoreRolloverPlayer1 0
        Controller.B2SSetScoreRolloverPlayer2 0
    end if
End Sub

Sub AddCredits(value)
    If Credits < 9 Then
        Credits = Credits + value
        CreditReel.SetValue Credits
        UpdateCredits
    end if
End Sub

Sub UpdateCredits
    CreditReel.SetValue credits
    If B2SOn then
        Controller.B2SSetScorePlayer 3, Credits
    end if
End Sub

Sub UpdateBallInPlay 'updates all the lights/reels on the backdrop
    BallInPlayR.SetValue Balls
    If B2SOn then
        Controller.B2SSetScorePlayer 4, Balls
    end if
    Select Case CurrentPlayer
        Case 1:pl1.State = 1:pl2.State = 0
        Case 2:pl1.State = 0:pl2.State = 1
    End Select
    If B2SOn then
        Controller.B2SSetPlayerUp CurrentPlayer
    end if
    Select Case PlayersPlayingGame
        Case 1:cp1.State = 1:cp2.State = 0
        Case 2:cp1.State = 0:cp2.State = 1
    End Select
    If B2SOn then
        Controller.B2SSetCanPlay PlayersPlayingGame
        Controller.B2SSetCanPlay PlayersPlayingGame
    end if
End Sub

'*************************
'        Awards
'*************************

Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
        PlaySound SoundFXDOF  ("fx_knocker", 300, DOFPulse, DOFKnocker)
        DOF 230, DOFPulse
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
        ' LightShootAgain.State = 1
        If B2SOn then
            Controller.B2SSetShootAgain 1
        end if
    END If
End Sub

Sub AwardSpecial()
    PlaySound SoundFXDOF  ("fx_knocker", 300, DOFPulse, DOFKnocker)
    DOF 230, DOFPulse
    AddCredits 1
    DOF 200, DOFOn
End Sub

' ********************************
'        Attract Mode
' ********************************

Sub StartAttractMode()
    bAttractMode = True
    For each x in aLights 'all lights blink in a defined pattern
        x.State = 2
    Next
    ' turn on game over lights
    GameOverR.SetValue 1
    ' start attrack timer
    AttractTimer.Enabled = 1
    If Credits > 0 Then DOF 200, DOFOn
    DOF 201, DOFOff
End Sub

Sub StopAttractMode()
    bAttractMode = False
    ResetScores
    For each x in aLights
        x.State = 0
    Next
    ' turn off game over light
    GameOverR.SetValue 0
    ' stops attrack timer
    AttractTimer.Enabled = 0
End Sub

Dim AttractStep
AttractStep = 0

Sub AttractTimer_Timer 'simple shows the highscore
    Select Case AttractStep
        Case 0
            AttractStep = 1
            ScoreReel1.SetValue Score(1)
            ScoreReel2.SetValue Score(2)
            If B2SOn then
                Controller.B2SSetScorePlayer1 Score(1)
                Controller.B2SSetScorePlayer2 Score(2)
            end if
        Case 1
            AttractStep = 0
            ScoreReel1.SetValue Highscore
            ScoreReel2.SetValue Highscore
            If B2SOn then
                Controller.B2SSetScorePlayer1 Highscore
                Controller.B2SSetScorePlayer2 Highscore
            end if
    End Select
    If B2SOn Then
        Controller.B2SSetScorePlayer 3, Credits 'for Credit LED
        Controller.B2SSetScorePlayer 4, 0       'for BiP LED at Start
    End If
End Sub

'****************************
'   Load & Save Highscore
'****************************

' save credits and highscore points

Sub Loadhs
    x = LoadValue(TableName, "HighScore")
    If(x <> "")Then HighScore = CDbl(x)Else HighScore = 50000 End If
    x = LoadValue(TableName, "Credits")
    If(x <> "")then Credits = CInt(x)Else Credits = 0 End If
End Sub

Sub Savehs
    SaveValue TableName, "HighScore", HighScore
    SaveValue TableName, "Credits", Credits
End Sub

Sub Reseths
    HighScore = 0
    Savehs
End Sub

'************************
'    Realtime updates
'************************
' all animations should go here, like flippers, gates

Sub GameTimer_Timer
    RollingUpdate
End Sub

'***********************************************************************
' *********************************************************************
'                      GAME CODE STARTS HERE
' *********************************************************************
'***********************************************************************

Sub VPObjects_Init
    sw12a.IsDropped = 1
End Sub

' table variables
Dim PlayerTargets(2, 14)
Dim ResetStep
Dim TargetsCompleted(2)
Dim bBonusReady

Sub Game_Init() 'call at the start of a new game

    'Start some music

    'init variables

    ResetStep = 0
    TargetsCompleted(1) = 0
    TargetsCompleted(2) = 0
    ResetAllTargets 1
    ResetAllTargets 2
    'start some timer
    ResetTargets.Enabled = 1
    'turn off lights
    TurnOffPlayfieldLights()
    bBonusReady = False
End Sub

Sub ResetNewBallVariables() 'variables for a new ball
End Sub

Sub ResetNewBallLights()    'lights for a new ball
    li17.State = 0
    li18.State = 0
    li19.State = 0
    li20.State = 0
    li21.State = 0
    li22.State = 0
    li23.State = 0
    li28.State = 0
    li28a.State = 0
    li48.State = 0
    li48a.State = 0
    li2.State = 0
    li1.State = 0
    ResetStep = 0
    ResetTargets.Enabled = 1 'update targets and lights in case the player changed
    UpdateMultiplierLights
End Sub

Sub TurnOffPlayfieldLights()
    For each x in aLights
        x.State = 0
    Next
End Sub

Sub ResetTargets_Timer 'when changing player
    Select Case ResetStep
        Case 0
            If ABS(sw33.IsDropped) <> PlayerTargets(CurrentPlayer, 1)Then PlaySoundAt SoundFXDOF ("fx_resetdrop", 108, DOFPulse, DOFDropTargets), sw33:End If
            sw33.Isdropped = PlayerTargets(CurrentPlayer, 1):li41.State = PlayerTargets(CurrentPlayer, 1):li14.State = PlayerTargets(CurrentPlayer, 1)
        Case 1
            If ABS(sw34.IsDropped) <> PlayerTargets(CurrentPlayer, 2)Then PlaySoundAt SoundFXDOF ("fx_resetdrop", 108, DOFPulse, DOFDropTargets), sw34:End If
            sw34.Isdropped = PlayerTargets(CurrentPlayer, 2):li42.State = PlayerTargets(CurrentPlayer, 2):li15.State = PlayerTargets(CurrentPlayer, 2)
        Case 2
            If ABS(sw35.IsDropped) <> PlayerTargets(CurrentPlayer, 3)Then PlaySoundAt SoundFXDOF ("fx_resetdrop", 108, DOFPulse, DOFDropTargets), sw35:End If
            sw35.Isdropped = PlayerTargets(CurrentPlayer, 3):li43.State = PlayerTargets(CurrentPlayer, 3):li16.State = PlayerTargets(CurrentPlayer, 3)
        Case 3
            If ABS(sw36.IsDropped) <> PlayerTargets(CurrentPlayer, 4)Then PlaySoundAt SoundFXDOF ("fx_resetdrop", 108, DOFPulse, DOFDropTargets), sw36:End If
            sw36.Isdropped = PlayerTargets(CurrentPlayer, 4):li44.State = PlayerTargets(CurrentPlayer, 4):li26.State = PlayerTargets(CurrentPlayer, 4)
        Case 4
            If ABS(sw37.IsDropped) <> PlayerTargets(CurrentPlayer, 5)Then PlaySoundAt SoundFXDOF ("fx_resetdrop", 108, DOFPulse, DOFDropTargets), sw37:End If
            sw37.Isdropped = PlayerTargets(CurrentPlayer, 5):li45.State = PlayerTargets(CurrentPlayer, 5):li27.State = PlayerTargets(CurrentPlayer, 5)
        Case 5
            If ABS(sw38.IsDropped) <> PlayerTargets(CurrentPlayer, 6)Then PlaySoundAt SoundFXDOF ("fx_resetdrop", 108, DOFPulse, DOFDropTargets), sw38:End If
            sw38.Isdropped = PlayerTargets(CurrentPlayer, 6):li46.State = PlayerTargets(CurrentPlayer, 6):li10.State = PlayerTargets(CurrentPlayer, 6)
        Case 6
            If ABS(sw39.IsDropped) <> PlayerTargets(CurrentPlayer, 7)Then PlaySoundAt SoundFXDOF ("fx_resetdrop", 108, DOFPulse, DOFDropTargets), sw39:End If
            sw39.Isdropped = PlayerTargets(CurrentPlayer, 7):li35.State = PlayerTargets(CurrentPlayer, 7):li11.State = PlayerTargets(CurrentPlayer, 7)
        Case 7
            If ABS(sw40.IsDropped) <> PlayerTargets(CurrentPlayer, 8)Then PlaySoundAt SoundFXDOF ("fx_resetdrop", 108, DOFPulse, DOFDropTargets), sw40:End If
            sw40.Isdropped = PlayerTargets(CurrentPlayer, 8):li36.State = PlayerTargets(CurrentPlayer, 8):li13.State = PlayerTargets(CurrentPlayer, 8)
        Case 8
            If ABS(sw41.IsDropped) <> PlayerTargets(CurrentPlayer, 9)Then PlaySoundAt SoundFXDOF ("fx_resetdrop", 108, DOFPulse, DOFDropTargets), sw41:End If
            sw41.Isdropped = PlayerTargets(CurrentPlayer, 9):li37.State = PlayerTargets(CurrentPlayer, 9):li25.State = PlayerTargets(CurrentPlayer, 9)
        Case 9
            If ABS(sw42.IsDropped) <> PlayerTargets(CurrentPlayer, 10)Then PlaySoundAt SoundFXDOF ("fx_resetdrop", 109, DOFPulse, DOFDropTargets), sw42:End If
            sw42.Isdropped = PlayerTargets(CurrentPlayer, 10):li47.State = PlayerTargets(CurrentPlayer, 10):li8.State = PlayerTargets(CurrentPlayer, 10)
        Case 10
            If ABS(sw43.IsDropped) <> PlayerTargets(CurrentPlayer, 11)Then PlaySoundAt SoundFXDOF ("fx_resetdrop", 109, DOFPulse, DOFDropTargets), sw43:End If
            sw43.Isdropped = PlayerTargets(CurrentPlayer, 11):li31.State = PlayerTargets(CurrentPlayer, 11):li9.State = PlayerTargets(CurrentPlayer, 11)
        Case 11
            If ABS(sw44.IsDropped) <> PlayerTargets(CurrentPlayer, 12)Then PlaySoundAt SoundFXDOF ("fx_resetdrop", 109, DOFPulse, DOFDropTargets), sw44:End If
            sw44.Isdropped = PlayerTargets(CurrentPlayer, 12):li32.State = PlayerTargets(CurrentPlayer, 12):li4.State = PlayerTargets(CurrentPlayer, 12)
        Case 12
            If ABS(sw45.IsDropped) <> PlayerTargets(CurrentPlayer, 13)Then PlaySoundAt SoundFXDOF ("fx_resetdrop", 109, DOFPulse, DOFDropTargets), sw45:End If
            sw45.Isdropped = PlayerTargets(CurrentPlayer, 13):li33.State = PlayerTargets(CurrentPlayer, 13):li24.State = PlayerTargets(CurrentPlayer, 13)
        Case 13
            If ABS(sw46.IsDropped) <> PlayerTargets(CurrentPlayer, 14)Then PlaySoundAt SoundFXDOF ("fx_resetdrop", 109, DOFPulse, DOFDropTargets), sw46:End If
            sw46.Isdropped = PlayerTargets(CurrentPlayer, 14):li34.State = PlayerTargets(CurrentPlayer, 14):li3.State = PlayerTargets(CurrentPlayer, 14)
        Case 14:li12.State = 0:ResetTargets.Enabled = 0
    End Select
    ResetStep = ResetStep + 1
End Sub

Sub ResetAllTargets(player)
    For x = 1 to 14
        PlayerTargets(player, x) = 0
    Next
    ResetStep = 0
End Sub

'******************************
'     DOF lights ball entrance
'******************************
'
Sub Trigger001_Hit
    DOF 240, DOFPulse
End sub


' *********************************************************************
'       Tables HIT events
'
' Each target should do something like this:
' - play a hit sound
' - do some animation, if needed
' - add a score
' - turn on/off some lights
' - check for an award or mode
' *********************************************************************

'*************
' Slingshots
'*************

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAtBall SoundFXDOF ("fx_slingshot",103,DOFPulse,DOFcontactors)
    DOF 203, DOFPulse
    If Epileptikdof = True Then DOF 204, DOFPulse End If
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' añade algunos puntos
    AddScore 10
' añade algún efecto a la mesa
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing2.Visible = 0:Lemk.RotX = -10:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAtBall SoundFXDOF ("fx_slingshot",104,DOFPulse,DOFcontactors)
    DOF 205, DOFPulse
    If Epileptikdof = True Then DOF 206, DOFPulse End If
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' añade algunos puntos
    AddScore 10
' añade algún efecto a la mesa
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing2.Visible = 0:Remk.RotX = -10:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

'*************
'   Rubbers
'*************

Sub RubberBand6_Hit
    If NOT Tilted Then
        ' añade 10 puntos
        AddScore 10
    End If
End Sub

Sub RubberBand7_Hit
    If NOT Tilted Then
        ' añade 10 puntos
        AddScore 10
    End If
End Sub

Sub RubberBand10_Hit
    If NOT Tilted Then
        ' añade 10 puntos
        AddScore 10
    End If
End Sub

'*********
' Bumpers
'*********

Sub Bumper1_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_Bumper",105,DOFPulse,DOFContactors), bumper1
    DOF 207, DOFPulse
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    ' añade algunos puntos
    AddScore 100
End Sub

Sub Bumper2_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_Bumper",106,DOFPulse,DOFContactors), bumper2
    DOF 208, DOFPulse
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    ' añade algunos puntos
    AddScore 100
End Sub

'*******************
'     Lanes
'*******************

Sub sw13_Hit
    PlaySoundAt "fx_sensor", sw13
    If Tilted Then Exit Sub
    DOF 213, DOFPulse
    AddScore 10000
    if li48.State Then AwardExtraBall
    if li28.State Then AwardSpecial
End Sub

Sub sw13a_Hit
    PlaySoundAt "fx_sensor", sw13a
    If Tilted Then Exit Sub
    DOF 214, DOFPulse
    AddScore 10000
    if li48a.State Then AwardExtraBall
    if li28a.State Then AwardSpecial
End Sub

Sub sw14_Hit
    PlaySoundAt "fx_sensor", sw14
    If Tilted Then Exit Sub
    DOF 215, DOFPulse
    AddScore 100 + 400 * li22.State
    li22.State = 1
End Sub

Sub sw17_Hit
    PlaySoundAt "fx_sensor", sw17
    If Tilted Then Exit Sub
    DOF 216, DOFPulse
    AddScore 100 + 400 * li23.State
    li23.State = 1
End Sub

Sub sw10_Hit
    PlaySoundAt "fx_sensor", sw10
    If Tilted Then Exit Sub
    DOF 217, DOFPulse
    AddScore 100 + 400 * li20.State
    li20.State = 1
End Sub

Sub sw11_Hit
    PlaySoundAt "fx_sensor", sw11
    If Tilted Then Exit Sub
    DOF 218, DOFPulse
    AddScore 100 + 400 * li21.State
    li21.State = 1
End Sub

Sub sw2_Hit
    PlaySoundAt "fx_sensor", sw2
    If Tilted Then Exit Sub
    DOF 219, DOFPulse
    AddScore 100 + 400 * li17.State
    li17.State = 1
End Sub

Sub sw3_Hit
    PlaySoundAt "fx_sensor", sw3
    If Tilted Then Exit Sub
    DOF 220, DOFPulse
    AddScore 100 + 400 * li18.State
    li18.State = 1
End Sub

Sub sw4_Hit
    PlaySoundAt "fx_sensor", sw4
    If Tilted Then Exit Sub
    DOF 221, DOFPulse
    AddScore 100 + 400 * li19.State
    li19.State = 1
End Sub

'******************
'   Droptargets
'******************

Sub sw33_Hit
    PlaySoundAtBall SoundFXDOF ("fx_droptarget", 108, DOFPulse, DOFDropTargets)
    If Tilted Then Exit Sub
    If Epileptikdof = True Then DOF 210, DOFPulse End If
    AddScore 300
    li41.State = 1
    li14.State = 1
    PlayerTargets(CurrentPlayer, 1) = 1
End Sub

Sub sw34_Hit
    PlaySoundAtBall SoundFXDOF ("fx_droptarget", 108, DOFPulse, DOFDropTargets)
    If Tilted Then Exit Sub
    If Epileptikdof = True Then DOF 210, DOFPulse End If
    AddScore 300
    AddBonus 1
    li42.State = 1
    li15.State = 1
    PlayerTargets(CurrentPlayer, 2) = 1
End Sub

Sub sw35_Hit
    PlaySoundAtBall SoundFXDOF ("fx_droptarget", 108, DOFPulse, DOFDropTargets)
    If Tilted Then Exit Sub
    If Epileptikdof = True Then DOF 210, DOFPulse End If
    AddScore 300
    AddBonus 1
    li43.State = 1
    li16.State = 1
    PlayerTargets(CurrentPlayer, 3) = 1
End Sub

Sub sw36_Hit
    PlaySoundAtBall SoundFXDOF ("fx_droptarget", 108, DOFPulse, DOFDropTargets)
    If Tilted Then Exit Sub
    If Epileptikdof = True Then DOF 210, DOFPulse End If
    AddScore 300
    AddBonus 1
    li44.State = 1
    li26.State = 1
    PlayerTargets(CurrentPlayer, 4) = 1
End Sub

Sub sw37_Hit
    PlaySoundAtBall SoundFXDOF ("fx_droptarget", 108, DOFPulse, DOFDropTargets)
    If Tilted Then Exit Sub
    If Epileptikdof = True Then DOF 211, DOFPulse End If
    AddScore 300
    AddBonus 1
    li45.State = 1
    li27.State = 1
    PlayerTargets(CurrentPlayer, 5) = 1
End Sub

Sub sw38_Hit
    PlaySoundAtBall SoundFXDOF ("fx_droptarget", 108, DOFPulse, DOFDropTargets)
    If Tilted Then Exit Sub
    If Epileptikdof = True Then DOF 211, DOFPulse End If
    AddScore 300
    AddBonus 1
    li46.State = 1
    li10.State = 1
    PlayerTargets(CurrentPlayer, 6) = 1
End Sub

Sub sw39_Hit
    PlaySoundAtBall SoundFXDOF ("fx_droptarget", 108, DOFPulse, DOFDropTargets)
    If Tilted Then Exit Sub
    If Epileptikdof = True Then DOF 211, DOFPulse End If
    AddScore 300
    AddBonus 1
    li35.State = 1
    li11.State = 1
    PlayerTargets(CurrentPlayer, 7) = 1
End Sub

Sub sw40_Hit
    PlaySoundAtBall SoundFXDOF ("fx_droptarget", 108, DOFPulse, DOFDropTargets)
    If Tilted Then Exit Sub
    If Epileptikdof = True Then DOF 211, DOFPulse End If
    AddScore 300
    AddBonus 1
    li36.State = 1
    li13.State = 1
    PlayerTargets(CurrentPlayer, 8) = 1
End Sub

Sub sw41_Hit
    PlaySoundAtBall SoundFXDOF ("fx_droptarget", 108, DOFPulse, DOFDropTargets)
    If Tilted Then Exit Sub
    If Epileptikdof = True Then DOF 211, DOFPulse End If
    AddScore 300
    AddBonus 1
    li37.State = 1
    li25.State = 1
    PlayerTargets(CurrentPlayer, 9) = 1
End Sub

Sub sw42_Hit
    PlaySoundAtBall SoundFXDOF ("fx_droptarget", 109, DOFPulse, DOFDropTargets)
    If Tilted Then Exit Sub
    If Epileptikdof = True Then DOF 212, DOFPulse End If
    AddScore 300
    AddBonus 1
    li47.State = 1
    li8.State = 1
    PlayerTargets(CurrentPlayer, 10) = 1
End Sub

Sub sw43_Hit
    PlaySoundAtBall SoundFXDOF ("fx_droptarget", 109, DOFPulse, DOFDropTargets)
    If Tilted Then Exit Sub
    If Epileptikdof = True Then DOF 212, DOFPulse End If
    AddScore 300
    AddBonus 1
    li31.State = 1
    li9.State = 1
    PlayerTargets(CurrentPlayer, 11) = 1
End Sub

Sub sw44_Hit
    PlaySoundAtBall SoundFXDOF ("fx_droptarget", 109, DOFPulse, DOFDropTargets)
    If Tilted Then Exit Sub
    If Epileptikdof = True Then DOF 212, DOFPulse End If
    AddScore 300
    AddBonus 1
    li32.State = 1
    li4.State = 1
    PlayerTargets(CurrentPlayer, 12) = 1
End Sub

Sub sw45_Hit
    PlaySoundAtBall SoundFXDOF ("fx_droptarget", 109, DOFPulse, DOFDropTargets)
    If Tilted Then Exit Sub
    If Epileptikdof = True Then DOF 212, DOFPulse End If
    AddScore 300
    AddBonus 1
    li33.State = 1
    li24.State = 1
    PlayerTargets(CurrentPlayer, 13) = 1
End Sub

Sub sw46_Hit
    PlaySoundAtBall SoundFXDOF ("fx_droptarget", 109, DOFPulse, DOFDropTargets)
    If Tilted Then Exit Sub
    If Epileptikdof = True Then DOF 212, DOFPulse End If
    AddScore 300
    AddBonus 1
    li34.State = 1
    li3.State = 1
    PlayerTargets(CurrentPlayer, 14) = 1
End Sub

'****************************************
' Extra ball, DoubleBonus & Tripplebonus
'****************************************

'*************
'8 ball target
'*************

Sub sw12_hit
    PlaySoundAtBall SoundFXDOF ("fx_target", 107, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 209, DOFPulse
    Addscore 300
    li12.State = 1
    CheckSpecials
End Sub

Sub CheckSpecials
    If TargetsDown = 14 AND NOT bBonusReady Then
        li48.State = 1 'extra ball
        li48a.State = 1
        TargetsCompleted(CurrentPlayer) = TargetsCompleted(CurrentPlayer) + 1
        If TargetsCompleted(CurrentPlayer) = 2 Then
            li28.State = 1                     'extra game
            li28a.State = 1
            BonusMultiplier(CurrentPlayer) = 2 'Double bonus
            UpdateMultiplierLights
        End If
        If TargetsCompleted(CurrentPlayer) > 2 Then
            li28.State = 1                     'extra game
            li28a.State = 1
            BonusMultiplier(CurrentPlayer) = 3 'Tripple bonus
            UpdateMultiplierLights
        End If
        bBonusReady = True
    End If
    If NOT bBonusReady Then
        li12.State = 0
    End If
End Sub

Function Targetsdown
    Dim tmp
    tmp = 0
    For x = 1 to 14
        tmp = tmp + PlayerTargets(CurrentPlayer, x)
    next
    Targetsdown = tmp
End Function