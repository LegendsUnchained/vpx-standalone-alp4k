' ****************************************************
'      Senna - Prototype by Luiz Culik, 2020
'
'      VPX8 Table by jpsalas, 2024, Version 1.0.0
' For more information about the table, please read the senna_tribute.pdf.
' Special thanks to:
' Carlos Guizzo for collecting all the graphics and sounds.
' Luis Culik for providing his images and sounds used in the original table.
' Ivo Vagner Simas for providing the backglass and cabinet illustrations he created.
' DanielZ for providing the playfield and plastics illustrations he created.
' Zé Americo for providing his voiceovers, including a special recording exclusive to this version.
' Halen for his work on the graphics and directb2s.
' ****************************************************

Option Explicit
Randomize

' Valores Constantes de las físicas de los flippers - se usan en la creación de las bolas, tienen que cargarse antes del core.vbs
Const BallSize = 50 ' el diametro de la bola. el tamaño normal es 50 unidades de VP
Const BallMass = 1  ' la pesadez de la bola, este valor va de acuerdo a la fuerza de los flippers y el plunger

' Carga el core.vbs para poder usas sus funciones, sobre todo el vpintimer.addtimer
LoadCoreFiles

Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    On Error Resume Next
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox "Can't open controller.vbs"
End Sub

'*****  PLAYER OPTIONS  ***********************************************

' color leds only for desktop view, the db2s must be changed manually
' Const Colorled = "red"
Const Colorled = "blue"

'**********************************************************************

' Valores Constants
Const TableName = "senna"
Const cGameName = "senna"
Const MaxPlayers = 4    ' de 1 a 4
Const BallsPerGame = 5  ' normalmente 3 ó 5
Const MaxBonusMultiplier = 5
Const MaxMultiballs = 3 ' max number of balls during multiballs
Const SongVolume = 1
Const Special1 = 620000 ' puntuación a obtener para partida extra
Const Special2 = 790000 ' puntuación a obtener para partida extra
Const Special3 = 960000 ' puntuación a obtener para partida extra

' Variables Globales
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim BallSaverTime ' in seconds of the first ball
Dim Bonus
Dim BonusMultiplier
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
Dim mBalls2Eject

' Variables de control
Dim BallsOnPlayfield

' Variables de tipo Boolean (verdadero ó falso, True ó False)
Dim bAttractMode
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bExtraBallWonThisBall
Dim bJustStarted
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim bBallSaverReady
Dim bMultiBallMode
Dim bMultiBallStarted(4)
Dim bMultiBallReady
Dim bAutoPlunger

Dim x

' core.vbs variables, como imanes, impulse plunger
Dim plungerIM 'used mostly as an autofire plunger during multiballs

' *********************************************************************
'                Rutinas comunes para todas las mesas
' *********************************************************************

Sub Table1_Init()

    ' Inicializar diversos objetos de la mesa, como droptargets, animations...
    VPObjects_Init
    LoadEM

    'Impulse Plunger as autoplunger
    Const IMPowerSetting = 45 ' Plunger Power
    Const IMTime = 0.5        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFXDOF("fx_kicker", 141, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 141, DOFPulse, DOFContactors)
        .CreateEvents "plungerIM"
    End With

    ' Carga los valores grabados highscore y créditos
    Loadhs
    'HighscoreReel.SetValue Highscore
    ' también pon el highscore en el reel del primer jugador
    ' ScoreReel1.SetValue Highscore
    ' en esta mesa el attractmode se encarga de mostrar el highscore

    UpdateCredits

    ' Juego libre o con monedas: si es True entonces no se usarán monedas
    bFreePlay = False 'queremos monedas

    ' Inicialiar las variables globales de la mesa
    bAttractMode = False
    bOnTheFirstBall = False
    BallSaverTime = 10 'at the start and multiball
    bBallSaverActive = False
    bBallSaverReady = False
    bGameInPlay = False
    bMultiBallMode = False
    bMultiBallReady = False
    bBallInPlungerLane = False
    bAutoPlunger = False
    BallsOnPlayfield = 0
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bJustStarted = True
    Add10 = 0
    Add100 = 0
    Add1000 = 0
    mBalls2Eject = 0

    'Enciende las luces GI despues de un segundo
    vpmtimer.addtimer 2500, "GiOn '"

    ' Handle color led options
    If Colorled = "blue" Then
        scorereel1.image = "leds2_taito_blue"
        scorereel2.image = "leds2_taito_blue"
        scorereel3.image = "leds2_taito_blue"
        scorereel4.image = "leds2_taito_blue"
        CreditsReel.image = "leds2_taito_blue"
        BipReel.image = "leds2_taito_blue"
    Else 'red
        scorereel1.image = "leds2_taito"
        scorereel2.image = "leds2_taito"
        scorereel3.image = "leds2_taito"
        scorereel4.image = "leds2_taito"
        CreditsReel.image = "leds2_taito"
        BipReel.image = "leds2_taito"
    End If

    ' quita reels
    If Table1.ShowDT = true then
        For each x in aReels
            x.Visible = 1
        Next
    else
        For each x in aReels
            x.Visible = 0
        Next
    end if
    LoadLUT

    PlaySound "vo_Machine_Startup"
    StartAttractMode
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)

    ' añade monedas
    If Keycode = AddCreditKey Then
        If(Tilted = False) Then
            AddCredits 1
            If Credits < 9 then
                PlaySound "fx_coin"
                PlaySound "sfx_F1_Radio_Notification"
            Else
                PlaySound "fx_coin2"
            End If
        End If
    End If

    ' el plunger
    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
    End If

    ' iluminación
    If keycode = LeftMagnaSave Then bLutActive = True:SetLUTLine "Color LUT image " & table1.ColorGradeImage
    If keycode = RightMagnaSave AND bLutActive Then NextLUT:End If

    ' Funcionamiento normal de los flipers y otras teclas durante el juego

    If bGameInPlay AND NOT Tilted Then
        ' teclas de la falta
        If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
        If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
        If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt
        If keycode = MechanicalTilt Then CheckTilt

        ' teclas de los flipers
        If keycode = LeftFlipperKey Then SolLFlipper 1
        If keycode = RightFlipperKey Then SolRFlipper 1

        ' tecla de empezar el juego
        If keycode = StartGameKey Then
            If((PlayersPlayingGame < MaxPlayers) AND(bOnTheFirstBall = True) ) Then

                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                Else
                    If(Credits> 0) then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        Credits = Credits - 1
                        UpdateCredits
                        UpdateBallInPlay
                    Else
                    ' no hay suficientes créditos para empezar el juego.
                    'PlaySound "sfx_nocredits"
                    End If
                End If
            End If
        End If
        Else ' If (GameInPlay)

            If keycode = StartGameKey Then
                If(bFreePlay = True) Then
                    If(BallsOnPlayfield = 0) Then
                        ResetScores
                        ResetForNewGame()
                    End If
                Else
                    If(Credits> 0) Then
                        If(BallsOnPlayfield = 0) Then
                            Credits = Credits - 1
                            UpdateCredits
                            ResetScores
                            ResetForNewGame()
                        End If
                    Else
                    ' Not Enough Credits to start a game.
                    'PlaySound "sfx_nocredits"
                    End If
                End If
            End If
    End If ' If (GameInPlay)
End Sub

Sub Table1_KeyUp(ByVal keycode)
    If keycode = LeftMagnaSave Then bLutActive = False:HideLUT

    If bGameInPlay AND NOT Tilted Then
        ' teclas de los flipers
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

'************************************
'       LUT - Darkness control
' 10 normal level & 10 warmer levels
'************************************

Dim bLutActive, LUTImage

Sub LoadLUT
    bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "") Then LUTImage = x Else LUTImage = 0
    UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT:LUTImage = (LUTImage + 1) MOD 22:UpdateLUT:SaveLUT:SetLUTLine "Color LUT image " & table1.ColorGradeImage:End Sub

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
GiIntensity = 1               'can be used by the LUT changing to increase the GI lights when the table is darker

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
    LUBack.imagea = "PostItNote"
    For xFor = 1 to 40
        Eval("LU" &xFor).imageA = GetHSChar(String, Index)
        Index = Index + 1
    Next
End Sub

Sub HideLUT
    SetLUTLine ""
    LUBack.imagea = "PostitBL"
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

'********************
'     Flippers
'********************

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToEnd
        LeftFlipperOn = 1
        RotateTopLightsLeft
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
        LeftFlipperOn = 0
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
        RightFlipper1.RotateToEnd
        RightFlipperOn = 1
        RotateTopLightsRight
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
        RightFlipper1.RotateToStart
        RightFlipperOn = 0
    End If
End Sub

' flippers top animations

Sub LeftFlipper_Animate:LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle:End Sub
Sub RightFlipper_Animate:RightFlipperTop.RotZ = RightFlipper.CurrentAngle:End Sub
Sub RightFlipper1_Animate:RightFlipperTop1.RotZ = RightFlipper1.CurrentAngle:End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper1_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
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
FullStrokeEOS_Torque = 0.3 ' EOS Torque when flipper hold up ( EOS Coil is fully charged. Ampere increase due to flipper can't move or when it pushed back when "On". EOS Coil have more power )
LiveStrokeEOS_Torque = 0.2 ' EOS Torque when flipper rotate to end ( When flipper move, EOS coil have less Ampere due to flipper can freely move. EOS Coil have less power )

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
    If LeftFlipper.CurrentAngle >= LeftFlipper.StartAngle - SOSAngle Then LeftFlipper.Strength = FlipperPower * SOSTorque else LeftFlipper.Strength = FlipperPower:End If

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
    If RightFlipper.CurrentAngle <= RightFlipper.StartAngle + SOSAngle Then RightFlipper.Strength = FlipperPower * SOSTorque else RightFlipper.Strength = FlipperPower:End If

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
' Luces GI
'*******************

Sub GiOn 'enciende las luces GI
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
    PlaySound "fx_gion"
    If B2SOn Then Controller.B2SSetData 60, 1
End Sub

Sub GiOff 'apaga las luces GI
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
    PlaySound "fx_gioff"
    If B2SOn Then Controller.B2SSetData 60, 0
End Sub

'**************
' TILT - Falta
'**************

'el "timer" TiltDecreaseTimer resta .01 de la variable "Tilt" cada ronda

Sub CheckTilt                     'esta rutina se llama cada vez que das un golpe a la mesa
    Tilt = Tilt + TiltSensitivity 'añade un valor al contador "Tilt"
    TiltDecreaseTimer.Enabled = True
    If Tilt> 15 Then              'Si la variable "Tilt" es más de 15 entonces haz falta
        StopSong
        PlaySound "vo_Tilt"
        Tilted = True
        l150.State = 1 'muestra Tilt en la pantalla
        If B2SOn then
            Controller.B2SSetTilt 1
        end if
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'empieza una pausa a fin de que todas las bolas se cuelen
        bMultiBallMode = False
        StopMBmodes
        BallSaverTimerExpired_Timer
    End If
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
        'Apaga todas las luces Gi de la mesa
        GiOff

        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        RightFlipper1.RotateToStart
        Bumper1.Threshold = 100
        Bumper2.Threshold = 100
        Bumper3.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        'enciende de nuevo todas las luces GI
        GiOn
        Bumper1.Threshold = 1
        Bumper2.Threshold = 1
        Bumper3.Threshold = 1
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' si todas las bolas se han colado, entonces ..
    If(BallsOnPlayfield = 0) Then
        bMultiBallMode = False
        '... haz el fin de bola normal
        EndOfBall()
        TiltRecoveryTimer.Enabled = False
    End If
' de lo contrario la rutina mirará si todavía hay bolas en la mesa
End Sub

' GI, light & flashers sequence effects

Sub GiEffect(n)
    Dim ii
    Select Case n
        Case 0 'all off
            LightSeqGi.Play SeqAlloff
        Case 1 'all blink
            LightSeqGi.UpdateInterval = 50
            LightSeqGi.Play SeqBlinking, , 8, 25
        Case 2 'random
            LightSeqGi.UpdateInterval = 25
            LightSeqGi.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqGi.UpdateInterval = 40
            LightSeqGi.Play SeqBlinking, , 10, 20
    End Select
End Sub

Sub LightEffect(n)
    Select Case n
        Case 0 ' all off
            LightSeqInserts.Play SeqAlloff
        Case 1 'all blink
            LightSeqInserts.UpdateInterval = 50
            LightSeqInserts.Play SeqBlinking, , 5, 10
        Case 2 'random
            LightSeqInserts.UpdateInterval = 40
            LightSeqInserts.Play SeqRandom, 25, , 1000
        Case 3 'all blink fast
            LightSeqInserts.UpdateInterval = 25
            LightSeqInserts.Play SeqBlinking, , 10, 10
        Case 4 'center - used in the bonus count
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqCircleOutOn, 15, 1
        Case 5 'top down
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqDownOn, 15, 2
        Case 6 'down to top
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 15, 1
    End Select
End Sub

'*****************************************
'         Internal Music
'*****************************************

Dim Song
Song = ""

Sub PlaySong(name)
    If Song <> name Then
        StopSound Song
        Song = name
        PlaySound Song, -1, SongVolume
    End If
End Sub

Sub ChangeSong
End Sub

Sub StopSong
    StopSound Song
    Song = ""
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
    If tmp> 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = (SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / TableHeight-1
    If tmp> 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.2, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAt2(soundname, tableobj) 'play sound at X and Y position of an object with less random, like flippers
    PlaySound soundname, 0, 1, Pan(tableobj), 0.05, 0, 0, 0, AudioFade(tableobj)
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
Const maxvel = 32 'max ball velocity
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
    RollingTimer.Enabled = 1
End Sub

Sub RollingTimer_Timer()
    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
        aBallShadow(b).Y = 1930
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there are no more balls on the playfield

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y
        aBallShadow(b).Height = BOT(b).Z - Ballsize / 2

        If BallVel(BOT(b) )> 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b) )
                ballvol = Vol(BOT(b) )
            Else
                ballpitch = Pitch(BOT(b) ) + 50000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b) ) * 5
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b) ), 0, ballpitch, 1, 0, AudioFade(BOT(b) )
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z> 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        End If

        ' jps ball speed & spin control
        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            BOT(b).AngMomZ = BOT(b).AngMomZ * 0.95
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

'*****************************
' Sonido de las bolas chocando
'*****************************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound "fx_collide", 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'***************************************
' Sonidos de las colecciones de objetos
' como metales, gomas, plásticos, etc
'***************************************

Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
Sub aMetalWires_Hit(idx):PlaySoundAtBall "fx_MetalWire":End Sub
Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
Sub aRubber_LongBands_Hit(idx):PlaySoundAtBall "fx_rubber_longband":End Sub
Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
Sub aRubber_Pegs_Hit(idx):PlaySoundAtBall "fx_rubber_peg":End Sub
Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub

'************************************************************************************************************************
' Solo para VPX 10.2 y posteriores.
' FlashForMs hará parpadear una luz o un flash por unos milisegundos "TotalPeriod" cada tantos milisegundos "BlinkPeriod"
' Cuando el "TotalPeriod" haya terminado, la luz o el flasher se pondrá en el estado especificado por el valor "FinalState"
' El valor de "FinalState" puede ser: 0=apagado, 1=encendido, 2=regreso al estado anterior
'************************************************************************************************************************

Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState)

    If TypeName(MyLight) = "Light" Then ' la luz es del tipo "light"

        If FinalState = 2 Then
            FinalState = MyLight.State  'guarda el estado actual de la luz
        End If
        MyLight.BlinkInterval = BlinkPeriod
        MyLight.Duration 2, TotalPeriod, FinalState
    ElseIf TypeName(MyLight) = "Flasher" Then ' la luz es del tipo "flash"
        Dim steps
        ' Store all blink information
        steps = Int(TotalPeriod / BlinkPeriod + .5) 'número de encendidos y apagados que hay que ejecutar
        If FinalState = 2 Then                      'guarda el estado actual del flash
            FinalState = ABS(MyLight.Visible)
        End If
        MyLight.UserValue = steps * 10 + FinalState 'guarda el número de parpadeos

        ' empieza los parpadeos y crea la rutina que se va a ejecutar como un timer que se va a ejecutar los parpadeos
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

    'pone a cero los marcadores y apaga las luces de espera.
    StopAttractMode
    If B2SOn then
        Controller.B2SSetGameOver 0
    end if
    ' enciende las luces GI si estuvieran apagadas
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
    Next
    Bonus = 0
    BonusMultiplier = 1
    UpdateBallInPlay

    ' Clear Match

    ' inicializa otras variables
    Tilt = 0

    ' inicializa las variables del juego
    Game_Init()

    ' ahora puedes empezar una música si quieres
    ' empieza la rutina "Firstball" despues de una pequeña pausa
    vpmtimer.addtimer 2000, "FirstBall '"
End Sub

' esta pausa es para que la mesa tenga tiempo de poner los marcadores a cero y actualizar las luces

Sub FirstBall
    'debug.print "FirstBall"
    ' ajusta la mesa para una bola nueva, sube las dianas abatibles, etc
    ResetForNewPlayerBall()
    ' crea una bola nueva en la zona del plunger
    CreateNewBall()
End Sub

' (Re-)inicializa la mesa para una bola nueva, tanto si has perdido la bola, oe le toca el turno al otro jugador

Sub ResetForNewPlayerBall()
    'debug.print "ResetForNewPlayerBall"
    ' Se asegura que los marcadores están activados para el jugador de turno
    AddScore 0

    ' ajusta el multiplicador del bonus multiplier a 1X (si hubiese multiplicador en la mesa)
    BonusMultiplier = 1
    UpdateBonusXLights

    ' enciende las luces, reinicializa las variables del juego, etc
    bExtraBallWonThisBall = False
    ResetNewBallLights
    ResetNewBallVariables

    'This is a new ball, so activate the ballsaver
    bBallSaverReady = True
End Sub

' Crea una bola nueva en la mesa

Sub CreateNewBall()
    ' crea una bola nueva basada en el tamaño y la masa de la bola especificados al principio del script
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass

    ' incrementa el número de bolas en el tablero, ya que hay que contarlas
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' actualiza las luces del backdrop
    UpdateBallInPlay

    ' y expulsa la bola
    PlaySoundAt SoundFXDOF("fx_Ballrel", 104, DOFPulse, DOFContactors), BallRelease
    'PlaySound"sfx_NewBall"
    BallRelease.Kick 90, 4

' if there is 2 or more balls Then set the multibal flag (remember to check for locked balls and other balls used for animations)
' set the bAutoPlunger flag to kick the ball in play automatically
    If BallsOnPlayfield> 1 Then
        bMultiBallMode = True
        bAutoPlunger = True
    End If
End Sub

' Add extra balls to the table with autoplunger
' Use it as AddMultiball 4 to add 4 extra balls to the table

Sub AddMultiball(nballs)
    mBalls2Eject = mBalls2Eject + nballs
    CreateMultiballTimer.Enabled = True
    'and eject the first ball
    CreateMultiballTimer_Timer
End Sub

' Eject the ball after the delay, AddMultiballDelay
Sub CreateMultiballTimer_Timer()
    ' wait if there is a ball in the plunger lane
    If bBallInPlungerLane Then
        Exit Sub
    Else
        If BallsOnPlayfield < MaxMultiballs Then
            CreateNewBall()
            mBalls2Eject = mBalls2Eject -1
            If mBalls2Eject = 0 Then 'if there are no more balls to eject Then stop the timer
                CreateMultiballTimer.Enabled = False
            End If
        Else 'the max number of multiballs is reached, so stop the timer
            mBalls2Eject = 0
            CreateMultiballTimer.Enabled = False
        End If
    End If
End Sub

' El jugador ha perdido su bola, y ya no hay más bolas en juego
' Empieza a contar los bonos

Sub EndOfBall()
    'debug.print "EndOfBall"
    StopSong
    ' La primera se ha perdido. Desde aquí ya no se puede aceptar más jugadores
    bOnTheFirstBall = False

    ' solo recoge los bonos si no hay falta
    ' el sistema del la falta se encargará de nuevas bolas o del fin de la partida

    If NOT Tilted Then
        BonusCountTimer.Interval = 100
        CreditLight.State = 2 'taito tables
        PlaySong "mu_bonus"
        BonusCountTimer.Enabled = 1
    Else 'Si hay falta simplemente espera un momento y va directo a la segunta parte después de perder la bola
        vpmtimer.addtimer 400, "EndOfBall2 '"
    End If
End Sub

Sub BonusCountTimer_Timer 'añade los bonos y actualiza las luces
    'debug.print "BonusCount_Timer"
    If Bonus> 0 Then
        PlaySound "sfx_bonus"
        Bonus = Bonus -1
        AddScore 1000 * BonusMultiplier
        UpdateBonusLights
    Else
        ' termina la cuenta de los bonos y continúa con el fin de bola
        CreditLight.State = 0 'Taito tables
        BonusCountTimer.Enabled = 0
        vpmtimer.addtimer 1000, "EndOfBall2 '"
    End If
End Sub

' La cuenta de los bonos ha terminado. Mira si el jugador ha ganado bolas extras
' y si no mira si es el último jugador o la última bola
'
Sub EndOfBall2()
    'debug.print "EndOfBall2"
    ' si hubiese falta, quítala, y pon la cuenta a cero de la falta para el próximo jugador, ó bola

    Tilted = False
    Tilt = 0
    l150.State = 0
    If B2SOn then
        Controller.B2SSetTilt 0
    end if
    DisableTable False 'activa de nuevo los bumpers y los slingshots

    ' ¿ha ganado el jugador una bola extra?
    If(ExtraBallsAwards(CurrentPlayer)> 0) Then
        'debug.print "Extra Ball"

        ' sí? entonces se la das al jugador
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1

        ' si no hay más bolas apaga la luz de jugar de nuevo
        If(ExtraBallsAwards(CurrentPlayer) = 0) Then
            LightShootAgain.State = 0
            If B2SOn then
                Controller.B2SSetShootAgain 0
            end if
        End If

' aquí se podría poner algún sonido de bola extra o alguna luz que parpadee

' En esta mesa hacemos la bola extra igual como si fuese la siguente bola, haciendo un reset de las variables y dianas
        ResetForNewPlayerBall()

        ' creamos una bola nueva en el pasillo de disparo
        CreateNewBall()
    Else ' no hay bolas extras

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1

        ' ¿Es ésta la última bola?
        If(BallsRemaining(CurrentPlayer) <= 0) Then

            ' miramos si la puntuación clasifica como el Highscore
            CheckHighScore()
        End If

        ' ésta no es la última bola para éste jugador
        ' y si hay más de un jugador continúa con el siguente
        EndOfBallComplete()
    End If
End Sub

' Esta rutina se llama al final de la cuenta del bonus
' y pasa a la siguente bola o al siguente jugador
'
Sub EndOfBallComplete()
    'debug.print "EndOfBallComplete"
    Dim NextPlayer

    'debug.print "EndOfBall - Complete"

    ' ¿hay otros jugadores?
    If(PlayersPlayingGame> 1) Then
        ' entonces pasa al siguente jugador
        NextPlayer = CurrentPlayer + 1
        ' ¿vamos a pasar del último jugador al primero?
        ' (por ejemplo del jugador 4 al no. 1)
        If(NextPlayer> PlayersPlayingGame) Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'debug.print "Next Player = " & NextPlayer

    ' ¿Hemos llegado al final del juego? (todas las bolas se han jugado de todos los jugadores)
    If((BallsRemaining(CurrentPlayer) <= 0) AND(BallsRemaining(NextPlayer) <= 0) ) Then

        ' aquí se empieza la lotería, normalmente cuando se juega con monedas
        If bFreePlay = False Then
            'match
            vpmtimer.AddTimer 4000, "Verification_Match '"
        Else
            ' ahora se pone la mesa en el modo de final de juego
            EndOfGame()
        End If
    Else
        ' pasamos al siguente jugador
        CurrentPlayer = NextPlayer

        ' nos aseguramos de que el backdrop muestra el jugador actual
        AddScore 0

        ' hacemos un reset del la mesa para el siguente jugador (ó bola)
        ResetForNewPlayerBall()

        ' y sacamos una bola
        CreateNewBall()

        ' play a sound if more than 1 player
        If PlayersPlayingGame> 1 Then
            PlaySound "vo_Pilot" &CurrentPlayer
        End If
    End If
End Sub

' Esta función se llama al final del juego

Sub EndOfGame()
    'debug.print "EndOfGame"
    StopSong
    bGameInPLay = False
    bJustStarted = False
    If B2SOn then
        Controller.B2SSetGameOver 1
        Controller.B2SSetBallInPlay 0
        Controller.B2SSetPlayerUp 0
        Controller.B2SSetCanPlay 0
    end if
    ' asegúrate de que los flippers están en modo de reposo
    SolLFlipper 0
    SolRFlipper 0

    ' pon las luces en el modo de fin de juego
    PlaySound "vo_Game_Over"
    StartAttractMode
End Sub

' Esta función calcula el no de bolas que quedan
Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp> BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
    End If
End Function

' Esta función calcula el Highscore y te da una partida gratis si has conseguido el Highscore
Sub CheckHighscore
    For x = 1 to PlayersPlayingGame
        If Score(x)> Highscore Then
            l80.State = 1
            l70.State = 1
            If B2SOn then
                Controller.B2SSetData 70, 1
                Controller.B2SSetData 80, 1
            End If
            Highscore = Score(x)
            PlaySound "vo_New_Record"
            AddCredits 1
        End If
    Next
End Sub

'******************
'     Match
'******************

Dim M1, M2

Sub Verification_Match()
    PlaySound "vo_Game_Draw"
    'start animation
    Match1.Enabled = 1
    Match2.Enabled = 1
    M2 = Score(CurrentPlayer) MOD 10
    M1 = M2
End Sub

Sub Match1_Timer
    M1 = M1 + 100000
    scorereel1.SetValue M1
    If B2SOn then
        Controller.B2SSetScorePlayer1 M1
    End If
End Sub

Sub Match2_Timer
    Match1.Enabled = 0
    Match2.Enabled = 0
    Verification_Match2
End Sub

Sub Verification_Match2
    Match = INT(RND(1) * 10) ' random between 0 and 9
    M1 = Match * 100000 + M2
    scorereel1.SetValue M1
    If B2SOn then
        Controller.B2SSetScorePlayer1 M1
    End If
    If M2 = Match Then
        PlaySound SoundFXDOF("fx_knocker", 122, DOFPulse, DOFknocker)
        AddCredits 1
    End If
    vpmtimer.AddTimer 4500, "EndOfGame '"
End Sub

' *********************************************************************
'                      Drain / Plunger Functions
' *********************************************************************

' has perdido la bola ;-( mira cuantas bolas hay en el tablero.
' si solamente hay una entonces reduce el número de bola y mira si es la última para finalizar el juego
' si hay más de una, significa que hay multiball, entonces continua con la partida
'
Sub Drain_Hit()
    ' destruye la bola
    Drain.DestroyBall

    BallsOnPlayfield = BallsOnPlayfield - 1

    ' haz sonar el ruido de la bola
    PlaySoundAt "fx_drain", Drain

    'si hay falta el systema de tilt se encargará de continuar con la siguente bola/jugador
    If Tilted Then
        Exit Sub
    End If

    ' si estás jugando y no hay falta
    If(bGameInPLay = True) AND(Tilted = False) Then

        ' ¿está el salva bolas activado?
        If(bBallSaverActive = True) Then

            ' ¿sí?, pues creamos una bola
            ' we use the Addmultiball in case the multiballs are being ejected
            AddMultiball 1
            ' we kick the ball with the autoplunger
            bAutoPlunger = True
            ' you may wish to put something on a display or play a sound at this point
            ' stop the ballsaver timer during the launch ball saver time, but not during multiballs
            If NOT bMultiBallMode Then
                BallSaverTimerExpired_Timer
            End If
        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
            If(BallsOnPlayfield = 1) Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True) Then
                    ' not in multiball mode any more
                    bMultiBallMode = False
                    ' you may wish to change any music over at this point and
                    PlaySong "mu_F1_McLaren"
                    ' turn off any multiball specific lights
                    ChangeGIIntensity 1
                    'stop any multiball modes of this game
                    StopMBmodes
                End If
            End If
            ' ¿es ésta la última bola en juego?
            If(BallsOnPlayfield = 0) Then
                vpmtimer.addtimer 500, "EndOfBall '" 'hacemos una pequeña pausa anter de continuar con el fin de bola
                Exit Sub
            End If
        End If
    End If
End Sub

Sub swPlungerRest_Hit()
    If bOnTheFirstBall And CurrentPlayer = 1 Then
        StopSound "vo_Machine_Startup"
        PlaySound "vo_Credits_Inserted"
    End If
    bBallInPlungerLane = True
    If BallsRemaining(CurrentPlayer) = 1 AND bMultiBallStarted(CurrentPlayer) = False Then
        AddMultiball 2
        bAutoPlunger = True
    End If

    If bMultiBallMode Then
        bAutoPlunger = True ' kick the ball in play if the bAutoPlunger flag is on
    End If
    ' kick the ball in play if the bAutoPlunger flag is on
    If bAutoPlunger Then
        'debug.print "autofire the ball"
        vpmtimer.addtimer 1500, "PlungerIM.AutoFire:PlaySoundAt ""fx_kicker"", swPlungerRest:bAutoPlunger = False '"
    End If
End Sub

' La bola ha sido disparada, así que cambiamos la variable, que en esta mesa se usa solo para que el sonido del disparador cambie según hay allí una bola o no
' En otras mesas podrá usarse para poner en marcha un contador para salvar la bola

Sub swPlungerRest_UnHit()
    bBallInPlungerLane = False
    If bMultiBallMode Then
        PlaySong "mu_Multiball"
    Else
        PlaySong "mu_F1_McLaren"
    End If
    ' if there is a need for a ball saver, Then start off a timer
    ' only start if it is ready, and it is currently not running, else it will reset the time period
    If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
        EnableBallSaver BallSaverTime
    End If
End Sub

Sub EnableBallSaver(seconds)
    'debug.print "Ballsaver started"
    ' set our game flag
    bBallSaverActive = True
    bBallSaverReady = False
    ' stop the ball saver grace period if it was started
    BallSaverGracePeriod.Enabled = 0
    ' start the timers
    BallSaverTimerExpired.Interval = 1000 * seconds
    BallSaverTimerExpired.Enabled = False
    BallSaverTimerExpired.Enabled = True
    BallSaverSpeedUpTimer.Interval = 1000 * (seconds) - 3000 'the last 3 seconds will the light blink faster
    BallSaverSpeedUpTimer.Enabled = False
    BallSaverSpeedUpTimer.Enabled = True
    ' if you have a ball saver light you might want to turn it on at this point (or make it flash)
    LightShootAgain.BlinkInterval = 160
    LightShootAgain.State = 0
    LightShootAgain.State = 2
End Sub

' The ball saver timer has expired.  Turn it off AND reset the game flag
'
Sub BallSaverTimerExpired_Timer()
    'debug.print "Ballsaver ended"
    BallSaverTimerExpired.Enabled = False
    BallSaverSpeedUpTimer.Enabled = False 'ensure this timer is also stopped
    ' clear the flag after a few seconds of grace period
    BallSaverGracePeriod.Interval = 1500
    BallSaverGracePeriod.Enabled = 0 'restart the grace period timer
    BallSaverGracePeriod.Enabled = 1
    ' if you have a ball saver light Then turn it off at this point
    LightShootAgain.State = 0
    ' if the table uses the same lights for the extra ball or replay Then turn them on if needed
    If ExtraBallsAwards(CurrentPlayer)> 0 Then
        LightShootAgain.State = 1
    End If
End Sub

Sub BallSaverGracePeriod_Timer 'grace period 1,5 seconds
    bBallSaverActive = False   '"
    BallSaverGracePeriod.Enabled = 0
End Sub

Sub BallSaverSpeedUpTimer_Timer()
    'debug.print "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    LightShootAgain.BlinkInterval = 80
    LightShootAgain.State = 2
End Sub

' *********************************************************************
'               Funciones para la cuenta de los puntos
' *********************************************************************

' Añade puntos al jugador, hace sonar las campanas y actualiza el backdrop

Sub AddScore(Points)
    If Tilted Then Exit Sub
    ' añade los puntos a la variable del actual jugador
    Score(CurrentPlayer) = Score(CurrentPlayer) + points
    ' actualiza los contadores
    UpdateScore points
    ' aquí se puede hacer un chequeo si el jugador ha ganado alguna puntuación alta y darle un crédito ó bola extra
    If Score(CurrentPlayer) >= Special1 AND Special1Awarded(CurrentPlayer) = False Then
        PlaySound SoundFXDOF("fx_knocker", 122, DOFPulse, DOFknocker)
        AddCredits 1
        Special1Awarded(CurrentPlayer) = True
    End If
    If Score(CurrentPlayer) >= Special2 AND Special2Awarded(CurrentPlayer) = False Then
        PlaySound SoundFXDOF("fx_knocker", 122, DOFPulse, DOFknocker)
        AddCredits 1
        Special2Awarded(CurrentPlayer) = True
    End If
    If Score(CurrentPlayer) >= Special3 AND Special3Awarded(CurrentPlayer) = False Then
        PlaySound SoundFXDOF("fx_knocker", 122, DOFPulse, DOFknocker)
        AddCredits 1
        Special3Awarded(CurrentPlayer) = True
    End If
End Sub

'*******************
'     BONOS
'*******************

Sub AddBonus(bonuspoints)
    If(Tilted = False) Then
        ' añade los bonos al jugador actual
        Bonus = Bonus + bonuspoints
        If Bonus> 99 Then
            Bonus = 99
        End If
        ' actualiza las luces
        UpdateBonusLights
    End if
End Sub

Sub UpdateBonusLights 'enciende o apaga las luces de los bonos según la variable "Bonus"
    Select Case Bonus MOD 10
        Case 0:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0
        Case 1:bl1.State = 1:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0
        Case 2:bl1.State = 1:bl2.State = 1:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0
        Case 3:bl1.State = 1:bl2.State = 1:bl3.State = 1:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0
        Case 4:bl1.State = 1:bl2.State = 1:bl3.State = 1:bl4.State = 1:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0
        Case 5:bl1.State = 1:bl2.State = 1:bl3.State = 1:bl4.State = 1:bl5.State = 1:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0
        Case 6:bl1.State = 1:bl2.State = 1:bl3.State = 1:bl4.State = 1:bl5.State = 1:bl6.State = 1:bl7.State = 0:bl8.State = 0:bl9.State = 0
        Case 7:bl1.State = 1:bl2.State = 1:bl3.State = 1:bl4.State = 1:bl5.State = 1:bl6.State = 1:bl7.State = 1:bl8.State = 0:bl9.State = 0
        Case 8:bl1.State = 1:bl2.State = 1:bl3.State = 1:bl4.State = 1:bl5.State = 1:bl6.State = 1:bl7.State = 1:bl8.State = 1:bl9.State = 0
        Case 9:bl1.State = 1:bl2.State = 1:bl3.State = 1:bl4.State = 1:bl5.State = 1:bl6.State = 1:bl7.State = 1:bl8.State = 1:bl9.State = 1
    End Select
    Select Case Bonus \ 10
        Case 0:bl10.State = 0:bl20.State = 0:bl40.State = 0:bl60.State = 0:bl80.State = 0
        Case 1:bl10.State = 1:bl20.State = 0:bl40.State = 0:bl60.State = 0:bl80.State = 0
        Case 2:bl10.State = 0:bl20.State = 1:bl40.State = 0:bl60.State = 0:bl80.State = 0
        Case 3:bl10.State = 1:bl20.State = 1:bl40.State = 0:bl60.State = 0:bl80.State = 0
        Case 4:bl10.State = 0:bl20.State = 0:bl40.State = 1:bl60.State = 0:bl80.State = 0
        Case 5:bl10.State = 1:bl20.State = 0:bl40.State = 1:bl60.State = 0:bl80.State = 0
        Case 6:bl10.State = 0:bl20.State = 0:bl40.State = 0:bl60.State = 1:bl80.State = 0
        Case 7:bl10.State = 1:bl20.State = 0:bl40.State = 0:bl60.State = 1:bl80.State = 0
        Case 8:bl10.State = 0:bl20.State = 0:bl40.State = 0:bl60.State = 0:bl80.State = 1
        Case 9:bl10.State = 1:bl20.State = 0:bl40.State = 0:bl60.State = 0:bl80.State = 1
    End Select
End Sub

Sub AddBonusMultiplier(n)
    ' if not at the maximum bonus level
    if BonusMultiplier + n <= MaxBonusMultiplier then
        ' then add and set the lights
        PlaySOund "sfx_Xbonus"
        BonusMultiplier = BonusMultiplier + n
        UpdateBonusXLights
    End if
End Sub

Sub UpdateBonusXLights '4 lights in this table, from 2x to 5x
    ' Update the lights
    Select Case BonusMultiplier
        Case 1:bxl2.State = 0:bxl3.State = 0:bxl4.State = 0:bxl5.State = 0
        Case 2:bxl2.State = 1:bxl3.State = 0:bxl4.State = 0:bxl5.State = 0
        Case 3:bxl2.State = 1:bxl3.State = 1:bxl4.State = 0:bxl5.State = 0
        Case 4:bxl2.State = 1:bxl3.State = 1:bxl4.State = 1:bxl5.State = 0
        Case 5:bxl2.State = 1:bxl3.State = 1:bxl4.State = 1:bxl5.State = 1
    End Select
End Sub

'***********************************************************************************
'        Score reels - puntuaciones - y actualiza otras luces del backdrop
'***********************************************************************************
'esta es al rutina que actualiza la puntuación del jugador

Sub UpdateScore(playerpoints)
    Select Case CurrentPlayer
        Case 1:ScoreReel1.Addvalue playerpoints
        Case 2:ScoreReel2.Addvalue playerpoints
        Case 3:ScoreReel3.Addvalue playerpoints
        Case 4:ScoreReel4.Addvalue playerpoints
    End Select
    If B2SOn then
        Controller.B2SSetScorePlayer CurrentPlayer, Score(CurrentPlayer)
        If Score(CurrentPlayer) >= 100000 then
            Controller.B2SSetScoreRollover 24 + CurrentPlayer, 1
        end if
    end if
End Sub

' pone todos los marcadores a 0
Sub ResetScores
    ScoreReel1.ResetToZero
    ScoreReel2.ResetToZero
    ScoreReel3.ResetToZero
    ScoreReel4.ResetToZero
    If B2SOn then
        Controller.B2SSetScorePlayer1 0
        Controller.B2SSetScorePlayer2 0
        Controller.B2SSetScorePlayer3 0
        Controller.B2SSetScorePlayer4 0
        Controller.B2SSetScoreRolloverPlayer1 0
        Controller.B2SSetScoreRolloverPlayer2 0
        Controller.B2SSetScoreRolloverPlayer3 0
        Controller.B2SSetScoreRolloverPlayer4 0
    end if
End Sub

Sub AddCredits(value)
    If Credits < 9 Then
        Credits = Credits + value
        CreditsReel.SetValue Credits
        UpdateCredits
    end if
End Sub

Sub UpdateCredits
    If Credits> 0 Then 'this is for Bally tables
    'CreditLight.State = 1
    Else
    'CreditLight.State = 0
    End If
    CreditsReel.SetValue credits
    If B2SOn then
        Controller.B2SSetCredits Credits
    End If
End Sub

Sub UpdateBallInPlay 'actualiza los marcadores de las bolas, el número de jugador y el número total de jugadores
    BipReel.SetValue Balls
    If B2SOn then
        Controller.B2SSetScorePlayer5 Balls
    end if

    Select Case CurrentPlayer
        Case 1:pl1.State = 1:pl2.State = 0:pl3.State = 0:pl4.State = 0
        Case 2:pl1.State = 0:pl2.State = 1:pl3.State = 0:pl4.State = 0
        Case 3:pl1.State = 0:pl2.State = 0:pl3.State = 1:pl4.State = 0
        Case 4:pl1.State = 0:pl2.State = 0:pl3.State = 0:pl4.State = 1
    End Select
    If B2SOn then
        Controller.B2SSetPlayerUp CurrentPlayer
    end if

    If Table1.ShowDT Then
        Select Case PlayersPlayingGame
            Case 0:scorereel1.Visible = 0:scorereel2.Visible = 0:scorereel3.Visible = 0:scorereel4.Visible = 0
            Case 1:scorereel1.Visible = 1:scorereel2.Visible = 0:scorereel3.Visible = 0:scorereel4.Visible = 0
            Case 2:scorereel1.Visible = 1:scorereel2.Visible = 1:scorereel3.Visible = 0:scorereel4.Visible = 0
            Case 3:scorereel1.Visible = 1:scorereel2.Visible = 1:scorereel3.Visible = 1:scorereel4.Visible = 0
            Case 4:scorereel1.Visible = 1:scorereel2.Visible = 1:scorereel3.Visible = 1:scorereel4.Visible = 1
        End Select
    End If
    If B2SOn then
        Controller.B2SSetCanPlay PlayersPlayingGame
    end if
End Sub

'*************************
' Partidas y bolas extras
'*************************

Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
        PlaySound "vo_Extra_Ball_Awarded"
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
        LightShootAgain.State = 1
        If B2SOn then
            Controller.B2SSetShootAgain 1
        end if
    END If
End Sub

Sub AwardSpecial()
    PlaySound "vo_Special_Awarded"
    AddCredits 1
End Sub

' ********************************
'        Attract Mode
' ********************************
' las luces simplemente parpadean de acuerdo a los valores que hemos puesto en el "Blink Pattern" de cada luz

Dim AttractStep, CircuitStep, VoiceCount

Sub StartAttractMode()
    bAttractMode = True
    For each x in aLights
        x.State = 2
    Next
    AttractStep = 0
    CircuitStep = 0
    VoiceCount = 0
    ' enciente la luz de fin de partida
    l149.State = 1
    ' empieza un timer
    AttractTimer.Enabled = 1
    AttractTimer2.Enabled = 1
    If B2SOn then
        Controller.B2SSetGameOver 1
        Controller.B2SSetBallInPlay 0
        Controller.B2SSetPlayerUp 0
        Controller.B2SSetCanPlay 0
    end if
End Sub

Sub StopAttractMode()
    bAttractMode = False
    ResetScores
    For each x in aLights
        x.State = 0
    Next

    ' apaga la luz de fin de partida
    l149.State = 0
    ' apaga timer
    AttractTimer.Enabled = 0
    AttractTimer2.Enabled = 0
End Sub

Sub AttractTimer_Timer
    Select Case AttractStep
        Case 0
            l70.State = 0
            AttractStep = 1
            ScoreReel1.SetValue Score(1)
            ScoreReel2.SetValue Score(2)
            ScoreReel3.SetValue Score(3)
            ScoreReel4.SetValue Score(4)
            If B2SOn then
                Controller.B2SSetData 70, 0
                Controller.B2SSetScorePlayer1 Score(1)
                Controller.B2SSetScorePlayer2 Score(2)
                Controller.B2SSetScorePlayer3 Score(3)
                Controller.B2SSetScorePlayer4 Score(4)
            end if
        Case 1
            l70.State = 1
            AttractStep = 0
            ScoreReel1.SetValue Highscore
            ScoreReel2.SetValue Highscore
            ScoreReel3.SetValue Highscore
            ScoreReel4.SetValue Highscore
            If B2SOn then
                Controller.B2SSetData 70, 1
                Controller.B2SSetScorePlayer1 Highscore
                Controller.B2SSetScorePlayer2 Highscore
                Controller.B2SSetScorePlayer3 Highscore
                Controller.B2SSetScorePlayer4 Highscore
            end if
    End Select
    UpdateCredits
End Sub

Sub AttractTimer2_Timer 'circuit lights
    ' Backdrop Lights
    Select Case CircuitStep
        Case 0:C8.State = 0:C1.State = 1
            If B2SOn then
                Controller.B2SSetData 78, 0
                Controller.B2SSetData 71, 1
            End If
        Case 1:C1.State = 0:C2.state = 1
            If B2SOn then
                Controller.B2SSetData 71, 0
                Controller.B2SSetData 72, 1
            End If
        Case 2:C2.State = 0:C3.state = 1
            If B2SOn then
                Controller.B2SSetData 72, 0
                Controller.B2SSetData 73, 1
            End If
        Case 3:C3.State = 0:C4.state = 1
            If B2SOn then
                Controller.B2SSetData 73, 0
                Controller.B2SSetData 74, 1
            End If
        Case 4:C4.State = 0:C5.state = 1
            If B2SOn then
                Controller.B2SSetData 74, 0
                Controller.B2SSetData 75, 1
            End If
        Case 5:C5.State = 0:C6.state = 1
            If B2SOn then
                Controller.B2SSetData 75, 0
                Controller.B2SSetData 76, 1
            End If
        Case 6:C6.State = 0:C7.state = 1
            If B2SOn then
                Controller.B2SSetData 76, 0
                Controller.B2SSetData 77, 1
            End If
        Case 7:C7.State = 0:C8.state = 1
            If B2SOn then
                Controller.B2SSetData 77, 0
                Controller.B2SSetData 78, 1
            End If
            VoiceCount = (VoiceCount + 1) MOD 15
            If VoiceCount = 0 Then
                PlaySound "vo_Attract_Mode"
            End If
    End Select
    CircuitStep = (CircuitStep + 1) MOD 8
End Sub

'************************************************
'    Load (cargar) / Save (guardar)/ Highscore
'************************************************

' solamente guardamos el número de créditos y la puntuación más alta

Sub Loadhs
    x = LoadValue(TableName, "HighScore")
    If(x <> "") Then HighScore = CDbl(x) Else HighScore = 0 End If
    x = LoadValue(TableName, "Credits")
    If(x <> "") then Credits = CInt(x) Else Credits = 0 End If
End Sub

Sub Savehs
    SaveValue TableName, "HighScore", HighScore
    SaveValue TableName, "Credits", Credits
End Sub

' por si se necesitara quitar la actual puntuación más alta, se le puede poner a una tecla,
' o simplemente abres la ventana de debug y escribes Reseths y le das al enter
Sub Reseths
    HighScore = 0
    Savehs
End Sub

'***********************************************************************
' *********************************************************************
'            Aquí empieza el código particular a la mesa
' (hasta ahora todas las rutinas han sido muy generales para todas las mesas)
' (y hay muy pocas rutinas que necesitan cambiar de mesa a mesa)
' *********************************************************************
'***********************************************************************

' se inicia las dianas abatibles, primitivas, etc.
' aunque en el VPX no hay muchos objetos que necesitan ser iniciados

Sub VPObjects_Init
End Sub

' variables de la mesa
Dim AYRPos(4)
Dim TONPos(4)
Dim HolePos(4)
Dim AYRDroppedCount(4)
Dim TONDroppedCount(4)
Dim SennaDroppedCount(4)
Dim SennaWonCount(4)
Dim bCircuitReady(4)

Sub Game_Init() 'esta rutina se llama al principio de un nuevo juego
    Dim i

    'Empezar alguna música, si hubiera música en esta mesa

    'Iniciar algunas luces
    TurnOffPlayfieldLights()
    'turn off the new record if it was on on the db2s
    l70.State = 0
    l80.State = 0
    If B2SOn then
        Controller.B2SSetData 70, 0
        Controller.B2SSetData 80, 0
    End If
    'iniciar variables, en esta mesa hay muy pocas variables ya que usamos las luces, y el UserValue de las dianas
    For i = 0 to 4
        AYRDroppedCount(i) = 0
        SennaDroppedCount(i) = 0
        SennaWonCount(i) = 0
        TONDroppedCount(i) = 0
        AYRPos(i) = 1
        TONPos(i) = 0
        HolePos(i) = 0
        bMultiBallStarted(i) = False
        bCircuitReady(i) = False
    Next
    UpdateAYR
    UpdateTON
'iniciar algún timer

End Sub

Sub StopMBmodes             'stop multiball modes after loosing the last multiball
End Sub

Sub ResetNewBallVariables() 'inicia las variable para una bola ó jugador nuevo
    'reset the droptargets
    ResetSenna
    vpmTimer.AddTimer 500, "ResetAYR '"
    vpmTimer.AddTimer 1000, "ResetTON '"
    ' update circuit lights for the new playerpoints
    UpdateWonCircuit
    UpdateCircuit
End Sub

Sub ResetNewBallLights() 'enciende ó apaga las luces para una bola nueva
    TurnOffPlayfieldLights()
    'top Lights & bumper lights
    ResetTopLights
    'pit Stop
    PitStopTimer_Timer 'stop the timer if it was on, turns on the pit stop lights
    'update droptarget lights
    UpdateAYR
    UpdateTON
    CheckAYRTON
    UpdateHole
    ' reset X bonus multi
    BonusMultiplier = 1
    UpdateBonusXLights
End Sub

Sub TurnOffPlayfieldLights()
    For each x in aLights
        x.State = 0
    Next
End Sub

' *********************************************************************
'       Eventos de la mesa - choque de la bola contra dianas
'
' En cada diana u objeto con el que la bola choque habrá que hacer:
' - sonar un sonido físico
' - hacer algún movimiento, si es necesario
' - añadir alguna puntuación
' - encender/apagar una luz
' - hacer algún chequeo para ver si el jugador ha completado algo
' *********************************************************************

'***************
'  Slingshots
'***************
Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt "fx_slingshot", Lemk
    PlaySound "sfx_Sling2"
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
    ' add points
    AddScore 30
    'alternate Special lights
    If specialL2.State Then
        specialL2.State = 0
        specialL1.State = 2
    End If
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
    PlaySoundAt "fx_slingshot", Remk
    PlaySound "sfx_Sling2"
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = 1
    ' add points
    AddScore 30
    'alternate Special lights
    If specialL1.State Then
        specialL2.State = 2
        specialL1.State = 0
    End If
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing2.Visible = 0:Remk.RotX = -10:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

'**************************
'   Gomas de 10 puntos
'**************************

Sub rlband006_Hit
    If Tilted Then Exit Sub
    AddScore 1
End Sub

Sub rlband005_Hit
    If Tilted Then Exit Sub
    AddScore 1
End Sub

Sub rlband004_Hit
    If Tilted Then Exit Sub
    AddScore 1
End Sub

Sub rlband003_Hit
    If Tilted Then Exit Sub
    AddScore 1
End Sub

Sub rlband002_Hit
    If Tilted Then Exit Sub
    AddScore 1
End Sub

'*********
' Bumpers
'*********

Sub Bumper1_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_Bumper", 105, DOFPulse, DOFContactors), bumper1
    ' añade algunos puntos
    'PlaySound "sfx_bumper", , , , 0.3
    If BumpL1.State Then
        Addscore 1000
    Else
        AddScore 100
    End If
End Sub

Sub Bumper2_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_Bumper", 107, DOFPulse, DOFContactors), bumper2
    ' añade algunos puntos
    'PlaySound "sfx_bumper", , , , 0.3
    If BumpL2.State Then
        Addscore 1000
    Else
        AddScore 100
    End If
End Sub

Sub Bumper3_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_Bumper", 107, DOFPulse, DOFContactors), bumper3
    ' añade algunos puntos
    'PlaySound "sfx_bumper", , , , 0.3
    If BumpL3.State Then
        Addscore 1000
    Else
        AddScore 100
    End If
End Sub

'******************************
'     Pasillos pit stop
'******************************

Sub sw12_Hit 'left pit stop
    PlaySoundAt "fx_sensor", sw12
    If Tilted Then Exit Sub
    If pitstopL.State Then
        PLaySound"sfx_TopLanes"
        AddScore 5000
        pitstopL.State = 0
        CheckPitStop
    Else
        Addscore 3000
    End If
End Sub

Sub sw52_Hit 'right pit stop
    PlaySoundAt "fx_sensor", sw52
    If Tilted Then Exit Sub
    If pitstopR.State Then
        PLaySound"sfx_TopLanes"
        AddScore 5000
        pitstopR.State = 0
        CheckPitStop
    Else
        Addscore 3000
    End If
End Sub

Sub CheckPitStop 'if both lights are off turn on collect bonus temporally
    If pitstopL.State + pitstopR.State = 0 Then
        If collectbL.State = 0 Then
            collectbL.State = 2
            PitStopTimer.Enabled = 1
        End If
    End If
End Sub

Sub PitStopTimer_Timer 'to stop the collect bonus light, 15 seconds
    PitStopTimer.Enabled = 0
    collectbL.State = 0
    pitstopL.State = 1
    pitstopR.State = 1
End Sub

' Outlanes

Sub sw22_Hit 'left outlane
    PlaySoundAt "fx_sensor", sw22
    If Tilted Then Exit Sub
    Addscore 10000
    If SpecialL1.State then
        AwardSpecial
    Else
        Playsound "sfx_Ball_Lost_In_Lane"
    End If
End Sub

Sub sw62_Hit 'right outlane
    PlaySoundAt "fx_sensor", sw62
    If Tilted Then Exit Sub
    Addscore 10000
    If SpecialL2.State then
        AwardSpecial
    Else
        Playsound "sfx_Ball_Lost_In_Lane"
    End If
End Sub

'******************************
'     Pasillos superiores
'******************************

Sub sw11_Hit
    PlaySoundAt "fx_sensor", sw11
    If Tilted Then Exit Sub
    PlaySound "sfx_TopLanes"
    If l123.State Then AwardExtraBall:l123.State = 0
    If x1l.State Then
        Addscore 5000
        FlashForMs x1l, 500, 50, 0
        BumpL1.State = 0:BumpL1a.State = 0
        CheckX
    Else
        Addscore 3000
    End If
End Sub

Sub sw21_Hit
    PlaySoundAt "fx_sensor", sw21
    If Tilted Then Exit Sub
    PlaySound "sfx_TopLanes"
    If l123.State Then AwardExtraBall:l123.State = 0
    If x2l.State Then
        Addscore 5000
        FlashForMs x2l, 500, 50, 0
        BumpL2.State = 0:BumpL2a.State = 0
        CheckX
    Else
        Addscore 3000
    End If
End Sub

Sub sw31_Hit
    PlaySoundAt "fx_sensor", sw31
    If Tilted Then Exit Sub
    PlaySound "sfx_TopLanes"
    If l123.State Then AwardExtraBall:l123.State = 0
    If x3l.State Then
        Addscore 5000
        FlashForMs x3l, 500, 50, 0
        BumpL3.State = 0:BumpL3a.State = 0
        CheckX
    Else
        Addscore 3000
    End If
End Sub

Sub CheckX
    If x1l.State + x2l.State + x3l.State = 0 Then
        AddBonusmultiplier 1
        vpmTimer.AddTimer 1500, "ResetTopLights '"
        If BonusMultiplier = 3 Then l123.State = 1:PlaySound "vo_Extra_Ball_Lit_V01"
    End If
End Sub

Sub ResetTopLights
    x1l.State = 1:BumpL1.State = 1:BumpL1a.State = 1
    x2l.State = 1:BumpL2.State = 1:BumpL2a.State = 1
    x3l.State = 1:BumpL3.State = 1:BumpL3a.State = 1
End Sub

' Bonus rollovers

Sub sw41_Hit
    PlaySoundAt "fx_sensor", sw41
    If Tilted Then Exit Sub
    AddScore 1000
    AddBonus 1
    AdvanceAYR
End Sub

Sub sw51_Hit
    PlaySoundAt "fx_sensor", sw51
    If Tilted Then Exit Sub
    AddScore 1000
    AddBonus 1
    AdvanceAYR
End Sub

'******************************
'       Drop Targets
'******************************

' left bank SENNA

Sub dt1_Hit
    PlaySoundAt "fx_droptarget", dt1
    If Tilted Then Exit Sub
    PlaySound "sfx_dropSENNA", , , , 0.3
    dt1l.State = 0
    Addscore 2000
    Addbonus 2
End Sub

Sub dt2_Hit
    PlaySoundAt "fx_droptarget", dt2
    If Tilted Then Exit Sub
    PlaySound "sfx_dropSENNA", , , , 0.3
    dt2l.State = 0
    Addscore 2000
    Addbonus 2
End Sub

Sub dt3_Hit
    PlaySoundAt "fx_droptarget", dt3
    If Tilted Then Exit Sub
    PlaySound "sfx_dropSENNA", , , , 0.3
    dt3l.State = 0
    Addscore 2000
    Addbonus 2
End Sub

Sub dt4_Hit
    PlaySoundAt "fx_droptarget", dt4
    If Tilted Then Exit Sub
    PlaySound "sfx_dropSENNA", , , , 0.3
    dt4l.State = 0
    Addscore 2000
    Addbonus 2
End Sub

Sub dt5_Hit
    PlaySoundAt "fx_droptarget", dt5
    If Tilted Then Exit Sub
    PlaySound "sfx_dropSENNA", , , , 0.3
    dt5l.State = 0
    Addscore 2000
    Addbonus 2
End Sub

Sub dt1_Dropped:CheckSenna:End Sub
Sub dt2_Dropped:CheckSenna:End Sub
Sub dt3_Dropped:CheckSenna:End Sub
Sub dt4_Dropped:CheckSenna:End Sub
Sub dt5_Dropped:CheckSenna:End Sub

Sub CheckSenna
    If dt1.IsDropped AND dt2.IsDropped AND dt3.IsDropped AND dt4.IsDropped AND dt5.IsDropped Then
        LightEffect 2
        AdvanceHole 'hole scores
        'make ready for next circuit
        If bCircuitReady(CurrentPlayer) = False then
            If PitStopTimer.Enabled = 1 Then  'stop the timer so it doesn't turn off the collect bonus light
                PitStopTimer.Enabled = 0
                pitstopL.State = 1
                pitstopR.State = 1
            End If
            bCircuitReady(CurrentPlayer) = True 'so the hole will score the circuit
            SennaDroppedCount(CurrentPlayer) = SennaDroppedCount(CurrentPlayer) + 1
            UpdateCircuit                       'blink the light
            collectbL.State = 2
        End If
        vpmTimer.AddTimer 1500, "ResetSenna '"
    End If
End Sub

Sub ResetSenna
    dt1.IsDropped = 0
    dt2.IsDropped = 0
    dt3.IsDropped = 0
    dt4.IsDropped = 0
    dt5.IsDropped = 0
    dt1l.State = 1
    dt2l.State = 1
    dt3l.State = 1
    dt4l.State = 1
    dt5l.State = 1
    PlaySoundAt "fx_resetdrop", dt3
End Sub

Sub UpdateCircuit                  'after completing senna targets, blinks the current circuit
    Select Case SennaDroppedCount(CurrentPlayer)
        Case 1:c1.TimerEnabled = 1 'blink desktop and db2s lights
        Case 2:c2.TimerEnabled = 1
        Case 3:c3.TimerEnabled = 1
        Case 4:c4.TimerEnabled = 1
        Case 5:c5.TimerEnabled = 1
        Case 6:c6.TimerEnabled = 1
        Case 7:c7.TimerEnabled = 1
        Case 8:c8.TimerEnabled = 1:PlaySound "vo_Special_Is_Lit_V01":PlaySong "mu_Special":specialL2.State = 2
    End Select
    If bCircuitReady(CurrentPlayer) Then collectbL.State = 2
End Sub

Sub WinCircuit 'after hitting hole
    bCircuitReady(CurrentPlayer) = False
    Select case SennaDroppedCount(CurrentPlayer)
        Case 1:c1.TimerEnabled = 0:PlaySound "vo_Circuit_Jacarepagua"
        Case 2:c2.TimerEnabled = 0:PlaySound "vo_Circuit_Detroit"
        Case 3:c3.TimerEnabled = 0:PlaySound "vo_Circuit_Monaco"
        Case 4:c4.TimerEnabled = 0:PlaySound "vo_Circuit_Estoril"
        Case 5:c5.TimerEnabled = 0:PlaySound "vo_Circuit_Interlagos"
        Case 6:c6.TimerEnabled = 0:PlaySound "vo_Circuit_Donington_Park"
        Case 7:c7.TimerEnabled = 0:PlaySound "vo_Circuit_Suzuka"
    End Select
    SennaWonCount(CurrentPlayer) = SennaDroppedCount(CurrentPlayer)
    UpdateWonCircuit
End Sub

Sub UpdateWonCircuit 'after changing PlayersPlayingGame
    c1.TimerEnabled = 0
    c2.TimerEnabled = 0
    c3.TimerEnabled = 0
    c4.TimerEnabled = 0
    c5.TimerEnabled = 0
    c6.TimerEnabled = 0
    c7.TimerEnabled = 0
    c8.TimerEnabled = 0
    Select case SennaWonCount(CurrentPlayer)
        Case 0:c1.State = 0:c2.State = 0:c3.State = 0:c4.State = 0:C5.State = 0:c6.State = 0:c7.State = 0:c8.State = 0
        Case 1:c1.State = 1:c2.State = 0:c3.State = 0:c4.State = 0:C5.State = 0:c6.State = 0:c7.State = 0:c8.State = 0
        Case 2:c1.State = 1:c2.State = 1:c3.State = 0:c4.State = 0:C5.State = 0:c6.State = 0:c7.State = 0:c8.State = 0
        Case 3:c1.State = 1:c2.State = 1:c3.State = 1:c4.State = 0:C5.State = 0:c6.State = 0:c7.State = 0:c8.State = 0
        Case 4:c1.State = 1:c2.State = 1:c3.State = 1:c4.State = 1:C5.State = 0:c6.State = 0:c7.State = 0:c8.State = 0
        Case 5:c1.State = 1:c2.State = 1:c3.State = 1:c4.State = 1:c5.State = 1:c6.State = 0:c7.State = 0:c8.State = 0
        Case 6:c1.State = 1:c2.State = 1:c3.State = 1:c4.State = 1:c5.State = 1:c6.State = 1:c7.State = 0:c8.State = 0
        Case 7:c1.State = 1:c2.State = 1:c3.State = 1:c4.State = 1:c5.State = 1:c6.State = 1:c7.State = 1:c8.State = 0
        Case 8:c1.State = 1:c2.State = 1:c3.State = 1:c4.State = 1:c5.State = 1:c6.State = 1:c7.State = 1:c8.State = 1
    End Select
    If B2SOn Then
        Select case SennaWonCount(CurrentPlayer)
            Case 0:Controller.B2SSetData 71, 0:Controller.B2SSetData 72, 0:Controller.B2SSetData 73, 0:Controller.B2SSetData 74, 0:Controller.B2SSetData 75, 0:Controller.B2SSetData 76, 0:Controller.B2SSetData 77, 0:Controller.B2SSetData 78, 0
            Case 1:Controller.B2SSetData 71, 1:Controller.B2SSetData 72, 0:Controller.B2SSetData 73, 0:Controller.B2SSetData 74, 0:Controller.B2SSetData 75, 0:Controller.B2SSetData 76, 0:Controller.B2SSetData 77, 0:Controller.B2SSetData 78, 0
            Case 2:Controller.B2SSetData 71, 1:Controller.B2SSetData 72, 1:Controller.B2SSetData 73, 0:Controller.B2SSetData 74, 0:Controller.B2SSetData 75, 0:Controller.B2SSetData 76, 0:Controller.B2SSetData 77, 0:Controller.B2SSetData 78, 0
            Case 3:Controller.B2SSetData 71, 1:Controller.B2SSetData 72, 1:Controller.B2SSetData 73, 1:Controller.B2SSetData 74, 0:Controller.B2SSetData 75, 0:Controller.B2SSetData 76, 0:Controller.B2SSetData 77, 0:Controller.B2SSetData 78, 0
            Case 4:Controller.B2SSetData 71, 1:Controller.B2SSetData 72, 1:Controller.B2SSetData 73, 1:Controller.B2SSetData 74, 1:Controller.B2SSetData 75, 0:Controller.B2SSetData 76, 0:Controller.B2SSetData 77, 0:Controller.B2SSetData 78, 0
            Case 5:Controller.B2SSetData 71, 1:Controller.B2SSetData 72, 1:Controller.B2SSetData 73, 1:Controller.B2SSetData 74, 1:Controller.B2SSetData 75, 1:Controller.B2SSetData 76, 0:Controller.B2SSetData 77, 0:Controller.B2SSetData 78, 0
            Case 6:Controller.B2SSetData 71, 1:Controller.B2SSetData 72, 1:Controller.B2SSetData 73, 1:Controller.B2SSetData 74, 1:Controller.B2SSetData 75, 1:Controller.B2SSetData 76, 1:Controller.B2SSetData 77, 0:Controller.B2SSetData 78, 0
            Case 7:Controller.B2SSetData 71, 1:Controller.B2SSetData 72, 1:Controller.B2SSetData 73, 1:Controller.B2SSetData 74, 1:Controller.B2SSetData 75, 1:Controller.B2SSetData 76, 1:Controller.B2SSetData 77, 1:Controller.B2SSetData 78, 0
            Case 8:Controller.B2SSetData 71, 1:Controller.B2SSetData 72, 1:Controller.B2SSetData 73, 1:Controller.B2SSetData 74, 1:Controller.B2SSetData 75, 1:Controller.B2SSetData 76, 1:Controller.B2SSetData 77, 1:Controller.B2SSetData 78, 1
        End Select
    End If
End Sub

Sub C1_Timer
    Dim tmp
    tmp = ABS(C1.State -1)
    c1.State = tmp
    If B2SOn Then
        Controller.B2SSetData 71, tmp
    End If
End Sub

Sub C2_Timer
    Dim tmp
    tmp = ABS(C2.State -1)
    c2.State = tmp
    If B2SOn Then
        Controller.B2SSetData 72, tmp
    End If
End Sub

Sub C3_Timer
    Dim tmp
    tmp = ABS(C3.State -1)
    c3.State = tmp
    If B2SOn Then
        Controller.B2SSetData 73, tmp
    End If
End Sub

Sub C4_Timer
    Dim tmp
    tmp = ABS(C4.State -1)
    c4.State = tmp
    If B2SOn Then
        Controller.B2SSetData 74, tmp
    End If
End Sub

Sub C5_Timer
    Dim tmp
    tmp = ABS(C5.State -1)
    c5.State = tmp
    If B2SOn Then
        Controller.B2SSetData 75, tmp
    End If
End Sub

Sub C6_Timer
    Dim tmp
    tmp = ABS(C6.State -1)
    c6.State = tmp
    If B2SOn Then
        Controller.B2SSetData 76, tmp
    End If
End Sub

Sub C7_Timer
    Dim tmp
    tmp = ABS(C7.State -1)
    c7.State = tmp
    If B2SOn Then
        Controller.B2SSetData 77, tmp
    End If
End Sub

Sub C8_Timer
    Dim tmp
    tmp = ABS(C8.State -1)
    c8.State = tmp
    If B2SOn Then
        Controller.B2SSetData 78, tmp
    End If
End Sub

' top bank AYR

Sub dt6_Hit
    PlaySoundAt "fx_droptarget", dt6
    If Tilted Then Exit Sub
    PlaySound "sfx_dropAYR", , , , 0.3
    Addscore 1000
    Addbonus 1
    If l1.State then Addscore 2000:Addbonus 2
    If l4.State then AddBonusMultiplier 1
End Sub

Sub dt7_Hit
    PlaySoundAt "fx_droptarget", dt7
    If Tilted Then Exit Sub
    PlaySound "sfx_dropAYR", , , , 0.3
    Addscore 1000
    Addbonus 1
    If l2.State then Addscore 2000:Addbonus 2
    If l4.State then AddBonusMultiplier 1
End Sub

Sub dt8_Hit
    PlaySoundAt "fx_droptarget", dt8
    If Tilted Then Exit Sub
    PlaySound "sfx_dropAYR", , , , 0.3
    Addscore 1000
    Addbonus 1
    If l3.State then Addscore 2000:Addbonus 2
    If l4.State then AddBonusMultiplier 1
End Sub

Sub dt6_Dropped:CheckAYR:End Sub
Sub dt7_Dropped:CheckAYR:End Sub
Sub dt8_Dropped:CheckAYR:End Sub

Sub CheckAYR
    If dt6.IsDropped AND dt7.IsDropped AND dt8.IsDropped Then
        LightEffect 1
        Addscore 2000
        AddBonus 2
        AYRDroppedCount(CurrentPlayer) = AYRDroppedCount(CurrentPlayer) + 1
        vpmTimer.AddTimer 1500, "ResetAYR '"
        If AYRDroppedCount(CurrentPlayer) = 3 Then xballlislit.State = 1:PlaySound "vo_Extra_Ball_Lit_V01"
        CheckAYRTON
    End If
End Sub

Sub ResetAYR
    dt6.IsDropped = 0
    dt7.IsDropped = 0
    dt8.IsDropped = 0
    PlaySoundAt "fx_resetdrop", dt7
End Sub

' right bank TON

Sub dt9_Hit
    PlaySoundAt "fx_droptarget", dt9
    If Tilted Then Exit Sub
    PlaySound "sfx_dropTON", , , , 0.3
    Addscore 1000
    Addbonus 1
End Sub

Sub dt10_Hit
    PlaySoundAt "fx_droptarget", dt10
    If Tilted Then Exit Sub
    PlaySound "sfx_dropTON", , , , 0.3
    Addscore 1000
    Addbonus 1
End Sub

Sub dt11_Hit
    PlaySoundAt "fx_droptarget", dt11
    If Tilted Then Exit Sub
    PlaySound "sfx_dropTON", , , , 0.3
    Addscore 1000
    Addbonus 1
End Sub

Sub dt9_Dropped:CheckTON:End Sub
Sub dt10_Dropped:CheckTON:End Sub
Sub dt11_Dropped:CheckTON:End Sub

Sub CheckTON
    If dt9.IsDropped AND dt10.IsDropped AND dt11.IsDropped Then
        LightEffect 1
        'Add extra scores
        Select Case TONDroppedCount(CurrentPlayer)
            Case 1:Addscore 3000
            Case 2:Addscore 6000
            Case 3:Addscore 9000
            Case 4:Addscore 14000
            Case 5:Addscore 16000
            Case 6:Addscore 20000
        End Select
        AdvanceTON
        'increase the count how many times has been dropped
        TONDroppedCount(CurrentPlayer) = TONDroppedCount(CurrentPlayer) + 1
        vpmTimer.AddTimer 1500, "ResetTON '"
        CheckAYRTON
    End If
End Sub

Sub ResetTON
    dt9.IsDropped = 0
    dt10.IsDropped = 0
    dt11.IsDropped = 0
    PlaySoundAt "fx_resetdrop", dt10
    CheckAYRTON
End Sub

Sub AdvanceTON
    TONPos(CurrentPlayer) = (TONPos(CurrentPlayer) + 1) MOD 7
    UpdateTON
    'multiball
    If TONPos(CurrentPlayer) = 6 Then
        bAutoPlunger = True
        EnableBallSaver BallSaverTime
        AddMultiball 2
        bMultiBallStarted(CurrentPlayer) = True
        PlaySong "mu_Multiball"
    End If
End Sub

Sub UpdateTON
    Select Case TONPos(CurrentPlayer)
        Case 0:tl1.State = 0:tl2.State = 0:tl3.State = 0:tl4.State = 0:tl5.State = 0:tl6.State = 0
        Case 1:tl1.State = 1:tl2.State = 0:tl3.State = 0:tl4.State = 0:tl5.State = 0:tl6.State = 0
        Case 2:tl1.State = 1:tl2.State = 1:tl3.State = 0:tl4.State = 0:tl5.State = 0:tl6.State = 0
        Case 3:tl1.State = 1:tl2.State = 1:tl3.State = 1:tl4.State = 0:tl5.State = 0:tl6.State = 0
        Case 4:tl1.State = 1:tl2.State = 1:tl3.State = 1:tl4.State = 1:tl5.State = 0:tl6.State = 0
        Case 5:tl1.State = 1:tl2.State = 1:tl3.State = 1:tl4.State = 1:tl5.State = 1:tl6.State = 0
        Case 6:tl1.State = 1:tl2.State = 1:tl3.State = 1:tl4.State = 1:tl5.State = 1:tl6.State = 1
    End Select
End SUb

Sub CheckAYRTON
    If AYRDroppedCount(CurrentPlayer) >= 3 AND TONDroppedCount(CurrentPlayer) >= 3 Then
        xballlislit.State = 1
        AYRDroppedCount(CurrentPlayer) = 0
        TONDroppedCount(CurrentPlayer) = 0
    End If
End Sub

'  Hole

Sub sw2_Hit
    Dim tmp, pause
    PlaySoundAt "fx_kicker_enter", sw2
    If Tilted Then
        vpmtimer.addtimer 500, "PlaySoundAt ""fx_kicker"", sw2: sw2.kick 198,9 '"
        Exit Sub
    End IF
    GiEffect 1
    Pause = 1500
    ' hole scoring
    Select Case HolePos(CurrentPlayer)
        Case 1:Addscore 5000:FlashForMs hl1, 500, 50, 1
            PlaySound "sfx_F1_Radio_Notification"
        Case 2:Addscore 5000:FlashForMs hl1, 500, 50, 1
            PlaySound "sfx_F1_Radio_Notification"
            vpmTimer.AddTimer 750, "Addscore 10000: FlashForMs hl2, 500, 50, 1 '"
            pause = pause + 750
        Case 3:Addscore 5000:FlashForMs hl1, 500, 50, 1
            PlaySound "sfx_F1_Radio_Notification"
            vpmTimer.AddTimer 750, "Addscore 10000: FlashForMs hl2, 500, 50, 1 '"
            vpmTimer.AddTimer 1500, "Addscore 20000: FlashForMs hl3, 500, 50, 1 '"
            vpmtimer.AddTimer 2000, "ResetHole '"
            pause = pause + 2500
    End Select

    If xballlislit.State Then
        AwardExtraBall
        FlashForMs xballlislit, 500, 50, 0
        pause = pause + 1000
    End If

    If collectbL.State Then
        collectbL.State = 0
        tmp = Bonus \ 10 * 10 'only the tens
        AddScore 1000 * tmp * BonusMultiplier
        pause = pause + 1000
        If bCircuitReady(CurrentPlayer) Then
            If SennaDroppedCount(CurrentPlayer) < 8 Then
                vpmTimer.AddTimer 1000, "PlaySound""vo_Advance_Circuit"" '"
                pause = pause + 1500
            End If
            vpmTimer.AddTimer 2000, "WinCircuit '"
            pause = pause + 2000
        End If
    End If

    'kick the ball out
    vpmtimer.AddTimer pause, "PlaySound""sfx_PitStop"" '"
    pause = pause + 3000
    vpmtimer.addtimer pause, "PlaySoundAt ""fx_kicker"", sw2: sw2.kick 198,9 '"
End Sub

Sub AdvanceHole
    If HolePos(CurrentPlayer) < 3 Then
        HolePos(CurrentPlayer) = HolePos(CurrentPlayer) + 1
        UpdateHole
    End If
End Sub

Sub UpdateHole
    Select Case HolePos(CurrentPlayer)
        Case 0:hl1.State = 0:hl2.State = 0:hl3.State = 0
        Case 1:hl1.State = 1:hl2.State = 0:hl3.State = 0
        Case 2:hl1.State = 1:hl2.State = 1:hl3.State = 0
        Case 3:hl1.State = 1:hl2.State = 1:hl3.State = 1
    End Select
End Sub

Sub ResetHole
    HolePos(CurrentPlayer) = 0
    UpdateHole
End Sub

' Spinner

Sub spinner1_Spin
    PlaySoundAt "fx_spinner", spinner1
    If Tilted Then Exit Sub
    PlaySound "sfx_spin"
    Addscore 1000
    AdvanceAYR
End Sub

Sub AdvanceAYR
    AYRPos(CurrentPlayer) = (AYRPos(CurrentPlayer) + 1) MOD 10
    UpdateAYR
End Sub

Sub UpdateAYR 'AYR lights
    Select Case AYRPos(CurrentPlayer)
        Case 0:l1.State = 0:l2.State = 0:l3.State = 0:l4.State = 0
        Case 1:l1.State = 1:l2.State = 0:l3.State = 0:l4.State = 0
        Case 2:l1.State = 0:l2.State = 1:l3.State = 0:l4.State = 0
        Case 3:l1.State = 0:l2.State = 0:l3.State = 1:l4.State = 0
        Case 4:l1.State = 1:l2.State = 1:l3.State = 0:l4.State = 0
        Case 5:l1.State = 1:l2.State = 0:l3.State = 1:l4.State = 1
        Case 6:l1.State = 0:l2.State = 1:l3.State = 1:l4.State = 0
        Case 7:l1.State = 1:l2.State = 0:l3.State = 1:l4.State = 0
        Case 8:l1.State = 0:l2.State = 1:l3.State = 0:l4.State = 0
        Case 9:l1.State = 1:l2.State = 1:l3.State = 1:l4.State = 1
    End Select
End Sub

' Rotate flipper Lights
Sub RotateTopLightsLeft
    Dim tmp
    tmp = x1l.State
    x1l.State = x2l.state
    BumpL1.State = x2l.state:BumpL1a.State = x2l.state
    x2l.State = x3l.State
    BumpL2.State = x3l.state:BumpL2a.State = x3l.state
    x3l.State = tmp
    BumpL3.State = tmp:BumpL3a.State = tmp
End Sub

Sub RotateTopLightsRight
    Dim tmp
    tmp = x3l.State
    x3l.State = x2l.state
    BumpL3.State = x2l.state:BumpL3a.State = x2l.state
    x2l.State = x1l.State
    BumpL2.State = x1l.state:BumpL2a.State = x1l.state
    x1l.State = tmp
    BumpL1.State = tmp:BumpL1a.State = tmp
End Sub

Sub Kicker001_Hit
    Kicker001.DestroyBall
End Sub