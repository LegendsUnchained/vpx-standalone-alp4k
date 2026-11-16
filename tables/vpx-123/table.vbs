' 1·2·3... / IPD No. 5247 / 1 Player
' Talleres del Llobregat S.A. [Automaticos], of Barcelona,
' VPX8 table, jpsalas 2026, v7.0.0


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
' Bumpers  - 105/106/107/108/109
' Targets - 110/111/112/113/114
'
' LED backboard
' Flasher Outside Left - 203/209/214/218/225/227/240/250
' Flasher left - 207/213/215/218/222/229/240/250
' Flasher center - 211/212/215/217/218/221/223/232/240/250
' Flasher right - 208/213/217/221/224/231/240/250
' Flasher Outside - 205/210/216/220/226/228/240/250
' PF Flashers L/R - 204/206
' Start Button - 200
' Undercab - 201/202
' Strobe - 230
' Knocker - 300
' Chimes - 301/302/303

' Valores Constantes de las físicas de los flippers - se usan en la creación de las bolas, tienen que cargarse antes del core.vbs
Const BallSize = 25 ' el radio de la bola. el tamaño normal es 50 unidades de VP
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

' Valores Constants
Const TableName = "TlD_123" ' se usa para cargar y grabar los highscore y creditos
Const cGameName = "TlD_123" ' para el B2S
Const MaxPlayers = 1        ' de 1 a 4
Const MaxMultiplier = 5     ' limita el bonus multiplicador a 5
Const Special1 = 80000      ' puntuación a obtener para partida extra
Const Special2 = 90000      ' puntuación a obtener para partida extra
Const Special3 = 100000     ' puntuación a obtener para partida extra

' Variables Globales
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim Bonus
Dim DoubleBonus
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
Dim x

' Variables de control
Dim LastSwitchHit
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

' core.vbs variables, como imanes, impulse plunger

' *********************************************************************
'                Rutinas comunes para todas las mesas
' *********************************************************************

Sub Table1_Init()
    Dim x

    ' Inicializar diversos objetos de la mesa, como droptargets, animations...
    VPObjects_Init
    LoadEM

    ' Carga los valores grabados highscore y créditos
    Loadhs

    UpdateCredits

    ' Juego libre o con monedas: si es True entonces no se usarán monedas
    bFreePlay = True 'queremos monedas

    ' Inicialiar las variables globales de la mesa
    bAttractMode = False
    bOnTheFirstBall = False
    bGameInPlay = False
    bBallInPlungerLane = False
    LastSwitchHit = ""
    BallsOnPlayfield = 0
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    Match = 0
    bJustStarted = True
    Add10 = 0
    Add100 = 0
    Add1000 = 0

    ' pone la mesa en modo de espera
    EndOfGame

    'Enciende las luces GI despues de un segundo
    vpmtimer.addtimer 1000, "GiOn '"

    ' Quita los laterales y las puntuaciones cuando la mesa se juega en modo FS
    If Table1.ShowDT then
        lrail.Visible = True
        rrail.Visible = True
        For each x in aReels
            x.Visible = 1
        Next
    Else
        lrail.Visible = False
        rrail.Visible = False
        For each x in aReels
            x.Visible = 0
        Next
    End If
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)

    ' Reset High Score using the [7] key
    if keycode = "8" then 
        ResetHS
        ResHS.Visible = 1
        vpmTimer.AddTimer 2000, "Reshs.visible = 0 '"
    End If

    If EnteringInitials then
        CollectInitials(keycode)
        Exit Sub
    End If

    ' añade monedas
    If Keycode = AddCreditKey OR Keycode = AddCreditKey2 Then
        If(Tilted = False)Then
            AddCredits 1
            PlaySound "fx_coin"
            DOF 200, DOFOn
        End If
    End If

    ' el plunger
    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
    End If

    ' Funcionamiento normal de los flipers y otras teclas durante el juego

    If bGameInPlay AND NOT Tilted Then
        If Credits = 0 Then DOF 200, DOFOff
        ' teclas de la falta
    If keycode = LeftTiltKey Then Nudge 90, 2:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25:CheckTilt
    If keycode = RightTiltKey Then Nudge 270, 2:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25:CheckTilt
    If keycode = CenterTiltKey Then Nudge 0, 2:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25:CheckTilt
        If keycode = MechanicalTilt Then Tilt = 16:CheckTilt

        ' teclas de los flipers
        If keycode = LeftFlipperKey Then SolLFlipper 1
        If keycode = RightFlipperKey Then SolRFlipper 1

        ' tecla de empezar el juego
        If keycode = StartGameKey Then
            If((PlayersPlayingGame < MaxPlayers)AND(bOnTheFirstBall = True))Then

                If(bFreePlay = True)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    PlayersReel.SetValue, PlayersPlayingGame
                'PlaySound "so_fanfare1"
                Else
                    If(Credits > 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        Credits = Credits - 1
                        UpdateCredits
                        UpdateBallInPlay
                    Else
                    DOF 200, DOFOff
                    ' no hay suficientes créditos para empezar el juego.
                    'PlaySound "so_nocredits"
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
                    End If
                Else
                    If(Credits > 0)Then
                        If(BallsOnPlayfield = 0)Then
                            Credits = Credits - 1
                            UpdateCredits
                            ResetScores
                            ResetForNewGame()
                        End If
                    Else
                    ' Not Enough Credits to start a game.
                    'PlaySound "so_nocredits"
                    End If
                End If
            End If
    End If ' If (GameInPlay)
End Sub

Sub Table1_KeyUp(ByVal keycode)

    If EnteringInitials then
        Exit Sub
    End If

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

'*************
' Para la mesa
'*************

Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub table1_Exit
    Savehs
'Controller.Stop
End Sub

'********************
'     Flippers
'********************

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF ("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToEnd
        LeftFlipperOn = 1
    Else
        PlaySoundAt SoundFXDOF ("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
        LeftFlipperOn = 0
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF ("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
        RightFlipperOn = 1
    Else
        PlaySoundAt SoundFXDOF ("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
        RightFlipperOn = 0
    End If
End Sub

'Sub LeftFlipper_Animate:LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle: End Sub
'Sub RightFlipper_Animate: RightFlipperTop.RotZ = RightFlipper.CurrentAngle: End Sub

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

'**********************************
' Real Time Flipper EOS adjustment
' based on JLouLouLou flipper script
'     JP's physics Version 5.0
'**********************************

Dim LeftFlipperOn
Dim RightFlipperOn

' EOS Torque when flipper hold up ( EOS Coil is fully charged. 
' Ampere increase due to flipper can't move or when it pushed back when "On". EOS Coil have more power )
Const FullStrokeEOS_Torque = 0.9 

' EOS Torque when flipper rotate to end ( When flipper move, EOS coil have 
' less Ampere due to flipper can freely move. EOS Coil have less power )
Const LiveStrokeEOS_Torque = 0.2 

LeftFlipper.TimerInterval = 1
LeftFlipper.TimerEnabled = 1

Sub LeftFlipper_Timer 'flipper's EOS changer
    'Left End Of Stroke Routine
    If LeftFlipperOn = 1 Then
        If LeftFlipper.CurrentAngle = LeftFlipper.EndAngle then
            LeftFlipper.EOSTorque = FullStrokeEOS_Torque
        End If
    Else
        LeftFlipper.EOSTorque = LiveStrokeEOS_Torque
    End If
    'Right End Of Stroke Routine
    If RightFlipperOn = 1 Then
        If RightFlipper.CurrentAngle = RightFlipper.EndAngle Then
            RightFlipper.EOSTorque = FullStrokeEOS_Torque
        End If
    Else
        RightFlipper.EOSTorque = LiveStrokeEOS_Torque
    End If
End Sub

'*******************
' Luces GI
'*******************

Sub GiOn 'enciende las luces GI
    DOF 201, DOFOn
    Dim bulb
    PlaySound"fx_gion"
    For each bulb in aGiLights
        bulb.State = 1
    Next
End Sub

Sub GiOff 'apaga las luces GI
    DOF 201, DOFOff
    Dim bulb
    PlaySound"fx_gioff"
    For each bulb in aGiLights
        bulb.State = 0
    Next
End Sub

'**************
' TILT - Falta
'**************

'el "timer" TiltDecreaseTimer resta .01 de la variable "Tilt" cada ronda

Sub CheckTilt                     'esta rutina se llama cada vez que das un golpe a la mesa
    Tilt = Tilt + TiltSensitivity 'añade un valor al contador "Tilt"
    TiltDecreaseTimer.Enabled = True
    If Tilt > 15 Then             'Si la variable "Tilt" es más de 15 entonces haz falta
        Tilted = True
        TiltReel.SetValue 1       'muestra Tilt en la pantalla
        If B2SOn then
            Controller.B2SSetTilt 1
        end if
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'empieza una pausa a fin de que todas las bolas se cuelen
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
        'Apaga todas las luces Gi de la mesa
        GiOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        Bumper1.Threshold = 100
        Bumper2.Threshold = 100
        Bumper3.Threshold = 100
        Bumper4.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
        DOF 101, DOFOff
        DOF 102, DOFOff
    Else
        'enciende de nuevo todas las luces GI, bumpers y slingshots
        GiOn
        Bumper1.Threshold = 1
        Bumper2.Threshold = 1
        Bumper3.Threshold = 1
        Bumper4.Threshold = 1
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' si todas las bolas se han colado, entonces ..
    If(BallsOnPlayfield = 0)Then
        '... haz el fin de bola normal
        EndOfBall()
        TiltRecoveryTimer.Enabled = False
    End If
' de lo contrario esta rutina continúa hasta que todas las bolas se han colado
End Sub

'***************************************************************
'             Supporting Ball & Sound Functions v5.0
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

Sub PlaySoundAtBall(soundname) ' play a collision sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall) * 10, 0, 0, AudioFade(ActiveBall)
End Sub

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function

Dim MyPi: MyPi = Round(4 * Atn(1), 6) / 90

'***********************************************
'      JP's VPX8 Ball Rolling Sounds 2026
'***********************************************

' physics 2026

Dim maxvel
maxvel = 100 'initial value

Sub Maxvelswitch_Hit
    maxvel = 30 'max ball velocity during game play. EM: 20-35  SS: 35-50
End Sub

Sub maxvelswitch2_Hit
    maxvel = 100 'new ball
End Sub

' rolling routines

Const tnob = 19   'total number of balls, 20 balls, from 0 to 19
Const lob = 0    'number of locked balls
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
    Dim BOT, b, ballpitch, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball
    For b = lob to UBound(BOT)
        If BallVel(BOT(b) )> 1 Then
            If BOT(b).z <0 Then
                ballpitch = Pitch(BOT(b) ) - 5000 'decrease the pitch under the playfield
            ElseIf BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b) )
            Else
                ballpitch = Pitch(BOT(b) ) + 30000 'increase the pitch on a ramp or platform
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, ballpitch, 1, 0, AudioFade(BOT(b) )
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' dropping sounds
        If BOT(b).VelZ <-1 Then
            'from ramp
            If BOT(b).z <55 and BOT(b).z> 27 Then PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
            'down a hole
            If BOT(b).z <10 and BOT(b).z> -10 Then PlaySound "fx_hole_enter", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        End If

        ' jps ball spin control
        BOT(b).AngMomZ = BOT(b).AngMomZ * 0.95

        ' jps ball speed control 2
        If ABS(BallVel(BOT(b) ) )> maxvel Then
            If BOT(b).VelX <> 0 Then
                speedfactorx = ABS(maxvel / BOT(b).VelX)
                If speedfactorx <1 Then
                    BOT(b).VelX = BOT(b).VelX * speedfactorx
                    BOT(b).VelY = BOT(b).VelY * speedfactorx
                End If
            End If
            If BOT(b).VelY <> 0 Then
                speedfactory = ABS(maxvel / BOT(b).VelY)
                If speedfactory <1 Then
                    BOT(b).VelX = BOT(b).VelX * speedfactory
                    BOT(b).VelY = BOT(b).VelY * speedfactory
                End If
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

'***************************************
' Sonidos de las colecciones de objetos
' como metales, gomas, plásticos, etc
'***************************************

Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
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
    DoubleBonus = 1
    Bonus = 0
    UpdateBallInPlay

    Clear_Match

    ' inicializa otras variables
    Tilt = 0

    ' inicializa las variables del juego
    Game_Init()

    ' ahora puedes empezar una música si quieres
    ' empieza la rutina "Firstball" despues de una pequeña pausa
    vpmtimer.addtimer 2000, "FirstBall '"
    If Credits = 0 Then DOF 200, DOFOff
End Sub

' esta pausa es para que la mesa tenga tiempo de poner los marcadores a cero y actualizar las luces

Sub FirstBall
    'debug.print "FirstBall"
    ' ajusta la mesa para una bola nueva, sube las dianas abatibles, etc
    ResetForNewPlayerBall()
    ' crea una bola nueva en la zona del plunger
    CreateNewBall()
    DOF 250, DOFPulse
End Sub

' (Re-)inicializa la mesa para una bola nueva, tanto si has perdido la bola, oe le toca el turno al otro jugador

Sub ResetForNewPlayerBall()
    'debug.print "ResetForNewPlayerBall"
    ' Se asegura que los marcadores están activados para el jugador de turno
    AddScore 0

    ' ajusta el multiplicador del bonus multiplier a 1X (si hubiese multiplicador en la mesa)

    ' enciende las luces, reinicializa las variables del juego, etc
    bExtraBallWonThisBall = False
    ResetNewBallLights
    ResetNewBallVariables
End Sub

' Crea una bola nueva en la mesa

Sub CreateNewBall()
    ' crea una bola nueva basada en el tamaño y la masa de la bola especificados al principio del script
    BallRelease.CreateSizedBallWithMass BallSize, BallMass

    ' incrementa el número de bolas en el tablero, ya que hay que contarlas
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' actualiza las luces del backdrop
    UpdateBallInPlay

    ' y expulsa la bola
    PlaySoundAt SoundFXDOF ("fx_Ballrel", 104, DOFPulse, DOFContactors), BallRelease
    BallRelease.Kick 90, 4
End Sub

' El jugador ha perdido su bola, y ya no hay más bolas en juego
' Empieza a contar los bonos

Sub EndOfBall()
    'debug.print "EndOfBall"
    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 0
    ' La primera se ha perdido. Desde aquí ya no se puede aceptar más jugadores
    bOnTheFirstBall = False

    ' solo recoge los bonos si no hay falta
    ' el sistema del la falta se encargará de nuevas bolas o del fin de la partida

    If NOT Tilted Then
        If DoubleBonus = 2 Then
            BonusCountTimer.Interval = 400
        Else
            BonusCountTimer.Interval = 250
        End If
        BonusCountTimer.Enabled = 1
    Else 'Si hay falta simplemente espera un momento y va directo a la segunta parte después de perder la bola
        vpmtimer.addtimer 400, "EndOfBall2 '"
    End If
End Sub

Sub BonusCountTimer_Timer 'añade los bonos y actualiza las luces
    'debug.print "BonusCount_Timer"
    If Bonus > 0 Then
        Bonus = Bonus -1
        AddScore 1000 * DoubleBonus
        UpdateBonusLights
    Else
        ' termina la cuenta de los bonos y continúa con el fin de bola
        BonusCountTimer.Enabled = 0
        vpmtimer.addtimer 1000, "EndOfBall2 '"
    End If
End Sub

Sub UpdateBonusLights 'enciende o apaga las luces de los bonos según la variable "Bonus"
    Select Case Bonus
        Case 0:li2.State = 0:li3.State = 0:li4.State = 0:li5.State = 0:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 0
        Case 1:li2.State = 1:li3.State = 0:li4.State = 0:li5.State = 0:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 0
        Case 2:li2.State = 0:li3.State = 1:li4.State = 0:li5.State = 0:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 0
        Case 3:li2.State = 0:li3.State = 0:li4.State = 1:li5.State = 0:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 0
        Case 4:li2.State = 0:li3.State = 0:li4.State = 0:li5.State = 1:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 0
        Case 5:li2.State = 0:li3.State = 0:li4.State = 0:li5.State = 0:li6.State = 1:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 0
        Case 6:li2.State = 0:li3.State = 0:li4.State = 0:li5.State = 0:li6.State = 0:li7.State = 1:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 0
        Case 7:li2.State = 0:li3.State = 0:li4.State = 0:li5.State = 0:li6.State = 0:li7.State = 0:li8.State = 1:li9.State = 0:li10.State = 0:li11.State = 0
        Case 8:li2.State = 0:li3.State = 0:li4.State = 0:li5.State = 0:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 1:li10.State = 0:li11.State = 0
        Case 9:li2.State = 0:li3.State = 0:li4.State = 0:li5.State = 0:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 1:li11.State = 0
        Case 10:li2.State = 0:li3.State = 0:li4.State = 0:li5.State = 0:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 1
        Case 11:li2.State = 1:li3.State = 0:li4.State = 0:li5.State = 0:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 1
        Case 12:li2.State = 0:li3.State = 1:li4.State = 0:li5.State = 0:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 1
        Case 13:li2.State = 0:li3.State = 0:li4.State = 1:li5.State = 0:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 1
        Case 14:li2.State = 0:li3.State = 0:li4.State = 0:li5.State = 1:li6.State = 0:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 1
        Case 15:li2.State = 0:li3.State = 0:li4.State = 0:li5.State = 0:li6.State = 1:li7.State = 0:li8.State = 0:li9.State = 0:li10.State = 0:li11.State = 1
    End Select
End Sub

' La cuenta de los bonos ha terminado. Mira si el jugador ha ganado bolas extras
' y si no mira si es el último jugador o la última bola
'
Sub EndOfBall2()
    'debug.print "EndOfBall2"
    ' si hubiese falta, quítala, y pon la cuenta a cero de la falta para el próximo jugador, ó bola

    Tilted = False
    Tilt = 0
    TiltReel.SetValue 0
    If B2SOn then
        Controller.B2SSetTilt 0
    end if
    DisableTable False 'activa de nuevo los bumpers y los slingshots

    ' ¿ha ganado el jugador una bola extra?
    If(ExtraBallsAwards(CurrentPlayer) > 0)Then
        'debug.print "Extra Ball"

        ' sí? entonces se la das al jugador
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer)- 1

        ' si no hay más bolas apaga la luz de jugar de nuevo
        If(ExtraBallsAwards(CurrentPlayer) = 0)Then
        'LightShootAgain.State = 0
        'If B2SOn then
        '    Controller.B2SSetShootAgain 0
        'end if
        End If

' aquí se podría poner algún sonido de bola extra o alguna luz que parpadee

' En esta mesa hacemos la bola extra igual como si fuese la siguente bola, haciendo un reset de las variables y dianas
        ResetForNewPlayerBall()

        ' creamos una bola nueva en el pasillo de disparo
        CreateNewBall()
    Else ' no hay bolas extras

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer)- 1

        ' ¿Es ésta la última bola?
        If(BallsRemaining(CurrentPlayer) <= 0)Then

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
    If(PlayersPlayingGame > 1)Then
        ' entonces pasa al siguente jugador
        NextPlayer = CurrentPlayer + 1
        ' ¿vamos a pasar del último jugador al primero?
        ' (por ejemplo del jugador 4 al no. 1)
        If(NextPlayer > PlayersPlayingGame)Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'debug.print "Next Player = " & NextPlayer

    ' ¿Hemos llegado al final del juego? (todas las bolas se han jugado de todos los jugadores)
    If((BallsRemaining(CurrentPlayer) <= 0)AND(BallsRemaining(NextPlayer) <= 0))Then

        ' aquí se empieza la lotería, normalmente cuando se juega con monedas
        If bFreePlay = False Then
            Verification_Match
        End If

        ' ahora se pone la mesa en el modo de final de juego
        EndOfGame()
    Else
        ' pasamos al siguente jugador
        CurrentPlayer = NextPlayer

        ' nos aseguramos de que el backdrop muestra el jugador actual
        AddScore 0

        ' hacemos un reset del la mesa para el siguente jugador (ó bola)
        ResetForNewPlayerBall()

        ' y sacamos una bola
        CreateNewBall()
    End If
End Sub

' Esta función se llama al final del juego

Sub EndOfGame()
    'debug.print "EndOfGame"
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
    DOF 250, DOFPulse

    ' pon las luces en el modo de fin de juego
    StartAttractMode
End Sub

' Esta función calcula el no de bolas que quedan
Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp > BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
    End If
End Function

'******************
'  Match - Loteria
'******************

Sub IncreaseMatch
    Match = (Match + 10)MOD 100
End Sub

Sub Verification_Match()
    PlaySound "fx_match"
    Display_Match
    If(Score(CurrentPlayer)MOD 100) = Match Then
        PlaySound SoundFXDOF ("fx_knocker", 300, DOFPulse, DOFKnocker)
        DOF 230, DOFPulse
        AddCredits 1
        DOF 200, DOFOn
    End If
End Sub

Sub Clear_Match()
    MatchReel.SetValue 0
    If B2SOn then
        Controller.B2SSetMatch 0
    end if
End Sub

Sub Display_Match()
    MatchReel.SetValue(Match \ 10) + 1
    If B2SOn then
        If Match = 0 then
            Controller.B2SSetMatch 100
        else
            Controller.B2SSetMatch Match
        end if
    end if
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
    DOF 232, DOFPulse

    'si hay falta el sistema de tilt se encargará de continuar con la siguente bola/jugador
    If Tilted Then
        StopEndOfBallMode
    End If

    ' si estás jugando y no hay falta
    If(bGameInPLay = True)AND(Tilted = False)Then

        ' ¿está el salva bolas activado?
        If(bBallSaverActive = True)Then

            ' ¿sí?, pues creamos una bola
            CreateNewBall()
        Else
            ' ¿es ésta la última bola en juego?
            If(BallsOnPlayfield = 0)Then
                StopEndOfBallMode
                vpmtimer.addtimer 500, "EndOfBall '" 'hacemos una pequeña pausa anter de continuar con el fin de bola
                Exit Sub
            End If
        End If
    End If
End Sub

Sub swPlungerRest_Hit()
    bBallInPlungerLane = True
End Sub

' La bola ha sido disparada, así que cambiamos la variable, que en esta mesa se usa solo para que el sonido del disparador cambie según hay allí una bola o no
' En otras mesas podrá usarse para poner en marcha un contador para salvar la bola

Sub swPlungerRest_UnHit()
    bBallInPlungerLane = False
End Sub

' *********************************************************************
'               Funciones para la cuenta de los puntos
' *********************************************************************

' Añade puntos al jugador, hace sonar las campanas y actualiza el backdrop

Sub AddScore(Points)
    If Tilted Then Exit Sub
    Select Case Points
        Case 10, 100, 1000
            ' añade los puntos a la variable del actual jugador
            Score(CurrentPlayer) = Score(CurrentPlayer) + points
            ' actualiza los contadores
            UpdateScore points
            ' hace sonar las campanillas de acuerdo a los puntos obtenidos
            If Points = 100 AND(Score(CurrentPlayer)MOD 1000) \ 100 = 0 Then  'nuevo reel de 1000
                PlaySound SoundFXDOF ("fx_bell1000", 303, DOFPulse, DOFChimes)
            ElseIf Points = 10 AND(Score(CurrentPlayer)MOD 100) \ 10 = 0 Then 'nuevo reel de 100
                PlaySound SoundFXDOF ("fx_bell100", 302, DOFPulse, DOFChimes)
            Else
                PlaySound SoundFXDOF ("fx_bell", 301, DOFPulse, DOFChimes) &Points
            End If
        Case 50
            Add10 = Add10 + 5
            AddScore10Timer.Enabled = TRUE
        Case 300
            Add100 = Add100 + 3
            AddScore100Timer.Enabled = TRUE
        Case 500
            Add100 = Add100 + 5
            AddScore100Timer.Enabled = TRUE
        Case 2000, 3000, 4000, 5000
            Add1000 = Add1000 + Points \ 1000
            AddScore1000Timer.Enabled = TRUE
    End Select

    ' ' aquí se puede hacer un chequeo si el jugador ha ganado alguna puntuación alta y darle un crédito ó bola extra
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

'******************************
'TIMER DE 10, 100 y 1000 PUNTOS
'******************************

' hace sonar las campanillas según los puntos

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

'*******************
'     BONOS
'*******************

' ESTA MESA NO USA BONOS

' avanza el bono y actualiza las luces
' Los bonos están limitados a 1500 puntos

Sub AddBonus(bonuspoints)
    If(Tilted = False)Then
        ' añade los bonos al jugador actual
        Bonus = Bonus + bonuspoints
        If Bonus > 15 Then
            Bonus = 15
        End If
        ' actualiza las luces
        UpdateBonusLights
    End if
End Sub

'***********************************************************************************
'        Score reels - puntuaciones - y actualiza otras luces del backdrop
'***********************************************************************************
'esta es al rutina que actualiza la puntuación del jugador

Sub UpdateScore(playerpoints)
    Select Case CurrentPlayer
        Case 1:ScoreReel1.Addvalue playerpoints
    ' Case 2:ScoreReel2.Addvalue playerpoints
    ' Case 3:ScoreReel3.Addvalue playerpoints
    ' Case 4:ScoreReel4.Addvalue playerpoints
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

    If B2SOn then
        Controller.B2SSetScorePlayer1 0
        Controller.B2SSetScoreRolloverPlayer1 0
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
    If Credits > 0 Then 'in las mesas de Bally
    'CreditLight.State = 1
    Else
    'CreditLight.State = 0
    End If
    PlaySound "fx_relay"
    CreditsReel.SetValue credits
    If B2SOn then
        Controller.B2SSetCredits Credits
    end if
End Sub

Sub UpdateBallInPlay 'actualiza los marcadores de las bolas, el número de jugador y el número total de jugadores
    Select Case Balls
        Case 0:BallDisplay.ImageA = "Ballnr0"
        Case 1:BallDisplay.ImageA = "Ballnr1"
        Case 2:BallDisplay.ImageA = "Ballnr2"
        Case 3:BallDisplay.ImageA = "Ballnr3"
        Case 4:BallDisplay.ImageA = "Ballnr4"
        Case 5:BallDisplay.ImageA = "Ballnr5"
    End Select
    If B2SOn then
        Controller.B2SSetBallInPlay Balls
    end if
End Sub

'*************************
' Partidas y bolas extras
'*************************

Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
        PlaySound SoundFXDOF ("fx_knocker", 300, DOFPulse, DOFKnocker)
        DOF 230, DOFPulse
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
    'LightShootAgain.State = 1
    'If B2SOn then
    '    Controller.B2SSetShootAgain 1
    'end if
    END If
End Sub

Sub AwardSpecial()
    PlaySound SoundFXDOF ("fx_knocker", 300, DOFPulse, DOFKnocker)
    DOF 230, DOFPulse
    AddCredits 1
End Sub

' ********************************
'        Attract Mode
' ********************************
' las luces simplemente parpadean de acuerdo a los valores que hemos puesto en el "Blink Pattern" de cada luz

Sub StartAttractMode()
    Dim x
    bAttractMode = True
    StartLightSeq
    ' enciente la luz de fin de partida, bola en juego, ++
    GameOverR.SetValue 1
    BallDisplay.ImageA = "ballnr0"
    If Credits > 0 Then DOF 200, DOFOn
    DOF 201, DOFOff
End Sub

Sub StopAttractMode()
    Dim x
    bAttractMode = False
    'LightSeqAttract.StopPlay
    ResetScores
    ' apaga la luz de fin de partida
    GameOverR.SetValue 0
End Sub

Sub StartLightSeq()
    'lights sequences
    'LightSeqAttract.UpdateInterval = 250
    'LightSeqAttract.Play SeqRandom, 10, , 50000
End Sub

Sub LightSeqAttract_PlayDone()
    StartLightSeq()
End Sub

'*************************************
'      Highscore Routines 2026
' Based og Black's and GNANCE's code
'*************************************

Dim EnteringInitials
EnteringInitials = False

Dim SelectedChar   ' character under the "cursor" when entering initials
Dim HSTimerCount:HSTimerCount = 0
Dim InitialString  ' the string holding the player's initials as they're entered
Dim AlphaString:AlphaString = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_<"
Dim AlphaStringPos ' pointer to AlphaString, move Forward and backward with flipper keys
Dim HSNewHigh      ' The new score to be recorded
Dim HSScore(5)     ' High Scores read in from config file
Dim HSName(5)      ' High Score Initials read in from config file
Dim HS 'array with the flashers
HS = Array(HSpostit,HS1,HS2,HS3,HS4,HS5,HS6,HS7,HS8,HS9,HS10,HS11,HS12,HS13,HS14,HS15,HS16,HS17,HS18,HS19,HS20,HS21,HS22,HS23,HS24,HS25,HS26,HS27,HS28,HS29,HS30,HS31)

Sub Loadhs
    Dim x
    x = LoadValue(TableName, "HighScore1")
    If(x <> "") Then HSScore(1) = CDbl(x) Else HSScore(1) = 20000 End If
    x = LoadValue(TableName, "HighScore1Name")
    If(x <> "") Then HSName(1) = x Else HSName(1) = "AAA" End If
    x = LoadValue(TableName, "HighScore2")
    If(x <> "") then HSScore(2) = CDbl(x) Else HSScore(2) = 19000 End If
    x = LoadValue(TableName, "HighScore2Name")
    If(x <> "") then HSName(2) = x Else HSName(2) = "BBB" End If
    x = LoadValue(TableName, "HighScore3")
    If(x <> "") then HSScore(3) = CDbl(x) Else HSScore(3) = 18000 End If
    x = LoadValue(TableName, "HighScore3Name")
    If(x <> "") then HSName(3) = x Else HSName(3) = "CCC" End If
    x = LoadValue(TableName, "HighScore4")
    If(x <> "") then HSScore(4) = CDbl(x) Else HSScore(4) = 17000 End If
    x = LoadValue(TableName, "HighScore4Name")
    If(x <> "") then HSName(4) = x Else HSName(4) = "DDD" End If
    x = LoadValue(TableName, "HighScore5")
    If(x <> "") then HSScore(5) = CDbl(x) Else HSScore(5) = 16000 End If
    x = LoadValue(TableName, "HighScore5Name")
    If(x <> "") then HSName(5) = x Else HSName(5) = "EEE" End If
    x = LoadValue(TableName, "Credits")
    If(x <> "") then Credits = CInt(x) Else Credits = 0
End Sub

Sub Savehs
    SaveValue TableName, "HighScore1", HSScore(1)
    SaveValue TableName, "HighScore1Name", HSName(1)
    SaveValue TableName, "HighScore2", HSScore(2)
    SaveValue TableName, "HighScore2Name", HSName(2)
    SaveValue TableName, "HighScore3", HSScore(3)
    SaveValue TableName, "HighScore3Name", HSName(3)
    SaveValue TableName, "HighScore4", HSScore(4)
    SaveValue TableName, "HighScore4Name", HSName(4)
    SaveValue TableName, "HighScore5", HSScore(5)
    SaveValue TableName, "HighScore5Name", HSName(5)
    SaveValue TableName, "Credits", Credits
End Sub

Sub Reseths
    HSName(1) = "AAA"
    HSName(2) = "BBB"
    HSName(3) = "CCC"
    HSName(4) = "DDD"
    HSName(5) = "EEE"
    HSScore(1) = 20000
    HSScore(2) = 19000
    HSScore(3) = 18000
    HSScore(4) = 17000
    HSScore(5) = 16000
    Savehs
End Sub

' Sort Highscores

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 1 to 5
        For j = 1 to 4
            If HSScore(j) < HSScore(j + 1) Then
                tmp = HSScore(j + 1)
                tmp2 = HSName(j + 1)
                HSScore(j + 1) = HSScore(j)
                HSName(j + 1) = HSName(j)
                HSScore(j) = tmp
                HSName(j) = tmp2
            End If
        Next
    Next
End Sub

' check for new highscore

Sub CheckHighscore
    HighScoreTimer.Enabled = 1
    'If Score(CurrentPlayer)> HSSCore(1)Then
    '   AwardSpecial
    'End If
    If Score(CurrentPlayer) > HSSCore(5) Then
        NewHighScore Score(CurrentPlayer), CurrentPlayer
    End If
End Sub

' Highscore Timer

Sub HighScoreTimer_Timer
    If EnteringInitials then
        SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
    ElseIf bGameInPlay then
        SetHSLine 1, "HIGH SCORE"
        SetHSLine 2, HSScore(1)
        SetHSLine 3, HSName(1)
        Me.Enabled = 0
    Else
        ' cycle through high scores
        SetHSLine 1, "HIGH SCORE" & (HSTimerCount + 1)
        SetHSLine 2, HSScore(HSTimerCount + 1)
        SetHSLine 3, HSName(HSTimerCount + 1)
        HSTimerCount = (HSTimerCount + 1) MOD 5
    End If
End Sub

' Enter Highscore initials

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

Sub SetHsLine(LineNo, String)
    Dim StrLen
    Dim Index
    Dim StartHSArray
    Dim EndHSArray
    Dim xFor
    StartHSArray = array(0, 1, 12, 22)
    EndHSArray = array(0, 11, 21, 31)
    StrLen = len(string)
    Index = 1
    For xFor = StartHSArray(LineNo) to EndHSArray(LineNo)
        HS(xFor).imageA = GetHSChar(String, Index)
        Index = Index + 1
    Next
End Sub

Sub NewHighScore(NewScore, PlayNum)
    If NewScore > HSScore(5) then
        HighScoreTimer.interval = 500
        AlphaStringPos = 1      ' start with first character "A"
        EnteringInitials = true ' intercept the control keys while entering initials
        InitialString = ""      ' initials entered so far, initialize to empty
        SetHSLine 1, "PLAYER " + FormatNumber(PlayNum, 0)
        SetHSLine 2, "ENTER NAME"
        SetHSLine 3, MID(AlphaString, AlphaStringPos, 1)
        HSNewHigh = NewScore
    End If
End Sub

Sub CollectInitials(keycode)
    Dim i
    If keycode = LeftFlipperKey Then
        ' back up to previous character
        AlphaStringPos = AlphaStringPos - 1
        If AlphaStringPos < 1 then
            AlphaStringPos = len(AlphaString) ' handle wrap from beginning to End
            If InitialString = "" then
                ' Skip the backspace If there are no characters to backspace over
                AlphaStringPos = AlphaStringPos - 1
            End If
        End If
        SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
        PlaySound "fx_Previous"
    ElseIf keycode = RightFlipperKey Then
        ' advance to Next character
        AlphaStringPos = AlphaStringPos + 1
        If AlphaStringPos > len(AlphaString) or(AlphaStringPos = len(AlphaString) and InitialString = "") then
            ' Skip the backspace If there are no characters to backspace over
            AlphaStringPos = 1
        End If
        SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
        PlaySound "fx_Next"
    ElseIf keycode = StartGameKey or keycode = PlungerKey Then
        SelectedChar = MID(AlphaString, AlphaStringPos, 1)
        If SelectedChar = "_" then
            InitialString = InitialString & " "
            PlaySound("fx_Esc")
        ElseIf SelectedChar = "<" then
            InitialString = MID(InitialString, 1, len(InitialString) - 1)
            If len(InitialString) = 0 then
                ' If there are no more characters to back over, don't leave the < displayed
                AlphaStringPos = 1
            End If
            PlaySound("fx_Esc")
        Else
            InitialString = InitialString & SelectedChar
            PlaySound("fx_Enter")
        End If
        If len(InitialString) < 3 then
            SetHSLine 3, InitialString & SelectedChar
        End If
    End If
    If len(InitialString) = 3 then
        EnteringInitials = False
        HSScore(5) = HSNewHigh
        HSName(5) = InitialString
        SortHighScore
        HighScoreTimer.interval = 2000
    End If
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

Sub VPObjects_Init 'inicia objetos al empezar

    TurnOffPlayfieldLights()
    'top lanes, solo se encienden al arrancar la mesa
    li10.State = 1
    li11.State = 1
    li12.State = 1
'
End Sub

' variables de la mesa

Sub Game_Init() 'esta rutina se llama al principio de un nuevo juego

'Empezar alguna música, si hubiera música en esta mesa

'iniciar variables, en esta mesa hay muy pocas variables ya que usamos las luces, y el UserValue de las dianas

'iniciar algún timer

'Iniciar algunas luces
'TurnOffPlayfieldLights()

End Sub

Sub StopEndOfBallMode()     'this sub is called after the last ball is drained

End Sub

Sub ResetNewBallVariables() 'inicia las variable para una bola ó jugador nuevo

End Sub

Sub ResetNewBallLights()    'enciende ó apaga las luces para una bola nueva
    li5.State = 1           'dianas centrales
    li6.State = 1
    li7.State = 1
    li3.State = 0 'bola extras
    li4.State = 0
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
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

' la bola choca contra los Slingshots
' hacemos una animación manual de los slingshots usando gomas
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
        Case 3:LeftSLing2.Visible = 0:Lemk.RotX = -20:LeftSlingShot.TimerEnabled = 0
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
        Case 3:RightSLing2.Visible = 0:Remk.RotX = -20:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

'***********************
'  Gomas de 10 puntos
'***********************

Sub RubberBand4_Hit
    If Tilted Then Exit Sub
    LastSwitchHit = "RubberBand4"
    ' añade 10 puntos
    AddScore 10
End Sub

Sub RubberBand14_Hit
    If Tilted Then Exit Sub
    LastSwitchHit = "RubberBand14"
    ' añade 10 puntos
    AddScore 10
End Sub

Sub RubberBand6_Hit
    If Tilted Then Exit Sub
    LastSwitchHit = "RubberBand6"
    ' añade 10 puntos
    AddScore 10
End Sub

Sub RubberBand7_Hit
    If Tilted Then Exit Sub
    LastSwitchHit = "RubberBand7"
    ' añade 10 puntos
    AddScore 10
End Sub

Sub RubberBand8_Hit
    If Tilted Then Exit Sub
    LastSwitchHit = "RubberBand8"
    ' añade 10 puntos
    AddScore 10
End Sub

Sub RubberBand9_Hit
    If Tilted Then Exit Sub
    LastSwitchHit = "RubberBand9"
    ' añade 10 puntos
    AddScore 10
End Sub

Sub RubberBand10_Hit
    If Tilted Then Exit Sub
    LastSwitchHit = "RubberBand10"
    ' añade 10 puntos
    AddScore 10
End Sub

Sub RubberBand11_Hit
    If Tilted Then Exit Sub
    LastSwitchHit = "RubberBand11"
    ' añade 10 puntos
    AddScore 10
End Sub

'******************************
'     DOF lights ball entrance
'******************************
'
Sub Trigger001_Hit
    DOF 240, DOFPulse
End sub

'*********
' Bumpers
'*********

Sub Bumper1_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_Bumper",105,DOFPulse,DOFContactors), bumper1
    DOF 207, DOFPulse
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    ' añade algunos puntos
    AddScore 10 + 90 * LightBumper1.State
    LastSwitchHit = "bumper1"
End Sub

Sub Bumper2_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_Bumper",107,DOFPulse,DOFContactors), bumper2
    DOF 208, DOFPulse
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    ' añade algunos puntos
    AddScore 10 + 90 * LightBumper2.State
    LastSwitchHit = "bumper2"
End Sub

Sub Bumper3_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_Bumper",108,DOFPulse,DOFContactors), bumper3
    DOF 209, DOFPulse
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    ' añade algunos puntos
    AddScore 10 + 90 * LightBumper3.State
    LastSwitchHit = "bumper3"
End Sub

Sub Bumper4_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_Bumper",109,DOFPulse,DOFContactors), bumper4
    DOF 210, DOFPulse
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    ' añade algunos puntos
    AddScore 10 + 90 * LightBumper4.State
    LastSwitchHit = "bumper4"
End Sub

Sub Bumper5_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_Bumper",106,DOFPulse,DOFContactors), bumper5
    DOF 211, DOFPulse
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    ' añade algunos puntos
    AddScore 100 + 900 * LightBumper5.State
    IncreaseMatch 'es lo único que aumenta el valor de la lotería.
    LastSwitchHit = "bumper5"
End Sub

'*****************
'     Pasillos
'*****************

' superiores

Sub Trigger5_Hit
    PlaySoundAt "fx_sensor", Trigger5
    If Tilted Then Exit Sub
    DOF 222, DOFPulse
    AddScore 100
    LastSwitchHit = "trigger5"
    li10.State = 0
    'check?
    CheckTopLights
End Sub

Sub Trigger6_Hit
    PlaySoundAt "fx_sensor", Trigger6
    If Tilted Then Exit Sub
    DOF 223, DOFPulse
    AddScore 100
    LastSwitchHit = "trigger6"
    li11.State = 0
    'check?
    CheckTopLights
End Sub

Sub Trigger7_Hit
    PlaySoundAt "fx_sensor", Trigger7
    If Tilted Then Exit Sub
    DOF 224, DOFPulse
    AddScore 100
    LastSwitchHit = "trigger7"
    li12.State = 0
    'check?
    CheckTopLights
End Sub

' superiores laterales

Sub Trigger8_Hit
    PlaySoundAt "fx_sensor", Trigger8
    If Tilted Then Exit Sub
    DOF 225, DOFPulse
    AddScore 500
    LastSwitchHit = "trigger8"
    'check?
    If li8.State Then
        AwardSpecial
        ResetAdvance
    End If
End Sub

Sub Trigger9_Hit
    PlaySoundAt "fx_sensor", Trigger9
    If Tilted Then Exit Sub
    DOF 226, DOFPulse
    AddScore 500
    LastSwitchHit = "trigger9"
    'check?
    If li9.State Then
        AwardSpecial
        ResetAdvance
    End If
End Sub

' outlanes

Sub Trigger1_Hit
    PlaySoundAt "fx_sensor", Trigger1
    If Tilted Then Exit Sub
    DOF 227, DOFPulse
    LastSwitchHit = "trigger1"
    AddScore 1000
    'check?
    If li1.State Then
        AwardSpecial
        StopSpecialTimer
    End If
    If li3.State Then
        AwardExtraBall
    End If
End Sub

Sub Trigger4_Hit
    PlaySoundAt "fx_sensor", Trigger4
    If Tilted Then Exit Sub
    DOF 228, DOFPulse
    AddScore 1000
    LastSwitchHit = "trigger4"
    'check?
    If li2.State Then
        AwardSpecial
        StopSpecialTimer
    End If
    If li4.State Then
        AwardExtraBall
    End If
End Sub

' inlanes

Sub Trigger2_Hit
    PlaySoundAt "fx_sensor", Trigger1
    If Tilted Then Exit Sub
    DOF 229, DOFPulse
    AddScore 500
    LastSwitchHit = "trigger2"
'check?
End Sub

Sub Trigger3_Hit
    PlaySoundAt "fx_sensor", Trigger3
    If Tilted Then Exit Sub
    DOF 231, DOFPulse
    LastSwitchHit = "trigger3"
    AddScore 500
'check?
End Sub

Sub CheckTopLights 'luces 123 encienden los bumper y el special de los outlanes, y empieza el timer de 30 segundos
    Dim x
    IF li10.State + li11.State + li12.State = 0 Then
        If NOT SpecialTimerStarted Then
            For each x in bumperlights
                x.State = 1
            Next
            li1.State = 1
            li2.State = 1
            SpecialTimerStep = 9
            SpecialTimer.Interval = Tiempo * 100
            SpecialTimerStarted = True
            SpecialTimer_Timer
            SpecialTimer.Enabled = 1
        End If
    End If
End Sub

Dim SpecialTimerStep
Dim SpecialTimerStarted
SpecialTimerStarted = False

Sub SpecialTimer_Timer
    If SpecialTimerStep = -1 Then
        StopSpecialTimer
    Else
        StopSound "Time"
        PlaySound "time"
        If B2sOn then
            Controller.B2SSetData 50, 0
            Controller.B2SSetData 51, 0
            Controller.B2SSetData 52, 0
            Controller.B2SSetData 53, 0
            Controller.B2SSetData 54, 0
            Controller.B2SSetData 55, 0
            Controller.B2SSetData 56, 0
            Controller.B2SSetData 57, 0
            Controller.B2SSetData 58, 0
            Controller.B2SSetData 59, 0
            Controller.B2SSetData 50 + SpecialTimerStep, 1
        end if
        Select Case SpecialTimerStep
            Case 9:tl9.State = 1
            Case 8:tl9.State = 0:tl8.State = 1
            Case 7:tl8.State = 0:tl7.State = 1
            Case 6:tl7.State = 0:tl6.State = 1
            Case 5:tl6.State = 0:tl5.State = 1
            Case 4:tl5.State = 0:tl4.State = 1
            Case 3:tl4.State = 0:tl3.State = 1
            Case 2:tl3.State = 0:tl2.State = 1
            Case 1:tl2.State = 0:tl1.State = 1
            Case 0:tl1.State = 0:tl0.State = 1
        End Select
        SpecialTimerStep = SpecialTimerStep - 1
    End If
End Sub

Sub StopSpecialTimer
    StopSound "Time"
    Dim x
    For each x in bumperlights
        x.State = 0
    Next
    For each x in TimerLights
        x.State = 0
    Next
    If B2SOn then
        For x = 50 to 59
            Controller.B2SSetData x, 0
        next
    end if
    li1.State = 0
    li2.State = 0
    li10.State = 1
    li11.State = 1
    li12.State = 1
    SpecialTimer.Enabled = 0
    SpecialTimerStarted = False
End Sub

Sub ResetAdvance
    Dim x
    For each x in AdvanceLights
        x.State = 0
    Next
    Li8.State = 0
    Li9.State = 0
End Sub

'************************
'       Dianas
'************************

Sub Target1_hit
    PlaySoundAtBall SoundFXDOF ("fx_target", 110, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 212, DOFPulse
    If Epileptikdof = True Then DOF 213, DOFPulse End If
    LastSwitchHit = "target1"
    AddScore 1000
    li5.State = 0
'check
End Sub

Sub Target2_hit
    PlaySoundAtBall SoundFXDOF ("fx_target", 110, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 212, DOFPulse
    If Epileptikdof = True Then DOF 213, DOFPulse End If
    LastSwitchHit = "target2"
    AddScore 1000
    If li5.State = 0 Then li6.State = 0
'check
End Sub

Sub Target3_hit
    PlaySoundAtBall SoundFXDOF ("fx_target", 110, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    LastSwitchHit = "target3"
    DOF 212, DOFPulse
    If Epileptikdof = True Then DOF 213, DOFPulse End If
    AddScore 1000
    If li5.State = 0 AND li6.State = 0 Then 'enciende la bola extra
        li7.State = 0
        li3.State = 1
        li4.State = 1
    End If
'check
End Sub

'******
' Setas
'******

Sub ch1_hit 'verdes
    PlaySoundAtBall SoundFXDOF ("fx_target", 111, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 214, DOFPulse
    If Epileptikdof = True Then DOF 215, DOFPulse End If
    LastSwitchHit = "ch1"
    AddScore 100
    If Li21.State = 0 Then li21.State = 1:Exit Sub
    If Li17.State = 0 Then li17.State = 1:Exit Sub
    If Li13.State = 0 Then li13.State = 1
End Sub

Sub ch1_animate:mushrromP001.TransZ = - ch1.CurrentRingOffset / 6:End Sub

Sub ch2_hit 'azules
    PlaySoundAtBall SoundFXDOF ("fx_target", 112, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 216, DOFPulse
    If Epileptikdof = True Then DOF 217, DOFPulse End If
    LastSwitchHit = "ch2"
    AddScore 100
    If Li15.State AND li24.State = 0 Then li24.State = 1:Exit Sub
    If Li24.State AND Li20.State = 0 Then li20.State = 1:Exit Sub
    If Li20.State AND Li16.State = 0 Then
        li16.State = 1
        li8.State = 1
        li9.State = 1
    End If
End Sub

Sub ch2_animate:mushrromP002.TransZ = - ch2.CurrentRingOffset / 6:End Sub

Sub ch3_hit 'amarillos
    PlaySoundAtBall SoundFXDOF ("fx_target", 113, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 218, DOFPulse
    If Epileptikdof = True Then DOF 219, DOFPulse End If
    LastSwitchHit = "ch3"
    AddScore 100
    If Li13.State AND li22.State = 0 Then li22.State = 1:Exit Sub
    If Li22.State AND Li18.State = 0 Then li18.State = 1:Exit Sub
    If Li18.State AND Li14.State = 0 Then li14.State = 1
End Sub

Sub ch3_animate:mushrromP003.TransZ = - ch3.CurrentRingOffset / 6:End Sub

Sub ch4_hit 'rojos
    PlaySoundAtBall SoundFXDOF ("fx_target", 114, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 220, DOFPulse
    If Epileptikdof = True Then DOF 221, DOFPulse End If
    LastSwitchHit = "ch4"
    AddScore 100
    If Li14.State AND li23.State = 0 Then li23.State = 1:Exit Sub
    If Li23.State AND Li19.State = 0 Then li19.State = 1:Exit Sub
    If Li19.State AND Li15.State = 0 Then li15.State = 1
End Sub

Sub ch4_animate:mushrromP004.TransZ = - ch4.CurrentRingOffset / 6:End Sub

'*********************************
' Table Options F12 User Options
'*********************************
' Table1.Option arguments are: 
' - option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional array of literal strings

Dim LUTImage, BallsPerGame, Tiempo
Dim dspTriggered : dspTriggered = False
LUTImage = Array("LUT0", "LUT1", "LUT2", "LUT3", "LUT4", "LUT5", "LUT6", "LUT7", "LUT8", "LUT9", "LUT10", _
        "LUT Warm 0", "LUT Warm 1", "LUT Warm 2", "LUT Warm 3", "LUT Warm 4", "LUT Warm 5", "LUT Warm 6", "LUT Warm 7", "LUT Warm 8", "LUT Warm 9", "LUT Warm 10", _
        "LUT Warmer 0", "LUT Warmer 1", "LUT Warmer 2", "LUT Warmer 3", "LUT Warmer 4", "LUT Warmer 5", "LUT Warmer 6", "LUT Warmer 7", "LUT Warmer 8", "LUT Warmer 9", "LUT Warmer 10")

Sub Table1_OptionEvent(ByVal eventId)
    If eventId = 1 And Not dspTriggered Then dspTriggered = True : DisableStaticPreRendering = True : End If

    Dim x, y

    'LUT
    x = Table1.Option("Select LUT", 0, 32, 1, 0, 0, Array("Normal 0", "Normal 1", "Normal 2", "Normal 3", "Normal 4", "Normal 5", "Normal 6", "Normal 7", "Normal 8", "Normal 9", "Normal 10", _
        "Warm 0", "Warm 1", "Warm 2", "Warm 3", "Warm 4", "Warm 5", "Warm 6", "Warm 7", "Warm 8", "Warm 9", "Warm 10", _
        "Warmer 0", "Warmer 1", "Warmer 2", "Warmer 3", "Warmer 4", "Warmer 5", "Warmer 6", "Warmer 7", "Warmer 8", "Warmer 9", "Warmer 10") )
    table1.ColorGradeImage =  LUTImage(x)

    ' Cabinet rails
    x = Table1.Option("Cabinet Rails", 0, 1, 1, 1, 0, Array("Hide", "Show") )
    For each y in aRails:y.visible = x:next

    ' Balls per Game
    x = Table1.Option("Balls per Game", 0, 1, 1, 1, 0, Array("3 Balls", "5 Balls") )
    If x = 1 Then BallsPerGame = 5 Else BallsPerGame = 3

    ' Game Time
    x = Table1.Option("Game Time", 1, 3, 1, 3, 0, Array("10 Seconds", "20 Seconds","30 Seconds") )
    Tiempo = x * 10

    ' Table's Slope
    x = Table1.Option("Table's Slope", 0, 10, 0.1, 5, 1)
    Table1.SlopeMin = x
    Table1.SlopeMax = x

    If eventId = 3 And dspTriggered Then dspTriggered = False : DisableStaticPreRendering = False : End If
End Sub