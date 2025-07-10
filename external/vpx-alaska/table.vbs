' ****************************************************************
'               Alaska / IPD No. 6604 /
'                   by 7he S4ge
' ****************************************************************

' 1.0 First version (by 7he S4ge)
' 1.1 DOF changes : separated some of the solenoid call outs and added two, added them to the DOF website (by Outhere)

Option Explicit
Randomize
'
' DOF config - leeoneil
'
' Option for more lights effects with DOF (Undercab effects on bumpers and slingshots) 
' Replace "False" by "True" to activate (False by default)
' Example : if you award extra ball, we enable DOF 208, DOFPulse
Const Epileptikdof = False
'
' Flippers L/R - 101/102
' Slingshot right - 104 (for ball release only)
' Bumpers L/R - 105/106/107
' Targets L/R - 103
' Drop Targets - 108/109
' Kickers - 111/112
' Drop Target Resets - 113/114
'
' LED backboard
' Flasher Outside Left - 205/208/209/215/219/221/227/228
' Flasher left - 203/208/211/216/223/224/227/228
' Flasher center - 207/227/228/231
' Flasher right - 204/208/212/214/217/225/226/227/228
' Flasher Outside Right - 
'
' Start Button - 150
' Undercab - 201/202
' Strobe - 230
' Knocker - 300


' Valores Constantes de las físicas de los flippers - se usan en la creación de las bolas, tienen que cargarse antes del core.vbs
Const BallSize = 50 ' el tamaño normal es 50 unidades de VP.
Const BallMass = 1  ' la pesadez de la bola, este valor va de acuerdo a la fuerza de los flippers y el plunger

' Game explain :
' Extra ball : all drop targets must be down, after that push the center target (not a drop target). If you won, the light between flippers turns on
' Double bonus : top left central lane, increases score on side bumpers
' Triple bonus : top right central lane, increases score on central bumper
' Advance bonus (5000pts and bonus) : through side top lanes, when hitting central target, or targets on both sides

' Carga el core.vbs para poder usas sus funciones, sobre todo el vpintimer.addtimer
LoadCoreFiles

Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    On Error Resume Next
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox "Can't open controller.vbs"
    On Error Resume Next
End Sub

' Valores Constants
Const TableName = "alaska" ' se usa para cargar y grabar los highscore y creditos
Const cGameName = "alaska" ' para el B2S
Const MaxPlayers = 4           ' de 1 a 4
Const MaxMultiplier = 3        ' limita el bonus multiplicador a 3
Const BallsPerGame = 5         ' normalmente 3 ó 5
Const Special1 = 640000        ' puntuación a obtener para partida extra
Const Special2 = 770000        ' puntuación a obtener para partida extra
Const Special3 = 890000        ' puntuación a obtener para partida extra

' Variables Globales
Dim PlayersPlayingGame
PlayersPlayingGame = 1
Dim CurrentPlayer
Dim Credits
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

Dim Blinker1

' Variables de control
Dim BallsOnPlayfield
Dim x, i, j

' Variables de tipo Boolean (verdadero ó falso, True ó False)
Dim bAttractMode
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bExtraBallWonThisBall
Dim bJustStarted
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim bAdvance ' True if advance mode activated

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
    ScoreReel1.SetValue HSScore(1)
    UpdateCredits

    ' Juego libre o con monedas: si es True entonces no se usarán monedas
    bFreePlay = False 'queremos monedas

    ' Inicialiar las variables globales de la mesa
    bAttractMode = False
    bOnTheFirstBall = False
    bGameInPlay = False
    bBallInPlungerLane = False
    BallsOnPlayfield = 0
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    Match = 0
    bJustStarted = True
	bAdvance = False
    Add10 = 0
    Add100 = 0
    Add1000 = 0

    ' pone la mesa en modo de espera
    EndOfGame

    'Turn on GI lights after one second
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
    If EnteringInitials then
        CollectInitials(keycode)
        Exit Sub
    End If

    ' add coins
    If Keycode = AddCreditKey Then
        If(Tilted = False)Then
            AddCredits 1
            PlaySound "fx_coin"
            DOF 150, DOFOn
        End If
    End If

	' Test match/lotery function
	If Keycode = 39 Then
		Verification_Match
	End If

    ' the plunger
    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
    End If

    ' Tilt keys
    If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
    If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
    If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt

    ' Funcionamiento normal de los flipers y otras teclas durante el juego

    If bGameInPlay AND NOT Tilted Then
		If Credits = 0 Then DOF 150, DOFOff
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
						DOF 150, DOFOff
						' no hay suficientes créditos para empezar el juego.
						'PlaySound "so_nocredits"
                    End If
                End If
            End If
        End If
    Else ' If (GameInPlay)
		If keycode = StartGameKey Then
			If(bFreePlay = True)Then ' Free play
				If(BallsOnPlayfield = 0)Then
					' New game
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
	
	If B2SOn then
        Controller.B2SSetCanPlay PlayersPlayingGame
    end if
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
	If B2SOn then
		Controller.Stop
	End if
End Sub

'********************
'     Flippers
'********************

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF ("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFXDOF ("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF ("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFXDOF ("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
    End If
End Sub

' el sonido de la bola golpeando los flipers

Sub LeftFlipper_Collide(parm)
    PlaySoundAtBall "fx_rubber_flipper"
End Sub

Sub RightFlipper_Collide(parm)
    PlaySoundAtBall "fx_rubber_flipper"
End Sub

'*******************
' Luces GI
'*******************

Sub GiOn 'enciende las luces GI
    DOF 201, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
End Sub

Sub GiOff 'apaga las luces GI
    DOF 201, DOFOff
    Dim bulb
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
        DisableTable True ' Tilt so we disable bumpers and slingshots
        'Esta mesa penaliza la partida, así que quítale las bolas al jugador
        BallsRemaining(CurrentPlayer) = 0
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

' Disable bumpers and slingshots because Tilt, or enable after
Sub DisableTable(Enabled)
    If Enabled Then
        'Apaga todas las luces Gi de la mesa
        GiOff
        'Disable slings, bumpers etc
        LeftFlipper2.RotateToStart
        LeftFlipper3.RotateToStart
        LeftFlipper4.RotateToStart
        LeftFlipper5.RotateToStart
        RightFlipper2.RotateToStart
        RightFlipper3.RotateToStart
        RightFlipper4.RotateToStart
        RightFlipper5.RotateToStart
        Bumper1.Force = 0
        Bumper2.Force = 0
		Bumper3.Force = 0
        DOF 101, DOFOff
        DOF 102, DOFOff
    'LeftSlingshot.Disabled = 1
    'RightSlingshot.Disabled = 1
    Else
        'enciende de nuevo todas las luces GI, bumpers y slingshots
        GiOn
        Bumper1.Force = 10
        Bumper2.Force = 10
		Bumper3.Force = 10
    'LeftSlingshot.Disabled = 0
    'RightSlingshot.Disabled = 0
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

' *********************************************************************
'               Funciones para los sonidos de la mesa
' *********************************************************************

Function Vol(ball) ' Calcula el volumen del sonido basado en la velocidad de la bola
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculala posición estéreo de la bola (izquierda a derecha)
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function

Function Pitch(ball) ' Calcula el tono según la velocidad de la bola
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calcula la velocidad de la bola
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2)))
End Function

Function AudioFade(ball) 'Solo para VPX 10.4 y siguentes: calcula la posición de arriba/abajo de la bola, para mesas con el sonido dolby
    Dim tmp
    tmp = ball.y * 2 / Table1.height-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) ' Hace sonar un sonido en la posición de un objeto, como bumpers y flippers
    PlaySound soundname, 0, 1, Pan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' hace sonar un sonido en la posición de la bola
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

'*****************************************
'      Los sonidos de la bola/s rodando
'*****************************************

Const tnob = 6 ' número total de bolas
Const lob = 0  'número de bola encerradas
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch
    BOT = GetBalls

    ' para el sonido de bolas perdidas
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

    ' sale de la rutina si no hay más bolas en la mesa
    If UBound(BOT) = lob - 1 Then Exit Sub

    ' hace sonar el sonido de la bola rodando para cada bola
    For b = lob to UBound(BOT)
        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 20000 'aumenta el tono del sonido si la bola está sobre una rampa
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), Pan(BOT(b)), 0, ballpitch, 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

'*****************************
' Sonido de las bolas chocando
'*****************************

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
' Initialize the table for a new game
'****************************************

Sub ResetForNewGame()
    'debug.print "ResetForNewGame"
    Dim i

    bGameInPLay = True
    bBallSaverActive = False
	bAdvance = False
	li11.State = 0

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
        
        ExtraBallsAwards(i) = 0
        Special1Awarded(i) = False
        Special2Awarded(i) = False
        Special3Awarded(i) = False
        BallsRemaining(i) = BallsPerGame
    Next
    BonusMultiplier = 1
    Bonus = 0
    UpdateBallInPlay

    Clear_Match

    ' inicializa otras variables
    Tilt = 0

    ' inicializa las variables del juego
    Game_Init()

    ' ahora puedes empezar una música si quieres
    ' empieza la rutina "Firstball" despues de una pequeña pausa
    'PlaySound "start"
    vpmtimer.addtimer 2000, "FirstBall '"
    DOF 227, DOFPulse
	If Credits = 0 Then DOF 150, DOFOff
End Sub

' esta pausa es para que la mesa tenga tiempo de poner los marcadores a cero y actualizar las luces

Sub FirstBall
    'debug.print "FirstBall"
    ' ajusta la mesa para una bola nueva, sube las drop targets, etc
    ResetForNewPlayerBall()
    ' crea una bola nueva en la zona del plunger
    CreateNewBall()
    AddScore 1
    AddScore -1
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
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass

    ' incrementa el número de bolas en el tablero, ya que hay que contarlas
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' actualiza las luces del backdrop
    UpdateBallInPlay

    ' y expulsa la bola
    PlaySoundAt SoundFXDOF ("fx_Ballrel", 104, DOFPulse, DOFContactors), BallRelease
    If Epileptikdof = True Then DOF 229, DOFPulse End If
    BallRelease.Kick 90, 4

	If B2SOn then
        Controller.B2SSetPlayerUp CurrentPlayer
    end if
End Sub

' The player has lost his ball, and there are no more balls in play
' Start counting bonuses

Sub EndOfBall()
    'debug.print "EndOfBall"
    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 0
    ' La primera se ha perdido. Desde aquí ya no se puede aceptar más jugadores
    bOnTheFirstBall = False

	bAdvance = False
	li11.State = 0

    ' solo recoge los bonos si no hay falta
    ' el sistema del la falta se encargará de nuevas bolas o del fin de la partida

    If NOT Tilted Then
        Select Case BonusMultiplier
            Case 1:BonusCountTimer.Interval = 250
            Case 2:BonusCountTimer.Interval = 400
            Case 3:BonusCountTimer.Interval = 550
        End Select
        BonusCountTimer.Enabled = 1
    Else 'If there is a fault just wait a moment and go straight to the second part after losing the ball
        vpmtimer.addtimer 400, "EndOfBall2 '"
    End If
End Sub

Sub BonusCountTimer_Timer 'add the bonuses and upgrade the lights
    'debug.print "BonusCount_Timer"
    If Bonus > 0 Then
        Bonus = Bonus -1
        AddScore 1000 * BonusMultiplier
        UpdateBonusLights
    Else
        ' ends of the bonus count and continues with the second part of the end of the ball
        BonusCountTimer.Enabled = 0
        vpmtimer.addtimer 1000, "EndOfBall2 '"
    End If
End Sub

Sub UpdateBonusLights 'turn the bonus lights on or off based on the "Bonus" variable
	' Bonus max : 15
    Select Case Bonus
        Case 0:li20.State = 0:li21.State = 0:li22.State = 0:li23.State = 0:li24.State = 0:li25.State = 0:li26.State = 0:li27.State = 0:li28.State = 0:li29.State = 0
        Case 1:li20.State = 1:li21.State = 0:li22.State = 0:li23.State = 0:li24.State = 0:li25.State = 0:li26.State = 0:li27.State = 0:li28.State = 0:li29.State = 0
        Case 2:li20.State = 0:li21.State = 1:li22.State = 0:li23.State = 0:li24.State = 0:li25.State = 0:li26.State = 0:li27.State = 0:li28.State = 0:li29.State = 0
        Case 3:li20.State = 0:li21.State = 0:li22.State = 1:li23.State = 0:li24.State = 0:li25.State = 0:li26.State = 0:li27.State = 0:li28.State = 0:li29.State = 0
        Case 4:li20.State = 0:li21.State = 0:li22.State = 0:li23.State = 1:li24.State = 0:li25.State = 0:li26.State = 0:li27.State = 0:li28.State = 0:li29.State = 0
        Case 5:li20.State = 0:li21.State = 0:li22.State = 0:li23.State = 0:li24.State = 1:li25.State = 0:li26.State = 0:li27.State = 0:li28.State = 0:li29.State = 0
        Case 6:li20.State = 0:li21.State = 0:li22.State = 0:li23.State = 0:li24.State = 0:li25.State = 1:li26.State = 0:li27.State = 0:li28.State = 0:li29.State = 0
        Case 7:li20.State = 0:li21.State = 0:li22.State = 0:li23.State = 0:li24.State = 0:li25.State = 0:li26.State = 1:li27.State = 0:li28.State = 0:li29.State = 0
        Case 8:li20.State = 0:li21.State = 0:li22.State = 0:li23.State = 0:li24.State = 0:li25.State = 0:li26.State = 0:li27.State = 1:li28.State = 0:li29.State = 0
        Case 9:li20.State = 0:li21.State = 0:li22.State = 0:li23.State = 0:li24.State = 0:li25.State = 0:li26.State = 0:li27.State = 0:li28.State = 1:li29.State = 0
        Case 10:li20.State = 0:li21.State = 0:li22.State = 0:li23.State = 0:li24.State = 0:li25.State = 0:li26.State = 0:li27.State = 0:li28.State = 0:li29.State = 1
        Case 11:li20.State = 1:li21.State = 0:li22.State = 0:li23.State = 0:li24.State = 0:li25.State = 0:li26.State = 0:li27.State = 0:li28.State = 0:li29.State = 1
        Case 12:li20.State = 0:li21.State = 1:li22.State = 0:li23.State = 0:li24.State = 0:li25.State = 0:li26.State = 0:li27.State = 0:li28.State = 0:li29.State = 1
        Case 13:li20.State = 0:li21.State = 0:li22.State = 1:li23.State = 0:li24.State = 0:li25.State = 0:li26.State = 0:li27.State = 0:li28.State = 0:li29.State = 1
        Case 14:li20.State = 0:li21.State = 0:li22.State = 0:li23.State = 1:li24.State = 0:li25.State = 0:li26.State = 0:li27.State = 0:li28.State = 0:li29.State = 1
        Case 15:li20.State = 0:li21.State = 0:li22.State = 0:li23.State = 0:li24.State = 1:li25.State = 0:li26.State = 0:li27.State = 0:li28.State = 0:li29.State = 1
    End Select
End Sub

' Ball lost (sometimes because Tilt)
' We check if the player has won extra balls
' If not we check if he is the last player or if this is the last ball
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
    DisableTable False 'enable bumpers and slingshots

    ' ¿ha ganado el jugador una bola extra?
    If(ExtraBallsAwards(CurrentPlayer) > 0)Then
        debug.print "Extra Ball"

        ' sí? entonces se la das al jugador
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer)- 1

        ' if there are no more balls turn off the 'shoot again' light
        If(ExtraBallsAwards(CurrentPlayer) = 0)Then
            LightShootAgain.State = 0
            If B2SOn then
                Controller.B2SSetShootAgain 0
            end if
        End If

' aquí se podría poner algún sonido de bola extra o alguna luz que parpadee

' En esta mesa hacemos la bola extra igual como si fuese la siguente bola, haciendo un reset de las variables y targets
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

	If B2SOn then
        Controller.B2SSetPlayerUp CurrentPlayer
    end if
End Sub

' Esta función se llama al final del juego

Sub EndOfGame()
    'debug.print "EndOfGame"
    bGameInPLay = False
    bJustStarted = False
	bAdvance = False
	li11.State = 0
    If B2SOn then
        Controller.B2SSetGameOver 1
        Controller.B2SSetBallInPlay 0
        Controller.B2SSetPlayerUp 0
        Controller.B2SSetCanPlay 0
    end if
    ' asegúrate de que los flippers están en modo de reposo
    SolLFlipper 0
    SolRFlipper 0
    DOF 227, DOFPulse
    ' pon las luces en el modo de fin de juego
    StartAttractMode
End Sub

' This function calculates the no of balls left (1 for the first ball in play, and 2, 3...)
Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp > BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
    End If
End Function

' Esta función calcula el Highscore y te da una partida gratis si has conseguido el Highscore
Sub CheckHighscore
    Dim playertops, si, sj, i, stemp, stempplayers
    For i = 1 to 4
        sortscores(i) = 0
        sortplayers(i) = 0
    Next
    playertops = 0
    For i = 1 to PlayersPlayingGame
        sortscores(i) = Score(i)
        sortplayers(i) = i
    Next
    For si = 1 to PlayersPlayingGame
        For sj = 1 to PlayersPlayingGame-1
            If sortscores(sj) > sortscores(sj + 1)then
                stemp = sortscores(sj + 1)
                stempplayers = sortplayers(sj + 1)
                sortscores(sj + 1) = sortscores(sj)
                sortplayers(sj + 1) = sortplayers(sj)
                sortscores(sj) = stemp
                sortplayers(sj) = stempplayers
            End If
        Next
    Next
    HighScoreTimer.interval = 100
    HighScoreTimer.enabled = True
    ScoreChecker = 4
    CheckAllScores = 1
    NewHighScore sortscores(ScoreChecker), sortplayers(ScoreChecker)
End Sub

'******************
'  Match - Loteria
'******************

Sub Verification_Match()
    'PlaySound "fx_match"
    Match = INT(RND(1) * 10) * 10 ' número aleatorio entre 0 y 90
	Display_Match
    If(Score(CurrentPlayer)MOD 100) = Match Then
        PlaySound SoundFXDOF ("fx_knocker", 300, DOFPulse, DOFKnocker)
        DOF 230, DOFPulse
        AddCredits 1
    End If
End Sub

Sub Clear_Match()
    Match0.SetValue 0
    Match1.SetValue 0
    Match2.SetValue 0
    Match3.SetValue 0
    Match4.SetValue 0
    Match5.SetValue 0
    Match6.SetValue 0
    Match7.SetValue 0
    Match8.SetValue 0
    Match9.SetValue 0
    If B2SOn then
        Controller.B2SSetMatch 0
    end if
End Sub

Sub Display_Match()
    Select case Match
        case 0:Match0.SetValue 1
        case 10:Match1.SetValue 1
        case 20:Match2.SetValue 1
        case 30:Match3.SetValue 1
        case 40:Match4.SetValue 1
        case 50:Match5.SetValue 1
        case 60:Match6.SetValue 1
        case 70:Match7.SetValue 1
        case 80:Match8.SetValue 1
        case 90:Match9.SetValue 1
    End Select
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
	DOF 231, DOFPulse

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
        Case 5000
            Add1000 = Add1000 + Points \ 1000
            AddScore1000Timer.Enabled = TRUE ' delays the addition of points
        Case 50000
            Add10000 = Add10000 + Points \ 10000
            AddScore10000Timer.Enabled = TRUE ' delays the addition of points
		Case Else
			' añade los puntos a la variable del actual jugador
            Score(CurrentPlayer) = Score(CurrentPlayer) + points
            ' actualiza los contadores
            UpdateScore points
            ' hace sonar las campanillas de acuerdo a los puntos obtenidos
            PlaySound Points
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

'************************************
'TIMER DE 10, 100, 1000 y 1000 PUNTOS
'************************************

' hace sonar las campanillas según los puntos
' delays the addition of points
Sub AddScore1000Timer_Timer()
    if Add1000 > 0 then
        AddScore 1000
        Add1000 = Add1000 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore10000Timer_Timer()
    if Add10000 > 0 then
        AddScore 10000
        Add10000 = Add10000 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

'*******************
'     BONUS
'*******************

' advance the bonus and upgrade the lights
' bonuses are limited to 1500 points

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

' Not used
Sub AddBonusMultiplier(multi)
    If(Tilted = False)Then
        ' add the bonuses to the current player
        BonusMultiplier = BonusMultiplier + multi
        If BonusMultiplier > MaxMultiplier Then
            BonusMultiplier = MaxMultiplier
        End If
        ' actualiza las luces
        UpdateMultiplierLights
    End if
End Sub

' Light for bonus multiplier
Sub UpdateMultiplierLights
    Select Case BonusMultiplier
        Case 1:li9.State = 0:li10.State = 0
        Case 2:li9.State = 1:li10.State = 0
		Case 3:li9.State = 0:li10.State = 1
    End Select
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
    millionR.SetValue Score(CurrentPlayer) \ 1000000
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
    If Credits > 0 Then 'en las mesas de Bally
    'CreditLight.State = 1
    Else
    'CreditLight.State = 0
    End If
    'PlaySound "fx_relay"
    CreditsReel.SetValue credits
    If B2SOn then
        Controller.B2SSetCredits Credits
    end if
End Sub

Sub UpdateBallInPlay 'actualiza los marcadores de las bolas, el número de jugador y el número total de jugadores
    Select Case Balls
        Case 1:bip1.State = 1
        Case 2:bip1.State = 0:bip2.State = 1
        Case 3:bip2.State = 0:bip3.State = 1
        Case 4:bip3.State = 0:bip4.State = 1
        Case 5:bip4.State = 0:bip5.State = 1
    End Select
    If B2SOn then
        Controller.B2SSetBallInPlay Balls
    end if
End Sub

'*************************
' Extra balls
'*************************

Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
        PlaySound SoundFXDOF ("fx_knocker", 300, DOFPulse, DOFKnocker)
        DOF 230, DOFPulse
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
        LightShootAgain.State = 1 ' turn on the light between the flippers
        If B2SOn then
            Controller.B2SSetShootAgain 1
        end if
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
    For each x in aLights
        x.State = 2
    Next
    ' enciente la luz de fin de partida, bola en juego, ++
    GameOverR.SetValue 1
    bip1.State = 0
    bip2.State = 0
    bip3.State = 0
    bip4.State = 0
    bip5.State = 0
	If Credits > 0 Then DOF 150, DOFOn
	DOF 201, DOFOff
End Sub

Sub StopAttractMode()
    Dim x
    bAttractMode = False
    TurnOffPlayfieldLights
    ResetScores
    ' apaga la luz de fin de partida
    GameOverR.SetValue 0
End Sub

Sub BackglassBlinker1_timer
	If B2SOn then
		if Blinker1=0 then
			Blinker1=1
			Controller.B2SSetData 60,1
		else
			Blinker1=0
			Controller.B2SSetData 60,0
		end if
	end if
end sub

'************************************************
'    Load (cargar) / Save (guardar)/ Highscore
'************************************************

Sub Loadhs
    ' Based on Black's Highscore routines
    Dim FileObj
    Dim ScoreFile, TextStr
    Dim temp1
    Dim temp2
    Dim temp3
    Dim temp4
    Dim temp5
    Dim temp6
    Dim temp8
    Dim temp9
    Dim temp10
    Dim temp11
    Dim temp12
    Dim temp13
    Dim temp14
    Dim temp15
    Dim temp16
    Dim temp17

    Set FileObj = CreateObject("Scripting.FileSystemObject")
    If Not FileObj.FolderExists(UserDirectory)then
        Credits = 0
        Exit Sub
    End If
    If Not FileObj.FileExists(UserDirectory & TableName& ".txt")then
        Credits = 0
        Exit Sub
    End If
    Set ScoreFile = FileObj.GetFile(UserDirectory & TableName& ".txt")
    Set TextStr = ScoreFile.OpenAsTextStream(1, 0)
    If(TextStr.AtEndOfStream = True)then
        Exit Sub
    End If
    temp1 = TextStr.ReadLine
    temp2 = textstr.readline

    HighScore = cdbl(temp1)
    If HighScore < 1 then
        temp8 = textstr.readline
        temp9 = textstr.readline
        temp10 = textstr.readline
        temp11 = textstr.readline
        temp12 = textstr.readline
        temp13 = textstr.readline
        temp14 = textstr.readline
        temp15 = textstr.readline
        temp16 = textstr.readline
        temp17 = textstr.readline
    End If
    TextStr.Close
    Credits = cdbl(temp2)

    If HighScore < 1 then
        HSScore(1) = int(temp8)
        HSScore(2) = int(temp9)
        HSScore(3) = int(temp10)
        HSScore(4) = int(temp11)
        HSScore(5) = int(temp12)

        HSName(1) = temp13
        HSName(2) = temp14
        HSName(3) = temp15
        HSName(4) = temp16
        HSName(5) = temp17
    End If
    Set ScoreFile = Nothing
    Set FileObj = Nothing
    SortHighscore 'added to fix a previous error
End Sub

Sub Savehs
    ' Based on Black's Highscore routines
    Dim FileObj
    Dim ScoreFile
    Dim xx
    Set FileObj = CreateObject("Scripting.FileSystemObject")
    If Not FileObj.FolderExists(UserDirectory)then
        Exit Sub
    End If
    Set ScoreFile = FileObj.CreateTextFile(UserDirectory & TableName& ".txt", True)
    ScoreFile.WriteLine 0
    ScoreFile.WriteLine Credits
    For xx = 1 to 5
        scorefile.writeline HSScore(xx)
    Next
    For xx = 1 to 5
        scorefile.writeline HSName(xx)
    Next
    ScoreFile.Close
    Set ScoreFile = Nothing
    Set FileObj = Nothing
End Sub

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 1 to 5
        For j = 1 to 4
            If HSScore(j) < HSScore(j + 1)Then
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

' ****************************************************
' GNMOD - Multiple High Score Display and Collection
' jpsalas: changed ramps by flashers
' ****************************************************

Dim EnteringInitials ' Normally zero, set to non-zero to enter initials
EnteringInitials = False
Dim ScoreChecker
ScoreChecker = 0
Dim CheckAllScores
CheckAllScores = 0
Dim sortscores(4)
Dim sortplayers(4)

Dim PlungerPulled
PlungerPulled = 0

Dim SelectedChar   ' character under the "cursor" when entering initials

Dim HSTimerCount   ' Pass counter For HS timer, scores are cycled by the timer
HSTimerCount = 5   ' Timer is initially enabled, it'll wrap from 5 to 1 when it's displayed

Dim InitialString  ' the string holding the player's initials as they're entered

Dim AlphaString    ' A-Z, 0-9, space (_) and backspace (<)
Dim AlphaStringPos ' pointer to AlphaString, move Forward and backward with flipper keys
AlphaString = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_<"

Dim HSNewHigh      ' The new score to be recorded

Dim HSScore(5)     ' High Scores read in from config file
Dim HSName(5)      ' High Score Initials read in from config file

' default high scores, remove this when the scores are available from the config file
HSScore(1) = 500000
HSScore(2) = 490000
HSScore(3) = 480000
HSScore(4) = 470000
HSScore(5) = 460000

HSName(1) = "AAA"
HSName(2) = "ZZZ"
HSName(3) = "XXX"
HSName(4) = "ABC"
HSName(5) = "BBB"

Sub HighScoreTimer_Timer
    If EnteringInitials then
        If HSTimerCount = 1 then
            SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
            HSTimerCount = 2
        Else
            SetHSLine 3, InitialString
            HSTimerCount = 1
        End If
    ElseIf bGameInPlay then
        SetHSLine 1, "HIGH SCORE1"
        SetHSLine 2, HSScore(1)
        SetHSLine 3, HSName(1)
        HSTimerCount = 5 ' set so the highest score will show after the game is over
        HighScoreTimer.enabled = false
    ElseIf CheckAllScores then
        NewHighScore sortscores(ScoreChecker), sortplayers(ScoreChecker)
    Else
        ' cycle through high scores
        HighScoreTimer.interval = 2000
        HSTimerCount = HSTimerCount + 1
        If HsTimerCount > 5 then
            HSTimerCount = 1
        End If
        SetHSLine 1, "HIGH SCORE" + FormatNumber(HSTimerCount, 0)
        SetHSLine 2, HSScore(HSTimerCount)
        SetHSLine 3, HSName(HSTimerCount)
    End If
End Sub

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
    Dim Letter
    Dim ThisDigit
    Dim ThisChar
    Dim StrLen
    Dim LetterLine
    Dim Index
    Dim StartHSArray
    Dim EndHSArray
    Dim LetterName
    Dim xFor
    StartHSArray = array(0, 1, 12, 22)
    EndHSArray = array(0, 11, 21, 31)
    StrLen = len(string)
    Index = 1

    For xFor = StartHSArray(LineNo)to EndHSArray(LineNo)
        Eval("HS" &xFor).imageA = GetHSChar(String, Index)
        Index = Index + 1
    Next
End Sub

Sub NewHighScore(NewScore, PlayNum)
    If NewScore > HSScore(5)then
        HighScoreTimer.interval = 500
        HSTimerCount = 1
        AlphaStringPos = 1      ' start with first character "A"
        EnteringInitials = true ' intercept the control keys while entering initials
        InitialString = ""      ' initials entered so far, initialize to empty
        SetHSLine 1, "PLAYER " + FormatNumber(PlayNum, 0)
        SetHSLine 2, "ENTER NAME"
        SetHSLine 3, MID(AlphaString, AlphaStringPos, 1)
        HSNewHigh = NewScore
        AwardSpecial
    End If
    ScoreChecker = ScoreChecker-1
    If ScoreChecker = 0 then
        CheckAllScores = 0
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
        If AlphaStringPos > len(AlphaString)or(AlphaStringPos = len(AlphaString)and InitialString = "")then
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
            InitialString = MID(InitialString, 1, len(InitialString)- 1)
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
        ' save the score
        For i = 5 to 1 step -1
            If i = 1 or(HSNewHigh > HSScore(i)and HSNewHigh <= HSScore(i - 1))then
                ' Replace the score at this location
                If i < 5 then
                    HSScore(i + 1) = HSScore(i)
                    HSName(i + 1) = HSName(i)
                End If
                EnteringInitials = False
                HSScore(i) = HSNewHigh
                HSName(i) = InitialString
                HSTimerCount = 5
                HighScoreTimer_Timer
                HighScoreTimer.interval = 2000
                PlaySound("fx_Bong")
                Exit Sub
            ElseIf i < 5 then
                ' move the score in this slot down by 1, it's been exceeded by the new score
                HSScore(i + 1) = HSScore(i)
                HSName(i + 1) = HSName(i)
            End If
        Next
    End If
End Sub

'****************************************
' Actualizaciones en tiempo real
'****************************************
' se usa sobre todo para hacer animaciones o sonidos que cambian en tiempo real
' como por ejemplo para sincronizar los flipers, puertas ó molinillos con primitivas

Sub GameTimer_Timer
    RollingUpdate 'actualiza el sonido de la bola rodando
' y también algunas animaciones, sobre todo de primitivas
	LeftFlipperP.RotZ = LeftFlipper.CurrentAngle
	RightFlipperP.RotZ = RightFlipper.CurrentAngle
End Sub

'***********************************************************************
' *********************************************************************
'            Aquí empieza el código particular a la mesa
' (hasta ahora todas las rutinas han sido muy generales para todas las mesas)
' (y hay muy pocas rutinas que necesitan cambiar de mesa a mesa)
' *********************************************************************
'***********************************************************************

' se inicia las drop targets, primitivas, etc.
' aunque en el VPX no hay muchos objetos que necesitan ser iniciados

Sub VPObjects_Init 'inicia objetos al empezar
End Sub

' variables de la mesa

Sub Game_Init() 'esta rutina se llama al principio de un nuevo juego

    'Empezar alguna música, si hubiera música en esta mesa

    'iniciar variables, en esta mesa hay muy pocas variables ya que usamos las luces, y el UserValue de las targets

    'iniciar algún timer

    'Iniciar algunas luces
    TurnOffPlayfieldLights()
End Sub

Sub StopEndOfBallMode()     'this sub is called after the last ball is drained
End Sub

Sub ResetNewBallVariables() 'inicia las variable para una bola ó jugador nuevo
    PlaySoundAt SoundFXDOF ("fx_ResetDrop", 113, DOFPulse, DOFContactors), target6
   ' PlaySoundAt "fx_ResetDrop", target6
    For each x in aDropTargets
        x.Isdropped = 0
        x.UserValue = 0
    Next
    PlaySoundAt SoundFXDOF ("fx_ResetDrop", 114, DOFPulse, DOFContactors), target12
    'PlaySoundAt "fx_ResetDrop", target12
End Sub

Sub ResetNewBallLights() 'turn the lights on and off for a new ball
    TurnOffPlayfieldLights
    li16.State = 1
    li17.State = 1
    li18.State = 1
    li19.State = 1
    li14.State = 0 ' special award
    li15.State = 0 ' special award
	li3.State = 1
	li4.State = 1
	li5.State = 1
	li6.State = 1
	li7.State = 1
	li8.State = 1
	li12.State = 1
	li13.State = 1	
	li9.State = 0 ' double bonus
	li10.State = 0 ' triple bonus

	' li2 : double bonus
	' If last ball : BonusMultiplier=1
	'If Balls = BallsPerGame Then li2.State = 1: BonusMultiplier = 2
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
	li7.State = 1
End Sub

' *********************************************************************
'       Eventos de la mesa - choque de la bola contra targets
'
' En cada diana u objeto con el que la bola choque habrá que hacer:
' - sonar un sonido físico
' - hacer algún movimiento, si es necesario
' - añadir alguna puntuación
' - encender/apagar una luz
' - hacer algún chequeo para ver si el jugador ha completado algo
' *********************************************************************

'*********
' Bumpers
'*********

'left side bumper
Sub Bumper1_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_Bumper",105,DOFPulse,DOFContactors), bumper1
    DOF 203, DOFPulse
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    ' añade algunos puntos
    AddScore 10 + 90 * li9.State 'DOUBLE BONUS LANE increases score on side bumpers
    RotateLights
End Sub

'right side bumper
Sub Bumper2_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_Bumper",107,DOFPulse,DOFContactors), bumper2
    DOF 204, DOFPulse
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    ' añade algunos puntos
    AddScore 10 + 90 * li9.State 'DOUBLE BONUS LANE increases score on side bumpers
    RotateLights
End Sub

'top center bumper
Sub Bumper3_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF ("fx_Bumper",106,DOFPulse,DOFContactors), bumper3
    DOF 204, DOFPulse
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    ' añade algunos puntos
    AddScore 10 + 90 * li10.State 'TRIPLE BONUS LANE increases score on central bumper
    RotateLights
End Sub

Sub RotateLights
    Dim tmp
    tmp = li8.State
    li8.State = li3.State
    li3.State = tmp
    tmp = li14.State ' Special 5000
    li14.State = li15.State
    li15.State = tmp ' Special 5000
End Sub

'*****************
'     Pasillos
'*****************

' outlanes

Sub Trigger2_Hit
    PlaySoundAt "fx_sensor", Trigger2
    If Tilted Then Exit Sub
    DOF 221, DOFPulse
    If li3.State Then
		AddScore 500
		li3.State = 0
	Else
		AddScore 100
	End If
    'check?
    'If li3.State Then AwardSpecial
End Sub

Sub Trigger5_Hit
    PlaySoundAt "fx_sensor", Trigger5
    If Tilted Then Exit Sub
    DOF 222, DOFPulse
    If li8.State Then
		AddScore 500
		li8.State = 0
	Else
		AddScore 100
	End If
    'check?
    'If li8.State Then AwardSpecial
End Sub

' inlanes
Sub Trigger1_Hit
    PlaySoundAt "fx_sensor", Trigger1
    If Tilted Then Exit Sub
    DOF 223, DOFPulse
    If li4.State Then
		AddScore 500
		li4.State = 0
	Else
		AddScore 100
	End If
End Sub

Sub Trigger3_Hit
    PlaySoundAt "fx_sensor", Trigger3
    If Tilted Then Exit Sub
    DOF 224, DOFPulse
    If li5.State Then
		AddScore 500
		li5.State = 0
	Else
		AddScore 100
	End If
End Sub

Sub Trigger4_Hit
    PlaySoundAt "fx_sensor", Trigger4
    If Tilted Then Exit Sub
    DOF 225, DOFPulse
    If li6.State Then
		AddScore 500
		li6.State = 0
	Else
		AddScore 100
	End If
End Sub

Sub Trigger6_Hit
    PlaySoundAt "fx_sensor", Trigger6
    If Tilted Then Exit Sub
    DOF 226, DOFPulse
    If li7.State Then
		AddScore 500
		li7.State = 0
	Else
		AddScore 100
	End If
End Sub

' side lanes

Sub Trigger7_Hit
    PlaySoundAt "fx_sensor", Trigger7
    If Tilted Then Exit Sub
    DOF 219, DOFPulse
	If li12.State Then
		AddScore 500
		li12.State = 0
	Else
		AddScore 100
	End If
End Sub

Sub Trigger8_Hit
    PlaySoundAt "fx_sensor", Trigger8
    If Tilted Then Exit Sub
    DOF 220, DOFPulse
    If li13.State Then
		AddScore 500
		li13.State = 0
	Else
		AddScore 100
	End If
End Sub

' top lanes

' Top left lane
Sub Trigger9_Hit
    PlaySoundAt "fx_sensor", Trigger9
    If Tilted Then Exit Sub
    DOF 215, DOFPulse
    If li16.State Then
        AddScore 5000
        AddBonus 1 'advance
        li16.State = 0
    Else
        Addscore 500
    End If
End Sub

' Active double bonus
Sub Trigger10_Hit
    PlaySoundAt "fx_sensor", Trigger10
    If Tilted Then Exit Sub
    DOF 216, DOFPulse
    If li17.State Then
        'AddScore 5000
        'AddBonus 1
		li9.State = 1
		BonusMultiplier = 2
		li10.State = 0
        li17.State = 0
    Else
        Addscore 500
		BonusMultiplier = 1
    End If
End Sub

' Active triple bonus
Sub Trigger11_Hit
    PlaySoundAt "fx_sensor", Trigger11
    If Tilted Then Exit Sub
    DOF 217, DOFPulse
    If li18.State Then
        'AddScore 5000
        'AddBonus 1
		li10.State = 1
		BonusMultiplier = 3
		li9.State = 0
		LightBumper1.State = 0
        li18.State = 0
    Else
        Addscore 500
		BonusMultiplier = 1
    End If
End Sub

' Top right lane
Sub Trigger12_Hit
    PlaySoundAt "fx_sensor", Trigger12
    If Tilted Then Exit Sub
    DOF 218, DOFPulse
    If li19.State Then
        AddScore 5000
        AddBonus 1 'advance
        li19.State = 0
    Else
        Addscore 500
    End If
End Sub

'************************
'       Targets
'************************

' Target at center (not a drop target)
' You can won 5000, 6000 if advance mode activated, or Extra ball
Sub Target1_hit
    If Tilted Then Exit Sub
    PlaySoundAtBall SoundFXDOF ("fx_target", 103, DOFPulse, DOFTargets)
    DOF 207, DOFPulse
    If Epileptikdof = True Then DOF 208, DOFPulse End If
    PlaySound "centraltarget"
	If bAdvance Then
		AddScore 6000
	Else
		AddScore 5000
	End If
	' Extra ball if all drop targets are down
    If li11.State Then
        AwardExtraBall
    End If
End Sub

'*****************
'    kickers (holes)
'*****************

Sub kicker1_hit
    PlaySoundAtBall "fx_kicker_enter"
    If Tilted Then
        kicker1Kick
        Exit Sub
    End If
    AddScore 500
    vpmtimer.AddTimer 1500, "kicker1kick '"
	' li14 : Special 5000
    If li14.State Then AwardSpecial
End Sub

Sub kicker1Kick
    PlaySoundAt SoundFXDOF ("fx_kicker", 111, DOFPulse, DOFContactors), kicker1
    DOF 209, DOFPulse
	If Epileptikdof = True Then DOF 202, DOFPulse End If
    kicker1.kick 250, 7
End Sub

Sub kicker2_hit
    PlaySoundAtBall "fx_kicker_enter"
    If Tilted Then
        kicker2Kick
        Exit Sub
    End If
    AddScore 500
    vpmtimer.AddTimer 1500, "kicker2kick '"
	' li15 : Special 5000
    If li15.State Then AwardSpecial
End Sub

Sub kicker2Kick
    PlaySoundAt SoundFXDOF ("fx_kicker", 112, DOFPulse, DOFContactors), kicker2
    DOF 210, DOFPulse
	If Epileptikdof = True Then DOF 202, DOFPulse End If
    kicker2.kick 200, 7
End Sub

'*******************
'  Drop targets
'*******************

'Left (red) drop targets

Sub Target4_hit
    PlaySoundAt SoundFXDOF ("fx_droptarget", 109, DOFPulse, DOFDropTargets), target4
    If Epileptikdof = True Then DOF 211, DOFPulse End If
    If Tilted Then Exit Sub
    If bAdvance Then
		AddScore 600
	Else
		AddScore 500
	End If
    AddBonus 1
    Target4.UserValue = 1
	CheckBlueTargets
    CheckAllDrop
End Sub

Sub Target5_hit
    PlaySoundAt SoundFXDOF ("fx_droptarget", 109, DOFPulse, DOFDropTargets), target6
    If Epileptikdof = True Then DOF 211, DOFPulse End If
    If Tilted Then Exit Sub
    If bAdvance Then
		AddScore 600
	Else
		AddScore 500
	End If
    AddBonus 1
    Target5.UserValue = 1
	CheckBlueTargets
    CheckAllDrop
End Sub

Sub Target6_hit
    PlaySoundAt SoundFXDOF ("fx_droptarget", 109, DOFPulse, DOFDropTargets), target8
    If Epileptikdof = True Then DOF 211, DOFPulse End If
    If Tilted Then Exit Sub
    If bAdvance Then
		AddScore 600
	Else
		AddScore 500
	End If
    AddBonus 1
    Target6.UserValue = 1
	CheckBlueTargets
    CheckAllDrop
End Sub

Sub Target7_hit
    PlaySoundAt SoundFXDOF ("fx_droptarget", 109, DOFPulse, DOFDropTargets), target10
    If Epileptikdof = True Then DOF 212, DOFPulse End If
    If Tilted Then Exit Sub
    If bAdvance Then
		AddScore 600
	Else
		AddScore 500
	End If
    AddBonus 1
    Target7.UserValue = 1
	CheckBlueTargets
    CheckAllDrop
End Sub

Sub Target8_hit
    PlaySoundAt SoundFXDOF ("fx_droptarget", 109, DOFPulse, DOFDropTargets), target12
    If Epileptikdof = True Then DOF 212, DOFPulse End If
    If Tilted Then Exit Sub
    If bAdvance Then
		AddScore 600
	Else
		AddScore 500
	End If
    AddBonus 1
    Target8.UserValue = 1
	CheckBlueTargets
    CheckAllDrop
End Sub

Sub CheckBlueTargets
    Dim tmp
    tmp = Target4.UserValue + Target5.UserValue + Target6.UserValue + Target7.UserValue + Target8.UserValue
    If tmp = 5 then
		li14.State = 1 'lit special
    End If
End Sub

Sub CheckAllDrop
    Dim tmp
    tmp = 0
    For each x in aDropTargets
        tmp = tmp + x.UserValue
    Next
    If tmp = 10 then
        li14.State = 1 ' Special 5000
        li15.State = 1 ' Special 5000
		li11.State = 1 ' lit center target advance bonus for extra ball
    End If
End Sub

'Right (yellow) drop targets

Sub Target9_hit
    PlaySoundAt SoundFXDOF ("fx_droptarget", 108, DOFPulse, DOFDropTargets), target9
    If Epileptikdof = True Then DOF 212, DOFPulse End If
    If Tilted Then Exit Sub
    If bAdvance Then
		AddScore 600
	Else
		AddScore 500
	End If
    AddBonus 1
    Target9.UserValue = 1
	CheckYellowTargets
    CheckAllDrop
End Sub

Sub Target10_hit
    PlaySoundAt SoundFXDOF ("fx_droptarget", 108, DOFPulse, DOFDropTargets), target5
    If Epileptikdof = True Then DOF 213, DOFPulse End If
    If Tilted Then Exit Sub
    If bAdvance Then
		AddScore 600
	Else
		AddScore 500
	End If
    Target10.UserValue = 1
    CheckYellowTargets
    CheckAllDrop
End Sub

Sub Target11_hit
    PlaySoundAt SoundFXDOF ("fx_droptarget", 108, DOFPulse, DOFDropTargets), target11
    If Epileptikdof = True Then DOF 214, DOFPulse End If
    If Tilted Then Exit Sub
    If bAdvance Then
		AddScore 600
	Else
		AddScore 500
	End If
    Target11.UserValue = 1
    CheckYellowTargets
    CheckAllDrop
End Sub

Sub Target12_hit
    PlaySoundAt SoundFXDOF ("fx_droptarget", 108, DOFPulse, DOFDropTargets), target7
    If Epileptikdof = True Then DOF 213, DOFPulse End If
    If Tilted Then Exit Sub
    If bAdvance Then
		AddScore 600
	Else
		AddScore 500
	End If
    Target12.UserValue = 1
    CheckYellowTargets
    CheckAllDrop
End Sub

Sub Target13_hit
    PlaySoundAt SoundFXDOF ("fx_droptarget", 108, DOFPulse, DOFDropTargets), target13
    If Epileptikdof = True Then DOF 214, DOFPulse End If
    If Tilted Then Exit Sub
    If bAdvance Then
		AddScore 600
	Else
		AddScore 500
	End If
    Target13.UserValue = 1
    CheckYellowTargets
    CheckAllDrop
End Sub

Sub CheckYellowTargets
    Dim tmp
    tmp = Target9.UserValue + Target10.UserValue + Target11.UserValue + Target12.UserValue + Target13.UserValue
    If tmp = 5 then
        li15.State = 1 'lit special award
    End If
End Sub

'*********************
' Gomas de 100 puntos
'*********************

Sub RubberBand3_hit
    If Tilted Then Exit Sub
    AddScore 10
End Sub

Sub RubberBand5_hit
    If Tilted Then Exit Sub
    AddScore 10
End Sub

Sub RubberBand13_hit
    If Tilted Then Exit Sub
    AddScore 10
End Sub

Sub RubberBand2_hit
    If Tilted Then Exit Sub
    AddScore 10
End Sub
'
'******************************
'     DOF lights ball entrance
'******************************
'
Sub Trigger001_Hit
    DOF 228, DOFPulse
End sub