Option Explicit

'*****************************************************************************************************
' CREDITS
' Initial table created by fuzzel, jimmyfingers, jpsalas, toxie & unclewilly (in alphabetical order)
' Flipper primitives by zany
' Ball rolling sound script by jpsalas
' Ball shadow by ninuzzu
' Ball control & ball dropping sound by rothbauerw
' DOF by arngrim
' Positional sound helper functions by djrobx
' Plus a lot of input from the whole community (sorry if we forgot you :/)
'*****************************************************************************************************
'
'
'
'		 ___  _          _        ___  _  _  _       
'		| _ )(_) _ _  __| |      | __|| || || |      
'		| _ \| || '_|/ _` |      | _| | | \_. |      
'		|___/|_||_|  \__/_|      |_|  |_| |__/       
'		
'		
'					_     _  _        _          _             _  _            
'					| |__ | || |      | |    ___ | | ___  __ _ | || | ___       
'					|  _ \ \_. |      | |__ / _ \| |/ _ \/ _` || || |/ _ \      
'					|____/ |__/       |____|\___/|_|\___/\__/_||_||_|\___/      
'		
'
'	
'	                     _            _            _ __       _        __ _          ___     __      ___     ___  
'	 __  _ _  ___  __ _ | |_  ___  __| |       ___| '_ \ _ _ (_) _ _  / _` |        |_  )   /  \    |_  )   |_  ) 
'	/ _|| '_|/ -_)/ _` ||  _|/ -_)/ _` |      (_-/| .__/| '_|| || ' \ \__. |         / /   | () |    / /     / /  
'	\__||_|  \___|\__/_| \__|\___|\__/_|      /__/|_|   |_|  |_||_||_||___/         /___|   \__/    /___|   /___| 
'
'
'
' https://www.vpforums.org/index.php?app=downloads&showfile=16555#
'
'*****************************************************************************************************
'
'  All animals sounds from 
'
'  							https://lasonotheque.org/
' 
' Please visit this site and why not a beer ?
'
'
'*****************************************************************************************************
' V0.8.0 :
' first upload
'
'*****************************************************************************************************
'
' V0.8.1 : 
' - added default background and "insert coin" sequence to avoid DMD display "File not found"
' - added the test of MusicOn value.
'		default is value is  MusicOn=True 
' 		Set the value to False if you experience a bug with music. Music won't be played
' - added "\" to value myMusicFolder = "\Bird_Fly" according a solution in VPF
'		That's working nor better nor worse for my PC. If worse for you, try to remove this"\" line 943
' - added CAT challenge (all column lights lit)
'
'*****************************************************************************************************
'
' V0.9.0 : 
' - added sound on bird advance and replacing
' - added RattAttack nice shot
' - added RattAttack all targets down
' - added teasers after Game Over
' - modified Ball rolling sound to hear ball rolling on ramps and platforms
' - added Flasher
' - point multiplier better managment
' - increased free game score to reach
' - tilt managment
' - Top scores/name recording
' - Bird Fly champion score/name recording
'    Note : this new feature make program crash on prior Bird Fly installations (non existing parameters)
'           Please, launch Bird Fly again and/or delete ../User/Bird_Fly.txt file first
' - added animated eggs and chicks in nest
' - added some lights
' - new B2S with animation
'
'*****************************************************************************************************
' To Do and known bugs :

'	- finir d'ajouter les règles prévues ou les actions imaginées
'	- améliorer les sons et faire du nettoyage
'	- voir comment mettre en oeuvre la cible qui bouge (le problème c'est le centre de la cible qui est fixe)
'	- ajouter des animations sur les oiseaux (bouger les ailes)
'	- ajouter des séquences hivernales (mode)
'	- music problem ?
'	- mettre en oeuvre le DMD : transitions et animations
'	- etc......
'
'*****************************************************************************************************



'First, try to load the Controller.vbs (DOF), which helps controlling additional hardware like lights, gears, knockers, bells and chimes (to increase realism)
'This table uses DOF via the 'SoundFX' calls that are inserted in some of the PlaySound commands, which will then fire an additional event, instead of just playing a sample/sound effect
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the Controller.vbs file in order to run this table (installed with the VPX package in the scripts folder)"
On Error Goto 0

'If using Visual PinMAME (VPM), place the ROM/game name in the constant below,
'both for VPM, and DOF to load the right DOF config from the Configtool, whether it's a VPM or an Original table

Const cGameName = "Bird_Fly"
Dim TwoScreens: TwoScreens=True 'Set to Yes if two screens available -> DMD will be ON

Dim Var_nb_sounds_Bird_x: Var_nb_sounds_Bird_x=7
Dim MusicOn: MusicOn=True 'Set to false if you experience a bug with music
Dim Var_Afficher_message: Var_Afficher_message=False 'Positionne cette variable à True pour afficher le debug

Dim NbCredits: NbCredits=0 'Nombre de crédits disponibles
Dim NbCreditsMax: NbCreditsMax=20 'Nombre de crédits maximum
Dim NbJoueurs: NbJoueurs=0 'Combien de joueurs se disputent la partie
Dim NbJoueursOld: NbJoueursOld=0
Dim NbJoueursParPartie: NbJoueursParPartie=4 'Nombre de joueurs maximum autorisés
Dim JoueurCourant: JoueurCourant=0 ' Quel joueur est actuellement en train de jouer
Dim BalleCourante: BalleCourante=0 ' Qeulle balle de la partie est en jeu
Dim BallesParPartie: BallesParPartie=3 'Nombre de balles par partie (max 9)
Dim ScorePlayer(4) 'Adapter avec le nombre de joueurs maximum autorisés
Dim Multiplier: Multiplier=1
Dim PhaseJeu: PhaseJeu="InsertCoins" 'Où en sommes nous de la phase du jeu ?
Dim BallSaving: BallSaving=True 'Faut-il sauver la balle
Dim BallSavingDuration: BallSavingDuration=32000 'Time in milliseconds for BallSaving
Dim ShowFlipperBird: ShowFlipperBird=True 'If True show Birds and not flippers (main flippers only)
Dim AnimateTrees: AnimateTrees=True 'On slow computers, set to False 
Dim MultiplierTemporaire: MultiplierTemporaire=1 'used for temporary score x2
Dim LoopsPlayer(4) 'Adapter avec le nombre de joueurs maximum autorisés
Dim NbTilt:	NbTilt=0
Dim Tilt: Tilt=False

'Scores pour partie gratuite
Dim ScoreToWin1: ScoreToWin1=2500000
Dim ScoreToWin2: ScoreToWin2=5000000
Dim ScoreToWin3: ScoreToWin3=10000000
Dim AwardGivenPlayer(4)

Dim ValeurBumper: ValeurBumper=1010
Dim ValeurTriggerBird: ValeurTriggerBird=3000
Dim ValeurTriggerBirdBonus: ValeurTriggerBirdBonus=35000
Dim ValeurTriggerROBIN: ValeurTriggerROBIN=3000
Dim ValeurTriggerROBINBonus: ValeurTriggerROBINBonus=35000
Dim ValeurTarget_Cat_000: ValeurTarget_Cat_000=5000
Dim ValeurTarget_Cat_001: ValeurTarget_Cat_001=5000
Dim ValeurTarget_Cat_002: ValeurTarget_Cat_002=5000
Dim ValeurTargetNest: ValeurTargetNest=10000
Dim ValeurNest: ValeurNest=40000
Dim ValeurJackpot: ValeurJackpot=100000
Dim ValeurTargetRat: ValeurTargetRat=15000
Dim ValeurTargetRatAttack: ValeurTargetRatAttack=7000
Dim ValeurRamp_A: ValeurRamp_A=30000
Dim ValeurRamp_B: ValeurRamp_B=35000
Dim ValeurTargetTree: ValeurTargetTree=5000
Dim ValeurTargetTreeBonus: ValeurTargetTreeBonus=5000
Dim ValeurGateBirdMove: ValeurGateBirdMove=1000
Dim ValeurGateBirdMoveBonus: ValeurGateBirdMoveBonus=100000
Dim ValeurExtraBall: ValeurExtraBall=20000
Dim NombreLignesCat: NombreLignesCat=4 'Nombre de lignes CAT à allumer pour avoir l'Extra Ball 2=facile, 4=default
Dim ExtraBallWon: ExtraBallWon=False
Dim ValeurAllMultiplier: ValeurAllMultiplier=50000

Dim KickerNestTimer: KickerNestTimer= 2000
Dim KickerRampTimer: KickerRampTimer=5000

Dim CorrectionVolume: CorrectionVolume=5 'Correction du volume (ajout) pour la balle qui roule

Dim EnableRetractPlunger
EnableRetractPlunger = False 'Change to true to enable retracting the plunger at a linear speed; wait for 1 second at the maximum position; move back towards the resting position; nice for button/key plungers


If Table1.ShowDT = False OR TwoScreens=True Then
    Scoretext.Visible = False
	LoadEM ' needed for backglass to run
	LoadUltraDMD 'Créer le DMD
	TimerB2S.Enabled=True
Else
	TimerB2S.Enabled=False	
End If


Dim EnableBallControl
EnableBallControl = True 'Change to true to enable manual ball control (or press C in-game) via the arrow keys and B (boost movement) keys

Const BallSize = 25  'Ball radius

Sub Table1_KeyDown(ByVal keycode)
	If keycode = PlungerKey Then
        If EnableRetractPlunger Then
            Plunger.PullBackandRetract
        Else
		    Plunger.PullBack
        End If
		PlaySound "plungerpull",0,1,AudioPan(Plunger),0.25,0,0,1,AudioFade(Plunger)
	End If

	If keycode = LeftTiltKey Then
		Nudge 90, 2
		GestionTilt
	End If

	If keycode = RightTiltKey Then
		Nudge 270, 2
		GestionTilt
	End If

	If keycode = CenterTiltKey Then
		Nudge 0, 2
		GestionTilt
	End If

	'Phase d'insertion de pièce et sélection du player uniquement
	'la variable est positionnée via la routine Gate007_Hit et la fin de la partie des joueurs
	If PhaseJeu <> "PartieEnCours" Then
		'test du nombre de crédits suffisant
		If Keycode=StartGameKey And NbCredits = 0 And InitialsChosen > 3 Then
			DMDScene NomScene, "Insert coins", IntensityFull, "No credit left", Intensity075, NoAnim, DureeScene, NoAnim, PriorityScene
			Playsound "Insert coin"
		End If
		'test de l'ajout de pieces
		If keycode=AddCreditKey And InitialsChosen > 3 And NbCredits < NbCreditsMax Then 'Ajout de pièces
			NbCredits=NbCredits + 1
			DMDScene DMD_Insertcoin, "Credit added ", IntensityFull, NbCredits & " available", Intensity075, NoAnim, DureeScene, NoAnim, PriorityScene
			PlaySound "CoinIn5"
		End If
		'test de l'ajout de Player
		If Keycode=StartGameKey And NbCredits > 0 And InitialsChosen > 3 Then 'Ajout de joueurs
			If NbJoueurs < NbJoueursParPartie Then
				'Passe en mode Teaser Off
				Teaser_Initialisation False, 1000, 3000
				PlaySound "Ball_lost",0,1,AudioPan(Drain),0.25,0,0,1,AudioFade(Drain)
				NbCredits=NbCredits-1
				NbJoueurs = NbJoueurs + 1
				JoueurCourant=1
				BalleCourante=1
				ScorePlayer(NbJoueurs)=0
				AwardGivenPlayer(NbJoueurs)=0
				DMDScene NomScene, "Welcome player " & NbJoueurs, IntensityFull, "Credits " & NbCredits, IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
				BalleCourante=1 'Initialisation du compteur de balles
				TimerCreateBall.Enabled=True 'On va créer la première balle
				Tilt=False 'On invalide le potentiel Tilt
				'Initialisations non génériques
				LoopsPlayer_Initialisation
			Else
				DMDScene NomScene, NbJoueursParPartie & " players only", IntensityFull, "", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
			End If
		End If
		Debug_text("BalleCourante/BallesParPartie " & BalleCourante & "/" & BallesParPartie & " JoueurCourant/NbJoueurs " & JoueurCourant & "/" & NbJoueurs )
	End If

	'Toutes les phases dès que au moins un player est sélectionné
	If NbJoueurs > 0 Then
		If keycode = LeftFlipperKey And Tilt=False Then
			LeftFlipper.TimerEnabled = True 'This line is only for ninuzzu's flipper shadows!
			LeftFlipper.RotateToEnd
			PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, .67, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
		
			'Ajouts pour cette Table ___________________________________________________________________________________
			FlipperHG.RotateToEnd
			TimerOiseauHGPlateauA.Enabled=True
			LightBird_Rotate("SensLeft")
		End If
		
		If keycode = RightFlipperKey And Tilt=False Then
			RightFlipper.TimerEnabled = True 'This line is only for ninuzzu's flipper shadows!
			RightFlipper.RotateToEnd
			PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, .67, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
		
			'Ajouts pour cette Table ___________________________________________________________________________________
			FlipperHD.RotateToEnd
			TimerOiseauHDPlateauA.Enabled=True
			LightBird_Rotate("SensRight")
			RightFlipper001.TimerEnabled = True 'This line is only for ninuzzu's flipper shadows!
			RightFlipper001.RotateToEnd
			PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, .67, AudioPan(RightFlipper001), 0.05,0,0,1,AudioFade(RightFlipper001)
		End If
		
		' Manual Ball Control
		If keycode = 46 Then	 				' C Key
			If EnableBallControl = 1 Then
				EnableBallControl = 0
			Else
				EnableBallControl = 1
			End If
		End If
		
		If EnableBallControl = 1 Then
			If keycode = 48 Then 				' B Key
				If BCboost = 1 Then
					BCboost = BCboostmulti
				Else
					BCboost = 1
				End If
			End If
			If keycode = 203 Then BCleft = 1	' Left Arrow
			If keycode = 200 Then BCup = 1		' Up Arrow
			If keycode = 208 Then BCdown = 1	' Down Arrow
			If keycode = 205 Then BCright = 1	' Right Arrow
		End If
	End If		
		'Ajouts pour cette Table ___________________________________________________________________________________
		'gestion de la sasie des High scores
		If keycode=2 And InitialsChosen<3 Then 
			SelectLetter
		End if						' "1" (start key) to enter initials	
		If keycode=LeftFlipperKey And InitialsChosen<3 Then
			ScrollLetterLeft
		End If		'Left flipper scrolls letter down the character set
		If keycode=RightFlipperKey And InitialsChosen<3 Then
			ScrollLetterRight
		End if		'Right flipper scrolls letter down the character set


End Sub

Sub Table1_KeyUp(ByVal keycode)
	If keycode = PlungerKey Then
		Plunger.Fire
		PlaySound "plunger",0,1,AudioPan(Plunger),0.25,0,0,1,AudioFade(Plunger)
	End If

	If keycode = LeftFlipperKey Then
		LeftFlipper.RotateToStart
		PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)

		'Ajouts pour cette Table ___________________________________________________________________________________
		FlipperHG.RotateToStart
	End If

	If keycode = RightFlipperKey Then
		RightFlipper.RotateToStart
		PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)

		'Ajouts pour cette Table ___________________________________________________________________________________
		FlipperHD.RotateToStart
		RightFlipper001.RotateToStart
		PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, AudioPan(RightFlipper001), 0.05,0,0,1,AudioFade(RightFlipper001)
	End If

    'Manual Ball Control
	If EnableBallControl = 1 Then
		If keycode = 203 Then BCleft = 0	' Left Arrow
		If keycode = 200 Then BCup = 0		' Up Arrow
		If keycode = 208 Then BCdown = 0	' Down Arrow
		If keycode = 205 Then BCright = 0	' Right Arrow
	End If
End Sub


Sub Drain_Hit()
	If BallSaving=True Then
		'On est dans la période BallSaving on continue
		Drain.DestroyBall
		BallSavingCreateBall
		DMDScene DMDEngage, "Ball", IntensityFull, "save", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
	Else
		'On n'est plus dans la période BallSaving
		PlaySound "drain",0,1,AudioPan(Drain),0.25,0,0,1,AudioFade(Drain)
		Drain.DestroyBall
		BIP = BIP - 1
		TimerCreateBall.Enabled=True
		DMDScene NomScene, "Ball " & BalleCourante & " lost", IntensityFull, ScorePlayer(JoueurCourant), IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		PlaySound "Ball_lost",0,1,AudioPan(Drain),0.25,0,0,1,AudioFade(Drain)
	End If
End Sub

Sub BallSavingCreateBall()
	If Tilt=True Then
		BallSaving=False
	End If 'Gestion du Tilt
	'On recrée la balle perdue pendant la période BallSaving
	BallRelease.CreateBall
	BallRelease.Kick 90, 7
	PlaySound SoundFX("ballrelease",DOFContactors), 0,1,AudioPan(BallRelease),0.25,0,0,1,AudioFade(BallRelease)
	'valide le kicker pour lancer de bille automatique
	KickerAuto.Enabled=True
	KickerAuto.TimerEnabled=True
End Sub

Sub KickerAuto_Timer()
	'Envoie la balle
	KickerAuto.TimerEnabled=False
	KickerAuto.Kick 0,42
	PlaySound SoundFX("ballrelease",DOFContactors), 0,1,AudioPan(BallRelease),0.25,0,0,1,AudioFade(BallRelease)
	KickerAuto.Enabled=False
	'Fait bouger l'oiseau et démarre son replacement
	TimerOiseauKicker.Interval=1000
	TimerOiseauKicker.Enabled=True
	OiseauKicker.RotX=55
End Sub

Sub TimerOiseauKicker_Timer()
	TimerOiseauKicker.Interval=75
	OiseauKicker.RotX=OiseauKicker.RotX-5
	If OiseauKicker.RotX <0 Then
		OiseauKicker.RotX=0
		TimerOiseauKicker.Interval=800
		TimerOiseauKicker.Enabled=False
	End If
End Sub	

Dim PhaseInit: PhaseInit="Initialisations"
Sub TimerCreateBall_Timer()
	If BIP = 0 then
		Tilt=False 'On invalide le Tilt
		Select Case PhaseInit
		Case "Initialisations" 'On lance les phases d'initialisation
			'Abaisser les cibles TargetRatAttack en début de bille
			TargetRatAttack_Initialisation(True)
			'On montre toutes les cibles TargetRat en début de bille
			TargetRat_Initialisation
			'Initialisation des targets Nest
			TargetNest_Initialisation
			'Initialisation des TargetsTree
			LightTargetTree_Initialisation
			'Initialisation des Triggers ROBIN
			TriggerROBIN_Initialisation
			'Initialisation des Triggers Bird
			TriggerBird_Initialisation("Tout")
			'Initialiser les lampes de Cat
			LightCat_Initialisation
			'Replacer l'oiseau pour toute nouvelle bille
			Var_Oiseau_Replace=True
			TimerOiseau.Enabled=True
			'Initialiser les compteurs des rampes pour toute nouvelle bille
			CompteursRampes_Initialisation
			'Initialiser le bonus X2 temporaire
			KickerRamp_Initialisation
			'Initialisation du BallSaving
			LightCentral_Initialisation(False)
			'Initialisation des oiseaux centraux
			LightBirdCircle_Initialisation
			'Initialisation des Bumpers
			Bumper_Initialisation
			TimerCreateBall.Enabled=True
			PhaseInit="Attente"

		Case "Attente" 'On boucle tant que l'oiseau n'est pas repositionné
			TimerCreateBall.Enabled=True
			If Var_Oiseau_TransX>-1 Then PhaseInit="CreateBall" End If 'On boucle sur la même phase tant que l'oiseau n'est pas repositionné sinon phase suivante

		Case "CreateBall"
			'teste si on a joué la dernière balle du dernier joueur et que l'Extra Ball ne soit pas allumée
			If BalleCourante = BallesParPartie And JoueurCourant = NbJoueurs  And ExtraBallWon=False Then
				PhaseJeu = "PartieTerminée"
				PhaseInit="Loterie"
				TimerCreateBall.Enabled=True
			Else
				If ExtraBallWon=False Then
					'On change de joueur et/ou de numéro de bille
					If JoueurCourant = NbJoueurs Then 'Le dernier joueur vient de perdre sa bille
						JoueurCourant = 1 'C'est au premier joueur de jouer
						If PhaseJeu = "PartieEnCours" Then BalleCourante = BalleCourante + 1 End If 'La bille courante est la suivante
					Else
						If PhaseJeu = "PartieEnCours" Then JoueurCourant = JoueurCourant + 1 End If 'Le joueur courant est le suivant, la bille courante ne change pas
					End If
					DMDScene NomScene, "Player "& JoueurCourant, IntensityFull, "ball " & BalleCourante, IntensityFull, AnimToLeft, DureeScene, NoAnim, PriorityScene
					DMDScore(-10) '-10 car l'appel de la fonction ajoute 10 pour la loterie
				Else
					DMDScene NomScene, "Player "& JoueurCourant & " shoots again", IntensityFull,"Ball " & BalleCourante, IntensityFull, AnimToLeft, DureeScene, NoAnim, PriorityScene
				End If
				'Plunger.CreateBall
				BallRelease.CreateBall
				BallRelease.Kick 90, 7
				PlaySound SoundFX("ballrelease",DOFContactors), 0,1,AudioPan(BallRelease),0.25,0,0,1,AudioFade(BallRelease)
				BIP = BIP + 1

			ExtraBallWon=False 'Réinitialisation de la variable Extra Balle
			TimerCreateBall.Enabled=False
			PhaseInit="Initialisations" 'On initialise la variable pour la prochaine perte de balle
			End If

		Case "Loterie"
			'La partie est finie on fait la loterie
			PhaseJeu = "PartieTerminée"
			NbJoueursOld=NbJoueurs
			NbJoueurs=0 'Initialise pour la partie suivante
			JoueurCourant=0 'Initialise pour la partie suivante
			BalleCourante=0 'Initialise pour la partie suivante
			TimerCreateBall.Enabled=False
			PhaseInit="Initialisations" 'On initialise la variable pour la prochaine perte de balle
			TimerPhaseLoterie.Interval=1000
			TimerPhaseLoterie.Enabled=True 'On débute la phase loterie
			PhaseLoterie=1000

	    End Select
	End If
	Debug_text("PhaseInit/PhaseJeu :" & PhaseInit & "/" & PhaseJeu & " BalleCourante/BallesParPartie " & BalleCourante & "/" & BallesParPartie & " JoueurCourant/NbJoueurs " & JoueurCourant & "/" & NbJoueurs )

End Sub


Dim BIP
BIP = 0

'*****GI Lights On
dim xx

For each xx in GI:xx.State = 1: Next

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	If Tilt=True Then Exit Sub End If 'Gestion du Tilt
    PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	gi1.State = 0:Gi2.State = 0
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0:gi1.State = 1:Gi2.State = 1
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	If Tilt=True Then Exit Sub End If 'Gestion du Tilt
    PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	gi3.State = 0:Gi4.State = 0
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0:gi3.State = 1:Gi4.State = 1
    End Select
    LStep = LStep + 1
End Sub


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

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub


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
    Vol = Csng(BallVel(ball) ^2 / 2000) + CorrectionVolume
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function


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


'********************************************************************
'      JP's VP10 Rolling Sounds (+rothbauerw's Dropping Sounds)
'********************************************************************

Const tnob = 5 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

'Add a second sound for ball rolling on ramps and floors (z>29)
Sub RollingTimer_Timer()
    Dim BOT, b
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
		StopSound("BallRoll" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

    For b = 0 to UBound(BOT)
        ' play the rolling sound for each ball
        If BallVel(BOT(b) ) > 1 Then
            rolling(b) = True
			If BOT(b).z < 30 Then
				StopSound("BallRoll" & b)
				PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
			Else
				StopSound("fx_ballrolling" & b)
				PlaySound("BallRoll" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
			End If
		Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
				StopSound("BallRoll" & b)
                rolling(b) = False
            End If
        End If

        ' play ball drop sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_ball_drop" & b, 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'*****************************************
'	ninuzzu's	FLIPPER SHADOWS v2
'*****************************************

'Add TimerEnabled=True to Table1_KeyDown procedure
' Example :
'Sub Table1_KeyDown(ByVal keycode)
'    If keycode = LeftFlipperKey Then
'        LeftFlipper.TimerEnabled = True 'Add this
'        LeftFlipper.RotateToEnd
'    End If
'    If keycode = RightFlipperKey Then
'        RightFlipper.TimerEnabled = True 'And add this
'        RightFlipper.RotateToEnd
'    End If
'End Sub

Sub LeftFlipper_Init()
    LeftFlipper.TimerInterval = 10
End Sub

Sub RightFlipper_Init()
    RightFlipper.TimerInterval = 10
End Sub

Sub LeftFlipper_Timer()
    FlipperLSh.RotZ = LeftFlipper.CurrentAngle
    If LeftFlipper.CurrentAngle = LeftFlipper.StartAngle Then
        LeftFlipper.TimerEnabled = False
    End If
End Sub

Sub RightFlipper_Timer()
    FlipperRSh.RotZ = RightFlipper.CurrentAngle
    If RightFlipper.CurrentAngle = RightFlipper.StartAngle Then
        RightFlipper.TimerEnabled = False
    End If
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


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
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
	PlaySound "Bird_" & Int(Rnd*Var_nb_sounds_Bird_x)+1, 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gatesenvol_Hit (idx)
	PlaySound "Birdy_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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

Sub RightFlipper001_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub FlipperHG_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub FlipperHD_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub


'____________________________________________________________________________________________________________________

'
' Initialisations et sortie de table
'
'____________________________________________________________________________________________________________________

Sub Table1_Init()
	KickerRamp.CreateBall 'Création de la balle prisonnière
	KickerRamp.Kick -60,5
	If MusicOn=True Then
		JouerMusique
	End If
	DMDScene NomScene, "", Intensity075, "Add player", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
	'Passe en mode Teaser On
	Teaser_Initialisation True, 994, 3000
	'Abaisser les cibles TargetRatAttack en début de partie
	TargetRatAttack_Initialisation(True)
	'On montre toutes les cibles TargetRat en début de partie
	TargetRat_Initialisation
	'Initialisation des targets Nest
	TargetNest_Initialisation
	'Initialisation des TargetsTree
	LightTargetTree_Initialisation
	'Initialisation des Triggers ROBIN
	TriggerROBIN_Initialisation
	'Initialisation des Triggers Bird
	TriggerBird_Initialisation("Tout")
	'Initialiser les lampes de Cat
	LightCat_Initialisation
	'Replacer l'oiseau
	Var_Oiseau_Replace=True
	TimerOiseau.Enabled=True
	'Initialiser les compteurs des rampes
	CompteursRampes_Initialisation
	'Initialiser le bonus X2 temporaire
	KickerRamp_Initialisation
	'Charger les records de la Table1	
	HighScoreLoad
	'Initialisation du BallSaving
	LightCentral_Initialisation(False)
	'Initialisation des oiseaux centraux
	LightBirdCircle_Initialisation
	'Initialisation des Bumpers
	Bumper_Initialisation
	If ShowFlipperBird=False Then
		'On masque les oiseaux et montre les flippers
		TimerOiseauBG.Enabled=False
		TimerOiseauBD.Enabled=False
		TimerOiseauMD.Enabled=False
		OiseauBG.Visible=False
		OiseauBD.Visible=False
		OiseauMD.Visible=False
		OiseauHGPlateauA.Visible=False
		OiseauHDPlateauA.Visible=False
		LeftFlipper.Visible=True
		RightFlipper.Visible=True
		RightFlipper001.Visible=True
		FlipperHG.Visible=True
		FlipperHD.Visible=True
	End If
End Sub

Sub Table1_exit()
	HighScoreSave 'On sauvegarde les High scores
End Sub

'____________________________________________________________________________________________________________________

'
' gestion des musiques
'
'____________________________________________________________________________________________________________________


' Code by Jogrady7 (Improved with error handling and optimizations)
' Link: https://www.vpforums.org/index.php?showtopic=13501

Sub JouerMusique()
    Dim fso, folder, files, r, musicPath, myMusicFolder

    On Error Resume Next ' Prevent script from crashing if issues occur

    myMusicFolder = "\Bird_Fly" ' The directory where your mp3 files are stored
    Set fso = CreateObject("Scripting.FileSystemObject")

    ' Get the absolute path to the Visual Pinball\Music folder
    musicPath = fso.GetAbsolutePathName(".")
    musicPath = Left(musicPath, Len(musicPath) - 6) + "Music\"

    Set folder = fso.GetFolder(musicPath & myMusicFolder)
    
    If folder Is Nothing Then
        Debug_text "Folder not found: " & myMusicFolder
        Exit Sub
    End If

    Set files = folder.Files

    If files.Count = 0 Then
        Debug_text "No music files found in: " & myMusicFolder
        Exit Sub
    End If

    ' Select a random MP3 file
    r = Int(files.Count * Rnd + 1)
    Dim ct: ct = 1

    For Each file In files
        If ct = r Then
            If LCase(Right(file.Name, 4)) = ".mp3" Then
                PlayMusic myMusicFolder & "\" & file.Name ' Play the selected MP3
                Exit For
            End If
        End If
        ct = ct + 1
    Next

    On Error GoTo 0 ' Re-enable normal error handling
End Sub

Sub Table1_MusicDone()
    If MusicOn = True Then
        JouerMusique ' Play the next song when the current one finishes
    End If
End Sub

Function Debug_text(Message)
    Debug001.Text = "" ' Clear previous messages
    If Var_Afficher_message = True Then
        Debug001.Text = Message ' Display the debug message
    End If
End Function

'____________________________________________________________________________________________________________________

'
' gestion des Bumpers
'
'____________________________________________________________________________________________________________________

Dim BumperNbHits
Dim BumperFloor(4)
	BumperFloor(1)=20
	BumperFloor(2)=40
	BumperFloor(3)=80
	BumperFloor(3)=250
Dim BumperPrize(4)
	BumperPrize(1)=5000
	BumperPrize(2)=15000
	BumperPrize(3)=30000
	BumperPrize(3)=100000
Dim BumperAwardGiven(4)
Sub Bumper_Initialisation()
	'Initialisation des variables relatives aux Bonus
	BumperNbHits=0
	BumperAwardGiven(1)=False
	BumperAwardGiven(2)=False
	BumperAwardGiven(3)=False
	BumperAwardGiven(4)=False
End Sub

Sub Bumper_check()
	Dim Niveau
	BumperNbHits=BumperNbHits+1
	For Niveau = 1 to 4
		If BumperNbHits = BumperFloor(Niveau) And BumperAwardGiven(Niveau)=False Then
			'Attribution du bonus lorsque on atteint le niveau si il n'a pas été donné
			DMDScore(BumperPrize(Niveau))
			BumperAwardGiven(Niveau)=True
			DMDScene NomScene, "", IntensityFull, "Owl bonus " & BumperPrize(Niveau), IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
			Playsound "Chouette"
			Exit Sub
		End If
	Next
End Sub

Sub Bumper000_Hit()
	If Tilt=True Then Exit Sub End If 'Gestion du Tilt
	BumperLight000.State=2
	BumperLight000A.State=2
	BumperLight000B.State=2
	BumperLight000.TimerEnabled=True
	PlaySound "fx_bumper0"
	DMDScore(ValeurBumper)
	Bumper_check
End Sub

Sub BumperLight000_Timer()
	BumperLight000.TimerEnabled=False
	BumperLight000.State=0
	BumperLight000A.State=0
	BumperLight000B.State=0
End Sub


Sub Bumper001_Hit()
	If Tilt=True Then Exit Sub End If 'Gestion du Tilt
	BumperLight001.State=2
	BumperLight001A.State=2
	BumperLight001B.State=2
	BumperLight001.TimerEnabled=True
	PlaySound "fx_bumper0"
	DMDScore(ValeurBumper)
	Bumper_check
End Sub

Sub BumperLight001_Timer()
	BumperLight001.TimerEnabled=False
	BumperLight001.State=0
	BumperLight001A.State=0
	BumperLight001B.State=0
End Sub

Sub Bumper002_Hit()
	If Tilt=True Then Exit Sub End If 'Gestion du Tilt
	BumperLight002.State=2
	BumperLight002A.State=2
	BumperLight002B.State=2
	BumperLight002.TimerEnabled=True
	PlaySound "fx_bumper0"
	DMDScore(ValeurBumper)
	Bumper_check
End Sub

Sub BumperLight002_Timer()
	BumperLight002.TimerEnabled=False
	BumperLight002.State=0
	BumperLight002A.State=0
	BumperLight002B.State=0
End Sub


'____________________________________________________________________________________________________________________

'
' gestion des LightBirdxxxx
'
'____________________________________________________________________________________________________________________

Function LightBirdCircle_Initialisation()
	Dim Objet
	For Each Objet in LightBirdCircle
		Objet.State=0
	Next
End Function

Sub KickerNest_Hit()
	PlaySound "Birdy_" & Int(Rnd*Var_nb_sounds_Birdy_x)+1, 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	KickerNest.TimerEnabled=True
	KickerNest.TimerInterval=KickerNestTimer
	KickerNestPhase=0
	DMDScore(ValeurNest)
End Sub

Dim KickerNestPhase: KickerNestPhase=0
Sub KickerNest_Timer()
	Dim NbRandom: NbRandom=0
	KickerNestPhase=KickerNestPhase+1
	KickerNest.TimerInterval=KickerNest.TimerInterval-200
	NbRandom=Int(Rnd*8)+1
	If KickerNestPhase < 10 Then
		LightSeqLightBirdCircle.TimerEnabled=True

		If Int(KickerNestPhase mod 2) = 0 Then 
			DMDScene NomScene, "Searching....", IntensityFull, "", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Else
			DMDScene NomScene, "Searching..", IntensityFull, "", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		End If
		'Création d'une bille supplémentaire
		If BIP < 2 Then
			DMDScene NomScene, "New ball", IntensityFull, "launch it !", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
			BallSavingCreateBall
			BIP = BIP + 1
		End If
	Else
		Select Case NbRandom
		Case 1:
			DMDScene NomScene, "Bird 1 song", IntensityFull, "", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
			LightBird001.State=2
		Case 2:
			DMDScene NomScene, "Bird 2 song", IntensityFull, "", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
			LightBird002.State=2
		Case 3:
			DMDScene NomScene, "Bird 3 song", IntensityFull, "", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
			LightBird003.State=2
		Case 4:
			DMDScene NomScene, "Bird 4 song", IntensityFull, "", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
			LightBird004.State=2
		Case 5:
			DMDScene NomScene, "Bird 5 song", IntensityFull, "", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
			LightBird005.State=2
		Case 6:
			DMDScene NomScene, "Bird 6 song", IntensityFull, "", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
			LightBird006.State=2
		Case 7:
			DMDScene NomScene, "Bird 7 song", IntensityFull, "", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
			LightBird007.State=2
		Case 8:
			DMDScene NomScene, "Bird 8 song", IntensityFull, "", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
			LightBird008.State=2
		Case 9:
			DMDScene NomScene, "Multiball", IntensityFull, "", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 10:
			DMDScore(ValeurJackpot)
			DMDScene NomScene, "Jackpot", IntensityFull, ValeurJackpot, IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		End Select
		PlaySound "popper_ball"
		KickerNest.TimerEnabled=False
		KickerNest.kick -0,65
		LightSeqLightBirdCircle.TimerEnabled=False
	End If
End Sub

Sub LightSeqLightBirdCircle_Timer()
	LightSeqLightBirdCircle.UpdateInterval = 10
	LightSeqLightBirdCircle.Play SeqScrewRightOn,10,1,0 ' total ms: 1000
	LightSeqLightCentral.UpdateInterval = 10
	LightSeqLightCentral.Play SeqScrewLeftOn,10,1,0 ' total ms: 1000
End Sub


'____________________________________________________________________________________________________________________

'
' gestion des Targets Nest
'
'____________________________________________________________________________________________________________________


Dim Var_nb_sounds_Birdy_x: Var_nb_sounds_Birdy_x=7
Function TargetNest_Initialisation()
	Dim Objet
	For Each Objet in TargetNest
		Objet.IsDropped=False
		Objet.UserValue=0
	Next
	For Each Objet in LightNest
		Objet.State=0
	Next
End Function

Sub TargetNest_Hit (idx)
	Debug_text("TargetNest : " & idx)
	DMDScore(ValeurTargetNest)
	PlaySound "TargetHit",1, Vol(ActiveBall), AudioPan(ActiveBall), 1, Pitch(ActiveBall), 1, 1, AudioFade(ActiveBall)
	'Ajouts pour cette Table ___________________________________________________________________________________
	TargetNest_Verification(idx) 'Vérifier si les cibles sont toutes dropped
End Sub

Sub TargetNest_Verification(idx)
	Dim Objet
	TargetNest(idx).UserValue=1
	LightNest(idx).State=1
	'On sort de la fonction à la première cible non baissée
	For Each Objet in TargetNest
		If Objet.UserValue=0 Then Exit Sub End If
	Next
	TargetNestN.TimerInterval=2000
	TargetNestN.TimerEnabled=True
End Sub

Sub TargetNestN_Timer()
	TargetNest_Initialisation
	TargetNestN.TimerEnabled=False
End Sub


'____________________________________________________________________________________________________________________

'
' gestion des Targets TargetTree
'
'____________________________________________________________________________________________________________________


Function LightTargetTree_Initialisation()
	Dim Objet
	For Each Objet in TargetTree
		Objet.UserValue=0
	Next
	For Each Objet in LightTargetTree
		Objet.State=0
	Next
End Function

Sub TargetTree_Hit(idx)
	DMDScore(ValeurTargetTree)
	TargetTree(idx).UserValue=1
	LightTargetTree(idx).State=1
	Debug_text("TargetTree : " & idx)
	PlaySound "TargetTree002", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	'Ajouts pour cette Table ___________________________________________________________________________________
	TargetTree_Check(idx) 'Vérifier si la cible est bonus
End Sub

Sub TargetTree_Check(idx)
	Dim Objet
	'On sort de la fonction à la première cible non baissée
	For Each Objet in TargetTree
		If Objet.UserValue=0 Then Exit Sub End If
	Next
	DMDScore(ValeurTargetTreeBonus)
	LightTargetTree_Initialisation
End Sub


'____________________________________________________________________________________________________________________

'
' gestion de LightCentral et de la fonction Ball Save
'
'____________________________________________________________________________________________________________________


Function LightCentral_Initialisation(Etat)
	Dim Objet
	For Each Objet in LightCentral
		Objet.State=1
	Next
	LightCentral001.TimerEnabled=Etat
	LightCentral001.TimerInterval=BallSavingDuration/ 16  '16 lights for countdown
	BallSaving=True
End Function

Sub LightCentral001_Timer()
	Dim Objet
	If Tilt=True Then
		LightCentral001.TimerEnabled=False
		BallSaving=False
		Exit Sub
	End If
	Debug_text("Ball saving : " & BallSaving & " BIP : " & BIP)
	For Each Objet in LightCentral
		'On éteint la première lumière allumée
		If Objet.State=1 Then 
			Objet.State=0
			Exit Sub
		End If
	Next
	'Toutes les lumières sont éteintes
	LightCentral001.TimerEnabled=False
	BallSaving=False
		DMDScene NomScene, "Ball saving", IntensityFull, "time over", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
End Sub


'____________________________________________________________________________________________________________________

'
' gestion des Triggers Bird et des mécanismes de multiplication du score
'
'____________________________________________________________________________________________________________________


Function TriggerBird_Initialisation(Quoi)
	Dim Objet
	For Each Objet in LightBird
		Objet.State=0
	Next
	If Quoi="Tout" Then
		Multiplier=1
		For Each Objet in LightX
			Objet.State=0
		Next
	End If
End Function

Sub TriggerBird_Hit (idx)
	LightBird(idx).State=1
	DMDScore(ValeurTriggerBird)
	Debug_text("TriggerBird : " & idx)
	'PlaySound TriggerBird(idx), 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	Check_TriggerBird(idx) 'Vérifier si les cibles sont toutes dropped pour augmenter le coeff multiplicateur
End Sub

Sub Check_TriggerBird(idx)
	Dim Objet
	'On sort de la fonction à la première lumière non allumée
	For Each Objet in LightBird
		If Objet.State<>1 Then Exit Sub End If
	Next
	DMDScore(ValeurTriggerBirdBonus)
	TriggerBird_Initialisation("Partiel")
	For Each Objet in LightX
		Objet.State=0
	Next
	Multiplier=Multiplier+1
	Select Case Multiplier
		Case 2:
			LightX2.State=1
			DMDScene NomScene, "Multiplier", Intensity075, "X" & Multiplier, IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 3:
			LightX3.State=1
			DMDScene NomScene, "Multiplier", Intensity075, "X" & Multiplier, IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 4:
			LightX4.State=1
			DMDScene NomScene, "Multiplier", Intensity075, "X" & Multiplier, IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 5:
			LightX5.State=1
			DMDScene NomScene, "Multiplier", Intensity075, "X" & Multiplier, IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 6:
			LightX5.State=1
			Multiplier=5
			DMDScore(ValeurAllMultiplier)
			DMDScene NomScene, "Multiplier Bonus", IntensityFull, ValeurAllMultiplier, Intensity075, NoAnim, DureeScene, NoAnim, PriorityScene
	End Select
End Sub

Function LightBird_Rotate(Sens)
Dim LightManoeuvreState: LightManoeuvreState=0
	If Sens="SensLeft" Then 'Rotation des cibles
		LightManoeuvreState=LightBirdB.State
		LightBirdB.State=LightBirdI.State
		LightBirdI.State=LightBirdR.State
		LightBirdR.State=LightBirdD.State
		LightBirdD.State=LightManoeuvreState 
	Else
		LightManoeuvreState=LightBirdD.State
		LightBirdD.State=LightBirdR.State
		LightBirdR.State=LightBirdI.State
		LightBirdI.State=LightBirdB.State
		LightBirdB.State=LightManoeuvreState
	End If
End Function

'____________________________________________________________________________________________________________________

'
' gestion des Triggers ROBIN
'
'____________________________________________________________________________________________________________________


Function TriggerROBIN_Initialisation()
	Dim Objet
	For Each Objet in TriggerROBIN
		Objet.UserValue=0
	Next
	For Each Objet in LightROBIN
		Objet.State=0
	Next
End Function

Sub TriggerROBIN_Hit (idx)
	TriggerROBIN(idx).UserValue=1
	LightROBIN(idx).State=1
	DMDScore(ValeurTriggerROBIN)
	Debug_text("TriggerROBIN : " & idx)
	'PlaySound TriggerROBIN(idx), 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	Check_TriggerROBIN(idx) 'Vérifier si les cibles sont toutes dropped
End Sub

Sub Check_TriggerROBIN(idx)
	Dim Objet
	'On sort de la fonction à la première cible non baissée
	For Each Objet in TriggerROBIN
		If Objet.UserValue=0 Then Exit Sub End If
	Next
	DMDScore(ValeurTriggerROBINBonus)
	TriggerROBIN_Initialisation
End Sub


'____________________________________________________________________________________________________________________

'
' gestion de l'oiseau du bas et des targets associées
'
'____________________________________________________________________________________________________________________

Dim Var_Oiseau_TransX: Var_Oiseau_TransX=0
Dim Var_Oiseau_Nb_Deplacement: Var_Oiseau_Nb_Deplacement=0
Dim Var_Oiseau_Val_Deplacement: Var_Oiseau_Val_Deplacement=7
Dim Var_Oiseau_Replace: Var_Oiseau_Replace=False
Dim Var_Oiseau001_ObjRotZ: Var_Oiseau001_ObjRotZ=0
Dim Var_Oiseau001_Val_ObjRotZ: Var_Oiseau001_Val_ObjRotZ=-3
Sub Oiseau_Init
	Oiseau.TransX=0 'Initialisation pour la prochaine fois
	Var_Oiseau_TransX=0 'Initialisation pour la prochaine fois
	Var_Oiseau_Nb_Deplacement=10
End Sub

Sub GateBirdMove001_Hit()
	If Var_Oiseau_Replace=False Then
		TimerOiseau.Enabled=True
		DMDScore(ValeurGateBirdMove)
		DMDScene DMDOiseauVole, "", IntensityFull, "", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
	End If
End Sub

Sub GateBirdMove002_Hit()
	If Var_Oiseau_Replace=False Then
		TimerOiseau.Enabled=True
		DMDScore(ValeurGateBirdMove)
		DMDScene DMDOiseauVole, "", IntensityFull, "", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
	End If
End Sub

Sub TimerOiseau_Timer()
	PlaysoundAt "moteur", Oiseau
	'Debug_text("Var_Oiseau_TransX : " & Var_Oiseau_TransX)
	If Var_Oiseau_Replace=True Then
	'Replacer l'oiseau en position de départ de manière rapide
		Var_Oiseau_TransX=Var_Oiseau_TransX+(Var_Oiseau_Val_Deplacement*8 mod 41)
		VisSansFin.RotZ=Var_Oiseau_TransX
		If Var_Oiseau_TransX > 0 Then
		'L'oiseau est à la position de départ on arrête
			Var_Oiseau_TransX=0
			TimerOiseau.Enabled=False
			If Table1.ShowDT = False OR TwoScreens=True Then Var_Oiseau_Replace=False End If
			Stopsound "moteur"
		End If
		Oiseau.TransX=Var_Oiseau_TransX	
	Else
	'Faire avancer l'oiseau
		Var_Oiseau_TransX=Var_Oiseau_TransX-Var_Oiseau_Val_Deplacement
		Var_Oiseau_Nb_Deplacement=Var_Oiseau_Nb_Deplacement-1
		VisSansFin.RotZ=Var_Oiseau_TransX
		Oiseau.TransX=Var_Oiseau_TransX
		If Var_Oiseau_Nb_Deplacement < 1 Then
			TimerOiseau.Enabled=False
			Stopsound "moteur"
			Var_Oiseau_Nb_Deplacement=10 'Initialisation pour la prochaine fois
		End If
	End If
	If Var_Oiseau_TransX < -960 Then
	'L'oiseau a atteint le nid, il faut le replacer
		DMDScene DMDOeuf, "Chick", IntensityFull, "hungry", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Var_Oiseau_Replace=True
		TimerOiseau.Enabled=True
		Var_Oiseau_Nb_Deplacement=10
		DMDScore(ValeurGateBirdMoveBonus)
	End If
End Sub

Sub TimerOiseau001_Timer()
	'Animation de l'oiseau en bas à gauche
	If Var_Oiseau001_ObjRotZ < -120 Then Var_Oiseau001_Val_ObjRotZ=Var_Oiseau001_Val_ObjRotZ*-1 End If
	If Var_Oiseau001_ObjRotZ > 20 Then Var_Oiseau001_Val_ObjRotZ=Var_Oiseau001_Val_ObjRotZ*-1 End If
	Var_Oiseau001_ObjRotZ=Var_Oiseau001_ObjRotZ+Var_Oiseau001_Val_ObjRotZ
	Oiseau001.ObjRotZ=Var_Oiseau001_ObjRotZ
End Sub

'____________________________________________________________________________________________________________________

'
' gestion des oiseaux du B2S
'
'____________________________________________________________________________________________________________________

Dim B2SSequence: B2SSequence=True
Sub TimerB2S_Timer()
	If B2SSequence=True Then
		Controller.B2SStartAnimation("OiseauQuiVole")
		TimerB2S.Interval=1000
		B2SSequence=False
	Else
		Controller.B2SStopAnimation("OiseauQuiVole")
		B2SSequence=True
		TimerB2S.Interval=10000+RND(500)*10
	End If
End Sub

'____________________________________________________________________________________________________________________

'
' gestion des oiseaux du Bas(flippers)
'
'____________________________________________________________________________________________________________________


Dim RotZOiseauBG: RotZOiseauBG=-90
Sub OiseauBG_Init()
	OiseauBG.ObjRotZ=LeftFlipper.CurrentAngle + RotZOiseauBG
End Sub

Sub TimerOiseauBG_Timer()
	OiseauBG.ObjRotZ=LeftFlipper.CurrentAngle + RotZOiseauBG
End Sub

Dim RotZOiseauBD: RotZOiseauBD=-90
Sub OiseauBD_Init()
	OiseauBD.ObjRotZ=RightFlipper.CurrentAngle + RotZOiseauBD
End Sub

Sub TimerOiseauBD_Timer()
	OiseauBD.ObjRotZ=RightFlipper.CurrentAngle + RotZOiseauBD
End Sub


'____________________________________________________________________________________________________________________

'
' gestion de l'oiseau du milieu(flippers) - Animation
'
'____________________________________________________________________________________________________________________


Dim RotZOiseauMD: RotZOiseauMD=-75
Sub OiseauMD_Init()
	OiseauMD.ObjRotZ=RightFlipper001.CurrentAngle + RotZOiseauMD
End Sub

Sub TimerOiseauMD_Timer()
	OiseauMD.ObjRotZ=RightFlipper001.CurrentAngle + RotZOiseauMD
End Sub


'____________________________________________________________________________________________________________________

'
' gestion des arbres - Animation
'
'____________________________________________________________________________________________________________________


Dim RotZArbre: RotZArbre=0
Dim MinRotZArbre: MinRotZArbre=-17
Dim MaxRotZArbre: MaxRotZArbre=17
Dim IncrementRotZArbre: IncrementRotZArbre=0.3
Sub Arbre001_Init()
	Arbre001.ObjRotZ=0
	Arbre002.ObjRotZ=0
	Arbre003.ObjRotZ=0
	If AnimateTrees Then
		TimerArbres.Enabled=True
	Else
		TimerArbres.Enabled=False	
	End If
End Sub

Sub TimerArbres_Timer()
	RotZArbre=RotZArbre+IncrementRotZArbre
	If RotZArbre > MaxRotZArbre or RotZArbre < MinRotZArbre Then IncrementRotZArbre = -1 * IncrementRotZArbre End If
	Arbre001.ObjRotZ=RotZArbre*0.65
	Arbre002.ObjRotZ=RotZArbre*1.11
	Arbre002.ObjRotY=RotZArbre*0.9
	Arbre003.ObjRotZ=RotZArbre*0.8
	Arbre003.ObjRotX=RotZArbre*0.9
	Arbre004.ObjRotY=RotZArbre*0.32
	gi001.Falloff=220+RotZArbre*4
	gi001.Intensity=15+RotZArbre*0.3
	'gi001.FallPower=5+abs(RotZArbre*0,3)
End Sub


'____________________________________________________________________________________________________________________

'
' gestion du chat et de lumières associées + Extra ball
'
'____________________________________________________________________________________________________________________


Dim Var_Cat_Val_ObjRotZ: Var_Cat_Val_ObjRotZ=1
Dim Var_Cat_ObjRotZ: Var_Cat_ObjRotZ=0

Sub Cat_Init()
	Dim Objet
	Cat.ObjRotZ=0
	TimerCat.Enabled=True
	TimerCat.Interval=100
End Sub

Sub TimerCat_Timer()
	'Animation du chat en haut à droite
	If Var_Cat_ObjRotZ < -25 Then Var_Cat_Val_ObjRotZ=Var_Cat_Val_ObjRotZ*-1 End If
	If Var_Cat_ObjRotZ > 25 Then Var_Cat_Val_ObjRotZ=Var_Cat_Val_ObjRotZ*-1 End If
	Var_Cat_ObjRotZ=Var_Cat_ObjRotZ+Var_Cat_Val_ObjRotZ
	Cat.ObjRotZ=Var_Cat_ObjRotZ
End Sub

Function LightCat_Initialisation()
	LightCatC_Initialisation
	LightCatA_Initialisation
	LightCatT_Initialisation
	LightExtraBall.State=0
End Function

Function LightCatC_Initialisation()
	Dim Objet
	For Each Objet in LightCatC
		Objet.State=2
		Objet.Intensity=5
	Next
	Target_Cat_000.UserValue=0
End Function

Function LightCatA_Initialisation()
	Dim Objet
	For Each Objet in LightCatA
		Objet.State=2
		Objet.Intensity=5
	Next
	Target_Cat_001.UserValue=0
End Function

Function LightCatT_Initialisation()
	Dim Objet
	For Each Objet in LightCatT
		Objet.State=2
		Objet.Intensity=5
	Next
	Target_Cat_002.UserValue=0
End Function

Sub Target_Cat_000_Hit()
	Dim Objet
	Var_Cat_ObjRotZ=20
	Cat.ObjRotZ=Var_Cat_ObjRotZ
	PlaySoundAt "Cat000", Target_Cat_000
	DMDScore(ValeurTarget_Cat_000)
	Target_Cat_000.UserValue=Target_Cat_000.UserValue+1
	ExtraBall_Check
	For Each Objet in LightCatC
		If Objet.State=2 Then Objet.State=1: Objet.Intensity=25: Exit Sub End If
	Next
	'Column C is Full !
	DMDScore(ValeurTarget_Cat_000*10)
	PlaySoundAt "Cat003", LightExtraBall
	DMDScene NomScene, "Cat C award", IntensityFull, ValeurTarget_Cat_000*10, IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
	LightCatC_Initialisation
	LightCatA_Initialisation
	LightCatT_Initialisation
End Sub

Sub Target_Cat_001_Hit()
	Dim Objet
	Var_Cat_ObjRotZ=0
	Cat.ObjRotZ=Var_Cat_ObjRotZ
	PlaySoundAt "Cat001", Target_Cat_001
	DMDScore(ValeurTarget_Cat_001)
	Target_Cat_001.UserValue=Target_Cat_001.UserValue+1
	ExtraBall_Check
	For Each Objet in LightCatA
		If Objet.State=2 Then Objet.State=1: Objet.Intensity=25: Exit Sub End If
	Next
	'Column A is Full !
	DMDScore(ValeurTarget_Cat_001*10)
	PlaySoundAt "Cat003", LightExtraBall
	DMDScene NomScene, "Cat A award", IntensityFull, ValeurTarget_Cat_001*10, IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
	LightCatC_Initialisation
	LightCatA_Initialisation
	LightCatT_Initialisation
End Sub

Sub Target_Cat_002_Hit()
	Dim Objet
	Var_Cat_ObjRotZ=-20
	Cat.ObjRotZ=Var_Cat_ObjRotZ
	PlaySoundAt "Cat002", Target_Cat_002
	DMDScore(ValeurTarget_Cat_002)
	Target_Cat_002.UserValue=Target_Cat_002.UserValue+1
	ExtraBall_Check
	For Each Objet in LightCatT
		If Objet.State=2 Then Objet.State=1: Objet.Intensity=25: Exit Sub End If
	Next
	'Column T is Full !
	DMDScore(ValeurTarget_Cat_002*10)
	PlaySoundAt "Cat003", LightExtraBall
	DMDScene NomScene, "Cat T award", IntensityFull, ValeurTarget_Cat_002*10, IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
	LightCatC_Initialisation
	LightCatA_Initialisation
	LightCatT_Initialisation
End Sub

Function ExtraBall_Check()
	If Target_Cat_000.UserValue < NombreLignesCat Then Exit Function
	If Target_Cat_001.UserValue < NombreLignesCat Then Exit Function
	If Target_Cat_002.UserValue < NombreLignesCat Then Exit Function
	'Le nombre de lignes nécessaires est atteint, on attribue l'ExtraBall
	If LightExtraBall.State=0 Then
		'Attribution de l'Extra Ball si pas déjà attribuée
		LightExtraBall.State=2
		ExtraBallWon=True 'variable pour l'attribution de la nouvelle balle
		PlaySoundAt "Cat003", LightExtraBall
		DMDScore(ValeurExtraBall)
		DMDScene NomScene, "Extra ball", IntensityFull, "", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
	End If
End Function


'____________________________________________________________________________________________________________________

'
' gestion des oiseaux du PlateauA (flippers)
'
'____________________________________________________________________________________________________________________


Dim RotZOiseauHGPlateauA: RotZOiseauHGPlateauA=-110
Sub OiseauHGPlateauA_Init()
	OiseauHGPlateauA.ObjRotZ=FlipperHG.CurrentAngle + RotZOiseauHGPlateauA
End Sub

Sub TimerOiseauHGPlateauA_Timer()
	OiseauHGPlateauA.ObjRotZ=FlipperHG.CurrentAngle + RotZOiseauHGPlateauA
End Sub

Dim RotZOiseauHDPlateauA: RotZOiseauHDPlateauA=-80
Sub OiseauHDPlateauA_Init()
	OiseauHDPlateauA.ObjRotZ=FlipperHD.CurrentAngle + RotZOiseauHDPlateauA
End Sub

Sub TimerOiseauHDPlateauA_Timer()
	OiseauHDPlateauA.ObjRotZ=FlipperHD.CurrentAngle + RotZOiseauHDPlateauA
End Sub


'____________________________________________________________________________________________________________________

'
' gestion des compteurs Rampe A et B et de la balle supplémentaire + mécanismes associés
'
'____________________________________________________________________________________________________________________

Function CompteursRampes_Initialisation()
	'On initialise tous les compteurs - ne pas cocher "static rendering"
	PrimitiveDzA000.Image="Target0"
	PrimitiveUnA000.Image="Target0"
	PrimitiveDzB000.Image="Target0"
	PrimitiveUnB000.Image="Target0"
	PrimitiveUnA000.Uservalue=0
	PrimitiveUnB000.UserValue=0
	LoopsCompeurA=0
	LoopsCompeurB=0
End Function

Sub LoopsPlayer_Initialisation()
	Dim Nombre
	For Nombre=1 to NbJoueursParPartie
		LoopsPlayer(Nombre)=0
	Next
End Sub

Dim LoopsCompeurA: LoopsCompeurA=0
Dim LoopsCompeurB: LoopsCompeurB=0
Sub TriggerRampA_Hit()
	TriggerRampA.TimerInterval=1000
	TriggerRampA.TimerEnabled=True
	DMDScore(ValeurRamp_A)
	DMDScene NomScene, "Bird Fly", IntensityFull, "", IntensityFull, AnimToLeft, DureeScene, NoAnim, PriorityScene
	LoopsPlayer(JoueurCourant)=LoopsPlayer(JoueurCourant)+1 'Compter le nombre de loops
	Debug_text("Player/loops : " & JoueurCourant & "/" & LoopsPlayer(JoueurCourant) )
End Sub

Sub TriggerRampA_Timer()
	'Augmenter et afficher le compteur à chaque accès à la rampe
	TriggerRampA.TimerEnabled=False
	LoopsCompeurA=LoopsCompeurA+1
	PrimitiveUnA000.Image="Target" & INT(LoopsCompeurA mod 10)
	If LoopsCompeurA > 9 Then PrimitiveDzA000.Image="Target" & (INT(LoopsCompeurA / 10) mod 10) Else PrimitiveDzA000.Image="Target0" End If
End Sub

Sub TriggerRampB_Hit()
	Dim Objet
	'Valide la modification du compteur Rampe B
	TriggerRampB.TimerInterval=1000
	TriggerRampB.TimerEnabled=True
	DMDScore(ValeurRamp_B)
	DMDScene NomScene, "Bird Fly", IntensityFull, "", IntensityFull, AnimToLeft, DureeScene, NoAnim, PriorityScene
	LoopsPlayer(JoueurCourant)=LoopsPlayer(JoueurCourant)+1 'Compter le nombre de loops
	Debug_text("Player/loops : " & JoueurCourant & "/" & LoopsPlayer(JoueurCourant) )
	'Point d'entrée pour valider les action sur les process liés aux cibles Target_l ? 
	TargetRat_Verification
End Sub

Sub TriggerRampB_Timer()
	'Augmenter et afficher le compteur à chaque accès à la rampe
	TriggerRampB.TimerEnabled=False
	LoopsCompeurB=LoopsCompeurB+1
	PrimitiveUnB000.Image="Target" & INT(LoopsCompeurB mod 10)
	If LoopsCompeurB > 9 Then PrimitiveDzB000.Image="Target" & (INT(LoopsCompeurB / 10) mod 10) Else PrimitiveDzB000.Image="Target0" End If
End Sub

Function KickerRamp_Initialisation()
	MultiplierTemporaire=1
	KickerRamp.TimerEnabled=False
	KickerRamp.Kick -60,5
End Function


Sub KickerRamp_Hit()
	PlaySound "Birdy_" & Int(Rnd*Var_nb_sounds_Birdy_x)+1, 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	KickerRamp.TimerEnabled=True
	'On modifie la période pour éjectionde la balle et initialise les CompteursRampes_Initialisation

	KickerRamp.TimerInterval=KickerRampTimer * (LoopsCompeurB + LoopsCompeurB + 1)
	DMDScene NomScene, "Temporary score X2", IntensityFull, Int(KickerRamp.TimerInterval/1000) & " seconds", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
	CompteursRampes_Initialisation
	MultiplierTemporaire=2
End Sub

Sub KickerRamp_Timer()
	MultiplierTemporaire=1
	DMDScene NomScene, "Temporary score X2", IntensityFull, "Over", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
	PlaySound "popper_ball"
	KickerRamp.TimerEnabled=False
	KickerRamp.kick -85,75
End Sub


'____________________________________________________________________________________________________________________

'
' gestion des oeufs
'
'____________________________________________________________________________________________________________________


Dim Egg001MaxZ: Egg001MaxZ=25
Dim Egg001Displacment: Egg001Displacment=1
Dim Egg002MaxZ: Egg002MaxZ=30
Dim Egg002Displacment: Egg002Displacment=2
Dim Egg003MaxZ: Egg003MaxZ=20
Dim Egg003Displacment: Egg003Displacment=3
Sub TimerEggs_Timer()
	OeufHaut001.TransY=OeufHaut001.TransY+Egg001Displacment
	OeufHaut001.RotX=OeufHaut001.RotX+Egg001Displacment*1.5
	Oisillon001.TransY=Oisillon001.TransY+Egg001Displacment
	If Oisillon001.TransY < 0 Then Egg001Displacment=Abs(Egg001Displacment) End If
	If Oisillon001.TransY > Egg001MaxZ Then Egg001Displacment=Abs(Egg001Displacment)*-1 End If

	OeufHaut002.TransY=OeufHaut002.TransY+Egg002Displacment
	Oisillon002.TransY=Oisillon002.TransY+Egg002Displacment
	If Oisillon002.TransY < 0 Then Egg002Displacment=Abs(Egg002Displacment) End If
	If Oisillon002.TransY > Egg002MaxZ Then Egg002Displacment=Abs(Egg002Displacment)*-1 End If

	OeufHaut003.RotZ=OeufHaut003.RotZ+Egg003Displacment*6
	Oisillon003.TransY=Oisillon003.TransY+Egg003Displacment
	If Oisillon003.TransY < 0 Then Egg003Displacment=Abs(Egg003Displacment) End If
	If Oisillon003.TransY > Egg003MaxZ Then Egg003Displacment=Abs(Egg003Displacment)*-1 End If
End Sub

'____________________________________________________________________________________________________________________

'
' gestion des cibles mouvantes
' Exporter en .obj sous Blender en -Y forward et -Z Up
'
'____________________________________________________________________________________________________________________

'x = x0 + r*cos(t)
'y = y0 + r*sin(t)

'où (x0,y0) sont les coord du centre, r est le rayon, et t l'angle.
Dim Cible001Angle 'Angle de la rotation actuelle
Dim Cible001DeltaAngle 'Pas de l'angle aappliqué - variable
Dim Cible001DeltaAngleMin 'Valeur à laquelle l'angle est diminué ou augmenté
Dim Cible001DeltaAngleMax'Valeur à laquelle l'angle est diminué ou augmenté
Dim Cible001Rayon ' rayon du cercle
Dim Cible001x0 'Initial position x of circle center
Dim Cible001y0 'Initial position y of circle center
Sub Cible001_Init()
	Cible001Angle=0
	Cible001DeltaAngle=0.015
	Cible001DeltaAngleMin=1
	Cible001DeltaAngleMax=3.5
	Cible001Rayon=200
	Cible001x0=820
	Cible001y0=1050
	Cible001.X= Int(Cible001x0 + Cible001Rayon*cos(Cible001Angle))
	Cible001.Y= Int(Cible001y0 + Cible001Rayon*sin(Cible001Angle))
	TimerCible001.Interval=100
	TimerCible001.Enabled=False
End Sub

Sub TimerCible001_Timer()
	Cible001.X= Int(Cible001x0 + Cible001Rayon*cos(Cible001Angle))
	Cible001.Y= Int(Cible001y0 + Cible001Rayon*sin(Cible001Angle))
	If Cible001Angle > Cible001DeltaAngleMax Then Cible001Angle=Cible001DeltaAngleMax: Cible001DeltaAngle=-1*Abs(Cible001DeltaAngle) End If
	If Cible001Angle < Cible001DeltaAngleMin Then Cible001Angle=Cible001DeltaAngleMin: Cible001DeltaAngle=Abs(Cible001DeltaAngle) End If
	Cible001Angle=Cible001Angle + Cible001DeltaAngle
	'Debug_text(Cible001.X & "/" & Cible001.Y & "/" & Cible001.TransX & "/" & Cible001.TransY & "/" & Cible001Angle)
End Sub

Sub Cible001_Hit()

End Sub


'____________________________________________________________________________________________________________________

'
' gestion des cibles TargetRat (haut droite) des compteurs Rampe B et gestion des cibles TargetRatAttack
'
'____________________________________________________________________________________________________________________

Dim TargetRatAttackDown
Dim TargetRatAttackRaw: TargetRatAttackRaw=4
Dim TargetRatAttackBonusRaw: TargetRatAttackBonusRaw=1500
Sub TargetRat_Hit(idx)
	Debug_text("TargetRat : " & idx)
	TargetRat(idx).UserValue=1
	DMDScore(ValeurTargetRat)
	PlaySound "TargetHit",1, Vol(ActiveBall), AudioPan(ActiveBall), 1, Pitch(ActiveBall), 1, 1, AudioFade(ActiveBall)
	FlasherRatIterations=0
	FlasherRat001.TimerEnabled=True
	LightRat001.State=2
End Sub

Function TargetRat_Verification()
	'Vérifier que toutes les cibles TargetRat sont baissées
	'Si elles sont baissées et que l'on passe sur la rampre centrale
	'on montre les cibles RatAttack et valide les timers
	Dim Objet
	For Each Objet in TargetRat
		If Objet.UserValue=0 Then Exit Function End If
	Next
	TargetRatR.TimerInterval=1000
	TargetRatR.TimerEnabled=True
	DMDScene NomScene, "Rat Attack", IntensityFull, "", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
End Function

Sub TriggerRat_Hit
	DMDScore(ValeurTargetRat)
	FlasherRatIterations=0
	FlasherRat001.TimerEnabled=True
	LightRat001.State=2
End Sub

Dim FlasherRatIterations: FlasherRatIterations=0
Sub FlasherRat001_Timer()
	FlasherRatIterations=FlasherRatIterations+1
	If (FlasherRatIterations Mod 2) = 1 Then
		FlasherRat001.Opacity=0 + RND(1)*10
		FlasherRat002.Opacity=70 - RND(1)*10
		FlasherRat003.Opacity=0 + RND(1)*10
	Else
		FlasherRat001.Opacity=70 - RND(1)*10
		FlasherRat002.Opacity=0 + RND(1)*10
		FlasherRat003.Opacity=70 - RND(1)*10
	End If
	If FlasherRatIterations > 10 Then
		FlasherRatIterations=0
		FlasherRat001.TimerEnabled=False
		FlasherRat001.Opacity=0
		FlasherRat002.Opacity=0
		FlasherRat003.Opacity=0
		LightRat001.State=0
	End If
End Sub

Sub TargetRatR_Timer()
	TargetRat_Initialisation 'On montre toutes les cibles TargetRat
	TargetRatAttackDown=0 'Initialiser le timer et les compteur pour nice shot on TargetRatAttack
	TargetRatAttack001.TimerInterval=1000
	TargetRatAttack001.TimerEnabled=True
	TargetRatAttack_Initialisation(False) 'On montre les cibles de TargetRatAttack
End Sub

Function TargetRat_Initialisation()
	Dim Objet
	For Each Objet in TargetRat
		Objet.IsDropped=False
		Objet.UserValue=0
	Next
	TargetRatR.TimerEnabled=False
	TargetRatAttack001.TimerEnabled=False
End Function

Sub TargetRatAttack_Hit(idx)
	'Abaisser la cible TargetRatAttack qui est touchée
	TargetRatAttack(idx).IsDropped=True
	TargetRatAttackDown=TargetRatAttackDown+1
	PlaySound "TargetHit",1, Vol(ActiveBall), AudioPan(ActiveBall), 1, Pitch(ActiveBall), 1, 1, AudioFade(ActiveBall)
	If TargetRatAttackDown < TargetRatAttackRaw Then
		DMDScore(ValeurTargetRatAttack)
	End If
End Sub

Sub TargetRatAttack001_Timer()
	Dim Objet
	Dim Coeff: Coeff=5
	If TargetRatAttackDown > TargetRatAttackRaw Then
		DMDScore((ValeurTargetRatAttack*Coeff)+(TargetRatAttackBonusRaw*TargetRatAttackDown))
		DMDScene NomScene, TargetRatAttackDown & " rats in a second", IntensityFull, "Bonus " & (ValeurTargetRatAttack*Coeff)+(TargetRatAttackBonusRaw*TargetRatAttackDown), IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
	End If
	TargetRatAttackDown=0		
	'Tester si toutes les cibles sont abaissées, on sort de la boucle à la première trouvée
	For Each Objet In TargetRatAttack
		If Objet.IsDropped=False Then
			Exit Sub
		End If
	Next
	DMDScore(ValeurTargetRatAttack*Coeff)
	DMDScene NomScene, "Rats defeated", IntensityFull, "Bonus " & ValeurTargetRatAttack*Coeff, IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
	TargetRatAttack001.TimerEnabled=False
End Sub

Function TargetRatAttack_Initialisation(Etat)
	'Abaisser ou relever toutes les TargetRatAttack
	Dim Objet
	For Each Objet In TargetRatAttack
		Objet.IsDropped=Etat
	Next
End Function


'____________________________________________________________________________________________________________________

'
' gestion du lancer de la boule
'
'____________________________________________________________________________________________________________________

Sub GateBallLaunched_Hit()
	PhaseJeu = "PartieEnCours" 'Positionner entre autres pour bloquer l'ajout de players, de credits...
	LightCentral001.TimerEnabled=True
	BallSaving=True
	Debug_text("PhaseJeu = " & PhaseJeu)
End Sub


'____________________________________________________________________________________________________________________

'
' gestion des High Scores
'
'____________________________________________________________________________________________________________________


'Nom du fichier des High Scores
Const HSFileName="Bird_Fly.txt"
Dim Letter					'Used to track the currently selected character
Dim Initials(3)				'Used to store the initials chosen
Dim InitialsChosen: InitialsChosen=4			'Used to flag how many initials have been entered
'Quels caractères sont autorisés dans les High Scores
Dim Caracteres: Caracteres="ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789<.-:+>?!"
Dim DernierCar: DernierCar=Len(Caracteres)
Dim HighScoreAction: HighScoreAction="Verifier"
Dim HighScoreNo: HighScoreNo=0
Dim CurrentTestedPlayer: CurrentTestedPlayer=1
Dim HSScore(5)
HSScore(1) = 18000000
HSScore(2) = 5000000
HSScore(3) = 750000
HSScore(4) = 200000
HSScore(5) = 150000
Dim HSName(5)
HSName(1) = "LOL"
HSName(2) = "TIT"
HSName(3) = "DAD"
HSName(4) = "MOM"
HSName(5) = "PAT"
Dim HLNumber
HLNumber=5
Dim HLName
HLName="LOL"

Sub ScrollLetterLeft 'Left flipper scrolls letter down the character set
	Letter=Letter-1
	If Letter < 1 Then Letter = DernierCar End If
End Sub

Sub ScrollLetterRight 'Right flipper scrolls letter down the character set
	Letter=Letter+1
	If Letter > DernierCar Then Letter = 1 End If
End Sub

Sub TimerHighScoreSaisie_Timer() 'Textbox timer is used to check if initials are to be entered & what to display if initials should be entered
	Select Case InitialsChosen
		Case 0:
			If TimerHighScoreSaisie.UserValue=1 Then
				DMDScene NomScene, "Select letter" , Intensity075, "  ", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
			Else
				DMDScene NomScene, "Select letter" , Intensity075, MID(Caracteres, Letter, 1), IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
			End If
		Case 1:
			If TimerHighScoreSaisie.UserValue=1 Then
				DMDScene NomScene, "Select letter" , Intensity075, Initials(0) & " ", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
			Else
				DMDScene NomScene, "Select letter" , Intensity075, Initials(0) & MID(Caracteres, Letter, 1), IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
			End If
		Case 2:
			If TimerHighScoreSaisie.UserValue=1 Then
				DMDScene NomScene, "Select letter" , Intensity075, Initials(0) & Initials(1) & " ", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
			Else
				DMDScene NomScene, "Select letter" , Intensity075, Initials(0) & Initials(1) & MID(Caracteres, Letter, 1), IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
			End If
		Case 3:
			DMDScene NomScene, "Bird Fly award" , Intensity075, Initials(0) &  Initials(1) & MID(Caracteres, Letter, 1), IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
			TimerHighScoreSaisie.Enabled=False
			If HighScoreAction="SaisieInitials" Then
				HighScoreAction="Valoriser" 'Trigramme choisi, on valorise le nom du High Score
			Else
				HighScoreAction="LoopsValoriser" 'Trigramme choisi, on valorise le nom champion des loops
			End If
	End Select
	'On inverse la visibilité de la lettre pour la faire clignoter
	If TimerHighScoreSaisie.UserValue=1 Then TimerHighScoreSaisie.UserValue=0 Else TimerHighScoreSaisie.UserValue=1 End If
End Sub

Sub SelectLetter							'When "1" is pressed a character is saved to InitialsChosen array
	Initials(InitialsChosen)=MID(Caracteres, Letter, 1)
	InitialsChosen=InitialsChosen+1			'Sets the count to the correct number
End Sub

Sub HighScoreSave()
	' Based on Black's Highscore routines
	Dim FileObj
	Dim ScoreFile
	Dim Objet

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) Then 
		Exit Sub
	End If
	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & HSFileName,True)
	Scorefile.writeline NbCredits
	For Objet=1 to 5
		Scorefile.writeline HSScore(Objet)
	Next
	For Objet=1 to 5
		Scorefile.writeline HSName(Objet)
	Next
	Scorefile.writeline HLNumber
	Scorefile.writeline HLName
	ScoreFile.Close
	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub

Sub HighScoreLoad()
    ' Based on Black's Highscore routines
    Dim FileObj
	Dim ScoreFile
	Dim TempHSScore(5), TempHSName(5)
    Dim TextStr

    Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) Then 
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & HSFileName) Then
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & HSFileName)
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
	If (TextStr.AtEndOfStream=True) then
		Exit Sub
	End If
	NbCredits=Int(TextStr.ReadLine)
	Debug_Text("NbCredits :" & NbCredits)
	TempHSScore(1)=TextStr.ReadLine
	HSName(1)=int(TempHSScore(1))
	TempHSScore(2)=TextStr.ReadLine
	HSScore(2)=int(TempHSScore(2))
	TempHSScore(3)=TextStr.ReadLine
	HSScore(3)=int(TempHSScore(3))
	TempHSScore(4)=TextStr.ReadLine
	HSScore(4)=int(TempHSScore(4))
	TempHSScore(5)=TextStr.ReadLine
	HSScore(5)=int(TempHSScore(5))
	HSName(1)=TextStr.ReadLine
	HSName(2)=TextStr.ReadLine
	HSName(3)=TextStr.ReadLine
	HSName(4)=TextStr.ReadLine
	HSName(5)=TextStr.ReadLine
	HLNumber=TextStr.ReadLine
	HLName=TextStr.ReadLine
	TextStr.Close
	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub

Function DecalerHighScore(HighScoreNo)
	Select Case HighScoreNo
		Case 1:
			HSScore(5)=HSScore(4): HSName(5)=HSName(4)
			HSScore(4)=HSScore(3): HSName(4)=HSName(3)
			HSScore(3)=HSScore(2): HSName(3)=HSName(2)
			HSScore(2)=HSScore(1): HSName(2)=HSName(1)
		Case 2:
			HSScore(5)=HSScore(4): HSName(5)=HSName(4)
			HSScore(4)=HSScore(3): HSName(4)=HSName(3)
			HSScore(3)=HSScore(2): HSName(3)=HSName(2)
		Case 3:
			HSScore(5)=HSScore(4): HSName(5)=HSName(4)
			HSScore(4)=HSScore(3): HSName(4)=HSName(3)
		Case 4:
			HSScore(5)=HSScore(4): HSName(5)=HSName(4)
		Case 5:
			'Rien à faire
	End Select
End Function

Dim LoopChampion: LoopChampion=0
Sub TimerHighScore_Timer()
	Dim Nombre
	Select Case HighScoreAction
		Case "Verifier":
			Initials(0)=""
			Initials(1)=""
			Initials(2)=""
			Letter=1	'Reset the default 1st character to "A"
			InitialsChosen=0	'Reset this flag to show number of initials actually chosen			
			For HighScoreNo=1 to 5
				Debug_Text("Player/Score : " & CurrentTestedPlayer & "/" & ScorePlayer(CurrentTestedPlayer) & "HighscoreNo/Score : " & CurrentTestedPlayer & "/" & ScorePlayer(CurrentTestedPlayer))
				If ScorePlayer(CurrentTestedPlayer) > HSScore(HighScoreNo) Then 
					DecalerHighScore(HighScoreNo)
					HSScore(HighScoreNo) = ScorePlayer(CurrentTestedPlayer)
					HighScoreAction="AfficherRang"
					Exit Sub
				End If
			Next
			'Si on est ici c'est que le joueur n'a pas fait de High score.
			'On va controler le joueur suivant
			HighScoreAction="Termine"
		Case "AfficherRang":
			DMDScene NomScene, "Player " & CurrentTestedPlayer, Intensity075, "High score " & HighScoreNo, IntensityFull, AnimFadeIn, DureeScene, AnimFadeOut, PriorityScene
			PlaySound "Ball_lost",0,1,AudioPan(Drain),0.25,0,0,1,AudioFade(Drain)
			HighScoreAction="EnterInitials"
		Case "EnterInitials":
			DMDScene NomScene, "", Intensity075, "Enter initials", IntensityFull, AnimFadeIn, DureeScene, AnimFadeOut, PriorityScene
			HighScoreAction="AutoriserSaisie"
		Case "AutoriserSaisie":
			HighScoreAction="SaisieInitials"
			TimerHighScoreSaisie.Enabled=True 'Autoriser la saisie
		Case "Valoriser":
			HSName(HighScoreNo)=Initials(0) & Initials(1) & Initials(2)
			HighScoreSave
			HighScoreAction="Termine"
		Case "Termine":
			TimerHighScoreSaisie.Enabled=False 'Interdire la saisie
			If CurrentTestedPlayer = NbJoueursOld Then
				HighScoreAction="LoopsChampionTest"
			Else
				'On va traiter le score du joueur suivant
				CurrentTestedPlayer=CurrentTestedPlayer+1
				HighScoreAction="Verifier"
			End if
		Case "LoopsChampionTest":
			LoopChampion=0
			For nombre = 1 to NbJoueursOld
				Debug_text("Test Player/loops/HLNumber : " & Nombre & "/" & LoopsPlayer(Nombre) & "/" & HLNumber )
				If LoopsPlayer(Nombre) > Int(HLNumber) Then
					'Nombre de Loops battu
					HLNumber=LoopsPlayer(Nombre)
					LoopChampion=Nombre
				End If
			Next
			If LoopChampion=0 Then
				'Nombre de loops pas battu, on va aux teasers
				HighScoreAction="Teasers"
			Else
				HighScoreAction="LoopsChampionAward1"
				Debug_text("Final Player/loops/HLNumber : " & LoopChampion & "/" & LoopsPlayer(LoopChampion) & "/" & HLNumber )
				DMDScene NomScene, "Player " & LoopChampion & " is", Intensity075, "Bird Fly champion", IntensityFull, AnimFadeIn, DureeScene, AnimFadeOut, PriorityScene
				PlaySound "Ball_lost",0,1,AudioPan(Drain),0.25,0,0,1,AudioFade(Drain)
			End If	
		Case "LoopsChampionAward1":
			Initials(0)=""
			Initials(1)=""
			Initials(2)=""
			Letter=1	'Reset the default 1st character to "A"
			InitialsChosen=0	'Reset this flag to show number of initials actually chosen
			DMDScene NomScene, HLNumber & " Bird Fly", IntensityFull, "", IntensityFull, AnimFadeIn, DureeScene, AnimFadeOut, PriorityScene
			HighScoreAction="LoopsChampionAward2"
		Case "LoopsChampionAward2":
			DMDScene NomScene, "", Intensity075, "Enter initials", IntensityFull, AnimFadeIn, DureeScene, AnimFadeOut, PriorityScene
			HighScoreAction="LooopAutoriserSaisie"
		Case "LooopAutoriserSaisie"
			HighScoreAction="SaisieInitialsLoops"
			TimerHighScoreSaisie.Enabled=True 'Autoriser la saisie
		Case "LoopsValoriser":
			DMDScene NomScene, "Player " & LoopChampion & " is", Intensity075, "Bird Fly champion", IntensityFull, AnimFadeIn, DureeScene, AnimFadeOut, PriorityScene
			HLName=Initials(0) & Initials(1) & Initials(2)
			HighScoreSave
			HighScoreAction="Teasers"
		Case "Teasers":
			'On a traité le score du dernier joueur ainsi que le champion des LoopS
			DMDScene NomScene, "", IntensityFull, "Bird Fly", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
			TimerHighScore.Enabled=False
			InitialsChosen=4 'Permet de valider joueur ou ajout de pièce
			CurrentTestedPlayer=1
			HighScoreAction="Verifier"
			'On peut partir sur les teasers
			Teaser_Initialisation True,1042, 3000
	End Select
End Sub
'____________________________________________________________________________________________________________________

'
' gestion des High Scores - spécificités de la table
'
'____________________________________________________________________________________________________________________

	Dim Nombre
	For Nombre=1 to NbJoueursParPartie
		LoopsPlayer(Nombre)=0
	Next
'____________________________________________________________________________________________________________________

'
' gestion de la loterie
'
'____________________________________________________________________________________________________________________
Dim PhaseLoterie: PhaseLoterie=1000
Sub TimerPhaseLoterie_Init()
	TimerPhaseLoterie.Enabled=False
End Sub

Sub TimerPhaseLoterie_Timer()
'Gestion de la loterie
	Dim Win: Win=False
	Dim NumToWin: NumToWin=0
	Dim PremierePhase: PremierePhase=999
	Dim DernierePhase: DernierePhase=1015
	Dim Message: Message=""
	Dim Nombre: Nombre=0

	If PhaseLoterie < PremierePhase Then PhaseLoterie=PremierePhase End If
	If PhaseLoterie > DernierePhase Then PhaseLoterie=PremierePhase: Exit Sub End If

	PhaseLoterie=PhaseLoterie+1
	TimerPhaseLoterie.Enabled=True

	Select Case PhaseLoterie
	Case 1000, 1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008, 1009, 1010, 1011, 1012, 1013:
		Message = "Your score "
		For Nombre=1 to NbJoueursOld
			Message = Message & " " & ((ScorePlayer(Nombre) Mod 100) /10) & "0"
		Next

		NumToWin = (Int(100 * RND(1)) mod 10) * 10
		If NumToWin = 0 Then
			DMDScene NomScene, Message, IntensityFull, "Match to win 00", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Else
			DMDScene NomScene, Message, IntensityFull, "Match to win " & NumToWin, IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		End If
		If PhaseLoterie = 1000 Then
			TimerPhaseLoterie.Interval=3500		
		ElseIf PhaseLoterie < 1009 Then
			TimerPhaseLoterie.Interval=300
		Else
			TimerPhaseLoterie.Interval=200		
		End If
		Playsound "Beep.1"

	Case 1014:
		NumToWin = (Int(100 * RND(1)) mod 10) * 10
		Win=False
		For Nombre=1 to NbJoueursOld
			If NumToWin=(ScorePlayer(Nombre) Mod 100) Then
				NbCredits=NbCredits+1
				Win=True
				Playsound "Partie_gratuite"
			End If
		Next
		If Win=True Then
			'Partie gratuite gagnée à la loterie
			DMDScene NomScene, NumToWin/10 & "0 good fruit picked", IntensityFull, "Credits " & NbCredits, IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Else
			DMDScene NomScene, NumToWin/10 & "0 bad fruit picked", IntensityFull, "Credits " & NbCredits, IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene		
		End If
		TimerPhaseLoterie.Interval=4000

	 Case DernierePhase:
		'Fin de la gestion de la loterie
		DMDScene NomScene, "Game over", IntensityFull, "", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Playsound "Beep.1"
		PhaseLoterie=PremierePhase 'On initialise pour la prochaine fois
		TimerPhaseLoterie.Enabled=False
		'On va tester les High scores
		CurrentTestedPlayer=1
		HighScoreAction="Verifier"
		TimerHighScore.Enabled=True
	End Select
End Sub


'____________________________________________________________________________________________________________________

'
' gestion des Affichages
'
'____________________________________________________________________________________________________________________

'Variables pour le DMD
Dim UltraDMD
Dim OldDMDPrio
Dim PauseAdjuster: PauseAdjuster=1
Dim DMDFolder: DMDFolder="Bird_Fly.DMD" 'Initialiser dans LoadUltraDMD
Dim ImgList, DMD_Default, DMD_Insertcoin, DMDEngage, DMDOeuf, DMDOiseauVole
Dim Insertcoin
Dim UltraDMD_VideoMode_Middle: UltraDMD_VideoMode_Middle=2

Dim NomScene: NomScene="DMD.Default.png" 'Initialiser dans LoadUltraDMD
Dim DureeScene: DureeScene=2100
'0:  Fade In
'1:  Fade Out
'2:  Zoom In
'3:  Zoom Out
'4:  Scroll Off Left
'5:  Scroll Off Right
'6:  Scroll On Left
'7:  Scroll On Right
'8:  Scroll Off Up
'9:  Scroll Off Down
'10: Scroll On Up
'11: Scroll On Down
'14: No animation
Dim AnimFadeIn: AnimFadeIn=0
Dim AnimFadeOut: AnimFadeOut=1
Dim AnimZoomIn: AnimZoomIn=2
Dim AnimZoomOut: AnimZoomIn=3
Dim AnimToLeft: AnimToLeft=6
Dim AnimToRight: AnimToRight=5
Dim AnimDown: AnimDown=9
Dim AnimUp: AnimUp=10
Dim IntensityFull: IntensityFull=15
Dim Intensity075: Intensity075=11
Dim NoAnim: NoAnim=14
Dim PriorityScene: PriorityScene=50

Sub LoadUltraDMD()
    Dim fso
    Dim curDir
	NomScene="DMD.Default.png"
	DMDFolder="Bird_Fly.DMD"
	If Table1.ShowDT = False OR TwoScreens=True Then
		Set UltraDMD = CreateObject("UltraDMD.DMDObject")
		UltraDMD.Init
		Set fso = CreateObject("Scripting.FileSystemObject")
		curDir = fso.GetAbsolutePathName(".")
		UltraDMD.SetProjectFolder curDir & "\" & DMDFolder    'Directory where to find DMD objects for the table

		'Donner la liste des animations ____________________________________________________
		' L image par défaut est rajoutée à la fin de l'animation

		ImgList="DMD.insertcoin.000.png,DMD.insertcoin.001.png,DMD.insertcoin.002.png,DMD.insertcoin.003.png,DMD.insertcoin.004.png,DMD.insertcoin.005.png,DMD.insertcoin.006.png,DMD.insertcoin.007.png,DMD.insertcoin.008.png,DMD.insertcoin.009.png,DMD.insertcoin.010.png,DMD.insertcoin.011.png,DMD.insertcoin.012.png,DMD.insertcoin.013.png,DMD.insertcoin.014.png,DMD.insertcoin.015.png,DMD.insertcoin.016.png,DMD.insertcoin.017.png,DMD.insertcoin.018.png,DMD.insertcoin.019.png,DMD.Default.png"
		DMD_Insertcoin=UltraDMD.CreateAnimationFromImages(15, false, ImgList)

		ImgList="DMDEngage001.png,DMDEngage002.png,DMDEngage003.png,DMDEngage004.png,DMDEngage005.png,DMDEngage006.png,DMDEngage007.png,DMDEngage008.png,DMDEngage009.png,DMDEngage010.png,DMDEngage011.png,DMDEngage012.png, DMD.Default.png"
		DMDEngage=UltraDMD.CreateAnimationFromImages(15, false, ImgList)

		ImgList="DMDOeuf001.png,DMDOeuf002.png,DMDOeuf003.png,DMDOeuf004.png,DMDOeuf005.png,DMDOeuf006.png,DMDOeuf007.png,DMDOeuf008.png,DMDOeuf009.png,DMDOeuf010.png,DMDOeuf011.png,DMDOeuf012.png,DMDOeuf013.png,DMDOeuf014.png,DMDOeuf015.png,DMDOeuf016.png,DMDOeuf017.png,DMDOeuf018.png,DMDOeuf019.png,DMDOeuf020.png,DMDOeuf021.png,DMDOeuf022.png,DMDOeuf023.png,DMDOeuf024.png,DMDOeuf025.png,DMDOeuf026.png,DMDOeuf027.png,DMDOeuf028.png,DMDOeuf029.png,DMDOeuf030.png,DMDOeuf031.png,DMDOeuf032.png,DMDOeuf033.png,DMDOeuf034.png,DMDOeuf035.png,DMDOeuf036.png, DMD.Default.png"
		DMDOeuf=UltraDMD.CreateAnimationFromImages(25, false, ImgList)

		ImgList="DMDOiseauVole001.png,DMDOiseauVole002.png,DMDOiseauVole001.png,DMDOiseauVole002.png,DMDOiseauVole001.png,DMDOiseauVole002.png,DMDOiseauVole001.png,DMDOiseauVole002.png,DMDOiseauVole001.png,DMDOiseauVole002.png,DMDOiseauVole001.png,DMDOiseauVole002.png,DMDOiseauVole001.png,DMDOiseauVole002.png,DMD.Default.png"
		DMDOiseauVole=UltraDMD.CreateAnimationFromImages(25, false, ImgList)

		'Afficher l'écran de présentation
		DMDScene NomScene, "", IntensityFull, "Bird Fly", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
	End If
End Sub

Sub DMDScene (background, toptext, topbright, bottomtext, bottombright, animatein, pause, animateout, prio)
	ScoreText.Text=toptext & " " & bottomtext
	If topbright=-1 Then bottombright=15 End If
	If bottombright=-1 Then bottombright=15 End If
	'regular DMD call with priority
	If Table1.ShowDT = False OR TwoScreens=True Then
		If prio >= OldDMDPrio Then
			DMDSceneInt background, toptext, topbright, bottomtext, bottombright, animatein, pause, animateout
			OldDMDPrio = prio
		End If
	End If
End Sub

Sub DMDSceneInt (background, toptext, topbright, bottomtext, bottombright, animatein, pause, animateout)
	'This gets called if the priority is greater than or equal to the current scene in order to interrupt it
	Dim X
	If Table1.ShowDT = False OR TwoScreens=True Then
		If Not UltraDMD is Nothing Then
			UltraDMD.CancelRendering
			UltraDMD.DisplayScene00 background, toptext, topbright, bottomtext, bottombright, animatein, pause*PauseAdjuster, animateout
			If pause > 0 OR animateIn < 14 OR animateOut < 14 Then
				TiDMDScore.Enabled = False
				TiDMDScore.Enabled = True
			End If
		End If
	End If
End Sub

Sub DMDSceneQ (background, toptext, topbright, bottomtext, bottombright, animatein, pause, animateout)
	'EP- Called to queue up a video
	Dim X
	If Table1.ShowDT = False OR TwoScreens=True Then
		If Not UltraDMD is Nothing Then
			UltraDMD.DisplayScene00 background, toptext, topbright, bottomtext, bottombright, animatein, pause*PauseAdjuster, animateout
			If pause > 0 OR animateIn < 14 OR animateOut < 14 Then
				TiDMDScore.Enabled = False
				TiDMDScore.Enabled = True
			End If
		End If
	End If
End Sub

Sub TiDMDScore_Timer()
	If Table1.ShowDT = False OR TwoScreens=True Then
		If Not UltraDMD is Nothing Then
			On Error Resume Next
			If Not UltraDMD.IsRendering Then
				'When the scene finishes rendering, then immediately display the scoreboard
				me.Enabled = False
				DMDScore()
			End If
			On Error GoTo 0
		End If
	End If
End Sub

Sub DMDScore(Ajouter)
	Debug_Text("Score ajouté/Multiplier : "  & Ajouter & "/" & Multiplier & " Score total Player" & JoueurCourant & " :" & ScorePlayer(JoueurCourant) )
	If Tilt=True then Exit Sub End If 'Gestion du tilt
	ScorePlayer(JoueurCourant)=ScorePlayer(JoueurCourant)+(Ajouter*Multiplier*MultiplierTemporaire)+10
	ScoreText.Text="Player "& JoueurCourant & " ball " & BalleCourante & "             " & ScorePlayer(JoueurCourant)
	'Ajout de partie gratuite sur atteinte du score
	If ScorePlayer(JoueurCourant) > ScoreToWin1 And AwardGivenPlayer(JoueurCourant) = 0 Then
		AwardGivenPlayer(JoueurCourant)=1
		NbCredits=NbCredits+1
		DMDScene NomScene, "Free play", IntensityFull, "Next " & ScoreToWin2, IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		PlaySound "fx_partie_gratuite"
	End If
	If ScorePlayer(JoueurCourant) > ScoreToWin2 And AwardGivenPlayer(JoueurCourant) = 1 Then
		AwardGivenPlayer(JoueurCourant)=2
		NbCredits=NbCredits+1
		DMDScene NomScene, "Free play", IntensityFull, "Next " & ScoreToWin3, IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		PlaySound "fx_partie_gratuite"
	End If
	If ScorePlayer(JoueurCourant) > ScoreToWin3 And AwardGivenPlayer(JoueurCourant) = 2 Then
		AwardGivenPlayer(JoueurCourant)=3
		NbCredits=NbCredits+1
		DMDScene NomScene, "Free play", IntensityFull, "", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		PlaySound "fx_partie_gratuite"
	End If
	If Table1.ShowDT = False OR TwoScreens=True Then
		If Not UltraDMD is Nothing Then
			On Error Resume Next
			OldDMDPrio = 0
			UltraDMD.SetScoreboardBackgroundImage NomScene, 15, 10
			UltraDMD.DisplayScoreboard NbJoueurs, JoueurCourant, ScorePlayer(1), ScorePlayer(2), ScorePlayer(3), ScorePlayer(4), "Ball " & BalleCourante, "Credits " & NbCredits
			On Error GoTo 0
		End If
	End If
End sub


'Afficher les scores à battre sur l'étiquette de la caisse
Sub Affichage1_Init()
	AfficherScoreToWin 1,ScoreToWin1
	AfficherScoreToWin 2,ScoreToWin2
	AfficherScoreToWin 3,ScoreToWin3
	AfficherScoreToWin 4,BallesParPartie
End Sub

Sub AfficherScoreToWin(LineNo, String)
	'Afficher les scores à battre sur l'étiquette de la caisse
	Dim Letter
	Dim ThisDigit
	Dim ThisChar
	Dim QuelleImage
	Dim StrLen
	Dim LetterLine
	Dim Index
	Dim StartHSArray
	Dim EndHSArray
	Dim LetterName
	Dim xfor
	StartHSArray=array(0,1,12,23,34)
	EndHSArray=array(0,11,22,33,34)
	StrLen = len(string)
	Index = 1

	For xfor = StartHSArray(LineNo) to EndHSArray(LineNo)
		ThisChar = Mid(String, Index, 1)
		'Eval("HS"&xfor).image = GetHSChar(String, Index)
		If ThisChar = " " or ThisChar = "" then
			QuelleImage = "Postit" & "BL"
		Else 
			QuelleImage = "Postit" & ThisChar
		End If
		Eval("Affichage"&xfor).image = QuelleImage
		Index = Index + 1
	Next
End Sub


'____________________________________________________________________________________________________________________

'
' gestion du Tilt
'
'____________________________________________________________________________________________________________________

Sub GestionTilt() 'Procédure appellée à chaque coup sur la caisse (Nudge)
	NbTilt=NbTilt+1
	TimerTilt.Enabled=True
	If NbTilt > 1 And NbTilt < 5 Then
		DMDScene NomScene, "Tilt", Intensity075, "warning", IntensityFull, AnimFadeIn, DureeScene, AnimFadeOut, PriorityScene
	End If
	If NbTilt > 4 And TimerTilt.Interval > 100 Then
		TimerTilt.Interval=99
	End If
End Sub

Sub TimerTilt_Timer() 'Test si on a dépassé le nombre de secousses dans le temps à tester
	TimerTilt.Enabled=False
	TimerTilt.Interval=3000
	Tilt=False
	If NbTilt > 4 Then
		Tilt=True
		BallSaving=false
		Playsound "tilt"
		DMDScene NomScene, "", Intensity075, "Tilt", IntensityFull, AnimFadeIn, DureeScene, AnimFadeOut, PriorityScene
		RightFlipper.RotateToStart
		RightFlipper001.RotateToStart
		LeftFlipper.RotateToStart
		FlipperHD.RotateToStart
		FlipperHG.RotateToStart
	End If
	NbTilt=0
End Sub

'____________________________________________________________________________________________________________________

'
' gestion de la loterie
'
'____________________________________________________________________________________________________________________

Sub Teaser_Initialisation(Action, Sequence, Duree)
	TimerTeaser.UserValue=Sequence
	TimerTeaser.Interval=Duree
	TimerTeaser.Enabled=Action
	
End Sub

Sub	TimerTeaser_Timer()
	'patch si on appuie juste au moment du message "Game Over"
	'qui peut éventuellement générer un bug
	If BIP > 0 Then
		TimerTeaser.Enabled=False
		Exit Sub
	End If
	Select Case TimerTeaser.UserValue
		Case 1000:
			DMDScene NomScene, "", Intensity075, "Best players", IntensityFull, AnimFadeIn, DureeScene, AnimFadeOut, PriorityScene
		Case 1001:
			DMDScene NomScene, "", Intensity075, "Player 5", IntensityFull, AnimFadeIn, DureeScene, AnimFadeOut, PriorityScene
		Case 1002:
			DMDScene NomScene, HSName(5), Intensity075, HSScore(5), IntensityFull, AnimFadeIn, DureeScene, AnimFadeOut, PriorityScene
		Case 1003:
			DMDScene NomScene, "", Intensity075, "Player 4", IntensityFull, AnimFadeIn, DureeScene, AnimFadeOut, PriorityScene
		Case 1004:
			DMDScene NomScene, HSName(4), Intensity075, HSScore(4), IntensityFull, AnimFadeIn, DureeScene, AnimFadeOut, PriorityScene
		Case 1005:
			DMDScene NomScene, "", Intensity075, "Player 3", IntensityFull, AnimFadeIn, DureeScene, AnimFadeOut, PriorityScene
		Case 1006:
			DMDScene NomScene, HSName(3), Intensity075, HSScore(3), IntensityFull, AnimFadeIn, DureeScene, AnimFadeOut, PriorityScene
		Case 1007:
			DMDScene NomScene, "", Intensity075, "Player 2", IntensityFull, AnimFadeIn, DureeScene, AnimFadeOut, PriorityScene
		Case 1008:
			DMDScene NomScene, HSName(2), Intensity075, HSScore(2), IntensityFull, AnimFadeIn, DureeScene, AnimFadeOut, PriorityScene
		Case 1009:
			DMDScene NomScene, "", Intensity075, "Champion", IntensityFull, AnimFadeIn, DureeScene, AnimFadeOut, PriorityScene
		Case 1010:
			DMDScene NomScene, HSName(1), Intensity075, HSScore(1), IntensityFull, AnimFadeIn, DureeScene, AnimFadeOut, PriorityScene
		Case 1011:
			DMDScene NomScene, "", Intensity075, " Loop Champion", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1012:
			DMDScene NomScene, "", Intensity075,  HLName & " " & HLNumber & " loops", IntensityFull, AnimFadeIn, DureeScene, AnimFadeOut, PriorityScene
		Case 1013:
			DMDScene NomScene, "One coin for", Intensity075, BallesParPartie & " balls", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1014:
			DMDScene NomScene, "Light BIRD", Intensity075, "add X multipler", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1015:
			DMDScene NomScene, "Hit RAT taget", Intensity075, "", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1016:
			DMDScene NomScene, "fly through", Intensity075, "Central Ramp", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1017:
			DMDScene NomScene, "and run", Intensity075, "Rat Attack", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1018:
			DMDScene NomScene, "Hit " & TargetRatAttackRaw+1 & " Rat targets", IntensityFull, "in a second", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1019:
			DMDScene NomScene, "for Rat Attack", Intensity075, "bonus", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1020:
			DMDScene NomScene, "Fill " & NombreLignesCat & " cat lines", Intensity075, "gives", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1021:
			DMDScene NomScene, "", Intensity075, "Extra Ball", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1022:
			DMDScene NomScene, "Hit", Intensity075, "captive ball", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1023:
			DMDScene NomScene, "gives timed X2", Intensity075, "score", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1024:
			DMDScene NomScene, "Hit", Intensity075, "floating targets", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1025:
			DMDScene NomScene, "and bird", Intensity075, "will reach nest", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1026:
			DMDScene NomScene, "Lock the ball", Intensity075, "under nest", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1027:
			DMDScene NomScene, "for", Intensity075, "Multiball", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1028:
			DMDScene NomScene, "Bird Fly", Intensity075, "", IntensityFull, AnimToLeft, DureeScene, AnimToRight, PriorityScene
		Case 1029:
			DMDScene NomScene, "Original Pinball", Intensity075, "", IntensityFull, AnimToLeft, DureeScene, AnimToRight, PriorityScene
		Case 1030:
			DMDScene NomScene, "on", Intensity075, "Visual pinball", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1031:
			DMDScene NomScene, "1st replay for", Intensity075, ScoreToWin1 & " points", IntensityFull, AnimUp, DureeScene, NoAnim, PriorityScene
		Case 1032:
			DMDScene NomScene, "2nd replay for", Intensity075, ScoreToWin2 & " points", IntensityFull, AnimUp, DureeScene, NoAnim, PriorityScene
		Case 1033:
			DMDScene NomScene, "3rd replay for", Intensity075, ScoreToWin3 & " points", IntensityFull, AnimUp, DureeScene, NoAnim, PriorityScene
		Case 1034:
			DMDScene NomScene, "1 to "& NbJoueursParPartie & " players", Intensity075, "can play", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1035:
			If NbCredits=0 Then
				DMDScene NomScene, "Please", Intensity075, "Insert coins", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
				Playsound "CoinIn5"
			Else
				DMDScene NomScene, NbCredits & " Credits", Intensity075, "available", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
			End If
		Case 1036:
			DMDScene NomScene, "", Intensity075, "All sounds", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1037:
			DMDScene NomScene, "", Intensity075, "lasonotheque.org", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1038:
			DMDScene NomScene, "programming", Intensity075, "Bird Fly", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1039:
			DMDScene NomScene, "", Intensity075, "Loloallo", IntensityFull, AnimToLeft, DureeScene, AnimToRight, PriorityScene
		Case 1040:
			DMDScene NomScene, "", Intensity075, "Spring 2022", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1041:
			DMDScene NomScene, "Pinball is only", Intensity075, "for Fun", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1042:
			DMDScene NomScene, "Please", Intensity075, "Add Player", IntensityFull, NoAnim, DureeScene, NoAnim, PriorityScene
		Case 1043:
			'On repart sur la boucle
			TimerTeaser.UserValue=999
	End Select
	TimerTeaser.UserValue=TimerTeaser.UserValue+1
End Sub

