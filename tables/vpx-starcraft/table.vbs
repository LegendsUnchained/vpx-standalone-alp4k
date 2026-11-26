'     ___| __ __|   \      _ \    ___|   _ \      \     ____| __ __|
'   \___ \    |    _ \    |   |  |      |   |    _ \    |        |
'         |   |   ___ \   __ <   |      __ <    ___ \   __|      |
'   _____/   _| _/    _\ _| \_\ \____| _| \_\ _/    _\ _|       _|  v1.2

'A RESKIN OF...
'*******************************************************
'SEGA PINBALL INC
'STARSHIP TROOPERS 1997
'*******************************************************
' Design by:	Joe Balcer, Joe Kaminkow
' Art by:		Morgan Weistling
' Music by:		Brian Schmidt
' Sound by:		Brian Schmidt
' Software by:	Neil Falconer, Orin Day
'*******************************************************


'*******************************************************
'*******************************************************
' v1.0 STARCRAFT RESKIN by cheese3075; Pup by gman. Released Oct-2026.
'      Three main playfields / Aprons / Pups have been created, one for each race.  Each race looks like a new game.  Rules were not changed: Kill required
'      enemies to move onto the next world/map!
'      * Planet/Map 1: Terran vs Zerg
'      * Planet/Map 2: Terran vs Protoss
'      * Planet/Map 3: Protoss vs Zerg
'      * Planet/Map 4: Protoss vs Terran
'      * Planet/Map 5: Zerg vs Protoss
'      * Planet/Map 6: Zerg vs Terran
'      Victory screens after completing each.
'      Mine resources by hitting slings to complete buildings and farms/depots/overloards. Plus many surprises along the way!  Enjoy!
'v1.01 Ramp alignment fixed
'v1.02 Added low graphics option in f12 menu and optimized file sizes and code
'v1.10 RACE SWAP WITH RIGHT MAGNA SAVE! (only active on first launch of the table, before the game is started).  Added color accents for Protoss and Zerg races into playfields.
'      Added GI and ramp colors for Protoss \ Zerg races, these can be turned off in the f12 menu.
'v1.20 Fixed major bug with lights on sequential playfields do not work.  Fixed starting race bug with v1.1 code.  Adjusted race symbols on planets to be a bit more dim. 
'*******************************************************
'*******************************************************


'*******************************************************
' StarShip Troopers recreated for Visual Pinball by Knorr and Clark Kent.
'
' Updated by VPW v2.0 Mar-2023
'   Eighties8: Complete rescale and nFozzy physics rebuild, Fleep sounds
'   TastyWasps: VR room integration
'	iaakki: Physics, lighting, and animation support
'   Aubrel: Lighting adjustments
'   Sixtoe: Table level and target animation fixes
'   apophis: Dynamic shadows, some visual, sound, and physics support
'   leojreimroc and HauntFreaks: VR backglass
'*******************************************************

'*******************************************************
' Special Thanks to:
' JPSalas, Arngrim, Vinthar
' Toxie, Fuzzel, VPX Dev Team and SuperTilted
'*******************************************************

'*******************************************************
' v1.0		First Release by Knorr and Clark Kent
'*******************************************************
' v1.1 		fixed left kicker
'			added missing post
'			slowed down orbit
'*******************************************************
' v2.0   - VPW Mod
' 001 e8 - Rescaled the whole project to match the current understanding of physics
' 002 iaakki - Rescaled ramp heights, brainbug position and animations fixed, pf physics reworked, light bulb ball reflections fixed (sub addBallReflections), GI bulbs fixed
' 003 e8 - Refactored scale from 2.0 to 1.9
' 004 e8 - Added nfozzy flipper physics
' 005 e8 - Added nfozzy dampening physics code. Applied new physics material settings and added rubbers and primatives to playfield
' 006 e8 - Refactored scale to 20.25 and rescaled graphics to match, added latest slings implementation w correction functions
' 007 e8 - Updated slings to nfozzy and added correction scripts.Added zCol mat to ramps, Adjusted pops settings.
' 008 e8 - Fixed: slingshot sounds, autoplunger, sling geometries, Logo ball, subway drop height
' 009 e8 - Fixed: Brain bug position, flipper strength, pf friction, fixed drop targets Z, aligned ramps
' 010/14 e8 - Updated WarriorBug positions in script. Fixed ball block from subway wall under target area
' 015 e8 - Updated WB script values to match new scale base 222 steps. Synced WB origins to same x/y base. Realigned left ramp inner post/sleeve to match overlay. 
' 016 e8 - Replaced Wall022 at corner of WB target with rubber post.
' 018 e8 - Fixed wall heights around ramps and warrior bug
' 019 Aubrel - Lights_BlurU, General_Illumination, FlasherCaps, Lights, Lights_Blur, flasherside layers
' 020 e8 - Fixed rubber post sizes broken since scaled-7 
' 021 e8 - Fixed sleeve positions and sizes around right side drops.
' 022 e8 - Fixed PFGI alignment, lowered flipper strength from 3000 to 2850 helps flipper tricks?
' 023 e8 - Fixed PFGI alignment again, rescaled playfield mesh to further precision 1.8485 (18.485) to fix PFGI alignment. Rescaled GIFL1 to match PF mesh
' 024 e8 - Added n/r physics to small flipper
' 025 e8 - Fixed subway floors and wall heights, added block above tanker beetle scoop, lowered flipper strength to 2500
' 026 e8 - More tweaks to subway to tighten geometries, removed * from layer names, fixed WP imageswap, replaced post with sleeve at left side DT bank
' 027-29 Aubrel - lighting adjustments to account for scaling
' 030 Aubrel - fixed side flashers hard edges on sidewalls. Added scripting to scale flasher effects
' 031 e8 - Added sub to enable blue "bugball" Const BugBall = 1
' 032 e8 - Reset glass height back to 400 to try fix cabinent POV
' 033 e8 - Added LockBarKey to control right mini flipper
' 034 e8 - Added Fleep Sounds, fixed position of PFGI bulbs!
' 035 e8 - Added Shooter Groove to help with autoplunge alignment (Thx Uncle_Paulie). Adjusted plunger release speed. Replaced more stock sounds with fleep.
' 036 e8 - Fixed spinner Z values! added Fleep SoundSpinner for ramp spinners.Added small flipper fleep sound and collide
' 037 e8 - Added WB & BB switches to Fleep Targets. Added all zCol Posts & Sleeves to Fleep Rubbers 
' 038 e8 - Changed PF friction from .25 to .15 (thanks Apophis)
' 039 e8 - Increased difficulty 20->50, flipper strength 2250->3125, flipper length 146.25->147.5, mini flip strength++ Reset target falloffs to 0, adjusted kicker actions
' 041 e8 - Added Toys Visibility option and Hopper bug alignment option (need to remove bolt from prim)
' 042 e8 - Removed the hex_bolt that sits atop the hopper bug (vpx export->blender import->blender export->vpx insert prim
' 043 Sixtoe - Fixed target hit threshold and prim position, fixed right outlane issue, fixed metal fixture prim wrong position, fixed plunger lane guide size, fixed broken target animation and timers
' 044 e8 - Enabled plastic ramp sounds (no fleep equivalent). Fixed brainbug wall height to prevent ball locks
' 045 TastyWasps - Added hybrid minimal VR Room integration.
' 046 iaakki - VR ramp material tweaks and z-fighting fixes. All changes are around UpdateGi sub..
' 047 TastyWasps - Added Sphere VR Room as default.  Thanks RajoJoey!
' 048 apophis - Added dynamic shadows. Added kick to ball if on top of brainbug when it raises. Updated ball image an ball scratches. Updated environmental image and emission scale. Adjusted flipper DL with GI state. 
' 049 apophis - Added flipper mod (thanks tomate!)
' RC1 apophis - Reworked ramp rolling sounds. Sound effect file cleanup. Locked all objects.
' RC2 TastyWasps - VR Backglass material and disable lighting settings tweaked. BrainBugCage normal map removed.  AO disabled by default.
' RC3 TastyWasps - VR flippers animated. VR Cabinet shiny factor reduced. Platform for sphere adjusted.
' RC4 apophis -  minor sleeve position adjustment near scoop. Set Wall014 physics material to MetalFO. Reduced WireRamp_Stop volume. Fixed top two bumpers height scale. Top comment block cleanup.
' RC5 apophis - Added inlane slowdown code. Reduced BumperSoundFactor to 3 and VolumeDial to 0.5. Updated desktop background default image (thanks PSD). Updated kicker angles (thanks tomate). Added some kicker randomness.
' v2.0 StarshipTrooper - VPW Release
'*******************************************************
' v1.0 STARCRAFT RESKIN by cheese3075; Pup by gman. Released Oct-2026.
'      Three main playfields / Aprons / Pups have been created, one for each race.  Each race looks like a new game.  Rules were not changed: Kill required
'      enemies to move onto the next world/map!
'      * Planet/Map 1: Terran vs Zerg
'      * Planet/Map 2: Terran vs Protoss
'      * Planet/Map 3: Protoss vs Zerg
'      * Planet/Map 4: Protoss vs Terran
'      * Planet/Map 5: Zerg vs Protoss
'      * Planet/Map 6: Zerg vs Terran
'      Victory screens after completing each.
'      Mine resources by hitting slings to complete buildings and farms/depots/overloards. Plus many surprises along the way!  Enjoy!
'v1.01 Ramp alignment fixed
'v1.02 Added low graphics option in f12 menu and optimized file sizes and code
'v1.10 RACE SWAP WITH RIGHT MAGNA SAVE! (only active on first launch of the table, before the game is started).  Added color accents for Protoss and Zerg races into playfields.
'      Added GI and ramp colors for Protoss \ Zerg races, these can be turned off in the f12 menu.
'v1.20 Fixed major bug with lights on sequential playfields do not work.  Fixed starting race bug with v1.1 code.  Adjusted race symbols on planets to be a bit more dim.
'*******************************************************
'*******************************************************

Option Explicit
Randomize

'****** PuP Variables ******

Dim usePUP: Dim cPuPPack: Dim PuPlayer: Dim PUPStatus: PUPStatus=false ' dont edit this line!!!

'*************************** PuP Settings for this table ********************************

usePUP   = true               ' enable Pinup Player functions for this table
cPuPPack = "startrp2sc"    ' name of the PuP-Pack / PuPVideos folder for this table

'//////////////////// PINUP PLAYER: STARTUP & CONTROL SECTION //////////////////////////

' This is used for the startup and control of Pinup Player

Sub PuPStart(cPuPPack)
    If PUPStatus=true then Exit Sub
    If usePUP=true then
        Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")
        If PuPlayer is Nothing Then
            usePUP=false
            PUPStatus=false
        Else
            PuPlayer.B2SInit "",cPuPPack 'start the Pup-Pack
            PUPStatus=true
        End If
    End If
End Sub

Sub pupevent(EventNum)
    if (usePUP=false or PUPStatus=false) then Exit Sub
    PuPlayer.B2SData "E"&EventNum,1  'send event to Pup-Pack
End Sub

'************ PuP-Pack Startup **************

'Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")   
'PuPlayer.B2SInit "", cPuPPack


PuPStart(cPuPPack) 'Check for PuP - If found, then start Pinup Player / PuP-Pack
'pupevent xxx



' --------------------- USER OPTIONS ---------------------

'///////////////////////-----General Sound Options-----///////////////////////
'// VolumeDial:
'// VolumeDial is the actual global volume multiplier for the mechanical sounds.
'// Values smaller than 1 will decrease mechanical sounds volume.
'// Recommended values should be no greater than 1.
Const VolumeDial = 0.5
Const BallRollVolume = 0.5		    'Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume = 0.4		    'Level of ramp rolling volume. Value between 0 and 1

'///////////////////////-----Toys Visibility-----///////////////////////
'// Toys Visibility:
'// Can help with visibility of targets on the playfield
Const HideToys = 0 					'Set to 1 to hide ALL toys over playfield


'///////////////////////-----Shadow Options-----///////////////////////
Const DynamicBallShadowsOn = 0		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 0		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									'2 = flasher image shadow, but it moves like ninuzzu's


' ------------------ END OF USER OPTIONS --------------------


If HideToys = 1 then 
	Hopper_bracket.visible = 0
end if

'----- VR Room Auto-Detect -----
Dim VR_Room_Choice, VR_Obj			

VR_Room_Choice = 1		' 1 = Star Sphere Room, 2 = Minimal Room			  

If RenderingMode = 2 Then
	If VR_Room_Choice = 1 Then
		For Each VR_Obj in Sphere: VR_Obj.Visible = 1 : Next
		For Each VR_Obj in VR_Minimal : VR_Obj.Visible = 0 : Next
	Else
		For Each VR_Obj in Sphere: VR_Obj.Visible = 0 : Next
		For Each VR_Obj in VR_Minimal : VR_Obj.Visible = 1 : Next
	End If
Else
	For Each VR_Obj in Sphere: VR_Obj.Visible = 0 : Next
	For Each VR_Obj in VR_Minimal : VR_Obj.Visible = 0 : Next
End If

Dim VarHidden, UseVPMColoredDMD
If Table1.ShowDT = true then
    UseVPMColoredDMD = True
    VarHidden = 1
else
    UseVPMColoredDMD = False
    VarHidden = 0
end if


Const BallMass = 1
Const BallSize = 50


On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the Controller.vbs file in order to run this table (installed with the VPX package in the scripts folder)"
On Error Goto 0

LoadVPM "01560000", "sega.vbs", 3.57

Const UseSolenoids = 15
Const UseLamps = 0
Const UseSync = 0
Const HandleMech = 0

'' special fastflips
'Const cSingleLFlip = 0
'Const cSingleRFlip = 0

' Standard Sounds
Const SSolenoidOn = "fx_solenoidon"
Const SSolenoidOff = "fx_solenoidoff"
Const SCoin = ""



'***ROM***ROM***ROM***ROM***

'Const cGameName = "startrp2"   'Do not uncomment
Const cGameName = "startrp2sc"  'add the following line to \vPinball\VisualPinball\VPinMAME\VPMAlias.txt.        startrp2sc, startrp2

'***************************

Dim bsTrough, mBug, bsSuperVUK, bsLeftVUK, FlippersEnabled, mLeftMagnet, mRightMagnet


Set GICallback = GetRef("UpdateGI")


'************
' Table Init 
'************

Sub Table1_Init
    vpmInit Me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "StarCraft - Sega 1997" & vbNewLine & "Starship Troopers Reskin"
        .Games(cGameName).Settings.Value("rol") = 1 'set it to 1 to rotate the DMD to the left
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = VarHidden
		.Games("startrp2").Settings.Value("sound") = 0
		.Games("startrp2").Settings.Value("samples") = 0
        '.SetDisplayPosition 0, 0, GetPlayerHWnd 'uncomment this line if you don't see the DMD
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
'        .Switch(22) = 1 'close coin door
'        .Switch(24) = 1 'and keep it close
		' Main Timer init
		PinMAMETimer.Interval = PinMAMEInterval
		PinMAMETimer.Enabled = 1
		RealTime.Enabled = 1
		' Nudging
		vpmNudge.TiltSwitch = 56
		vpmNudge.Sensitivity = 2
		vpmNudge.TiltObj = Array(bumper1, bumper2, bumper3, Leftslingshot, Rightslingshot)
	End With

    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 0, 15, 14, 13, 12, 0, 0, 0
        .InitKick BallRelease, 80, 11
        '.InitExitSnd SoundFX("fx_ballrelease", DOFContactors), SoundFX("fx_solenoid", DOFContactors)
        .Balls = 4
        .IsTrough = 1
    End With


    Set bsSuperVUK = New cvpmBallStack
    With bsSuperVUK
        .InitSw 0, 46, 0, 0, 0, 0, 0, 0
        .InitKick sw46, 182, 72 '42
        '.InitExitSnd SoundFX("fx_supervuk", DOFContactors), SoundFX("fx_solenoid", DOFContactors)
        .KickZ = 1.15
		.KickAngleVar = 5
		.KickForceVar = 3
    End With


    Set bsLeftVUK = New cvpmSaucer
    With bsLeftVUK
        .InitKicker sw45, 45, 131, 15, 15
        .InitExitVariance 2, 1
        '.InitSounds "fx_safehousehit", "fx_solenoid", SoundFX("fx_leftvuk",DOFContactors)
        .CreateEvents "bsLeftVUK", sw45
    End With


    Set mLeftMagnet = New cvpmMagnet
    With mLeftMagnet
        .InitMagnet LeftMagnet, 30 
        .GrabCenter = 1
		.Solenoid = 5
        .CreateEvents "mLeftMagnet"
    End With

    Set mRightMagnet = New cvpmMagnet
    With mRightMagnet
        .InitMagnet RightMagnet, 30
        .GrabCenter = 1
		.Solenoid = 6
        .CreateEvents "mRightMagnet"
    End With

'e8
    Set mBug = New cvpmMyMech
	With mBug
	' In VPX 10.7+ should use "vpmMechFourStepSol" which happens to be the same as setting both StepSol and TwoDirSol
		'.MType=vpmMechFourStepSol+vpmMechStopEnd+vpmMechLinear+vpmMechFast
		.MType=vpmMechStepSol+vpmMechTwoDirSol+vpmMechStopEnd+vpmMechLinear+vpmMechFast
		.Sol1=17
		.Length=600 '150 'Time duration for mech trip
		.Steps=222 '222 '120 'primitive trans -110 to +10 'scaled prim trans -110 to +93.5 + 18.5 = 222
		.AddSw 34,0,1 'Add switch 34, start 0 end 1
		.AddSw 33,221,222 '33,119,120
		.Callback=GetRef("UpdateWarriorBug")
		.Start
	End With

 	sw38.IsDropped=1
 	sw38a.IsDropped=1


	for each xx in BWalls:xx.isDropped = 1:Next

	PuPInit

'**********************
'***DesktopMode Init***
'**********************

If Table1.ShowDT = False then
'Cabinet mode
		KorpusFs_inside.visible = true
		f125.rotx = -22 : f125.height = 425
		f126.rotx = -22 : f126.height = 425
		f127.rotx = -22 : f127.height = 425
		f128.rotx = -22 : f128.height = 425
	Else
'Desktop mode
		KorpusFS_inside.visible = false
		f125.rotx = -7.45 : f125.height = 296
		f126.rotx = -7.45 : f126.height = 296
		f127.rotx = -7.45 : f127.height = 296
		f128.rotx = -7.45 : f128.height = 296
	End if


 
End Sub


'**********************
'***Ball Release***
'**********************

Sub BallRelease_UnHit(): MakeBugBalls : RandomSoundBallRelease BallRelease : End Sub


'**********************
'***Bugballs***
'**********************

Sub MakeBugBalls()
	If BugBall = 1 Then
		dim b
		for each b in getballs 
			b.Image="brainbug_nrm"
			b.FrontDecal = "Scratches"
		next
	End If
End Sub


Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_exit():Controller.Pause = False:Controller.Stop:End Sub


Sub TextboxTimer_Timer()
'	TextBox003.Text = WarriorBugAssembly004.transZ
'	TextBox003.Text = materialamount
'	TextBox001.Text = GIFL1.opacity
'	TextBox003.Text = plunger.position
'	TextBox003.Text = fadestepb

'	MassboxL.text = Leftflipper.Mass
'	MassboxR.text = Rightflipper.Mass
End Sub




'************************************
' Game timer for real time updates
'************************************

Dim bPlanet1
Dim bPlanet2
Dim bPlanet3
Dim bPlanet4
Dim bPlanet5
Dim bPlanet6
Dim bBallInMotion
Dim bVictory
Dim RaceTerran
Dim RaceProtoss
Dim RaceZerg
Dim StartingRace
Dim RaceChangeVar: RaceChangeVar=1
Dim RaceLoop: RaceLoop=0

Sub RealTime_Timer
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
    RollingUpdate
    For each BrainBugStuff in BrainBugGroup: BrainBugStuff.TransY = BrainBugFlipper.CurrentAngle * 2:next

    GateP_plungerlane.rotx = Gate_plungerlane.currentangle +25
    GateP_RightRamp.rotx = Gate_rightramp.currentangle +25
    sw25p.rotx = sw25spinner.currentangle
    sw52p.rotx = sw52spinner.currentangle
    sw53p.rotx = sw53spinner.currentangle


    LeftFlipperP.objrotz = LeftFlipper.CurrentAngle
    RightFlipperP.objrotz = RightFlipper.CurrentAngle
    RightFlipperPS.objrotz = RightFlipperS.CurrentAngle
    FlipperLShadow.RotZ = LeftFlipper.CurrentAngle
    FlipperRShadow.RotZ = RightFlipper.CurrentAngle
    FlipperRsShadow.RotZ = RightFlipperS.CurrentAngle

	PlungerP.TransZ = Plunger.position-((plunger.position-4)*3)

'	*****FlipperScript****

	if fadeb = 0 and fadestepB > 0 then fadestepB = fadestepB -fadespeeddown
	if fadeb = 1 And FadeStepB < 999 then fadestepB = fadestepB +fadespeedup
	MaterialStepB = FadestepB/1000
	UpdateMaterial "FlasherCapsLitBlue",0,1,1,1,materialstepB,materialstepB,materialstepB,RGB(255,255,255),0,0,False,True,0,0,0,0

	if fader = 0 and fadestepR > 0 then fadestepR = fadestepR -fadespeeddown
	if fader = 1 And FadeStepR < 999 then fadestepR = fadestepR +fadespeedup
	MaterialStepR = FadestepR/1000
	UpdateMaterial "FlasherCapsLitRed",0,1,1,1,materialstepR,materialstepR,materialstepR,RGB(255,255,255),0,0,False,True,0,0,0,0

	if fadey = 0 and fadestepY > 0 then fadestepY = fadestepY -fadespeeddown
	if fadey = 1 And FadeStepY < 999 then fadestepY = fadestepY +fadespeedup
	MaterialStepY = FadestepY/1000
	UpdateMaterial "FlasherCapsLitYellow",0,1,1,1,materialstepY,materialstepY,materialstepY,RGB(255,255,255),0,0,False,True,0,0,0,0

	if fadeG = 0 and fadestepG > 0 then fadestepG = fadestepG -fadespeeddown
	if fadeG = 1 And FadeStepG < 999 then fadestepG = fadestepG +fadespeedup
	MaterialStepG = FadestepG/1000
	UpdateMaterial "FlasherCapsLitGreen",0,1,1,1,materialstepG,materialstepG,materialstepG,RGB(255,255,255),0,0,False,True,0,0,0,0

'TextBox001.Text = RaceTerran & RaceProtoss & RaceZerg 'TESTING

'************************************
' Planet Mode Changes - OLD LOGIC, REPLACED BELOW.
'************************************
'PLANET 2 --Terran v Protoss--
'	If l57.IntensityScale >0.1 and l58.IntensityScale >0.1 and l59.IntensityScale >0.1 and l61.IntensityScale=0 and l60.IntensityScale=0 and l63.IntensityScale=0 and l62.IntensityScale=0 and decals_apronP.Material="Plastic_decals" then               'Green 7
'		If pf_ProtossVsZerg.visible = 0 then   'First and only time hitting this
'		End If
'	End If
'PLANET 3 --Protoss v Zerg--
'	If l66.IntensityScale >0.1 and l65.IntensityScale >0.1 and l67.IntensityScale >0.1 and l68.IntensityScale >0.1 and l71.IntensityScale >0.1 and l69.IntensityScale=0 and l70.IntensityScale=0 and l62.IntensityScale >0.1 and l54.IntensityScale >0.1 and decals_apronP.Material="Plastic_decals" then   'Blue 3 PLUS Any green 5 & Any yellow 6
'		If pf_ProtossVsZerg.visible = 0 then   'First and only time hitting this
'		End If
'	End If
'PLANET 4 --Protoss v Terran--
'	If l73.IntensityScale >0.1 and l75.IntensityScale >0.1 and l76.IntensityScale >0.1 and l78.IntensityScale >0.1 and l79.IntensityScale >0.1 and l74.IntensityScale=0 and l77.IntensityScale=0 and l54.IntensityScale >0.1 and decals_apronP.Material="Plastic_decals" then   'Red 5 PLUS Any Yellow 9
'		If pf_ProtossVsTerran.visible = 0 then 'First and only time hitting this
'		End If
'	End If
'PLANET 5 --Zerg v Protoss--
'	If l73.IntensityScale >0.1 and l74.IntensityScale >0.1 and l75.IntensityScale >0.1 and l76.IntensityScale=0 and l77.IntensityScale=0 and l78.IntensityScale=0 and l79.IntensityScale=0 and l54.IntensityScale >0.1 and decals_apronZ.Material="Plastic_decals" then           'Red 7 PLUS Any Yellow 9
'		If pf_ZergVsProtoss.visible = 0 then   'First and only time hitting this
'		End If
'	End If
'PLANET 6 --Zerg v Terran--
'	If l73.IntensityScale >0.1 and l74.IntensityScale >0.1 and l75.IntensityScale >0.1 and l78.IntensityScale >0.1 and l79.IntensityScale >0.1 and l76.IntensityScale=0 and l77.IntensityScale=0 and l54.IntensityScale >0.1 and decals_apronZ2.Material = "Plastic_decals" then   'Red 9 PLUS Any Yellow 9
'		If pf_ZergVsProtoss.visible = 0 then   'First and only time hitting this
'		End If
'	End If
'End Sub

    'PLANET 1 (DEFAULT) Terran v Zerg  --  OR  --Protoss v Zerg--  OR  --Zerg v Protross--

	'PLANET 2 (DEFAULT) Terran v Protoss  --  OR  --Protoss v Terran--  OR  --Zerg v Terran--
	If l57.IntensityScale >0.1 and l58.IntensityScale >0.1 and l59.IntensityScale >0.1 and l61.IntensityScale=0 and l60.IntensityScale=0 and l63.IntensityScale=0 and l62.IntensityScale=0 and bPlanet1=true and bplanet2=false then   'Green 7
		bPlanet1 = False
		bPlanet2 = True
		bVictory = 	False
		VictoryScreenTimer.Enabled = False
		If RaceTerran = True Then  '*DEFAULT*
			pf_TerranVsProtoss.visible = 1
			pf_Victory_Terran_overlay.visible = 0
			pf_Victory_Terran2_overlay.visible = 0
			pupevent 701:pupevent 702
			SpeciesTracker.opacity = 125
			SpeciesTrackerTimer.Enabled = True
		Elseif RaceProtoss = True Then
			pf_ProtossVsZerg.visible = 0
			pf_ProtossVsTerran.visible = 1
			pf_Victory_Protoss_overlay.visible = 0
			pf_Victory_Protoss2_overlay.visible = 0
			pupevent 705:pupevent 706
		Elseif RaceZerg = True Then
			decals_apronZ.Material = "Plastic_decals_trans"
			decals_apronZ2.Material = "Plastic_decals"
			pf_ZergVsProtoss.visible = 0
			pf_ZergVsTerran.visible = 1
			pf_Victory_Zerg_overlay.visible = 0
			pf_Victory_Zerg2_overlay.visible = 0
			pupevent 709 : pupevent 710
		End If
	End If

	'PLANET 3 (DEFAULT) Protoss v Zerg --  OR  --Terran v Zerg--  OR  --Zerg v Protoss--
	If l66.IntensityScale >0.1 and l65.IntensityScale >0.1 and l67.IntensityScale >0.1 and l68.IntensityScale >0.1 and l71.IntensityScale >0.1 and l69.IntensityScale=0 and l70.IntensityScale=0 and l62.IntensityScale >0.1 and l54.IntensityScale >0.1 and bPlanet2=true and bplanet3=false then   'Blue 3 PLUS Any green 5 & Any yellow 6
		bPlanet2 = False
		bPlanet3 = True
		bVictory = 	False
		VictoryScreenTimer.Enabled = False
		TurnOffBuildings
		Building1RingTimer.Enabled = False 'Turns off lights
		Building2RingTimer.Enabled = False '
		Building3RingTimer.Enabled = False '
		BuildingRingsOff				   '
		FirstFarm = 0        	     	   'Reset variables
		SecondFarm = 0        	           '
		LastFarm = 0         	           '
		WaitingBuildingNext = 1            '
		KhaydarinCrystals = 0  		       'Resets resources
		VespeneGas = 0           	 	   '
		If RaceTerran = True and RaceProtoss = False and RaceZerg = False Then 'change to Protoss  *DEFAULT*
			RaceProtoss = True
			decals_apronT.Material = "Plastic_decals_trans"
			decals_apronZ.Material = "Plastic_decals_trans"
			decals_apronZ2.Material = "Plastic_decals_trans"
			decals_apronP.Material = "Plastic_decals"
			pf_TerranVsProtoss.visible = 0     'Turns off old playfield
			pf_ProtossVsZerg.visible = 1       'Turns on new playfield
			pf_Victory_Terran_overlay.visible = 0
			pf_Victory_Terran2_overlay.visible = 0
			SpeciesTracker.imageA = "Frame13"
			playsound "FX_SpeciesTrackerCompleted"
			SpeciesTrackerTimer.Enabled = True
			pylon1a.visible = 1        	       'Turns ON building
			Pylon1b.visible = 1        	       '
			pylon1c.visible = 1        	       '
			pylon2a.visible = 1        	       '
			Pylon2b.visible = 1        	       '
			pylon2c.visible = 1        	       '
			backglass.image = "backbox_on_p"   'Changes b2s on backbox
			If ColoredRamps = true Then 
				ChangeGi darkblue
				ChangeGiIntensity 2
				ramp_off.image = "ramps_off_mod_Protoss" 'Ramps to blue
				ramp_on.image = "ramps_on_mod_Protoss"
			End If
			walls_metal.image = "walls_metal_on_mod_blue"
			pupevent 703 : pupevent 704:pupevent 799
			SpeciesTracker.imageA = "Frame23"
			SpeciesTracker.opacity = 100
			SpeciesTrackerTimer.Enabled = True
		ElseIf RaceZerg = True and RaceTerran = False and RaceProtoss = False Then 'change to Terran
			RaceTerran = True
			decals_apronT.Material = "Plastic_decals"
			decals_apronP.Material = "Plastic_decals_trans"
			decals_apronZ.Material = "Plastic_decals_trans"
			decals_apronZ2.Material = "Plastic_decals_trans"
			pf_ZergVsTerran.visible = 0     'Turns off old playfield
			'pf_TerranVsZerg.visible = 1       'Always should be on
			pf_Victory_Zerg_overlay.visible = 0
			pf_Victory_Zerg2_overlay.visible = 0
			pupevent 701               'Terran overlay
			pupevent 783               'Terran Backglass restart
			pupevent 799               'Clear supply
			Turret1.visible = 1        'Turns ON Building
			Turret2.visible = 1        '
			backglass.image = "backbox_on_t"
			If ColoredRamps = true Then 
				ChangeGi white
				ChangeGiIntensity 1.2
				ramp_off.image = "ramp_off" 'normal
				ramp_on.image = "ramp_on"
			End If
			walls_metal.image = "walls_metal_on"
			JackpotCelebration
		ElseIf RaceProtoss = True and RaceTerran = False and RaceZerg = False Then 'change to Zerg
			RaceZerg = True
			decals_apronT.Material = "Plastic_decals_trans"
			decals_apronP.Material = "Plastic_decals_trans"
			decals_apronZ.Material = "Plastic_decals"
			pf_ProtossVsTerran.visible = 0     'Turns off old playfield
			pf_ZergVsProtoss.visible = 1       'Turns on new playfield
			pf_Victory_Protoss_overlay.visible = 0
			pf_Victory_Protoss2_overlay.visible = 0
			Overlord1.visible = 1        	   'Turns ON building
			Overlord2.visible = 1        	   '
			backglass.image = "backbox_on_z"   'Changes b2s on backbox
			If ColoredRamps = true Then 
				ChangeGi purple
				ChangeGiIntensity 1.2
				ramp_off.image = "ramps_off_mod_Zerg" 'Ramps to purple
				ramp_on.image = "ramps_on_mod_Zerg"
			End If
			walls_metal.image = "walls_metal_on_mod_purple"
			pupevent 707 : pupevent 708:pupevent 799
		End If
		If decals_apronT.Material = "Plastic_decals" Then 'Turn off old race
			RaceZerg = False
		End If
		If decals_apronP.Material = "Plastic_decals" Then 'Turn off old race
			RaceTerran = False
		End If
		If decals_apronZ.Material = "Plastic_decals" Then 'Turn off old race
			RaceProtoss = False
		End If
		UpdatePupVariables() 
	End If

	'PLANET 4 (DEFAULT) Protoss v Terran  --  OR  --Zerg v Terran--  OR  --Terran v Protoss--
	If l73.IntensityScale >0.1 and l75.IntensityScale >0.1 and l76.IntensityScale >0.1 and l78.IntensityScale >0.1 and l79.IntensityScale >0.1 and l74.IntensityScale=0 and l77.IntensityScale=0 and l54.IntensityScale >0.1 and bPlanet3=true and bplanet4=false then   'Red 5 PLUS Any Yellow 9
		bPlanet3 = False
		bPlanet4 = True
		bVictory = 	False
		VictoryScreenTimer.Enabled = False
		If RaceTerran = True Then
			pf_TerranVsProtoss.visible = 1
			pf_Victory_Terran_overlay.visible = 0
			pf_Victory_Terran2_overlay.visible = 0
			pupevent 701:pupevent 702
		Elseif RaceProtoss = True Then  '*DEFAULT*
			pf_ProtossVsZerg.visible = 0
			pf_ProtossVsTerran.visible = 1
			pf_Victory_Protoss_overlay.visible = 0
			pf_Victory_Protoss2_overlay.visible = 0
			pupevent 705:pupevent 706
			SpeciesTracker.opacity = 125
			SpeciesTrackerTimer.Enabled = True
		Elseif RaceZerg = True Then
			decals_apronZ.Material = "Plastic_decals_trans"
			decals_apronZ2.Material = "Plastic_decals"
			pf_ZergVsProtoss.visible = 0
			pf_ZergVsTerran.visible = 1
			pf_Victory_Zerg_overlay.visible = 0
			pf_Victory_Zerg2_overlay.visible = 0
			pupevent 709 : pupevent 710
		End If
	End If

	'PLANET 5 (DEFAULT) Zerg v Protoss  --  OR  --Protoss v Zerg--  OR  --Terran v Zerg--
	If l73.IntensityScale >0.1 and l74.IntensityScale >0.1 and l75.IntensityScale >0.1 and l76.IntensityScale=0 and l77.IntensityScale=0 and l78.IntensityScale=0 and l79.IntensityScale=0 and l54.IntensityScale >0.1 and bPlanet4=true and bplanet5=false then           'Red 7 PLUS Any Yellow 9
		bPlanet4 = False
		bPlanet5 = True
		bVictory = 	False
		VictoryScreenTimer.Enabled = False
		TurnOffBuildings
		Building1RingTimer.Enabled = False 'Turns off lights
		Building2RingTimer.Enabled = False '
		Building3RingTimer.Enabled = False '
		BuildingRingsOff				   '
		FirstFarm = 0        	     	   'Reset variables
		SecondFarm = 0        	           '
		LastFarm = 0         	           '
		WaitingBuildingNext = 1            '
		KhaydarinCrystals = 0              'Resets resources
		VespeneGas = 0                     '
		If RaceTerran = True and RaceProtoss = False and RaceZerg = False Then 'change to Protoss
			RaceProtoss = True
			decals_apronT.Material = "Plastic_decals_trans"
			decals_apronZ.Material = "Plastic_decals_trans"
			decals_apronZ2.Material = "Plastic_decals_trans"
			decals_apronP.Material = "Plastic_decals"
			pf_TerranVsProtoss.visible = 0     'Turns off old playfield
			pf_ProtossVsZerg.visible = 1       'Turns on new playfield
			pf_Victory_Terran_overlay.visible = 0
			pf_Victory_Terran2_overlay.visible = 0
			pylon1a.visible = 1        	       'Turns ON building
			Pylon1b.visible = 1        	       '
			pylon1c.visible = 1        	       '
			pylon2a.visible = 1        	       '
			Pylon2b.visible = 1        	       '
			pylon2c.visible = 1        	       '
			backglass.image = "backbox_on_p"   'Changes b2s on backbox
			If ColoredRamps = true Then 
				ChangeGi darkblue
				ChangeGiIntensity 2
				ramp_off.image = "ramps_off_mod_Protoss" 'Ramps to blue
				ramp_on.image = "ramps_on_mod_Protoss"
			End If
			walls_metal.image = "walls_metal_on_mod_blue"
			pupevent 703 : pupevent 704:pupevent 799
		ElseIf RaceZerg = True and RaceTerran = False and RaceProtoss = False Then 'change to Terran
			RaceTerran = True
			decals_apronT.Material = "Plastic_decals"
			decals_apronP.Material = "Plastic_decals_trans"
			decals_apronZ.Material = "Plastic_decals_trans"
			decals_apronZ2.Material = "Plastic_decals_trans"
			pf_ZergVsTerran.visible = 0     'Turns off old playfield
			'pf_TerranVsZerg.visible = 1       'Always should be on
			pf_Victory_Zerg_overlay.visible = 0
			pf_Victory_Zerg2_overlay.visible = 0
			pupevent 701               'Terran overlay
			pupevent 783               'Terran Backglass restart
			pupevent 799               'Clear supply
			Turret1.visible = 1        'Turns ON Building
			Turret2.visible = 1        '
			backglass.image = "backbox_on_t"
			If ColoredRamps = true Then 
				ChangeGi white
				ChangeGiIntensity 1.2
				ramp_off.image = "ramp_off" 'normal
				ramp_on.image = "ramp_on"
			End If
			walls_metal.image = "walls_metal_on"
			JackpotCelebration
		ElseIf RaceProtoss = True and RaceTerran = False and RaceZerg = False Then 'change to Zerg  *DEFAULT*
			RaceZerg = True
			decals_apronT.Material = "Plastic_decals_trans"
			decals_apronP.Material = "Plastic_decals_trans"
			decals_apronZ.Material = "Plastic_decals"
			pf_ProtossVsTerran.visible = 0     'Turns off old playfield
			pf_ZergVsProtoss.visible = 1       'Turns on new playfield
			pf_Victory_Protoss_overlay.visible = 0
			pf_Victory_Protoss2_overlay.visible = 0
			Overlord1.visible = 1        	   'Turns ON building
			Overlord2.visible = 1        	   '
			backglass.image = "backbox_on_z"   'Changes b2s on backbox
			If ColoredRamps = true Then 
				ChangeGi purple
				ChangeGiIntensity 1.2
				ramp_off.image = "ramps_off_mod_Zerg" 'Ramps to purple
				ramp_on.image = "ramps_on_mod_Zerg"
			End If
			walls_metal.image = "walls_metal_on_mod_purple"
			pupevent 707 : pupevent 708:pupevent 799
			SpeciesTracker.imageA = "Frame15"
			playsound "FX_SpeciesTrackerCompleted"
			SpeciesTrackerTimer.Enabled = True
			SpeciesTracker.imageA = "Frame25"
			SpeciesTracker.opacity = 100
			SpeciesTrackerTimer.Enabled = True
		End If
		If decals_apronT.Material = "Plastic_decals" Then 'Turn off old race
			RaceZerg = False
		End If
		If decals_apronP.Material = "Plastic_decals" Then 'Turn off old race
			RaceTerran = False
		End If
		If decals_apronZ.Material = "Plastic_decals" Then 'Turn off old race
			RaceProtoss = False
		End If
		UpdatePupVariables() 
	End If

	'PLANET 6 (DEFAULT) Zerg v Terran  --  OR  --Protoss v Terran--  OR  --Terran v Protoss--
	If L73.IntensityScale >0.1 and L74.IntensityScale >0.1 and L75.IntensityScale >0.1 and L78.IntensityScale >0.1 and L79.IntensityScale >0.1 and L76.IntensityScale=0 and L77.IntensityScale=0 and L54.IntensityScale >0.1 and bPlanet5=true and bplanet6=false then  'Red 9 PLUS Any Yellow 9
		bPlanet5 = False
		bPlanet6 = True
		bVictory = 	False
		VictoryScreenTimer.Enabled = False
		If RaceTerran = True Then
			pf_TerranVsProtoss.visible = 1
			pf_Victory_Terran_overlay.visible = 0
			pf_Victory_Terran2_overlay.visible = 0
			pupevent 701:pupevent 702
		Elseif RaceProtoss = True Then
			pf_ProtossVsZerg.visible = 0
			pf_ProtossVsTerran.visible = 1
			pf_Victory_Protoss_overlay.visible = 0
			pf_Victory_Protoss2_overlay.visible = 0
			pupevent 705:pupevent 706
		Elseif RaceZerg = True Then  '*DEFAULT*
			decals_apronZ.Material = "Plastic_decals_trans"
			decals_apronZ2.Material = "Plastic_decals"
			pf_ZergVsProtoss.visible = 0
			pf_ZergVsTerran.visible = 1
			pf_Victory_Zerg_overlay.visible = 0
			pf_Victory_Zerg2_overlay.visible = 0
			pupevent 709 : pupevent 710
			SpeciesTracker.opacity = 125
			SpeciesTrackerTimer.Enabled = True
		End If
	End If

'************************************
' Victory screens
'************************************
	If L49.IntensityScale >0.1 and L50.IntensityScale >0.1 and L51.IntensityScale >0.1 and L52.IntensityScale >0.1 and L53.IntensityScale >0.1 and L54.IntensityScale >0.1 and L55.IntensityScale=0 and L57.IntensityScale >0.1 and L58.IntensityScale >0.1 and L59.IntensityScale >0.1 and L60.IntensityScale >0.1 and L61.IntensityScale >0.1 and L62.IntensityScale >0 and L63.IntensityScale=0 and L65.IntensityScale >0.1 and L66.IntensityScale >0.1 and L67.IntensityScale >0.1 and L68.IntensityScale >0.1 and L69.IntensityScale >0.1 and L70.IntensityScale >0.1 and L71.IntensityScale=0 and L73.IntensityScale >0.1 and L74.IntensityScale >0.1 and L75.IntensityScale >0.1 and L76.IntensityScale >0.1 and L77.IntensityScale >0.1 and L78.IntensityScale >0.1 and L79.IntensityScale=0 and bVictory = false then
		if RaceTerran = True Then 'Terran planets
			pf_Victory_Terran_overlay.visible = 1
			pf_Victory_Terran_overlay.opacity = 100
			bVictory = 	True
		Elseif RaceProtoss = True Then 'Protoss planets
			pf_Victory_Protoss_overlay.visible = 1
			pf_Victory_Protoss_overlay.opacity = 100
			bVictory = 	True
		Elseif RaceZerg = True Then 'Zerg planets
			pf_Victory_Zerg_overlay.visible = 1
			pf_Victory_Zerg_overlay.opacity = 100
			bVictory = 	True
		End If
		VictoryScreenTimer.Enabled = True
	End If

'************************************
' Nuclear Launch or Jackpots
'************************************
	If bPlanet1 = True or bPlanet2 = True or bPlanet3 = True or bPlanet4 = True or bPlanet5 = True or bPlanet6 = True then
		if fl35.IntensityScale > 0.9 and fl47.IntensityScale > 0.9  and fl39.IntensityScale > 0.9 and fl36.IntensityScale = 0 and fl48.IntensityScale = 0 and bBallInMotion = true and NuclearLaunchTimer.enabled = false Then	
			If bVictory = True then
				PlaySound "SFX_NuclearLaunchDetected"
				NuclearLaunchTimer.Enabled = true
				pupevent 700
				Select Case Int(rnd*14) 'Random nuke location 1 to 14
					Case 1: NukeTarget01.visible = 1:NuclearTargetTimer01.enabled = True
					Case 2: NukeTarget02.visible = 1:NuclearTargetTimer02.enabled = True
					Case 3: NukeTarget03.visible = 1:NuclearTargetTimer03.enabled = True
					Case 4: NukeTarget04.visible = 1:NuclearTargetTimer04.enabled = True
					Case 5: NukeTarget05.visible = 1:NuclearTargetTimer05.enabled = True
					Case 6: NukeTarget06.visible = 1:NuclearTargetTimer06.enabled = True
					Case 7: NukeTarget07.visible = 1:NuclearTargetTimer07.enabled = True
					Case 8: NukeTarget08.visible = 1:NuclearTargetTimer08.enabled = True
					Case 9: NukeTarget09.visible = 1:NuclearTargetTimer09.enabled = True
					Case 10: NukeTarget10.visible = 1:NuclearTargetTimer10.enabled = True
					Case 11: NukeTarget11.visible = 1:NuclearTargetTimer11.enabled = True
					Case 12: NukeTarget12.visible = 1:NuclearTargetTimer12.enabled = True
					Case 13: NukeTarget13.visible = 1:NuclearTargetTimer13.enabled = True
					Case 14: NukeTarget14.visible = 1:NuclearTargetTimer14.enabled = True
				End Select
			ElseIf bVictory = False then
				JackpotCelebration
				NuclearLaunchTimer.Enabled = true
			End If
		End If
	End If
End Sub

Sub TurnOffBuildings ()
	Turret1.visible = 0        'Terran
	Turret2.visible = 0        '
	Barracks.visible = 0       '
	StarPort.visible = 0       '
	CommandCenter.visible = 0  '
	pylon1a.visible = 0        'Protoss
	Pylon1b.visible = 0        '
	pylon1c.visible = 0        '
	pylon2a.visible = 0        '
    	Pylon2b.visible = 0        '
	pylon2c.visible = 0        '
	Nexus.visible = 0          '
	Gateway.visible = 0        '
	StarGate.visible = 0       '
	Overlord1.visible = 0      'Zerg
	Overlord2.visible = 0      '
	Hive.visible = 0           '
	Spawing_Pool.visible = 0   '
	Spire.visible = 0          '
	Building1RingTimer.Enabled = False 'Turns off lights
	Building2RingTimer.Enabled = False '
	Building3RingTimer.Enabled = False '
End Sub

Dim opacityVar
Sub VictoryScreenTimer_Timer()
	opacityVar = 100
	VictoryScreenTimer.Enabled = False
	VictoryScreenFadeTimer.Enabled = True
End Sub

Sub VictoryScreenFadeTimer_Timer ()
	opacityVar = opacityVar - 1
	If RaceTerran = True Then
		pf_Victory_Terran_overlay.opacity = opacityVar
	Elseif RaceProtoss = True Then
		pf_Victory_Protoss_overlay.opacity = opacityVar
	Elseif RaceZerg = True Then
		pf_Victory_Zerg_overlay.opacity = opacityVar
	End If
	If opacityVar <= 50 Then
		VictoryScreenFadeTimer.Enabled = False
		If RaceTerran = True Then
			pf_Victory_Terran_overlay.visible = 0
			pf_Victory_Terran2_overlay.visible = 1
			pf_Victory_Terran2_overlay.opacity = 50
		Elseif RaceProtoss = True Then
			pf_Victory_Protoss_overlay.visible = 0
			pf_Victory_Protoss2_overlay.visible = 1
			pf_Victory_Protoss2_overlay.opacity = 50
		Elseif RaceZerg = True Then
			pf_Victory_Zerg_overlay.visible = 0
			pf_Victory_Zerg2_overlay.visible = 1
			pf_Victory_Zerg2_overlay.opacity = 50
		End If
	End If
End Sub

dim stepvar: stepvar = 1
Sub SpeciesTrackerAnimation
	If stepvar = 23 Then 'Open animation complete
		opacityVar = 175 'adds delay before fade which starts at 100
	ElseIf stepvar = 25 Then
		SpeciesTracker.imageA = "Frame24"
		opacityVar = 175 'adds delay before fade which starts at 100
	ElseIf stepvar = 27 Then
		SpeciesTracker.imageA = "Frame26"
		opacityVar = 175 'adds delay before fade which starts at 100
	Else
		SpeciesTracker.imageA = "Frame" & Int(stepvar)
		SpeciesTrackerFadeTimer.Enabled = True 'Pauses inbetween steps
	End If
End Sub

Sub SpeciesTrackerTimer_Timer()
	If bPlanet2 = True and bplanet3 = False Then
		opacityVar = 100
		SpeciesTrackerFadeTimer.Enabled = True
	ElseIf bPlanet3 = True and bPlanet4 = False Then 'Terran complete
		stepvar = stepvar + 1 'set to 24
		SpeciesTrackerFadeTimer.Enabled = True 'Pauses inbetween steps
	ElseIf bPlanet4 = True and bplanet5 = False Then
		opacityVar = 100
		SpeciesTrackerFadeTimer.Enabled = True
	ElseIf bPlanet5 = True and bplanet6 = False Then 'Protoss complete
		stepvar = stepvar + 1 'set to 26
		SpeciesTrackerFadeTimer.Enabled = True 'Pauses inbetween steps
	ElseIf bPlanet6 = True Then
		opacityVar = 100
		SpeciesTrackerFadeTimer.Enabled = True
	End If
	SpeciesTrackerTimer.Enabled = False
End Sub

Sub SpeciesTrackerFadeTimer_Timer () 'Fade out Species Tracker
	If stepvar < 23 and LowEndPC=False Then          'loop
		stepvar = stepvar + 1     '
		SpeciesTrackerAnimation() '
	ElseIf stepvar = 24 and bplanet3=true Then 'Terran complete                  '
		stepvar = stepvar + 1 'set to 25
		SpeciesTrackerAnimation() 
	ElseIf stepvar = 26 and bplanet5=true Then 'Protoss complete
		stepvar = stepvar + 1 'set to 27
		SpeciesTrackerAnimation()  
	Else
		SpeciesTracker.opacity = opacityVar         'Fade until 10%
		opacityVar = opacityVar - 1                 '
		If opacityVar <= 10 Then                    '
			SpeciesTrackerFadeTimer.Enabled = False '
		End If
	End If
End Sub

Sub NuclearLaunchTimer_timer ()  'Limits Nuclear Launch notifications to 1.5second between occurances
	NuclearLaunchTimer.Enabled = false
End Sub

Sub NuclearTargetTimer01_timer ()  'adjusts how long target stays on playfield
	NuclearTargetTimer01.Enabled = false
	NukeTarget01.visible = 0
End Sub
Sub NuclearTargetTimer02_timer ()  'adjusts how long target stays on playfield
	NuclearTargetTimer02.Enabled = false
	NukeTarget02.visible = 0
End Sub
Sub NuclearTargetTimer03_timer ()  'adjusts how long target stays on playfield
	NuclearTargetTimer03.Enabled = false
	NukeTarget03.visible = 0
End Sub
Sub NuclearTargetTimer04_timer ()  'adjusts how long target stays on playfield
	NuclearTargetTimer04.Enabled = false
	NukeTarget04.visible = 0
End Sub
Sub NuclearTargetTimer05_timer ()  'adjusts how long target stays on playfield
	NuclearTargetTimer05.Enabled = false
	NukeTarget05.visible = 0
End Sub
Sub NuclearTargetTimer06_timer ()  'adjusts how long target stays on playfield
	NuclearTargetTimer06.Enabled = false
	NukeTarget06.visible = 0
End Sub
Sub NuclearTargetTimer07_timer ()  'adjusts how long target stays on playfield
	NuclearTargetTimer07.Enabled = false
	NukeTarget07.visible = 0
End Sub
Sub NuclearTargetTimer08_timer ()  'adjusts how long target stays on playfield
	NuclearTargetTimer08.Enabled = false
	NukeTarget08.visible = 0
End Sub
Sub NuclearTargetTimer09_timer ()  'adjusts how long target stays on playfield
	NuclearTargetTimer09.Enabled = false
	NukeTarget09.visible = 0
End Sub
Sub NuclearTargetTimer10_timer ()  'adjusts how long target stays on playfield
	NuclearTargetTimer10.Enabled = false
	NukeTarget10.visible = 0
End Sub
Sub NuclearTargetTimer11_timer ()  'adjusts how long target stays on playfield
	NuclearTargetTimer11.Enabled = false
	NukeTarget11.visible = 0
End Sub
Sub NuclearTargetTimer12_timer ()  'adjusts how long target stays on playfield
	NuclearTargetTimer12.Enabled = false
	NukeTarget12.visible = 0
End Sub
Sub NuclearTargetTimer13_timer ()  'adjusts how long target stays on playfield
	NuclearTargetTimer13.Enabled = false
	NukeTarget13.visible = 0
End Sub
Sub NuclearTargetTimer14_timer ()  'adjusts how long target stays on playfield
	NuclearTargetTimer14.Enabled = false
	NukeTarget14.visible = 0
End Sub

Sub JackpotCelebration 'Fires gun
	If RaceTerran = True Then 'only when on Terran planets, which is the only one with this image on the pf.
		Playsound "SFX_GunFire"
		FlashForMs TerranBlaster1, 300, 100, 0
		CelebrationTimer.enabled = true
	End If
End Sub

Sub CelebrationTimer_Timer
	CelebrationTimer.enabled = false
	FlashForMs TerranBlaster2, 300, 100, 0
End Sub

'**********
' Keys
'**********
Dim BIPL : BIPL=0

Sub table1_KeyDown(ByVal Keycode)

    If KeyCode = MechanicalTilt Then vpmTimer.PulseSw vpmNudge.TiltSwitch: Exit Sub: End if

	If KeyCode = PlungerKey Then
		Plunger.Pullback
		SoundPlungerPull()
		TimerPlunger.Enabled = True
		TimerPlunger2.Enabled = False
	End If

    If keycode = LeftMagnaSave Then 'halplay
			If halplay = 0 and bBallInMotion=False and GIFL1.IntensityScale > 0.1 and (bPlanet2 = False and bPlanet3 = False and bPlanet4 = False and bPlanet5 = False and bPlanet6 = False) Then
				halgame
				Timer067.Enabled = True
				TextBox002.Text = "Control given to Mission Command"
				if bPlanet1 = True Then
					TextBox001.Text = "Launch ball"
				Else
					TextBox001.Text = "Start Game"
				End If
			ElseIf halplay = 1 Then  'Turn HAL off
                halplay = 0
				Trigger018001.Enabled = False
				Trigger019001.Enabled = False
				Trigger020001.Enabled = False
				Trigger021001.Enabled = False
				Trigger022001.Enabled = False
				Trigger023001.Enabled = False
				Trigger025001.Enabled = False
				Kicker025001.Enabled  = False
				Trigger026001.Enabled = False
				Timer067.Enabled      = False
				TextBox002.Text = "Control passed back to player"
				TextBox001.Text = "Ha. GOOD LUCK!"
			End If
	End if 'halplay

    If KeyCode = RightMagnaSave OR KeyCode = LockbarKey Then 
		If bPlanet1 or bPlanet2 or bPlanet3 or bPlanet4 or bPlanet5 or bPlanet6 Then 'do not allow changing if game has started
		Else
			If RaceLoop >2 Then
				RaceLoop =1
			Else
				RaceLoop = RaceLoop + 1
			End If
			If RaceLoop = 0 or RaceLoop = 1 Then 'Terran
				RaceChangeVar = 1
				PupEvent 702
				PupEvent 701
				pf_ProtossVsZerg.visible = 0
				pf_ZergVsProtoss.visible = 0
				decals_apronT.Material = "Plastic_decals"
				decals_apronP.Material = "Plastic_decals_trans"
				decals_apronZ.Material = "Plastic_decals_trans"
				decals_apronZ2.Material = "Plastic_decals_trans"
				PlanetRace.visible = 1
				PlanetRace.imageA = "Planets_T"
				backglass.image = "backbox_on_t"
				StartingRace = 1 'Terran
				RaceTerran = True
				RaceProtoss = False
				RaceZerg = False
				If ColoredRamps = true Then 
					ChangeGi white
					ChangeGiIntensity 1.2
					ramp_off.image = "ramp_off" 'normal
					ramp_on.image = "ramp_on"
				End If
				walls_metal.image = "walls_metal_on"
			Elseif RaceLoop = 2 Then 'Protoss
				RaceChangeVar = 2
				PupEvent 704
				PupEvent 703
				pf_ZergVsProtoss.visible = 0
				pf_ProtossVsZerg.visible = 1
				decals_apronT.Material = "Plastic_decals_trans"
				decals_apronP.Material = "Plastic_decals"
				decals_apronZ.Material = "Plastic_decals_trans"
				decals_apronZ2.Material = "Plastic_decals_trans"
				PlanetRace.imageA = "Planets_P"
				backglass.image = "backbox_on_p"
				StartingRace = 2 'Protoss
				RaceTerran = False
				RaceProtoss = True
				RaceZerg = False
				If ColoredRamps = true Then 
					ChangeGi darkblue
					ChangeGiIntensity 2
					ramp_off.image = "ramps_off_mod_Protoss" 'Ramps to blue
					ramp_on.image = "ramps_on_mod_Protoss"
				End If
				walls_metal.image = "walls_metal_on_mod_blue"
			Elseif RaceLoop = 3 Then 'Zerg
				RaceChangeVar = 3
				PupEvent 708
				PupEvent 707
				pf_ProtossVsZerg.visible = 0
				pf_ZergVsProtoss.visible = 1
				decals_apronT.Material = "Plastic_decals_trans"
				decals_apronP.Material = "Plastic_decals_trans"
				decals_apronZ.Material = "Plastic_decals"
				decals_apronZ2.Material = "Plastic_decals_trans"
				PlanetRace.imageA = "Planets_Z"
				backglass.image = "backbox_on_z"
				StartingRace = 3 'Zerg
				RaceTerran = False
				RaceProtoss = False
				RaceZerg = True
				If ColoredRamps = true Then 
					ChangeGi purple
					ChangeGiIntensity 1.2
					ramp_off.image = "ramps_off_mod_Zerg" 'Ramps to purple
					ramp_on.image = "ramps_on_mod_Zerg"
				End If
				walls_metal.image = "walls_metal_on_mod_purple"
			End If
		End If
	End if

	If keycode = LeftFlipperKey Then
		FlipperActivate LeftFlipper, LFPress
		LeftFlipperButton.X = LeftFlipperButton.X + 10
		TextBox001.Text = ""
		TextBox002.Text = ""
	End If

	If keycode = RightFlipperKey Then
		FlipperActivate RightFlipper, RFPress
		'Control the small right flipper with the main right flipper button.
		Controller.Switch(88) = 1
		If FlippersEnabled Then SolRFlipperS True
        FlipperActivate RightFlipperS, RFPress2 'small flipper
		RightFlipperButtonMagna.X = RightFlipperButtonMagna.X - 10
		RightFlipperButton.X = RightFlipperButton.X - 10
	End If

	'*********************************
	'Keydown Sounds
	'*********************************
	If keycode = LeftTiltKey Then Nudge 90, 5:SoundNudgeLeft()
	If keycode = RightTiltKey Then Nudge 270, 5:SoundNudgeRight()
	If keycode = CenterTiltKey Then Nudge 0, 3:SoundNudgeCenter()

	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If

	if keycode=StartGameKey and GIFL1.IntensityScale > 0.1 then  'will not start while booting up
		if bBallInMotion=False then                              'will not start if ball is in motion
			soundStartButton()
			pf_Victory_Protoss2_overlay.visible = 0
			pf_Victory_Protoss_overlay.visible = 0
			pf_Victory_Terran2_overlay.visible = 0
			pf_Victory_Terran_overlay.visible = 0
			pf_Victory_Zerg2_overlay.visible = 0
			pf_Victory_Zerg_overlay.visible = 0
			VictoryScreenTimer.Enabled = False
			FirstFarm = 0              'Resets value for new game 
			SecondFarm = 0             '
			LastFarm = 0               '
			WaitingBuildingNext = 1    '
			pupevent 799               'Clear Supply Overlay
			Building1RingTimer.Enabled = False
			Building2RingTimer.Enabled = False
			Building3RingTimer.Enabled = False
			BuildingRingsOff
			WarpTimer1.Enabled = false
			WarpTimer2.Enabled = false
			WarpTimer3.Enabled = false
			WarpTimersOff
			KhaydarinCrystals = 0      'Resets resources 
			VespeneGas = 0             'Resets resources
			Turret1.visible = 0        'Turns OFF Buildings 
			Turret2.visible = 0        '
			Barracks.visible = 0       '
			StarPort.visible = 0       '
			CommandCenter.visible = 0  '
			pylon1a.visible = 0        '
			Pylon1b.visible = 0        '
			pylon1c.visible = 0        '
			pylon2a.visible = 0        '
			Pylon2b.visible = 0        '
			pylon2c.visible = 0        '
			Nexus.visible = 0          '
			Gateway.visible = 0        '
			StarGate.visible = 0       '
			Overlord1.visible = 0      '
			Overlord2.visible = 0      '
			Hive.visible = 0           '
			Spawing_Pool.visible = 0   '
			Spire.visible = 0          '
			bPlanet1 = True
			bPlanet2 = False
			bPlanet3 = False
			bPlanet4 = False
			bPlanet5 = False
			bPlanet6 = False
			bVictory = False
			If StartingRace = 1 Then ' Resets starting race after a game has been played
				RaceTerran = True
				RaceProtoss = False
				RaceZerg = False
			ElseIf StartingRace = 2 Then
				RaceTerran = False
				RaceProtoss = True
				RaceZerg = False
			ElseIf StartingRace = 3 Then
				RaceTerran = False
				RaceProtoss = False
				RaceZerg = True
			End If
			If RaceProtoss = False and RaceZerg = False Then
				StartingRace = 1
				RaceTerran = True
				RaceProtoss = False
				RaceZerg = False
				decals_apronT.Material = "Plastic_decals"
				decals_apronP.Material = "Plastic_decals"
				decals_apronZ.Material = "Plastic_decals"
				decals_apronZ2.Material = "Plastic_decals"
				pf_TerranVsProtoss.visible = 0
				pf_ProtossVsZerg.visible = 0
				pf_ProtossVsTerran.visible = 0
				pf_ZergVsProtoss.visible = 0
				pf_ZergVsTerran.visible = 0
				pupevent 701               'Terran overlay - Used when restarting
				pupevent 783               'Terran Backglass restart
				Turret1.visible = 1        'Turns ON Building
				Turret2.visible = 1        '
				backglass.image = "backbox_on_t"
				PlanetRace.visible = 1
				PlanetRace.imageA = "Planets_T"
				JackpotCelebration
				stepvar = 1
				SpeciesTracker.visible = True       'Species Tracker logic
				PlaySound "FX_SpeciesTrackerLaunch" '
				If LowEndPC = True Then                      'Reduced graphics tracker
					SpeciesTracker.imageA = "Frame22"        '
					opacityVar = 175                         ' 
					SpeciesTrackerFadeTimer.Enabled = True   '
				Else                                         '
					SpeciesTrackerAnimation ()               'Full graphics tracker
				End If
			Elseif RaceProtoss = True Then
				PupEvent 704
				PupEvent 703
				pf_TerranVsProtoss.visible = 0
				pf_ProtossVsZerg.visible = 1    'ON
				pf_ProtossVsTerran.visible = 0
				pf_ZergVsProtoss.visible = 0
				pf_ZergVsTerran.visible = 0
				decals_apronT.Material = "Plastic_decals_trans"
				decals_apronP.Material = "Plastic_decals"
				decals_apronZ.Material = "Plastic_decals_trans"
				decals_apronZ2.Material = "Plastic_decals_trans"
				backglass.image = "backbox_on_p"
				If ColoredRamps = true Then 
					ChangeGi darkblue
					ChangeGiIntensity 2
					ramp_off.image = "ramps_off_mod_Protoss" 'Ramps to blue
					ramp_on.image = "ramps_on_mod_Protoss"
				End If
				walls_metal.image = "walls_metal_on_mod_blue"
				SpeciesTracker.visible = False       'Species Tracker logic
				pylon1a.visible = 1
				Pylon1b.visible = 1
				pylon1c.visible = 1
				pylon2a.visible = 1
				Pylon2b.visible = 1
				pylon2c.visible = 1
			Else
				PupEvent 708
				PupEvent 707
				pf_TerranVsProtoss.visible = 0
				pf_ProtossVsZerg.visible = 0
				pf_ProtossVsTerran.visible = 0
				pf_ZergVsProtoss.visible = 1     'ON
				pf_ZergVsTerran.visible = 0
				decals_apronT.Material = "Plastic_decals_trans"
				decals_apronP.Material = "Plastic_decals_trans"
				decals_apronZ.Material = "Plastic_decals"
				decals_apronZ2.Material = "Plastic_decals_trans"
				backglass.image = "backbox_on_z"
				If ColoredRamps = true Then 
					ChangeGi purple
					ChangeGiIntensity 1.2
					ramp_off.image = "ramps_off_mod_Zerg" 'Ramps to purple
					ramp_on.image = "ramps_on_mod_Zerg"
				End If
				walls_metal.image = "walls_metal_on_mod_purple"
				Overlord1.visible = 1
				Overlord2.visible = 1 
			End If
			UpdatePupVariables()       'Updates pup screen variables (crystals/gas)
		End If
	End If

	If vpmKeyDown(keycode) Then Exit Sub

End Sub

Sub table1_KeyUp(ByVal Keycode)

    If keycode = LeftMagnaSave Then
		'bLutActive = False
	End If

	If keycode = LeftFlipperKey Then 
		FlipperDeActivate LeftFlipper, LFPress
		LeftFlipperButton.X = LeftFlipperButton.X - 10
	End If

	If keycode = RightFlipperKey Then
		FlipperDeActivate RightFlipper, RFPress
		SolRFlipperS False
		Controller.Switch(88) = 0
        FlipperDeActivate RightFlipperS, RFPress2
		RightFlipperButtonMagna.X = RightFlipperButtonMagna.X + 10
		RightFlipperButton.X = RightFlipperButton.X + 10		
	End If

	'*********************************
	'KeyUp Sounds
	'*********************************
	If KeyCode = PlungerKey Then
		Plunger.Fire
		TimerPlunger.Enabled = False
		TimerPlunger2.Enabled = True
		plungerP.Y = 0
		If BIPL = 1 Then
				SoundPlungerReleaseBall() 'Plunger release sound when there is a ball in shooter lane
		Else
				SoundPlungerReleaseNoBall() 'Plunger release sound when there is no ball in shooter lane
		End If
	End If

	If vpmKeyUp(keycode) Then Exit Sub

End Sub


'******************************************
' Change light color - simulate color leds
' changes the light color and state
' 12 colors: red, orange, amber, yellow...
'******************************************

'colors
Const red = 1
Const orange = 2
Const amber = 3
Const yellow = 4
Const darkgreen = 5
Const green = 6
Const blue = 7
Const darkblue = 8
Const purple = 9
Const white = 10
Const teal = 11
Const ledwhite = 12

Sub SetLightColor(n, col, stat) 'stat 0 = off, 1 = on, 2 = blink, -1= no change
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
            n.color = RGB(0, 16, 0)
            n.colorfull = RGB(0, 128, 0)
        Case blue
            n.color = RGB(0, 18, 18)
            n.colorfull = RGB(0, 255, 255)
        Case darkblue
            n.color = RGB(0, 8, 255)
            n.colorfull = RGB(0, 64, 255)
        Case purple
            n.color = RGB(64, 0, 255)
            n.colorfull = RGB(128, 0, 255)
        Case white 'bulb
            n.color = RGB(193, 91, 0)
            n.colorfull = RGB(255, 197, 143)
        Case teal
            n.color = RGB(1, 64, 62)
            n.colorfull = RGB(2, 128, 126)
        Case ledwhite
            n.color = RGB(255, 197, 143)
            n.colorfull = RGB(255, 252, 224)
    End Select
    If stat <> -1 Then
        n.State = 0
        n.State = stat
    End If
End Sub

Sub SetFlashColor(n, col, stat) 'stat 0 = off, 1 = on, -1= no change - no blink for the flashers, use FlashForMs
    Select Case col
        Case red
            n.color = RGB(255, 0, 0)
        Case orange
            n.color = RGB(255, 64, 0)
        Case amber
            n.color = RGB(255, 153, 0)
        Case yellow
            n.color = RGB(255, 255, 0)
        Case darkgreen
            n.color = RGB(0, 64, 0)
        Case green
            n.color = RGB(0, 128, 0)
        Case blue
            n.color = RGB(0, 255, 255)
        Case darkblue
            n.color = RGB(0, 64, 64)
        Case purple
            n.color = RGB(128, 0, 192)
        Case white 'bulb
            n.color = RGB(255, 197, 143)
            stat = 0
        Case teal
            n.color = RGB(2, 128, 126)
        Case ledwhite
            n.color = RGB(255, 252, 224)
    End Select
    If stat <> -1 Then
        n.Visible = stat
    End If
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
    For each bulb in GILamps
        SetLightColor bulb, col, -1
    Next
    'SetFlashColor GiFlasher, col, 1
End Sub

Sub ChangeGiIntensity(factor) 'changes the intensity scale
    Dim bulb
    For each bulb in GiLamps
        bulb.IntensityScale = factor
    Next
End Sub

Sub GIUpdateTimer_Timer
    Dim tmp, obj
    tmp = Getballs
    If UBound(tmp) <> OldGiState Then
        OldGiState = Ubound(tmp)
        If UBound(tmp) = 0 Then '-1 means no balls, 0 is the first captive ball, 1 is the second captive ball...)
            GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
        Else
            Gion
        End If
    End If
End Sub

Sub GiOn
    PlaySoundAt "fx_GiOn", li008 'about the center of the table
    DOF 118, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
End Sub

Sub GiOff
    PlaySoundAt "fx_GiOff", li008 'about the center of the table
    DOF 118, DOFOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
End Sub

' GI, light & flashers sequence effects

Sub GiEffect(n)
    Dim ii
    Select Case n
        Case 0 'all off
            LightSeqGi.Play SeqAlloff
        Case 1 'all blink
            LightSeqGi.UpdateInterval = 40
            LightSeqGi.Play SeqBlinking, , 15, 25
        Case 2 'random
            LightSeqGi.UpdateInterval = 25
            LightSeqGi.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqGi.UpdateInterval = 20
            LightSeqGi.Play SeqBlinking, , 10, 10
        Case 4 'seq up
            LightSeqGi.UpdateInterval = 3
            LightSeqGi.Play SeqUpOn, 25, 3
        Case 5 'seq down
            LightSeqGi.UpdateInterval = 3
            LightSeqGi.Play SeqDownOn, 25, 3
    End Select
End Sub

Sub LightEffect(n)
    Select Case n
        Case 0 ' all off
            LightSeqInserts.Play SeqAlloff
        Case 1 'all blink
            LightSeqInserts.UpdateInterval = 40
            LightSeqInserts.Play SeqBlinking, , 15, 25
        Case 2 'random
            LightSeqInserts.UpdateInterval = 25
            LightSeqInserts.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqInserts.UpdateInterval = 20
            LightSeqInserts.Play SeqBlinking, , 10, 10
        Case 4 'center
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqCircleOutOn, 15, 2
        Case 5 'top down
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqDownOn, 15, 1
        Case 6 'down to top
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 15, 1
    End Select
End Sub

'****************************
'   Plunger Animations for VR
'****************************
Sub TimerPlunger_Timer
  If plungerP.Y < 100 then
  		plungerP.Y = plungerP.Y + 5
  End If
End Sub

Sub TimerPlunger2_Timer
  plungerP.Y = 0 + (5* Plunger.Position) -20
End Sub


''*********
''Solenoids
''*********
'
SolCallback(1) = "SolRelease"
SolCallback(2) = "SolAutolaunch"
'SolCallback(3) = "bsLeftvuk.SolOut"
SolCallback(3) = "SolLeftVuk"
SolCallback(4) = "bsSupervuk.SolOut"
SolCallback(5) = "SolLeftMagnet"
SolCallback(6) = "SolRightMagnet"
SolCallBack(7) = "SolBrainBug"
SolCallBack(8)= "SolKnocker"
SolCallBack(15)= "FlippersEnabled="
SolCallBack(23) = "SetLamp 123," 	'flash brainbug
SolCallBack(25) = "Sol25" 	'Flash Red
SolCallBack(26) = "Sol26"	'Flash Yellow
SolCallBack(27) = "Sol27"	'Flash Green
SolCallBack(28) = "Sol28"	'Flash Blue
SolCallBack(29) = "SetLamp 129," 'F5 WarBugSled Flasher X4
SolCallBack(30) = "SetLamp 130," 'F6 Left Ramp Flasher X4
SolCallBack(31) = "SetLamp 131," 'F7 Right Ramp Flasher X4
SolCallBack(32) = "SetLamp 132," 'F8 Pop Bumpers Flasher X2


Sub sol25(enabled): If enabled then FlashBlinkingRed:Else FlashoffRed:End if:End Sub
Sub sol26(enabled): If enabled then FlashBlinkingYellow:Else FlashoffYellow:End if:End Sub
Sub sol27(enabled): If enabled then FlashBlinkingGreen:Else FlashoffGreen:End if:End Sub
Sub sol28(enabled): If enabled then FlashBlinkingBlue:Else FlashoffBlue:End if:End Sub


Sub SolKnocker(Enabled) : If enabled Then KnockerSolenoid : End If : End Sub 'Add knocker position object

'*************
'BALL RELEASE
'*************
Sub SolRelease(Enabled)
    If Enabled Then
        If bsTrough.Balls > 0 Then bsTrough.ExitSol_On
    End If
End Sub


Sub SolAutoLaunch(Enabled)
	If Enabled Then
		Plunger.Autoplunger = True
		Plunger.Fire
		PlaySound SoundFX("fx_AutoPlunger", DOFContactors)
		Else
		Plunger.Autoplunger = False
	End If
 End Sub


'Sub Drain_Hit:bsTrough.AddBall Me : RandomSoundDrain drain : End Sub

Sub Drain_Hit
	if halplay=1 Then 'halplay, original drain_hit sub above
		Kicker025001.enabled=1 'halplay
	end If 'halplay
	'If bPlanet1 = true and lf7.IntensityScale = 0 then 'Terran
	'		pupevent 731
	'Elseif bPlanet3 = true or bPlanet4 = True then 'Protoss
	'	pupevent 732
	'Elseif bPlanet5 = true or bPlanet6 = True then 'Zerg
	'	pupevent 733
	'End If
	bsTrough.AddBall Me
	RandomSoundDrain drain
End Sub


if halplay=1 Then 'halplay
Kicker025001.enabled=1 'halplay
end If 'halplay


'leftVUK
dim KB

Sub SolLeftVuk(Enabled)
	if enabled then
		KB = True
		bsleftvuk.exitSol_on
		vuk_switch.RotX = 90
		sw45.TimerEnabled = True
	End if
End Sub

Sub sw45a_Hit()
	Vuk_Switch.RotX = 80
	SoundSaucerLock
	If RaceTerran = True then 'Terran
		pupevent 721
	Elseif RaceProtoss = True then 'Protoss
		pupevent 722
	Elseif RaceZerg = True then 'Zerg
		pupevent 723
	End If
End Sub
Sub sw45a_UnHit(): SoundSaucerKick 1, sw45 : End Sub

Sub sw45_Timer()
	if KB = True and vuk_kicker.TransZ < -25 then vuk_kicker.TransZ = vuk_kicker.TransZ +5
	if KB = False and vuk_kicker.TransZ > -50 then vuk_kicker.TransZ = vuk_kicker.TransZ -5
	if vuk_kicker.TransZ >= -25 then KB = False
	if KB = False And vuk_kicker.TransZ = -50 then me.TimerEnabled = False
End Sub


Sub sw46_Hit: SoundSaucerLock:  bsSuperVUK.AddBall Me : End Sub
Sub sw46_UnHit(): SoundSaucerKick 1, sw46 : MakeBugBalls : End Sub


'**********
'BRAIN BUG
'**********

Dim BrainBugStuff

Sub SolBrainBug(Enabled)
	if enabled then
		dim BOT: BOT=getballs
		dim b: for each b in BOT  'kick ball out of way if on top of brainbug
			if InRect(b.x,b.y,545,871,472,1054,633,1126,714,944) then
				b.velz = 20
				b.vely = b.vely + 5
			end if
		next
		BrainBug.Material = "Toys"
		BrainBugflipper.rotatetoend
		BrainBug.image = "brainbugup_on"
		sw38.isDropped = 0
		sw38a.isDropped = 0
		PlaySoundAT SoundFX("fx_trapdoorhigh",DOFContactors), sw46
		Controller.Switch(37) = True
		Controller.Switch(39) = True
		ShakeBrainBug
	Else
		BrainBugFlipper.rotatetostart
		BrainBug.image = "brainbugup_off"
		sw38.isDropped = 1
		sw38a.isDropped = 1
		PlaySoundAT SoundFX("fx_trapdoorlow",DOFContactors), sw46
		Controller.Switch(37) = False
		Controller.Switch(39) = False
		BrainBug.Material = "Toys_alpha"
	end if
End Sub


Sub sw38_Hit:ShakeBrainBug2:PlaySoundAtBall "fx_brainbughit":Me.TimerEnabled = 1:End Sub
Sub sw38_timer: vpmTimer.PulseSw 38:Me.TimerEnabled = 0:End Sub

'BRAINBUG_SHAKE

Dim BrainBugPos

Sub ShakeBrainBug
    BrainBugPos = 8
    ShakeBrainBugTimer.Enabled = 1
End Sub

Sub ShakeBrainBugTimer_Timer()
    For each BrainBugStuff in BrainBugGroup:BrainBugStuff.RotAndTra4 = BrainBugPos:Next
    If BrainBugPos = 0 Then ShakeBrainBugTimer.Enabled = False:Exit Sub
    If BrainBugPos < 0 Then
        BrainBugPos = ABS(BrainBugPos) - 1
    Else
        BrainBugPos = - BrainBugPos + 1
    End If
End Sub

Sub ShakeBrainBug2
    BrainBugPos = 2
    ShakeBrainBugTimer2.Enabled = 1
End Sub

Sub ShakeBrainBugTimer2_Timer()
    For each BrainBugStuff in BrainBugGroup:BrainBugStuff.RotAndTra2 = BrainBugPos:Next
    If BrainBugPos = 0 Then ShakeBrainBugTimer2.Enabled = False:Exit Sub
    If BrainBugPos < 0 Then
        BrainBugPos = ABS(BrainBugPos) - 1
    Else
        BrainBugPos = - BrainBugPos + 1
    End If
End Sub


'**********************
'Warrior Bug Animation
'**********************

Dim BugWalls, xx
BugWalls = Array (BWall1,BWall2,BWall3,BWall4,BWall5,BWall6,BWall7,BWall8,BWall9,BWall10,BWall11,BWall12,BWall13,BWall14,BWall15,BWall16,BWall17,BWall18,BWall19,BWall20,BWall21,BWall22,BWall23,BWall24)

'e8
Sub UpdateWarriorBug(NewPos, aSpeed, LastPos)
	dim Position, LastPosition, WarriorBugStuff
	For each WarriorBugStuff in WarriorBugGroup: WarriorBugStuff.TransZ = NewPos-111:next '-110 -1 difference to starting point

	'Position = NewPos * 23 / 120 ' There are 23 targets + this one
	'LastPosition = LastPos * 23 / 120 'If this is distance -110 to +10, new distance -110 to +93.5 + 18.5 = 222
    Position = NewPos * 23 / 222 '120 steps in original (from -110 to 10)
	LastPosition = LastPos * 23 / 222

	BugWalls(LastPosition).IsDropped=1
	BugWalls(LastPosition).collidable = 0
	BugWalls(Position).IsDropped=0
	BugWalls(Position).collidable = 1
End Sub


Sub BWalls_Hit(idx):vpmTimer.PulseSw 35:ShakeTarget:playsound "SFX_hit":End Sub 'fx_switch

'e8
Sub ShakeTarget
    'TargetPos = 4	'8
    TargetPos = 7.36 ' 1.85 x 4
    ShakeTargetTimer.Enabled = 1
End Sub

Dim TargetPos

'e8
Sub ShakeTargetTimer_Timer()
    WarriorBugAssembly003.RotAndTra4 = TargetPos
    If TargetPos = 0 Then ShakeTargetTimer.Enabled = False:Exit Sub
    If TargetPos < 0 Then
        TargetPos = ABS(TargetPos) - 1
    Else
        TargetPos = - TargetPos + 1
    End If
End Sub

Sub sw10_Hit:vpmTimer.PulseSw 10: vpmTimer.AddTimer 1800, "SuperVukAddBall'":Me.Enabled = 0:Me.TimerEnabled = 1: End Sub

Sub sw10_Timer: Me.DestroyBall:Me.TimerEnabled = 0:Me.Enabled = 1:End Sub

Sub SuperVukAddBall()
	bsSuperVuk.AddBall 1
End Sub





'********************************************
'DJRobX's Warrior Bug Stepper Supporting Code
'********************************************

Class cvpmMyMech
	Public Sol1, Sol2, MType, Length, Steps, Acc, Ret
	Private mMechNo, mNextSw, mSw(), mLastPos, mLastSpeed, mCallback

	Private Sub Class_Initialize
		ReDim mSw(10)
		gNextMechNo = gNextMechNo + 1 : mMechNo = gNextMechNo : mNextSw = 0 : mLastPos = 0 : mLastSpeed = 0
		MType = 0 : Length = 0 : Steps = 0 : Acc = 0 : Ret = 0 : vpmTimer.addResetObj Me
	End Sub

	Public Sub AddSw(aSwNo, aStart, aEnd)
		mSw(mNextSw) = Array(aSwNo, aStart, aEnd, 0)
		mNextSw = mNextSw + 1
	End Sub

	Public Sub AddPulseSwNew(aSwNo, aInterval, aStart, aEnd)
		If Controller.Version >= "01200000" Then
			mSw(mNextSw) = Array(aSwNo, aStart, aEnd, aInterval)
		Else
			mSw(mNextSw) = Array(aSwNo, -aInterval, aEnd - aStart + 1, 0)
		End If
		mNextSw = mNextSw + 1
	End Sub

	Public Sub Start
		Dim sw, ii
		With Controller
			.Mech(1) = Sol1 : .Mech(2) = Sol2 : .Mech(3) = Length
			.Mech(4) = Steps : .Mech(5) = MType : .Mech(6) = Acc : .Mech(7) = Ret
			ii = 10
			For Each sw In mSw
				If IsArray(sw) Then
					.Mech(ii) = sw(0) : .Mech(ii+1) = sw(1)
					.Mech(ii+2) = sw(2) : .Mech(ii+3) = sw(3)
					ii = ii + 10
				End If
			Next
			.Mech(0) = mMechNo
		End With
		If IsObject(mCallback) Then mCallBack 0, 0, 0 : mLastPos = 0 : vpmTimer.EnableUpdate Me, True, True  ' <------- All for this.
	End Sub

	Public Property Get Position : Position = Controller.GetMech(mMechNo) : End Property
	Public Property Get Speed    : Speed = Controller.GetMech(-mMechNo)   : End Property
	Public Property Let Callback(aCallBack) : Set mCallback = aCallBack : End Property

	Public Sub Update
		Dim currPos, speed
		currPos = Controller.GetMech(mMechNo)
		speed = Controller.GetMech(-mMechNo)
		If currPos < 0 Or (mLastPos = currPos And mLastSpeed = speed) Then Exit Sub
		mCallBack currPos, speed, mLastPos : mLastPos = currPos : mLastSpeed = speed
	End Sub

	Public Sub Reset : Start : End Sub
	
End Class



Sub SolLeftMagnet(Enabled):if enabled then PlaySoundAt "fx_magnet", Leftmagnet:Else:StopSound "fx_magnet":End if:End Sub
Sub SolRightMagnet(Enabled):if enabled then PlaySoundAt "fx_magnet", Rightmagnet:Else:StopSound "fx_magnet":End if:End Sub


'**************
' Flipper Subs
'**************


SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
'SolCallback(14) = "SolRFlipperS" ' mini flipper


Const ReflipAngle = 20

Sub SolLFlipper(Enabled)

	If Enabled Then
		LF.Fire  'leftflipper.rotatetoend

		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
				RandomSoundReflipUpLeft LeftFlipper
		Else 
				SoundFlipperUpAttackLeft LeftFlipper
				RandomSoundFlipperUpLeft LeftFlipper
		End If                
	Else
		LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
				RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
    End If

End Sub

Sub SolRFlipper(Enabled)
    
If Enabled Then
		RF.Fire 'rightflipper.rotatetoend

		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
				RandomSoundReflipUpRight RightFlipper
		Else 
				SoundFlipperUpAttackRight RightFlipper
				RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
				RandomSoundFlipperDownRight RightFlipper
		End If        
		FlipperRightHitParm = FlipperUpSoundLevel
	End If

End Sub

Sub SolRFlipperS(Enabled)
    If Enabled Then
		RandomSoundReflipUpRight RightFlipperS
        RFs.fire 'RightFlipper.RotateToEnd
    Else
		RandomSoundFlipperDownRight RightFlipperS
        RightFlipperS.RotateToStart
    End If
End Sub

'************
' SlingShots
'************


Dim RStep, Lstep, VespeneGas, KhaydarinCrystals

Sub Rightslingshot_Slingshot
	RS.VelocityCorrect(ActiveBall)
    RandomSoundSlingshotRight Sling1
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransY = -17
    RStep = 0
    Rightslingshot.TimerEnabled = 1
    vpmTimer.PulseSw 59
	plasticsslingshots_MiningR.Material = "Plastics_textured_off"
	VespeneGas = VespeneGas + 100
	UpdatePupVariables()


	Buildbuldings
End Sub

Sub Rightslingshot_Timer
    Select Case RStep
        Case 1:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransY = -5
        Case 2:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransY = 0:Rightslingshot.TimerEnabled = 0:plasticsslingshots_MiningR.Material = "Plastic_decals_trans"
    End Select
    RStep = RStep + 1
End Sub

Sub Leftslingshot_Slingshot
	LS.VelocityCorrect(ActiveBall)
    RandomSoundSlingshotLeft Sling2
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransY = -12
    LStep = 0
    Leftslingshot.TimerEnabled = 1
    vpmTimer.PulseSw 62
	plasticsslingshots_MiningL.Material = "Plastics_textured_off"
	KhaydarinCrystals = KhaydarinCrystals + 100
	UpdatePupVariables()
	Buildbuldings
End Sub

Sub Leftslingshot_Timer
    Select Case LStep
        Case 1:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransY = -5
        Case 2:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransY = 0:Leftslingshot.TimerEnabled = 0:plasticsslingshots_MiningL.Material = "Plastic_decals_trans"
    End Select
    LStep = LStep + 1
End Sub

Dim FirstFarm, SecondFarm, LastFarm, WaitingBuildingNext

Sub UpdatePupVariables
	PuPlayer.LabelSet pDMD,"CurrCrystals"," " & KhaydarinCrystals & " ",1,"{'mt':2,'fonth':6,'xpos':41,'ypos':94}"
	PuPlayer.LabelSet pDMD,"CurrGas"," " & VespeneGas & " ",1,"{'mt':2,'fonth':6,'xpos':49,'ypos':94}"
		If raceTerran = True Then 'Terran - Slight change in variable locations
			PuPlayer.LabelSet pDMD,"CurrCrystalsMini"," " & KhaydarinCrystals & " ",1,"{'mt':2,'fonth':2.5,'xpos':85.3,'ypos':20.8}"
			PuPlayer.LabelSet pDMD,"CurrGasMini"," " & VespeneGas & " ",1,"{'mt':2,'fonth':2.5,'xpos':91.3,'ypos':20.8}"
			If LastFarm = 1 Then
				PuPlayer.LabelSet pDMD,"CurrSupplyMini"," " & "128" & " ",1,"{'mt':2,'fonth':2.5,'xpos':97.1,'ypos':20.8}"
			ElseIf SecondFarm = 1 Then
				PuPlayer.LabelSet pDMD,"CurrSupplyMini"," " & "64" & " ",1,"{'mt':2,'fonth':2.5,'xpos':97.1,'ypos':20.8}"
			ElseIf FirstFarm = 1 Then
				PuPlayer.LabelSet pDMD,"CurrSupplyMini"," " & "32" & " ",1,"{'mt':2,'fonth':2.5,'xpos':97.1,'ypos':20.8}"
			Else
				PuPlayer.LabelSet pDMD,"CurrSupplyMini"," " & "16" & " ",1,"{'mt':2,'fonth':2.5,'xpos':97.1,'ypos':20.8}"
			End If
		ElseIf RaceProtoss = True Then 'Protoss - Slight change in variable locations
			PuPlayer.LabelSet pDMD,"CurrCrystalsMini"," " & KhaydarinCrystals & " ",1,"{'mt':2,'fonth':2.5,'xpos':85.3,'ypos':19.1}"
			PuPlayer.LabelSet pDMD,"CurrGasMini"," " & VespeneGas & " ",1,"{'mt':2,'fonth':2.5,'xpos':91.3,'ypos':19.1}"
			If LastFarm = 1 Then
				PuPlayer.LabelSet pDMD,"CurrSupplyMini"," " & "128" & " ",1,"{'mt':2,'fonth':2.5,'xpos':97.1,'ypos':19.1}"
			ElseIf SecondFarm = 1 Then
				PuPlayer.LabelSet pDMD,"CurrSupplyMini"," " & "64" & " ",1,"{'mt':2,'fonth':2.5,'xpos':97.1,'ypos':19.1}"
			ElseIf FirstFarm = 1 Then
				PuPlayer.LabelSet pDMD,"CurrSupplyMini"," " & "32" & " ",1,"{'mt':2,'fonth':2.5,'xpos':97.1,'ypos':19.1}"
			Else
				PuPlayer.LabelSet pDMD,"CurrSupplyMini"," " & "16" & " ",1,"{'mt':2,'fonth':2.5,'xpos':97.1,'ypos':19.1}"
			End If
		ElseIf RaceZerg = True Then 'Zerg - Slight change in variable locations
			PuPlayer.LabelSet pDMD,"CurrCrystalsMini"," " & KhaydarinCrystals & " ",1,"{'mt':2,'fonth':2.5,'xpos':85.3,'ypos':22.3}"
			PuPlayer.LabelSet pDMD,"CurrGasMini"," " & VespeneGas & " ",1,"{'mt':2,'fonth':2.5,'xpos':91.3,'ypos':22.3}"
			If LastFarm = 1 Then
				PuPlayer.LabelSet pDMD,"CurrSupplyMini"," " & "128" & " ",1,"{'mt':2,'fonth':2.5,'xpos':97.1,'ypos':22.3}"
			ElseIf SecondFarm = 1 Then
				PuPlayer.LabelSet pDMD,"CurrSupplyMini"," " & "64" & " ",1,"{'mt':2,'fonth':2.5,'xpos':97.1,'ypos':22.3}"
			ElseIf FirstFarm = 1 Then
				PuPlayer.LabelSet pDMD,"CurrSupplyMini"," " & "32" & " ",1,"{'mt':2,'fonth':2.5,'xpos':97.1,'ypos':22.3}"
			Else
				PuPlayer.LabelSet pDMD,"CurrSupplyMini"," " & "16" & " ",1,"{'mt':2,'fonth':2.5,'xpos':97.1,'ypos':22.3}"
			End If
		End If
End Sub

Sub Buildbuldings
	If VespeneGas > 300 and KhaydarinCrystals > 300 Then
		If RaceTerran = True Then 'Terran
			If FirstFarm = 0 and SecondFarm = 0 and LastFarm = 0 and WaitingBuildingNext = 0 Then 'Farm 1 - Cost 300 gas AND 300 crystals (autobuilds)
				WaitingBuildingNext = 1
				FirstFarm = 1
				VespeneGas = VespeneGas - 300
				KhaydarinCrystals = KhaydarinCrystals - 300
				UpdatePupVariables()
				PlaySound "SFX_SupplyDepotsTerran"
				pupevent 790
			Elseif FirstFarm = 1 and SecondFarm = 0 and LastFarm = 0 and WaitingBuildingNext = 0 Then 'Farm 2 - Cost 300 gas AND 300 crystals (autobuilds)
				WaitingBuildingNext = 1
				SecondFarm = 1
				VespeneGas = VespeneGas - 300
				KhaydarinCrystals = KhaydarinCrystals - 300
				UpdatePupVariables()
				PlaySound "SFX_SupplyDepotsTerran"
				pupevent 791
			Elseif FirstFarm = 1 and SecondFarm = 1 and LastFarm = 0 and WaitingBuildingNext = 0 Then 'Farm 3 - Cost 300 gas AND 300 crystals (autobuilds)
				WaitingBuildingNext = 1
				LastFarm = 1
				VespeneGas = VespeneGas - 300
				KhaydarinCrystals = KhaydarinCrystals - 300
				UpdatePupVariables()
				PlaySound "SFX_SupplyDepotsTerran"
				pupevent 792
			End If
		ElseIf RaceProtoss = True Then 'Protoss
			If FirstFarm = 0 and SecondFarm = 0 and LastFarm = 0 and WaitingBuildingNext = 0 Then 'Pylon 1 - Cost 300 gas AND 300 crystals (autobuilds)
				WaitingBuildingNext = 1
				FirstFarm = 1
				VespeneGas = VespeneGas - 300
				KhaydarinCrystals = KhaydarinCrystals - 300
				UpdatePupVariables()
				PlaySound "SFX_SupplyDepotsProtoss"
				pupevent 793
			Elseif FirstFarm = 1 and SecondFarm = 0 and LastFarm = 0 and WaitingBuildingNext = 0 Then 'Pylon 2 - Cost 300 gas AND 300 crystals (autobuilds)
				WaitingBuildingNext = 1
				SecondFarm = 1
				VespeneGas = VespeneGas - 300
				KhaydarinCrystals = KhaydarinCrystals - 300
				UpdatePupVariables()
				PlaySound "SFX_SupplyDepotsProtoss"
				pupevent 794
			Elseif FirstFarm = 1 and SecondFarm = 1 and LastFarm = 0 and WaitingBuildingNext = 0 Then 'Pylon 3 - Cost 300 gas AND 300 crystals (autobuilds)
				WaitingBuildingNext = 1
				LastFarm = 1
				VespeneGas = VespeneGas - 300
				KhaydarinCrystals = KhaydarinCrystals - 300
				UpdatePupVariables()
				PlaySound "SFX_SupplyDepotsProtoss"
				pupevent 795
			End If
		ElseIf RaceZerg = True Then 'Zerg
			If FirstFarm = 0 and SecondFarm = 0 and LastFarm = 0 and WaitingBuildingNext = 0 Then 'Overlord 1 - Cost 300 gas AND 300 crystals (autobuilds)
				WaitingBuildingNext = 1
				FirstFarm = 1
				VespeneGas = VespeneGas - 300
				KhaydarinCrystals = KhaydarinCrystals - 300
				UpdatePupVariables()
				PlaySound "SFX_SupplyDepotsZerg"
				pupevent 796
			Elseif FirstFarm = 1 and SecondFarm = 0 and LastFarm = 0 and WaitingBuildingNext = 0 Then 'Overlord 2 - Cost 300 gas AND 300 crystals (autobuilds)
				WaitingBuildingNext = 1
				SecondFarm = 1
				VespeneGas = VespeneGas - 300
				KhaydarinCrystals = KhaydarinCrystals - 300
				UpdatePupVariables()
				PlaySound "SFX_SupplyDepotsZerg"
				pupevent 797
			Elseif FirstFarm = 1 and SecondFarm = 1 and LastFarm = 0 and WaitingBuildingNext = 0 Then 'Overlord 3 - Cost 300 gas AND 300 crystals (autobuilds)
				WaitingBuildingNext = 1
				LastFarm = 1
				VespeneGas = VespeneGas - 300
				KhaydarinCrystals = KhaydarinCrystals - 300
				UpdatePupVariables()
				PlaySound "SFX_SupplyDepotsZerg"
				pupevent 798
			End If
		End If
	End If
	If VespeneGas > 500 and KhaydarinCrystals > 500 and CommandCenter.visible = 0 and Nexus.visible = 0 and Hive.visible = 0 Then
		If RaceTerran = True Then 'Terran Command Center - Cost 500 gas AND 500 crystals (autobuilds)
			Building_timer.Enabled = true
			WarpTimer1.Enabled = true
			WaitingBuildingNext = 0
			VespeneGas = VespeneGas - 500
			KhaydarinCrystals = KhaydarinCrystals - 500
			UpdatePupVariables()
			PlaySound "SFX_BuildingProgressTerran"
		ElseIf RaceProtoss = True Then 'Protoss Nexus - Cost 500 gas AND 500 crystals (autobuilds)
			Building_timer.Enabled = true
			WarpTimer1.Enabled = true
			WaitingBuildingNext = 0
			VespeneGas = VespeneGas - 500
			KhaydarinCrystals = KhaydarinCrystals - 500
			UpdatePupVariables()
			PlaySound "SFX_BuildingProgressProtoss"
		ElseIf RaceZerg = True Then 'Zerg Hive - Cost 500 gas AND 500 crystals (autobuilds)
			Building_timer.Enabled = true
			WarpTimer1.Enabled = true
			WaitingBuildingNext = 0
			VespeneGas = VespeneGas - 500
			KhaydarinCrystals = KhaydarinCrystals - 500
			UpdatePupVariables()
			PlaySound "SFX_BuildingProgressZerg"
		End If
	End If
	If VespeneGas > 600 and KhaydarinCrystals > 600 and StarPort.visible = 0 and Gateway.visible = 0 and Spawing_Pool.visible = 0 Then
		If RaceTerran = True Then 'Terran StarPort - Cost 600 gas AND 600 crystals (autobuilds)
			Building_timer.Enabled = true
			WarpTimer2.Enabled = true
			WaitingBuildingNext = 0
			VespeneGas = VespeneGas - 600
			KhaydarinCrystals = KhaydarinCrystals - 600
			UpdatePupVariables()
			PlaySound "SFX_BuildingProgressTerran"
		ElseIf RaceProtoss = True Then 'Protoss Gateway - Cost 600 gas AND 600 crystals (autobuilds)
			Building_timer.Enabled = true
			WarpTimer2.Enabled = true
			WaitingBuildingNext = 0
			VespeneGas = VespeneGas - 600
			KhaydarinCrystals = KhaydarinCrystals - 600
			UpdatePupVariables()
			PlaySound "SFX_BuildingProgressProtoss"
		ElseIf RaceZerg = True Then 'Zerg Spawing Pool - Cost 600 gas AND 600 crystals (autobuilds)
			Building_timer.Enabled = true
			WarpTimer2.Enabled = true
			WaitingBuildingNext = 0
			VespeneGas = VespeneGas - 600
			KhaydarinCrystals = KhaydarinCrystals - 600
			UpdatePupVariables()
			PlaySound "SFX_BuildingProgressZerg"
		End If
	End If
	If VespeneGas > 700 and KhaydarinCrystals > 700 and Barracks.visible = 0 and StarGate.visible = 0 and Spire.visible = 0 Then
		If RaceTerran = True Then 'Terran Barracks - Cost 700 gas AND 700 crystals (autobuilds)
			Building_timer.Enabled = true
			WarpTimer3.Enabled = true
			WaitingBuildingNext = 0
			VespeneGas = VespeneGas - 700
			KhaydarinCrystals = KhaydarinCrystals - 700
			UpdatePupVariables()
			PlaySound "SFX_BuildingProgressTerran"
		ElseIf RaceProtoss = True Then 'Protoss StarGate - Cost 700 gas AND 700 crystals (autobuilds)
			Building_timer.Enabled = true
			WarpTimer3.Enabled = true
			WaitingBuildingNext = 0
			VespeneGas = VespeneGas - 700
			KhaydarinCrystals = KhaydarinCrystals - 700
			UpdatePupVariables()
			PlaySound "SFX_BuildingProgressProtoss"
		ElseIf RaceZerg = True Then 'Zerg Spire - Cost 700 gas AND 700 crystals (autobuilds)
			Building_timer.Enabled = true
			WarpTimer3.Enabled = true
			WaitingBuildingNext = 0
			VespeneGas = VespeneGas - 700
			KhaydarinCrystals = KhaydarinCrystals - 700
			UpdatePupVariables()
			PlaySound "SFX_BuildingProgressZerg"
		End If
	End If
End Sub

'*********
' Switches
'*********

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 49 : RandomSoundBumperTop Bumper1 : End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 51 : RandomSoundBumperMiddle Bumper2 : End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 50 : RandomSoundBumperBottom Bumper3 : End Sub



'rollover 'was all fx_switch
Sub sw16_Hit:Controller.Switch(16) = 1:sw16p.RotX = 70: BIPL=1 : End Sub
Sub sw16_UnHit:Controller.Switch(16) = 0:sw16p.RotX = 90: BIPL=0 : End Sub

Sub sw41_Hit:Controller.Switch(41) = 1:sw41p.RotX = 70:End Sub
Sub sw41_UnHit:Controller.Switch(41) = 0:sw41p.RotX = 90:End Sub

Sub sw42_Hit:Controller.Switch(42) = 1:sw42p.RotX = 70:End Sub
Sub sw42_UnHit:Controller.Switch(42) = 0:sw42p.RotX = 90:End Sub

Sub sw43_Hit:Controller.Switch(43) = 1:sw43p.RotX = 70:End Sub
Sub sw43_UnHit:Controller.Switch(43) = 0:sw43p.RotX = 90:End Sub

Sub sw57_Hit:Controller.Switch(57) = 1:sw57p.RotX = 70:End Sub
Sub sw57_UnHit:Controller.Switch(57) = 0:sw57p.RotX = 90:End Sub

Sub sw58_Hit:Controller.Switch(58) = 1:sw58p.RotX = 70: activeball.vely=activeball.vely*0.8: activeball.angmomz=0: End Sub
Sub sw58_UnHit:Controller.Switch(58) = 0:sw58p.RotX = 90:End Sub

Sub sw61_Hit:Controller.Switch(61) = 1:sw61p.RotX = 70: activeball.vely=activeball.vely*0.8: activeball.angmomz=0: End Sub
Sub sw61_UnHit:Controller.Switch(61) = 0:sw61p.RotX = 90:End Sub

Sub sw60_Hit:Controller.Switch(60) = 1:sw60p.RotX = 70:End Sub
Sub sw60_UnHit:Controller.Switch(60) = 0:sw60p.RotX = 90:End Sub

'orbit/optos
Sub sw47_Hit:Controller.Switch(47)= 1:End Sub
Sub sw47_UnHit:Controller.Switch(47) = 0:End Sub

Sub sw48_Hit:Controller.Switch(48)= 1:End Sub
Sub sw48_UnHit:Controller.Switch(48) = 0:End Sub


'undertrough

'skillshot
Sub sw9_Hit:Controller.Switch(9)= 1:RandomSoundHole:WireRampOff:End Sub
Sub sw9_UnHit:Controller.Switch(9) = 0:End Sub

'bugexit
'Sub sw10a_Hit:RandomSoundHole:End Sub 'Use fleep? RandomSoundDelayedBallDropOnPlayfield
Sub sw10a_Hit:RandomSoundDelayedBallDropOnPlayfield(ActiveBall):End Sub 'Use fleep? RandomSoundDelayedBallDropOnPlayfield


'bumperexit
Sub sw40_Hit:Controller.Switch(40)= 1:PlaySoundAtBall "fx_tedhit":End Sub
Sub sw40_UnHit:Controller.Switch(40) = 0:End Sub


'targets 'was all fx_switch sounds. Now using targets collection sounds
'Zergling - Zealot - Ghost
Sub T17_Hit:vpmTimer.pulseSw 17: T17p.RotX = T17p.RotX +4:Me.TimerEnabled = 1:DeathSound 1:Splatter1.visible = 1:Splatter1.RotZ = Rnd * 360:SplatterFunction1:End Sub
Sub T17_Timer:T17p.RotX = 90:Me.TimerEnabled = 0:End Sub

'Hydralist - Immortal - Marine
Sub T18_Hit:vpmTimer.pulseSw 18: T18p.RotX = T18p.RotX +4:Me.TimerEnabled = 1:DeathSound 2:Splatter2.visible = 1:Splatter2.RotZ = Rnd * 360:SplatterFunction1:End Sub
Sub T18_Timer:T18p.RotX = 90:Me.TimerEnabled = 0:End Sub

'Baneling - Archon - Reaper
Sub T19_Hit:vpmTimer.pulseSw 19: T19p.RotX = T19p.RotX +4:Me.TimerEnabled = 1:DeathSound 3:Splatter3.visible = 1:Splatter3.RotZ = Rnd * 360:SplatterFunction1:End Sub
Sub T19_Timer:T19p.RotX = 90:Me.TimerEnabled = 0:End Sub

'Hydralist - Immortal - Marine
Sub T20_Hit:vpmTimer.pulseSw 20: T20p.RotX = T20p.RotX +4:Me.TimerEnabled = 1:DeathSound 2:Splatter4.visible = 1:Splatter4.RotZ = Rnd * 360:SplatterFunction1:End Sub
Sub T20_Timer:T20p.RotX = 90:Me.TimerEnabled = 0:End Sub

'Baneling - Archon - Reaper
Sub T21_Hit:vpmTimer.pulseSw 21: T21p.RotX = T21p.RotX +4:Me.TimerEnabled = 1:DeathSound 3:Splatter14.visible = 1:Splatter14.RotZ = Rnd * 360:SplatterFunction2:End Sub
Sub T21_Timer:T21p.RotX = 90:Me.TimerEnabled = 0:End Sub

'Hydralist - Immortal - Marine
Sub T22_Hit:vpmTimer.pulseSw 22: T22p.RotX = T22p.RotX +4:Me.TimerEnabled = 1:DeathSound 2:Splatter13.visible = 1:Splatter13.RotZ = Rnd * 360:SplatterFunction2:End Sub
Sub T22_Timer:T22p.RotX = 90:Me.TimerEnabled = 0:End Sub

'Zergling - Zealot - Ghost
Sub T23_Hit:vpmTimer.pulseSw 23: T23p.RotX = T23p.RotX +4:Me.TimerEnabled = 1:DeathSound 1:Splatter12.visible = 1:Splatter12.RotZ = Rnd * 360:SplatterFunction2:End Sub
Sub T23_Timer:T23p.RotX = 90:Me.TimerEnabled = 0:End Sub

'Hydralist - Immortal - Marine
Sub T24_Hit:vpmTimer.pulseSw 24: T24p.RotX = T24p.RotX +4:Me.TimerEnabled = 1:DeathSound 2:Splatter11.visible = 1:Splatter11.RotZ = Rnd * 360:SplatterFunction2:End Sub
Sub T24_Timer:T24p.RotX = 90:Me.TimerEnabled = 0:End Sub

'Hydralist - Immortal - Marine
Sub T27_Hit:vpmTimer.pulseSw 27: T27p.RotX = T27p.RotX +4:Me.TimerEnabled = 1:DeathSound 2:Splatter5.visible = 1:Splatter5.RotZ = Rnd * 360:SplatterFunction2:End Sub
Sub T27_Timer:T27p.RotX = 90:Me.TimerEnabled = 0:End Sub

'Ultralisk - Reaver - Tank
Sub T28_Hit:vpmTimer.pulseSw 28: T28p.RotX = T28p.RotX +4:Me.TimerEnabled = 1:DeathSound 4:Splatter6.visible = 1:Splatter6.RotZ = Rnd * 360:SplatterFunction1:End Sub
Sub T28_Timer:T28p.RotX = 90:Me.TimerEnabled = 0:End Sub

'Zergling - Zealot - Ghost
Sub T29_Hit:vpmTimer.pulseSw 29: T29p.RotX = T29p.RotX +4:Me.TimerEnabled = 1:DeathSound 1:Splatter7.visible = 1:Splatter7.RotZ = Rnd * 360:SplatterFunction1:End Sub
Sub T29_Timer:T29p.RotX = 90:Me.TimerEnabled = 0:End Sub

'Zergling - Zealot - Ghost
Sub T30_Hit:vpmTimer.pulseSw 30: T30p.RotX = T30p.RotX +4:Me.TimerEnabled = 1:DeathSound 1:Splatter8.visible = 1:Splatter8.RotZ = Rnd * 360:SplatterFunction2:End Sub
Sub T30_Timer:T30p.RotX = 90:Me.TimerEnabled = 0:End Sub

'Ultralisk - Reaver - Tank
Sub T31_Hit:vpmTimer.pulseSw 31: T31p.RotX = T31p.RotX +4:Me.TimerEnabled = 1:DeathSound 4:Splatter9.visible = 1:Splatter9.RotZ = Rnd * 360:SplatterFunction2:End Sub
Sub T31_Timer:T31p.RotX = 90:Me.TimerEnabled = 0:End Sub

'Hydralist - Immortal - Marine
Sub T32_Hit:vpmTimer.pulseSw 32: T32p.RotX = T32p.RotX +4:Me.TimerEnabled = 1:DeathSound 2:Splatter10.visible = 1:Splatter10.RotZ = Rnd * 360:SplatterFunction1:End Sub
Sub T32_Timer:T32p.RotX = 90:Me.TimerEnabled = 0:End Sub


Sub sw25_Hit:vpmTimer.PulseSW 25: End Sub 
Sub sw52_Hit:vpmTimer.PulseSW 52: End Sub
Sub sw53_Hit:vpmTimer.PulseSW 53: End Sub

Sub sw26_Hit:vpmTimer.PulseSW 26: WireRampOff: End Sub
Sub sw26_UnHit:WireRampOn False: End Sub


Sub DeathSound (targetenemy)
	If (pf_TerranVsProtoss.visible = 0 and pf_ProtossVsZerg.visible = 0 and pf_ProtossVsTerran.visible = 0 and pf_ZergVsProtoss.visible = 0 and pf_ZergVsTerran.visible = 0) or (RaceProtoss and pf_ProtossVsTerran.visible = 0) then
		Select Case targetenemy
			Case 1: PlaySound "SFX_Z_ZerglingDeath1"                  'Zergling - Zealot - Ghost
			Case 2: PlaySound "SFX_Z_HydralistDeath"&Int(Rnd*2)+1     'Hydralist - Immortal - Marine
			Case 3: PlaySound "SFX_Z_BanelingDeath"&Int(Rnd*2)+1      'Baneling - Archon - Reaper
			Case 4: PlaySound "SFX_Z_UltraliskDeath"&Int(Rnd*3)+1     'Ultralisk - Reaver - Tank
		End Select
	Elseif RaceZerg and pf_ZergVsTerran.visible = 0 then 'Protoss
		Select Case targetenemy
			Case 1: PlaySound "SFX_P_ZealotDeath"                     'Zergling - Zealot - Ghost
			Case 2: PlaySound "SFX_P_ImmortalDeath"                   'Hydralist - Immortal - Marine
			Case 3: PlaySound "SFX_P_ArchonDeath"&Int(Rnd*2)+1        'Baneling - Archon - Reaper
			Case 4: PlaySound "SFX_P_ReaverDeath"                     'Ultralisk - Reaver - Tank
		End Select
	Elseif (RaceProtoss and pf_ProtossVsZerg.visible = 0) or (RaceZerg and pf_ZergVsProtoss.visible = 0) then 'Terran
		Select Case targetenemy
			Case 1: PlaySound "SFX_T_GhostDeath"&Int(Rnd*3)+1         'Zergling - Zealot - Ghost
			Case 2: PlaySound "SFX_T_MarineDeath"&Int(Rnd*2)+1        'Hydralist - Immortal - Marine
			Case 3: PlaySound "SFX_T_ReaperDeath"&Int(Rnd*3)+1        'Baneling - Archon - Reaper
			Case 4: PlaySound "SFX_T_TankDeath"                       'Ultralisk - Reaver - Tank
		End Select
	Else 'Protoss
		Select Case targetenemy
			Case 1: PlaySound "SFX_P_ZealotDeath"                     'Zergling - Zealot - Ghost
			Case 2: PlaySound "SFX_P_ImmortalDeath"                   'Hydralist - Immortal - Marine
			Case 3: PlaySound "SFX_P_ArchonDeath"&Int(Rnd*2)+1        'Baneling - Archon - Reaper
			Case 4: PlaySound "SFX_P_ReaverDeath"                     'Ultralisk - Reaver - Tank
		End Select
	End If
End Sub

Dim opacityVar2
Sub SplatterFunction1 ()
	opacityVar2 = 100
	SplatterFadeTimer1.Enabled = True
End Sub

Dim opacityVar3
Sub SplatterFunction2 ()
	opacityVar3 = 100
	SplatterFadeTimer2.Enabled = True
End Sub

Sub SplatterFadeTimer1_Timer ()
	opacityVar2 = opacityVar2 - 1
	Splatter1.opacity = opacityVar2
	Splatter2.opacity = opacityVar2
	Splatter3.opacity = opacityVar2
	Splatter4.opacity = opacityVar2
	Splatter6.opacity = opacityVar2
	Splatter7.opacity = opacityVar2
	Splatter10.opacity = opacityVar2
	If opacityVar2 <= 20 Then
		SplatterFadeTimer1.Enabled = False
		Splatter1.visible = 0
		Splatter2.visible = 0
		Splatter3.visible = 0
		Splatter4.visible = 0
		Splatter6.visible = 0
		Splatter7.visible = 0
		Splatter10.visible = 0
	End If
End Sub

Sub SplatterFadeTimer2_Timer ()
	opacityVar3 = opacityVar3 - 1
	Splatter5.opacity = opacityVar3
	Splatter8.opacity = opacityVar3
	Splatter9.opacity = opacityVar3
	Splatter11.opacity = opacityVar3
	Splatter12.opacity = opacityVar3
	Splatter13.opacity = opacityVar3
	Splatter14.opacity = opacityVar3
	If opacityVar3 <= 20 Then
		SplatterFadeTimer2.Enabled = False
		Splatter5.visible = 0
		Splatter8.visible = 0
		Splatter9.visible = 0
		Splatter11.visible = 0
		Splatter12.visible = 0
		Splatter13.visible = 0
		Splatter14.visible = 0
	End If
End Sub

'***********
' Update GI
'***********
If RenderingMode = 2 Then	'VR
	ramp_off.material = "ramps_off_vr"
	ramp_on.material = "ramps_vr"
	ramp_off.size_x = 18.5005
	ramp_off.size_y = 18.5001
	ramp_off.size_z = 18.5001
	ramp_off.x = -0.015

	plasticsslingshots_on.size_x = 18.5001
	plasticsslingshots_on.size_y = 18.5001
	plasticsslingshots_on.size_z = 18.5001

	plasticsrest_on.size_x = 18.5001
	plasticsrest_on.size_y = 18.5001
	plasticsrest_on.size_z = 18.5001
	plasticsrest_on.y = 0.01

	plasticsset2_on.size_x = 18.5001
	plasticsset2_on.size_y = 18.5001
	plasticsset2_on.size_z = 18.5001

	plastics_trans_on.size_x = 18.5001
	plastics_trans_on.size_y = 18.5001
	plastics_trans_on.size_z = 18.5001

	plastics_bumpercaps_on.size_x = 18.5001
	plastics_bumpercaps_on.size_y = 18.5001
	plastics_bumpercaps_on.size_z = 18.5001

end if

Dim MaterialAmount, x, StepM

Sub UpdateGi(no, enabled)
	If No = False Then
		Select Case no
			Case 0
				SetLamp 200, enabled
			end select
	End if

		Select Case no
			Case 0
			if enabled then
				for StepM  =  1 to 998 Step +1:next
				for each x in targetgroup: x.image = "targets_on":next
				for each x in imageswap: x.image = Replace(x.image,"_off","_on"):next
				LeftFlipperP.blenddisablelighting = 1.5
				RightFlipperP.blenddisablelighting = 1.5
				RightFlipperPS.blenddisablelighting = 1.0
			else
				for StepM  =  998 to 1 Step -1:next
				for each x in targetgroup: x.image = "targets_off":next
				for each x in imageswap: x.image = Replace(x.image,"_on","_off"):next
				   LeftFlipperP.blenddisablelighting = 0.5
				RightFlipperP.blenddisablelighting = 0.5
				RightFlipperPS.blenddisablelighting = 0.5
			End if
	End Select
	MaterialAmount = stepm / 1000




	UpdateMaterial "plastics_textured" ,0,0,1,1,1,1,MaterialAmount,RGB(255,255,255),0,0,False,True,0,0,0,0
	If RenderingMode = 2 Then	'VR
		UpdateMaterial "ramps_vr" 			   ,0,0,1,0.7,MaterialAmount,MaterialAmount * 0.5,MaterialAmount * 0.2 ,RGB(200,200,200),0,0,False,True,0,0,0,0
	Else
		UpdateMaterial "ramps" 			   ,0,0,1,1,MaterialAmount,MaterialAmount,MaterialAmount,RGB(255,255,255),0,0,False,True,0,0,0,0
	end if
	UpdateMaterial "plastics_trans" ,0,1,1,1,MaterialAmount,MaterialAmount,MaterialAmount,RGB(255,255,255),0,0,False,True,0,0,0,0

'Wrap/Shininess/use_image/thickness/edge_brightness/edgeopacity/alpha/BaseColor/
End Sub



'*******************************************************************************************************************************************
'*******************************************************************************************************************************************
'Flasher_Strobing
'*******************************************************************************************************************************************
'The command is called from the solenoid with "FlashonRed" or "FlashblinkingRed" or "FlashOffRed" similar to the "states" of a single light
'Flashtimer 1+2 controls the blinking speed of the red flashers Lights
'Flashtimer 3+4 controls the blinking speed of the green flashers Lights
'Flashtimer 5+6 controls the blinking speed of the yellow flashers Lights
'Flashtimer 7+8 controls the blinking speed of the blue flashers Lights
'Fadespeedup and Fadespeeddown only controls the fading of the second primitive with the "lit" texture#
'FadeTimer updates the "lit" material of the flasherdomes
'*******************************************************************************************************************************************
'*******************************************************************************************************************************************

Dim bulb, fbulb, FlasherOnR, FlasherOffR, FlasherOnG, FlasherOffG, FlasherOnY, FlasherOffY, FlasherOnB, FlasherOffB
Dim FadeStepR, FadeStepG, FadeStepY, FadeStepB, materialstepR, MaterialstepG, MaterialStepY, MaterialStepB, fadespeedup, fadespeeddown
Dim FadeR, FadeG, FadeY, FadeB
FadestepR = 0
FadestepG = 0
FadestepY = 0
FadestepB = 0
Fadespeedup = 33
Fadespeeddown = 18		'high is fast, lower is slower



Sub FlashOffRed()
	FlasherOffR = True
	for each bulb in StrobeFlashersLRed
	bulb.state = 0
	for each fbulb in StrobeFlashersFRed
	fbulb.visible = False
	next
	next
End Sub

Sub FlashOffGreen()
	FlasherOffG = True
	for each bulb in StrobeFlashersLGreen
	bulb.state = 0
	for each fbulb in StrobeFlashersFGreen
	fbulb.visible = False
	next
	next
End Sub

Sub FlashOffYellow()
	FlasherOffY = True
	for each bulb in StrobeFlashersLYellow
	bulb.state = 0
	for each fbulb in StrobeFlashersFYellow
	fbulb.visible = False
	next
	next
End Sub

Sub FlashOffBlue()
	FlasherOffB = True
	for each bulb in StrobeFlashersLBlue
	bulb.state = 0
	for each fbulb in StrobeFlashersFBlue
	fbulb.visible = False
	next
	next
End Sub

'Sub FlashOn()
'	FlasherOn = True
'	for each bulb in StrobeFlashersL
'	bulb.state = 1
'	for each fbulb in StrobeFlashersF
'	fbulb.visible = True
'	next
'	next
'End Sub

Sub FlashBlinkingRed()
	FlasherOnR = False
	FlasherOffR = False
	FlasherTimer2.Enabled = True
End Sub

Sub FlashBlinkingGreen()
	FlasherOnG = False
	FlasherOffG = False
	FlasherTimer4.Enabled = True
End Sub


Sub FlashBlinkingYellow()
	FlasherOnY = False
	FlasherOffY = False
	FlasherTimer6.Enabled = True
End Sub


Sub FlashBlinkingBlue()
	FlasherOnB = False
	FlasherOffB = False
	FlasherTimer8.Enabled = True
End Sub



'FlasherStrobing off/on...

Sub FlasherTimer1_Timer()

	FadeR = 0
'	capsred.image = "CapsRed_On"
	for each fbulb in StrobeFlashersFRed
	fbulb.visible = 0
	for each bulb in StrobeFlashersLRed
	bulb.state = 0
	FlasherTimer1.Enabled = 0
	If FlasherOffR = False then FlasherTimer2.Enabled = 1
	next
	next
End Sub

Sub FlasherTimer2_Timer()

	FadeR = 1
'	CapsRed.image = "CapsRed_lit"
	for each fbulb in StrobeFlashersFRed
	fbulb.visible = 1
	for each bulb in StrobeFlashersLRed
	bulb.state = 1
	If FlasherOnR = False then FlasherTimer1.Enabled = 1: Else 
	FlasherTimer2.Enabled = 0
	next
	next
End Sub




Sub FlasherTimer3_Timer()

	FadeG = 0
'	capsgreen.image = "CapsGreen_on"
	for each fbulb in StrobeFlashersFGreen
	fbulb.visible = 0
	for each bulb in StrobeFlashersLGreen
	bulb.state = 0
	FlasherTimer3.Enabled = 0
	If FlasherOffG = False then FlasherTimer4.Enabled = 1
	next
	next
End Sub

Sub FlasherTimer4_Timer()

	FadeG = 1
'	capsgreen.image = "CapsGreen_Lit"
	for each fbulb in StrobeFlashersFGreen
	fbulb.visible = 1
	for each bulb in StrobeFlashersLGreen
	bulb.state = 1
	If FlasherOnG = False then FlasherTimer3.Enabled = 1: Else 
	FlasherTimer4.Enabled = 0
	next
	next
End Sub




Sub FlasherTimer5_Timer()

	FadeY = 0
'	CapsYellow.image = "CapsYellow_On"
	for each fbulb in StrobeFlashersFYellow
	fbulb.visible = 0
	for each bulb in StrobeFlashersLYellow
	bulb.state = 0
	FlasherTimer5.Enabled = 0
	If FlasherOffY = False then FlasherTimer6.Enabled = 1
	next
	next
End Sub

Sub FlasherTimer6_Timer()

	FadeY = 1
'	Capsyellow.image = "CapsYellow_Lit"
	for each fbulb in StrobeFlashersFYellow
	fbulb.visible = 1
	for each bulb in StrobeFlashersLYellow
	bulb.state = 1
	If FlasherOnY = False then FlasherTimer5.Enabled = 1: Else 
	FlasherTimer6.Enabled = 0
	next
	next
End Sub



Sub FlasherTimer7_Timer()

	FadeB = 0
'	CapsBlue.image = "capsblue_on"
	for each fbulb in StrobeFlashersFBlue
	fbulb.visible = 0
	for each bulb in StrobeFlashersLBlue
	bulb.state = 0
	FlasherTimer7.Enabled = 0
	If FlasherOffB = False then FlasherTimer8.Enabled = 1
	next
	next
End Sub

Sub FlasherTimer8_Timer()

	FadeB = 1
'	CapsBlue.image = "capsblue_lit"
	for each fbulb in StrobeFlashersFBlue
	fbulb.visible = 1
	for each bulb in StrobeFlashersLBlue
	bulb.state = 1
	If FlasherOnB = False then FlasherTimer7.Enabled = 1: Else 
	FlasherTimer8.Enabled = 0
	next
	next
End Sub



'********************************************************************************************
' FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light or flasher will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Return to previous State
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

'**********************************************
'*******FadeTimer Moved to the Gametimer*******
'**********************************************

'Sub FadeTimer_Timer()
'	if fadeb = 0 and fadestepB > 0 then fadestepB = fadestepB -fadespeeddown
'	if fadeb = 1 And FadeStepB < 999 then fadestepB = fadestepB +fadespeedup
'	MaterialStepB = FadestepB/1000
'	UpdateMaterial "flashercapslitblue",0,1,1,1,materialstepB,materialstepB,materialstepB,RGB(255,255,255),0,0,False,True,0,0,0,0
'
'	if fader = 0 and fadestepR > 0 then fadestepR = fadestepR -fadespeeddown
'	if fader = 1 And FadeStepR < 999 then fadestepR = fadestepR +fadespeedup
'	MaterialStepR = FadestepR/1000
'	UpdateMaterial "flashercapslitred",0,1,1,1,materialstepR,materialstepR,materialstepR,RGB(255,255,255),0,0,False,True,0,0,0,0
'
'	if fadey = 0 and fadestepY > 0 then fadestepY = fadestepY -fadespeeddown
'	if fadey = 1 And FadeStepY < 999 then fadestepY = fadestepY +fadespeedup
'	MaterialStepY = FadestepY/1000
'	UpdateMaterial "flashercapslityellow",0,1,1,1,materialstepY,materialstepY,materialstepY,RGB(255,255,255),0,0,False,True,0,0,0,0
'
'	if fadeG = 0 and fadestepG > 0 then fadestepG = fadestepG -fadespeeddown
'	if fadeG = 1 And FadeStepG < 999 then fadestepG = fadestepG +fadespeedup
'	MaterialStepG = FadestepG/1000
'	UpdateMaterial "flashercapslitgreen",0,1,1,1,materialstepG,materialstepG,materialstepG,RGB(255,255,255),0,0,False,True,0,0,0,0
'End Sub



'Ball reflections - iaakki
addBallReflections


sub addBallReflections
	dim rr,gg,bb
	dim origColori

	for each xx in GILamps
		origColori = round(xx.colorfull,0)

		rr = (origColori Mod 256)
		gg = (origColori \ 256) Mod 256
		bb = (origColori \ 65536) Mod 256

		xx.color = rgb(rr/4,gg/4,bb/4)
	Next

	for each xx in BallRefl
		origColori = round(xx.colorfull,0)

		rr = (origColori Mod 256)
		gg = (origColori \ 256) Mod 256
		bb = (origColori \ 65536) Mod 256

'		msgbox rr & "." & gg & "." & bb
		xx.color = rgb(rr,gg,bb)
	Next

end sub

sub removeBallReflections
	for each xx in GILamps:xx.color = RGB(0,0,0):Next
	for each xx in BallRefl:xx.color = RGB(0,0,0):Next
end sub


'**********************************************************
'     JP's Flasher Fading for VPX and Vpinmame
'       (Based on Pacdude's Fading Light System)
' This is a fast fading for the Flashers in vpinmame tables
'  just 4 steps, like in Pacdude's original script.
' Included the new Modulated flashers & Lights for WPC
'**********************************************************

Dim LampState(200), FadingState(200), FlashLevel(200)

InitLamps() ' turn off the lights and flashers and reset them to the default parameters

' vpinmame Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp)Then
        For ii = 0 To UBound(chgLamp)
			LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
            FadingState(chgLamp(ii, 0)) = chgLamp(ii, 1) + 3  'fading step
        Next
    End If
    UpdateLamps
End Sub

Sub UpdateLamps

    lampm 1, l1
    lampm 1, l1b
    flash 1, lf1
    lampm 2, l2
    lampm 2, l2b
    flash 2, lf2
    lampm 3, l3
    lampm 3, l3b
    flash 3, lf3
    lampm 4, l4
    lampm 4, l4b
    flash 4, lf4
    lampm 5, l5
    lampm 5, l5b
    flash 5, lf5
    lampm 6, l6
    lampm 6, l6b
    flash 6, lf6
    lampm 7, l7
    lampm 7, l7b
    flash 7, lf7
    lampm 8, l8
    lampm 8, l8b
    flash 8, lf8
    lampm 9, l9
    lampm 9, l9b
    flash 9, lf9
    lampm 10, l10
    lampm 10, l10b
    flash 10, lf10

	lampm 11, l11
	lampm 11, l11b
	flash 11, lf11
    lampm 12, l12
    lampm 12, l12b
    flash 12, lf12
    lampm 13, l13a
    lampm 13, l13b
    flash 13, lf13
    lampm 14, l14a
    lampm 14, l14b
    flash 14, lf14
    lampm 15, l15a
    lampm 15, l15b
    flash 15, lf15
    lampm 16, l16a
    lampm 16, l16b
    flash 16, lf16
    lampm 17, l17
    flash 17, lf17
    lampm 18, l18
    lampm 18, l18b
    flash 18, lf18
    lampm 19, l19
    flash 19, lf19
    lampm 20, l20
    lampm 20, l20b
    flash 20, lf20
    lampm 21, l21
    flash 21, lf21
    lampm 22, l22
    flash 22, lf22
    lampm 23, l23
    flash 23, lf23
    lampm 24, l24
    flash 24, lf24
    lampm 25, l25
    lampm 25, l25b
    flash 25, lf25
    lampm 26, l26
    lampm 26, l26b
    flash 26, lf26
    lampm 27, l27
    lampm 27, l27b
    flash 27, lf27

    lampm 28, l28
    lampm 28, l28b
    lamp 28, l28a
    lampm 29, l29
    lampm 29, l29b
    lamp 29, l29a
    lampm 30, l30
    lampm 30, l30b
    lamp 30, l30a

    lampm 31, l31
    flash 31, lf31
    lampm 32, l32
    lampm 32, l32b
    flash 32, lf32

    lampm 35, l35
    flashm 35, fl35
    flash 35, fl35a
    lampm 36, l36
    flashm 36, fl36
    flash 36, fl36a
    lampm 37, l37
    lampm 37, l37b
    flashm 37, lf37s
    flash 37, lf37
    lampm 38, l38
    lampm 38, l38b
    flashm 38, lf38s
    flash 38, lf38
    lampm 39, l39
    flashm 39, fl39
    flash 39, fl39a


    lampm 40, l40
    lampm 40, l40a
    lampm 40, l40b
    flashm 40, lf40
    flash 40, lf40a

    lampm 41, l41
    lampm 41, l41b
    flash 41, lf41
    lampm 42, l42
    lampm 42, l42b
    flash 42, lf42
    lampm 43, l43
    flash 43, lf43
    lampm 44, l44
    lampm 44, l44b
    flash 44, lf44
    lampm 45, l45
    lampm 45, l45b
    flash 45, lf45
    lampm 46, l46
    lampm 46, l46b
    flash 46, lf46

    lampm 47, l47
    lampm 47, l47b
    flash 47, fl47
    lampm 48, l48
    lampm 48, l48b
    flash 48, fl48
    flash 49, l49
    flash 50, l50
    flash 51, l51
    flash 52, l52
    flash 53, l53
    flash 54, l54
    flash 55, l55

    flash 57, l57
    flash 58, l58
    flash 59, l59
    flash 60, l60
    flash 61, l61
    flash 62, l62
    flash 63, l63

    flash 65, l65
    flash 66, l66
    flash 67, l67
    flash 68, l68
    flash 69, l69
    flash 70, l70
    flash 71, l71

    flash 73, l73
    flash 74, l74
    flash 75, l75
    flash 76, l76
    flash 77, l77
    flash 78, l78
    flash 79, l79

    lampm 56, l56
    lampm 56, l56b
    flash 56, lf56

    lampm 64, l64
    lampm 64, l64b
    flash 64, lf64

    lampm 72, l72
    lampm 72, l72b
    flash 72, lf72

    lampm 80, l80
    lampm 80, l80b
    flash 80, lf80

'	BrainBug_Flash
'	flash 123, f123
	lampm 123, l123a
	lampm 123, l123b

'	WarriorBug_Flash
	lampm 129, l129
	lampm 129, l129a
	lampm 129, l129b
	flashm 129, f129s
	flashm 129, lf129
	flash 129, lf129b


'	LT_Ramp_Flash
	lampm 130, l130b
	lampm 130, l130c
	lampm 130, l130d
	lampm 130, l130e
	flashm 130, f130a
	flashm 130, f130b
	flashm 130, f130c
	flash 130, f130d

'	RT_Ramp_Flash
	lampm 131, l131b
	lampm 131, l131c
	lampm 131, l131d
	lampm 131, l131e
	flashm 131, f131a
	flashm 131, f131b
	flashm 131, f131c
	flash 131, f131d

	lampm 132, l132
	flash 132, lf132

'General_Illumination
	lampm 200, Light001
	lampm 200, Light002
	lampm 200, Light003
	lampm 200, Light004
	lampm 200, Light005
	lampm 200, Light006
	lampm 200, Light007
	lampm 200, Light008
	lampm 200, Light009
	lampm 200, Light010
	lampm 200, Light011
	lampm 200, Light012
	lampm 200, Light013
	lampm 200, Light014
	lampm 200, Light015
	lampm 200, Light016
	lampm 200, Light017
	lampm 200, Light018
	lampm 200, Light019
	lampm 200, Light020
	lampm 200, Light021
	lampm 200, Light022
	lampm 200, Light023
	lampm 200, Light024
	lampm 200, Light025
	lampm 200, Light026
	lampm 200, Light027
	lampm 200, Light028
	flashm 200, GIFL1
	flashm 200, lfgi001
	flashm 200, lfgi002
	flashm 200, lfgi003
	flash 200, lfgi004

End Sub

' div lamp subs

' Normal Lamp & Flasher subs

Sub InitLamps()
    Dim x
'    LampTimer.Interval = 25 ' flasher fading speed
    LampTimer.Interval = 10 ' flasher fading speed
    LampTimer.Enabled = 1
    For x = 0 to 200
        LampState(x) = 0
        FadingState(x) = 3 ' used to track the fading state
        FlashLevel(x) = 0
    Next
End Sub

Sub SetLamp(nr, value) ' 0 is off, 1 is on
    FadingState(nr) = abs(value) + 3
End Sub

' Lights: used for VPX standard lights, the fading is handled by VPX itself, they are here to be able to make them work together with the flashers

Sub Lamp(nr, object)
    Select Case FadingState(nr)
        Case 4:object.state = 1:FadingState(nr) = 0
        Case 3:object.state = 0:FadingState(nr) = 0
    End Select
End Sub

Sub Lampm(nr, object) ' used for multiple lights, it doesn't change the fading state
    Select Case FadingState(nr)
        Case 4:object.state = 1
        Case 3:object.state = 0
    End Select
End Sub

' Flashers: 4 is on,3,2,1 fade steps. 0 is off

Sub Flash(nr, object)
    Select Case FadingState(nr)
        Case 4:Object.IntensityScale = 1:FadingState(nr) = 0
        Case 3:Object.IntensityScale = 0.66:FadingState(nr) = 2
        Case 2:Object.IntensityScale = 0.33:FadingState(nr) = 1
        Case 1:Object.IntensityScale = 0:FadingState(nr) = 0
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it doesn't change the fading state
    Select Case FadingState(nr)
        Case 4:Object.IntensityScale = 1
        Case 3:Object.IntensityScale = 0.66
        Case 2:Object.IntensityScale = 0.33
        Case 1:Object.IntensityScale = 0
    End Select
End Sub

' Desktop Objects: Reels & texts (you may also use lights on the desktop)

' Reels

Sub Reel(nr, object)
    Select Case FadingState(nr)
        Case 4:object.SetValue 1:FadingState(nr) = 0
        Case 3:object.SetValue 2:FadingState(nr) = 2
        Case 2:object.SetValue 3:FadingState(nr) = 1
        Case 1:object.SetValue 0:FadingState(nr) = 0
    End Select
End Sub

Sub Reelm(nr, object)
    Select Case FadingState(nr)
        Case 4:object.SetValue 1
        Case 3:object.SetValue 2
        Case 2:object.SetValue 3
        Case 1:object.SetValue 0
    End Select
End Sub

Sub NFadeReel(nr, object)
    Select Case FadingState(nr)
        Case 4:object.SetValue 1:FadingState(nr) = 1
        Case 3:object.SetValue 0:FadingState(nr) = 0
    End Select
End Sub

Sub NFadeReelm(nr, object)
    Select Case FadingState(nr)
        Case 4:object.SetValue 1
        Case 3:object.SetValue 0
    End Select
End Sub

'Texts

Sub Text(nr, object, message)
    Select Case FadingState(nr)
        Case 4:object.Text = message:FadingState(nr) = 0
        Case 3:object.Text = "":FadingState(nr) = 0
    End Select
End Sub

Sub Textm(nr, object, message)
    Select Case FadingState(nr)
        Case 4:object.Text = message
        Case 3:object.Text = ""
    End Select
End Sub

' Modulated Subs for the WPC tables

Sub SetModLamp(nr, level)
    FlashLevel(nr) = level / 150 'lights & flashers
End Sub

Sub LampMod(nr, object)          ' modulated lights used as flashers
    Object.IntensityScale = FlashLevel(nr)
    Object.State = 1             'in case it was off
End Sub

Sub FlashMod(nr, object)         'sets the flashlevel from the SolModCallback
    Object.IntensityScale = FlashLevel(nr)
End Sub

'Walls and mostly Primitives used as 4 step fading lights
'a,b,c,d are the images used from on to off

Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingState(nr)
        Case 4:object.image = a:FadingState(nr) = 0 'fading to off...
        Case 3:object.image = b:FadingState(nr) = 2
        Case 2:object.image = c:FadingState(nr) = 1
        Case 1:object.image = d:FadingState(nr) = 0
    End Select
End Sub

Sub FadeObjm(nr, object, a, b, c, d)
    Select Case FadingState(nr)
        Case 4:object.image = a
        Case 3:object.image = b
        Case 2:object.image = c
        Case 1:object.image = d
    End Select
End Sub

Sub NFadeObj(nr, object, a, b)
    Select Case FadingState(nr)
        Case 4:object.image = a:FadingState(nr) = 0 'off
        Case 3:object.image = b:FadingState(nr) = 0 'on
    End Select
End Sub

Sub NFadeObjm(nr, object, a, b)
    Select Case FadingState(nr)
        Case 4:object.image = a
        Case 3:object.image = b
    End Select
End Sub

'***************************************************************
'             Supporting Ball & Sound Functions v3.0
'  includes random pitch in PlaySoundAt and PlaySoundAtBall
'***************************************************************

Dim TableWidth, TableHeight

TableWidth = Table1.width
TableHeight = Table1.height


Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / TableWidth-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function




'******************************************************
'                BALL ROLLING AND DROP SOUNDS
'******************************************************

Const tnob = 4 ' total number of balls
ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Sub InitRolling
        Dim i
        For i = 0 to tnob
                rolling(i) = False
        Next
		bBallInMotion = False
End Sub

'Sub RollingTimer_Timer()
Sub RollingUpdate()
	Dim BOT, b
	BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob
		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next
	bBallInMotion = False

	' exit the sub if no balls on the table
	If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

	For b = 0 to UBound(BOT)
		If BallVel(BOT(b)) > 1 AND BOT(b).z < 30 Then
			rolling(b) = True
			bBallInMotion = True
			PlaySound ("BallRoll_" & b), - 1, VolPlayfieldRoll(BOT(b)) * BallRollVolume * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
			bBallInMotion = False
		End If

		'***Ball Drop Sounds***
		If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If BOT(b).velz > -7 Then
					RandomSoundBallBouncePlayfieldSoft BOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard BOT(b)
				End If                                
			End If
		End If
		If DropCount(b) < 5 Then
				DropCount(b) = DropCount(b) + 1
		End If

		' "Static" Ball Shadows
		If AmbientBallShadowOn = 0 Then
			If BOT(b).Z > 30 Then
				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
			Else
				BallShadowA(b).height=BOT(b).z - BallSize/2 + 5
			End If
			BallShadowA(b).Y = BOT(b).Y + Ballsize/5 + offsetY
			BallShadowA(b).X = BOT(b).X + offsetX
			BallShadowA(b).visible = 1
		End If
	Next
End Sub




'******************************************************
' 	ZRRL: RAMP ROLLING SFX
'******************************************************

'Ball tracking ramp SFX 1.0
'   Reqirements:
'		  * Import A Sound File for each ball on the table for plastic ramps.  Call It RampLoop<Ball_Number> ex: RampLoop1, RampLoop2, ...
'		  * Import a Sound File for each ball on the table for wire ramps. Call it WireLoop<Ball_Number> ex: WireLoop1, WireLoop2, ...
'		  * Create a Timer called RampRoll, that is enabled, with a interval of 100
'		  * Set RampBAlls and RampType variable to Total Number of Balls
'	Usage:
'		  * Setup hit events and call WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'		  * To stop tracking ball
'				 * call WireRampOff
'				 * Otherwise, the ball will auto remove if it's below 30 vp units
'

Dim RampMinLoops
RampMinLoops = 4

' RampBalls
' Setup:  Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RampBalls(6,2)
Dim RampBalls(5,2)
'x,0 = ball x,1 = ID, 2 = Protection against ending early (minimum amount of updates)

'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
' Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
' Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
Dim RampType(5)

Sub WireRampOn(input)
	Waddball ActiveBall, input
	RampRollUpdate
End Sub

Sub WireRampOff()
	WRemoveBall ActiveBall.ID
End Sub

' WaddBall (Active Ball, Boolean)
Sub Waddball(input, RampInput) 'This subroutine is called from WireRampOn to Add Balls to the RampBalls Array
	' This will loop through the RampBalls array checking each element of the array x, position 1
	' To see if the the ball was already added to the array.
	' If the ball is found then exit the subroutine
	Dim x
	For x = 1 To UBound(RampBalls)	'Check, don't add balls twice
		If RampBalls(x, 1) = input.id Then
			If Not IsEmpty(RampBalls(x,1) ) Then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next
	
	' This will itterate through the RampBalls Array.
	' The first time it comes to a element in the array where the Ball Id (Slot 1) is empty.  It will add the current ball to the array
	' The RampBalls assigns the ActiveBall to element x,0 and ball id of ActiveBall to 0,1
	' The RampType(BallId) is set to RampInput
	' RampBalls in 0,0 is set to True, this will enable the timer and the timer is also turned on
	For x = 1 To UBound(RampBalls)
		If IsEmpty(RampBalls(x, 1)) Then
			Set RampBalls(x, 0) = input
			RampBalls(x, 1) = input.ID
			RampType(x) = RampInput
			RampBalls(x, 2) = 0
			'exit For
			RampBalls(0,0) = True
			RampRoll.Enabled = 1	 'Turn on timer
			'RampRoll.Interval = RampRoll.Interval 'reset timer
			Exit Sub
		End If
		If x = UBound(RampBalls) Then	 'debug
			Debug.print "WireRampOn error, ball queue is full: " & vbNewLine & _
			RampBalls(0, 0) & vbNewLine & _
			TypeName(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & "type:" & RampType(1) & vbNewLine & _
			TypeName(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & "type:" & RampType(2) & vbNewLine & _
			TypeName(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & "type:" & RampType(3) & vbNewLine & _
			TypeName(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & "type:" & RampType(4) & vbNewLine & _
			TypeName(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & "type:" & RampType(5) & vbNewLine & _
			" "
		End If
	Next
End Sub

' WRemoveBall (BallId)
Sub WRemoveBall(ID) 'This subroutine is called from the RampRollUpdate subroutine and is used to remove and stop the ball rolling sounds
	'   Debug.Print "In WRemoveBall() + Remove ball from loop array"
	Dim ballcount
	ballcount = 0
	Dim x
	For x = 1 To UBound(RampBalls)
		If ID = RampBalls(x, 1) Then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		End If
		'if RampBalls(x,1) = Not IsEmpty(Rampballs(x,1) then ballcount = ballcount + 1
		If Not IsEmpty(Rampballs(x,1)) Then ballcount = ballcount + 1
	Next
	If BallCount = 0 Then RampBalls(0,0) = False	'if no balls in queue, disable timer update
End Sub

Sub RampRoll_Timer()
	RampRollUpdate
End Sub

Sub RampRollUpdate()	'Timer update
	Dim x
	For x = 1 To UBound(RampBalls)
		If Not IsEmpty(RampBalls(x,1) ) Then
			If BallVel(RampBalls(x,0) ) > 1 Then ' if ball is moving, play rolling sound
				If RampType(x) Then
					PlaySound("RampLoop" & x), - 1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), - 1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				End If
				RampBalls(x, 2) = RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
			End If
			If RampBalls(x,0).Z < 30 And RampBalls(x, 2) > RampMinLoops Then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		End If
	Next
	If Not RampBalls(0,0) Then RampRoll.enabled = 0
End Sub

' This can be used to debug the Ramp Roll time.  You need to enable the tbWR timer on the TextBox
Sub tbWR_Timer()	'debug textbox
	Me.text = "on? " & RampBalls(0, 0) & " timer: " & RampRoll.Enabled & vbNewLine & _
	"1 " & TypeName(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & " type:" & RampType(1) & " Loops:" & RampBalls(1, 2) & vbNewLine & _
	"2 " & TypeName(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & " type:" & RampType(2) & " Loops:" & RampBalls(2, 2) & vbNewLine & _
	"3 " & TypeName(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & " type:" & RampType(3) & " Loops:" & RampBalls(3, 2) & vbNewLine & _
	"4 " & TypeName(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & " type:" & RampType(4) & " Loops:" & RampBalls(4, 2) & vbNewLine & _
	"5 " & TypeName(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & " type:" & RampType(5) & " Loops:" & RampBalls(5, 2) & vbNewLine & _
	"6 " & TypeName(RampBalls(6, 0)) & " ID:" & RampBalls(6, 1) & " type:" & RampType(6) & " Loops:" & RampBalls(6, 2) & vbNewLine & _
	" "
End Sub

Function BallPitch(ball) ' Calculates the pitch of the sound based on the ball speed
	BallPitch = pSlope(BallVel(ball), 1, - 1000, 60, 10000)
End Function

Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
	BallPitchV = pSlope(BallVel(ball), 1, - 4000, 60, 7000)
End Function


'Ramp triggers

Sub RampLFxstart_Hit
	WireRampOn True
End Sub

Sub RampLFxstart_UnHit
	If activeball.vely>0 Then WireRampOff
End Sub

Sub RampRFXstart_Hit
	WireRampOn True
End Sub

Sub RampRFXstart_UnHit
	If activeball.vely>0 Then WireRampOff
End Sub

Sub StopWireSoundTrigger_Hit
	WireRampOff
End Sub

Sub StopWireSoundTrigger1_Hit
	WireRampOff
End Sub

Sub StopWireSoundTrigger2_Hit
	WireRampOff
End Sub

Sub LeftRampEnd_Hit
	PlaySoundAtLevelActiveBall ("WireRamp_Stop"), Vol(ActiveBall) * 0.001
End Sub


'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************


Sub RandomSoundHole()
    Select Case Int(Rnd * 6) + 1
        Case 1:PlaySound "fx_fallinramp1", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 2:PlaySound "fx_fallinramp2", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 3:PlaySound "fx_fallinramp3", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 4:PlaySound "fx_fallinramp4", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 5:PlaySound "fx_fallinramp5", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 6:PlaySound "fx_fallinramp6", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    End Select
End Sub




'******************************************************
'****  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity
dim RFs : Set RFs = New FlipperPolarity

InitPolarity

Sub InitPolarity()
        dim x, a : a = Array(LF, RF, RFs)
        for each x in a
                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
                x.enabled = True
                x.TimeDelay = 60
        Next

        AddPt "Polarity", 0, 0, 0
        AddPt "Polarity", 1, 0.05, -5.5
        AddPt "Polarity", 2, 0.4, -5.5
        AddPt "Polarity", 3, 0.6, -5.0
        AddPt "Polarity", 4, 0.65, -4.5
        AddPt "Polarity", 5, 0.7, -4.0
        AddPt "Polarity", 6, 0.75, -3.5
        AddPt "Polarity", 7, 0.8, -3.0
        AddPt "Polarity", 8, 0.85, -2.5
        AddPt "Polarity", 9, 0.9,-2.0
        AddPt "Polarity", 10, 0.95, -1.5
        AddPt "Polarity", 11, 1, -1.0
        AddPt "Polarity", 12, 1.05, -0.5
        AddPt "Polarity", 13, 1.1, 0
        AddPt "Polarity", 14, 1.3, 0

        addpt "Velocity", 0, 0,         1
        addpt "Velocity", 1, 0.16, 1.06
        addpt "Velocity", 2, 0.41,         1.05
        addpt "Velocity", 3, 0.53,         1'0.982
        addpt "Velocity", 4, 0.702, 0.968
        addpt "Velocity", 5, 0.95,  0.968
        addpt "Velocity", 6, 1.03,         0.945

        LF.Object = LeftFlipper        
        LF.EndPoint = EndPointLp
        RF.Object = RightFlipper
        RF.EndPoint = EndPointRp
        RFs.Object = RightFlipperS
        RFs.EndPoint = EndPointRpS
End Sub

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub
Sub TriggerRFs_Hit() 	: RFs.Addball activeball : End Sub
Sub TriggerRFs_UnHit() 	: RFs.PolarityCorrect activeball : End Sub


'******************************************************
'                        FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
        dim a : a = Array(LF, RF)
        dim x : for each x in a
                x.addpoint aStr, idx, aX, aY
        Next
End Sub

Class FlipperPolarity
        Public DebugOn, Enabled
        Private FlipAt        'Timer variable (IE 'flip at 723,530ms...)
        Public TimeDelay        'delay before trigger turns off and polarity is disabled TODO set time!
        private Flipper, FlipperStart,FlipperEnd, FlipperEndY, LR, PartialFlipCoef
        Private Balls(20), balldata(20)
        
        dim PolarityIn, PolarityOut
        dim VelocityIn, VelocityOut
        dim YcoefIn, YcoefOut
        Public Sub Class_Initialize 
                redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
                Enabled = True : TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new SpoofBall : next 
        End Sub
        
        Public Property let Object(aInput) : Set Flipper = aInput : StartPoint = Flipper.x : End Property
        Public Property Let StartPoint(aInput) : if IsObject(aInput) then FlipperStart = aInput.x else FlipperStart = aInput : end if : End Property
        Public Property Get StartPoint : StartPoint = FlipperStart : End Property
        Public Property Let EndPoint(aInput) : FlipperEnd = aInput.x: FlipperEndY = aInput.y: End Property
        Public Property Get EndPoint : EndPoint = FlipperEnd : End Property        
        Public Property Get EndPointY: EndPointY = FlipperEndY : End Property
        
        Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
                Select Case aChooseArray
                        case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
                        Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
                        Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
                End Select
                if gametime > 100 then Report aChooseArray
        End Sub 

        Public Sub Report(aChooseArray)         'debug, reports all coords in tbPL.text
                if not DebugOn then exit sub
                dim a1, a2 : Select Case aChooseArray
                        case "Polarity" : a1 = PolarityIn : a2 = PolarityOut
                        Case "Velocity" : a1 = VelocityIn : a2 = VelocityOut
                        Case "Ycoef" : a1 = YcoefIn : a2 = YcoefOut 
                        case else :tbpl.text = "wrong string" : exit sub
                End Select
                dim str, x : for x = 0 to uBound(a1) : str = str & aChooseArray & " x: " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
                tbpl.text = str
        End Sub
        
        Public Sub AddBall(aBall) : dim x : for x = 0 to uBound(balls) : if IsEmpty(balls(x)) then set balls(x) = aBall : MakeBugBalls : exit sub :end if : Next :  End Sub

        Private Sub RemoveBall(aBall)
                dim x : for x = 0 to uBound(balls)
                        if TypeName(balls(x) ) = "IBall" then 
                                if aBall.ID = Balls(x).ID Then
                                        balls(x) = Empty
                                        Balldata(x).Reset
                                End If
                        End If
                Next
        End Sub
        
        Public Sub Fire() 
                Flipper.RotateToEnd
                processballs
        End Sub

        Public Property Get Pos 'returns % position a ball. For debug stuff.
                dim x : for x = 0 to uBound(balls)
                        if not IsEmpty(balls(x) ) then
                                pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
                        End If
                Next                
        End Property

        Public Sub ProcessBalls() 'save data of balls in flipper range
                FlipAt = GameTime
                dim x : for x = 0 to uBound(balls)
                        if not IsEmpty(balls(x) ) then
                                balldata(x).Data = balls(x)
                        End If
                Next
                PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
                PartialFlipCoef = abs(PartialFlipCoef-1)
        End Sub
        Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function        'Timer shutoff for polaritycorrect
        
        Public Sub PolarityCorrect(aBall)
                if FlipperOn() then 
                        dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1

                        'y safety Exit
                        if aBall.VelY > -8 then 'ball going down
                                RemoveBall aBall
                                exit Sub
                        end if

                        'Find balldata. BallPos = % on Flipper
                        for x = 0 to uBound(Balls)
                                if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
                                        idx = x
                                        BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
                                        if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)                                'find safety coefficient 'ycoef' data
                                end if
                        Next

                        If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
                                BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
                                if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)                                                'find safety coefficient 'ycoef' data
                        End If

                        'Velocity correction
                        if not IsEmpty(VelocityIn(0) ) then
                                Dim VelCoef
         :                         VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

                                if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

                                if Enabled then aBall.Velx = aBall.Velx*VelCoef
                                if Enabled then aBall.Vely = aBall.Vely*VelCoef
                        End If

                        'Polarity Correction (optional now)
                        if not IsEmpty(PolarityIn(0) ) then
                                If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
                                dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
        
                                if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
                                'playsound "fx_knocker"
                        End If
                End If
                RemoveBall aBall
        End Sub
End Class

'******************************************************
'                FLIPPER POLARITY AND RUBBER DAMPENER
'                        SUPPORTING FUNCTIONS 
'******************************************************

' Used for flipper correction and rubber dampeners
'Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
'        dim x, aCount : aCount = 0
'        redim a(uBound(aArray) )
'        for x = 0 to uBound(aArray)        'Shuffle objects in a temp array
'                if not IsEmpty(aArray(x) ) Then
'                        if IsObject(aArray(x)) then 
'                                Set a(aCount) = aArray(x)
'                        Else
'                                a(aCount) = aArray(x)
'                        End If
'                        aCount = aCount + 1
'                End If
'        Next
'        if offset < 0 then offset = 0
'        redim aArray(aCount-1+offset)        'Resize original array
'        for x = 0 to aCount-1                'set objects back into original array
'                if IsObject(a(x)) then 
'                        Set aArray(x) = a(x)
'                Else
'                        aArray(x) = a(x)
'                End If
'        Next
'End Sub


' Used for flipper correction
Class spoofball 
        Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius 
        Public Property Let Data(aBall)
                With aBall
                        x = .x : y = .y : z = .z : velx = .velx : vely = .vely : velz = .velz
                        id = .ID : mass = .mass : radius = .radius
                end with
        End Property
        Public Sub Reset()
                x = Empty : y = Empty : z = Empty  : velx = Empty : vely = Empty : velz = Empty 
                id = Empty : mass = Empty : radius = Empty
        End Sub
End Class




'******************************************************
'                        FLIPPER TRICKS
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
        FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
        FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
        FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
        FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
        FlipperTricks RightFlipperS, RFPress2, RFCount2, RFEndAngle2, RFState2
end sub

Dim LFEOSNudge, RFEOSNudge

'e8 updated to latest
Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
    Dim BOT, b

    If abs(Flipper1.currentangle) < abs(Endangle1) + 3 and EOSNudge1 <> 1 Then
        EOSNudge1 = 1
        If Flipper2.currentangle = EndAngle2 Then 
            BOT = GetBalls
            For b = 0 to Ubound(BOT)
                If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
                    'Debug.Print "ball in flip1. exit"
                     exit Sub
                end If
            Next
            For b = 0 to Ubound(BOT)
                If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
                    'debug.print "flippernudge!!"
                    BOT(b).velx = BOT(b).velx /1.3
                    BOT(b).vely = BOT(b).vely - 0.5
                end If
            Next
        End If
    Else 
        If abs(Flipper1.currentangle) > abs(Endangle1) + 30 then EOSNudge1 = 0
    End If
End Sub


'*****************
' Maths
'*****************
Const PI = 3.1415927

Function dSin(degrees)
        dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
        dcos = cos(degrees * Pi/180)
End Function

Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy)
        Dim AB, BC, CD, DA
        AB = (bx*py) - (by*px) - (ax*py) + (ay*px) + (ax*by) - (ay*bx)
        BC = (cx*py) - (cy*px) - (bx*py) + (by*px) + (bx*cy) - (by*cx)
        CD = (dx*py) - (dy*px) - (cx*py) + (cy*px) + (cx*dy) - (cy*dx)
        DA = (ax*py) - (ay*px) - (dx*py) + (dy*px) + (dx*ay) - (dy*ax)
 
        If (AB <= 0 AND BC <=0 AND CD <= 0 AND DA <= 0) Or (AB >= 0 AND BC >=0 AND CD >= 0 AND DA >= 0) Then
                InRect = True
        Else
                InRect = False       
        End If
End Function

'*************************************************
' Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
        Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
        DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
        Radians = Degrees * PI /180
End Function

Function AnglePP(ax,ay,bx,by)
        AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
        DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle+90))+Flipper.x, Sin(Radians(Flipper.currentangle+90))+Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
        Dim DiffAngle
        DiffAngle  = ABS(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
        If DiffAngle > 180 Then DiffAngle = DiffAngle - 360

        If DistanceFromFlipper(ballx,bally,Flipper) < 48 and DiffAngle <= 90 and Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
                FlipperTrigger = True
        Else
                FlipperTrigger = False
        End If        
End Function

' Used for drop targets and stand up targets
Function Atn2(dy, dx)
        dim pi
        pi = 4*Atn(1)

        If dx > 0 Then
                Atn2 = Atn(dy / dx)
        ElseIf dx < 0 Then
                If dy = 0 Then 
                        Atn2 = pi
                Else
                        Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
                end if
        ElseIf dx = 0 Then
                if dy = 0 Then
                        Atn2 = 0
                else
                        Atn2 = Sgn(dy) * pi / 2
                end if
        End If
End Function

'*************************************************
' End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress, RFPress2, LFCount, RFCount, RFCount2
dim LFState, RFState
dim RFState2 'small flipper
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle
dim RFEndAngle2 'small flipper

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
Const EOSTnew = 0.8 
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
SOSRampup = 2.5
Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.025

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle
RFEndAngle2 = RightFlipperS.endangle

Sub FlipperActivate(Flipper, FlipperPress)
        FlipperPress = 1
        Flipper.Elasticity = FElasticity

        Flipper.eostorque = EOST         
        Flipper.eostorqueangle = EOSA         
End Sub

Sub FlipperDeactivate(Flipper, FlipperPress)
        FlipperPress = 0
        Flipper.eostorqueangle = EOSA
        Flipper.eostorque = EOST*EOSReturn/FReturn

        
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
                Dim BOT, b
                BOT = GetBalls
                        
                For b = 0 to UBound(BOT)
                        If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
                                If BOT(b).vely >= -0.4 Then BOT(b).vely = -0.4
                        End If
                Next
        End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState) 
        Dim Dir
        Dir = Flipper.startangle/Abs(Flipper.startangle)        '-1 for Right Flipper

        If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
                If FState <> 1 Then
                        Flipper.rampup = SOSRampup 
                        Flipper.endangle = FEndAngle - 3*Dir
                        Flipper.Elasticity = FElasticity * SOSEM
                        FCount = 0 
                        FState = 1
                End If
        ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) and FlipperPress = 1 then
                if FCount = 0 Then FCount = GameTime

                If FState <> 2 Then
                        Flipper.eostorqueangle = EOSAnew
                        Flipper.eostorque = EOSTnew
                        Flipper.rampup = EOSRampup                        
                        Flipper.endangle = FEndAngle
                        FState = 2
                End If
        Elseif Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 and FlipperPress = 1 Then 
                If FState <> 3 Then
                        Flipper.eostorque = EOST        
                        Flipper.eostorqueangle = EOSA
                        Flipper.rampup = Frampup
                        Flipper.Elasticity = FElasticity
                        FState = 3
                End If

        End If
End Sub

Const LiveDistanceMin = 30  'minimum distance in vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114  'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)


Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
        Dim Dir
        Dir = Flipper.startangle/Abs(Flipper.startangle)    '-1 for Right Flipper
        Dim LiveCatchBounce                                                                                                                        'If live catch is not perfect, it won't freeze ball totally
        Dim CatchTime : CatchTime = GameTime - FCount

        if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
                if CatchTime <= LiveCatch*0.5 Then                                                'Perfect catch only when catch time happens in the beginning of the window
                        LiveCatchBounce = 0
                else
                        LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)        'Partial catch when catch happens a bit late
                end If

                If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
                ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
                ball.angmomx= 0
                ball.angmomy= 0
                ball.angmomz= 0
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm
        End If
End Sub


'**************************************************
'        Flipper Collision Subs
'NOTE: COpy and overwrite collision sound from original collision subs over
'RandomSoundFlipper()' below
'**************************************************'

Sub LeftFlipper_Collide(parm)
    CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
    LeftFlipperCollide parm   'This is the Fleep code
End Sub

Sub RightFlipper_Collide(parm)
    CheckLiveCatch Activeball, RightFlipper, RFCount, parm
    RightFlipperCollide parm  'This is the Fleep code
End Sub

Sub RightFlipperS_Collide(parm)
    CheckLiveCatch Activeball, RightFlipperS, RFCount2, parm
    RightFlipperCollide parm  'This is the Fleep code
End Sub


'****************************************************************************
'PHYSICS DAMPENERS
'****************************************************************************

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR


Sub dPosts_Hit(idx) 
        RubbersD.dampen Activeball
End Sub

Sub dSleeves_Hit(idx) 
        SleevesD.Dampen Activeball
End Sub


dim RubbersD : Set RubbersD = new Dampener        'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False        'shows info in textbox "TBPout"
RubbersD.Print = False        'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967        'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64        'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener        'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False        'shows info in textbox "TBPout"
SleevesD.Print = False        'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'#########################    Adjust these values to increase or lessen the elasticity

dim FlippersD : Set FlippersD = new Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False        
FlippersD.addpoint 0, 0, 1.1        
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
        Public Print, debugOn 'tbpOut.text
        public name, Threshold         'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
        Public ModIn, ModOut
        Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): End Sub 

        Public Sub AddPoint(aIdx, aX, aY) 
                ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
                if gametime > 100 then Report
        End Sub

        public sub Dampen(aBall)
                if threshold then if BallSpeed(aBall) < threshold then exit sub end if end if
                dim RealCOR, DesiredCOR, str, coef
                DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
                RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
                coef = desiredcor / realcor 
                if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
                "actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
                if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)

                aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
                if debugOn then TBPout.text = str
        End Sub

        public sub Dampenf(aBall, parm) 'Rubberizer is handle here
                dim RealCOR, DesiredCOR, str, coef
                DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
                RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
                coef = desiredcor / realcor 
                If abs(aball.velx) < 2 and aball.vely < 0 and aball.vely > -3.75 then 
                        aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
                End If
        End Sub

        Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
                dim x : for x = 0 to uBound(aObj.ModIn)
                        addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
                Next
        End Sub


        Public Sub Report()         'debug, reports all coords in tbPL.text
                if not debugOn then exit sub
                dim a1, a2 : a1 = ModIn : a2 = ModOut
                dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
                TBPout.text = str
        End Sub

End Class



'******************************************************
'                TRACK ALL BALL VELOCITIES
'                 FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
        public ballvel, ballvelx, ballvely

        Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

        Public Sub Update()        'tracks in-ball-velocity
                dim str, b, AllBalls, highestID : allBalls = getballs

                for each b in allballs
                        if b.id >= HighestID then highestID = b.id
                Next

                if uBound(ballvel) < highestID then redim ballvel(highestID)        'set bounds
                if uBound(ballvelx) < highestID then redim ballvelx(highestID)        'set bounds
                if uBound(ballvely) < highestID then redim ballvely(highestID)        'set bounds

                for each b in allballs
                        ballvel(b.id) = BallSpeed(b)
                        ballvelx(b.id) = b.velx
                        ballvely(b.id) = b.vely
                Next
        End Sub
End Class

Sub RDampen_Timer()
       Cor.Update
End Sub


'******************************************************
'  SLINGSHOT CORRECTION FUNCTIONS
'******************************************************
' To add these slingshot corrections:
' 	- On the table, add the endpoint primitives that define the two ends of the Slingshot
'	- Initialize the SlingshotCorrection objects in InitSlingCorrection
' 	- Call the .VelocityCorrect methods from the respective _Slingshot event sub


dim LS : Set LS = New SlingshotCorrection
dim RS : Set RS = New SlingshotCorrection

InitSlingCorrection

Sub InitSlingCorrection

	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS

	RS.Object = RightSlingshot
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS

	'Slingshot angle corrections (pt, BallPos in %, Angle in deg)
	' These values are best guesses. Retune them if needed based on specific table research.
	AddSlingsPt 0, 0.00,	-4
	AddSlingsPt 1, 0.45,	-7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4

End Sub


Sub AddSlingsPt(idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LS, RS)
	dim x : for each x in a
		x.addpoint idx, aX, aY
	Next
End Sub

'' The following sub are needed, however they may exist somewhere else in the script. Uncomment below if needed
'Dim PI: PI = 4*Atn(1)
'Function dSin(degrees)
'	dsin = sin(degrees * Pi/180)
'End Function
'Function dCos(degrees)
'	dcos = cos(degrees * Pi/180)
'End Function

Function RotPoint(x,y,angle)
    dim rx, ry
    rx = x*dCos(angle) - y*dSin(angle)
    ry = x*dSin(angle) + y*dCos(angle)
    RotPoint = Array(rx,ry)
End Function

Class SlingshotCorrection
	Public DebugOn, Enabled
	private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2

	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): Enabled = True : End Sub 

	Public Property let Object(aInput) : Set Slingshot = aInput : End Property
	Public Property Let EndPoint1(aInput) : SlingX1 = aInput.x: SlingY1 = aInput.y: End Property
	Public Property Let EndPoint2(aInput) : SlingX2 = aInput.x: SlingY2 = aInput.y: End Property

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		If gametime > 100 then Report
	End Sub

	Public Sub Report()         'debug, reports all coords in tbPL.text
		If not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub


	Public Sub VelocityCorrect(aBall)
		dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then 
			XL = SlingX1 : YL = SlingY1 : XR = SlingX2 : YR = SlingY2
		Else
			XL = SlingX2 : YL = SlingY2 : XR = SlingX1 : YR = SlingY1
		End If

		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then 
			If ABS(XR-XL) > ABS(YR-YL) Then 
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If

		'Velocity angle correction
		If not IsEmpty(ModIn(0) ) then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'debug.print " BallPos=" & BallPos &" Angle=" & Angle 
			'debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled then aBall.Velx = RotVxVy(0)
			If Enabled then aBall.Vely = RotVxVy(1)
			'debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			'debug.print " " 
		End If
	End Sub

End Class



'******************************************************
'  FLIPPER POLARITY. RUBBER DAMPENER, AND SLINGSHOT CORRECTION SUPPORTING FUNCTIONS 
'******************************************************


'Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
'	dim a : a = Array(LF, RF)
'	dim x : for each x in a
'		x.addpoint aStr, idx, aX, aY
'	Next
'End Sub


' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)        'Shuffle objects in a temp array
		if not IsEmpty(aArray(x) ) Then
			if IsObject(aArray(x)) then 
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	if offset < 0 then offset = 0
	redim aArray(aCount-1+offset)        'Resize original array
	for x = 0 to aCount-1                'set objects back into original array
		if IsObject(a(x)) then 
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub

' Used for flipper correction and rubber dampeners
Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub

' Used for flipper correction, rubber dampeners, and drop targets
Function BallSpeed(ball) 'Calculates the ball speed
	BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)        'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function


' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)        'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)        'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) )         'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )        'Clamp upper

	LinearEnvelope = Y
End Function



'e8 Fleep Sounds


'////////////////////////////  MECHANICAL SOUNDS  ///////////////////////////
'//  This part in the script is an entire block that is dedicated to the physics sound system.
'//  Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for this table.


'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1                                                                                                                'volume level; range [0, 1]
NudgeLeftSoundLevel = 1                                                                                                        'volume level; range [0, 1]
NudgeRightSoundLevel = 1                                                                                                'volume level; range [0, 1]
NudgeCenterSoundLevel = 1                                                                                                'volume level; range [0, 1]
StartButtonSoundLevel = 0.1                                                                                                'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr                                                                                        'volume level; range [0, 1]
PlungerPullSoundLevel = 1                                                                                                'volume level; range [0, 1]
RollingSoundFactor = 1.1/5                

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010                                                           'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635                                                                'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                                                                        'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                                                                      'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel                                                                'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel                                                                'sound helper; not configurable
SlingshotSoundLevel = 0.95                                                                                                'volume level; range [0, 1]
BumperSoundFactor = 3                                                                                                'volume multiplier; must not be zero
KnockerSoundLevel = 1                                                                                                         'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2                                                                        'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055/5                                                                                        'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5                                                                                        'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5                                                                                'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025                                                                        'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025                                                                        'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8                                                                        'volume level; range [0, 1]
WallImpactSoundFactor = 0.075                                                                                        'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5                                                                                                        'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10                                                                                        'volume multiplier; must not be zero
DTSoundLevel = 0.25                                                                                                                'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                                                                      'volume level; range [0, 1]
SpinnerSoundLevel = 0.5                                                                      'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8                                                                                                                'volume level; range [0, 1]
BallReleaseSoundLevel = 1                                                                                                'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2                                                                        'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015                                                                                'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5                                                                                                        'volume multiplier; must not be zero


'/////////////////////////////  SOUND PLAYBACK FUNCTIONS  ////////////////////////////
'/////////////////////////////  POSITIONAL SOUND PLAYBACK METHODS  ////////////////////////////
' Positional sound playback methods will play a sound, depending on the X,Y position of the table element or depending on ActiveBall object position
' These are similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
' For surround setup - positional sound playback functions will fade between front and rear surround channels and pan between left and right channels
' For stereo setup - positional sound playback functions will only pan between left and right channels
' For mono setup - positional sound playback functions will not pan between left and right channels and will not fade between front and rear channels

' PlaySound full syntax - PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)
' Note - These functions will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlaySoundAtLevelStatic(playsoundparams, aVol, tableobj)
        PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
        PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
        PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
        PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
        PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
        PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
        PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
        PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
        PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlaySoundAt(soundname, tableobj)
        PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
        PlaySound soundname, 1, aVol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
        PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
        Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
        Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
        PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub


'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
  Dim tmp
    tmp = tableobj.y * 2 / tableheight-1

        if tmp > 7000 Then
                tmp = 7000
        elseif tmp < -7000 Then
                tmp = -7000
        end if

    If tmp > 0 Then
                AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / tablewidth-1

        if tmp > 7000 Then
                tmp = 7000
        elseif tmp < -7000 Then
                tmp = -7000
        end if

    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
        Vol = Csng(BallVel(ball) ^2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
        Volz = Csng((ball.velz) ^2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
        Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
        BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
        VolPlayfieldRoll = RollingSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
        PitchPlayfieldRoll = BallVel(ball) ^2 * 15
End Function

Function RndInt(min, max)
        RndInt = Int(Rnd() * (max-min + 1) + min)' Sets a random number integer between min and max
End Function

Function RndNum(min, max)
        RndNum = Rnd() * (max-min) + min' Sets a random number between min and max
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////
Sub SoundStartButton()
        PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
        PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
End Sub

Sub SoundNudgeRight()
        PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
        PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
End Sub


Sub SoundPlungerPull()
        PlaySoundAtLevelStatic ("Plunger_Pull_1"), PlungerPullSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseBall()
        PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, Plunger        
End Sub

Sub SoundPlungerReleaseNoBall()
        PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub


'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////
Sub KnockerSolenoid()
        PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, KnockerPosition
End Sub

'/////////////////////////////  DRAIN SOUNDS  ////////////////////////////
Sub RandomSoundDrain(drainswitch)
        PlaySoundAtLevelStatic ("Drain_" & Int(Rnd*11)+1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
        PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd*7)+1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundSlingshotLeft(sling)
        PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd*10)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
        PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd*8)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBumperTop(Bump)
        PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
        PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
        PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

'/////////////////////////////  SPINNER SOUNDS  ////////////////////////////
Sub SoundSpinner(spinnerswitch)
        PlaySoundAtLevelStatic ("Spinner"), SpinnerSoundLevel, spinnerswitch
End Sub


'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////
Sub SoundFlipperUpAttackLeft(flipper)
        FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
        PlaySoundAtLevelStatic ("Flipper_Attack-L01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
        FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
        PlaySoundAtLevelStatic ("Flipper_Attack-R01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////
Sub RandomSoundFlipperUpLeft(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd*9)+1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd*9)+1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd*7)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd*8)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
        FlipperLeftHitParm = parm/10
        If FlipperLeftHitParm > 1 Then
                FlipperLeftHitParm = 1
        End If
        FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
        RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
        FlipperRightHitParm = parm/10
        If FlipperRightHitParm > 1 Then
                FlipperRightHitParm = 1
        End If
        FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
        RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
        PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd*7)+1), parm  * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////
Sub RandomSoundRollover()
        PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd*4)+1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
        RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////
Sub Rubbers_Hit(idx)
        dim finalspeed
        finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
        If finalspeed > 5 then                
                RandomSoundRubberStrong 1
        End if
        If finalspeed <= 5 then
                RandomSoundRubberWeak()
        End If        
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////
Sub RandomSoundRubberStrong(voladj)
        Select Case Int(Rnd*10)+1
                Case 1 : PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 2 : PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 3 : PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 4 : PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 5 : PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 6 : PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 7 : PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 8 : PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 9 : PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 10 : PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6*voladj
        End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////
Sub RandomSoundRubberWeak()
        PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd*9)+1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub Walls_Hit(idx)
        RandomSoundWall()      
End Sub

Sub RandomSoundWall()
        dim finalspeed
        finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
        If finalspeed > 16 then 
                Select Case Int(Rnd*5)+1
                        Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 5 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
                End Select
        End if
        If finalspeed >= 6 AND finalspeed <= 16 then
                Select Case Int(Rnd*4)+1
                        Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
                End Select
        End If
        If finalspeed < 6 Then
                Select Case Int(Rnd*3)+1
                        Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
                End Select
        End if
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////
Sub RandomSoundMetal()
        PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd*13)+1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub

'/////////////////////////////  METAL - EVENTS  ////////////////////////////

Sub Metals_Hit (idx)
        RandomSoundMetal
End Sub

Sub ShooterDiverter_collide(idx)
        RandomSoundMetal
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE  ////////////////////////////
'/////////////////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////////////
Sub RandomSoundBottomArchBallGuide()
        dim finalspeed
        finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
        If finalspeed > 16 then 
                PlaySoundAtLevelActiveBall ("Apron_Bounce_"& Int(Rnd*2)+1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
        End if
        If finalspeed >= 6 AND finalspeed <= 16 then
                Select Case Int(Rnd*2)+1
                        Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
                        Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
                End Select
        End If
        If finalspeed < 6 Then
                Select Case Int(Rnd*2)+1
                        Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
                        Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
                End Select
        End if
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////
Sub RandomSoundBottomArchBallGuideHardHit()
        PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd*3)+1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron1_Hit (idx)
        If Abs(cor.ballvelx(activeball.id) < 4) and cor.ballvely(activeball.id) > 7 then
                RandomSoundBottomArchBallGuideHardHit()
        Else
                RandomSoundBottomArchBallGuide
        End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////
Sub RandomSoundFlipperBallGuide()
        dim finalspeed
        finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
        If finalspeed > 16 then 
                Select Case Int(Rnd*2)+1
                        Case 1 : PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
                        Case 2 : PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
                End Select
        End if
        If finalspeed >= 6 AND finalspeed <= 16 then
                PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd*3)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
        End If
        If finalspeed < 6 Then
                PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd*7)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
        End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////
Sub RandomSoundTargetHitStrong()
        PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()                
        PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
        dim finalspeed
        finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
        If finalspeed > 10 then
                RandomSoundTargetHitStrong()
                RandomSoundBallBouncePlayfieldSoft Activeball
        Else 
                RandomSoundTargetHitWeak()
        End If        
End Sub

Sub Targets_Hit (idx)
        PlayTargetSound        
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////
Sub RandomSoundBallBouncePlayfieldSoft(aBall)
        Select Case Int(Rnd*9)+1
                Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
                Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
                Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
                Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
                Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
                Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
                Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
                Case 8 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
                Case 9 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
        End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
        PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd*7)+1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////
Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
        Select Case Int(Rnd*5)+1
                Case 1 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
                Case 2 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
                Case 3 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
                Case 4 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
                Case 5 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
        End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()                        
        PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd*2)+1), GateSoundLevel, Activeball
End Sub

Sub SoundHeavyGate()
        PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, Activeball
End Sub

Sub Gates_hit(idx)
        SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)        
        SoundPlayfieldGate        
End Sub        

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
        PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
        PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub


Sub Arch1_hit()
        If Activeball.velx > 1 Then SoundPlayfieldGate
        StopSound "Arch_L1"
        StopSound "Arch_L2"
        StopSound "Arch_L3"
        StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
        If activeball.velx < -8 Then
                RandomSoundRightArch
        End If
End Sub

Sub Arch2_hit()
        If Activeball.velx < 1 Then SoundPlayfieldGate
        StopSound "Arch_R1"
        StopSound "Arch_R2"
        StopSound "Arch_R3"
        StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
        If activeball.velx > 10 Then
                RandomSoundLeftArch
        End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
    PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd*2)+1), SaucerLockSoundLevel, Activeball
End Sub

Sub SoundSaucerKick(scenario, saucer)
        Select Case scenario
                Case 0: PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
                Case 1: PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
        End Select
End Sub


'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////
Sub OnBallBallCollision(ball1, ball2, velocity)
        Dim snd
        Select Case Int(Rnd*7)+1
                Case 1 : snd = "Ball_Collide_1"
                Case 2 : snd = "Ball_Collide_2"
                Case 3 : snd = "Ball_Collide_3"
                Case 4 : snd = "Ball_Collide_4"
                Case 5 : snd = "Ball_Collide_5"
                Case 6 : snd = "Ball_Collide_6"
                Case 7 : snd = "Ball_Collide_7"
        End Select

        PlaySound (snd), 0, Csng(velocity) ^2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
        PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd*6)+1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
        PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd*6)+1), 200, obj
End Sub

'/////////////////////////////  GI AND FLASHER RELAYS  ////////////////////////////

Const RelayFlashSoundLevel = 0.315                                                                        'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05                                                                        'volume level; range [0, 1];

Sub Sound_GI_Relay(toggle, obj)
        Select Case toggle
                Case 1
                        PlaySoundAtLevelStatic ("Relay_GI_On"), 0.025*RelayGISoundLevel, obj
                Case 0
                        PlaySoundAtLevelStatic ("Relay_GI_Off"), 0.025*RelayGISoundLevel, obj
        End Select
End Sub

Sub Sound_Flash_Relay(toggle, obj)
        Select Case toggle
                Case 1
                        PlaySoundAtLevelStatic ("Relay_Flash_On"), 0.025*RelayFlashSoundLevel, obj                        
                Case 0
                        PlaySoundAtLevelStatic ("Relay_Flash_Off"), 0.025*RelayFlashSoundLevel, obj                
        End Select
End Sub

'/////////////////////////////////////////////////////////////////
'                                        End Mechanical Sounds
'/////////////////////////////////////////////////////////////////






'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

'****** INSTRUCTIONS please read ******

'****** Part A:  Table Elements ******
'
' Import the "bsrtx7" and "ballshadow" images
' Import the shadow materials file (3 sets included) (you can also export the 3 sets from this table to create the same file)
' Copy in the BallShadowA flasher set and the sets of primitives named BallShadow#, RtxBallShadow#, and RtxBall2Shadow#
'	* Count from 0 up, with at least as many objects each as there can be balls, including locked balls.  You'll get an "eval" warning if tnob is higher
'	* Warning:  If merging with another system (JP's ballrolling), you may need to check tnob math and add an extra BallShadowA# flasher (out of range error)
' Ensure you have a timer with a -1 interval that is always running
' Set plastic ramps DB to *less* than the ambient shadows (-10000) if you want to see the pf shadow through the ramp

' Create a collection called DynamicSources that includes all light sources you want to cast ball shadows
' It's recommended that you be selective in which lights go in this collection, as there are limitations:
' 1. The shadows can "pass through" solid objects and other light sources, so be mindful of where the lights would actually able to cast shadows
' 2. If there are more than two equidistant sources, the shadows can suddenly switch on and off, so places like top and bottom lanes need attention
' 3. At this time the shadows get the light on/off from tracking gilvl, so if you have lights you want shadows for that are on at different times you will need to either:
'	a) remove this restriction (shadows think lights are always On)
'	b) come up with a custom solution (see TZ example in script)
' After confirming the shadows work in general, use ball control to move around and look for any weird behavior

'****** End Part A:  Table Elements ******


'****** Part B:  Code and Functions ******

' *** Timer sub
' The "DynamicBSUpdate" sub should be called by a timer with an interval of -1 (framerate)
' Example timer sub:

'Sub FrameTimer_Timer()
'	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
'End Sub

' *** These are usually defined elsewhere (ballrolling), but activate here if necessary
'Const tnob = 4 ' total number of balls
Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done
'Dim tablewidth: tablewidth = Table1.width
'Dim tableheight: tableheight = Table1.height

' *** User Options - Uncomment here or move to top for easy access by players
'----- Shadow Options -----
'Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
'Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
'									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
'									'2 = flasher image shadow, but it moves like ninuzzu's

' *** This segment goes within the RollingUpdate sub, so that if Ambient...=0 and Dynamic...=0 the entire DynamicBSUpdate sub can be skipped for max performance
' *** Change gBOT to BOT if using existing getballs code
' *** Includes lines commonly found there, for reference:
'	' stop the sound of deleted balls
'	For b = UBound(gBOT) + 1 to tnob
'		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
'		...rolling(b) = False
'		...StopSound("BallRoll_" & b)
'	Next
'
' ...rolling and drop sounds...

'		If DropCount(b) < 5 Then
'			DropCount(b) = DropCount(b) + 1
'		End If
'
'		' "Static" Ball Shadows
'		If AmbientBallShadowOn = 0 Then
'			If gBOT(b).Z > 30 Then
'				BallShadowA(b).height=gBOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
'			Else
'				BallShadowA(b).height=gBOT(b).z - BallSize/2 + 5
'			End If
'			BallShadowA(b).Y = gBOT(b).Y + Ballsize/5 + offsetY
'			BallShadowA(b).X = gBOT(b).X + offsetX
'			BallShadowA(b).visible = 1
'		End If

' *** Required Functions, enable these if they are not already present elswhere in your table
Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function

'Function Distance(ax,ay,bx,by)
'	Distance = SQR((ax - bx)^2 + (ay - by)^2)
'End Function

'Dim PI: PI = 4*Atn(1)

'Function Atn2(dy, dx)
'	If dx > 0 Then
'		Atn2 = Atn(dy / dx)
'	ElseIf dx < 0 Then
'		If dy = 0 Then 
'			Atn2 = pi
'		Else
'			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
'		end if
'	ElseIf dx = 0 Then
'		if dy = 0 Then
'			Atn2 = 0
'		else
'			Atn2 = Sgn(dy) * pi / 2
'		end if
'	End If
'End Function

'Function AnglePP(ax,ay,bx,by)
'	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
'End Function

'****** End Part B:  Code and Functions ******


'****** Part C:  The Magic ******

' *** These define the appearance of shadows in your table	***

'Ambient (Room light source)
Const AmbientBSFactor 		= 0.9	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const offsetX				= 0		'Offset x position under ball	(These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY				= 0		'Offset y position under ball	 (for example 5,5 if the light is in the back left corner)

'Dynamic (Table light sources)
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness is technically most accurate for lights at z ~25 hitting a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source

' ***														***

' *** Trim or extend these to *match* the number of balls/primitives/flashers on the table!
dim objrtx1(4), objrtx2(4)
dim objBallShadow(4)
Dim OnPF(5)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4)
Dim DSSources(30), numberofsources', DSGISide(30) 'Adapted for TZ with GI left / GI right

Dim ClearSurface:ClearSurface = True		'Variable for hiding flasher shadow on wire and clear plastic ramps
									'Intention is to set this either globally or in a similar manner to RampRolling sounds

'Initialization
DynamicBSInit

sub DynamicBSInit()
	Dim iii, source

	for iii = 0 to tnob - 1								'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = 1 + iii/1000 + 0.01			'Separate z for layering without clipping
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = 1 + iii/1000 + 0.02
		objrtx2(iii).visible = 0

		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = 1 + iii/1000 + 0.04
		objBallShadow(iii).visible = 0

		BallShadowA(iii).Opacity = 100*AmbientBSFactor
		BallShadowA(iii).visible = 0
	Next

	iii = 0

	For Each Source in DynamicSources
		DSSources(iii) = Array(Source.x, Source.y)
'		If Instr(Source.name , "Left") > 0 Then DSGISide(iii) = 0 Else DSGISide(iii) = 1	'Adapted for TZ with GI left / GI right
		iii = iii + 1
	Next
	numberofsources = iii
end sub


Sub BallOnPlayfieldNow(yeh, num)		'Only update certain things once, save some cycles
	If yeh Then
		OnPF(num) = True
'		debug.print "Back on PF"
		UpdateMaterial objBallShadow(num).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(num).size_x = 5
		objBallShadow(num).size_y = 4.5
		objBallShadow(num).visible = 1
		BallShadowA(num).visible = 0
	Else
		OnPF(num) = False
'		debug.print "Leaving PF"
		If Not ClearSurface Then
			BallShadowA(num).visible = 1
			objBallShadow(num).visible = 0
		Else
			objBallShadow(num).visible = 1
		End If
	End If
End Sub

Sub DynamicBSUpdate
	Dim falloff: falloff = 150 'Max distance to light sources, can be changed dynamically if you have a reason
	Dim ShadowOpacity1, ShadowOpacity2 
	Dim s, LSd, iii
	Dim dist1, dist2, src1, src2
	Dim gBOT: gBOT=getballs	'Uncomment if you're deleting balls - Don't do it! #SaveTheBalls

	'Hide shadow of deleted balls
	For s = UBound(gBOT) + 1 to tnob - 1
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next

	If UBound(gBOT) < lob Then Exit Sub		'No balls in play, exit

'The Magic happens now
	For s = lob to UBound(gBOT)

' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 units and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible

		If AmbientBallShadowOn = 1 Then			'Primitive shadow on playfield, flasher shadow in ramps
			If gBOT(s).Z > 30 Then							'The flasher follows the ball up ramps while the primitive is on the pf
				If OnPF(s) Then BallOnPlayfieldNow False, s		'One-time update

				If Not ClearSurface Then							'Don't show this shadow on plastic or wire ramps (table-wide variable, for now)
					BallShadowA(s).X = gBOT(s).X + offsetX
					BallShadowA(s).Y = gBOT(s).Y + BallSize/5
					BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				Else
					If gBOT(s).X < tablewidth/2 Then
						objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
					Else
						objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
					End If
					objBallShadow(s).Y = gBOT(s).Y + BallSize/10 + offsetY
					objBallShadow(s).size_x = 5 * ((gBOT(s).Z+BallSize)/80)			'Shadow gets larger and more diffuse as it moves up
					objBallShadow(s).size_y = 4.5 * ((gBOT(s).Z+BallSize)/80)
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor*(30/(gBOT(s).Z)),RGB(0,0,0),0,0,False,True,0,0,0,0
				End If

			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf, primitive only
				If Not OnPF(s) Then BallOnPlayfieldNow True, s

				If gBOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
				Else
					objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
				End If
				objBallShadow(s).Y = gBOT(s).Y + offsetY
'				objBallShadow(s).Z = gBOT(s).Z + s/1000 + 0.04		'Uncomment (and adjust If/Elseif height logic) if you want the primitive shadow on an upper/split pf

			Else												'Under pf, no shadows
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 0
			end if

		Elseif AmbientBallShadowOn = 2 Then		'Flasher shadow everywhere
			If gBOT(s).Z > 30 Then							'In a ramp
				If Not ClearSurface Then							'Don't show this shadow on plastic or wire ramps (table-wide variable, for now)
					BallShadowA(s).X = gBOT(s).X + offsetX
					BallShadowA(s).Y = gBOT(s).Y + BallSize/5
					BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				Else
					BallShadowA(s).X = gBOT(s).X + offsetX
					BallShadowA(s).Y = gBOT(s).Y + offsetY
				End If
			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf
				BallShadowA(s).visible = 1
				If gBOT(s).X < tablewidth/2 Then
					BallShadowA(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
				Else
					BallShadowA(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
				End If
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height=0.1
			Else											'Under pf
				BallShadowA(s).visible = 0
			End If
		End If

' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If gBOT(s).Z < 30 And gBOT(s).X < 850 Then	'Parameters for where the shadows can show, here they are not visible above the table (no upper pf) or in the plunger lane
				dist1 = falloff:
				dist2 = falloff
				For iii = 0 to numberofsources - 1 ' Search the 2 nearest influencing lights
					LSd = Distance(gBOT(s).x, gBOT(s).y, DSSources(iii)(0), DSSources(iii)(1)) 'Calculating the Linear distance to the Source
					If LSd < falloff Then
'					If LSd < dist2 And ((DSGISide(iii) = 0 And Lampz.State(100)>0) Or (DSGISide(iii) = 1 And Lampz.State(104)>0)) Then	'Adapted for TZ with GI left / GI right
						dist2 = dist1
						dist1 = LSd
						src2 = src1
						src1 = iii
					End If
				Next
				ShadowOpacity1 = 0
				If dist1 < falloff Then
					objrtx1(s).visible = 1 : objrtx1(s).X = gBOT(s).X : objrtx1(s).Y = gBOT(s).Y
					'objrtx1(s).Z = gBOT(s).Z - 25 + s/1000 + 0.01 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx1(s).rotz = AnglePP(DSSources(src1)(0), DSSources(src1)(1), gBOT(s).X, gBOT(s).Y) + 90
					ShadowOpacity1 = 1 - dist1 / falloff
					objrtx1(s).size_y = Wideness * ShadowOpacity1 + Thinness
					UpdateMaterial objrtx1(s).material,1,0,0,0,0,0,ShadowOpacity1*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx1(s).visible = 0
				End If
				ShadowOpacity2 = 0
				If dist2 < falloff Then
					objrtx2(s).visible = 1 : objrtx2(s).X = gBOT(s).X : objrtx2(s).Y = gBOT(s).Y + offsetY
					'objrtx2(s).Z = gBOT(s).Z - 25 + s/1000 + 0.02 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx2(s).rotz = AnglePP(DSSources(src2)(0), DSSources(src2)(1), gBOT(s).X, gBOT(s).Y) + 90
					ShadowOpacity2 = 1 - dist2 / falloff
					objrtx2(s).size_y = Wideness * ShadowOpacity2 + Thinness
					UpdateMaterial objrtx2(s).material,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx2(s).visible = 0
				End If
				If AmbientBallShadowOn = 1 Then
					'Fades the ambient shadow (primitive only) when it's close to a light
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor*(1 - max(ShadowOpacity1, ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					BallShadowA(s).Opacity = 100 * AmbientBSFactor * (1 - max(ShadowOpacity1, ShadowOpacity2))
				End If
			Else 'Hide dynamic shadows everywhere else, just in case
				objrtx2(s).visible = 0 : objrtx1(s).visible = 0
			End If
		End If
	Next
End Sub

'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************

Sub Toy_Spin_Timer_Timer()
	pylon1a.RotY = pylon1a.RotY + 5
	Pylon1b.RotY = Pylon1b.RotY - 5
	pylon1c.RotY = pylon1c.RotY - 5
	pylon2a.RotY = pylon2a.RotY - 5
	Pylon2b.RotY = Pylon2b.RotY + 5
	pylon2c.RotY = pylon2c.RotY + 5
	Turret1.RotY = Turret1.RotY + 5
	Turret2.RotY = Turret2.RotY - 5
	Overlord1.RotY = Overlord1.RotY + 2
	Overlord2.RotY = Overlord2.RotY - 2
End Sub

Sub Building_timer_Timer()
	If CommandCenter.visible = 0 and Nexus.visible = 0 and Hive.visible = 0 Then
		If RaceTerran Then
			CommandCenter.visible = 1
			Building_timer.Enabled = false
			WarpTimer1.Enabled = false
			WarpTimersOff
			Building1RingTimer.Enabled = true
			PlaySound "SFX_UpgradeCompleteTerran"
		ElseIf RaceProtoss Then
			Nexus.visible = 1
			Building_timer.Enabled = false
			WarpTimer1.Enabled = false
			WarpTimersOff
			Building1RingTimer.Enabled = true
			PlaySound "SFX_UpgradeCompleteProtoss"
		ElseIf RaceZerg Then
			Hive.visible = 1
			Building_timer.Enabled = false
			WarpTimer1.Enabled = false
			WarpTimersOff
			Building1RingTimer.Enabled = true
			PlaySound "SFX_UpgradeCompleteZerg"
		End If
	ElseIf StarPort.visible = 0 and Gateway.visible = 0 and Spawing_Pool.visible = 0 Then
		If RaceTerran Then
			StarPort.visible = 1
			Building_timer.Enabled = false
			WarpTimer2.Enabled = false
			WarpTimersOff
			Building2RingTimer.Enabled = true
			PlaySound "SFX_UpgradeCompleteTerran"
		ElseIf RaceProtoss Then
			Gateway.visible = 1
			Building_timer.Enabled = false
			WarpTimer2.Enabled = false
			WarpTimersOff
			Building2RingTimer.Enabled = true
			PlaySound "SFX_UpgradeCompleteProtoss"
		ElseIf RaceZerg Then
			Spawing_Pool.visible = 1
			Building_timer.Enabled = false
			WarpTimer2.Enabled = false
			WarpTimersOff
			Building2RingTimer.Enabled = true
			PlaySound "SFX_UpgradeCompleteZerg"
		End If
	Elseif Barracks.visible = 0 and StarGate.visible = 0 and Spire.visible = 0 Then
		If RaceTerran Then
			Barracks.visible = 1
			Building_timer.Enabled = false
			WarpTimer3.Enabled = false
			WarpTimersOff
			Building3RingTimer.Enabled = true
			PlaySound "SFX_UpgradeCompleteTerran"
		ElseIf RaceProtoss Then
			StarGate.visible = 1
			Building_timer.Enabled = false
			WarpTimer3.Enabled = false
			WarpTimersOff
			Building3RingTimer.Enabled = true
			PlaySound "SFX_UpgradeCompleteProtoss"
		ElseIf RaceZerg Then
			Spire.visible = 1
			Building_timer.Enabled = false
			WarpTimer3.Enabled = false
			WarpTimersOff
			Building3RingTimer.Enabled = true
			PlaySound "SFX_UpgradeCompleteZerg"
		End If
	End If
End Sub


Dim TempStep1: TempStep1 = 1
Dim TempStep2: TempStep2 = 1
Dim TempStep3: TempStep3 = 1
Dim TempStep4: TempStep4 = 1
Dim TempStep5: TempStep5 = 1
Dim TempStep6: TempStep6 = 1

Sub WarpTimer1_Timer()
	select case TempStep1
		case 1
			WarpBuilding1_1.visible = True
			WarpBuilding1_2.visible = False
			WarpBuilding1_3.visible = False
		case 2
			WarpBuilding1_1.visible = True
			WarpBuilding1_2.visible = True
			WarpBuilding1_3.visible = False
		case 3
			WarpBuilding1_1.visible = True
			WarpBuilding1_2.visible = True
			WarpBuilding1_3.visible = True
		Case 4
			WarpBuilding1_1.visible = False 'True 'desktop
			WarpBuilding1_2.visible = True
			WarpBuilding1_3.visible = True 'False 'desktop
		case 5
			WarpBuilding1_1.visible = False 'True 'desktop
			WarpBuilding1_2.visible = False
			WarpBuilding1_3.visible = True 'False 'desktop
		case 6
			WarpBuilding1_1.visible = False
			WarpBuilding1_2.visible = False
			WarpBuilding1_3.visible = False
	end Select
	TempStep1 = TempStep1 + 1
	if TempStep1 = 7 then TempStep1 = 1
End Sub

Sub WarpTimer2_Timer()
	select case TempStep2
		case 1
			WarpBuilding2_1.visible = True
			WarpBuilding2_2.visible = False
			WarpBuilding2_3.visible = False
		case 2
			WarpBuilding2_1.visible = True
			WarpBuilding2_2.visible = True
			WarpBuilding2_3.visible = False
		case 3
			WarpBuilding2_1.visible = True
			WarpBuilding2_2.visible = True
			WarpBuilding2_3.visible = True
		Case 4
			WarpBuilding2_1.visible = False 'True 'desktop
			WarpBuilding2_2.visible = True
			WarpBuilding2_3.visible = True 'False 'desktop
		case 5
			WarpBuilding2_1.visible = False 'True 'desktop
			WarpBuilding2_2.visible = False
			WarpBuilding2_3.visible = True 'False 'desktop
		case 6
			WarpBuilding2_1.visible = False
			WarpBuilding2_2.visible = False
			WarpBuilding2_3.visible = False
	end Select
	TempStep2 = TempStep2 + 1
	if TempStep2 = 7 then TempStep2 = 1
End Sub

Sub WarpTimer3_Timer()
	select case TempStep3
		case 1
			WarpBuilding3_1.visible = True
			WarpBuilding3_2.visible = False
			WarpBuilding3_3.visible = False
		case 2
			WarpBuilding3_1.visible = True
			WarpBuilding3_2.visible = True
			WarpBuilding3_3.visible = False
		case 3
			WarpBuilding3_1.visible = True
			WarpBuilding3_2.visible = True
			WarpBuilding3_3.visible = True
		Case 4
			WarpBuilding3_1.visible = False 'True
			WarpBuilding3_2.visible = True
			WarpBuilding3_3.visible = True 'False
		case 5
			WarpBuilding3_1.visible = False 'True
			WarpBuilding3_2.visible = False
			WarpBuilding3_3.visible = True 'False
		case 6
			WarpBuilding3_1.visible = False
			WarpBuilding3_2.visible = False
			WarpBuilding3_3.visible = False
	end Select
	TempStep3 = TempStep3 + 1
	if TempStep3 = 7 then TempStep3 = 1
End Sub

Sub WarpTimersOff
	WarpBuilding1_1.visible = False
	WarpBuilding1_2.visible = False
	WarpBuilding1_3.visible = False
	WarpBuilding2_1.visible = False
	WarpBuilding2_2.visible = False
	WarpBuilding2_3.visible = False
	WarpBuilding3_1.visible = False
	WarpBuilding3_2.visible = False
	WarpBuilding3_3.visible = False
End Sub

Sub Building1RingTimer_Timer()
	select case TempStep4
		case 1
			Building1_ring1.visible = True
			Building1_ring2.visible = False
			Building1_ring3.visible = False
			Building1_ring4.visible = False
			Building1_ring5.visible = False
			Building1_ring6.visible = False
			Building1_ring7.visible = False
			Building1_ring8.visible = False
		case 2
			Building1_ring1.visible = False
			Building1_ring2.visible = True
		case 3
			Building1_ring2.visible = False
			Building1_ring3.visible = True
		Case 4
			Building1_ring3.visible = False
			Building1_ring4.visible = True
		case 5
			Building1_ring4.visible = False
			Building1_ring5.visible = True
		case 6
			Building1_ring5.visible = False
			Building1_ring6.visible = True
		case 8
			Building1_ring6.visible = False
			Building1_ring7.visible = True
		case 9
			Building1_ring7.visible = False
			Building1_ring8.visible = True
	end Select
	TempStep4 = TempStep4 + 1
	if TempStep4 = 10 then TempStep4 = 1
End Sub

Sub Building2RingTimer_Timer()
	select case TempStep5
		case 1
			Building2_ring1.visible = True
			Building2_ring2.visible = False
			Building2_ring3.visible = False
			Building2_ring4.visible = False
			Building2_ring5.visible = False
			Building2_ring6.visible = False
			Building2_ring7.visible = False
			Building2_ring8.visible = False
		case 2
			Building2_ring1.visible = False
			Building2_ring2.visible = True
		case 3
			Building2_ring2.visible = False
			Building2_ring3.visible = True
		Case 4
			Building2_ring3.visible = False
			Building2_ring4.visible = True
		case 5
			Building2_ring4.visible = False
			Building2_ring5.visible = True
		case 6
			Building2_ring5.visible = False
			Building2_ring6.visible = True
		case 8
			Building2_ring6.visible = False
			Building2_ring7.visible = True
		case 9
			Building2_ring7.visible = False
			Building2_ring8.visible = True
	end Select
	TempStep5 = TempStep5 + 1
	if TempStep5 = 10 then TempStep5 = 1
End Sub

Sub Building3RingTimer_Timer()
	select case TempStep6
		case 1
			Building3_ring1.visible = True
			Building3_ring2.visible = False
			Building3_ring3.visible = False
			Building3_ring4.visible = False
			Building3_ring5.visible = False
			Building3_ring6.visible = False
			Building3_ring7.visible = False
			Building3_ring8.visible = False
		case 2
			Building3_ring1.visible = False
			Building3_ring2.visible = True
		case 3
			Building3_ring2.visible = False
			Building3_ring3.visible = True
		Case 4
			Building3_ring3.visible = False
			Building3_ring4.visible = True
		case 5
			Building3_ring4.visible = False
			Building3_ring5.visible = True
		case 6
			Building3_ring5.visible = False
			Building3_ring6.visible = True
		case 8
			Building3_ring6.visible = False
			Building3_ring7.visible = True
		case 9
			Building3_ring7.visible = False
			Building3_ring8.visible = True
	end Select
	TempStep6 = TempStep6 + 1
	if TempStep6 = 10 then TempStep6 = 1
End Sub

Sub BuildingRingsOff
	Building1_ring1.visible = False
	Building1_ring2.visible = False
	Building1_ring3.visible = False
	Building1_ring4.visible = False
	Building1_ring5.visible = False
	Building1_ring6.visible = False
	Building1_ring7.visible = False
	Building1_ring8.visible = False
	Building2_ring1.visible = False
	Building2_ring2.visible = False
	Building2_ring3.visible = False
	Building2_ring4.visible = False
	Building2_ring5.visible = False
	Building2_ring6.visible = False
	Building2_ring7.visible = False
	Building2_ring8.visible = False
	Building3_ring1.visible = False
	Building3_ring2.visible = False
	Building3_ring3.visible = False
	Building3_ring4.visible = False
	Building3_ring5.visible = False
	Building3_ring6.visible = False
	Building3_ring7.visible = False
	Building3_ring8.visible = False
End Sub

'*********************************
' Table Options F12 User Options
'*********************************
' Table1.Option arguments are:
' - option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional array of literal strings

Dim LUTImage, UseFlexDMD, OldUseFlex, FlexDMDHighQuality, SongVolume, aBallGlowing, FlipperPin, KidMode, FlipperMod, BugBall, LowEndPC, ColoredRamps
'UseFlexDMD = False 'initialize variable
'OldUseFlex = False

Sub Table1_OptionEvent(ByVal eventId)
    Dim x, y

    'LUT
    LutImage = Table1.Option("Select LUT", 0, 21, 1, 0, 0, Array("Normal 0", "Normal 1", "Normal 2", "Normal 3", "Normal 4", "Normal 5", "Normal 6", "Normal 7", "Normal 8", "Normal 9", "Normal 10", _
        "Warm 0", "Warm 1", "Warm 2", "Warm 3", "Warm 4", "Warm 5", "Warm 6", "Warm 7", "Warm 8", "Warm 9", "Warm 10") )
    UpdateOptions

    ' Desktop DMD
    'x = Table1.Option("DMD Type", 0, 1, 1, 1, 0, Array("Desktop DMD", "FlexDMD") )
    'If UseFlexDMD AND x = 0 Then FlexDMD.Run = False
    'If X then UseFlexDMD = True Else UseFlexDMD = False

    ' FlexDMD Quality
    'x = Table1.Option("FlexDMD Quality", 0, 1, 1, 1, 0, Array("Low", "High") )
    'If x Then FlexDMDHighQuality = True Else FlexDMDHighQuality = False
    'If OldUseFlex <> UseFlexDMD Then
    '    DMD_Init
    '    If NOT bGameInPlay Then ShowTableInfo
    '    OldUseFlex = UseFlexDMD 
    'End If

    ' Cabinet rails
    'x = Table1.Option("Cabinet Rails", 0, 1, 1, 1, 0, Array("Hide", "Show") )
    'For each y in aRails:y.visible = x:next

	' Colored Ramps
    x = Table1.Option("Colored ramps - Zerg/protoss", 0, 1, 1, 0, 0, Array("On", "Off") )
    If x = 1 Then ColoredRamps = False Else ColoredRamps = True
	UpdateOptions

	' Low End PCs
    x = Table1.Option("Low-end PCs / Reduced graphics", 0, 1, 1, 0, 0, Array("No", "Yes") )
    If x = 1 Then LowEndPC = True Else LowEndPC = False

    ' Hide Toys
    x = Table1.Option("Warrior/Live Assult Range toy", 0, 1, 1, 0, 0, Array("On", "Off") )
    If x = 1 Then WarriorBugAssembly004.visible = 0	Else WarriorBugAssembly004.visible = 1
		
    ' Bug ball
    x = Table1.Option("Bug ball (requires restart)", 0, 1, 1, 0, 0, Array("Off", "On") )
    If x = 1 Then BugBall = 1 Else BugBall = 0
	UpdateOptions

    ' Flipper colors
    x = Table1.Option("Flipper colors", 0, 2, 1, 0, 0, Array("Black", "Dark Blue", "Blue") )
    If x = 1 Then 
		FlipperMod = 1
	ElseIf x = 2 Then
		FlipperMod = 2
	Else 
		FlipperMod = 0
	End If
	UpdateOptions

	' Pin between flippers
    x = Table1.Option("Trust Pin", 0, 1, 1, 0, 0, Array("On (Default)", "Off") )
    If x = 1 Then FlipperPin = False Else FlipperPin = True
	UpdateOptions

	' Kid / Testing mode - Blocks outlanes
    x = Table1.Option("Kid mode - Blocks outlanes", 0, 1, 1, 0, 0, Array("False (Default)", "True") )
    If x = 1 Then KidMode = True Else KidMode = False
	UpdateOptions

    ' Music  On/Off
    'x = Table1.Option("Music", 0, 1, 1, 1, 0, Array("OFF", "ON") )
    'If x Then bMusicOn = True Else bMusicOn = False

	' Alternate Attract Music
    'x = Table1.Option("Alternate Attract Music", 0, 1, 1, 0, 0, Array("No", "Yes") )
    'If x = 1 Then AttractMusicAlt = 1 Else AttractMusicAlt = 0
	'UpdateOptions

    ' Music Volume
    'SongVolume = Table1.Option("Music Volume", 0, 1, 0.1, 0.3, 0)
    'If bMusicOn Then
    '    PlaySound Song, -1, SongVolume, , , , 1, 0
    'Else
    '    StopSound Song
    'End If

	'Table has 10 different desktop backdrops.  Click 'Toggle backglass view' from editor toolbar.  Click dropdown 'DT image'.  Select new backdrop file.
    x = Table1.Option("Desktop backdrop changable in editor mode only", 0, 1, 1, 1, 0, Array("Not available", "Not available") )

End Sub

Sub UpdateOptions
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

    If FlipperPin = True Then
       CenterPinMetal.Visible = 1
       CenterRubber.Visible = 1
       CenterPin.Collidable = 1
    Else
       CenterPinMetal.Visible = 0
       CenterRubber.Visible = 0
       CenterPin.Collidable = 0
    End If

    If KidMode = True Then
		RightRubber.Visible = 1
		LeftRubber.Visible = 1
		RightPin.Collidable = 1
		LeftPin.Collidable = 1
		RightPinMetal.Visible = 1
		LeftPinMetal.Visible = 1	
	Else
		RightRubber.Visible = 0
		LeftRubber.Visible = 0
		RightPin.Collidable = 0
		LeftPin.Collidable = 0
		RightPinMetal.Visible = 0
		LeftPinMetal.Visible = 0	
	End If

	If FlipperMod=1 Then
		leftflipperP.image = "flipperL_mod_off"
		rightflipperP.image = "flipperR_mod_off"
	ElseIf FlipperMod=2 Then
		leftflipperP.image = "flipperL_mod_off_2"
		rightflipperP.image = "flipperR_mod_off_2"
	Else
		leftflipperP.image = "flipperL_off"
		rightflipperP.image = "flipperR_off"
	End If

	If BugBall = 1 Then Table1.BallDecalMode = True else Table1.BallDecalMode = False

	If ColoredRamps = true Then
		If RaceProtoss Then
			ChangeGi darkblue
			ChangeGiIntensity 2
			ramp_off.image = "ramps_off_mod_Protoss" 'Ramps to blue
			ramp_on.image = "ramps_on_mod_Protoss"
		ElseIf RaceZerg Then
			ChangeGi purple
			ChangeGiIntensity 1.2
			ramp_off.image = "ramps_off_mod_Zerg" 'Ramps to purple
			ramp_on.image = "ramps_on_mod_Zerg"
		End If
	Else
		ChangeGi white
		ChangeGiIntensity 1.2
		ramp_off.image = "ramp_off" 'normal
		ramp_on.image = "ramp_on"
	End If
End Sub



'*****begin HAL playing against himself************

' halplay variables
dim halplay 'halplay
halplay=0 'halplay


sub trigger018001_hit	
If Activeball.VelY < -5 Then Exit Sub
        Trigger020001.Enabled = True
        Trigger023001.Enabled = True
        FlipperActivate LeftFlipper, LFPress
SolLFlipper 1
Timer065.enabled=1
end Sub

sub timer065_timer
        FlipperDeActivate LeftFlipper, LFPress
SolLFlipper 0
Timer065.enabled=0
end Sub

sub trigger023001_hit	
trigger018001.enabled=1
Trigger019001.enabled=1
Trigger023001.enabled=0	
        FlipperActivate LeftFlipper, LFPress
SolLFlipper 1
Timer065.enabled=1
end Sub

sub trigger020001_hit		
trigger018001.enabled=1
Trigger019001.enabled=1
trigger020001.enabled=0
        FlipperActivate LeftFlipper, LFPress
SolLFlipper 1
Timer065.enabled=1
end Sub

sub trigger019001_hit	
If Activeball.VelY < -5 Then Exit Sub
        Trigger022001.Enabled = True
        Trigger021001.Enabled = True
        FlipperActivate RightFlipper, RFPress
SolRFlipper 1
Timer066.enabled=1
end Sub

sub trigger022001_hit	
trigger018001.Enabled=1
Trigger019001.Enabled=1	
trigger022001.enabled=0
        FlipperActivate RightFlipper, RFPress
SolRFlipper 1
Timer066.enabled=1
end Sub

sub trigger021001_hit	
trigger018001.enabled=1
Trigger019001.enabled=1	
trigger021001.enabled=0
        FlipperActivate RightFlipper, RFPress
SolRFlipper 1
Timer066.enabled=1
end Sub

sub trigger025001_hit	
If Int(Rnd * 3) = 0 Then
        FlipperActivate RightFlipperS, RFPress2	
SolRFlipperS 1
Timer068.enabled=1
end If
end Sub

sub Timer068_timer
        FlipperDeActivate RightFlipperS, RFPress2
SolRFlipperS 0
Timer068.enabled=0
end Sub

sub Timer066_timer
        FlipperDeActivate RightFlipper, RFPress
SolRFlipper 0
Timer066.enabled=0
end Sub

sub Kicker025001_Hit 'hal kicker for autoplay
Plunger.AutoPlunger = True
TextBox001.Text = ""
kicker025001.timerenabled=1
end sub

sub kicker025001_timer 'hal kicker for autoplay
kicker025001.kick 0, 60
'playsound "fx_kicker"
playsound "Plunger"
plunger.fire
Plunger.AutoPlunger = False
kicker025001.enabled=0
kicker025001.timerenabled=0
end Sub

sub halgame
		'vpmTimer.PulseSw 54
        trigger018001.enabled=1
        Trigger019001.enabled=1
        Trigger020001.enabled=1
        Trigger021001.enabled=1
        Trigger022001.enabled=1
        Trigger023001.enabled=1
        Trigger025001.enabled=1
        kicker025001.enabled=1
        halplay=1

		If bPlanet1 = true and Turret1.visible = 1 and bBallInMotion=False Then 'Can start HAL only once game is started on the first level
			soundStartButton()
			pf_Victory_Protoss2_overlay.visible = 0
			pf_Victory_Protoss_overlay.visible = 0
			pf_Victory_Terran2_overlay.visible = 0
			pf_Victory_Terran_overlay.visible = 0
			pf_Victory_Zerg2_overlay.visible = 0
			pf_Victory_Zerg_overlay.visible = 0
			VictoryScreenTimer.Enabled = False
			FirstFarm = 0              'Resets value for new game 
			SecondFarm = 0             '
			LastFarm = 0               '
			WaitingBuildingNext = 1    '
			pupevent 799               'Clear Supply Overlay
			Building1RingTimer.Enabled = False
			Building2RingTimer.Enabled = False
			Building3RingTimer.Enabled = False
			BuildingRingsOff
			WarpTimer1.Enabled = false
			WarpTimer2.Enabled = false
			WarpTimer3.Enabled = false
			WarpTimersOff
			KhaydarinCrystals = 0      'Resets resources 
			VespeneGas = 0             'Resets resources
			Turret1.visible = 0        'Turns OFF Buildings 
			Turret2.visible = 0        '
			Barracks.visible = 0       '
			StarPort.visible = 0       '
			CommandCenter.visible = 0  '
			pylon1a.visible = 0        '
			Pylon1b.visible = 0        '
			pylon1c.visible = 0        '
			pylon2a.visible = 0        '
			Pylon2b.visible = 0        '
			pylon2c.visible = 0        '
			Nexus.visible = 0          '
			Gateway.visible = 0        '
			StarGate.visible = 0       '
			Overlord1.visible = 0      '
			Overlord2.visible = 0      '
			Hive.visible = 0           '
			Spawing_Pool.visible = 0   '
			Spire.visible = 0          '
			bPlanet1 = True
			bPlanet2 = False
			bPlanet3 = False
			bPlanet4 = False
			bPlanet5 = False
			bPlanet6 = False
			bVictory = False
			If StartingRace = 1 Then ' Resets starting race after a game has been played
				RaceTerran = True
				RaceProtoss = False
				RaceZerg = False
			ElseIf StartingRace = 2 Then
				RaceTerran = False
				RaceProtoss = True
				RaceZerg = False
			ElseIf StartingRace = 3 Then
				RaceTerran = False
				RaceProtoss = False
				RaceZerg = True
			End If
			If RaceProtoss = False and RaceZerg = False Then
				StartingRace = 1
				RaceTerran = True
				RaceProtoss = False
				RaceZerg = False
				decals_apronT.Material = "Plastic_decals"
				decals_apronP.Material = "Plastic_decals"
				decals_apronZ.Material = "Plastic_decals"
				decals_apronZ2.Material = "Plastic_decals"
				pf_TerranVsProtoss.visible = 0
				pf_ProtossVsZerg.visible = 0
				pf_ProtossVsTerran.visible = 0
				pf_ZergVsProtoss.visible = 0
				pf_ZergVsTerran.visible = 0
				pupevent 701               'Terran overlay - Used when restarting
				pupevent 783               'Terran Backglass restart
				Turret1.visible = 1        'Turns ON Building
				Turret2.visible = 1        '
				backglass.image = "backbox_on_t"
				PlanetRace.visible = 1
				PlanetRace.imageA = "Planets_T"
				JackpotCelebration
				stepvar = 1
				SpeciesTracker.visible = True       'Species Tracker logic
				PlaySound "FX_SpeciesTrackerLaunch" '
				If LowEndPC = True Then                      'Reduced graphics tracker
					SpeciesTracker.imageA = "Frame22"        '
					opacityVar = 175                         ' 
					SpeciesTrackerFadeTimer.Enabled = True   '
				Else                                         '
					SpeciesTrackerAnimation ()               'Full graphics tracker
				End If
			Elseif RaceProtoss = True Then
				PupEvent 704
				PupEvent 703
				pf_TerranVsProtoss.visible = 0
				pf_ProtossVsZerg.visible = 1    'ON
				pf_ProtossVsTerran.visible = 0
				pf_ZergVsProtoss.visible = 0
				pf_ZergVsTerran.visible = 0
				decals_apronT.Material = "Plastic_decals_trans"
				decals_apronP.Material = "Plastic_decals"
				decals_apronZ.Material = "Plastic_decals_trans"
				decals_apronZ2.Material = "Plastic_decals_trans"
				backglass.image = "backbox_on_p"
				If ColoredRamps = true Then 
					ChangeGi darkblue
					ChangeGiIntensity 2
					ramp_off.image = "ramps_off_mod_Protoss" 'Ramps to blue
					ramp_on.image = "ramps_on_mod_Protoss"
				End If
				walls_metal.image = "walls_metal_on_mod_blue"
				SpeciesTracker.visible = False       'Species Tracker logic
				pylon1a.visible = 1
				Pylon1b.visible = 1
				pylon1c.visible = 1
				pylon2a.visible = 1
				Pylon2b.visible = 1
				pylon2c.visible = 1
			Else
				PupEvent 708
				PupEvent 707
				pf_TerranVsProtoss.visible = 0
				pf_ProtossVsZerg.visible = 0
				pf_ProtossVsTerran.visible = 0
				pf_ZergVsProtoss.visible = 1     'ON
				pf_ZergVsTerran.visible = 0
				decals_apronT.Material = "Plastic_decals_trans"
				decals_apronP.Material = "Plastic_decals_trans"
				decals_apronZ.Material = "Plastic_decals"
				decals_apronZ2.Material = "Plastic_decals_trans"
				backglass.image = "backbox_on_z"
				If ColoredRamps = true Then 
					ChangeGi purple
					ChangeGiIntensity 1.2
					ramp_off.image = "ramps_off_mod_Zerg" 'Ramps to purple
					ramp_on.image = "ramps_on_mod_Zerg"
				End If
				walls_metal.image = "walls_metal_on_mod_purple"
				Overlord1.visible = 1
				Overlord2.visible = 1 
			End If
			UpdatePupVariables()       'Updates pup screen variables (crystals/gas)
		End if
end sub

Sub Trigger026001_Hit  'when hal is playing and a ball doesnt clear the launch lane, this will shoot it back out
if halplay=1 then
    activeball.VelY = -60
    PlaySound "saucer_kick"
end if
End Sub

sub timer067_timer
'halgame
end sub

'*****end HAL playing against himself************



'PupDMD Stuffs

'********************* START OF PUPDMD FRAMEWORK v3.0 BETA *************************
'******************************************************************************
'*****   Create a PUPPack within PUPPackEditor for layout config!!!  **********
'******************************************************************************
'
'
'  Quick Steps:
'      1>  create a folder in PUPVideos with Starter_PuPPack.zip and call the folder "yourgame"
'      2>  above set global variable pGameName="yourgame"
'      3>  copy paste the settings section above to top of table script for user changes.
'      4>  on Table you need to create ONE timer only called pupDMDUpdate and set it to 250 ms enabled on startup.
'      5>  go to your table1_init or table first startup function and call PUPINIT function
'      6>  Go to bottom on framework here and setup game to call the appropriate events like pStartGame (call that in your game code where needed)...etc
'      7>  attractmodenext at bottom is setup for you already,  just go to each case and add/remove as many as you want and setup the messages to show.  
'      8>  Have fun and use pDMDDisplay(xxxx)  sub all over where needed.  remember its best to make a bunch of mp4 with text animations... looks the best for sure!
'
'

'**************************
'   PinUp Player USER Config
'**************************

Dim pGameName       : pGameName="startrp2sc"  'pupvideos foldername, probably set to cGameName in realworld


Const HasPuP = True   'dont set to false as it will break pup


dim PUPDMDObject  'for realtime mirroring.
Dim pDMDlastchk: pDMDLastchk= -1    'performance of updates
Dim pDMDCurPage: pDMDCurPage= 0     'default page is empty.
Dim pInAttract : pInAttract=false   'pAttract mode
Dim pFrameSizeX: pFrameSizeX=1920     'DO NOT CHANGE, this is pupdmd author framesize
Dim pFrameSizeY: pFrameSizeY=1080     'DO NOT CHANGE, this is pupdmd author framesize
Dim pUseFramePos : pUseFramePos=0     'DO NOT CHANGE, this is pupdmd author setting




'*************  starts PUP system,  must be called AFTER b2s/controller running so put in last line of table1_init
Sub PuPInit



'PuPType = 0
'
'	pupPackScreenFile = PuPlayer.GetRoot & "startrp2sc\ScreenType.txt"
'	Set ObjFso = CreateObject("Scripting.FileSystemObject")
'	Set ObjFile = ObjFso.OpenTextFile(pupPackScreenFile)
'	DMDType = ObjFile.ReadLine
'
'	pupPackSizeFile = PuPlayer.GetRoot & "startrp2sc\PupSize.txt"
'	Set ObjFso = CreateObject("Scripting.FileSystemObject")
'	Set ObjFile = ObjFso.OpenTextFile(pupPackSizeFile)
'	PuPType = ObjFile.ReadLine
'
'	if DMDType = 3 Then 
'		pDMDVideo = 5
'	Else
'		pDMDVideo = 11
'	End If
'
'	if DMDType > 1 Then
'		pDMD = 5
'	Else
'		pDMD = 2
'	End If

pDMD = 15
	CheckPupVersion
	PuPlayer.LabelInit pDMD
'	PuPlayer.LabelInit pDMDVideo
	PuPlayer.LabelInit pBackglass

'	if DMDType = 99 Then 
'		BatReminder.Visible = True
'	Else
'		BatReminder.Visible = False
'	End If

pSetPageLayouts

pDMDSetPage(pDMDBlank)   'set blank text overlay page.

pDMDStartUP






End Sub 'end PUPINIT

	Const cWhite = 	16777215
	Const cRed = 	397512
	Const cGold = 	1604786
	Const cGold2 = 46079
	Const cGreen = 32768
	Const cGrey = 	8421504
	Const cYellow = 65535
	Const cOrange = 33023
	Const cPurple = 16711808
	Const cBlue = 16711680
	Const cLightBlue = 16744448
	Const cBoltYellow = 2148582
	Const cLightGreen = 9747818
	Const cBlack = 0
	Const cPink = 12615935
	Const cSilver = 8421504


Const dmddef="sayfull"

'pages
Const pDMDBlank = 0
Const pGamePlay= 1
Const pAttract = 2
'Const pPrevScores = 3
'Const pCredits = 4
'Const pSlotMachine = 5
'Const pBonus = 6
'Const pEvent = 7
'Const pHighScore = 8


Sub CheckPupVersion

	Dim strPupVersion
	strPupVersion = PuPlayer.GetVersion
	strPupVersion = Replace(strPupVersion, ".", "")
	strPupVersion = mid (strPupVersion, 1,3)
    strPupVersion = CDbl(strPupVersion)

	If strPupVersion => 150 then
		exit sub
	Else
		msgbox "This table requires PuP Player version 1.5 or greater.  Please update your pup install to play the table", 0
		Table1_Exit	
	End if
End sub

Sub pupCreateLabelImageDMD(lName, lFilename,xpos, ypos, Iwidth, Iheight, pagenum, lvis)
	PuPlayer.LabelNew pDMD,lName ,"",50,RGB(100,100,100),0,1,1,1,1,pagenum,lvis
	PuPlayer.LabelSet pDMD,lName,lFilename,lvis,"{'mt':2,'width':"&IWidth&",'height':"&Iheight&",'xpos':"&xpos&",'ypos':"&ypos&"}"
end Sub

Sub pSetPageLayouts
	Dim i

	pDMDAlwaysPAD		'we pad all text with space before and after for shadow clipping/etc


	pupCreateLabelImageDMD "DMDOverlay", "PuPOverlays\\DefaultDMD.png",0,0,100,100,1,0

	'pupCreateLabelImageBG "AttractCredits","PuPOverlays\\Credits1.png",0,0,100,100,88,0


'	PuPlayer.playlistadd pDMDVideo,"StartBBQ", 1 , 0
'	PuPlayer.playlistadd pDMDVideo,"StartBeer", 1 , 0
'	PuPlayer.playlistadd pDMDVideo,"StartBill", 1 , 0
'	PuPlayer.playlistadd pDMDVideo,"StartBobby", 1 , 0
'	PuPlayer.playlistadd pDMDVideo,"StartCotton", 1 , 0
'	PuPlayer.playlistadd pDMDVideo,"StartDale", 1 , 0
'	PuPlayer.playlistadd pDMDVideo,"StartHank", 1 , 0
'	PuPlayer.playlistadd pDMDVideo,"StartLadybird", 1 , 0
'	PuPlayer.playlistadd pDMDVideo,"StartLuanne", 1 , 0
'	PuPlayer.playlistadd pDMDVideo,"StartPeggy", 1 , 0
'
'	PuPlayer.playlistadd pDMDVideo,"FailBBQ", 1 , 0
'	PuPlayer.playlistadd pDMDVideo,"FailBeer", 1 , 0
'	PuPlayer.playlistadd pDMDVideo,"FailBill", 1 , 0
'	PuPlayer.playlistadd pDMDVideo,"FailBobby", 1 , 0
'	PuPlayer.playlistadd pDMDVideo,"FailCotton", 1 , 0
'	PuPlayer.playlistadd pDMDVideo,"FailDale", 1 , 0
'	PuPlayer.playlistadd pDMDVideo,"FailHank", 1 , 0
'	PuPlayer.playlistadd pDMDVideo,"FailLadybird", 1 , 0
'	PuPlayer.playlistadd pDMDVideo,"FailLuanne", 1 , 0
'	PuPlayer.playlistadd pDMDVideo,"FailPeggy", 1 , 0
'
'	PuPlayer.playlistadd pDMDVideo,"Drain", 1 , 0
'	PuPlayer.playlistadd pDMDVideo,"Multiball", 1 , 0
'	PuPlayer.playlistadd pDMDVideo,"BallLock", 1 , 0
'	PuPlayer.playlistadd pDMDVideo,"BallSave", 1 , 0
'	PuPlayer.playlistadd pDMDVideo,"GameLaunch", 1 , 0
'	PuPlayer.playlistadd pTopper,"Topper", 1 , 0

'labelNew <screen#>, <Labelname>, <fontName>,<size%>,<colour>,<rotation>,<xalign>,<yalign>,<xpos>,<ypos>,<PageNum>,<visible>
'***********************************************************************'
'<screen#>, in standard we’d set this to pDMD ( or 1)
'<Labelname>, your name of the label. keep it short no spaces (like 8 chars) although you can call it anything really. When setting the label you will use this labelname to access the label.
'<fontName> Windows font name, this must be exact match of OS front name. if you are using custom TTF fonts then double check the name of font names.
'<size%>, Height as a percent of display height. 20=20% of screen height.
'<colour>, integer value of windows color.
'<rotation>, degrees in tenths   (900=90 degrees)
'<xAlign>, 0= horizontal left align, 1 = center horizontal, 2= right horizontal
'<yAlign>, 0 = top, 1 = center, 2=bottom vertical alignment
'<xpos>, this should be 0, but if you want to ‘force’ a position you can set this. it is a % of horizontal width. 20=20% of screen width.
'<ypos> same as xpos.
'<PageNum> IMPORTANT… this will assign this label to this ‘page’ or group.
'<visible> initial state of label. visible=1 show, 0 = off.


	PuPlayer.LabelNew pDMD,"CurrGAS",     			         dmddef,20,cGreen   ,0,1,1,0,0,1,1
	PuPlayer.LabelNew pDMD,"CurrCrystals",	        	     dmddef,15,cBlue   ,0,1,1,0,0,1,1
	PuPlayer.LabelNew pDMD,"CurrGASMini",     			     dmddef,5,cWhite  ,0,0,1,0,0,1,1
	PuPlayer.LabelNew pDMD,"CurrCrystalsMini",	        	 dmddef,5,cWhite   ,0,0,1,0,0,1,1
	PuPlayer.LabelNew pDMD,"CurrSupplyMini",	        	 dmddef,5,cWhite   ,0,0,1,0,0,1,1
	'PuPlayer.LabelNew pDMD,"Position1Score",	             dmddef,5,cBlack   ,0,0,1,33,79.5,1,0
	'PuPlayer.LabelNew pDMD,"Position2Name",	             dmddef,5,cBlack   ,0,0,0,20,89,1,0
	'PuPlayer.LabelNew pDMD,"Position2Score",	             dmddef,5,cBlack  ,0,0,1,20,94,1,0
	'PuPlayer.LabelNew pDMD,"Position3Name",	             dmddef,5,cBlack   ,0,0,0,50,89,1,0
	'PuPlayer.LabelNew pDMD,"Position3Score",	             dmddef,5,cBlack   ,0,0,1,50,94,1,0
	'PuPlayer.LabelNew pDMD,"Position4Name",	             dmddef,5,cBlack   ,0,0,0,80,89,1,0
	'PuPlayer.LabelNew pDMD,"Position4Score",	             dmddef,5,cBlack   ,0,0,1,80,94,1,0

	'pDMDLabelSetBorder "CurrGAS",cBlack,10,10,1
	'pDMDLabelSetBorder "CurrCrystals",cBlack,10,10,1

	'pDMDLabelSetBorder "Position1Score",cBlack,10,10,1
	'pDMDLabelSetBorder "Position2Score",cBlack,10,10,1
	'pDMDLabelSetBorder "Position3Score",cBlack,10,10,1
	'pDMDLabelSetBorder "Position4Score",cBlack,10,10,1
	'pDMDLabelSetBorder "Position1Name",cBlack,10,10,1
	'pDMDLabelSetBorder "Position2Name",cBlack,10,10,1
	'pDMDLabelSetBorder "Position3Name",cBlack,10,10,1
	'pDMDLabelSetBorder "Position4Name",cBlack,10,10,1

	' EOB Sequence
	'PuPlayer.LabelNew pDMD, "Event5A", dmddef, 6, cRed, 0, 1, 1, 50, 10, pScores, 0
	'PuPlayer.LabelNew pDMD, "Event5B", dmddef, 6, cRed, 0, 1, 1, 50, 20, pScores, 0
	'PuPlayer.LabelNew pDMD, "Event5C", dmddef, 6, cRed, 0, 1, 1, 50, 30, pScores, 0
	'PuPlayer.LabelNew pDMD, "Event5D", dmddef, 6, cRed, 0, 1, 1, 50, 40, pScores, 0
	'PuPlayer.LabelNew pDMD, "Event5E", dmddef, 6, cWhite, 0, 1, 1, 50, 50, pScores, 0
	'PuPlayer.LabelNew pDMD, "Event5F", dmddef, 6, cBlue, 0, 1, 1, 50, 60, pScores, 0

	'pDMDLabelSetBorder "Event5A",cBlack,6,6,1
	'pDMDLabelSetBorder "Event5B",cBlack,6,6,1
	'pDMDLabelSetBorder "Event5C",cBlack,6,6,1
	'pDMDLabelSetBorder "Event5D",cBlack,6,6,1
	'pDMDLabelSetBorder "Event5E",cBlack,6,6,1
	'pDMDLabelSetBorder "Event5F",cBlack,6,6,1


	'PuPlayer.LabelNew pDMD,"BallValue",	             dmddef,6,cWhite   ,0,1,1,65,77.1,pScores,0
	'pDMDLabelSetBorder "BallValue",cBlack,4,4,1

	'PuPlayer.LabelNew pDMD,"MainModeTimerValue",	             dmddef,8,cRed   ,0,1,1,3.8,76,1,0
	'pDMDLabelSetBorder "MainModeTimerValue",cBlack,10,10,1


	'PuPlayer.LabelNew pDMD,"BMValue",	             dmddef,12,cWhite   ,0,1,1,21.75,87.5,1,0
	'pDMDLabelSetBorder "BMValue",cBlack,4,4,1

	'PuPlayer.LabelNew pDMD,"PFMValue",	             dmddef,12,cWhite   ,0,1,1,78.75,87.5,1,0
	'pDMDLabelSetBorder "PFMValue",cBlack,4,4,1

	'PuPlayer.LabelNew pDMD,"TimerValue",	             dmddef,12,cRed   ,0,1,1,78.75,87.5,1,0
	'pDMDLabelSetBorder "TimerValue",cBlack,4,4,1

	'PuPlayer.LabelNew pDMD,"LockValue",	             dmddef,12,cRed   ,0,1,1,78.75,87.5,1,0
	'pDMDLabelSetBorder "LockValue",cBlack,4,4,1

End Sub


Sub pDMDStartUP
'do stuff fancy pants on first run
'	pInAttract = True
	'DMDSetPage(pScores)
	pDMDSetPage (1)

'PuPlayer.LabelSet pDMD, "DMDOverlay", "PupOverlays\\defaultDMD.png",1,"{'mt':2,'zback':1}"
	'PuPlayer.LabelSet pDMD,"CurrGas"," " & VespeneGas & " ",1,"{'mt':2,'fonth':14,'xpos':50,'ypos':89.5}"
	'if ScorbitActive Then
	'	dbg "Calling SCORBIT check pairing"
	'	BallHandlingQueue.Add "CheckPairing","CheckPairing",24,2000,0,0,0,False
	'End If
end Sub 'end DMDStartup

Sub pTranslatePos(Byref xpos, byref ypos)  'if using uUseFramePos then all coordinates are based on framesize
   xpos=int(xpos/pFrameSizeX*10000) / 100
   ypos=int(ypos/pFrameSizeY*10000) / 100
end Sub

Sub pTranslateY(Byref ypos)           'if using uUseFramePos then all heights are based on framesize
   ypos=int(ypos/pFrameSizeY*10000) / 100
end Sub

Sub pTranslateX(Byref xpos)           'if using uUseFramePos then all heights are based on framesize
   xpos=int(xpos/pFrameSizeX*10000) / 100
end Sub



'***********************************************************PinUP Player DMD Helper Functions

Sub pDMDLabelSet(labName,LabText)
PuPlayer.LabelSet pDMD,labName,LabText,1,""   
end sub


Sub pDMDLabelHide(labName)
PuPlayer.LabelSet pDMD,labName,"`u`",0,""   
end sub

Sub pDMDLabelShow(labName)
PuPlayer.LabelSet pDMD,labName,"`u`",1,""   
end sub

Sub pDMDLabelVisible(labName, isVis)
PuPlayer.LabelSet pDMD,labName,"`u`",isVis,""   
end sub

Sub pDMDLabelSendToBack(labName)
PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'zback': 1 }"   
end sub

Sub pDMDLabelSendToFront(labName)
PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'ztop': 1 }"   
end sub

sub pDMDLabelSetPos(labName, byVal xpos, byVal ypos)
   if pUseFramePos=1 Then pTranslatePos xpos,ypos
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'xpos':"&xpos& ",'ypos':"&ypos&"}"    
end sub

sub pDMDLabelSetSizeImage(labName, byVal lWidth, byVal lHeight)
   if pUseFramePos=1 Then pTranslatePos lWidth,lHeight
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'width':"& lWidth & ",'height':"&lHeight&"}" 
end sub

sub pDMDLabelSetSizeText(labName, byVal fHeight)
   if pUseFramePos=1 Then pTranslateY fHeight
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'fonth':"&fHeight&"}" 
end sub

sub pDMDLabelSetAutoSize(labName, byVal lWidth, byVal lHeight)
   if pUseFramePos=1 Then pTranslatePos lWidth,lHeight
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'autow':"& lWidth & ",'autoh':"&lHeight&"}" 
end sub

sub PDMDLabelSetAlign(labName,xAlign, YAlign)  '0=left 1=center 2=right,  note you should use center as much as possible because some things like rotate/zoom/etc only look correct with center align!
    PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'xalign':"& xAlign & ",'yalign':"&yAlign&"}"     
end sub

sub pDMDLabelStopAnis(labName)    'stop any pup animations on label/image (zoom/flash/pulse).  this is not about animated gifs
     PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'stopani':1 }" 
end sub

sub pDMDLabelSetRotateText(labName, fAngle)  ' in tenths.  so 900 is 90 degrees.
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'rotate':"&fAngle&"}" 
end sub

sub pDMDLabelSetRotate(labName, fAngle)  ' in tenths.  so 900 is 90 degrees. rotate support for images too.  note images must be aligned center to rotate properly(default)
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'rotate':"&fAngle&"}" 
end sub

sub pDMDLabelSetZoom(labName, fFactor)  ' fFactor is 120 for 120% of current height, 80% etc...
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'zoom':"&fFactor&"}" 
end sub

sub pDMDLabelSetColor(labName, lCol)
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'color':"&lCol&"}" 
end sub

sub pDMDLabelSetAlpha(labName, lAlpha)  '0-255  255=full, 0=blank
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'alpha':"&lAlpha&"}" 
end sub

sub pDMDLabelSetColorGradient(labName, byVal startCol, byVal EndCol)
dim GS: GS=1
if startCol=EndCol Then GS=0  'turn grad off is same colors.
PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'color':"&startCol&" ,'gradstate':"&GS&" , 'gradcolor':"&endCol&"}" 
end sub

sub pDMDLabelSetColorGradientPercent(labName, byVal startCol, byVal EndCol, byVal StartPercent)
if startCol=EndCol Then StartPercent=0  'turn grad off is same colors.
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'color':"&startCol&" ,  'gradstate':"&StartPercent&", 'gradcolor':"&endCol&"}" 
end sub

sub pDMDLabelSetGrayScale(labName, isGray)  'only on image objects.  will show as grayscale.  1=gray filter on 0=off normal mode
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'grayscale':"&isGray&"}" 
end sub

sub pDMDLabelSetFilter(labName, fMode)  ''fmode 1-5 (invertRGB, invert,grayscale,invertalpha,clear),blur)
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'filter':"&fmode&"}" 
end sub

Sub pDMDLabelFlashFilter(LabName,byVal timeSec,fMode)   'timeSec in ms  'fmode 1-5 (invertRGB, invert,grayscale,invertalpha,clear,blur)
    if timeSec<20 Then timeSec=timeSec*1000
    PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':1,'at':9,'fq':150,'len':" & (timeSec) & ",'fm':" & fMode & "}"   
end sub



sub pDMDLabelSetShadow(labName,lCol,offsetx,offsety,isVis)  ' shadow of text
dim ST: ST=1 : if isVIS=false Then St=0
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'shadowcolor':"&lCol&",'shadowtype': "&ST&", 'xoffset': "&offsetx&", 'yoffset': "&offsety&"}"
end sub

sub pDMDLabelSetBorder(labName,lCol,offsetx,offsety,isVis)   'outline/border around text.
dim ST: ST=2 : if isVIS=false Then St=0
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'shadowcolor':"&lCol&",'shadowtype': "&ST&", 'xoffset': "&offsetx&", 'yoffset': "&offsety&"}"
end sub



'animations   'pDMDLabelPulseText "pulsetext","jackpot",4000,rgb(100,0,0)

sub pDMDLabelPulseText(LabName,LabValue,mLen,mColor)       'mlen in ms
    PuPlayer.LabelSet pDMD,labName,LabValue,1,"{'mt':1,'at':4,'hstart':80,'hend':120,'len':" & (mLen) & ",'pspeed': 0,'fc':" & mColor & ",'aa':0 }"
end Sub

sub pDMDLabelPulseNumber(LabName,LabValue,mLen,mColor,pNumStart,pNumEnd,pNumformat)   'pnumformat 0 no format, 1 with thousands  mLen=ms
     PuPlayer.LabelSet pDMD,labName,LabValue,1,"{'mt':1,'at':4,'hstart':80,'hend':120,'len':" & (mLen) & ",'pspeed': 0,'fc':" & mColor & ",'numstart':"&pNumStart&",'numend' :"&pNumEnd&", 'numformat':"&pNumFormat&",'aa':0 }"    
end Sub

sub pDMDLabelPulseImage(LabName,mLen,isVis)       'mlen in ms isVis is state after animation
    PuPlayer.LabelSet pDMD,labName,"`u`",isVis,"{'mt':1,'at':4,'hstart':80,'hend':120,'len':" & (mLen) & ",'pspeed': 0 }"
end Sub

sub pDMDLabelPulseTextEX(LabName,LabValue,mLen,mColor,isVis,zStart,zEnd)       'mlen in ms  same subs as above but youspecifiy zoom start and zoom end in % height of original font.
    PuPlayer.LabelSet pDMD,labName,LabValue,isVis,"{'mt':1,'at':4,'hstart':"&zStart&",'hend':"&zEnd&",'len':" & (mLen) & ",'pspeed': 0,'fc':" & mColor & ",'aa':0 }"
end Sub

sub pDMDLabelPulseNumberEX(LabName,LabValue,mLen,mColor,pNumStart,pNumEnd,pNumformat,isVis,zStart,zEnd)   'pnumformat 0 no format, 1 with thousands  mLen=ms
     PuPlayer.LabelSet pDMD,labName,LabValue,isVis,"{'mt':1,'at':4,'hstart':"&zStart&",'hend':"&zEnd&",'len':" & (mLen) & ",'pspeed': 0,'fc':" & mColor & ",'numstart':"&pNumStart&",'numend' :"&pNumEnd&", 'numformat':"&pNumFormat&",'aa':0}"    
end Sub

sub pDMDLabelPulseImageEX(LabName,mLen,isVis,zStart,zEnd)       'mlen in ms isVis is state after animation
    PuPlayer.LabelSet pDMD,labName,"`u`",isVis,"{'mt':1,'at':4,'hstart':"&zStart&",'hend':"&zEnd&",'len':" & (mLen) & ",'pspeed': 0 }"
end Sub

sub pDMDLabelWiggleText(LabName,LabValue,mLen,mColor)       'mlen in ms  zstart MUST be less than zEND.  -40 to 40 for example
    PuPlayer.LabelSet pDMD,labName,LabValue,1,"{'mt':1,'at':8,'rstart':-45,'rend':45,'len':" & (mLen) & ",'rspeed': 5,'fc':" & mColor & ",'aa':0 }"
end Sub

sub pDMDLabelWiggleTextEX(LabName,LabValue,mLen,mColor,isVis,zStart,zEnd)       'mlen in ms  zstart MUST be less than zEND.  -40 to 40 for example
    PuPlayer.LabelSet pDMD,labName,LabValue,isVis,"{'mt':1,'at':8,'rstart':"&zStart&",'rend':"&zEnd&",'len':" & (mLen) & ",'rspeed': 5,'fc':" & mColor & ",'aa':0 }"
end Sub

sub pDMDLabelWiggleImage(LabName,mLen,isVis)         'mlen in ms  zstart MUST be less than zEND.  -40 to 40 for example
    PuPlayer.LabelSet pDMD,labName,"`u`",isVis,"{'mt':1,'at':8,'rstart':-45,'rend':45,'len':" & (mLen) & ",'rspeed': 5,'fc':" & 0 & ",'aa':0 }"
end Sub

sub pDMDLabelWiggleImageEX(LabName,mLen,isVis,zStart,zEnd)       'mlen in ms  zstart MUST be less than zEND.  -40 to 40 for example
    PuPlayer.LabelSet pDMD,labName,"`u`",isVis,"{'mt':1,'at':8,'rstart':"&zStart&",'rend':"&zEnd&",'len':" & (mLen) & ",'rspeed': 5,'fc':" & 0 & ",'aa':0 }"
end Sub

sub pDMDLabelClone(LabName,LabValue,mLen,mColor,pX,pY)   'px,PY  use with temp label to repeat control.
     if pUseFramePos=1 Then pTranslatePos pX,pY
     PuPlayer.LabelSet pDMD,labName,LabValue,0,"{'mt':1,'at':10, 'len':" & (mLen) & ", 'fc':" & mColor & ",'xp':"&pX&",'yp' :"&pY&" ,'ad':1, 'dl':1000 }"    
end Sub

sub pDMDLabelCloneDelay(LabName,LabValue,mLen,mColor,pX,pY,dL)   'px,PY  use with temp label to repeat control.  dL delay ms
     if pUseFramePos=1 Then pTranslatePos pX,pY
     PuPlayer.LabelSet pDMD,labName,LabValue,0,"{'mt':1,'at':10, 'len':" & (mLen) & ", 'fc':" & mColor & ",'xp':"&pX&",'yp' :"&pY&" ,'ad':1, 'dl':"&dL&" }"    
end Sub


sub pDMDPNGAnimate(labName,cSpeed)  'speed is frame timer, 0 = stop animation  100 is 10fps for animated png and gif nextframe timer.
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'animate':"&cSpeed&"}" 
end sub

sub pDMDPNGAnimateEx(labName,startFrame,endFrame,LoopMode)  'sets up the apng/gif settings before you call animate.  if you set start/end frame same if will display that frame, set start to -1 to reset settings.
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'gifstart':"&startFrame&",'gifend':"&endFrame&",'gifloop':"&loopMode&" }"          'gifstart':3, 'gifend':10, 'gifloop': 1
end sub

sub pDMDPNGShowFrame(labName,fFrame)  'in a animated png/gif, will set it to an individual frame so you could use as an imagelist control
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'gifstart':"&fFrame&",'gifend':"&fFrame&" }"          '
end sub

sub pDMDPNGAnimateOnce(labName,cSpeed)  'will show an animated gif/png and then hide when done, overrides loop to force stop at end.
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'animate':"&cSpeed&", 'gifloop': 0 , 'aniendhide':1 }" 
end sub

sub pDMDPNGAnimateReset(labName)  'speed is frame timer, 0 = stop animation  100 is 10fps for animated png and gif nextframe timer, this will show anigif and hide at end no loop
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'animate':0, 'gifloop': 1 , 'aniendhide':0 , 'gifstart':-1}" 
end sub

sub pDMDPNGAnimateOnceAndDispose(labName,fName, cSpeed)  'speed is frame timer, 0 = stop animation  100 is 10fps for animated png and gif nextframe timer, this will show anigif and hide at end no loop
   PuPlayer.LabelSet pDMD,labName,fName,1,"{'mt':2,'animate':"&cSpeed&", 'gifloop': 0 , 'aniendhide':1, 'anidispose':1 }" 
end sub

sub pDMDLabelSetOutShadow(labName, lCol,offsetx,offsety,isOutline,isVis)
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'shadowcolor':"&lCol&",'shadowstate': "&isVis&", 'xoffset': "&offsetx&", 'yoffset': "&offsety&", 'outline': "&isOutline&"}"
end sub

sub pDMDLabelMoveHorz(LabName,LabValue,mLen,mColor,pMoveStart,pMoveEnd)   'pmovestart is -1= left-off 0=current pos 1=right-off    or can use % 
     PuPlayer.LabelSet pDMD,labName,LabValue,1,"{'mt':1,'at':2, 'len':" & (mLen) & ", 'fc':" & mColor & ",'xps':"&pMoveStart&",'xpe' :"&pMoveEnd&", 'tt':2,'ad':1 }"    
end Sub

sub pDMDLabelMoveVert(LabName,LabValue,mLen,mColor,pMoveStart,pMoveEnd)   'pmovestart is -1= left-off 0=current pos 1=right-off   or can use %  
     PuPlayer.LabelSet pDMD,labName,LabValue,1,"{'mt':1,'at':2, 'len':" & (mLen) & ", 'fc':" & mColor & ",'yps':"&pMoveStart&",'ype' :"&pMoveEnd&", 'tt':2,'ad':1 }"    
end Sub

sub pDMDLabelMoveTO(LabName,LabValue,mLen,mColor,byVal pStartX,byVal pStartY,byVal pEndX,byVal pEndY)   'pmovestart is -1= left-off 0=current pos 1=right-off
     if pUseFramePos=1 AND (pStartX+pStartY+pEndx+pendY)>4 Then 
                       pTranslatePos pStartX,pStartY
                       pTranslatePos pEndX,pEndY
     end IF 
     PuPlayer.LabelSet pDMD,labName,LabValue,1,"{'mt':1,'at':2, 'len':" & (mLen) & ", 'fc':" & mColor & ",'xps':"&pStartX&",'xpe' :"&pEndX& ",'yps':"&pStartY&",'ype' :"&pEndY&", 'tt':2 ,'ad':1}"    
end Sub

sub pDMDLabelMoveHorzFade(LabName,LabValue,mLen,mColor,pMoveStart,pMoveEnd)   'pmovestart is -1= left-off 0=current pos 1=right-off, or can use %
     PuPlayer.LabelSet pDMD,labName,LabValue,0,"{'mt':1,'at':2, 'len':" & (mLen) & ", 'fc':" & mColor & ",'xps':"&pMoveStart&",'xpe' :"&pMoveEnd&", 'tt':2 ,'ad':1, 'af':700}"    
end Sub

sub pDMDLabelMoveVertFade(LabName,LabValue,mLen,mColor,pMoveStart,pMoveEnd)   'pmovestart is -1= left-off 0=current pos 1=right-off  or can use %   
     PuPlayer.LabelSet pDMD,labName,LabValue,0,"{'mt':1,'at':2, 'len':" & (mLen) & ", 'fc':" & mColor & ",'yps':"&pMoveStart&",'ype' :"&pMoveEnd&", 'tt':2 ,'ad':1, 'af':700}"    
end Sub

sub pDMDLabelMoveTOFade(LabName,LabValue,mLen,mColor,byVal pStartX,byVal pStartY,byVal pEndX,byVal pEndY)   'pmovestart is -1= left-off 0=current pos 1=right-off
     if pUseFramePos=1 AND (pStartX+pStartY+pEndx+pendY)>4 Then 
                       pTranslatePos pStartX,pStartY
                       pTranslatePos pEndX,pEndY
     end IF 
     PuPlayer.LabelSet pDMD,labName,LabValue,0,"{'mt':1,'at':2, 'len':" & (mLen) & ", 'fc':" & mColor & ",'xps':"&pStartX&",'xpe' :"&pEndX& ",'yps':"&pStartY&",'ype' :"&pEndY&", 'tt':6 ,'ad':1, 'af':700}"    
end Sub

sub pDMDLabelFadeOut(LabName,mLen)   'alpha is 255 max, 0=clear.  
     PuPlayer.LabelSet pDMD,labName,"`u`",0,"{'mt':1,'at':5,'astart':255,'aend':0,'len':" & (mLen) & " }"    
end Sub

sub pDMDLabelFadeIn(LabName,mLen)    'alpha is 255 max, 0=clear. 
     PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':1,'at':5,'astart':0,'aend':255,'len':" & (mLen) & " }"    
end Sub


sub pDMDLabelFadePulse(LabName,mLen,mColor)   'alpha is 255 max, 0=clear. alpha start/end and pulsespeed of change
    PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':1,'at':6,'astart':70,'aend':255,'len':" & (mLen) & ",'pspeed': 40,'fc':" & mColor & "}" 
end Sub

Sub pDMDLabelFlash(LabName,byVal timeSec, mColor)   'timeSec in ms
    if timeSec<20 Then timeSec=timeSec*1000
    PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':1,'at':1,'fq':150,'len':" & (timeSec) & ",'fc':" & mColor & "}"   
end sub



sub pDMDScreenFadeOut(LabName,mLen)   'alpha is 255 max, 0=clear.  
     PuPlayer.LabelSet pDMD,labName,"`u`",0,"{'mt':1,'at':7,'astart':255,'aend':0,'len':" & (mLen) & " }"    
end Sub

sub pDMDScreenFadeIn(LabName,mLen)    'alpha is 255 max, 0=clear. 
     PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':1,'at':7,'astart':0,'aend':255,'len':" & (mLen) & " }"    
end Sub



Sub pDMDScrollBig(LabName,msgText,byVal timeSec,mColor) 'timeSec in MS
if timeSec<20 Then timeSec=timeSec*1000
PuPlayer.LabelSet pDMD,LabName,msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':" & (timeSec) & ",'mlen':" & (timeSec*1) & ",'tt':0,'fc':" & mColor & "}"
end sub

Sub pDMDScrollBigV(LabName,msgText,byVal timeSec,mColor) 'timeSec in MS
if timeSec<20 Then timeSec=timeSec*1000
PuPlayer.LabelSet pDMD,LabName,msgText,0,"{'mt':1,'at':2,'yps':1,'ype':-1,'len':" & (timeSec) & ",'mlen':" & (timeSec*0.8) & ",'tt':0,'fc':" & mColor & "}"
end sub


Sub pDMDZoomBig(LabName,msgText,byVal timeSec,mColor,isVis,byVal zStart,byVal zEnd)  'timeSec in MS  zstart/end is % of screen height  notice aa antialias is 0 for big font zooms for performance.  'ns is size by %label height.
if timeSec<20 Then timeSec=timeSec*1000
PuPlayer.LabelSet pDMD,LabName,msgText,isVis,"{'mt':1,'at':3,'hstart':" & (zStart) & ",'hend':" & (zEnd) & ",'len':" & (timeSec) & ",'mlen':" & (timeSec*0.4) & ",'tt':" & 0 & ",'fc':" & mColor & ", 'ns':1, 'aa':0}"
end sub




Sub AudioDuckPuP(MasterPuPID,VolLevel)  
'will temporary volume duck all pups (not masterid) till masterid currently playing video ends.  will auto-return all pups to normal.
'VolLevel is number,  0 to mute 99 for 99%  
PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& MasterPuPID& ", ""FN"": 42, ""DV"": "&VolLevel&" }"             
end Sub

Sub AudioDuckPuPAll(MasterPuPID,VolLevel)  
'will temporary volume duck all pups (not masterid) till masterid currently playing video ends.  will auto-return all pups to normal.
'VolLevel is number,  0 to mute 99 for 99%  
PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& MasterPuPID& ", ""FN"": 42, ""DV"": "&VolLevel&" , ""ALL"":1 }"             
end Sub




Sub pSetAspectRatio(PuPID, arWidth, arHeight)
     PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&PuPID& ", ""FN"": 50, ""WIDTH"": "&arWidth&", ""HEIGHT"": "&arHeight&" }"   
end Sub  

Sub pDisableLoopRefresh(PuPID)
     PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&PuPID& ", ""FN"": 2, ""FF"":0, ""FO"":0 }"   
end Sub  

'set safeloop mode on current playing media.  Good for background videos that refresh often?  { "mt":301, "SN": XX, "FN":41 }
Sub pSafeLoopModeCurrentVideo(PuPID)
     PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&PuPID& ", ""FN"": 41 }"   
end Sub  

Sub pSetLowQualityPc  'sets fulldmd to run in lower quality mode (slowpc mode)  AAlevel for text is removed and other performance/quality items.  default is always run quality, 
     PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 5, ""FN"":45, ""SP"":1 }"    'slow pc mode
end Sub 

Sub pDMDSetTextQuality(AALevel)  '0 to 4 aa.  4 is sloooooower.  default 1,  perhaps use 2-3 if small desktop view.  only affect text quality.  can set per label too with 'qual' settings.
     PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 5, ""FN"":52, ""SC"": "& AALevel &" }"    'slow pc mode
end Sub   

Sub pDMDLabelDispose(labName)   'not needed unless you want to want to free a heavy resource label from cache/memory.  or temp lables that you created.  performance reasons.
      PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'dispose': 1 }"   
end Sub

Sub pDMDAlwaysPAD  'will pad all text with a space before and after to help with possible text clipping.
     PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 5, ""FN"":46, ""PA"":1 }"    'slow pc mode
end Sub   


Sub pDMDSetHUD(isVis)   'show hide just the pBackGround object (HUD overlay).      
    pDMDLabelVisible "pBackGround",isVis
end Sub

Sub pDMDSplashTwoLines(msgText,msgText2,timeSec,mColor)'Para uso normal del DMD
	PuPlayer.LabelShowPage pDMD,1,timeSec,""
	SupressModeMessages timeSec

	PuPlayer.LabelSet pDMD,"Event3A","",0,""
	PuPlayer.LabelSet pDMD,"Event3B","",0,""
	PuPlayer.LabelSet pDMD,"Event3C","",0,""

	PuPlayer.LabelSet pDMD,"Splash2a",msgText,1,"{'mt':2,'color': " & mColor &" }"  
	PuPlayer.LabelSet pDMD,"Splash2b",msgText2,1,"{'mt':2,'color': " & mColor &" }"  
end Sub  

Sub ClearTwoLines
	PuPlayer.LabelSet pDMD,"Splash2a","",0,""  
	PuPlayer.LabelSet pDMD,"Splash2b","",0,""
End Sub


Sub pDMDSetPage(pagenum)    
    PuPlayer.LabelShowPage pDMD,pagenum,0,""   'set page to blank 0 page if want off
    PDMDCurPage=pagenum
end Sub

Sub pDMDSplashPage(pagenum, cTime)    'cTime is seconds.  3 5,  it will auto return to current page after ctime
    PuPlayer.LabelShowPage pDMD,pagenum,cTime,""   'set page to blank 0 page if want off
    PDMDCurPage=pagenum
end Sub

Dim PBackglassCurPage
Sub pBackglassSetPage(pagenum)    
    PuPlayer.LabelShowPage pBackglass,pagenum,0,""   'set page to blank 0 page if want off
    PBackglassCurPage=pagenum
end Sub

Sub pBackglassLabelShow(labName)
PuPlayer.LabelSet pBackglass,labName,"",1,""   
end sub

Sub pBackglassLabelHide(labName)
PuPlayer.LabelSet pBackglass,labName,"",0,""   
end sub


Sub PDMDSplashPagePlaying(pagenum)  'will hide HUD and show labepage while current media is playing. and then autoreturn.
    PuPlayer.LabelShowPage pDMD,pagenum,500,"hidehudplay"
end Sub    

Sub PDMDSplashPagePlayingHUD(pagenum)  'will show labelpage and auto return to def after current video stopped
    PuPlayer.LabelShowPage pDMD,pagenum,500,"returnplay"
end Sub    


Sub pHideOverlayDuringCurrentPlay() 'will hide pup text labels and HUD till current video stops playing.
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& "5"& ", ""FN"": 34 }"             'hideoverlay text during next videoplay on DMD auto return
end Sub


Sub pSetVideoPosMS(mPOS)  'set position of video/audio in ms,  must be playing already or will be ignored.  { "mt":301, "SN": XX, "FN":51, "SP": 3431} 
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& "5"& ", ""FN"": 51, ""SP"":"&mPOS&" }"
end Sub

sub pAllVisible(lvis)   '0/1 to show hide pup text overlay and HUD
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& "5"& ",""OT"":"&lvis&", ""FN"": 3 }"             'hideoverlay text force
end Sub


Sub pDMDSetBackFrame(fname)
  PuPlayer.playlistplayex pDMD,"PUPFrames",fname,0,1    
end Sub

Sub pDMDBackLoopStart(fPlayList,fname)
  PuPlayer.playlistplayex pDMD,fPlayList,fname,0,1
  PuPlayer.SetBackGround pDMD,1
end Sub

Sub pDMDBackLoopStop
  PuPlayer.SetBackGround pDMD,0
  PuPlayer.playstop pDMD
end Sub

'jukebox mode will auto advance to next media in playlist and you can use next/prior sub to manuall advance
'you should really have a specific pupid# display like musictrack that is only used for the playlist
'sub PUPDisplayAsJukebox(pupid) needs to be called/set prior to sending your first media to that pupdisplay.
'pupid=pupdiplay# like pMusic

Sub PUPDisplayAsJukebox(pupid)
PuPlayer.SendMSG("{'mt':301, 'SN': " & pupid & ", 'FN':30, 'PM':1 }")
End Sub

Sub PuPlayListPrior(pupid)
 PuPlayer.SendMSG("{'mt':301, 'SN': " & pupid & ", 'FN':31, 'PM':1 }")
End Sub

Sub PuPlayListNext(pupid)
 PuPlayer.SendMSG("{'mt':301, 'SN': " & pupid & ", 'FN':31, 'PM':2 }")
End Sub

Sub pDMDPause()
 PuPlayer.playpause pDMD
end Sub

Sub pDMDResume()
 PuPlayer.playresume pDMD
end Sub

Sub pDMDStop()
 PuPlayer.playstop pDMD
end Sub

Sub pDMDVolumeDef(cVol)  'sets the default volume of player, doesnt affect current playing media
 PuPlayer.setVolume pdmd,cVol
end Sub

Sub pDMDVolumeCurrent(cVol)  'sets the volume of current media (like to duck audio), doesnt affect default volume for next media.
 PuPlayer.setVolumeCurrent pdmd,cVol
end Sub

Sub pDMDSetLoop(isLoop)     'it will loop the currently playing file 0=cancel looping 1=loop
 PuPlayer.setLoop pDMD,isLoop
end Sub

Sub pDMDBackground(isBack)  'will set the currently playing file as background video and continue to loop and return to it automatically 0=turn off as background.
 PuPlayer.setBackground pDMD,isBack
end Sub


Sub pupCreateLabel(lName, lValue, lFont, lSize, lColor, xpos, ypos,pagenum, lvis)
PuPlayer.LabelNew pDMD,lName ,lFont,lSize,lColor,0,1,1,1,1,pagenum,lvis
if pUseFramePos=1 Then pTranslatePos xpos,ypos
if pUseFramePos=1 Then pTranslateY lSize
PuPlayer.LabelSet pDMD,lName,lValue,lvis,"{'mt':2,'xpos':"& xpos & ",'ypos':"&ypos&",'fonth':"&lsize&",'v2':1 }"
end Sub

Sub pupCreateLabelImage(lName, lFilename,xpos, ypos, Iwidth, Iheight, pagenum, lvis)
PuPlayer.LabelNew pDMD,lName ,"",50,RGB(100,100,100),0,1,1,0,1,pagenum,lvis
if pUseFramePos=1 Then pTranslatePos xpos,ypos
if pUseFramePos=1 Then pTranslatePos Iwidth,iHeight
PuPlayer.LabelSet pDMD,lName,lFilename,lvis,"{'mt':2,'width':"&IWidth&",'height':"&Iheight&",'xpos':"&xpos&",'ypos':"&ypos&",'v2':1 }"
end Sub

Sub pupCreateLabelImageBG(lName, lFilename,xpos, ypos, Iwidth, Iheight, pagenum, lvis)
PuPlayer.LabelNew pBackglass,lName ,"",50,RGB(100,100,100),0,1,1,0,1,pagenum,lvis
if pUseFramePos=1 Then pTranslatePos xpos,ypos
if pUseFramePos=1 Then pTranslatePos Iwidth,iHeight
PuPlayer.LabelSet pBackglass,lName,lFilename,lvis,"{'mt':2,'width':"&IWidth&",'height':"&Iheight&",'xpos':"&xpos&",'ypos':"&ypos&",'v2':1 }"
end Sub



	Const pTopper	=0
	Dim pDMD
	Const pBackglass=2
	Dim pDMDFull 
	Dim pDMDVideo 
	Const pPlayfield=3
	Const pMusic	=4
	Const pAudio	=7
	Const pCallouts	=8
	Const pOverVid	=11
	Const pTransp   =15
	Const pHS		=16
	Const pHS2		=17




