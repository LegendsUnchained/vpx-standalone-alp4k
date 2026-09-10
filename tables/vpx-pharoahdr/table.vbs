Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="pharo_l2",UseSolenoids=1,UseLamps=1,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff",SCoin="coin"

LoadVPM"01500000","S7.VBS",3.1
Dim DesktopMode: DesktopMode = PharaohDeadRise.ShowDT
Dim startgamesound

'*************************************************************

'Solenoid Call backs
'********************************************************************************************************** 
SolCallback(1)="bsTrough.SolIn"
SolCallback(2)="bsTrough.SolOut"
SolCallback(4)= "UGI"	'Upper PF GI Relay
SolCallback(5)= "PGI"	'PF GI Relay
SolCallback(6)="bsSlavesTomb.SolOut"
SolCallback(7)="bsHiddenTomb.SolOut"
SolCallback(9)="dtUL.SolDropUp"
SolCallback(10)="dtUR.SolDropUp"
SolCallback(11)="dtLL.SolDropUp"
SolCallback(12)="dtLR.SolDropUp"
SolCallback(13)="bsLock.SolOut"
SolCallback(14)="bsSaucer.SolOut"
SolCallback(23)="vpmNudge.SolGameOn"

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound"fx_Flipperup":LeftFlipper.RotateToEnd:LeftFlipper1.RotateToEnd
     Else
         PlaySound "fx_Flipperdown":LeftFlipper.RotateToStart:LeftFlipper1.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound"fx_Flipperup":RightFlipper.RotateToEnd:RightFlipper1.RotateToEnd
     Else
         PlaySound "fx_Flipperdown":RightFlipper.RotateToStart:RightFlipper1.RotateToStart
     End If
End Sub

Sub Spinner1_Spin
PlaySound "fx_spinner"
End Sub

'*************************************************************
'	     FLASHERS AND TORCHES
'*************************************************************

Sub FlasherTimer_Timer
if Spotlight.state=1 then 
        SpotlightPharaoh.visible=1
     else      
	    SpotlightPharaoh.visible=0
    end If

if Spotlight.state=1 then 
        SpotlightLeftWall.visible=1
     else      
	    SpotlightLeftWall.visible=0
    end If

if Spotlight.state=1 then 
        SpotlightBackWall.visible=1
     else      
	    SpotlightBackWall.visible=0
    end If

if Spotlight.state=1 then 
        SpotlightBackWallEdge.visible=1
     else      
	    SpotlightBackWallEdge.visible=0
    end If

if CobraSpotlight.state=1 then 
        CobraHighlight.visible=1
     else      
	    CobraHighlight.visible=0
    end If

if CobraSpotlight.state=1 then 
        CobraAcorn.visible=1
     else      
	    CobraAcorn.visible=0
    end If

if GI_26.state=1 then 
        PharaohChamberFlasher.visible=1
     else      
	    PharaohChamberFlasher.visible=0
    end If

if GI_26.state=1 then 
        PCFlasherL.visible=1
     else      
	    PCFlasherL.visible=0
    end If

if GI_26.state=1 then 
        PCFlasherR.visible=1
     else      
	    PCFlasherR.visible=0
    end If

if GI_66.state=1 then 
        AltarShuteGlow.visible=1
     else      
	    AltarShuteGlow.visible=0
    end If

if GI_66.state=1 then 
        MummyHandShadow.visible=1
     else      
	    MummyHandShadow.visible=0
    end If

if GI_66.state=1 then 
        AltarShuteLeftWallFlasher.visible=1
     else      
	    AltarShuteLeftWallFlasher.visible=0
    end If

if GI_22.state=1 then 
        DaggerGlowRight.visible=1
     else      
	    DaggerGlowRight.visible=0
    end If

if GI_24.state=1 then 
        DaggerGlowLeft.visible=1
     else      
	    DaggerGlowLeft.visible=0
    end If

if GI_26.state=1 then 
        PharaohChamberFlasher.visible=1
     else      
	    PharaohChamberFlasher.visible=0
    end If

if GI_26.state=1 then 
        PCFlasherL.visible=1
     else      
	    PCFlasherL.visible=0
    end If

if GI_26.state=1 then 
        PCFlasherR.visible=1
     else      
	    PCFlasherR.visible=0
    end If

if GI_34.state=1 then 
        SarcophagusGlow.visible=1
     else      
	    SarcophagusGlow.visible=0
    end If

if L21.state=1 then 
        FlasherP.visible=1
     else      
	    FlasherP.visible=0
    end If

if L22.state=1 then 
        FlasherH1.visible=1
     else      
	    FlasherH1.visible=0
    end If

if L23.state=1 then 
        FlasherA1.visible=1
     else      
	    FlasherA1.visible=0
    end If

if L24.state=1 then 
        FlasherR.visible=1
     else      
	    FlasherR.visible=0
    end If

if L25.state=1 then 
        FlasherA2.visible=1
     else      
	    FlasherA2.visible=0
    end If

if L26.state=1 then 
        FlasherO.visible=1
     else      
	    FlasherO.visible=0
    end If

if L27.state=1 then 
        FlasherH2.visible=1
     else      
	    FlasherH2.visible=0
    end If

if L49.state=1 then 
        FlasherHORUSEYE.visible=1
     else      
	    FlasherHORUSEYE.visible=0
    end If

if L49.state=1 then 
        FlasherHORUSEYEreflect.visible=1
     else      
	    FlasherHORUSEYEreflect.visible=0
    end If

if L50.state=1 then 
        FlasherDEAD.visible=1
     else      
	    FlasherDEAD.visible=0
    end If

if L50.state=1 then 
        FlasherDEADreflect.visible=1
     else      
	    FlasherDEADreflect.visible=0
    end If

if L51.state=1 then 
        FlasherRISE.visible=1
     else      
	    FlasherRISE.visible=0
    end If

if L51.state=1 then 
        FlasherRISEreflect.visible=1
     else      
	    FlasherRISEreflect.visible=0
    end If

if GI_59.state=1 then 
        daggereyesleft.visible=1
     else      
	    daggereyesleft.visible=0
    end If

if GI_54.state=1 then 
        AltarGlow.visible=1
     else      
	    AltarGlow.visible=0
    end If

if GI_51.state=1 then 
        PlugGlow.visible=1
     else      
	    PlugGlow.visible=0
    end If

if GI_39.state=1 then 
        PlugGlow2.visible=1
     else      
	    PlugGlow2.visible=0
    end If

if GI_51.state=1 then 
        SpotlightGlowUp.visible=1
     else      
	    SpotlightGlowUp.visible=0
    end If

if GI_39.state=1 then 
        SpotlightGlowUp2.visible=1
     else      
	    SpotlightGlowUp2.visible=0
    end If

if GI_59.state=1 then 
        daggereyesleft.visible=1
        ApronGlowLeft.visible=1
     else      
	    daggereyesleft.visible=0
        ApronGlowLeft.visible=0
    end If

if GI_60.state=1 then 
        daggereyesright.visible=1
        ApronGlowRight.visible=1     
     else      
	    daggereyesright.visible=0
        ApronGlowRight.visible=0
    end If

if GI_25.state=1 then 
        LeftRockWallHighlightLower.visible=1
     else      
	    LeftRockWallHighlightLower.visible=0
    end If

if GI_34.state=1 then 
        LeftRockWallHLUF.visible=1
     else      
	    LeftRockWallHLUF.visible=0
    end If

if GI_65.state=1 then 
        LeftRockWallHLUB.visible=1
     else      
	    LeftRockWallHLUB.visible=0
    end If

if GI_12.state=1 then 
        RightRockWallHLUF.visible=1
     else      
	    RightRockWallHLUF.visible=0
    end If

if GI_14.state=1 then 
        RightRockWallHLUB.visible=1
     else      
	    RightRockWallHLUB.visible=0
    end If

if GI_14.state=1 then 
        CobraBottomHighlight.visible=1
     else      
	    CobraBottomHighlight.visible=0
    end If

if GI_65.state=1 then 
        BracketGlow.visible=1
     else      
	    BracketGlow.visible=0
    end If

if GI_14.state=1 then 
        LBracketGlow.visible=1
     else      
	    LBracketGlow.visible=0
    end If

if GI_37.state=1 then 
        AnibusGlow.visible=1
     else      
	    AnibusGlow.visible=0
    end If

if GI_24.state=1 then 
        AcornNutGlow1.visible=1
        AcornNutGlow.visible=1
     else      
	    AcornNutGlow1.visible=0
        AcornNutGlow.visible=0 
    end If

if GI_22.state=1 then 
        AcornNutGlow2.visible=1
        AcornNutGlow3.visible=1
     else      
	    AcornNutGlow2.visible=0
        AcornNutGlow3.visible=0 
    end If

if GI_25.state=1 then 
        AcornNutGlow4.visible=1
     else      
	    AcornNutGlow4.visible=0
    end If

if GI_47.state=1 then 
        AcornNutGlow5.visible=1
        AcornNutGlow6.visible=1
        AcornNutGlow7.visible=1
        AcornNutGlow8.visible=1
     else      
	    AcornNutGlow5.visible=0
        AcornNutGlow6.visible=0
        AcornNutGlow7.visible=0
        AcornNutGlow8.visible=0
    end If

if GI_34.state=1 then 
        AcornNutGlow9.visible=1
        AcornNutGlow10.visible=1
     else      
	    AcornNutGlow9.visible=0
       AcornNutGlow10.visible=0  
    end If

if GI_54.state=1 then 
        AcornNutGlow11.visible=1
     else      
	    AcornNutGlow11.visible=0
    end If

if GI_39.state=1 then 
        AcornNutGlow12.visible=1
        AcornNutGlow13.visible=1  
     else      
	    AcornNutGlow12.visible=0
        AcornNutGlow13.visible=0   
    end If

if GI_16.state=1 then 
        AcornNutGlow14.visible=1
     else      
	    AcornNutGlow14.visible=0
    end If

if GI_14.state=1 then 
        AcornNutGlow15.visible=1
     else      
	    AcornNutGlow15.visible=0
    end If

if GI_37.state=1 then 
        AcornNutGlow16.visible=1
        AcornNutGlow17.visible=1    
        AcornNutGlow18.visible=1
     else      
	    AcornNutGlow16.visible=0
        AcornNutGlow17.visible=0   
        AcornNutGlow18.visible=0
    end If
End Sub

Sub SpotlightOFF_Hit
    if Activeball.vely < 0 then
		SpotlightPlastic.duration 1,2000,0
        Spotlight.duration 1,2000,0
		me.uservalue=1
		me.timerenabled= 1
	end if
End Sub

'*************************************************************
'	              TORCH FLICKERS BY JP SALAS
'*************************************************************

Dim LeftTorchStep, RightTorchStep
LeftTorchStep = 0
RightTorchStep = 0

Sub LeftTorchTimer_Timer
    Select Case LeftTorchStep
        Case 0 '1
            TorchLeft2.State = 1
            FlasherTorchLeftGlow.IntensityScale = 1
            FlasherTorchLeftGlowEdge.IntensityScale = 1
        Case 1 '0
            TorchLeft2.State = 0
            FlasherTorchLeftGlow.IntensityScale = 0.80
            FlasherTorchLeftGlowEdge.IntensityScale = 0.80
        Case 2 '1
            TorchLeft2.State = 1
            FlasherTorchLeftGlow.Visible = 1
            FlasherTorchLeftGlowEdge.Visible = 1
        Case 3 '1
        Case 4 '0
            TorchLeft2.State = 0
            FlasherTorchLeftGlow.IntensityScale = 0.80
            FlasherTorchLeftGlowEdge.IntensityScale = 0.80
        Case 5 '0
            FlasherTorchLeftGlow.IntensityScale = 0.60
            FlasherTorchLeftGlowEdge.IntensityScale = 0.60
        Case 6 '1
            TorchLeft2.State = 1
            FlasherTorchLeftGlow.IntensityScale = 1
            FlasherTorchLeftGlowEdge.IntensityScale = 1
        Case 7 '1
        Case 8 '1
        Case 9 '0
            TorchLeft2.State = 0
            FlasherTorchLeftGlow.IntensityScale = 0.80
            FlasherTorchLeftGlowEdge.IntensityScale = 0.80
        Case 10 '0
            FlasherTorchLeftGlow.IntensityScale = 0.60
            FlasherTorchLeftGlowEdge.IntensityScale = 0.60
        Case 11 '0
            FlasherTorchLeftGlow.IntensityScale = 0.40
            FlasherTorchLeftGlowEdge.IntensityScale = 0.40
        Case 12 '1
            TorchLeft2.State = 1
            FlasherTorchLeftGlow.IntensityScale = 1
            FlasherTorchLeftGlowEdge.IntensityScale = 1
        Case 13 '0
            TorchLeft2.State = 0
            FlasherTorchLeftGlow.IntensityScale = 0.80
            FlasherTorchLeftGlowEdge.IntensityScale = 0.80
        Case 14 '1
            TorchLeft2.State = 1
            FlasherTorchLeftGlow.IntensityScale = 1
            FlasherTorchLeftGlowEdge.IntensityScale = 1
        Case 15 '0
            TorchLeft2.State = 0
            FlasherTorchLeftGlow.IntensityScale = 0.80
            FlasherTorchLeftGlowEdge.IntensityScale = 0.80
    End Select
    LeftTorchStep = (LeftTorchStep + 1) MOD 16
End Sub

Sub RightTorchTimer_Timer
    Select Case RightTorchStep
        Case 0 '1
            TorchRight2.State = 1
            FlasherTorchRightGlow.IntensityScale = 1
            FlasherTorchRightGlowEdge.IntensityScale = 1
        Case 1 '0
            TorchRight2.State = 0
            FlasherTorchRightGlow.IntensityScale = 0.80
            FlasherTorchRightGlowEdge.IntensityScale = 0.80
        Case 2 '0
            FlasherTorchRightGlow.IntensityScale = 0.60
            FlasherTorchRightGlowEdge.IntensityScale = 0.60
        Case 3 '0
            FlasherTorchRightGlow.IntensityScale = 0.40
            FlasherTorchRightGlowEdge.IntensityScale = 0.40
        Case 4 '0
            FlasherTorchRightGlow.IntensityScale = 0.20
            FlasherTorchRightGlowEdge.IntensityScale = 0.20
        Case 5 '1
            TorchRight2.State = 1
            FlasherTorchRightGlow.IntensityScale = 1
            FlasherTorchRightGlowEdge.IntensityScale = 1
        Case 6 '1
        Case 7 '1
        Case 8 '0
            TorchRight2.State = 0
            FlasherTorchRightGlow.IntensityScale = 0.80
            FlasherTorchRightGlowEdge.IntensityScale = 0.80
        Case 9 '0
            FlasherTorchRightGlow.IntensityScale = 0.60
            FlasherTorchRightGlowEdge.IntensityScale = 0.60
        Case 10 '1
            TorchRight2.State = 1
            FlasherTorchRightGlow.IntensityScale = 1
            FlasherTorchRightGlowEdge.IntensityScale = 1
        Case 11 '1
        Case 12 '1
        Case 13 '1
            TorchRight2.State = 1
            FlasherTorchRightGlow.IntensityScale = 1
            FlasherTorchRightGlowEdge.IntensityScale = 1
        Case 14 '0
            TorchRight2.State = 0
            FlasherTorchRightGlow.IntensityScale = 0.80
            FlasherTorchRightGlowEdge.IntensityScale = 0.80
        Case 15 '1
            TorchRight2.State = 1
            FlasherTorchRightGlow.IntensityScale = 1
            FlasherTorchRightGlowEdge.IntensityScale = 1
        Case 16 '0
            TorchRight2.State = 0
            FlasherTorchRightGlow.IntensityScale = 0.80
            FlasherTorchRightGlowEdge.IntensityScale = 0.80
    End Select
    RightTorchStep = (RightTorchStep + 1) MOD 16
End Sub

'*************************************************************
'	     FLIPPER SHADOWS, COVERS & DROP TARGET SHADOWS
'*************************************************************

Sub DropTargetShadowsTimer_Timer

if sw17.isdropped then 
		sw17shadowlight.state=GI_53.state
     else
		sw17shadowlight.state=0
    end if

if sw18.isdropped then 
		sw18shadowlight.state=GI_53.state
     else
		sw18shadowlight.state=0
    end if

if sw19.isdropped then 
		sw19shadowlight.state=GI_53.state
     else
		sw19shadowlight.state=0
    end if

if sw21.isdropped then 
		sw21shadowlight.state=GI_52.state
     else
		sw21shadowlight.state=0
    end if

if sw22.isdropped then 
		sw22shadowlight.state=GI_52.state
     else
		sw22shadowlight.state=0
    end if

if sw23.isdropped then 
		sw23shadowlight.state=GI_52.state
     else
		sw23shadowlight.state=0
    end if

if sw25.isdropped then 
		sw25shadowlight.state=GI_46.state
     else
		sw25shadowlight.state=0
    end if

if sw26.isdropped then 
		sw26shadowlight.state=GI_46.state
     else
		sw26shadowlight.state=0
    end if

if sw27.isdropped then 
		sw27shadowlight.state=GI_46.state
     else
		sw27shadowlight.state=0
    end if

if sw29.isdropped then 
		sw29shadowlight.state=GI_36.state
     else
		sw29shadowlight.state=0
    end if

if sw30.isdropped then 
		sw30shadowlight.state=GI_36.state
     else
		sw30shadowlight.state=0
    end if

if sw31.isdropped then 
		sw31shadowlight.state=GI_36.state
     else
		sw31shadowlight.state=0
    end if

end Sub


sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
    FlipperLtopSh.RotZ = LeftFlipper1.currentangle
    FlipperRtopSh.RotZ = RightFlipper1.currentangle
 
end sub

Sub FlipperCoversTimer_Timer
	LFlip.RotY = LeftFlipper.CurrentAngle
	RFlip.RotY = RightFlipper.CurrentAngle
	Lfliptop.RotY = LeftFlipper1.CurrentAngle
    Rfliptop.RotY = RightFlipper1.CurrentAngle
	
end sub

'*************************************************************
'           BALL SHADOW by ninnuzu,  MOD by BorgDog
'*************************************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)
 
Sub BallShadowUpdate_timer()
    Dim BOT, b
	Dim maxXoffset
	maxXoffset=15		'max the shadow can be laterally from the ball center
    BOT = GetBalls

	' render the shadow for each ball
    For b = 0 to UBound(BOT)
	If BOT(b).Z > 20 then 			'use if want shadow on ramps and upper playfield as well as main playfield
'	If BOT(b).Z > 20 and BOT(b) < 50 Then	'use if only want shadows on main playfield
		BallShadow(b).visible = 1
		BallShadow(b).X = BOT(b).X-maxXoffset*(1-(Bot(b).X)/(PharaohDeadRise.Width/2))
		BallShadow(b).Y = BOT(b).Y + 10
		BallShadow(b).height = BOT(b).Z - 24
	   Else
		BallShadow(b).visible = 0
	End If
    Next
End Sub

'**********************************************************************************************************

'Solenoid Controlled Toys and Toy Animations
'**********************************************************************************************************

Sub UGI(Enabled)
	If Enabled Then
	'*****GI Lights Off
		dim xx
		For each xx in GIU:xx.State = 0: Next
	Else
		For each xx in GIU:xx.State = 1: Next
	End if
 End Sub

Sub PGI(Enabled)
	If Enabled Then
	'*****GI Lights Off
		dim xxx
		For each xxx in GIP:xxx.State = 0: Next
	Else
		For each xxx in GIP:xxx.State = 1: Next
	End if
End Sub

Sub SWMummyHand_Hit:
  GI_53.Duration 0, 2500, 1
  GI_54.Duration 0, 2500, 1
  GI_55.Duration 0, 2500, 1
  GI_56.Duration 0, 2500, 1
  GI_63.Duration 0, 2500, 1
  GI_65.Duration 0, 2500, 1
  GI_66.Duration 1, 2500, 0
  MummyHand.transx=0
  me.uservalue=10
  me.timerenabled= 1
end sub

SWMummyHand.timerinterval = 10

Sub SWMummyHand_timer

	If me.uservalue = 80 then
		Playsound "Servo1"
	elseif me.uservalue > 80 and me.uservalue <= 120 then
		MummyHand.transx  = MummyHand.transx - 2.25
	elseif me.uservalue > 190 and me.uservalue < 230 then
		MummyHand.transx  = MummyHand.transx + 2.25
	elseif me.uservalue = 230 then
		MummyHand.transx=0: me.timerenabled=0
	end if
	
	MummyHandShadow.transx = MummyHand.transx
	me.uservalue=me.uservalue+1
end sub



'Initiate Table
'**********************************************************************************************************

Dim bsTrough,bsSaucer,dtLL,dtLR,dtUL,dtUR,LMAG,RMAG,SubSpeed,bsLock,bsHiddenTomb,bsSlavesTomb, cpleft
Dim CBall
 
Sub PharaohDeadRise_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Pharaoh - Dead Rise (Williams 2019)"&chr(13)
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
        .hidden = 1
		If Err Then MsgBox Err.Description
	End With
	On Error Goto 0
		Controller.SolMask(0)=0
      vpmTimer.AddTimer 2000,"Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the timer to renable all the solenoids after 2 seconds
		Controller.Run
	If Err Then MsgBox Err.Description
	On Error Goto 0


	PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled=1

    vpmNudge.TiltSwitch=42
	vpmNudge.Sensitivity=5
	vpmNudge.TiltObj=Array(LeftSlingshot,RightSlingshot)
 
 	Set bsTrough=New cvpmBallStack
		bsTrough.InitSw 36,38,39,0,0,0,0,0
		bsTrough.InitKick BallRelease,90,6
		bsTrough.InitExitSnd "BallRelease", "Solenoid"
		bsTrough.Balls=2

	Set bsSaucer=New cvpmBallStack
		bsSaucer.InitSaucer Kicker35,35,206,6
		bsSaucer.InitExitSnd"Popper", "Solenoid"
 
 	Set bsLock=New cvpmBallStack
		bsLock.InitSaucer Kicker34,34,95,6
		bsLock.InitExitSnd"Popper", "Solenoid"

 	Set bsHiddenTomb=New cvpmBallStack
		bsHiddenTomb.InitSw 0,43,0,0,0,0,0,0
 		bsHiddenTomb.InitKick Kicker2,180,6
		bsHiddenTomb.InitExitSnd"Popper", "Solenoid"

	Set bsSlavesTomb=New cvpmBallStack
		bsSlavesTomb.InitSaucer Kicker33,33,0,12
 		bsSlavesTomb.InitExitSnd"Popper", "Solenoid"

 	Set dtLL=New cvpmDropTarget
		dtLL.InitDrop Array(sw25,sw26,sw27),Array(25,26,27)
		dtLL.InitSnd "DTDrop","DTReset"
 
	Set dtLR=New cvpmDropTarget
		dtLR.InitDrop Array(sw29,sw30,sw31),Array(29,30,31)
		dtLR.InitSnd "DTDrop","DTReset"
 
	Set dtUL=New cvpmDropTarget
		dtUL.InitDrop Array(sw17,sw18,sw19),Array(17,18,19)
		dtUL.InitSnd "DTDrop","DTReset"

	Set dtUR=New cvpmDropTarget
		dtUR.InitDrop Array(sw21,sw22,sw23),Array(21,22,23)
		dtUR.InitSnd "DTDrop","DTReset"

	Set LMAG=New cvpmMagnet
		LMAG.InitMagnet MagnetL,7
		LMAG.Solenoid=21
		LMAG.CreateEvents"LMAG"
		
	Set RMAG=New cvpmMagnet
		RMAG.InitMagnet MagnetR,7
		RMAG.Solenoid=22
		RMAG.CreateEvents"RMAG"

    Set CBall=Captive.CreateBall
 	Captive.Kick 180,1

End Sub

Sub startgametimer_timer
    startgamesound=1
	PlaySound "ambient", -1, .1			'adjust the volume of ambient music using the second parameter.	
    me.enabled=0
End Sub

Sub startgamename_timer			'****make random sound for starting game
	Dim v
	if GI_64.state=1 then 	
		v = INT(2 * RND(1) )
		Select Case v
			Case 0:PlaySound"Pharaoh Dead Rise"
			Case 1:PlaySound"Pharaoh Dead Rise"
        End Select
	end if
	me.enabled=0
End Sub

Sub PharaohGIUoff_Hit
   if Activeball.vely < 0 then 
    Dim bulb
    For each bulb in GIU
        bulb.duration 0, 2000, 1
    Next
End if
End Sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub PharaohDeadRise_KeyDown(ByVal KeyCode)
    If KeyDownHandler(keycode) Then Exit Sub
    If keycode = PlungerKey Then Plunger.Pullback:playsound"plungerpull"
    If keycode = LeftMagnaSave Then:Controller.Switch(9) = 1:End If
    If keycode = RightMagnaSave Then:Controller.Switch(10) = 1:End If
End Sub

Sub PharaohDeadRise_KeyUp(ByVal KeyCode)
    If KeyUpHandler(keycode) Then Exit Sub
    If keycode = PlungerKey Then Plunger.Fire:PlaySound"plunger"
    If keycode = LeftMagnaSave Then:Controller.Switch(9) = 0:End If
    If keycode = RightMagnaSave Then:Controller.Switch(10) = 0:End If
End Sub

'**********************************************************************************************************
'Switches Triggers
'**********************************************************************************************************

'Kickers
Sub Drain_Hit:bsTrough.AddBall Me:
    playsound"drain"
End Sub
Sub Kicker34_Hit:bsLock.AddBall Me:End Sub
Sub Kicker35_Hit:bsSaucer.AddBall 0:End Sub
Sub Kicker33_Hit:bsSlavesTomb.AddBall 0:End Sub
Sub Kicker43_Hit:bsHiddenTomb.AddBall Me:End Sub

Sub Enter_Hit
 	SubSpeed=ABS(ActiveBall.VelY)
 	Enter.DestroyBall
 	Enter2.CreateBall
 	Enter2.Kick 180,SQR(SubSpeed)
End Sub

'Wire Triggers
Sub SW13_Hit:Controller.Switch(13)=1 : playsound"rollover" :End Sub 
Sub SW13_unHit:Controller.Switch(13)=0:End Sub
Sub SW14_Hit:Controller.Switch(14)=1 : playsound"rollover" :End Sub 
Sub SW14_unHit:Controller.Switch(14)=0:End Sub
Sub SW15_Hit:Controller.Switch(15)=1 : playsound"rollover" :
  GI_1.Duration 0, 1250, 1
  GI_2.Duration 0, 1250, 1
  GI_3.Duration 0, 1250, 1
  GI_4.Duration 0, 1250, 1
  GI_22.Duration 0, 1250, 1
  GI_24.Duration 0, 1250, 1
  GI_59.Duration 0, 1250, 1
  GI_60.Duration 0, 1250, 1
  GI_61.Duration 0, 1250, 1
  GI_62.Duration 0, 1250, 1
  GI_69.Duration 0, 1250, 1
End Sub 
Sub SW15_unHit:Controller.Switch(15)=0:End Sub
Sub SW16_Hit:Controller.Switch(16)=1 : playsound"rollover" :
  GI_1.Duration 0, 1250, 1
  GI_2.Duration 0, 1250, 1
  GI_3.Duration 0, 1250, 1
  GI_4.Duration 0, 1250, 1
  GI_22.Duration 0, 1250, 1
  GI_24.Duration 0, 1250, 1
  GI_59.Duration 0, 1250, 1
  GI_60.Duration 0, 1250, 1
  GI_61.Duration 0, 1250, 1
  GI_62.Duration 0, 1250, 1
  GI_69.Duration 0, 1250, 1
End Sub 
Sub SW16_unHit:Controller.Switch(16)=0:End Sub

SW41.timerinterval = 10

Sub SW41_Hit:Controller.Switch(41)=1 : playsound"rollover" :
    Playsound "lid"   
    GI_16.Duration 0, 1900, 1
    GI_17.Duration 0, 1900, 1
    GI_26.Duration 0, 1900, 1
    GI_35.Duration 0, 1900, 1
    GI_40.Duration 0, 1900, 1
    GI_41.Duration 0, 1900, 1
    GI_42.Duration 0, 1900, 1
    TreasureLid.rotx=0
    me.uservalue=1
    me.timerenabled= 1
    End Sub

Sub sw41_timer
	If me.uservalue <= 50 then
		TreasureLid.rotx = TreasureLid.rotx + 1.8
	elseif me.uservalue > 130 and me.uservalue < 180 then
		TreasureLid.rotx = TreasureLid.rotx - 1.8
	elseif me.uservalue = 180 then
		TreasureLid.rotx=0: me.timerenabled=0
	end if
	me.uservalue=me.uservalue+1
End Sub    

Sub SW41_unHit:Controller.Switch(41)=0:End Sub

'lane trigger
Sub SW40_Hit:Controller.Switch(40)=1:
  Playsound "Soloff"
End Sub
Sub SW40_unHit:Controller.Switch(40)=0:End Sub

SW42.timerinterval = 10

Sub SW42_Hit:
    TreasureLidShadow.transx=0
    me.uservalue=1
    me.timerenabled= 1
    End Sub

Sub sw42_timer
	If me.uservalue <= 50 then
		TreasureLidShadow.transx = TreasureLidShadow.transx + 1.4
	elseif me.uservalue > 130 and me.uservalue < 180 then
		TreasureLidShadow.transx = TreasureLidShadow.transx - 1.4
	elseif me.uservalue = 180 then
		TreasureLidShadow.transx=0: me.timerenabled=0
	end if
	me.uservalue=me.uservalue+1
End Sub    

Sub sw43_Hit
  Dim w
	 w = INT(2 * RND(1) )
	 Select Case w
	 Case 1:PlaySound"Treasure"
	 Case 2:PlaySound"Pharaohs Treasure"
     End Select
end sub

'Stand Up targets

SW28.timerinterval = 10

Sub sw28_Hit:vpmTimer.PulseSw 28 :
  PlaySound "Cobrahit"    
  LeftSnakeEye.Duration 3, 500, 0
  RightSnakeEye.Duration 3, 500, 0
  MiddleSnakeLeftEye.Duration 3, 500, 0
  MiddleSnakeRightEye.Duration 3, 500, 0
  CobraSpotlight.Duration 1, 1000, 0
  CobraPFLight.Duration 1, 1000, 0
  CobraPlasticLight.Duration 1, 1000, 0
  Cobra.rotz=0
  me.uservalue=1
  me.timerenabled= 1
end sub

Sub sw28_timer
	If me.uservalue <= 40 then
		Cobra.rotz = Cobra.rotz + 1.5
	elseif me.uservalue > 40 and me.uservalue < 80 then
		Cobra.rotz = Cobra.rotz - 1.5
	elseif me.uservalue = 80 then
		Cobra.rotz=0: me.timerenabled=0
	end if
	me.uservalue=me.uservalue+1

	CobraShadow.transy = Cobra.rotz
	CobraHighlight.rotz = Cobra.rotz
	CobraEyes.rotz = Cobra.rotz
	CobraBottomHighlight.rotz = Cobra.rotz
end sub

Sub sw32_Hit:vpmTimer.PulseSw 32 :
    GI_10.Duration 2, 300, 1    
    GI_11.Duration 2, 300, 1
    GI_23.Duration 2, 300, 1    
    GI_25.Duration 2, 300, 1
    GI_29.Duration 2, 300, 1 
    GI_30.Duration 2, 300, 1 
End Sub

'Drop Targets
 Sub Sw17_Hit:dtUL.Hit 1 :End Sub
 Sub Sw18_Hit:dtUL.Hit 2 :End Sub 
 Sub Sw19_Hit:dtUL.Hit 3 :End Sub

 Sub Sw21_Hit:dtUR.Hit 1 :End Sub 
 Sub Sw22_Hit:dtUR.Hit 2 :End Sub 
 Sub Sw23_Hit:dtUR.Hit 3 :End Sub 

 Sub Sw25_Hit:dtLL.Hit 1 :End Sub  
 Sub Sw26_Hit:dtLL.Hit 2 :End Sub  
 Sub Sw27_Hit:dtLL.Hit 3 :End Sub  

 Sub Sw29_Hit:dtLR.Hit 1 :End Sub 
 Sub Sw30_Hit:dtLR.Hit 2 :End Sub 
 Sub Sw31_Hit:dtLR.Hit 3 :End Sub  
 Sub SW34_Hit
    PlaySound "popper_ball"
    GI_14.Duration 0, 300, 1
    GI_16.Duration 0, 300, 1    
    GI_17.Duration 0, 300, 1
    GI_35.Duration 0, 300, 1 
    GI_57.Duration 0, 300, 1
end sub
Sub SWEyesOn_Hit
    PharaohEyeLeftPF.Duration 1, 1, 1
    PharaohEyeRightPF.Duration 1, 1, 1
    GI_69.Duration 1, 1, 1
end sub

Sub SWDrainLight_Hit
    GI_69.Duration 0, 300, 1
end sub

Sub SWEyesOff_Hit
    PharaohEyeLeftPF.Duration 0, 0, 0
    PharaohEyeRightPF.Duration 0, 0, 0
end sub

Sub swPOST_Hit
    MiddleSnakeLeftEye.Duration 3, 500, 0
    MiddleSnakeRightEye.Duration 3, 500, 0
end sub

Sub swLaugh_Hit
  SarcophagusLeftEye.Duration 2, 700, 0  
  SarcophagusRightEye.Duration 2, 700, 0
  if Activeball.vely < 0 then
  Dim w
	 w = INT(2 * RND(1) )
	 Select Case w
	 Case 1:PlaySound"Laugh"
	 Case 2:PlaySound""
	 End Select
end if
end sub

Sub swBeware_Hit
  if Activeball.vely < 0 then
  GI_67.Duration 2,500,0
  Dim w
	 w = INT(2 * RND(1) )
	 Select Case w
	 Case 1:PlaySound"Beware"
	 Case 2:PlaySound""
	 End Select
end if
end sub

Sub swLeftInlane_Hit
  if Activeball.vely > 0 then
  Playsound "dagger"
  GI_59.duration 2,500,1
  GI_62.duration 2,500,1
end if
End Sub

Sub swRightInlane_Hit
  if Activeball.vely > 0 then
  Playsound "dagger"
  GI_60.duration 2,500,1
  GI_61.duration 2,500,1
end if
End Sub

'**********************************************************************************************************
'Map lights to an array
'**********************************************************************************************************
Set Lights(8)=L8
Set Lights(9)=L9
Set Lights(10)=L10
Set Lights(11)=L11
Set Lights(12)=L12
Set Lights(13)=L13
Set Lights(14)=L14
Set Lights(15)=L15
Set Lights(16)=L16
Set Lights(17)=L17
Set Lights(18)=L18
Set Lights(19)=L19
Set Lights(20)=L20
Set Lights(21)=L21
Set Lights(22)=L22
Set Lights(23)=L23
Set Lights(24)=L24
Set Lights(25)=L25
Set Lights(26)=L26
Set Lights(27)=L27
Set Lights(28)=L28
Set Lights(29)=L29
Set Lights(30)=L30
Set Lights(31)=L31
Set Lights(32)=L32
Set Lights(33)=L33
Set Lights(34)=L34
Set Lights(35)=L35
Set Lights(43)=L43
Set Lights(46)=L46
Set Lights(47)=L47
Set Lights(48)=L48
Set Lights(49)=L49
Set Lights(50)=L50
Set Lights(51)=L51
Set Lights(52)=L52
Set Lights(53)=L53
Set Lights(54)=L54
Set Lights(55)=L55
Set Lights(56)=L56
Set Lights(57)=L57
Set Lights(58)=L58
Set Lights(59)=L59
Set Lights(60)=L60
Set Lights(61)=L61
Set Lights(62)=L62
Set Lights(63)=L63
Set Lights(64)=L64
Set Lights(100)=L100


'BackGlass


'**********************************************************************************************************
' Backglass Light Displays (7 digit 7 segment displays)
Dim Digits(32)

' 1st Player
Digits(0) = Array(LED10,LED11,LED12,LED13,LED14,LED15,LED16)
Digits(1) = Array(LED20,LED21,LED22,LED23,LED24,LED25,LED26)
Digits(2) = Array(LED30,LED31,LED32,LED33,LED34,LED35,LED36)
Digits(3) = Array(LED40,LED41,LED42,LED43,LED44,LED45,LED46)
Digits(4) = Array(LED50,LED51,LED52,LED53,LED54,LED55,LED56)
Digits(5) = Array(LED60,LED61,LED62,LED63,LED64,LED65,LED66)
Digits(6) = Array(LED70,LED71,LED72,LED73,LED74,LED75,LED76)

' 2nd Player
Digits(7) = Array(LED80,LED81,LED82,LED83,LED84,LED85,LED86)
Digits(8) = Array(LED90,LED91,LED92,LED93,LED94,LED95,LED96)
Digits(9) = Array(LED100,LED101,LED102,LED103,LED104,LED105,LED106)
Digits(10) = Array(LED110,LED111,LED112,LED113,LED114,LED115,LED116)
Digits(11) = Array(LED120,LED121,LED122,LED123,LED124,LED125,LED126)
Digits(12) = Array(LED130,LED131,LED132,LED133,LED134,LED135,LED136)
Digits(13) = Array(LED140,LED141,LED142,LED143,LED144,LED145,LED146)

' 3rd Player
Digits(14) = Array(LED150,LED151,LED152,LED153,LED154,LED155,LED156)
Digits(15) = Array(LED160,LED161,LED162,LED163,LED164,LED165,LED166)
Digits(16) = Array(LED170,LED171,LED172,LED173,LED174,LED175,LED176)
Digits(17) = Array(LED180,LED181,LED182,LED183,LED184,LED185,LED186)
Digits(18) = Array(LED190,LED191,LED192,LED193,LED194,LED195,LED196)
Digits(19) = Array(LED200,LED201,LED202,LED203,LED204,LED205,LED206)
Digits(20) = Array(LED210,LED211,LED212,LED213,LED214,LED215,LED216)

' 4th Player
Digits(21) = Array(LED220,LED221,LED222,LED223,LED224,LED225,LED226)
Digits(22) = Array(LED230,LED231,LED232,LED233,LED234,LED235,LED236)
Digits(23) = Array(LED240,LED241,LED242,LED243,LED244,LED245,LED246)
Digits(24) = Array(LED250,LED251,LED252,LED253,LED254,LED255,LED256)
Digits(25) = Array(LED260,LED261,LED262,LED263,LED264,LED265,LED266)
Digits(26) = Array(LED270,LED271,LED272,LED273,LED274,LED275,LED276)
Digits(27) = Array(LED280,LED281,LED282,LED283,LED284,LED285,LED286)

' Credits
Digits(28) = Array(LED4,LED2,LED6,LED7,LED5,LED1,LED3)
Digits(29) = Array(LED18,LED9,LED27,LED28,LED19,LED8,LED17)
' Balls
Digits(30) = Array(LED39,LED37,LED48,LED49,LED47,LED29,LED38)
Digits(31) = Array(LED67,LED58,LED69,LED77,LED68,LED57,LED59)

Sub DisplayTimer_Timer
	Dim ChgLED,ii,num,chg,stat,obj
	ChgLed = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
If Not IsEmpty(ChgLED) Then
		If DesktopMode = True Then
		For ii = 0 To UBound(chgLED)
			num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
			if (num < 32) then
				For Each obj In Digits(num)
					If chg And 1 Then obj.State = stat And 1 
					chg = chg\2 : stat = stat\2
				Next
			else
				'if char(stat) > "" then msg(num) = char(stat)
			end if
		next
		end if
end if
End Sub

'**********************Sling Shot Animations**********************
' Rstep and Lstep  are the variables that increment the animation
'*****************************************************************
Dim RStep, Lstep, R2Step

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 44
    PlaySound "left_slingshot", 0, 1, 0.05, 0.05
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
    GI_3.Duration 2, 300, 1
    GI_4.Duration 2, 300, 1
    GI_22.Duration 2, 300, 1
    RightSnakeEye.Duration 3, 500, 0
    
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0:
    End Select
    RStep = RStep + 1
End Sub

Sub sw20_Slingshot
	vpmTimer.PulseSw 20
    PlaySound "left_slingshot", 0, 1, 0.05, 0.05
    R2Sling.Visible = 0
    R2Sling1.Visible = 1
    sling3.TransZ = -20
    R2Step = 0
    sw20.TimerEnabled = 1
    GI_18.Duration 2, 300, 1
    GI_19.Duration 2, 300, 1
    GI_37.Duration 2, 300, 1
    GI_38.Duration 2, 300, 1
End Sub

Sub sw20_Timer
    Select Case R2Step
        Case 3:R2SLing1.Visible = 0:R2SLing2.Visible = 1:sling3.TransZ = -10
        Case 4:R2SLing2.Visible = 0:R2SLing.Visible = 1:sling3.TransZ = 0:sw20.TimerEnabled = 0:
    End Select
    R2Step = R2Step + 1
End Sub


Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 24
    PlaySound "right_slingshot",0,1,-0.05,0.05
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
    GI_1.Duration 2, 300, 1
    GI_2.Duration 2, 300, 1
    GI_24.Duration 2, 300, 1
    LeftSnakeEye.Duration 3, 500, 0
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0:
    End Select
    LStep = LStep + 1
End Sub


' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "PharaohDeadRise" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / PharaohDeadRise.width-1
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

Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
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