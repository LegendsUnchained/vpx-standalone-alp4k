' ****************************************************************
'      JP's Space Cadet for VISUAL PINBALL X 10.8, version 5.6.0
'                 GALAXY EDITION by Funkatron101 
'       Based on the PC game by Cinematronics/Maxis from 1996
' ****************************************************************
'Mixed Reality supports chromakey on Virtual Desktop.
'  Change MR_mdoe in F12 Menu
'  Virtual Desktop settings
'  Chromakey color  RGB = 255 : 255 : 255
'  Similarity=2, Smoothness=2

Option Explicit
Randomize

Const MusicMode = 1    ' Set Music Soundtrack: 0 = Original music, 1 = Galaxy Edition Soundtrack
Const BallSize = 50     ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1      ' standard ball mass in JP's VPX Physics 3.0





'----- VR Room -----
Const VRRoomChoice = 2			  ' 1 - Minimal Room, 2 - Space Room, 3 - MR mode

'----- General Sound Options -----
Dim VolumeDial : VolumeDial = 0.8           	' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume : BallRollVolume = 0.5   	' Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.5 		' Level of ramp rolling volume. Value between 0 and 1
Dim SongVolume : SongVolume = 0.1 				' 1 is full volume, but I set it quite low to listen better the other sounds since I use headphones, adjust to your setup :)


    ' Sound volumes
	SongVolume = Table1.Option("Soundtrack Volume", 0, 1, 0.01, 0.2, 1)
    VolumeDial = Table1.Option("Mech Volume", 0, 1, 0.01, 0.8, 1)
    BallRollVolume = Table1.Option("Ball Roll Volume", 0, 1, 0.01, 0.5, 1)
	RampRollVolume = Table1.Option("Ramp Roll Volume", 0, 1, 0.01, 0.5, 1)




'**************************
'   FlexDMD (Discouraged unless you have a 4:1 DMD and REALLY want something on there)
'**************************

' Use FlexDMD if in FS mode
Dim UseFlexDMD
If Table1.ShowDT = True then
    UseFlexDMD = False
Else
    UseFlexDMD = False ' set to True if you want both Flex and PupDMD on
End If

'FlexDMD in high or normal quality
'True if you have an LCD screen, 256x64
'or False if you have a real DMD at 128x32 in size
Const FlexDMDHighQuality = True



'**************************
'   PinUp Player USER Config
'**************************

dim useRealDMDScale : useRealDMDScale=0    ' 0 or 1 for RealDMD scaling.  Choose which one you prefer.
dim useDMDVideos    : useDMDVideos=true   ' true or false to use DMD splash videos.
dim pGameName       : pGameName="jpspacecadetGE"  'pupvideos foldername, probably set to cGameName in realworld





Dim tablewidth
tablewidth = Table1.width
Dim tableheight
tableheight = Table1.height
Dim BIP							 'Balls in play
BIP = 0
Dim BIPL							'Ball in plunger lane
BIPL = False

' Load the core.vbs for supporting Subs and functions
LoadCoreFiles

Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox "Can't open controller.vbs"
    On Error Goto 0
End Sub

' Define any Constants
Const cGameName = "jpspacecadetGE"
Const myVersion = "5.6.0"
Const MaxPlayers = 4         ' from 1 to 4
Const BallSaverTime = 20     ' in seconds of the first ball
Const MaxMultiplier = 10     ' limit playfield multiplier
Const MaxBonusMultiplier = 5 'limit Bonus multiplier
Const BallsPerGame = 3       ' usually 3 or 5
Const MaxMultiballs = 3      ' max number of balls during multiballs


' Define Global Variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim BonusPoints(4)
Dim BonusHeldPoints(4)
Dim BonusMultiplier(4)
Dim PlayfieldMultiplier(4)
Dim PFxSeconds
Dim bBonusHeld
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Score(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim Jackpot(4)
Dim SuperJackpot(4)
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim mBalls2Eject
Dim SkillshotValue(4)
Dim SuperSkillshotValue(4)
Dim bAutoPlunger
Dim bInstantInfo
Dim bAttractMode
Dim i, j, k, x 'used in loops
Dim pStatusText
Dim pStatusDuration
Dim AllComplete

' Define Game Control Variables
Dim LastSwitchHit
Dim BallsOnPlayfield
Dim BallsInLock(4)
Dim BallsInHole

' Define Game Flags
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
Dim bJackpot

' core.vbs variables
Dim plungerIM 'used mostly as an autofire plunger during multiballs
Dim leftkickerIM
Dim rightkickerIM
Dim aMagnet
Dim bMagnet


' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
    Dim i
    Randomize

    'Impulse Plunger as autoplunger
    Const IMPowerSetting = 65 ' Plunger Power
    Const IMTime = 0.25       ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFXDOF("fx_kicker", 141, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 141, DOFPulse, DOFContactors)
        .CreateEvents "plungerIM"
    End With

    'Impulse Plunger as right kickback
    Set rightkickerIM = New cvpmImpulseP
    With rightkickerIM
        .InitImpulseP RightKicker, IMPowerSetting, IMTime
        '.Random .5
        .InitExitSnd SoundFXDOF("fx_kicker", 141, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 141, DOFPulse, DOFContactors)
        .CreateEvents "rightkickerIM"
    End With

    'Impulse Plunger as left kickback
    Set leftkickerIM = New cvpmImpulseP
    With leftkickerIM
        .InitImpulseP LeftKicker, IMPowerSetting, IMTime
        '.Random .5
        .InitExitSnd SoundFXDOF("fx_kicker", 141, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 141, DOFPulse, DOFContactors)
        .CreateEvents "leftkickerIM"
    End With

    ' Magnet
    Set aMagnet = New cvpmMagnet
    With aMagnet
        .InitMagnet GravityMagnet, 6
        .GrabCenter = True
        .CreateEvents "aMagnet"
    End With

    ' Victory Magnet
    Set bMagnet = New cvpmMagnet
    With bMagnet
        .InitMagnet VictoryMagnet, 6
        .GrabCenter = True
        .CreateEvents "bMagnet"
    End With

    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    ' load saved values, highscore, names, jackpot
    Credits = 0
    Loadhs

    ' Initalise the DMD display
    DMD_Init

	PUPInit  'this should be called in table1_init at bottom after all else b2s/controller running.

    pupDMDupdate.Enabled = false
    pupDMDupdate.Interval = 500
    pupDMDupdate.Enabled = true

    ' freeplay or coins
    bFreePlay = True 'we want coins

    if bFreePlay Then DOF 125, DOFOn

    ' Init main variables and any other flags
    bAttractMode = False
    bOnTheFirstBall = False
    bBallInPlungerLane = False
    bBallSaverActive = False
    bBallSaverReady = False
    bMultiBallMode = False
    PFxSeconds = 0
    bGameInPlay = False
    bAutoPlunger = False
    bMusicOn = True
    BallsOnPlayfield = 0
    BallsInHole = 0
    LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bBonusHeld = False
    bJustStarted = True
    bJackpot = False
    bInstantInfo = False
    ' set any lights for the attract mode
    GiOff
    StartAttractMode

    ' Start the RealTime timer
    RealTime.Enabled = 1

    ' Load table color
    LoadLut
End Sub



'******
'Nfozzy Start
'************
'******************************************************
'			FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt	'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay	'delay before trigger turns off and polarity is disabled TODO set time!
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

	Public Sub Report(aChooseArray) 	'debug, reports all coords in tbPL.text
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

	Public Sub AddBall(aBall) : dim x : for x = 0 to uBound(balls) : if IsEmpty(balls(x)) then set balls(x) = aBall : exit sub :end if : Next  : End Sub

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
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function	'Timer shutoff for polaritycorrect

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
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)				'find safety coefficient 'ycoef' data
				end if
			Next

			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)						'find safety coefficient 'ycoef' data
			End If

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				: 			VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1	'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR

				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
				'playsound "knocker"
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'		FLIPPER POLARITY AND RUBBER DAMPENER
'			SUPPORTING FUNCTIONS 
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)	'Shuffle objects in a temp array
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
	redim aArray(aCount-1+offset)	'Resize original array
	for x = 0 to aCount-1		'set objects back into original array
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
Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

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

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)	'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper

	LinearEnvelope = Y
End Function

' Used for drop targets and flipper tricks
Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function


'******************************************************
'		FLIPPER CORRECTION INITIALIZATION
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1	'disabled
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
addpt "Velocity", 0, 0, 1
addpt "Velocity", 1, 0.16, 1.06
addpt "Velocity", 2, 0.41, 1.05
addpt "Velocity", 3, 0.53, 1'0.982
addpt "Velocity", 4, 0.702, 0.968
addpt "Velocity", 5, 0.95, 0.968
addpt "Velocity", 6, 1.03, 0.945

	LF.Object = LeftFlipper	
	LF.EndPoint = EndPointLp
	RF.Object = RightFlipper
	RF.EndPoint = EndPointRp
End Sub

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub


dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim RFEndAngle, LFEndAngle

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle


'******************************************************
'			FLIPPER TRICKS
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True
leftflipper.timerinterval=1
leftflipper.timerenabled=True
sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
end sub

sub LeftFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
end sub


'dim LFPress, RFPress, LFCount, RFCount
'dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
'dim RFEndAngle, LFEndAngle

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
	Dir = Flipper.startangle/Abs(Flipper.startangle)	'-1 for Right Flipper

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

Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub

'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7

sub TargetBouncer(aBall,defvalue)
    dim zMultiplier, vel, vratio
    'debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
	vel = BallSpeed(aBall)
	if aBall.velx = 0 then vratio = 1 else vratio = aBall.vely/aBall.velx
	Select Case Int(Rnd * 6) + 1
		Case 1: zMultiplier = 0.2*defvalue
		Case 2: zMultiplier = 0.25*defvalue
		Case 3: zMultiplier = 0.3*defvalue
		Case 4: zMultiplier = 0.4*defvalue
		Case 5: zMultiplier = 0.45*defvalue
		Case 6: zMultiplier = 0.5*defvalue
	End Select
	aBall.velz = abs(vel * zMultiplier * TargetBouncerFactor)
	aBall.velx = sgn(aBall.velx) * sqr(abs((vel^2 - aBall.velz^2)/(1+vratio^2)))
	aBall.vely = aBall.velx * vratio
	'debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
	'debug.print "conservation check: " & BallSpeed(aBall)/vel
end sub

Sub TargetBounce_Hit
	TargetBouncer activeball, 1
End Sub

'******************************************************
' 	ZDMP:  RUBBER  DAMPENERS
'******************************************************
' These are data mined bounce curves,
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR

Sub dPosts_Hit(idx)
	RubbersD.dampen ActiveBall
	TargetBouncer ActiveBall, 1
End Sub

Sub dSleeves_Hit(idx)
	SleevesD.Dampen ActiveBall
	TargetBouncer ActiveBall, 0.7
End Sub

Dim RubbersD				'frubber
Set RubbersD = New Dampener
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	  'debug, reports In debugger (In vel, out cor); cor bounce curve (linear)

'for best results, try to match in-game velocity as closely as possible to the desired curve
'   RubbersD.addpoint 0, 0, 0.935   'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1		 'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	   'there's clamping so interpolate up to 56 at least

Dim SleevesD	'this is just rubber but cut down to 85%...
Set SleevesD = New Dampener
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	  'debug, reports In debugger (In vel, out cor)
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
		'if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & "actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		'if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)

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
'		TRACK ALL BALL VELOCITIES
' 		FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor: set cor = new CoRTracker

Class CoRTracker
	public ballvel, ballvelx, ballvely

	Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

	Public Sub Update()	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs

		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds
		if uBound(ballvelx) < highestID then redim ballvelx(highestID)	'set bounds
		if uBound(ballvely) < highestID then redim ballvely(highestID)	'set bounds

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

'************
'Nfozzy End
'************

'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************

' This part in the script is an entire block that is dedicated to the physics sound system.


'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1														'volume level; range [0, 1]
NudgeLeftSoundLevel = 1													'volume level; range [0, 1]
NudgeRightSoundLevel = 1												'volume level; range [0, 1]
NudgeCenterSoundLevel = 1												'volume level; range [0, 1]
StartButtonSoundLevel = 0.1												'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr											'volume level; range [0, 1]
PlungerPullSoundLevel = 1												'volume level; range [0, 1]
RollingSoundFactor = 1.1/5		

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010           						'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635								'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                        						'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                      						'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel								'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel								'sound helper; not configurable
SlingshotSoundLevel = 0.95												'volume level; range [0, 1]
BumperSoundFactor = 4.25												'volume multiplier; must not be zero
KnockerSoundLevel = 1 													'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2									'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055/5											'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5											'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5										'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025									'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025									'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8									'volume level; range [0, 1]
WallImpactSoundFactor = 0.075											'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5													'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10											'volume multiplier; must not be zero
DTSoundLevel = 0.25														'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                              					'volume level; range [0, 1]
SpinnerSoundLevel = 0.5                              					'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8														'volume level; range [0, 1]
BallReleaseSoundLevel = 1												'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2									'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015										'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5													'volume multiplier; must not be zero


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
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-L01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-R01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
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

Sub Apron_Hit (idx)
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

Const RelayFlashSoundLevel = 0.315									'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05									'volume level; range [0, 1];

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
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////


'******
' Keys
'******

Const ReflipAngle = 20

Sub Table1_KeyDown(ByVal Keycode)

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If
	If keycode = PlungerKey Then Plunger.Pullback : SoundPlungerPull()
	If keycode = LeftTiltKey Then Nudge 90, 1 : SoundNudgeLeft()	
	If keycode = RightTiltKey Then Nudge 270, 1 : SoundNudgeRight()	
	If keycode = CenterTiltKey Then Nudge 0, 1 : SoundNudgeCenter()

    If keycode = LeftMagnaSave Then bLutActive = True:SetLUTLine "Color LUT image " & table1.ColorGradeImage
    If keycode = RightMagnaSave AND bLutActive Then NextLUT:End If
    If Keycode = AddCreditKey OR Keycode = AddCreditKey2 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
        Credits = Credits + 1
        if bFreePlay = False Then DOF 125, DOFOn
        If(Tilted = False) Then
            DMDFlush
            DMD CL(1, "CREDITS " & Credits), "_", "", eNone, eNone, eNone, 500, True, "fx_coin"
            If NOT bGameInPlay Then ShowTableInfo
        End If
    End If

    ' Normal flipper action

    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftTiltKey Then CheckTilt 'only check the tilt during game
        If keycode = RightTiltKey Then CheckTilt
        If keycode = CenterTiltKey Then CheckTilt
        If keycode = MechanicalTilt Then CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1:InstantInfoTimer.Enabled = True:RotateLaneLights 1
        If keycode = RightFlipperKey Then SolRFlipper 1:InstantInfoTimer.Enabled = True:RotateLaneLights 0

        If keycode = StartGameKey Then
            If((PlayersPlayingGame < MaxPlayers) AND(bOnTheFirstBall = True) ) Then

                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eNone, eNone, 1000, True, ""
                Else
                    If(Credits> 0) then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eNone, eNone, 1000, True, ""
                        If Credits < 1 And bFreePlay = False Then DOF 125, DOFOff
                        Else
                            ' Not Enough Credits to start a game.
                            DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 1000, True, "vo_nocredits"
                    End If
                End If
            End If
        End If

        ' Activate VictoryReleaseBall if bMagnet is on when Start Game key is pressed
        If keycode = StartGameKey Then
            If bMagnet.MagnetON Then
                VictoryReleaseBall
            End If
        End If

        Else ' If (GameInPlay)

            If keycode = StartGameKey Then
                If(bFreePlay = True) Then
                    If(BallsOnPlayfield = 0) Then
                        ResetForNewGame()
                    End If
                Else
                    If(Credits> 0) Then
                        If(BallsOnPlayfield = 0) Then
                            Credits = Credits - 1
                            If Credits < 1 And bFreePlay = False Then DOF 125, DOFOff
                            ResetForNewGame()
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                        DMDFlush
                        DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 1000, True, "vo_nocredits"
                        ShowTableInfo
                    End If
                End If
            End If
    End If ' If (GameInPlay)
End Sub

Sub Table1_KeyUp(ByVal keycode)

If keycode = PlungerKey Then 
		Plunger.Fire
		If BIPL = 1 Then
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If

    If hsbModeActive Then
        Exit Sub
    End If
End If
    If keycode = LeftMagnaSave Then bLutActive = False:HideLUT

    If keycode = PlungerKey Then
        Plunger.Fire
        PlaySoundAt "fx_plunger", plunger
    End If

    ' Table specific

    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then
            SolLFlipper 0
            InstantInfoTimer.Enabled = False
            If bInstantInfo Then
                DMDScoreNow
                bInstantInfo = False
            End If
        End If
        If keycode = RightFlipperKey Then
            SolRFlipper 0
            InstantInfoTimer.Enabled = False
            If bInstantInfo Then
                DMDScoreNow
                bInstantInfo = False
            End If
        End If
    End If
End Sub

Sub InstantInfoTimer_Timer
    InstantInfoTimer.Enabled = False
    If hsbModeActive = False AND bInstantInfo = False Then
        bInstantInfo = True
        DMDFlush
        InstantInfo
    End If
End Sub

'*************
' Pause Table
'*************

Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub Table1_Exit
    Savehs
    If UseFlexDMD Then FlexDMD.Run = False
    If B2SOn = true Then Controller.Stop
End Sub


'********************
'     Flippers
'********************

Sub SolLFlipper(Enabled)
    If Enabled Then
		LF.Fire
       DOF 101, DOFOn ' Outhere
If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
	      End If		
	Else
       DOF 101, DOFOff ' Outhere
		LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If

    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
		RF.Fire
       DOF 102, DOFOn ' Outhere
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
       DOF 102, DOFOff ' Outhere
		RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel

    End If
End Sub


'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                     'Called when table is nudged
    Tilt = Tilt + TiltSensitivity                 'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt> TiltSensitivity) AND(Tilt < 15) Then 'show a warning
	    pupDMDDisplay "Splash","CAREFUL^ ", "", 4, 1, 10
        DMD "_", CL(1, "CAREFUL"), "_", eNone, eBlinkFast, eNone, 1500, True, "vo_Careful"
    End if
    If NOT Tilted AND Tilt> 15 Then 'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
        DMDFlush
	    pupDMDDisplay "Splash","YOU TILTED^ ", "", 4, 1, 10
        DMD CL(0, "YOU"), CL(1, "TILTED"), "", eNone, eNone, eNone, 2000, True, "vo_You tilted"
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
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
        'turn off GI and turn off all the lights
        GiOff
        LightSeqTilt.Play SeqAllOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        Bumper1.Threshold = 100
        Bumper2.Threshold = 100
        Bumper3.Threshold = 100
        Bumper4.Threshold = 100
        Bumper5.Threshold = 100
        Bumper6.Threshold = 100
        Bumper7.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
        LeftSlingshot2.Disabled = 1
        Rightslingshot2.Disabled = 1
        li094.State = 1
    Else
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
        Bumper1.Threshold = 1
        Bumper2.Threshold = 1
        Bumper3.Threshold = 1
        Bumper4.Threshold = 1
        Bumper5.Threshold = 1
        Bumper6.Threshold = 1
        Bumper7.Threshold = 1
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
        Leftslingshot2.Disabled = 0
        Rightslingshot2.Disabled = 0
        li094.State = 0
        'clean up the buffer display
        DMDFlush
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield = 0) Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        vpmtimer.Addtimer 2000, "EndOfBall() '"
        TiltRecoveryTimer.Enabled = False
    End If
' else retry (checks again in another second or so)
End Sub

'****************************************
'      Music as wav/ogg/mp3 sounds
'****************************************

Dim Song
Song = ""

Sub PlaySong(name)
    If bMusicOn Then
        If Song <> name Then
            StopSound Song
            Song = name
            PlaySound Song, -1, SongVolume
        End If
    End If
End Sub

Sub ChangeSong 'not used in this table
    Dim a
    Select Case Mission(CurrentPlayer)
        Case 0 'no Mission active so play the standard songs or the multiball songs
    End Select
End Sub

'********************
' Play random soundfx
'********************

Sub PlaySfx
    Dim tmp
    tmp = RndNbr(17)
    PlaySound "sc_" &tmp
    LightEffect 3
End Sub

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function
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
    For each bulb in aGILights
        SetLightColor bulb, col, -1
    Next
End Sub

Sub GIUpdateTimer_Timer
    Dim tmp, obj
    tmp = Getballs
    If UBound(tmp) <> OldGiState Then
        OldGiState = Ubound(tmp)
        If UBound(tmp) = -1 Then '-1 means no balls, 0 is the first captive ball, 1 is the second captive ball...)
            GiOff                ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
        Else
            Gion
        End If
    End If
End Sub

Sub GiOn
    PlaySoundAt "fx_GiOn", li036 'about the center of the table
    DOF 118, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
End Sub

Sub GiOff
    PlaySoundAt "fx_GiOff", li036 'about the center of the table
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
            LightSeqGi.UpdateInterval = 20
            LightSeqGi.Play SeqBlinking, , 15, 10
        Case 2 'random
            LightSeqGi.UpdateInterval = 20
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
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqBlinking, , 15, 10
        Case 2 'random
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqInserts.UpdateInterval = 10
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

Sub FlashEffect(n)
    Select Case n
        Case 0 ' all off
            LightSeqFlashers.Play SeqAlloff
        Case 1 'all blink
            LightSeqFlashers.UpdateInterval = 40
            LightSeqFlashers.Play SeqBlinking, , 15, 30
        Case 2 'random
            LightSeqFlashers.UpdateInterval = 30
            LightSeqFlashers.Play SeqRandom, 25, , 1500
        Case 3 'all blink fast
            LightSeqFlashers.UpdateInterval = 20
            LightSeqFlashers.Play SeqBlinking, , 10, 30
        Case 4 'center
            LightSeqFlashers.UpdateInterval = 4
            LightSeqFlashers.Play SeqCircleOutOn, 15, 2
        Case 5 'top down
            LightSeqFlashers.UpdateInterval = 4
            LightSeqFlashers.Play SeqDownOn, 15, 2
        Case 6 'down to top
            LightSeqFlashers.UpdateInterval = 4
            LightSeqFlashers.Play SeqUpOn, 15, 1
    End Select
End Sub




'***********************************************
'   JP's VP10 Rolling Sounds + Ballshadow v4.0
'   uses a collection of shadows, aBallShadow
'***********************************************

Const tnob = 19   'total number of balls
Const lob = 0     'number of locked balls
'Const maxvel = 30 'max ball velocity

ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Sub InitRolling
	Dim i
	For i = 0 To tnob
		rolling(i) = False
	Next
End Sub

Sub RollingUpdate()
	Dim b
	  Dim BOT
	  BOT = GetBalls
	
	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 To tnob - 1
		rolling(b) = False
		StopSound("BallRoll_" & b)
        aBallShadow(b).Y = 3000
	Next
	
	' exit the sub if no balls on the table
	If UBound(BOT) =  - 1 Then Exit Sub
	
	' play the rolling sound for each ball
	For b = 0 To UBound(BOT)
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y
        aBallShadow(b).Height = BOT(b).Z -Ballsize/2 + .02

		If BallVel(BOT(b)) > 1 And BOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), - 1, VolPlayfieldRoll(BOT(b)) * BallRollVolume * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If
		
		' Ball Drop Sounds
		If BOT(b).VelZ <  - 1 And BOT(b).z < 55 And BOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If BOT(b).velz >  - 7 Then
					RandomSoundBallBouncePlayfieldSoft BOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard BOT(b)
				End If
			End If
		End If
		
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If
	Next
End Sub

'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************




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
Dim RampBalls(6,2)
'x,0 = ball x,1 = ID, 2 = Protection against ending early (minimum amount of updates)

'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
' Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
' Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
Dim RampType(6)

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
			Debug.print "WireRampOn error, ball queue Is full: " & vbNewLine & _
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

Sub RandomSoundRampStop(obj)
	Select Case Int(rnd*3)
		Case 0: PlaySoundAtVol "wireramp_stop1", obj, 0.2*VolumeDial:PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 1: PlaySoundAtVol "wireramp_stop2", obj, 0.2*VolumeDial:PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 2: PlaySoundAtVol "wireramp_stop3", obj, 0.2*VolumeDial:PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
	End Select
End Sub

'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************


'*******************************************
'  Ramp Triggers
'*******************************************
Sub ramptrigger01_hit()
	WireRampOn True	 'Play Plastic Ramp Sound
End Sub

Sub ramptrigger02_hit()
	WireRampOff	 'Turn off the Plastic Ramp Sound
End Sub


'************************************
' Diverse Collection Hit Sounds v3.0
'************************************

'Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
Sub aMetalWires_Hit(idx):PlaySoundAtBall "fx_MetalWire":End Sub
'Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
'Sub aRubber_LongBands_Hit(idx):PlaySoundAtBall "fx_rubber_longband":End Sub
'Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
'Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
'Sub aRubber_Pegs_Hit(idx):PlaySoundAtBall "fx_rubber_peg":End Sub
'Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
'Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
'Sub aTriggers_Hit(idx):PlaySfx:End Sub

' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
    Dim i

    bGameInPLay = True

    'resets the score display, and turn off attract mode
    StopAttractMode
    GiOn

    TotalGamesPlayed = TotalGamesPlayed + 1
    CurrentPlayer = 1
    PlayersPlayingGame = 1
    bOnTheFirstBall = True
    For i = 1 To MaxPlayers
        Score(i) = 0
        BonusPoints(i) = 10000
        BonusHeldPoints(i) = 0
        BonusMultiplier(i) = 1
        PlayfieldMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
    Next

    ' initialise any other flags
    Tilt = 0

    ' initialise Game variables
    Game_Init()

    ' you may wish to start some music, play a sound, do whatever at this point
    PlaySound "sc_StartUp"

    vpmtimer.addtimer 1500, "FirstBall '"
	pDMDStartGame ' setup PuP
End Sub

' This is used to delay the start of a game to allow any attract sequence to
' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
    ' reset the table for a new ball
    Dim light
    For Each light In MissionStatusLights
        If light.State = 1 Then
            light.State = 0
        End If
   Next
    ResetForNewPlayerBall()
    ' create a new ball in the shooters lane
    CreateNewBall()
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))

Sub ResetForNewPlayerBall()
    ' make sure the correct display is up to date
    AddScore 0

    ' set the current player's bonus multiplier back down to 1X
    ' SetBonusMultiplier 1

    ' reduce the playfield multiplier
    ' reset any drop targets, lights, game Mode etc..

    BonusPoints(CurrentPlayer) = 10000
    bBonusHeld = False
    bExtraBallWonThisBall = False

    ' Reset any table-specific variables for the new ball
    ResetNewBallVariables

    ' This is a new ball, so activate the ball saver
    bBallSaverReady = True

    ' and the skill shot
    bSkillShotReady = True


    ' Update mission status lights based on the current player's progress
    UpdateMissionStatusLights

End Sub


' Create a new ball on the Playfield

Sub CreateNewBall()
    ' create a ball in the plunger lane kicker.
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' kick it out..
    		RandomSoundBallRelease BallRelease
    BallRelease.Kick 90, 4
        DOF 123, DOFPulse

' if there is 2 or more balls then set the multibal flag (remember to check for locked balls and other balls used for animations)
' set the bAutoPlunger flag to kick the ball in play automatically
    If BallsOnPlayfield> 1 Then
        DOF 143, DOFPulse
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
            If mBalls2Eject = 0 Then 'if there are no more balls to eject then stop the timer
                CreateMultiballTimer.Enabled = False
            End If
        Else 'the max number of multiballs is reached, so stop the timer
            mBalls2Eject = 0
            CreateMultiballTimer.Enabled = False
        End If
    End If
End Sub

Sub ResetMissionObjects()
    ' Array of mission objects to reset visibility
    Dim missionObjects
    missionObjects = Array(Decoy, Destroyer, Drone1, Drone2, Drone3, Drone4, Drone5, Drone6, Fienna1, Fienna2, Fienna3, Interceptor, Probe, Satellite1, Satellite2, SOS, Soldier1, Soldier2, FlasherFienna1a, FlasherFienna2a, FlasherNexus1a, FlasherNexus2a,FlasherTimeWarp1a, FlasherTimeWarp2a)

    ' Loop through the array and set visibility to 0 for each mission object
    Dim i
    For i = LBound(missionObjects) To UBound(missionObjects)
        missionObjects(i).Visible = 0
    Next
End Sub

' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded

Sub EndOfBall()
    Dim TotalBonus
    TotalBonus = 0
    PlaySound "sc_drain"
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False
	ResetMissionObjects
	PuPEvent 7
    ' only process any of this if the table is not tilted.
    '(the tilt recovery mechanism will handle any extra balls or end of game)

    If NOT Tilted Then

        'add in any bonus points (multipled by the bonus multiplier)

        'Crash Bonus
        DMD CL(0, FormatScore(BonusPoints(CurrentPlayer) ) ), CL(1, "CRASH BONUS"), "", eNone, eNone, eNone, 1500, True, "vo_Crash Bonus"

' calculate the totalbonus
' DMD CL(0, "BONUS MULTIPLIER"), CL(1, "X " &BonusMultiplier(CurrentPlayer)), "", eNone, eNone, eNone, 1000, True, ""
        TotalBonus = BonusPoints(CurrentPlayer) * BonusMultiplier(CurrentPlayer) + BonusHeldPoints(CurrentPlayer)

        ' handle the bonus held
        ' reset the bonus held value since it has been already added to the bonus
        BonusHeldPoints(CurrentPlayer) = 0

        ' the player has won the bonus held award so do something with it :)
        If bBonusHeld Then
            If Balls = BallsPerGame Then ' this is the last ball, so if bonus held has been awarded then double the bonus
                TotalBonus = TotalBonus * 2
            End If
        Else ' this is not the last ball so save the bonus for the next ball
            BonusHeldPoints(CurrentPlayer) = TotalBonus
        End If
        bBonusHeld = False

        ' Add the bonus to the score
        DMD CL(0, FormatScore(TotalBonus) ), CL(1, "TOTAL BONUS"), "", eNone, eNone, eNone, 1500, True, ""

        AddScore TotalBonus

        ' add a bit of a delay to allow for the bonus points to be shown & added up
        vpmtimer.addtimer 3000, "EndOfBall2 '"
    Else 'if tilted then only add a short delay and move to the 2nd part of the end of the ball
        vpmtimer.addtimer 100, "EndOfBall2 '"
    End If
End Sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the CurrentPlayer)
'
Sub EndOfBall2()
    ' if were tilted, reset the internal tilted flag (this will also
    ' set TiltWarnings back to zero) which is useful if we are changing player
    Tilted = False
    Tilt = 0
    DisableTable False 'enable again bumpers and slingshots

    ' has the player won an extra-ball ? (might be multiple outstanding)
    If(ExtraBallsAwards(CurrentPlayer) <> 0) Then
        'debug.print "Extra Ball"

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1

        ' if no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurrentPlayer) = 0) Then
            li030.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
        DMD CL(0, "EXTRA BALL"), CL(1, "SHOOT AGAIN"), "", eNone, eNone, eBlink, 1000, True, "vo_Shoot Again"
		pStatus "SHOOT AGAIN!", 1000

        ' In this table an extra ball will have the skillshot and ball saver, so we reset the playfield for the new ball
        ResetForNewPlayerBall()

        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0) Then
            'debug.print "No More Balls, High Score Entry"

            ' Submit the CurrentPlayers score to the High Score system
            CheckHighScore()
        ' you may wish to play some music at this point

        Else

            ' not the last ball (for that player)
            ' if multiple players are playing then move onto the next one
            EndOfBallComplete()
        End If
    End If
End Sub

' This function is called when the end of bonus display
' (or high score entry finished) AND it either end the game or
' move onto the next player (or the next ball of the same player)
'
Sub EndOfBallComplete()
    Dim NextPlayer

    'debug.print "EndOfBall - Complete"

    ' are there multiple players playing this game ?
    If(PlayersPlayingGame> 1) Then
        ' then move to the next player
        NextPlayer = CurrentPlayer + 1
        ' are we going from the last player back to the first
        ' (ie say from player 4 back to player 1)
        If(NextPlayer> PlayersPlayingGame) Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'debug.print "Next Player = " & NextPlayer

    ' is it the end of the game ? (all balls been lost for all players)
    If((BallsRemaining(CurrentPlayer) <= 0) AND(BallsRemaining(NextPlayer) <= 0) ) Then
        ' you may wish to do some sort of Point Match free game award here
        ' generally only done when not in free play mode

        ' set the machine into game over mode
        EndOfGame()
		pDMDGameOver
    ' you may wish to put a Game Over message on the desktop/backglass

    Else
        ' set the next player
        CurrentPlayer = NextPlayer

        ' make sure the correct display is up to date
        AddScore 0

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

        ' AND create a new ball
        CreateNewBall()

        ' play a sound if more than 1 player
        If PlayersPlayingGame> 1 Then
            Select Case CurrentPlayer
                Case 1:DMD "", CL(1, "PLAYER 1"), "", eNone, eNone, eNone, 1000, True, "vo_player1"
				pStatus "PLAYER 1", 1000
                Case 2:DMD "", CL(1, "PLAYER 2"), "", eNone, eNone, eNone, 1000, True, "vo_player2"
				pStatus "PLAYER 2", 1000
                Case 3:DMD "", CL(1, "PLAYER 3"), "", eNone, eNone, eNone, 1000, True, "vo_player3"
				pStatus "PLAYER 3", 1000
                Case 4:DMD "", CL(1, "PLAYER 4"), "", eNone, eNone, eNone, 1000, True, "vo_player4"
				pStatus "PLAYER 4", 1000
            End Select
        End If
    End If
    ' Now that 48 seconds have passed, turn off the VictoryMagnet
    bMagnet.MagnetON = False
End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
    'debug.print "End Of Game"
    bGameInPLay = False
    ' just ended your game then play the end of game tune
    If NOT bJustStarted Then

    End If
    bJustStarted = False
    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0

    Dim p, i ' Declare loop variables
    ' Reset mission status light states for all players
    For p = 0 To 3 ' Loop through all 4 players
        For i = 1 To 18 ' Loop through all missions
            MissionStatusLightStates(p, i) = 0 ' Reset light state
        Next
    Next

    ' set any lights for the attract mode
    GiOff
    StartAttractMode
' you may wish to light any Game Over Light you may have

End Sub

Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp> BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
    End If
End Function

' *********************************************************************
'                      Drain / Plunger Functions
' *********************************************************************

' lost a ball ;-( check to see how many balls are on the playfield.
' if only one then decrement the remaining count AND test for End of game
' if more than 1 ball (multi-ball) then kill of the ball but don't create
' a new one
'
Sub Drain_Hit()
    ' Destroy the ball
    Drain.DestroyBall
    If bGameInPLay = False Then Exit Sub 'don't do anything, just delete the ball
    ' Exit Sub ' only for debugging - this way you can add balls from the debug window

    BallsOnPlayfield = BallsOnPlayfield - 1

    ' pretend to knock the ball into the ball storage mech

	RandomSoundDrain Drain
    'if Tilted the end Ball Mode
    If Tilted Then
        StopEndOfBallMode
    End If

    ' if there is a game in progress AND it is not Tilted
    If(bGameInPLay = True) AND(Tilted = False) Then

        ' is the ball saver active,
        If(bBallSaverActive = True) Then

        ' Check if mli019.State = 1, which means forced end of game
        If mli019.State = 1 Then
            ' End the game immediately
            EndOfBallComplete()
            mli019.State = 0 ' Reset the flag after ending the game
            Exit Sub ' Exit here to avoid further processing in Drain_Hit
        End If


            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in case the multiballs are being ejected
            AddMultiball 1
            ' we kick the ball with the autoplunger
            bAutoPlunger = True
            ' you may wish to put something on a display or play a sound at this point
            DMD "_", CL(1, "RE-DEPLOY"), "_", eNone, eNone, eNone, 1000, True, ""
			PuPEvent 6
			pStatus "RE-DEPLOY", 1000
        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
            If(BallsOnPlayfield = 1) Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True) then
                    ' not in multiball mode any more
                    bMultiBallMode = False
                    ' you may wish to change any music over at this point and

                    ' turn off any multiball specific lights
                    ' ChangeGi white
                    'stop any multiball modes
                    StopMBmodes
                End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0) Then
                ' End Mode and timers

                ' ChangeGi white
                ' Show the end of ball animation
                ' and continue with the end of ball
                ' DMD something?
                StopEndOfBallMode
                vpmtimer.addtimer 200, "EndOfBall '" 'the delay is depending of the animation of the end of ball, if there is no animation then move to the end of ball
            End If
        End If
    End If
End Sub

' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.

Sub swPlungerRest_Hit()
    'debug.print "ball in plunger lane"
    ' some sound according to the ball position
	BIPL=1
    PlaySoundAt "fx_sensor", swPlungerRest
    bBallInPlungerLane = True
    ' turn on Launch light is there is one
    'LaunchLight.State = 2

    'be sure to update the Scoreboard after the animations, if any

    ' kick the ball in play if the bAutoPlunger flag is on
    If bAutoPlunger Then
        'debug.print "autofire the ball"
        vpmtimer.addtimer 1500, "PlungerIM.AutoFire:bAutoPlunger = False:DOF 143, DOFPulse '" ' Outhere
    End If
    'Start the Selection of the skillshot if ready
    If bSkillShotReady Then
 If MusicMode = 0 Then
            PlaySong "mu_wait"  ' Play default main music
        ElseIf MusicMode = 1 Then
            PlaySong "mu_GE_SkillshotWait"  ' Play GE music
        End If
        UpdateSkillshot()
    End If
End Sub

' The ball is released from the plunger turn off some flags and check for skillshot

Sub swPlungerRest_UnHit()
	BIPL=0
    lighteffect 6
    bBallInPlungerLane = False
    swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active
    If bSkillShotReady Then
        If MusicMode = 0 Then
            PlaySong "mu_Main"  ' Play default main music
        ElseIf MusicMode = 1 Then
            PlaySong "mu_GE_Main"  ' Play GE music
        End If
    End If
    ' if there is a need for a ball saver, then start off a timer
    ' only start if it is ready, and it is currently not running, else it will reset the time period
    If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
        EnableBallSaver BallSaverTime
    End If
    ' turn off LaunchLights
    li113.State = 0
    li114.State = 0
    li115.State = 0
    li116.State = 0
    li117.State = 0
    li118.State = 0
    ' Start Fuel & timers
    Fuel = 6
    UpdateFuelLights
    StartFuelTimers
End Sub

Sub EnableBallSaver(seconds)
    'debug.print "Ballsaver started"
    ' set our game flag
    bBallSaverActive = True
    bBallSaverReady = False
    ' start the timer
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
    bBallSaverActive = False
    ' if you have a ball saver light then turn it off at this point
    LightShootAgain.State = 0
End Sub

Sub BallSaverSpeedUpTimer_Timer()
    'debug.print "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    LightShootAgain.BlinkInterval = 80
    LightShootAgain.State = 2
End Sub

' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board

Sub AddScore(points)
    If Tilted Then Exit Sub
    ' add the points to the current players score variable
    Score(CurrentPlayer) = Score(CurrentPlayer) + points * PlayfieldMultiplier(CurrentPlayer)
    ' Add some points to the Bonus and Jackpot if they are enabled
    If bBonusActivated Then AddBonus points
    If bJackpotActivated Then AddJackpot points

' you may wish to check to see if the player has gotten a replay due to high score
End Sub

' Add bonus to the bonuspoints AND update the score board

Sub AddBonus(points)
    If Tilted Then Exit Sub
    ' add the bonus to the current players bonus variable
    BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
    If BonusPoints(CurrentPlayer)> 5000000 Then
        BonusPoints(CurrentPlayer) = 5000000
    End If
End Sub

' Add some points to the current Jackpot.
'
Sub AddJackpot(points)
    ' Jackpots only generally increment in multiball mode AND not tilted
    ' but this doesn't have to be the case
    If Tilted Then Exit Sub

    ' If(bMultiBallMode = True) Then
    Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + points
' DMD "_", CL(1, "INCREASED JACKPOT"), "_", eNone, eNone, eNone, 1000, True, ""
' you may wish to limit the jackpot to a upper limit, ie..
'	If (Jackpot >= 6000) Then
'		Jackpot = 6000
' 	End if
'End if
End Sub

Sub AwardExtraBall() 'more like a replay in this table
    If NOT bExtraBallWonThisBall Then
        DMD "_", CL(1, ("REPLAY AWARDED") ), "_", eNone, eNone, eNone, 1000, True, SoundFXDOF("vo_Replay Awarded", 122, DOFPulse, DOFKnocker)
		pStatus "REPLAY AWARDED", 1000
        DOF 121, DOFPulse
        ExtraBallsAwards(CurrentPlayer) = 1 ' only one ball per ball in this game
        bExtraBallWonThisBall = True
        li030.State = 1                     'light the shoot again lamp
        GiEffect 2
        LightEffect 2
    END If
End Sub

Sub AwardSpecial()
    DMD "_", CL(1, ("EXTRA GAME WON") ), "_", eNone, eNone, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
    DOF 121, DOFPulse
    Credits = Credits + 1
    If bFreePlay = False Then DOF 125, DOFOn
    LightEffect 2
    GiEffect 2
End Sub

Sub AwardJackpot() 'award a normal jackpot
	PuPEvent 69
    pStatus "JACKPOT", 2000
    DMD CL(0, FormatScore(Jackpot(CurrentPlayer) ) ), CL(1, "JACKPOT"), "", eNone, eNone, eNone, 1000, True, ""
    DOF 126, DOFPulse
    AddScore Jackpot(CurrentPlayer)
    'reset the Jackpot Value
    Jackpot(CurrentPlayer) = 20000
    LightEffect 2
    GiEffect 2
End Sub

Sub AwardSkillshot()
    'stop the skillshot lights
    StopSkillshot
	PuPEvent 48
	pStatus "SKILLSHOT", 1000
    'show dmd animation
    DMD CL(0, FormatScore(SkillshotValue(CurrentPlayer) ) ), CL(1, "SKILLSHOT"), "", eNone, eNone, eNone, 1000, True, ""
    DOF 127, DOFPulse
    Addscore SkillShotValue(CurrentPlayer)
    'do some light show
    GiEffect 2
    LightEffect 2
    PlaySound "sc_skillshot"
End Sub

'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
    Dim x
    x = LoadValue(cGameName, "HighScore1")
    If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 100000 End If
    x = LoadValue(cGameName, "HighScore1Name")
    If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If
    x = LoadValue(cGameName, "HighScore2")
    If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 100000 End If
    x = LoadValue(cGameName, "HighScore2Name")
    If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If
    x = LoadValue(cGameName, "HighScore3")
    If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 100000 End If
    x = LoadValue(cGameName, "HighScore3Name")
    If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If
    x = LoadValue(cGameName, "HighScore4")
    If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 100000 End If
    x = LoadValue(cGameName, "HighScore4Name")
    If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If
    x = LoadValue(cGameName, "Credits")
    If(x <> "") then Credits = CInt(x) Else Credits = 0:If bFreePlay = False Then DOF 125, DOFOff:End If
    x = LoadValue(cGameName, "TotalGamesPlayed")
    If(x <> "") then TotalGamesPlayed = CInt(x) Else TotalGamesPlayed = 0 End If
End Sub

Sub Savehs
    SaveValue cGameName, "HighScore1", HighScore(0)
    SaveValue cGameName, "HighScore1Name", HighScoreName(0)
    SaveValue cGameName, "HighScore2", HighScore(1)
    SaveValue cGameName, "HighScore2Name", HighScoreName(1)
    SaveValue cGameName, "HighScore3", HighScore(2)
    SaveValue cGameName, "HighScore3Name", HighScoreName(2)
    SaveValue cGameName, "HighScore4", HighScore(3)
    SaveValue cGameName, "HighScore4Name", HighScoreName(3)
    SaveValue cGameName, "Credits", Credits
    SaveValue cGameName, "TotalGamesPlayed", TotalGamesPlayed
End Sub

Sub Reseths
    HighScoreName(0) = "AAA"
    HighScoreName(1) = "BBB"
    HighScoreName(2) = "CCC"
    HighScoreName(3) = "DDD"
    HighScore(0) = 150000
    HighScore(1) = 140000
    HighScore(2) = 130000
    HighScore(3) = 120000
    Savehs
    Loadhs
End Sub

' ***********************************************************
'  High Score Initals Entry Functions - based on Black's code
' ***********************************************************

Dim hsbModeActive
Dim hsbStartButtonPresses
Dim hsEnteredName
Dim hsEnteredDigits(3)
Dim hsCurrentDigit
Dim hsValidLetters
Dim hsCurrentLetter
Dim hsLetterFlash

Sub CheckHighscore()
    Dim tmp
    tmp = Score(CurrentPlayer)

    If tmp > HighScore(0)Then 'add 1 credit for beating the highscore
        Credits = Credits + 1
        DOF 125, DOFOn
    End If

    If tmp > HighScore(3)Then
        PlaySound SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
        DOF 121, DOFPulse
        HighScore(3) = tmp
        'enter player's name
        HighScoreEntryInit()
    Else
        EndOfBallComplete()
    End If
End Sub


Sub HighScoreEntryInit()
    hsbModeActive = True
    PlaySound "vo_enterinitials"
	pDMDSetPage(8)
	PuPEvent 2
	puPlayer.LabelSet pDMD,"HighScore3","ENTER INITIALS" ,1,""
	puPlayer.LabelSet pDMD,"HighScore2","" ,1,""
	puPlayer.LabelSet pDMD,"HighScore4","" & FormatNumber(Score(CurrentPlayer),0),1,""
    hsbModeActive = True
	hsbStartButtonPresses = 0

    hsLetterFlash = 0

    hsEnteredDigits(0) = " "
    hsEnteredDigits(1) = " "
    hsEnteredDigits(2) = " "
    hsCurrentDigit = 0

    hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789<" ' < is back arrow
    hsCurrentLetter = 1
    DMDFlush()
    HighScoreDisplayNameNow()

    HighScoreFlashTimer.Interval = 250
    HighScoreFlashTimer.Enabled = True
End Sub

Sub EnterHighScoreKey(keycode)
    If keycode = LeftFlipperKey Then
        playsound "fx_rubber_longband"
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0)then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = RightFlipperKey Then
        playsound "fx_rubber_peg"
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter > len(hsValidLetters))then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = PlungerKey OR keycode = StartGameKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<")then
            playsound "sc_6"
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3)then
                HighScoreCommitName()
            else
                HighScoreDisplayNameNow()
            end if
        else
            playsound "fx_Esc"
            hsEnteredDigits(hsCurrentDigit) = " "
            if(hsCurrentDigit > 0)then
                hsCurrentDigit = hsCurrentDigit - 1
            end if
            HighScoreDisplayNameNow()
        end if
    end if
End Sub

Sub HighScoreDisplayNameNow()
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreDisplayName()
    Dim i
    Dim TempTopStr
    Dim TempBotStr

    TempTopStr = "YOUR NAME:"
    dLine(0) = ExpandLine(TempTopStr, 0)
    DMDUpdate 1

    TempBotStr = "    > "
    if(hsCurrentDigit > 0)then TempBotStr = TempBotStr & hsEnteredDigits(0)
    if(hsCurrentDigit > 1)then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit > 2)then TempBotStr = TempBotStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3)then
        if(hsLetterFlash <> 0)then
            TempBotStr = TempBotStr & "_"
        else
            TempBotStr = TempBotStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit < 1)then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit < 2)then TempBotStr = TempBotStr & hsEnteredDigits(2)

    TempBotStr = TempBotStr & " <    "
    dLine(1) = ExpandLine(TempBotStr, 1)
    DMDUpdate 2

	puPlayer.LabelSet pDMD,"HighScore",""& TempBotStr ,1,""
End Sub

Sub HighScoreFlashTimer_Timer()
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = hsLetterFlash + 1
    if(hsLetterFlash = 2)then hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreCommitName()
    HighScoreFlashTimer.Enabled = False
    hsbModeActive = False

    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ")then
        hsEnteredName = "YOU"
    end if

    HighScoreName(3) = hsEnteredName
    SortHighscore
    EndOfBallComplete()
End Sub

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 0 to 3
        For j = 0 to 2
            If HighScore(j) < HighScore(j + 1)Then
                tmp = HighScore(j + 1)
                tmp2 = HighScoreName(j + 1)
                HighScore(j + 1) = HighScore(j)
                HighScoreName(j + 1) = HighScoreName(j)
                HighScore(j) = tmp
                HighScoreName(j) = tmp2
            End If
        Next
    Next
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


Sub pStatus(text, duration)
    ' Check if status text is actually changing
    If pStatusText <> text Then
        ' Set the status text
        pStatusText = text
        
        ' Update the status label only if it has changed
        puPlayer.LabelSet pDMD, "pupStatus", pStatusText, 1, ""
        
        ' Restart the timer
        DMDTimer.Interval = duration
        DMDTimer.Enabled = True
    End If
End Sub




' *************************************************************************
'   JP's Reduced Display Driver Functions (based on script by Black)
' only 5 effects: none, scroll left, scroll right, blink and blinkfast
' 3 Lines, treats all 3 lines as text. 3rd line is just 1 character
' Example format:
' DMD "text1","text2","backpicture", eNone, eNone, eNone, 250, True, "sound"
' Short names:
' dq = display queue
' de = display effect
' *************************************************************************

Const eNone = 0        ' Instantly displayed
Const eScrollLeft = 1  ' scroll on from the right
Const eScrollRight = 2 ' scroll on from the left
Const eBlink = 3       ' Blink (blinks for 'TimeOn')
Const eBlinkFast = 4   ' Blink (blinks for 'TimeOn') at user specified intervals (fast speed)

Const dqSize = 64

Dim dqHead
Dim dqTail
Dim deSpeed
Dim deBlinkSlowRate
Dim deBlinkFastRate

Dim dCharsPerLine(2)
Dim dLine(2)
Dim deCount(2)
Dim deCountEnd(2)
Dim deBlinkCycle(2)

Dim dqText(2, 64)
Dim dqEffect(2, 64)
Dim dqTimeOn(64)
Dim dqbFlush(64)
Dim dqSound(64)

Dim FlexDMD
Dim DMDScene

Sub DMD_Init() 'default/startup values
    If UseFlexDMD Then
        Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
        If Not FlexDMD is Nothing Then
            If FlexDMDHighQuality Then
                FlexDMD.TableFile = Table1.Filename & ".vpx"
                FlexDMD.RenderMode = 2
                FlexDMD.Width = 256
                FlexDMD.Height = 64
                FlexDMD.Clear = True
                FlexDMD.GameName = cGameName
                FlexDMD.Run = True
                Set DMDScene = FlexDMD.NewGroup("Scene")
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.bkborder")
                DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
                For i = 0 to 40
                    DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.d_empty&dmd=2")
                    Digits(i).Visible = False
                Next
                digitgrid.Visible = False
                For i = 0 to 19 ' Top
                    DMDScene.GetImage("Dig" & i).SetBounds 8 + i * 12, 6, 12, 22
                Next
                For i = 20 to 39 ' Bottom
                    DMDScene.GetImage("Dig" & i).SetBounds 8 + (i - 20) * 12, 34, 12, 22
                Next
                FlexDMD.LockRenderThread
                FlexDMD.Stage.AddActor DMDScene
                FlexDMD.UnlockRenderThread
            Else
                FlexDMD.TableFile = Table1.Filename & ".vpx"
                FlexDMD.RenderMode = 2
                FlexDMD.Width = 128
                FlexDMD.Height = 32
                FlexDMD.Clear = True
                FlexDMD.GameName = cGameName
                FlexDMD.Run = True
                Set DMDScene = FlexDMD.NewGroup("Scene")
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.bkborder")
                DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
                For i = 0 to 40
                    DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.d_empty&dmd=2")
                    Digits(i).Visible = False
                Next
                digitgrid.Visible = False
                For i = 0 to 19 ' Top
                    DMDScene.GetImage("Dig" & i).SetBounds 4 + i * 6, 3, 6, 11
                Next
                For i = 20 to 39 ' Bottom
                    DMDScene.GetImage("Dig" & i).SetBounds 4 + (i - 20) * 6, 17, 6, 11
                Next
                FlexDMD.LockRenderThread
                FlexDMD.Stage.AddActor DMDScene
                FlexDMD.UnlockRenderThread
            End If
        End If
    End If

    Dim i, j
    DMDFlush()
    deSpeed = 20
    deBlinkSlowRate = 5
    deBlinkFastRate = 2
    dCharsPerLine(0) = 20 'characters lower line
    dCharsPerLine(1) = 20 'characters top line
    dCharsPerLine(2) = 1  'characters back line
    For i = 0 to 2
        dLine(i) = Space(dCharsPerLine(i) )
        deCount(i) = 0
        deCountEnd(i) = 0
        deBlinkCycle(i) = 0
        dqTimeOn(i) = 0
        dqbFlush(i) = True
        dqSound(i) = ""
    Next
    For i = 0 to 2
        For j = 0 to 64
            dqText(i, j) = ""
            dqEffect(i, j) = eNone
        Next
    Next
    DMD dLine(0), dLine(1), dLine(2), eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDFlush()
    Dim i
    DMDTimer.Enabled = False
    DMDEffectTimer.Enabled = False
    dqHead = 0
    dqTail = 0
    For i = 0 to 2
        deCount(i) = 0
        deCountEnd(i) = 0
        deBlinkCycle(i) = 0
    Next
End Sub

Sub DMDScore()
    Dim tmp, tmp1, tmp1a, tmp1b, tmp2, tmp3, tmp4
    if(dqHead = dqTail)Then
        'tmp = CL(0, FormatScore(Score(Currentplayer) ) )
        tmp = FL(0, "P" &CurrentPlayer& " B" &Balls, FormatScore(Score(Currentplayer)))
        tmp1 = ""
        tmp2 = "bkborder"
        tmp3 = True 'true or False

        'tmp1 = CL(1, "PLAYER " & CurrentPlayer & " BALL " & Balls)
        'tmp1 = FormatScore(Bonuspoints(Currentplayer) ) & " X" &BonusMultiplier(Currentplayer)
        Select Case Mission(CurrentPlayer)
            Case 0: 'no Mission active
                If Score(CurrentPlayer) = 0 OR bSkillshotReady OR bGameInPlay = False Then
                    tmp1 = CL(1, "AWAITING DEPLOYMENT")
					pStatus "AWAITING DEPLOYMENT", 1000
                Else
                    If MissionSelected Then
                        tmp1 = CL(1, "ACCEPT MISSION")
					pStatus "ACCEPT MISSION", 1000
                    Else
                        tmp1 = CL(1, "SELECT A MISSION")
					pStatus "SELECT YOUR MISSION", 1000
                    End If
                End If
            Case 1:tmp1 = " RAMP HITS LEFT " &MissionHits
				   pStatus "RAMP HITS LEFT " &MissionHits, 1000
            Case 2:tmp1 = " LANE HITS LEFT " &MissionHits
				   pStatus "LANE HITS LEFT " &MissionHits, 1000
            Case 3:tmp1 = "BUMPER HITS LEFT " &MissionHits
				   pStatus "BUMPER HITS LEFT " &MissionHits, 1000
            Case 4:tmp1 = "DROPTARGETS LEFT " &MissionHits
				   pStatus "DROP TARGETS LEFT " &MissionHits, 1000
            Case 5:tmp1 = "TARGET HITS LEFT " &MissionHits
				   pStatus "TARGETS HITS LEFT " &MissionHits, 1000
            Case 6:tmp1 = "   UPGRADE FLAGS"
				   pStatus "UPGRADE FLAGS", 1000
            Case 61:tmp1 = "ENTER HYPERSPACE"
				   pStatus "ENTER HYPERSPACE", 1000
            Case 7:tmp1 = "POWER UP PHOTONS"
				   pStatus "POWER UP PHOTONS", 1000
            Case 71:tmp1 = "BUMPER HITS LEFT " &MissionHits
				   pStatus "BUMPER HITS LEFT " &MissionHits, 1000
            Case 8:tmp1 = "HIT YELLOW WORMHOLE"
				   pStatus "HIT THE YELLOW WORMHOLE", 1000
            Case 81:tmp1 = "HIT RED WORMHOLE"
				   pStatus "HIT RED WORMHOLE", 1000
            Case 9:tmp1 = " HIT SHIELD TARGETS"
				   pStatus "HIT SHIELD TARGETS", 1000
            Case 91:tmp1 = "ENTER HYPERSPACE"
				   pStatus "ENTER HYPERSPACE", 1000
            Case 10:tmp1 = "HIT DESTROYER LIGHTS"
				   pStatus "HIT ALL DESTROYER LIGHTS", 1000
            Case 101:tmp1 = "ENTER ANY WORMHOLE"
				   pStatus "ENTER ANY WORMHOLE", 1000
            Case 11:tmp1 = "LIGHT ENGINE LANES"
				   pStatus "LIGHT ENGINE LANES", 1000
            Case 111:tmp1 = "ENTER THE BLACK HOLE"
				   pStatus "ENTER THE BLACK HOLE", 1000
            Case 12:tmp1 = "FLAG SPINS LEFT " &MissionHits
				   pStatus "FLAG SPINS LEFT " &MissionHits, 1000
            Case 121:tmp1 = "  HIT SPACE WARP"
				   pStatus "HIT SPACE WARP", 1000
            Case 13:tmp1 = "SATELLITE HITS " &MissionHits
				   pStatus "SATELLITE HITS LEFT " &MissionHits, 1000
            Case 14:tmp1 = "LANE HITS LEFT " &MissionHits
				   pStatus "LANE HITS LEFT " &MissionHits, 1000
            Case 15:tmp1 = "OUTLANE HITS LEFT " &MissionHits
				   pStatus "OUTLANE HITS LEFT " &MissionHits, 1000
            Case 16:tmp1 = "REBOUND HITS LEFT " &MissionHits
				   pStatus "REBOUND HITS LEFT " &MissionHits, 1000
            Case 161:tmp1 = "RAMP OR HYPERSPACE"
				   pStatus "HIT RAMP OR HYPERSPACE", 1000
            'Maelstrom
            Case 17:tmp1 = "DROPTARGETS LEFT " & MissionHits
				   pStatus "DROP TARGETS LEFT " &MissionHits, 1000
            Case 171:tmp1 = "SPOT TARGETS LEFT " & MissionHits
				   pStatus "SPOT TARGETS LEFT " &MissionHits, 1000
            Case 172:tmp1 = "LANES LEFT " & MissionHits
				   pStatus "LANES LEFT " &MissionHits, 1000
            Case 173:tmp1 = " HIT THE FUEL CHUTE"
				   pStatus "HIT THE FUEL CHUTE", 1000
            Case 174:tmp1 = "HIT THE LAUNCH RAMP"
				   pStatus "HIT THE LAUNCH RAMP", 1000
            Case 175:tmp1 = "HIT A SPINNER"
				   pStatus "HIT A SPINNER", 1000
            Case 176:tmp1 = "ENTER A WORMHOLE"
				   pStatus "ENTER A WORMHOLE", 1000
            Case 177:tmp1 = "ENTER HYPERSPACE"
				   pStatus "ENTER HYPERSPACE", 1000
            Case 18:tmp1 = "HIT YELLOW WORMHOLE"
				   pStatus "HIT THE YELLOW WORMHOLE", 1000
            Case 181:tmp1 = "HIT RED WORMHOLE"
				   pStatus "HIT RED WORMHOLE", 1000
            Case 182:tmp1 = "HIT GREEN WORMHOLE"
				   pStatus "HIT GREEN WORMHOLE", 1000

        End Select
    End If
    If Fuel = 1 then tmp1 = CL(1, "WARNING - LOW FUEL")
    If Fuel = 0 then tmp1 = CL(1, "RE-FUEL SHIP")
    DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 25, tmp3, ""
End Sub

Sub DMDScoreNow
    DMDFlush
    DMDScore
End Sub

Sub DMD(Text0, Text1, Text2, Effect0, Effect1, Effect2, TimeOn, bFlush, Sound)
    if(dqTail < dqSize) Then
        if(Text0 = "_") Then
            dqEffect(0, dqTail) = eNone
            dqText(0, dqTail) = "_"
        Else
            dqEffect(0, dqTail) = Effect0
            dqText(0, dqTail) = ExpandLine(Text0, 0)
        End If

        if(Text1 = "_") Then
            dqEffect(1, dqTail) = eNone
            dqText(1, dqTail) = "_"
        Else
            dqEffect(1, dqTail) = Effect1
            dqText(1, dqTail) = ExpandLine(Text1, 1)
        End If

        if(Text2 = "_") Then
            dqEffect(2, dqTail) = eNone
            dqText(2, dqTail) = "_"
        Else
            dqEffect(2, dqTail) = Effect2
            dqText(2, dqTail) = Text2 'it is always 1 letter in this table
        End If

        dqTimeOn(dqTail) = TimeOn
        dqbFlush(dqTail) = bFlush
        dqSound(dqTail) = Sound
        dqTail = dqTail + 1
        if(dqTail = 1) Then
            DMDHead()
        End If
    End If
End Sub

Sub DMDHead()
    Dim i
    deCount(0) = 0
    deCount(1) = 0
    deCount(2) = 0
    DMDEffectTimer.Interval = deSpeed

    For i = 0 to 2
        Select Case dqEffect(i, dqHead)
            Case eNone:deCountEnd(i) = 1
            Case eScrollLeft:deCountEnd(i) = Len(dqText(i, dqHead) )
            Case eScrollRight:deCountEnd(i) = Len(dqText(i, dqHead) )
            Case eBlink:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
                deBlinkCycle(i) = 0
            Case eBlinkFast:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
                deBlinkCycle(i) = 0
        End Select
    Next
    if(dqSound(dqHead) <> "") Then
        PlaySound(dqSound(dqHead) )
    End If
    DMDEffectTimer.Enabled = True
End Sub

Sub DMDEffectTimer_Timer()
    DMDEffectTimer.Enabled = False
    DMDProcessEffectOn()
End Sub

Sub DMDTimer_Timer()
    Dim Head
    DMDTimer.Enabled = False

    Head = dqHead
    dqHead = dqHead + 1
    if(dqHead = dqTail)Then
        if(dqbFlush(Head) = True)Then
            DMDScoreNow()
        Else
            dqHead = 0
            DMDHead()
        End If
    Else
        DMDHead()
    End If

    ' Clear the status label if it's the one being processed
    If dqText(1, Head) = "pStatusText" Then
        puPlayer.LabelSet pDMD, "pupStatus", "", 1, ""
    End If

End Sub

Sub DMDProcessEffectOn()
    Dim i
    Dim BlinkEffect
    Dim Temp

    BlinkEffect = False

    For i = 0 to 2
        if(deCount(i) <> deCountEnd(i) ) Then
            deCount(i) = deCount(i) + 1

            select case(dqEffect(i, dqHead) )
                case eNone:
                    Temp = dqText(i, dqHead)
                case eScrollLeft:
                    Temp = Right(dLine(i), dCharsPerLine(i) - 1)
                    Temp = Temp & Mid(dqText(i, dqHead), deCount(i), 1)
                case eScrollRight:
                    Temp = Mid(dqText(i, dqHead), (dCharsPerLine(i) + 1) - deCount(i), 1)
                    Temp = Temp & Left(dLine(i), dCharsPerLine(i) - 1)
                case eBlink:
                    BlinkEffect = True
                    if((deCount(i) MOD deBlinkSlowRate) = 0) Then
                        deBlinkCycle(i) = deBlinkCycle(i) xor 1
                    End If

                    if(deBlinkCycle(i) = 0) Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(dCharsPerLine(i) )
                    End If
                case eBlinkFast:
                    BlinkEffect = True
                    if((deCount(i) MOD deBlinkFastRate) = 0) Then
                        deBlinkCycle(i) = deBlinkCycle(i) xor 1
                    End If

                    if(deBlinkCycle(i) = 0) Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(dCharsPerLine(i) )
                    End If
            End Select

            if(dqText(i, dqHead) <> "_") Then
                dLine(i) = Temp
                DMDUpdate i
            End If
        End If
    Next

    if(deCount(0) = deCountEnd(0) ) and(deCount(1) = deCountEnd(1) ) and(deCount(2) = deCountEnd(2) ) Then

        if(dqTimeOn(dqHead) = 0) Then
            DMDFlush()
        Else
            if(BlinkEffect = True) Then
                DMDTimer.Interval = 10
            Else
                DMDTimer.Interval = dqTimeOn(dqHead)
            End If

            DMDTimer.Enabled = True
        End If
    Else
        DMDEffectTimer.Enabled = True
    End If
End Sub

Function ExpandLine(TempStr, id) 'id is the number of the dmd line
    If TempStr = "" Then
        TempStr = Space(dCharsPerLine(id) )
    Else
        if(Len(TempStr)> Space(dCharsPerLine(id) ) ) Then
            TempStr = Left(TempStr, Space(dCharsPerLine(id) ) )
        Else
            if(Len(TempStr) < dCharsPerLine(id) ) Then
                TempStr = TempStr & Space(dCharsPerLine(id) - Len(TempStr) )
            End If
        End If
    End If
    ExpandLine = TempStr
End Function

Function FormatScore(ByVal Num) 'it returns a string with commas (as in Black's original font)
    dim i
    dim NumString

    NumString = CStr(abs(Num) )

    For i = Len(NumString) -3 to 1 step -3
        if IsNumeric(mid(NumString, i, 1) ) then
            NumString = left(NumString, i-1) & chr(asc(mid(NumString, i, 1) ) + 128) & right(NumString, Len(NumString) - i)
        end if
    Next
    FormatScore = NumString
End function

Function FL(id, NumString1, NumString2) 'Fill line
    Dim Temp, TempStr
    Temp = dCharsPerLine(id) - Len(NumString1) - Len(NumString2)
    TempStr = NumString1 & Space(Temp) & NumString2
    FL = TempStr
End Function

Function CL(id, NumString) 'center line
    Dim Temp, TempStr
    If Len(NumString) < dCharsPerLine(id) Then
        Temp = (dCharsPerLine(id) - Len(NumString) ) \ 2
        TempStr = Space(Temp) & NumString & Space(Temp)
        CL = TempStr
    Else
        CL = LEFT(NumString, dCharsPerLine(id) )
    End If
End Function

Function RL(id, NumString) 'right line
    Dim Temp, TempStr
    If Len(NumString) < dCharsPerLine(id) Then
        Temp = dCharsPerLine(id) - Len(NumString)
        TempStr = Space(Temp) & NumString
        RL = TempStr
    Else
        RL = LEFT(NumString, dCharsPerLine(id) )
    End If
End Function

'**************
' Update DMD
'**************

Sub DMDUpdate(id)
    Dim digit, value
    If UseFlexDMD Then FlexDMD.LockRenderThread
    Select Case id
        Case 0 'top text line
            For digit = 0 to 19
                DMDDisplayChar mid(dLine(0), digit + 1, 1), digit
            Next
        Case 1 'bottom text line
            For digit = 20 to 39
                DMDDisplayChar mid(dLine(1), digit -19, 1), digit
            Next
        Case 2 ' back image - back animations
            If dLine(2) = "" OR dLine(2) = " " Then dLine(2) = "bkborder"
            Digits(40).ImageA = dLine(2)
            If UseFlexDMD Then DMDScene.GetImage("Back").Bitmap = FlexDMD.NewImage("", "VPX." & dLine(2) & "&dmd=2").Bitmap
    End Select
    If UseFlexDMD Then FlexDMD.UnlockRenderThread
End Sub

Sub DMDDisplayChar(achar, adigit)
    If achar = "" Then achar = " "
    achar = ASC(achar)
    Digits(adigit).ImageA = Chars(achar)
    If UseFlexDMD Then DMDScene.GetImage("Dig" & adigit).Bitmap = FlexDMD.NewImage("", "VPX." & Chars(achar) & "&dmd=2&add").Bitmap
End Sub

'*******************************
'  JP's new DMD using flashers
' can now use small letters too
'*******************************

Dim Digits, Chars(255), Images(255)

DMDInit

Sub DMDInit
    Dim i
    Digits = Array(digit001, digit002, digit003, digit004, digit005, digit006, digit007, digit008, digit009, digit010, _
        digit011, digit012, digit013, digit014, digit015, digit016, digit017, digit018, digit019, digit020,            _
        digit021, digit022, digit023, digit024, digit025, digit026, digit027, digit028, digit029, digit030,            _
        digit031, digit032, digit033, digit034, digit035, digit036, digit037, digit038, digit039, digit040,            _
        digit041)
    For i = 0 to 255:Chars(i) = "d_empty":Next

    Chars(32) = "d_empty"
    Chars(33) = ""        '!
    Chars(34) = ""        '"
    Chars(35) = ""        '#
    Chars(36) = ""        '$
    Chars(37) = ""        '%
    Chars(38) = ""        '&
    Chars(39) = ""        ''
    Chars(40) = ""        '(
    Chars(41) = ""        ')
    Chars(42) = ""        '*
    Chars(43) = "d_plus"  '+
    Chars(44) = ""        '
    Chars(45) = "d_minus" '-
    Chars(46) = "d_dot"   '.
    Chars(47) = ""        '/
    Chars(48) = "d_0"     '0
    Chars(49) = "d_1"     '1
    Chars(50) = "d_2"     '2
    Chars(51) = "d_3"     '3
    Chars(52) = "d_4"     '4
    Chars(53) = "d_5"     '5
    Chars(54) = "d_6"     '6
    Chars(55) = "d_7"     '7
    Chars(56) = "d_8"     '8
    Chars(57) = "d_9"     '9
    Chars(60) = "d_less"  '<
    Chars(61) = ""        '=
    Chars(62) = "d_more"  '>
    Chars(64) = ""        '@
    Chars(65) = "d_a"     'A
    Chars(66) = "d_b"     'B
    Chars(67) = "d_c"     'C
    Chars(68) = "d_d"     'D
    Chars(69) = "d_e"     'E
    Chars(70) = "d_f"     'F
    Chars(71) = "d_g"     'G
    Chars(72) = "d_h"     'H
    Chars(73) = "d_i"     'I
    Chars(74) = "d_j"     'J
    Chars(75) = "d_k"     'K
    Chars(76) = "d_l"     'L
    Chars(77) = "d_m"     'M
    Chars(78) = "d_n"     'N
    Chars(79) = "d_o"     'O
    Chars(80) = "d_p"     'P
    Chars(81) = "d_q"     'Q
    Chars(82) = "d_r"     'R
    Chars(83) = "d_s"     'S
    Chars(84) = "d_t"     'T
    Chars(85) = "d_u"     'U
    Chars(86) = "d_v"     'V
    Chars(87) = "d_w"     'W
    Chars(88) = "d_x"     'X
    Chars(89) = "d_y"     'Y
    Chars(90) = "d_z"     'Z
    Chars(94) = "d_up"    '^
    '    Chars(95) = '_
    Chars(96) = ""  '`
    Chars(97) = ""  'a
    Chars(98) = ""  'b
    Chars(99) = ""  'c
    Chars(100) = "" 'd
    Chars(101) = "" 'e
    Chars(102) = "" 'f
    Chars(103) = "" 'g
    Chars(104) = "" 'h
    Chars(105) = "" 'i
    Chars(106) = "" 'j
    Chars(107) = "" 'k
    Chars(108) = "" 'l
    Chars(109) = "" 'm
    Chars(110) = "" 'n
    Chars(111) = "" 'o
    Chars(112) = "" 'p
    Chars(113) = "" 'q
    Chars(114) = "" 'r
    Chars(115) = "" 's
    Chars(116) = "" 't
    Chars(117) = "" 'u
    Chars(118) = "" 'v
    Chars(119) = "" 'w
    Chars(120) = "" 'x
    Chars(121) = "" 'y
    Chars(122) = "" 'z
    Chars(123) = "" '{
    Chars(124) = "" '|
    Chars(125) = "" '}
    Chars(126) = "" '~
    ' numbers with the dot
    Chars(176) = "d_0a" '0.
    Chars(177) = "d_1a" '1.
    Chars(178) = "d_2a" '2.
    Chars(179) = "d_3a" '3.
    Chars(180) = "d_4a" '4.
    Chars(181) = "d_5a" '5.
    Chars(182) = "d_6a" '6.
    Chars(183) = "d_7a" '7.
    Chars(184) = "d_8a" '8.
    Chars(185) = "d_9a" '9.
End Sub

'********************
' Real Time updates
'********************
'used for all the real time updates

Sub RealTime_Timer
    RollingUpdate
    LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle
    RightFlipperTop.RotZ = RightFlipper.CurrentAngle
End Sub

Sub LeftLaneGate_Animate:LeftDiverter.RotZ = LeftLaneGate.CurrentAngle:End Sub
Sub RightLaneGate_Animate:RightDiverter.RotZ = RightLaneGate.CurrentAngle:End Sub

'********************************************************************************************
' Only for VPX 10.2 and higher.
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

'******************************************
' Change light color - simulate color leds
' changes the light color and state
' 11 colors: red, orange, amber, yellow...
'******************************************
' in this table this colors are use to keep track of the progress during the modes

'colors
Const red = 10
Const orange = 9
Const amber = 8
Const yellow = 7
Const darkgreen = 6
Const green = 5
Const blue = 4
Const darkblue = 3
Const purple = 2
Const white = 1
Const teal = 0

Sub SetLightColor(n, col, stat) 'stat 0 = off, 1 = on, 2 = blink, -1= no change
    Select Case col
        Case red
            n.color = RGB(18, 0, 0)
            n.colorfull = RGB(128, 0, 0)
        Case orange
            n.color = RGB(18, 3, 0)
            n.colorfull = RGB(255, 64, 0)
        Case amber
            n.color = RGB(193, 49, 0)
            n.colorfull = RGB(255, 153, 0)
        Case yellow
            n.color = RGB(18, 18, 0)
            n.colorfull = RGB(255, 240, 0)
        Case darkgreen
            n.color = RGB(0, 8, 0)
            n.colorfull = RGB(0, 64, 0)
        Case green
            n.color = RGB(0, 16, 0)
            n.colorfull = RGB(0, 128, 0)
        Case blue
            n.color = RGB(0, 8, 18)
            n.colorfull = RGB(0, 64, 128)
        Case darkblue
            n.color = RGB(0, 8, 8)
            n.colorfull = RGB(0, 64, 64)
        Case purple
            n.color = RGB(64, 0, 96)
            n.colorfull = RGB(128, 0, 192)
        Case white
            n.color = RGB(255, 197, 143)
            n.colorfull = RGB(255, 252, 224)
        Case teal
            n.color = RGB(1, 64, 62)
            n.colorfull = RGB(2, 128, 126)
    End Select
    If stat <> -1 Then
        n.State = 0
        n.State = stat
    End If
End Sub

Sub SetFlashColor(n, col, stat) 'stat 0 = off, 1 = on, -1= no change - no blink for the flashers
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
        Case white
            n.color = RGB(255, 252, 224)
        Case teal
            n.color = RGB(2, 128, 126)
    End Select
    If stat <> -1 Then
        n.Visible = stat
    End If
End Sub

'*************************
' Rainbow Changing Lights
'*************************

Dim RGBStep, RGBFactor, rRed, rGreen, rBlue, RainbowLights

Sub StartRainbow(n) 'n is a collection
    set RainbowLights = n
    RGBStep = 0
    RGBFactor = 5
    rRed = 255
    rGreen = 0
    rBlue = 0
    RainbowTimer.Enabled = 1
End Sub

Sub StopRainbow()
    RainbowTimer.Enabled = 0
    TurnOffArrows
End Sub

Sub TurnOffArrows() 'during Missions when changing modes
    For each x in aArrows
        x.State = 0
    Next
End Sub

Sub RainbowTimer_Timer 'rainbow led light color changing
    Dim obj
    Select Case RGBStep
        Case 0 'Green
            rGreen = rGreen + RGBFactor
            If rGreen> 255 then
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
            If rBlue> 255 then
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
            If rRed> 255 then
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

' ********************************
'   Table info & Attract Mode
' ********************************

Sub ShowTableInfo
    Dim ii
    'info goes in a loop only stopped by the credits and the startkey
    If Score(1) Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 1 " &FormatScore(Score(1) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(2) Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 2 " &FormatScore(Score(2) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(3) Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 3 " &FormatScore(Score(3) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(4) Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 4 " &FormatScore(Score(4) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    DMD "", CL(1, "GAME OVER"), "", eNone, eNone, eNone, 2000, False, ""
    If bFreePlay Then
        DMD "", CL(1, "FREE PLAY"), "", eNone, eNone, eNone, 2000, False, ""
    Else
        If Credits> 0 Then
            DMD CL(0, "CREDITS " & Credits), CL(1, "PRESS START"), "", eNone, eNone, eNone, 2000, False, ""
        Else
            DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
        End If
    End If
    DMD "        JPSALAS", "          PRESENTS", "d_jppresents", eNone, eNone, eNone, 3000, False, ""
    DMD "", "", "d_title", eNone, eNone, eNone, 4000, False, ""
    DMD "", CL(1, "ROM VERSION " &myversion), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL(0, "HIGHSCORES"), Space(dCharsPerLine(1) ), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL(0, "HIGHSCORES"), "", "", eNone, eNone, eNone, 1000, False, ""
    DMD CL(0, "HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(dCharsPerLine(0) ), Space(dCharsPerLine(1) ), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub

Sub StartAttractMode
    Dim a
    StartRainbow BumperLights
    StartLightSeq
    DMDFlush
    ShowTableInfo
    PlaySong ""
End Sub

Sub StopAttractMode
    StopRainbow
    DMDScoreNow
    LightSeqAttract.StopPlay
End Sub

Sub StartLightSeq()
    'lights sequences
    LightSeqAttract.UpdateInterval = 100
    LightSeqAttract.Play SeqRandom, 50, , 1000
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

'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' droptargets, animations, timers, etc
Sub VPObjects_Init
    LowerPost
    LeftLaneGate.RotateToEnd
    RightLaneGate.RotateToEnd
End Sub

' tables variables and Mode init
Dim BallsinWormHole
Dim WormHoleColor
Dim WormholeMBStep
Dim CurrentWormhole
Dim AttackBumperColor
Dim EngineBumperColor
Dim bFlagsUpgraded
Dim HyperSpaceHits
Dim bJackpotActivated
Dim bReflexRampShot
Dim bReflexHyperShot
Dim Fuel
Dim BoosterTarget1
Dim BoosterTarget2
Dim BoosterTarget3
Dim BoosterCount
Dim bBonusActivated
Dim MedalTarget1
Dim MedalTarget2
Dim MedalTarget3
Dim MedalCount
Dim xTarget1
Dim xTarget2
Dim xTarget3
Dim xTargetCount
Dim bMissionBonus
Dim Mission(4)
Dim Progress(4)
Dim Rank(4)
Dim MissionSelected
Dim MissionTarget1
Dim MissionTarget2
Dim MissionTarget3
Dim MissionHits 'hits to finish each Mission

Sub Game_Init() 'called at the start of a new game
    Dim i, j
    bExtraBallWonThisBall = False
    TurnOffPlayfieldLights()
    'Play some Music

    'Init Variables
    WormholeMBStep = 0
    MissionSelected = 0
    MissionHits = 0
    Fuel = 6
    For i = 0 to 4
        BonusPoints(i) = 10000 'crash bonus start points
        BonusMultiplier(i) = 1
        PlayfieldMultiplier(i) = 1
        BallsInLock(i) = 0
        Progress(i) = -1 'because the lights array starts at 0 and counts as 1
        Rank(i) = 0
        Mission(i) = 0
    Next
'MainMode Init()
End Sub

Sub InstantInfo
    DMD CL(0, "INSTANT INFO"), "", "", eScrollLeft, eNone, eNone, 1000, False, ""
    Select Case Mission(CurrentPlayer)
        Case 0
        case 1 'Launch Training 'top target
			pupDMDDisplay "splash","LAUNCH^TRAINING^PASS THE LAUNCH RAMP 3X", "", 3, 0, 10
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "LAUNCH TRAINING"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "PASS THE LAUNCH"), CL(1, "RAMP 3 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""

        case 2 'Re-entry Training 'middle target
			pupDMDDisplay "splash","RE-ENTRY^TRAINING^PASS THE RE-ENTRY LANES 3X", "", 3, 0, 10
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "RE-ENTRY TRAINING"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "PASS THE RE-ENTRY"), CL(1, "LANES 3 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""

        case 3 'Target Practice 'lower target
			pupDMDDisplay "splash","TARGET^PRACTICE^HIT ATTACK BUMPERS 8X", "", 3, 0, 10
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "TARGET PRACTICE"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "HIT THE ATTACK"), CL(1, "BUMPERS 8 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""

        case 4 'Satellite Mission 'all three targets
			pupDMDDisplay "splash","SATELLITE^MISSION^HIT 9 DROP TARGETS", "", 3, 0, 10
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "SATELLITE MISSION"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "HIT 9"), CL(1, "DROPTARGETS"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 5 'Bug Hunt Mission
			pupDMDDisplay "splash","MINING DRONE^MISSION^HIT 15 DROP TARGETS", "", 3, 0, 10
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "BUG HUNT MISSION"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "HIT 15"), CL(1, "TARGETS"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 6, 61 'Rescue Mission
			pupDMDDisplay "splash","RESCUE^MISSION^LIGHT FLAGS + HYPERSPACE", "", 3, 0, 10
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "RESCUE MISSION"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "LIGHT ALL BOOSTERS"), CL(1, "ENTER HYPERSPACE"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 7, 71 'Probe Mission
			pupDMDDisplay "splash","DESTROY^NEXUS PROBE^LIGHT TOP LANES + BUMPERS 8X", "", 3, 0, 10
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "DESTROY NEXUS PROBE"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "UPGRADE THE"), CL(1, "ATTACK BUMPERS"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "HIT THE ATTACK"), CL(1, "BUMPERS 8 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 8, 81, 82 'Andromeda Mission
			pupDMDDisplay "splash","ANDROMEDA^MISSION^ENTER WORMHOLES IN ORDER", "", 3, 0, 10
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "ANDROMEDA"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "ENTER THE 3"), CL(1, "WORMHOLES IN ORDER"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 9, 91 'Interceptor Mission
			pupDMDDisplay "splash","DESTROY^INTERCEPTOR^DISABLE SHIELDS + PHOTON BLAST", "", 3, 0, 10
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "INTERCEPTOR"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "LIGHT ALL 3"), CL(1, "SHIELD LIGHTS"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "ENTER THE"), CL(1, "HYPERSPACE KICKER"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 10, 101 'Escape Destroyer Mission
			pupDMDDisplay "splash","ESCAPE^DESTROYER^HIT 3 LIGHTS + WORMHOLE", "", 3, 0, 10
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "ESCAPE DESTROYER"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "LIGHT ALL 3"), CL(1, "DESTROYER LIGHTS"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "ENTER ANY"), CL(1, "WORMHOLE"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 11, 111 'Black Hole Mission
			pupDMDDisplay "splash","BLACK HOLE^MISSION^LIGHT 3 LANES + BLACKHOLE", "", 3, 0, 10
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "BLACK HOLE THREAT"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "LIGHT ALL"), CL(1, "ENGINE LANES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "ENTER THE"), CL(1, "BLACK HOLE"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 12, 121 'Cosmic Plague Mission
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "COSMIC PLAGUE"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "SPIN FLAGS"), CL(1, "75 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "HIT THE"), CL(1, "SPACE WARP ROLLOVER"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 13 'Satellite Repair Mission
			pupDMDDisplay "splash","SATELLITE^REPAIR^HIT SATELLITE BUMPER 3X", "", 3, 0, 10
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "SATELLITE Repair"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "HIT 3 TIMES"), "THE SATELLITE BUMPER", "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 14 'Recon Mission
			pupDMDDisplay "splash","RECON MISSION^PASS ANY LANE 15X", "", 3, 0, 10
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "RECON MISSION"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "PASS ANY OF THE"), CL(1, "LANES 15 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 15 'Evacuation Mission
			pupDMDDisplay "splash","EVACUATION^MISSION^HIT OUTLANES 3X", "", 3, 0, 10
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "EVACUATION"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "SEND BALL 3 TIMES"), CL(1, "THROUGH OUTLINES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 16, 161 'Security Breach Mission
			pupDMDDisplay "splash","SECURITY^BREACH^HIT REBOUNDS 25X", "", 3, 0, 10
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "TIME WARP"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "HIT THE REBOUNDS"), CL(1, "25 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "ENTER LAUNCH RAMP"), "OR HYPERSPACE KICKER", "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
        case 17, 171, 172, 173, 174, 175, 176, 177 'Maelstrom
			pupDMDDisplay "splash","ENTER THE^MAELSTROM^HIT 3 DROP TARGETS", "", 3, 0, 10
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "MAELSTROM"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD "_", CL(1, "HIT 3 DROPTARGETS"), "", eNone, eScrollLeft, eNone, 1500, True, ""
            DMD "_", CL(1, "HIT 3 SPOT TARGETS"), "", eNone, eScrollLeft, eNone, 1500, True, ""
            DMD "_", CL(1, "HIT 5 LANES"), "", eNone, eScrollLeft, eNone, 1500, True, ""
            DMD "_", CL(1, "HIT THE FUEL CHUTE"), "", eNone, eScrollLeft, eNone, 1500, True, ""
            DMD "_", CL(1, "HIT THE LAUNCH RAMP"), "", eNone, eScrollLeft, eNone, 1500, True, ""
            DMD "_", CL(1, "ROLL A FLAG"), "", eNone, eScrollLeft, eNone, 1500, True, ""
            DMD "_", CL(1, "ENTER A WORMHOLE"), "", eNone, eScrollLeft, eNone, 1500, True, ""
            DMD "_", CL(1, "ENTER HYPERSPACE"), "", eNone, eScrollLeft, eNone, 1500, True, ""
        case 18, 181, 182 'Time Warp
			pupDMDDisplay "splash","TIME^WARP^ENTER WORMHOLES IN ORDER", "", 3, 0, 10
            DMD CL(0, "MISSION ACCEPTED"), CL(1, "ANDROMEDA"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
            DMD CL(0, "ENTER THE 3"), CL(1, "WORMHOLES IN ORDER"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""

    End Select
    DMD CL(0, "BONUS POINTS"), CL(1, FormatScore(BonusPoints(CurrentPlayer))), "", eScrollLeft, eScrollLeft, eNone, 1500, False, ""

    DMD CL(0, "JACKPOT POINTS"), CL(1, FormatScore(Jackpot(CurrentPlayer))), "", eScrollLeft, eScrollLeft, eNone, 1500, False, ""

    If Score(1)Then
        DMD CL(0, "PLAYER 1 SCORE"), CL(1, FormatScore(Score(1))), "", eScrollLeft, eScrollLeft, eNone, 3000, False, ""


    End If
    If Score(2)Then
        DMD CL(0, "PLAYER 2 SCORE"), CL(1, FormatScore(Score(2))), "", eScrollLeft, eScrollLeft, eNone, 3000, False, ""
    End If
    If Score(3)Then
        DMD CL(0, "PLAYER 3 SCORE"), CL(1, FormatScore(Score(3))), "", eScrollLeft, eScrollLeft, eNone, 3000, False, ""
    End If
    If Score(4)Then
        DMD CL(0, "PLAYER 4 SCORE"), CL(1, FormatScore(Score(4))), "", eScrollLeft, eScrollLeft, eNone, 3000, False, ""
    End If
End Sub

Sub StopMBmodes 'stop multiball modes after loosing the last multibal
    If MusicMode = 0 Then
        PlaySong "mu_Main"  ' Play default main music
    ElseIf MusicMode = 1 Then
        PlaySong "mu_GE_Main"  ' Play GE music
    End If
    AsteroidsTimer.Enabled = 0
    li049.State = 0
    li050.State = 0
    li051.State = 0
End Sub

Sub StopEndOfBallMode() 'this sub is called after the last ball in play is drained, reset skillshot, modes, timers
    DisableGravity
    StopMissions
    DowngradeEngineTimer.Enabled = 0:EngineBumperColor = 0
    DowngradeWeaponsTimer.Enabled = 0:AttackBumperColor = 0
    pfXTimerExpired.Enabled = 0:xTargetCount = 0
    BoosterTimerExpired.Enabled = 0:BoosterCount = 0
    FuelSpeedUpTimer.Enabled = 0
    FuelTimerExpired.Enabled = 0
    HyperExpiredTimer.Enabled = 0:HyperSpaceHits = 0


    ' Loop through MissionStatusLights and reset any blinking light (State = 2)
    Dim light
    For Each light In MissionStatusLights
        If light.State = 2 Then
            light.State = 0
        End If
    Next
End Sub

Sub ResetNewBallVariables() 'reset objects, variables and lights for a new ball or player
    'lights
    TurnOffPlayfieldLights
    'objects
    OpenLeftGate
    vpmtimer.Addtimer 250, "OpenRightGate '"
    vpmtimer.Addtimer 500, "LowerPost '"
    'variables
    WormHoleColor = 0
    CurrentWormhole = 0
    BallsinWormHole = 0
    AttackBumperColor = 0
    EngineBumperColor = 0
    UpdateBumperColor
    bFlagsUpgraded = False
    BonusPoints(CurrentPlayer) = 10000
    bBonusActivated = False
    bBonusHeld = False
    HyperSpaceHits = 0
    UpdateHyperLights
    Jackpot(CurrentPLayer) = 20000
    bJackpotActivated = False
    StopReflexRampShot
    StopReflexHyperShot
    vpmtimer.Addtimer 750, "ResetBoosterTargets '"
    BoosterCount = 0
    UpdateBoosterLights
    bFlagsUpgraded = False
    bJackpotActivated = False
    vpmtimer.Addtimer 1000, "ResetMedalDT '"
    MedalCount = 0
    UpdateMedal
    vpmtimer.Addtimer 1250, "ResetXtargets '"
    PlayfieldMultiplier(CurrentPlayer) = 1
    xTargetCount = 0
    UpdatexTargets
    bMissionBonus = False
    PrepareForMission
    UpdateRankLights
    UpdateProgressLights
End Sub

Sub TurnOffPlayfieldLights()
    For each i in aLights
        i.State = 0
    Next
End Sub

Sub UpdateSkillShot()                'Setup and updates the skillshot lights
    DMD "", "HIT SKILLSHOT LIGHTS", "", eNone, eNone, eNone, 1500, True, ""
    LightSeqSkillshot.Play SeqAllOff 'turn off all the lights but the skillshot lights
    For each i in aSkillshotLights2:i.State = 2:Next
End Sub

Sub StopSkillshot() 'turn off the skillshot lights
    DMDScoreNow
    LightSeqSkillshot.StopPlay
    For each i in aSkillshotLights2:i.State = 0:Next
    bSkillshotReady = False
End Sub

' *********************************************************************
'                        Table Object Hit Events
'
' Any target hit Sub will follow this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/Mode this trigger is a member of
' - set the "LastSwitchHit" variable in case it is needed later
' *********************************************************************

'*********************************************************
' Slingshots has been hit

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
    RandomSoundSlingshotLeft Lemk
    DOF  103, DOFPulse ' Outhere
    PlaySound "sc_slingshot"
    DOF 105, DOFPulse
    LeftSling004.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add some points
    AddScore 500
    ' check modes
    ' add some effect to the table?
    FlashForMs gi002, 500, 50, 0
    FlashForMs gi004, 500, 50, 0
    ' check for modes
    Select Case Mission(CurrentPlayer)
        case 16
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing004.Visible = 0:LeftSLing003.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing003.Visible = 0:LeftSLing002.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing002.Visible = 0:Lemk.RotX = -10:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    If Tilted Then Exit Sub
    RandomSoundSlingshotRight Remk
    DOF  104, DOFPulse ' Outhere
    PlaySound "sc_slingshot"
    DOF 106, DOFPulse
    RightSling004.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add some points
    AddScore 500
    ' check modes
    ' add some effect to the table?
    FlashForMs gi001, 500, 50, 0
    FlashForMs gi003, 500, 50, 0
    ' check for modes
    Select Case Mission(CurrentPlayer)
        case 16
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing004.Visible = 0:RightSLing003.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing003.Visible = 0:RightSLing002.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing002.Visible = 0:Remk.RotX = -10:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

' Top Slingshots has been hit

Dim LStep2, RStep2

Sub LeftSlingShot2_Slingshot
    If Tilted Then Exit Sub
    RandomSoundSlingshotLeft Lemk2 
    PlaySound "sc_slingshot"
    DOF 105, DOFPulse
    Rubber018.Visible = 1
    Lemk2.RotX = 26
    LStep2 = 0
    LeftSlingShot2.TimerEnabled = True
    ' add some points
    AddScore 500
    ' check for modes
    Select Case Mission(CurrentPlayer)
        case 16
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub LeftSlingShot2_Timer
    Select Case LStep2
        Case 1:Rubber018.Visible = 0:Rubber017.Visible = 1:Lemk2.RotX = 14
        Case 2:Rubber017.Visible = 0:Rubber016.Visible = 1:Lemk2.RotX = 2
        Case 3:Rubber016.Visible = 0:Lemk2.RotX = -10:LeftSlingShot2.TimerEnabled = 0
    End Select
    LStep2 = LStep2 + 1
End Sub

Sub RightSlingShot2_Slingshot
    If Tilted Then Exit Sub
    RandomSoundSlingshotRight Remk2
    PlaySound "sc_slingshot"
    DOF 106, DOFPulse
    Rubber015.Visible = 1
    Remk2.RotX = 26
    RStep2 = 0
    RightSlingShot2.TimerEnabled = True
    ' add some points
    AddScore 500
    ' check for modes
    Select Case Mission(CurrentPlayer)
        case 16
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub RightSlingShot2_Timer
    Select Case RStep2
        Case 1:Rubber015.Visible = 0:Rubber014.Visible = 1:Remk2.RotX = 14
        Case 2:Rubber014.Visible = 0:Rubber013.Visible = 1:Remk2.RotX = 2
        Case 3:Rubber013.Visible = 0:Remk2.RotX = -10:Rightslingshot2.TimerEnabled = 0
    End Select
    RStep2 = RStep2 + 1
End Sub

'***********************
'    Attack Bumpers
'***********************

Sub Bumper1_Hit
    If Tilted Then Exit Sub
	RandomSoundBumperTop Bumper1
    PlaySound "sc_bumper"
    DOF 107, DOFPulse ' Outhere
    DOF 138, DOFPulse
    FlashForms li135, 1500, 75, 1
    ' add some points
    Select Case AttackBumperColor
        Case 0:AddScore 500
        Case 1:AddScore 1000
        Case 2:AddScore 1500
        Case 3:AddScore 2000
    End Select
    ' check for modes
    Select Case Mission(CurrentPlayer)
        case 3, 71:MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Bumper2_Hit
    If Tilted Then Exit Sub
    RandomSoundBumperMiddle Bumper2
    PlaySound "sc_bumper"
    DOF 108, DOFPulse ' Outhere
    DOF 138, DOFPulse
    FlashForms li136, 1500, 75, 1
    ' add some points
    Select Case AttackBumperColor
        Case 0:AddScore 500
        Case 1:AddScore 1000
        Case 2:AddScore 1500
        Case 3:AddScore 2000
    End Select
    ' check for modes
    Select Case Mission(CurrentPlayer)
        case 3, 71:MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Bumper3_Hit
    If Tilted Then Exit Sub
	RandomSoundBumperBottom Bumper3
    PlaySound "sc_bumper"
    DOF 109, DOFPulse ' Outhere
    DOF 138, DOFPulse
    FlashForms li137, 1500, 75, 1
    ' add some points
    Select Case AttackBumperColor
        Case 0:AddScore 500
        Case 1:AddScore 1000
        Case 2:AddScore 1500
        Case 3:AddScore 2000
    End Select
    ' check for modes
    Select Case Mission(CurrentPlayer)
        case 3, 71:MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Bumper4_Hit 'Satellite bumper
    If Tilted Then Exit Sub
    RandomSoundBumperTop Bumper4
    PlaySound "sc_bumper"
    DOF 113, DOFPulse ' Outhere
    DOF 138, DOFPulse
    FlashForms li138, 1500, 75, 1
    ' add some points
    Select Case AttackBumperColor
        Case 0:AddScore 1500
        Case 1:AddScore 2500
        Case 2:AddScore 3500
        Case 3:AddScore 4500
    End Select
    ' check for modes
    Select Case Mission(CurrentPlayer)
        case 13
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub UpgradeWeapons 'upgrade the color of the weapon bumpers
    AttackBumperColor = AttackBumperColor + 1
    Select Case AttackBumperColor
        Case 0 'blue
            For Each i In RightBumperLights
                SetLightColor i, Blue, 1
            Next
            DMD "_", CL(1, "PHOTONS AT 25"), "", eNone, eNone, eNone, 2500, True, ""
			PupEvent 50
			pStatus "PHOTONS AT 25%", 3500
        Case 1 'green
            For Each i In RightBumperLights
                SetLightColor i, Green, 1
            Next
            DMD "_", CL(1, "PHOTONS AT 50"), "", eNone, eNone, eNone, 2500, True, ""
			PupEvent 51
			pStatus "PHOTONS AT 50%", 3500
        Case 2 'yellow
            For Each i In RightBumperLights
                SetLightColor i, Yellow, 1
            Next
            DMD "_", CL(1, "PHOTONS AT 75"), "", eNone, eNone, eNone, 2500, True, ""
			PupEvent 52
			pStatus "PHOTONS AT 75%", 3500
        Case 3 'red
            For Each i In RightBumperLights
                SetLightColor i, Red, 1
            Next
            DMD "_", CL(1, "PHOTONS AT 100"), "", eNone, eNone, eNone, 2000, True, ""
			PupEvent 53
			pStatus "PHOTONS AT 100%", 3500
    End Select
    If AttackBumperColor > 3 Then AttackBumperColor = 3
    DowngradeWeaponsTimer.Enabled = 0
    DowngradeWeaponsTimer.Enabled = 1
    UpdateBumperColor
End Sub


Sub DowngradeWeapons 'upgrade the color of the weapon bumpers
    AttackBumperColor = AttackBumperColor - 1
    If AttackBumperColor <= 0 then
        AttackBumperColor = 0
        DowngradeWeaponsTimer.Enabled = 0
    End If
    UpdateBumperColor
End Sub

Sub DowngradeWeaponsTimer_Timer
    DowngradeWeapons
End Sub

Sub UpdateBumperColor
    Select case AttackBumperColor
        Case 0 'blue
            For each i in RightBumperLights:SetLightColor i, Blue, 1:next
        Case 1 'green
            For each i in RightBumperLights:SetLightColor i, Green, 1:next
        Case 2 'yellow
            For each i in RightBumperLights:SetLightColor i, Yellow, 1:next
        Case 3 'red
            For each i in RightBumperLights:SetLightColor i, Red, 1:next
    End Select
    Select case EngineBumperColor
        Case 0 'blue
            For each i in LeftBumperLights:SetLightColor i, Blue, 1:next
        Case 1 'green
            For each i in LeftBumperLights:SetLightColor i, Green, 1:next
        Case 2 'yellow
            For each i in LeftBumperLights:SetLightColor i, Yellow, 1:next
        Case 3 'red
            For each i in LeftBumperLights:SetLightColor i, Red, 1:next
    End Select
End Sub

'***********************
'    Engine Bumpers
'***********************

Sub Bumper5_Hit
    If Tilted Then Exit Sub
	RandomSoundBumperTop Bumper5
    DOF 138, DOFPulse
    PlaySound "sc_bumper"
    DOF 110, DOFPulse ' Outhere
    FlashForms li139, 1500, 75, 1
    ' add some points
    Select Case EngineBumperColor
        Case 0:AddScore 1500
        Case 1:AddScore 2500
        Case 2:AddScore 3500
        Case 3:AddScore 4500
    End Select
    ' check for modes
    Select Case Mission(CurrentPLayer)
        Case 0
    End Select
End Sub

Sub Bumper6_Hit
    If Tilted Then Exit Sub
    RandomSoundBumperMiddle Bumper6
    DOF 138, DOFPulse
    PlaySound "sc_bumper"
    DOF 111, DOFPulse ' Outhere
    FlashForms li140, 1500, 75, 1
    ' add some points
    Select Case EngineBumperColor
        Case 0:AddScore 1500
        Case 1:AddScore 2500
        Case 2:AddScore 3500
        Case 3:AddScore 4500
    End Select
    ' check for modes
    Select Case Mission(CurrentPLayer)
        Case 0
    End Select
End Sub

Sub Bumper7_Hit
    If Tilted Then Exit Sub
	RandomSoundBumperBottom Bumper7
    DOF 138, DOFPulse
    PlaySound "sc_bumper"
    DOF 112, DOFPulse ' Outhere
    FlashForms li141, 1500, 75, 1
    ' add some points
    Select Case EngineBumperColor
        Case 0:AddScore 1500
        Case 1:AddScore 2500
        Case 2:AddScore 3500
        Case 3:AddScore 4500
    End Select
    ' check for modes
    Select Case Mission(CurrentPLayer)
        Case 0
    End Select
End Sub

Sub UpgradeEngine 'upgrade the color of the Engine bumpers
    EngineBumperColor = EngineBumperColor + 1
    Select Case EngineBumperColor
        Case 0 'blue
            For Each i In LeftBumperLights
                SetLightColor i, Blue, 1
            Next
            DMD "_", CL(1, "ENGINES AT 25"), "", eNone, eNone, eNone, 2000, True, ""
			pStatus "ENGINES AT 25%", 2000
        Case 1 'green Engines 50%
            For Each i In LeftBumperLights
                SetLightColor i, Green, 1
            Next
            DMD "_", CL(1, "ENGINES AT 50"), "", eNone, eNone, eNone, 2000, True, ""
			PuPEvent 62
			pStatus "ENGINES AT 50%", 2000
        Case 2 'yellow
            For Each i In LeftBumperLights
                SetLightColor i, Yellow, 1
            Next
            DMD "_", CL(1, "ENGINES AT 75"), "", eNone, eNone, eNone, 2000, True, ""
			PuPEvent 63
			pStatus "ENGINES AT 75%", 2000
        Case 3 'red
            For Each i In LeftBumperLights
                SetLightColor i, Red, 1
            Next
            DMD "_", CL(1, "ENGINES AT 100"), "", eNone, eNone, eNone, 1500, True, ""
			PuPEvent 64
			pStatus "ENGINES AT 100%", 2000
    End Select
    If EngineBumperColor > 3 then EngineBumperColor = 3
    DowngradeEngineTimer.Enabled = 0
    DowngradeEngineTimer.Enabled = 1
    UpdateBumperColor
    If Mission(CurrentPlayer) = 11 Then CheckMission
End Sub


Sub DowngradeEngine 'upgrade the color of the engine bumper
    EngineBumperColor = EngineBumperColor - 1
    If EngineBumperColor <= 0 then
        EngineBumperColor = 0
        DowngradeEngineTimer.Enabled = 0
    End If
    UpdateBumperColor
End Sub

Sub DowngradeEngineTimer_Timer
    DowngradeEngine
End Sub

' Rotate lane lights
Sub RotateLaneLights(n) 'n is the direction, 1 or 0, left or right
    Dim tmp
    If n = 1 Then
        tmp = li124.State
        li124.State = li125.State
        li125.State = li126.State
        li126.State = tmp
        tmp = li063.State
        li063.State = li064.State
        li064.State = li065.State
        li065.State = tmp
    Else
        tmp = li126.State
        li126.State = li125.State
        li125.State = li124.State
        li124.State = tmp
        tmp = li065.State
        li065.State = li064.State
        li064.State = li063.State
        li063.State = tmp
    End If
End Sub

'*************
' Lower lanes
'*************

Sub Trigger001_Hit 'left outlane
    PlaySoundAt "fx_sensor", Trigger001
    If Tilted Then Exit Sub
    playSound "sc_lane"
    Addscore 20000
    If li032.State = 1 Then AwardExtraBall:li032.State = 0
    Select Case Mission(CurrentPlayer)
        case 14, 15, 172
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Trigger002_Hit 'left inlane, re-fuel, Bonus lane
    PlaySoundAt "fx_sensor", Trigger002
    If Tilted Then Exit Sub
    AddScore 10000
    'Mission Bonus AND/OR Booster Bonus activated
    If bMissionBonus OR bBonusActivated Then
		PuPEvent 70
	    pStatus "BONUS AWARDED", 1500
        DMD "_", CL(1, "BONUS AWARDED"), "", eNone, eNone, eNone, 1500, True, ""
        AddScore BonusHeldPoints(CurrentPlayer)
        If bMissionBonus Then bMissionBonus = False:li034.State = 0
    End If
    're-fuel
	pStatus "SHIP RE-FUELED", 1500
    DMD "", CL(1, "SHIP RE-FUELED"), "", eNone, eNone, eNone, 1500, True, "sc_shiprefueled"
    Fuel = 6:UpdateFuelLights:StartFuelTimers
    Select Case Mission(CurrentPlayer)
        case 14, 172
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Trigger003_Hit 'left inlane
 '   PlaySoundAt "fx_sensor", Trigger003
    If Tilted Then Exit Sub
    playSound "sc_lane"
    Addscore 5000
    If li033.State = 1 Then AddScore 20000
    li033.State = 0
    Select Case Mission(CurrentPlayer)
        case 14, 172
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Trigger004_Hit 'right inlane
'    PlaySoundAt "fx_sensor", Trigger004
    If Tilted Then Exit Sub
    playSound "sc_lane"
    Addscore 5000
    If li035.State = 1 Then AddScore 20000
    li035.State = 0
    Select Case Mission(CurrentPlayer)
        case 14, 172
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Trigger005_Hit 'right outlane
'    PlaySoundAt "fx_sensor", Trigger005
    If Tilted Then Exit Sub
    playSound "sc_lane"
    Addscore 20000
    If li036.State = 1 Then AwardExtraBall:li036.State = 0
    Select Case Mission(CurrentPlayer)
        case 14, 15, 172
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

'***********
' Top lanes
'***********

Sub Trigger006_Hit 'left
'    PlaySoundAt "fx_sensor", Trigger006
    If Tilted Then Exit Sub
    If bSkillShotready Then StopSkillShot
    playSound "sc_lane"
    Addscore 2000
    li063.State = 1
    CheckWeaponLights
    Select Case Mission(CurrentPlayer)
        case 2, 14, 172:MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Trigger007_Hit 'center
'    PlaySoundAt "fx_sensor", Trigger007
    If Tilted Then Exit Sub
    If bSkillShotready Then StopSkillShot
    playSound "sc_lane"
    Addscore 2000
    li064.State = 1
    CheckWeaponLights
    Select Case Mission(CurrentPlayer)
        case 2, 14, 172:MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Trigger008_Hit 'right
'    PlaySoundAt "fx_sensor", Trigger008
    If Tilted Then Exit Sub
    If bSkillShotready Then StopSkillShot
    playSound "sc_lane"
    Addscore 2000
    li065.State = 1
    CheckWeaponLights
    Select Case Mission(CurrentPlayer)
        case 2, 14, 172:MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub CheckWeaponLights
    If li063.State + li064.State + li065.State = 3 then
        UpgradeWeapons
        LightSeqReEntry.Play SeqBlinking, , 15, 10
        li063.State = 0
        li064.State = 0
        li065.State = 0
        If Mission(CurrentPlayer) = 7 Then CheckMission
    End If
End Sub

'**************
' Engine lanes
'**************

Sub Trigger009_Hit 'left
    'PlaySoundAt "fx_sensor", Trigger009
    If Tilted Then Exit Sub
    playSound "sc_lane"
    Addscore 500
    li124.State = 1
    CheckEngineLights
    Select Case Mission(CurrentPlayer)
        case 14
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Trigger010_Hit 'center
    'PlaySoundAt "fx_sensor", Trigger010
    If Tilted Then Exit Sub
    playSound "sc_lane"
    Addscore 500
    li125.State = 1
    CheckEngineLights
    Select Case Mission(CurrentPlayer)
        case 14, 172
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Trigger011_Hit 'right
    'PlaySoundAt "fx_sensor", Trigger011
    If Tilted Then Exit Sub
    playSound "sc_lane"
    Addscore 500
    li126.State = 1
    CheckEngineLights
    Select Case Mission(CurrentPlayer)
        case 14, 172
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub CheckEngineLights
    If li124.State + li125.State + li126.State = 3 then
        UpgradeEngine
        LightSeqEngine.Play SeqBlinking, , 15, 10
        li124.State = 0
        li125.State = 0
        li126.State = 0
    End If
End Sub


Sub Trigger030_Hit ' Ramp done
    If Tilted Then Exit Sub
    PlaySound "sc_end"
    Addscore 5000
    If bReflexRampShot Then
        AwardReflexRampShot
    Else
        StartReflexRampShot
    End If

    ' Check mission status and assign or accept mission depending on mli017
    Select Case Mission(CurrentPlayer)
        Case 0 ' No mission active
            If MissionSelected Then
                ' Use AssignMission if mli017 is 1, else use AcceptMission if mli017 is 0
                If mli017.State = 1 Then
                    AssignMission MissionSelected 
                Else
                    AcceptMission MissionSelected 
                End If
            End If
        Case 1: MissionHits = MissionHits - 1: CheckMission 
        Case 161: CheckMission
        Case 174: CheckMission
    End Select
End Sub





'**************
'  Fuel lanes
'**************

Sub Trigger012_Hit
    PlaySoundAt "fx_sensor", Trigger012
    If Tilted Then Exit Sub
    PlaySound "sc_fuellights"
    Addscore 500
    If Fuel < 1 Then
        Flashforms li128, 500, 50, 1
        Fuel = 1:StartFuelTimers
    Else
        Flashforms li128, 500, 50, 2
    End If
End Sub

Sub Trigger013_Hit
    PlaySoundAt "fx_sensor", Trigger013
    If Tilted Then Exit Sub
    PlaySound "sc_fuellights"
    Addscore 500
    If Fuel < 2 Then
        Flashforms li129, 500, 50, 1
        Fuel = 2:StartFuelTimers
    Else
        Flashforms li129, 500, 50, 2
    End If
End Sub

Sub Trigger014_Hit
    PlaySoundAt "fx_sensor", Trigger014
    If Tilted Then Exit Sub
    PlaySound "sc_fuellights"
    Addscore 500
    If Fuel < 3 Then
        Flashforms li130, 500, 50, 1
        Fuel = 3:StartFuelTimers
    Else
        Flashforms li130, 500, 50, 2
    End If
End Sub

Sub Trigger015_Hit
    PlaySoundAt "fx_sensor", Trigger015
    If Tilted Then Exit Sub
    PlaySound "sc_fuellights"
    Addscore 500
    If Fuel < 4 Then
        Flashforms li131, 500, 50, 1
        Fuel = 4:StartFuelTimers
    Else
        Flashforms li131, 500, 50, 2
    End If
End Sub

Sub Trigger016_Hit
    PlaySoundAt "fx_sensor", Trigger016
    If Tilted Then Exit Sub
    PlaySound "sc_fuellights"
    Addscore 500
    If Fuel < 5 Then
        Flashforms li131, 500, 50, 1
        Fuel = 5:StartFuelTimers
    Else
        Flashforms li131, 500, 50, 2
    End If
End Sub

Sub Trigger017_Hit
    PlaySoundAt "fx_sensor", Trigger017
    If Tilted Then Exit Sub
    'PlaySound "sc_fuellights"
    Addscore 500
    If li133.State = 0 Then Addscore 1000
    're-fuel
    DMD "", CL(1, "SHIP RE-FUELED"), "", eNone, eNone, eNone, 1000, True, "sc_shiprefueled"
    Fuel = 6:UpdateFuelLights:StartFuelTimers
    If Mission(CurrentPlayer) = 173 Then CheckMission
End Sub

'**************
' Fuel Timers
'**************

Sub StartFuelTimers
    FuelSpeedUpTimer.Enabled = 0
    FuelTimerExpired.Enabled = 0
    FuelSpeedUpTimer.Enabled = 1
    FuelTimerExpired.Enabled = 1
End Sub

Sub FuelTimerExpired_Timer 'reduces the fuel after 15 seconds
    Fuel = Fuel - 1
    UpdateFuelLights
    If Fuel = 0 then 'Stop Mission
        StopMissions
    End If
End Sub

Sub FuelSpeedUpTimer_Timer 'blinks the light 5 seconds before it expires
    Select Case Fuel
        Case 0:li128.State = 0:li129.State = 0:li130.State = 0:li131.State = 0:li132.State = 0:li133.State = 0
        Case 1:li128.State = 2:li129.State = 0:li130.State = 0:li131.State = 0:li132.State = 0:li133.State = 0
        Case 2:li128.State = 1:li129.State = 2:li130.State = 0:li131.State = 0:li132.State = 0:li133.State = 0
        Case 3:li128.State = 1:li129.State = 1:li130.State = 2:li131.State = 0:li132.State = 0:li133.State = 0
        Case 4:li128.State = 1:li129.State = 1:li130.State = 1:li131.State = 2:li132.State = 0:li133.State = 0
        Case 5:li128.State = 1:li129.State = 1:li130.State = 1:li131.State = 1:li132.State = 2:li133.State = 0
        Case 6:li128.State = 1:li129.State = 1:li130.State = 1:li131.State = 1:li132.State = 1:li133.State = 2
    End Select
End Sub

Sub UpdateFuelLights
    Select case Fuel
        Case 0:li128.State = 0:li129.State = 0:li130.State = 0:li131.State = 0:li132.State = 0:li133.State = 0
        Case 1:li128.State = 1:li129.State = 0:li130.State = 0:li131.State = 0:li132.State = 0:li133.State = 0
        Case 2:li128.State = 1:li129.State = 1:li130.State = 0:li131.State = 0:li132.State = 0:li133.State = 0
        Case 3:li128.State = 1:li129.State = 1:li130.State = 1:li131.State = 0:li132.State = 0:li133.State = 0
        Case 4:li128.State = 1:li129.State = 1:li130.State = 1:li131.State = 1:li132.State = 0:li133.State = 0
        Case 5:li128.State = 1:li129.State = 1:li130.State = 1:li131.State = 1:li132.State = 1:li133.State = 0
        Case 6:li128.State = 1:li129.State = 1:li130.State = 1:li131.State = 1:li132.State = 1:li133.State = 1
    End Select
End Sub

'*****************
' Skillshot lanes
'*****************

Sub Trigger018_Hit
    PlaySoundAt "fx_sensor", Trigger018
    If Tilted Then Exit Sub
    If bSkillShotready Then
        If ActiveBall.VelY < 0 Then 'only when the ball is going up
            PlaySound "sc_skillshotlight"
            li113.State = 1
            SkillShotValue(CurrentPLayer) = 15000
        End If
    End If
End Sub

Sub Trigger019_Hit
    PlaySoundAt "fx_sensor", Trigger019
    If Tilted Then Exit Sub
    If bSkillShotready Then
        If ActiveBall.VelY < 0 Then 'only when the ball is going up
            PlaySound "sc_skillshotlight"
            li114.State = 1
            SkillShotValue(CurrentPLayer) = 30000
        End If
    End If
End Sub

Sub Trigger020_Hit
    PlaySoundAt "fx_sensor", Trigger020
    If Tilted Then Exit Sub
    If bSkillShotready Then
        If ActiveBall.VelY < 0 Then 'only when the ball is going up
            PlaySound "sc_skillshotlight"
            li115.State = 1
            SkillShotValue(CurrentPLayer) = 75000
        End If
    End If
End Sub

Sub Trigger021_Hit
    PlaySoundAt "fx_sensor", Trigger021
    If Tilted Then Exit Sub
    If bSkillShotready Then
        If ActiveBall.VelY < 0 Then 'only when the ball is going up
            PlaySound "sc_skillshotlight"
            li116.State = 1
            SkillShotValue(CurrentPLayer) = 30000
        End If
    End If
End Sub

Sub Trigger022_Hit
    PlaySoundAt "fx_sensor", Trigger022
    If Tilted Then Exit Sub
    If bSkillShotready Then
        If ActiveBall.VelY < 0 Then 'only when the ball is going up
            PlaySound "sc_skillshotlight"
            li117.State = 1
            SkillShotValue(CurrentPLayer) = 15000
        End If
    End If
End Sub

Sub Trigger023_Hit
    PlaySoundAt "fx_sensor", Trigger023
    If Tilted Then Exit Sub
    If bSkillShotready Then
        If ActiveBall.VelY < 0 Then 'only when the ball is going up
            PlaySound "sc_skillshotlight"
            li118.State = 1
            SkillShotValue(CurrentPLayer) = 7500
        End If
    End If
End Sub

Sub Trigger029_Hit 'give skillshot
    PlaySoundAt "fx_sensor", Trigger029
    If Tilted Then Exit Sub
    If bSkillShotready Then AwardSkillshot
End Sub

'*******************
' Space Warp trigger
'*******************

Sub Trigger028_Hit
    PlaySoundAt "fx_sensor", Trigger028
    If Tilted Then Exit Sub
    PlaySound "sc_wormholetarget"
    FlashForMs li092, 1000, 50, 0
    AddScore 10000
    li033.State = 1 'turn on the inlane lights for extra scoring
    li035.State = 1
    If Mission(CurrentPlayer) = 121 Then CheckMission
End Sub

'*****************
' Wormhole target
'*****************

Sub Target001_Hit
	TargetBouncer Activeball, 1
    If Tilted Then Exit Sub
    Addscore 750
    Select Case Mission(CurrentPlayer)
        case 5, 171
            MissionHits = MissionHits-1:CheckMission
        case 8, 81, 18, 181,182 'do nothing
        case else
            Flashforms li047, 1000, 50, 1
            PlaySound "sc_wormholetarget"
            ' activate Wormholes and change the color of the Wormhole lights
            li142.State = 1
            li143.State = 1
            li144.State = 1
            If WormholeColor = 0 Then WormholeColor = RndNbr(3) 'activate wormholes by giving them a color
            ChangeArrowColor
    End Select
End Sub

Sub ChangeArrowColor
    If WormholeColor = 0 Then Exit Sub 'if it is called from the spinners and the wormholes are not activated
    WormHoleColor = WormHoleColor + 1
    If WormHoleColor> 3 Then WormHoleColor = 1
    Select case WormHoleColor
        Case 1
            SetLightColor li080, yellow, 1
            SetLightColor li081, yellow, 1
            SetLightColor li082, yellow, 1
        Case 2
            SetLightColor li080, red, 1
            SetLightColor li081, red, 1
            SetLightColor li082, red, 1
        Case 3
            SetLightColor li080, green, 1
            SetLightColor li081, green, 1
            SetLightColor li082, green, 1
    End Select
End Sub

'*****************
' Mission targets
'*****************

Sub Target002_Hit 'lower
	TargetBouncer Activeball, 1
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    Select Case Mission(CurrentPlayer)
        case 0
            FlashForMs li043, 500, 50, 1
            FlashForMs li096, 500, 50, 1
            MissionTarget3 = 1
            CheckMissiontargets 3
        case 5, 171
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target003_Hit 'center
	TargetBouncer Activeball, 1
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    Select Case Mission(CurrentPlayer)
        case 0
            FlashForMs li042, 500, 50, 1
            FlashForMs li096, 500, 50, 1
            MissionTarget2 = 1
            CheckMissiontargets 2
        case 5, 171
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target004_Hit 'upper
	TargetBouncer Activeball, 1
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    Select Case Mission(CurrentPlayer)
        case 0
            FlashForMs li041, 500, 50, 1
            FlashForMs li096, 500, 50, 1
            MissionTarget1 = 1
            CheckMissiontargets 1
        case 5, 171
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub


'********************
' Top re-fuel targets
'********************

Sub Target005_Hit
	TargetBouncer Activeball, 1
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    Addscore 750
    li066.State = 1
    CheckTopTargets
    Select Case Mission(CurrentPlayer)
        case 5, 171
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target006_Hit
	TargetBouncer Activeball, 1
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    Addscore 750
    li067.State = 1
    CheckTopTargets
    Select Case Mission(CurrentPlayer)
        case 5, 171
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target007_Hit
	TargetBouncer Activeball, 1
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    Addscore 750
    li068.State = 1
    CheckTopTargets
    Select Case Mission(CurrentPlayer)
        case 5, 171
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub CheckTopTargets
    If li066.State + li067.State + li068.State = 3 then
        're-fuel
        DMD "", CL(1, "SHIP RE-FUELED"), "", eNone, eNone, eNone, 1500, True, ""
        Fuel = 6:UpdateFuelLights:StartFuelTimers
        FlashForms li066, 1000, 50, 0
        FlashForms li067, 1000, 50, 0
        FlashForms li068, 1000, 50, 0
    End If
End Sub

'*****************************
' Comet targets -Right Hazard
'*****************************

Sub Target008_Hit 'lower
    TargetBouncer Activeball, 1
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    Addscore 750
    li052.State = 1
    CheckCometLights

    ' Trigger shake effect if Mission 9 (Interceptor mission) is active
    If Mission(CurrentPlayer) = 9 Then
        ShakeHit "Interceptor"
    End If

    Select Case Mission(CurrentPlayer)
        case 5, 171
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target009_Hit 'center
    TargetBouncer Activeball, 1
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    Addscore 750
    li053.State = 1
    CheckCometLights

    ' Trigger shake effect if Mission 9 (Interceptor mission) is active
    If Mission(CurrentPlayer) = 9 Then
        ShakeHit "Interceptor"
    End If

    Select Case Mission(CurrentPlayer)
        case 5, 171
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target010_Hit 'upper
    TargetBouncer Activeball, 1
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    Addscore 750
    li054.State = 1
    CheckCometLights

    ' Trigger shake effect if Mission 9 (Interceptor mission) is active
    If Mission(CurrentPlayer) = 9 Then
        ShakeHit "Interceptor"
    End If

    Select Case Mission(CurrentPlayer)
        case 5, 171
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub


Sub CheckCometLights
    If li052.State + li053.State + li054.State = 3 Then
        li052.State = 0
        li053.State = 0
        li054.State = 0
        LightSeqComet.Play SeqBlinking, , 15, 10
        OpenRightGate
        If Mission(CurrentPlayer) = 9 Then CheckMission
    End If
End Sub


'********************************
' Radiation targets -Left Hazard
'********************************

Sub Target011_Hit 'lower
	TargetBouncer Activeball, 1
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    Addscore 750
    li056.State = 1
    CheckRadiationLights
    Select Case Mission(CurrentPlayer)
        case 5, 171
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target012_Hit 'center
	TargetBouncer Activeball, 1
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    Addscore 750
    li057.State = 1
    CheckRadiationLights
    Select Case Mission(CurrentPlayer)
        case 5, 171
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target013_Hit 'upper
	TargetBouncer Activeball, 1
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    Addscore 750
    li058.State = 1
    CheckRadiationLights
    Select Case Mission(CurrentPlayer)
        case 5, 171
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub CheckRadiationLights
    If li056.State + li057.State + li058.State = 3 Then
        li056.State = 0
        li057.State = 0
        li058.State = 0
        LightSeqRadiation.Play SeqBlinking, , 15, 10
        OpenLeftGate
        If Mission(CurrentPlayer) = 10 Then CheckMission
    End If
End Sub

'*******************
' Medal droptargets
'*******************

Sub Target014_Hit 'right
    RandomSoundDropTargetReset Target014
    If Tilted Then Exit Sub
    PlaySound "sc_droptarget"
    MedalTarget1 = 1
    CheckMedal
    Select Case Mission(CurrentPlayer)
        case 4, 5, 17
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target015_Hit 'center
    RandomSoundDropTargetReset Target015
    If Tilted Then Exit Sub
    PlaySound "sc_droptarget"
    MedalTarget2 = 1
    CheckMedal
    Select Case Mission(CurrentPlayer)
        case 4, 5, 17
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target016_Hit 'left
    RandomSoundDropTargetReset Target016
    If Tilted Then Exit Sub
    PlaySound "sc_droptarget"
    MedalTarget3 = 1
    CheckMedal
    Select Case Mission(CurrentPlayer)
        case 4, 5, 17
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub ResetMedalDT
    PlaySoundAt SoundFXDOF("fx_resetdrop", 119, DOFPulse, DOFcontactors), Target015 ' Outhere
    ' PlaySoundAt "fx_resetdrop", Target015
    PlaySound "sc_topdtreset"
    Target014.IsDropped = 0
    Target015.IsDropped = 0
    Target016.IsDropped = 0
    MedalTarget1 = 0
    MedalTarget2 = 0
    MedalTarget3 = 0
End Sub

Sub CheckMedal
    If MedalTarget1 + MedalTarget2 + MedalTarget3 = 3 Then
        Addscore 1500
        MedalCount = MedalCount + 1
        UpdateMedal
        vpmtimer.addtimer 2000, "ResetMedalDT '"
        Select Case MedalCount
            Case 1 'Level 1 Commendation
                DMD "_", CL(1, "LEVEL 1 COMMENDATION"), "", eNone, eNone, eNone, 1000, True, ""
			    pStatus "LVL 1 COMMENDATION", 3000
				PuPEvent 58
                Addscore 10000
                EnableBallSaver 10
            Case 2
                DMD "_", CL(1, "LEVEL 2 COMMENDATION"), "", eNone, eNone, eNone, 1000, True, ""
			    pStatus "LVL 2 COMMENDATION", 3000
				PuPEvent 59
                Addscore 50000
                EnableBallSaver 20
            Case 3
                DMD "_", CL(1, "LEVEL 3 COMMENDATION"), "", eNone, eNone, eNone, 1000, True, ""
			    pStatus "LVL 3 COMMENDATION", 3000
				PuPEvent 60
                AwardExtraBall
        End Select
    Else
        AddScore 500
    End if
End Sub

Sub UpdateMedal
    Select Case MedalCount
        case 0:li044.State = 0:li045.State = 0:li046.State = 0
        Case 1:li046.State = 1
        Case 2:li045.State = 1
        Case 3:li044.State = 1
    End Select
End Sub

'*********************
' Field X droptargets
'*********************

Sub Target017_Hit 'right
    RandomSoundDropTargetReset Target017
    If Tilted Then Exit Sub
    PlaySound "sc_droptarget"
    xTarget1 = 1
    CheckPFX
    Select Case Mission(CurrentPlayer)
        case 4, 5, 17
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target018_Hit 'center
    RandomSoundDropTargetReset Target018
    If Tilted Then Exit Sub
    PlaySound "sc_droptarget"
    xTarget2 = 1
    CheckPFX
    Select Case Mission(CurrentPlayer)
        case 4, 5, 17
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target019_Hit 'left
    RandomSoundDropTargetReset Target019
    If Tilted Then Exit Sub
    PlaySound "sc_droptarget"
    xTarget3 = 1
    CheckPFX
    Select Case Mission(CurrentPlayer)
        case 4, 5, 17
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub ResetxTargets
    PlaySoundAt SoundFXDOF("fx_resetdrop", 117, DOFPulse, DOFcontactors), Target018 ' Outhere
    ' PlaySoundAt "fx_resetdrop", Target018
    PlaySound "sc_topdtreset"
    Target019.IsDropped = 0
    Target018.IsDropped = 0
    Target017.IsDropped = 0
    xTarget1 = 0
    xTarget2 = 0
    xTarget3 = 0
End Sub

Sub CheckPFX
    If xTarget1 + xTarget2 + xTarget3 = 3 Then 'all targets has been hit
        Addscore 1500
        xTargetCount = xTargetCount + 1
        If xTargetCount> 4 then xTargetCount = 4
        UpdatexTargets
        SetPlayfieldX
        pfXTimerExpired.Enabled = 0 're-start the timer to reset the countdown
        pfXTimerExpired.Enabled = 1
        vpmTimer.AddTimer 2000, "ResetxTargets '"
    Else
        Addscore 500
    End If
End Sub

Sub UpdatexTargets
    Select Case xTargetCount
        Case 0:li059.State = 0:li060.State = 0:li061.State = 0:li062.State = 0
        Case 1:li059.State = 1:li060.State = 0:li061.State = 0:li062.State = 0
        Case 2:li059.State = 1:li060.State = 1:li061.State = 0:li062.State = 0
        Case 3:li059.State = 1:li060.State = 1:li061.State = 1:li062.State = 0
        Case 4:li059.State = 1:li060.State = 1:li061.State = 1:li062.State = 1
    End Select
End Sub

Sub SetPlayfieldX
    Select case xTargetCount
        Case 0: '1x
            'DMD "_", CL(1, "PLAYFIELD 1 X"), "", eNone, eNone, eNone, 1500, True, ""
            PlayfieldMultiplier(CurrentPlayer) = 1
            UpdatexTargets
            vpmtimer.AddTimer 500, "ResetxTargets '"
        Case 1: '2x
            'DMD "_", CL(1, "PLAYFIELD 2 X"), "", eNone, eNone, eNone, 2000, True, ""
			PuPEvent 54
			pStatus "2X MULTIPLIER", 2000
            PlayfieldMultiplier(CurrentPlayer) = 2
            UpdatexTargets
            vpmtimer.AddTimer 500, "ResetxTargets '"
        Case 2: '3x
            'DMD "_", CL(1, "PLAYFIELD 3 X"), "", eNone, eNone, eNone, 2000, True, ""
			PuPEvent 55
			pStatus "3X MULTIPLIER", 2000
            PlayfieldMultiplier(CurrentPlayer) = 3
            UpdatexTargets
            vpmtimer.AddTimer 500, "ResetxTargets '"
        Case 3: '5x
            'DMD "_", CL(1, "PLAYFIELD 5 X"), "", eNone, eNone, eNone, 2000, True, ""
			PuPEvent 56
			pStatus "5X MULTIPLIER", 2000
            PlayfieldMultiplier(CurrentPlayer) = 5
            UpdatexTargets
            vpmtimer.AddTimer 500, "ResetxTargets '"
        Case 4: '10x
            'DMD "_", CL(1, "PLAYFIELD 10 X"), "", eNone, eNone, eNone, 3000, True, "v"
			PuPEvent 57
			pStatus "10X MULTIPLIER", 2000
            PlayfieldMultiplier(CurrentPlayer) = 10
            UpdatexTargets
            vpmtimer.AddTimer 500, "ResetxTargets '"
    End Select
End Sub

Sub pfXTimerExpired_Timer
    xTargetCount = xTargetCount - 1
    If xTargetCount <= 0 Then
        xTargetCount = 0
        Me.Enabled = 0
    End If
    SetPlayfieldX
End Sub

'**********************
' Booster droptargets
'***********************

Sub Target020_Hit 'lower
    RandomSoundDropTargetReset Target020
    If Tilted Then Exit Sub
    PlaySound "sc_droptarget"
    BoosterTarget1 = 1
    CheckBooster
    Select Case Mission(CurrentPlayer)
        case 4, 5, 17
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target021_Hit 'center
    RandomSoundDropTargetReset Target021
    If Tilted Then Exit Sub
    PlaySound "sc_droptarget"
    BoosterTarget2 = 1
    CheckBooster
    Select Case Mission(CurrentPlayer)
        case 4, 5, 17
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub Target022_Hit 'upper
    RandomSoundDropTargetReset Target022
    If Tilted Then Exit Sub
    PlaySound "sc_droptarget"
    BoosterTarget3 = 1
    CheckBooster
    Select Case Mission(CurrentPlayer)
        case 4, 5, 17
            MissionHits = MissionHits-1:CheckMission
    End Select
End Sub

Sub ResetBoosterTargets
    PlaySoundAt SoundFXDOF("fx_resetdrop", 120, DOFPulse, DOFcontactors), Target021 ' Outhere
    ' PlaySoundAt "fx_resetdrop", Target021
    PlaySound "sc_rightdtreset"
    Target020.isDropped = 0
    Target021.isDropped = 0
    Target022.isDropped = 0
    BoosterTarget1 = 0
    BoosterTarget2 = 0
    BoosterTarget3 = 0
End Sub

Sub CheckBooster
    If BoosterTarget1 + BoosterTarget2 + BoosterTarget3 = 3 Then 'all targets has been hit
        Addscore 5000
        BoosterCount = BoosterCount + 1
        vpmTimer.AddTimer 2000, "ResetBoosterTargets '"
        If BoosterCount> 4 Then
            BoosterCount = 4
            Exit Sub
        End If
        UpdateBoosterLights
        BoosterTimerExpired.Enabled = 0 'stop and start the timer to reset the count
        BoosterTimerExpired.Enabled = 1
        Select case BoosterCount
            Case 1: 'Upgrade Flags
				PupEvent 68
				pStatus "FLAGS BOOSTED", 1500
                DMD "_", CL(1, "FLAGS UPGRADED"), "", eNone, eNone, eNone, 1500, True, ""
                bFlagsUpgraded = True
                li055.State = 1
                li048.State = 1
                If Mission(CurrentPlayer) = 6 Then CheckMission
            Case 2: 'Activate Jackpot
				PupEvent 65
				pStatus "JACKPOT ACTIVATED", 2000
                DMD "_", CL(1, "JACKPOT ACTIVATED"), "", eNone, eNone, eNone, 1500, True, ""
                bJackpotActivated = True
            Case 3: 'Activate Bonus at the Bonus lane
				PupEvent 66
				pStatus "BONUS ACTIVATED", 2000
                DMD "_", CL(1, "BONUS ACTIVATED"), "", eNone, eNone, eNone, 1500, True, ""
                bBonusActivated = True
                li034.State = 1
            Case 4: 'Bonus Hold
				PupEvent 67
			pStatus "BONUS HOLD ACTIVATED", 2000
                DMD "_", "BONUS HOLD ACTIVATED", "", eNone, eNone, eNone, 1500, True, ""
                bBonusHeld = True
        End Select
    Else
        Addscore 500
    End If
End Sub

Sub UpdateBoosterLights
    Select Case BoosterCount
        Case 0:li040.State = 0:li039.State = 0:li038.State = 0:li037.State = 0
        Case 1:li040.State = 1:li039.State = 0:li038.State = 0:li037.State = 0
        Case 2:li040.State = 1:li039.State = 1:li038.State = 0:li037.State = 0
        Case 3:li040.State = 1:li039.State = 1:li038.State = 1:li037.State = 0
        Case 4:li040.State = 1:li039.State = 1:li038.State = 1:li037.State = 1
    End Select
End Sub

Sub BoosterTimerExpired_Timer
    ' Stop current mode
    Select Case BoosterCount
        Case 1: 'flags
            bFlagsUpgraded = False
            li055.State = 0
            li048.State = 0
        Case 2: 'Jackpot
            bJackpotActivated = False
        Case 3: 'Bonus
            bBonusActivated = false
            li034.State = 0
        Case 4: 'Bonus Hold
    'bBonusHeld = False 'do not reset it as it will be active when the ball drains
    End Select
    If BoosterCount> 0 Then
        BoosterCount = BoosterCount -1
        UpdateBoosterLights
    Else
        BoosterTimerExpired.Enabled = 0
    End If
End Sub

'**********
' Spinners
'**********

Sub Spinner001_Spin 'left
    DOF 133, 2      ' Outhere
	SoundSpinner Spinner001
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    If bFlagsUpgraded Then
        Addscore 2500
    Else
        Addscore 500
    End If
    ChangeArrowColor
    Select Case Mission(CurrentPlayer)
        Case 12
            MissionHits = MissionHits-1:CheckMission
        Case 175
            CheckMission
    End Select
End Sub

Sub Spinner002_Spin 'right
    DOF 133, 2      ' Outhere
	SoundSpinner  Spinner002
    If Tilted Then Exit Sub
    PlaySound "sc_spinner"
    If bFlagsUpgraded Then
        Addscore 2500
    Else
        Addscore 500
    End If
    ChangeArrowColor
    Select Case Mission(CurrentPlayer)
        Case 12
            MissionHits = MissionHits-1:CheckMission
        Case 175
            CheckMission
    End Select
End Sub

'************
' Black Hole
'************

Sub BlackHole_Hit
    SoundSaucerLock
    If Not Tilted Then
    Addscore 20000
    If Mission(CurrentPlayer) = 111 Then
        CheckMission
        DMD CL(0, "BLACK HOLE"), CL(1, "20000"), "", eNone, eNone, eNone, 1500, True, ""
    Else
        DMD CL(0, "BLACK HOLE"), CL(1, "20000"), "", eNone, eNone, eNone, 1500, True, "sc_blackhole Hole"
    End If
    End if
    ' Nothing left to do, so kick out the ball
    vpmtimer.addtimer 1500, "PlaySoundAt""fx_kicker"",BlackHole: BlackHole.kick 48+RND*6, 30:DOF 129,2 '" ' Outhere
End Sub

Sub BlackHole_UnHit
    SoundSaucerKick 1, BlackHole
End Sub

'******************
' Hyper Space Hole
'******************

Sub HyperSpaceHole_Hit
    SoundSaucerLock
    If Not Tilted Then
        HyperSpaceHits = HyperSpaceHits + 1
        HyperExpiredTimer.Enabled = 0 'stop and start the timer to reduce the hyperscape hits
        HyperExpiredTimer.Enabled = 1
        Select Case HyperSpaceHits
            Case 1
                DMD CL(0, "HYPER SPACE 1"), CL(1, "10000"), "", eNone, eBlink, eNone, 1500, True, "sc_hyperspace1"
                Addscore 10000
            Case 2
                DMD CL(0, "HYPER SPACE 2"), CL(1, "20000"), "", eNone, eBlink, eNone, 1500, True, "sc_hyperspace2"
                Addscore 20000
            Case 3
                DMD CL(0, "HYPER SPACE 3"), CL(1, "20000"), "", eNone, eBlink, eNone, 1500, True, "sc_hyperspace1"
                Addscore 20000:RisePost
            Case 4
                DMD CL(0, "HYPER SPACE 4"), CL(1, "50000"), "", eNone, eBlink, eNone, 1500, True, "sc_hyperspace4"
                Addscore 50000:li032.State = 1:li036.State = 1:
				PuPEvent 71
		        pStatus "EXTRA BALL AVAILABLE", 1000
                DMD "", "EXTRA BALL AVAILABLE", "", eNone, eNone, eNone, 1500, True, ""
            Case 5
                DMD CL(0, "GRAVITY WELL"), CL(1, "150000"), "", eNone, eBlink, eNone, 1500, True, "sc_gravitywell"
                Addscore 150000:EnableGravity:HyperSpaceHits = 0
        End Select
        UpdateHyperLights
        If bJackpotActivated Then
            AwardJackpot
        End if
        If bReflexHyperShot Then
            AwardReflexHyperShot
        Else
            StartReflexHyperShot
        End If
        Select Case Mission(CurrentPlayer)
            Case 61, 91, 177:CheckMission
            Case 161:CheckMission
			Case 19:CheckMission
        End Select
    End If
    ' Nothing left to do, so kick out the ball
    vpmtimer.addtimer 3000, "PlaySoundAt""fx_kicker"",HyperSpaceHole: PlaySound""sc_hyperspaceshootout"":HyperSpaceHole.kick 135, 30:DOF 130,2 '" ' Outhere
End Sub

Sub HyperSpaceHole_UnHit
SoundSaucerKick 1, HyperSpaceHole
End Sub




Sub UpdateHyperLights
    Select case HyperSpaceHits
        Case 0:li119.State = 0:li120.State = 0:li121.State = 0:li122.State = 0
        Case 1:li119.State = 1:li120.State = 0:li121.State = 0:li122.State = 0
        Case 2:li119.State = 1:li120.State = 1:li121.State = 0:li122.State = 0
        Case 3:li119.State = 1:li120.State = 1:li121.State = 1:li122.State = 0
        Case 4:li119.State = 1:li120.State = 1:li121.State = 1:li122.State = 1
    End Select
End Sub

Sub HyperExpiredTimer_Timer
    HyperSpaceHits = HyperSpaceHits - 1
    If HyperSpaceHits = 0 Then
        HyperExpiredTimer.Enabled = 0
    End If
    UpdateHyperLights
End Sub

'************
' Wormholes
'************

Sub Wormhole1_Hit
    PlaySoundAt "fx_hole_enter", Wormhole1
    Wormhole1.Destroyball
    BallsOnPlayfield = BallsOnPlayfield - 1
    BallsinWormHole = BallsinWormHole + 1
    Flashforms li142, 500, 50, 2
    PlaySound "sc_wormhole"
    ' Score some points
    Select Case WormHoleColor
        Case 0 'wormhole no activated
            Addscore 2000
            kickWormhole 1 '"
        Case 1 'same color then lock the ball  or award extraball if in multiball mode
            Addscore 5000
            If bMultiBallMode Then
                AwardExtraball
                kickWormhole 1 '"
            Else
                vpmtimer.addtimer 300, "LockBall '"
            End If
        Case 2, 3 'kick the ball from the other hole
            Addscore 7500
            kickWormhole WormHoleColor '"
    End Select
    Select Case Mission(CurrentPlayer)
        Case 8, 18, 101, 176
            CheckMission
    End Select
End Sub

Sub Wormhole2_Hit
    PlaySoundAt "fx_hole_enter", Wormhole2
    Wormhole2.Destroyball
    BallsOnPlayfield = BallsOnPlayfield - 1
    BallsinWormHole = BallsinWormHole + 1
    Flashforms li143, 500, 50, 2
    PlaySound "sc_wormhole"
    ' Score some points
    Select Case WormHoleColor
        Case 0 'wormhole no activated
            Addscore 2000
            kickWormhole 2 '"
        Case 2 'same color then lock the ball  or award extraball if in multiball mode
            Addscore 5000
            If bMultiBallMode Then
                AwardExtraball
                kickWormhole 2 '"
            Else
                vpmtimer.addtimer 300, "LockBall '"
            End If
        Case 1, 3 'kick the ball from the other holes
            Addscore 7500
            kickWormhole WormHoleColor '"
    End Select
    Select Case Mission(CurrentPlayer)
        Case 81, 181, 101, 176
            CheckMission
    End Select
End Sub

Sub Wormhole3_Hit
    PlaySoundAt "fx_hole_enter", Wormhole3
    Wormhole3.Destroyball
    BallsOnPlayfield = BallsOnPlayfield - 1
    BallsinWormHole = BallsinWormHole + 1
    Flashforms li144, 500, 50, 2
    PlaySound "sc_wormhole"
    ' Score some points
    Select Case WormHoleColor
        Case 0 'wormhole no activated
            Addscore 2000
            kickWormhole 3 '"
        Case 3 'same color then lock the ball or award extraball if in multiball mode
            Addscore 5000
            If bMultiBallMode Then
                AwardExtraball
                kickWormhole 3 '"
            Else
                vpmtimer.addtimer 300, "LockBall '"
            End If
        Case 1, 2 'kick the ball from the other holes
            Addscore 7500
            kickWormhole WormHoleColor '"
    End Select
    Select Case Mission(CurrentPlayer)
        Case  101, 176, 182
            CheckMission
    End Select
End Sub

Sub LockBall
    ' remove the ball from the wormhole count as new ball will be kicked out from the wormholes or to the plunger
    BallsinWormHole = BallsinWormHole - 1
    ' add the ball to the lock
    BallsInLock(CurrentPlayer) = BallsInLock(CurrentPlayer) + 1
	playsound "vo_Balllocked" &BallsInLock(CurrentPlayer):DOF 134, 2
    DMD "", CL(1, "BALL " &BallsInLock(CurrentPlayer) & " LOCKED"), "", eNone, eBlinkFast, eNone, 2000, True, "" ' Outhere
    If BallsInLock(CurrentPlayer) = 3 Then
        vpmtimer.addtimer 1500, "StartWormholeMultiball '"
    Else
        ' we use the Addmultiball to create a ball at the plunger lane
        vpmtimer.addtimer 500, "AddMultiball 1 '"
        ' we kick the ball with the autoplunger
        bAutoPlunger = True
        ' turn off the wormholes
        TurnOffWormholes
    End If
End Sub

Sub kickWormhole(hole) 'kicks the ball out in the color wormhole
	dim oldlightstate
    CurrentWormhole = hole
    If BallsinWormHole > 0 Then
        BallsinWormHole = BallsinWormHole - 1
        BallsOnPlayfield = BallsOnPlayfield + 1
        Select case hole
            Case 1   
				oldlightstate = li142.state				 									'yellow
                vpmtimer.addtimer 1501, "PlaySoundAt SoundFXDOF(""fx_popper"", 114, DOFPulse, DOFcontactors), WormHole1 '" ' Outhere
                Flashforms li142, 1500, 50, 2
                vpmtimer.addtimer 1500, "Wormhole1.CreateSizedBallWithMass BallSize / 2, BallMass '"
                vpmtimer.addtimer 1501, "Wormhole1.kick 220, 20 '"
				If (WormholeColor = 0) Then
					vpmtimer.addtimer 1502, "li142.state=0 '"
				Else
					vpmtimer.addtimer 1502, "li142.state=1 '"
				end If
            Case 2      
				oldlightstate = li142.state	                                                 'red
                vpmtimer.addtimer 1501, "PlaySoundAt SoundFXDOF(""fx_popper"", 115, DOFPulse, DOFcontactors), WormHole2 '" ' Outhere
                Flashforms li143, 1500, 50, oldlightstate
                vpmtimer.addtimer 1500, "Wormhole2.CreateSizedBallWithMass BallSize / 2, BallMass '"
                vpmtimer.addtimer 1501, "Wormhole2.kick 220, 20 '"
				If (WormholeColor = 0) Then
					vpmtimer.addtimer 1502, "li143.state=0 '"
				Else
					vpmtimer.addtimer 1502, "li143.state=1 '"
				end If
            Case 3    
				oldlightstate = li142.state	                                                 'green
                vpmtimer.addtimer 1501, "PlaySoundAt SoundFXDOF(""fx_popper"", 116, DOFPulse, DOFcontactors), WormHole3 '" ' Outhere
                Flashforms li144, 1500, 50, oldlightstate
                vpmtimer.addtimer 1500, "Wormhole3.CreateSizedBallWithMass BallSize / 2, BallMass '"
                vpmtimer.addtimer 1501, "Wormhole3.kick 307, 20 '"
				If (WormholeColor = 0) Then
					vpmtimer.addtimer 1502, "li144.state=0 '"
				Else
					vpmtimer.addtimer 1502, "li144.state=1 '"
				end If
        End Select
		vpmtimer.addtimer 1501, "PlaySound ""sc_wormhole"" '"
        vpmtimer.addtimer 1500, "kickWormhole CurrentWormhole '" 'kick out the rest of the balls, if any
    End If
End Sub

Sub TurnOffWormholes
    WormHoleColor = 0
    li047.State = 0
    li142.State = 0
    li143.State = 0
    li144.State = 0
    li080.State = 0
    li081.State = 0
    li082.State = 0
End Sub

Sub StartWormholeMultiball
    PuPEvent 40
	pStatus "XARA MULTIBALL", 6000
    DMD CL(0, "XARA"), CL(1, "MULTIBALL"), "", eNone, eNone, eNone, 5000, True, "sc_wormhole"
    WormholeMBStep = 0
    BallsInLock(CurrentPlayer) = 0
    WormHoleMultiball.Enabled = 1
    AsteroidsTimer.Enabled = 1
    TurnOffWormholes
    If MusicMode = 0 Then
        PlaySong "mu_multiball"  ' Play default main music
    ElseIf MusicMode = 1 Then
        PlaySong "mu_GE_Multiball"  ' Play GE music
    End If
End Sub

Sub AsteroidsTimer_Timer
    Select Case BallsOnPlayfield
        Case 1:li049.State = 0:li050.State = 0:li051.State = 2
        Case 2:li049.State = 0:li050.State = 2:li051.State = 2
        Case 3:li049.State = 2:li050.State = 2:li051.State = 2
    End Select
End Sub

Sub WormHoleMultiball_Timer
    Select case WormholeMBStep
        Case 0
            PlaySoundAt SoundFXDOF("fx_popper", 114, DOFPulse, DOFcontactors), WormHole1 ' Outhere
            Flashforms li142, 500, 50, 2
            Wormhole1.CreateSizedBallWithMass BallSize / 2, BallMass
            BallsOnPlayfield = BallsOnPlayfield + 1
            Wormhole1.kick 220, 20
            WormholeMBStep = 1
            EnableBallSaver 20
        Case 1
            PlaySoundAt SoundFXDOF("fx_popper", 115, DOFPulse, DOFcontactors), WormHole2 ' Outhere
            Flashforms li143, 500, 50, 2
            Wormhole2.CreateSizedBallWithMass BallSize / 2, BallMass
            BallsOnPlayfield = BallsOnPlayfield + 1
            Wormhole2.kick 220, 20
            DOF 143, DOFPulse
            bMultiBallMode = True
            WormholeMBStep = 2
        Case 2
            PlaySoundAt SoundFXDOF("fx_popper", 116, DOFPulse, DOFcontactors), WormHole3 ' Outhere
            Flashforms li144, 1500, 50, 2
            Wormhole3.CreateSizedBallWithMass BallSize / 2, BallMass
            BallsOnPlayfield = BallsOnPlayfield + 1
            Wormhole3.kick 312, 20
            WormHoleMultiball.Enabled = 0
    End Select
End Sub

'***********************
' Left & Right KickBacks
'***********************

Sub LeftKickerRest_Hit
    PlaySoundAt "fx_kicker_enter", LeftKickerRest
    FlashForMs li102, 2000, 50, 0
    vpmtimer.addtimer 600, "PlaySound ""sc_wormhole"":LeftKickerIM.AutoFire:DOF 124,2 '" ' Outhere
    vpmtimer.addtimer 1000, "CloseLeftGate '"
End Sub

Sub RightKickerRest_Hit
    PlaySoundAt "fx_kicker_enter", RightKickerRest
    FlashForMs li103, 2000, 50, 0
    vpmtimer.addtimer 600, "PlaySound ""sc_wormhole"":RightKickerIM.AutoFire:DOF 128,2 '" ' Outhere
    vpmtimer.addtimer 1000, "CloseRightGate '"
End Sub

Sub OpenLeftGate
    PlaySoundAt "fx_diverter", LeftLaneGate
    LeftLaneGate.RotateToEnd
    li072.State = 1
End Sub

Sub CloseLeftGate
    PlaySoundAt "fx_diverter", LeftLaneGate
    LeftLaneGate.RotateToStart
    li072.State = 0
End Sub

Sub OpenRightGate
    PlaySoundAt "fx_diverter", RightLaneGate
    RightLaneGate.RotateToEnd
    li095.State = 1
End Sub

Sub CloseRightGate
    PlaySoundAt "fx_diverter", RightLaneGate
    RightLaneGate.RotateToStart
    li095.State = 0
End Sub

'*****************
' Gravity Magnet
'*****************

Sub GravityCenter_Hit 'the ball rests on this target, so kick the ball upwards
    If NOT Tilted Then
        If aMagnet.MagnetON Then
            Addscore 50000
            'deactivate magnet
            vpmtimer.Addtimer 2000, "GravityReleaseBall '"
        End If
    End If
End Sub

Sub EnableGravity 'aMagnet On. After 5 hits to the Hyper Space hole
    If NOT Tilted Then
		PupEvent 72
		pStatus "GRAVITY MALFUNCTION", 1000
        DMD "_", CL(1, "GRAVITY WELL"), "", eNone, eNone, eNone, 1500, True, ""
        aMagnet.MagnetON = True
        li002.State = 2
    End If
End Sub

Sub DisableGravity
    li002.State = 0
    aMagnet.MagnetOn = False
End Sub

Sub GravityReleaseBall 'aMagnet off and kick the ball if any
    Dim dir, speed, ball
    For Each ball In aMagnet.Balls
        With ball
            dir = Rnd * 6.28:speed = 25 + Rnd * 5
            .VelX = speed * Sin(dir)
            .VelY = - speed * ABS(Cos(dir) ) 'only upwards
        End With
    Next
	PuPEvent 73
	pStatus "GRAVITY NORMALIZED", 1000
    DMD "_", CL(1, "GRAVITY NORMALIZED"), "", eNone, eNone, eNone, 1500, True, ""
    DisableGravity
End Sub

'*****************
' Victory Magnet
'*****************

Sub VictoryCenter_Hit 'The ball rests on this target, so kick the ball upwards
    Fuel = 6
    UpdateFuelLights
    StartFuelTimers

    If bMagnet.MagnetON Then
        Addscore 500000
        
        ' Use the existing allComplete condition to set the timer
		If mli019.State = 1 Then
            vpmtimer.Addtimer 44000, "VictoryReleaseBall '"
			BallsRemaining(CurrentPlayer) = 0 
			ExtraBallsAwards(CurrentPlayer) = 0 
        Else
            vpmtimer.Addtimer 81000, "VictoryReleaseBall '"
        End If
    End If
End Sub



Sub EnableVictory 'bMagnet On. After Maelstrom Complete
        bMagnet.MagnetON = True
        li002.State = 2
		Playsong ""
		Fienna3.visible=1
End Sub

Sub DisableVictory
    li002.State = 0
    bMagnet.MagnetOn = False
	Fuel = 6
	UpdateFuelLights
	StartFuelTimers
End Sub


Sub VictoryReleaseBall ' bMagnet off and kick the ball if any
    Dim dir, speed, ball
    For Each ball In bMagnet.Balls
        With ball
            dir = Rnd * 6.28
            speed = 25 + Rnd * 5
            .VelX = speed * Sin(dir)
            .VelY = -speed * ABS(Cos(dir)) ' Only upwards
        End With
    Next
    DisableVictory
    Fienna3.visible = 0
    PupEvent 251

    ' Stop any music
    PlaySong ""

    ' If mli019.State = 1, force end of game for this player
    If mli019.State = 1 Then
		PupEvent 253
        Tilted = True ' Force disable flippers & slings
        ' Start Tilt Recovery (handles draining and resets automatically)
        TiltRecoveryTimer.Enabled = True 

    Else
        ' Normal game behavior
        If MusicMode = 0 Then
            PlaySong "mu_Main"
        ElseIf MusicMode = 1 Then
            PlaySong "mu_GE_Maelstrom-Triumph"
        End If
    End If
End Sub





'**************
' Flipper Post
'**************

Sub RisePost
    PlaySoundAt "Fx_SolenoidOn", rpeg001
	pStatus "CENTER POST ACTIVATED", 1000
    DMD "_", CL(1, "CENTER POST ACTIVE"), "", eNone, eNone, eNone, 1500, True, "vo_Center post"
    li001.State = 1:li001.TimerEnabled = 1
    rpeg001.Z = 0
    rpin009.IsDropped = 0
    rpin009.TimerEnabled = 1
End Sub

Sub LowerPost
    PlaySoundAt "Fx_SolenoidOff", rpeg001
    li001.TimerEnabled = 0
    li001.State = 0
    rpeg001.Z = -52
    rpin009.IsDropped = 1
    rpin009.TimerEnabled = 0
End Sub

Sub li001_Timer
    li001.TimerEnabled = 0
    li001.State = 2
End Sub

Sub rpin009_Timer '60 seconds
    LowerPost
End Sub

'***********************************
' Reflex Shots: Ramp and Hyper Space
'***********************************

Sub AwardReflexRampShot
	pStatus "REFLEX SHOT", 1000
    DMD "_", CL(1, "REFLEX SHOT"), "", eNone, eNone, eNone, 1500, True, ""
    Addscore 20000
    StopReflexRampShot
End Sub

Sub AwardReflexHyperShot
	pStatus "REFLEX SHOT", 1000
    DMD "_", CL(1, "REFLEX SHOT"), "", eNone, eNone, eNone, 1500, True, ""
    Addscore 20000
    StopReflexHyperShot
End Sub

Sub StartReflexRampShot
    li098.State = 2
    li098.TimerEnabled = 1
    bReflexRampShot = True
End Sub

Sub li098_Timer
    StopReflexRampShot
End Sub

Sub StopReflexRampShot
    li098.State = 0
    li098.TimerEnabled = 0
    bReflexRampShot = False
End Sub

Sub StartReflexHyperShot
    li101.State = 2
    li101.TimerEnabled = 1
    bReflexHyperShot = True
End Sub

Sub li101_Timer
    StopReflexHyperShot
End Sub

Sub StopReflexHyperShot
    li101.State = 0
    li101.TimerEnabled = 0
    bReflexHyperShot = False
End Sub

'**********************
' Rotate lights Timers
'**********************

Sub RotateRankLights_Timer
    Dim tmp
    tmp = li003.State
    For i = 0 to 7
        RankLights(i).State = RankLights(i + 1).State
    Next
    li011.State = tmp
End Sub

Sub RotateProgressLights_Timer
    Dim tmp
    tmp = li029.State
    For i = 17 to 1 step -1
        ProgressLights(i).State = ProgressLights(i-1).State
    Next
    li012.State = tmp
End Sub

'****************
' Progress Lights
'****************
' Collection ProgressLights, 18 Lights
' variables Progress(CurrentPlayer) value 0 to 17

Sub UpdateProgressLights
    For i = 0 to 17
        ProgressLights(i).State = 0
    Next
    If Progress(CurrentPlayer)> 0 Then
        For i = 0 to Progress(CurrentPlayer)
            ProgressLights(i).State = 1
        Next
    End If
End Sub

Sub AddProgress(n)
    Progress(CurrentPlayer) = Progress(CurrentPlayer) + n
    If Progress(CurrentPlayer) >= 17 Then '+ winmission and update rank
        PromoteRank
        Progress(CurrentPlayer) = 0
    End If
    UpdateProgressLights
End Sub

'****************
'    Rank
'****************
' Collection RankLights, 9 Lights
' variables Rank(CurrentPlayer) value 0 to 8

Sub UpdateRankLights
    For i = 0 to 8
        RankLights(i).State = 0
    Next
    For i = 0 to Rank(CurrentPlayer)
        RankLights(i).State = 1
    Next
End Sub

Sub PromoteRank
    PuPEvent 505
    PuPEvent 503
    If Rank(CurrentPlayer) < 9 Then
        LightEffect 4
        Rank(CurrentPlayer) = Rank(CurrentPlayer) + 1
        If Rank(CurrentPlayer) > 8 then Rank(CurrentPlayer) = 8
        UpdateRankLights
        'show DMD new rank
        Select Case Rank(CurrentPlayer)
            Case 0:DMD CL(0, "PROMOTION TO"), CL(1, "CADET"), "", eNone, eNone, eNone, 1500, True, "" 'it should not happen :)
            Case 1:DMD CL(0, "PROMOTION TO"), CL(1, "ENSIGN"), "", eNone, eNone, eNone, 6000, True, ""
					PuPEvent 200
				    pupDMDDisplay "splash4a"," ", "", 6, 0, 10
            Case 2:DMD CL(0, "PROMOTION TO"), CL(1, "LIEUTENANT"), "", eNone, eNone, eNone, 5000, True, ""
					PuPEvent 201
				    pupDMDDisplay "splash4a"," ", "", 6, 0, 10
            Case 3:DMD CL(0, "PROMOTION TO"), CL(1, "LT COMMANDER"), "", eNone, eNone, eNone, 4000, True, ""
	                PuPEvent 202	
				    pupDMDDisplay "splash4a"," ", "", 4, 0, 10
            Case 4:DMD CL(0, "PROMOTION TO"), CL(1, "COMMANDER"), "", eNone, eNone, eNone, 13000, True, ""
	                PuPEvent 203
				    pupDMDDisplay "splash4a"," ", "", 13, 0, 10
            Case 5:DMD CL(0, "PROMOTION TO"), CL(1, "CAPTAIN"), "", eNone, eNone, eNone, 4000, True, ""
	                PuPEvent 204
	                pupDMDDisplay "splash3b","", "", 3, 0, 10
            Case 6:DMD CL(0, "PROMOTION TO"), CL(1, "COMMODORE"), "", eNone, eNone, eNone, 4000, True, ""
	                PuPEvent 205
	                pupDMDDisplay "splash3b","", "", 4, 0, 10
            Case 7:DMD CL(0, "PROMOTION TO"), CL(1, "ADMIRAL"), "", eNone, eNone, eNone, 4000, True, ""
	                PuPEvent 206
	                pupDMDDisplay "splash3b","", "", 4, 0, 10
            Case 8:DMD CL(0, "PROMOTION TO"), CL(1, "FLEET ADMIRAL"), "", eNone, eNone, eNone, 3000, True, ""
	                PuPEvent 207
	                pupDMDDisplay "splash3b","", "", 3, 0, 10
        End Select
    End If
End Sub



'***************
'   MISSIONS
'***************
' variables used:
' MissionSelected = 1 to 4 after a target has been hit
' Mission(CurrentPlayer) = x,  being x the mission number currently active

Sub PrepareForMission 'Called after the skillshot on a new ball or after a mission is completed
    RotateProgressLights.Enabled = 0:UpdateProgressLights
	If mli017.State <> 1 Then
		RotateRankLights.Enabled = 0:UpdateRankLights
	End If
    For Each i In aArrows
        i.State = 0
    Next
    Mission(CurrentPlayer) = 0
    MissionSelected = 0
    li041.State = 0
    li042.State = 0
    li043.State = 0
    li123.State = 0
    li076.State = 2 'Arrow light
    MissionTarget1 = 0
    MissionTarget2 = 0
    MissionTarget3 = 0

    If bMagnet.MagnetON Then
        ' Magnet is active, so stop any music from playing
        PlaySong "" 
    Else
        ' Magnet is not active, proceed with normal music logic
        If bMultiballMode Then
            If MusicMode = 0 Then
                PlaySong "mu_multiball"  ' Play default multiball music
            ElseIf MusicMode = 1 Then
                PlaySong "mu_GE_Multiball"  ' Play GE multiball music
            End If
        Else
            If MusicMode = 0 Then
                PlaySong "mu_Main"  ' Play default main music
            ElseIf MusicMode = 1 Then
                PlaySong "mu_GE_Main"  ' Play GE main music
            End If
        End If
    End If
End Sub

Sub HideAllCompleteFlasher()
    AllCompleteFlasher.Visible = False
    AllCompleteTimer.Enabled = False ' Stop the timer after it runs
End Sub

Sub AllCompleteTimer_Timer()
    HideAllCompleteFlasher
End Sub


Sub CheckMissionTargets(n)
    Dim i, allComplete
    allComplete = True ' Assume all are lit (1) initially
    
    ' Check if all missionstatuslights are 1
    For i = 1 To 18 ' Loop through all 18 lights
        If MissionStatusLightStates(CurrentPlayer, i) = 0 Then
            allComplete = False
            Exit For
        End If
    Next

If allComplete Then
    DMD CL(0, "MISSION SELECTED"), CL(1, "ALL COMPLETE"), "", eNone, eNone, eNone, 1500, True, "" 
    pupDMDDisplay "splash3b","MISSION^SELECTED^ALL COMPLETE", "", 2, 0, 10

    ' Make AllCompleteFlasher visible
    AllCompleteFlasher.Visible = True

    ' Enable the playfield timer to call HideAllCompleteFlasher after 3 seconds
    AllCompleteTimer.Interval = 3000
    AllCompleteTimer.Enabled = True
	Mission(CurrentPlayer) = 19
    
    Exit Sub
End If

    ' a target has been hit, select mission
    ' n is from 1 to 4, 123 are target hits, mission 4 is when all 3 targets are hit
    If MissionTarget1 + MissionTarget2 + MissionTarget3 = 3 Then
        MissionSelected = 4
        ' turn off the targets lights
        li041.State = 0
        li042.State = 0
        li043.State = 0
        LightSeqMission.Play SeqBlinking, , 15, 10
        MissionTarget1 = 0
        MissionTarget2 = 0
        MissionTarget3 = 0
    Else
        MissionSelected = n
        li076.State = 0
        li078.State = 2
        li123.State = 2
    End If


    ' Check if mli017 is ON and select mission based on the lowest unlit light
    If mli017.State = 1 Then
				Rank(CurrentPlayer) = 0
        ' Find the lowest unlit mission light in MissionStatusLights
        Dim availableMission
        availableMission = -1
        
        For i = 1 To 18 ' Loop through all mission lights (adjust the range if necessary)
            If MissionStatusLightStates(CurrentPlayer, i) = 0 Then ' Unlit mission light
                availableMission = i ' Assign the first unlit mission light
                Exit For ' Exit once the first unlit light is found
            End If
        Next
        
        ' If a mission is available, assign it
        If availableMission <> -1 Then
			MissionSelection availableMission
            MissionSelected = availableMission
        End If
    End If

    ' Select Mission
    Select Case Rank(CurrentPlayer)
        Case 0         'Cadet
            Select case MissionSelected
                Case 1: MissionSelection 1  ' Launch Training
                Case 2: MissionSelection 2  ' Re-Entry Training
                Case 3: MissionSelection 3  ' Target Practice
                Case 4: MissionSelection 4  ' Satellite Deployment 'all three targets
            End Select
        Case 1, 2      'Ensign & Lieutenant
            Select case MissionSelected
                Case 1: MissionSelection 5  ' Bug Hunt Mission
                Case 2: MissionSelection 6  ' Rescue Mission
                Case 3: MissionSelection 7  ' Nexus Probe Mission
                Case 4: MissionSelection 8  ' Andromeda Mission
            End Select
        Case 3, 4      'Lieut. Commander & Commander
            Select case MissionSelected
                Case 1: MissionSelection 9  ' Interceptor Mission
                Case 2: MissionSelection 10 ' Destroyer Mission
                Case 3: MissionSelection 11 ' Black Hole Mission
                Case 4: MissionSelection 12 ' Hailing All Fleets Mission
            End Select
        Case 5, 6      'Captain & Commodore
            Select case MissionSelected
                Case 1: MissionSelection 13 ' Satellite Repair Mission
                Case 2: MissionSelection 14 ' Recon Mission
                Case 3: MissionSelection 15 ' Evacuation Mission
                Case 4: MissionSelection 16 ' Security Breach Mission
            End Select
        Case 7      'Admiral
            Select case MissionSelected
                Case 1: MissionSelection 12 ' Hailing All Fleets Mission
                Case 2: MissionSelection 18 ' Time Warp
                Case 3: MissionSelection 16 ' Security Breach Mission
                Case 4: MissionSelection 17 ' Maelstrom
            End Select

        Case 8      'Fleet Admiral
            Select case MissionSelected
                Case 1: MissionSelection 12 ' Hailing All Fleets Mission
                Case 2: MissionSelection 17 ' Maelstrom
                Case 3: MissionSelection 16 ' Security Breach Mission
                Case 4: MissionSelection 18 ' Time Warp
            End Select
    End Select
End Sub

Sub MissionSelection(n)
            Select case n
                case 1 'Launch Training 'top target
                    DMD CL(0, "MISSION SELECTED"), CL(1, "LAUNCH TRAINING"), "", eNone, eNone, eNone, 1500, True, ""
					pupDMDDisplay "splash3b","MISSION^SELECTED^LAUNCH TRAINING", "", 2, 0, 10
                case 2 'Re-entry Training
                    DMD CL(0, "MISSION SELECTED"), CL(1, "RE-ENTRY TRAINING"), "", eNone, eNone, eNone, 1500, True, ""
	                pupDMDDisplay "splash3b","MISSION^SELECTED^RE-ENTRY TRAINING", "", 2, 0, 10
                case 3 'Target Practice
                    DMD CL(0, "MISSION SELECTED"), CL(1, "TARGET PRACTICE"), "", eNone, eNone, eNone, 1500, True, ""
					pupDMDDisplay "splash3b","MISSION^SELECTED^TARGET PRACTICE", "", 2, 0, 10
                case 4 'Satellite Deployment
                    DMD CL(0, "MISSION SELECTED"), CL(1, "SATELLITE DEPLOYMENT"), "", eNone, eNone, eNone, 1500, True, ""
	                pupDMDDisplay "splash3b","MISSION^SELECTED^SATELLITE DEPLOYMENT", "", 2,0, 10
                case 5 'Bug Hunt Mission
                    DMD CL(0, "MISSION SELECTED"), CL(1, "BUG HUNT MISSION"), "", eNone, eNone, eNone, 1500, True, ""
	                pupDMDDisplay "splash3b","MISSION^SELECTED^BUG HUNT", "", 2, 0, 10
                case 6 'Rescue Mission
                    DMD CL(0, "MISSION SELECTED"), CL(1, "RESCUE MISSION"), "", eNone, eNone, eNone, 1500, True, ""
	                pupDMDDisplay "splash3b","MISSION^SELECTED^RESCUE COMRADES", "", 2, 0, 10
                case 7 'Nexus Probe
                    DMD CL(0, "MISSION SELECTED"), CL(1, "NEXUS PROBE"), "", eNone, eNone, eNone, 1500, True, ""
	                pupDMDDisplay "splash3b","MISSION^SELECTED^NEXUS PROBE", "", 2, 0, 10
                case 8 'Andromeda Mission
                    DMD CL(0, "MISSION SELECTED"), CL(1, "ANDROMEDA MISSION"), "", eNone, eNone, eNone, 1500, True, ""
	                pupDMDDisplay "splash3b","MISSION^SELECTED^ANDROMEDA MISSION", "", 2, 0, 10
                case 9 'Interceptor Mission
                    DMD CL(0, "MISSION SELECTED"), CL(1, "DESTROY INTERCEPTOR"), "", eNone, eNone, eNone, 1500, True, ""
	                pupDMDDisplay "splash3b","MISSION^SELECTED^DESTROY INTERCEPTOR", "", 2, 0, 10
                case 10 'Escape Destroyer Mission
                    DMD CL(0, "MISSION SELECTED"), CL(1, "ESCAPE DESTROYER"), "", eNone, eNone, eNone, 1500, True, ""
	                pupDMDDisplay "splash3b","MISSION^SELECTED^ESCAPE DESTROYER", "", 2, 0, 10
                case 11 'Black Hole Mission
                    DMD CL(0, "MISSION SELECTED"), CL(1, "BLACK HOLE"), "", eNone, eNone, eNone, 1500, True, ""
	                pupDMDDisplay "splash3b","MISSION^SELECTED^BLACK HOLE", "", 2, 0, 10
                case 12 'Hail All Fleets Mission
                    DMD CL(0, "MISSION SELECTED"), CL(1, "HAIL ALL FLEETS"), "", eNone, eNone, eNone, 1500, True, ""
	                pupDMDDisplay "splash3b","MISSION^SELECTED^HAIL ALL FLEETS", "", 2, 0, 10
                case 13 'Satellite Repair Mission
                    DMD CL(0, "MISSION SELECTED"), CL(1, "SATELLITE REPAIR"), "", eNone, eNone, eNone, 1500, True, ""
	                pupDMDDisplay "splash3b","MISSION^SELECTED^SATELLITE REPAIR", "", 2, 0, 10
                case 14 'Recon Mission
                    DMD CL(0, "MISSION SELECTED"), CL(1, "RECON MISSION"), "", eNone, eNone, eNone, 1500, True, ""
                case 15 'Evacuation Mission
                    DMD CL(0, "MISSION SELECTED"), CL(1, "EVACUATION"), "", eNone, eNone, eNone, 1500, True, ""
					pupDMDDisplay "splash3b","MISSION^SELECTED^EVACUATION", "", 2, 0, 10
                case 16 'Security Breach
                    DMD CL(0, "MISSION SELECTED"), CL(1, "SECURITY BREACH"), "", eNone, eNone, eNone, 1500, True, ""
	                pupDMDDisplay "splash3b","MISSION^SELECTED^SECURITY BREACH", "", 2, 0, 10
                case 17 'Maelstrom
                    DMD CL(0, "MISSION SELECTED"), CL(1, "MAELSTROM"), "", eNone, eNone, eNone, 1500, True, ""
	                pupDMDDisplay "splash3b","MISSION^SELECTED^MAELSTROM", "", 2, 0, 10
                case 18 'Time Warp
                    DMD CL(0, "?"), CL(1, ""), "", eNone, eNone, eNone, 1500, True, ""
	                pupDMDDisplay "splash3b","?^^", "", 2, 0, 10
            End Select
End Sub

Sub AcceptMission(n)
    For each i in aArrows
        i.State = 0
    Next
    FlashForMs li123, 750, 50, 0
    FlashForMs li096, 750, 50, 0
    li123.State = 1
    ' rotate progress lights during a mission
    RotateProgressLights.Enabled = 1
    RotateRankLights.Enabled = 1
    PlaySong "mu_mission"
    ' increase the base value of the crash bonus if it is still 10000
    If BonusPoints(CurrentPlayer) = 10000 Then BonusPoints(CurrentPlayer) = 25000

    ' a mission has been accepted, select the mission based on the rank,
    Select Case Rank(CurrentPlayer)
        Case 0         'Cadet
            Select case MissionSelected
                Case 1: AssignMission 1  ' Launch Training
                Case 2: AssignMission 2  ' Re-Entry Training
                Case 3: AssignMission 3  ' Target Practice
                Case 4: AssignMission 4  ' Satellite Deployment 'all three targets
            End Select
        Case 1, 2      'Ensign & Lieutenant
            Select case MissionSelected
                Case 1: AssignMission 5  ' Bug Hunt Mission
                Case 2: AssignMission 6  ' Rescue Mission
                Case 3: AssignMission 7  ' Nexus Probe Mission
                Case 4: AssignMission 8  ' Andromeda Mission

            End Select
        Case 3, 4      'Lieut. Commander & Commander
            Select case MissionSelected
                Case 1: AssignMission 9  ' Interceptor Mission
                Case 2: AssignMission 10 ' Destroyer Mission
                Case 3: AssignMission 11 ' Black Hole Mission
                Case 4: AssignMission 12 ' Hailing All Fleets Mission
            End Select
        Case 5, 6      'Captain & Commodore
            Select case MissionSelected
                Case 1: AssignMission 13 ' Satellite Repair Mission
                Case 2: AssignMission 14 ' Recon Mission
                Case 3: AssignMission 15 ' Evacuation Mission
                Case 4: AssignMission 16 ' Security Breach Mission
            End Select
        Case 7      'Admiral
            Select case MissionSelected
                Case 1: AssignMission 12 ' Hailing All Fleets Mission
                Case 2: AssignMission 18 ' Time Warp
                Case 3: AssignMission 16 ' Security Breach Mission
                Case 4: AssignMission 17 ' Maelstrom
            End Select

      Case 8      'Fleet Admiral
            Select case MissionSelected
                Case 1: AssignMission 12 ' Hailing All Fleets Mission
                Case 2: AssignMission 17 ' Maelstrom
                Case 3: AssignMission 16 ' Security Breach Mission
                Case 4: AssignMission 18 ' Time Warp
            End Select
    End Select
End Sub


Sub AssignMission(n)
    For each i in aArrows
        i.State = 0
    Next
    FlashForMs li123, 750, 50, 0
    FlashForMs li096, 750, 50, 0
    li123.State = 1
    ' rotate progress lights during a mission
    RotateProgressLights.Enabled = 1
    RotateRankLights.Enabled = 1
    PlaySong "mu_mission"
    ' increase the base value of the crash bonus if it is still 10000
    If BonusPoints(CurrentPlayer) = 10000 Then BonusPoints(CurrentPlayer) = 25000
        Select case n
				case 1 'Launch Training 'top target
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "LAUNCH TRAINING"), "", eNone, eNone, eNone, 1500, True, ""
                    DMD CL(0, "PASS THE LAUNCH"), CL(1, "RAMP 3 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
					PuPEvent 8
				    pupDMDDisplay "splash4a"," ", "", 3, 0, 10
                    ' Override music only if MusicMode = 1
                    If MusicMode = 1 Then
                        PlaySong "mu_GE_LaunchTraining"
                    End If
                    Mission(CurrentPlayer) = 1
                    MissionHits = 3
                    li078.State = 2
					mli001.State = 2
                    Addscore 10000 '10000 points to accept mission
					Fienna1.visible=1
                case 2             'Re-entry Training 'middle target
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "RE-ENTRY TRAINING"), "", eNone, eNone, eNone, 1500, True, ""
                    DMD CL(0, "PASS THE RE-ENTRY"), CL(1, "LANES 3 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
					PuPEvent 9
				    pupDMDDisplay "splash4a"," ", "", 3, 0, 10
                    If MusicMode = 1 Then
                        PlaySong "mu_GE_ReEntryTraining"
                    End If
                    Mission(CurrentPlayer) = 2
                    MissionHits = 3
                    li091.State = 2
					mli002.State = 2
                    Addscore 10000 '10000 points to accept mission
					Fienna2.visible=1
                case 3             'Target Practice 'lower target
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "TARGET PRACTICE"), "", eNone, eNone, eNone, 1500, True, ""
                    DMD CL(0, "HIT THE ATTACK"), CL(1, "BUMPERS 8 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
					PuPEvent 10
				    pupDMDDisplay "splash4a"," ", "", 3, 0, 10
                    If MusicMode = 1 Then
                        PlaySong "mu_GE_TargetPractice"
                    End If
                    Mission(CurrentPlayer) = 3
                    MissionHits = 8
                    li085.State = 2
					mli003.State = 2
                    Addscore 10000 '10000 points to accept mission
					Decoy.visible=1
					StartFlicker FlasherFienna1, FlasherFienna2, FlasherFienna1a, FlasherFienna2a


                case 4             'Satellite Deployment 'all three targets
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "SATELLITE DEPOLY"), "", eNone, eNone, eNone, 1500, True, ""
                    DMD CL(0, "HIT 9"), CL(1, "DROPTARGETS"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
					PuPEvent 11	
				    pupDMDDisplay "splash4a"," ", "", 3, 0, 10
                    If MusicMode = 1 Then
                        PlaySong "mu_GE_SatelliteDeployment"
                    End If
                    Mission(CurrentPlayer) = 4
                    MissionHits = 9
                    li088.State = 2
                    li079.State = 2
                    li084.State = 2
					mli004.State = 2
                    Addscore 10000 '10000 points to accept mission
                    'reset droptargets
                    ResetMedalDT
                    ResetxTargets
                    ResetBoosterTargets
					Satellite1.visible=1
                case 5 'Bug Hunt Mission
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "BUG HUNT MISSION"), "", eNone, eNone, eNone, 1500, True, ""
                    DMD CL(0, "HIT 15"), CL(1, "TARGETS"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
					PuPEvent 12	
				    pupDMDDisplay "splash4a"," ", "", 3, 0, 10	
                    If MusicMode = 1 Then
                        PlaySong "mu_GE_BugHunt"
                    End If
                    Mission(CurrentPlayer) = 5
                    MissionHits = 15
                    Addscore 20000 '20000 points to accept mission
                    'hit 15 targets, all target lights goes out and droptargets reset
                    'reset droptargets
                    ResetMedalDT
                    ResetxTargets
                    ResetBoosterTargets
                    'turn off target lights
                    li037.State = 0
                    li038.State = 0
                    li039.State = 0
                    li040.State = 0
                    li041.State = 0
                    li042.State = 0
                    li043.State = 0
                    li052.State = 0
                    li053.State = 0
                    li054.State = 0
                    li056.State = 0
                    li057.State = 0
                    li058.State = 0
                    li059.State = 0
                    li060.State = 0
                    li061.State = 0
                    li062.State = 0
                    li047.State = 0
                    'turn on arrow lights
                    li084.State = 2
                    li076.State = 2
                    li079.State = 2
                    li093.State = 2
                    li088.State = 2
                    li086.State = 2
                    li090.State = 2
					Drone1.visible=1
					Drone2.visible=1
					Drone3.visible=1
					Drone4.visible=1
					Drone5.visible=1
					Drone6.visible=1
					mli005.State = 2
                case 6 'Rescue Mission
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "RESCUE MISSION"), "", eNone, eNone, eNone, 1500, True, ""
                    DMD CL(0, "UPGRADE FLAGS +"), CL(1, "ENTER HYPERSPACE"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
					PuPEvent 13	
				    pupDMDDisplay "splash4a"," ", "", 3, 0, 10
                    If MusicMode = 1 Then
                        PlaySong "mu_GE_RescueMission"
                    End If	 
                    Mission(CurrentPlayer) = 6
                    Addscore 20000 '20000 points to accept mission
                    bFlagsUpgraded = False
                    BoosterCount = 0
                    ResetBoosterTargets
                    li079.State = 2
					SOS.visible=1
					mli006.State = 2
                case 7 'Nexus Probe Mission
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "NEXUS PROBE"), "", eNone, eNone, eNone, 1500, True, ""
                    DMD CL(0, "LIGHT ALL"), CL(1, "PHOTON LANES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
                    DMD CL(0, "HIT THE ATTACK"), CL(1, "BUMPERS 8 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
					PuPEvent 14	
				    pupDMDDisplay "splash4a"," ", "", 3, 0, 10
                    If MusicMode = 1 Then
                        PlaySong "mu_GE_NexusProbe"
                    End If	
                    Mission(CurrentPlayer) = 7
                    Addscore 20000 '20000 points to accept mission
                    AttackBumperColor = 0
                    UpdateBumperColor
                    li091.State = 2
				    Probe.visible=1
					mli007.State = 2
                case 8 'Andromeda Mission
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "ANDROMEDA MISSION"), "", eNone, eNone, eNone, 1500, True, ""
                    DMD CL(0, "ENTER 2"), CL(1, "WORMHOLES IN ORDER"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
					PuPEvent 15	
				    pupDMDDisplay "splash4a"," ", "", 4, 0, 10
                    If MusicMode = 1 Then
                        PlaySong "mu_GE_Andromeda"
                    End If	
                    Mission(CurrentPlayer) = 8
                    Addscore 30000 '30000 points to accept mission
                    'prepare the yellow Wormhole
                    li144.State = 0
                    li143.State = 0
                    li142.State = 2
                    li080.State = 2
					mli008.State = 2
                case 9 'Interceptor Mission
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "INTERCEPTOR"), "", eNone, eNone, eNone, 1500, True, ""
                    DMD CL(0, "DISABLE ALL 3"), CL(1, "INTERCEPTOR SHIELDS"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
					PuPEvent 16
				    pupDMDDisplay "splash4a"," ", "", 3, 0, 10
                    If MusicMode = 1 Then
                        PlaySong "mu_GE_DestroyInterceptor"
                    End If	
                    Mission(CurrentPlayer) = 9
                    Addscore 20000 '20000 points to accept mission
                    li093.State = 2
                    li052.State = 0
                    li053.State = 0
                    li054.State = 0
				    Interceptor.visible=1
					mli009.State = 2
                case 10 'Destroyer Mission
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "ESCAPE DESTROYER"), "", eNone, eNone, eNone, 5000, True, ""
                    DMD CL(0, "HIT ALL 3"), CL(1, "DESTROYER LIGHTS"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
					PuPEvent 17
				    pupDMDDisplay "splash4a"," ", "", 3, 0, 10
                    If MusicMode = 1 Then
                        PlaySong "mu_GE_EscapeDestroyer"
                    End If	
                    Mission(CurrentPlayer) = 10
                    Addscore 20000 '20000 points to accept mission
                    li056.State = 0
                    li057.State = 0
                    li058.State = 0
                    li086.State = 2
					Destroyer.visible=1
					mli010.State = 2
                case 11 'Black Hole Mission
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "BLACK HOLE THREAT"), "", eNone, eNone, eNone, 5000, True, ""
                    DMD CL(0, "LIGHT ALL"), CL(1, "ENGINE LANES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
					PuPEvent 18
				    pupDMDDisplay "splash4a"," ", "", 6, 0, 10
                    If MusicMode = 1 Then
                        PlaySong "mu_GE_BlackHole"
                    End If	
                    Mission(CurrentPlayer) = 11
                    Addscore 20000 '20000 points to accept mission
                    EngineBumperColor = 0
                    UpdateBumperColor
                    li078.State = 2
					mli011.State = 2
                case 12 'Hailing All Fleets Mission
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "HAIL ALL FLEETS"), "", eNone, eNone, eNone, 1500, True, ""
                    DMD CL(0, "SPIN FLAGS"), CL(1, "75 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
					PuPEvent 25
				    pupDMDDisplay "splash4a"," ", "", 6, 0, 10
                    If MusicMode = 1 Then
                        PlaySong "mu_GE_CosmicPlague"
                    End If	
                    Mission(CurrentPlayer) = 12
                    MissionHits = 75
                    Addscore 30000 '30000 points to accept mission
                    li077.State = 2
                    li134.State = 2
					Satellite1.visible=1
					mli012.State = 2
                case 13 'Satellite Repair Mission
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "SATELLITE REPAIR"), "", eNone, eNone, eNone, 1500, True, ""
                    DMD CL(0, "HIT 3 TIMES"), "THE SATELLITE BUMPER", "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
					PuPEvent 20
				    pupDMDDisplay "splash4a"," ", "", 6, 0, 10

                    If MusicMode = 1 Then
                        PlaySong "mu_GE_SatelliteRepair"
                    End If	

                    Mission(CurrentPlayer) = 13
                    MissionHits = 3
                    Addscore 20000 '20000 points to accept mission
                    li090.State = 2
					Satellite2.visible=1
					mli013.State = 2
                case 14             'Recon Mission
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "RECON MISSION"), "", eNone, eNone, eNone, 1500, True, ""
                    DMD CL(0, "PASS ANY OF THE"), CL(1, "LANES 15 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 6000, True, ""
					PuPEvent 21
				    pupDMDDisplay "splash4a"," ", "", 6, 0, 10
                    If MusicMode = 1 Then
                        PlaySong "mu_GE_AlienMenace"
                    End If	
                    Mission(CurrentPlayer) = 14
                    MissionHits = 15
                    Addscore 20000 '20000 points to accept mission
                    li091.State = 2
                    li027.State = 2
                    li070.State = 2
                    li069.State = 2
                    li074.State = 2
                    li075.State = 2
					mli014.State = 2
					StartFlicker FlasherNexus1, FlasherNexus2, FlasherNexus1a, FlasherNexus2a

                case 15 'Evacuation Mission
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "EVACUATION"), "", eNone, eNone, eNone, 1500, True, ""
                    DMD CL(0, "SEND BALL 3 TIMES"), CL(1, "THROUGH OUTLINES"), "", eScrollLeft, eScrollLeft, eNone, 7000, True, ""
					PuPEvent 22
				    pupDMDDisplay "splash4a"," ", "", 7, 0, 10
                    If MusicMode = 1 Then
                        PlaySong "mu_GE_DoomsdayMachine"
                    End If	
                    Mission(CurrentPlayer) = 15
                    MissionHits = 3
                    Addscore 20000 '20000 points to accept mission
                    li069.State = 2
                    li075.State = 2
					mli015.State = 2
                case 16 'Security Breach Mission
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "SECURITY BREACH"), "", eNone, eNone, eNone, 1500, True, ""
                    DMD CL(0, "HIT THE REBOUNDS"), CL(1, "25 TIMES"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
                    DMD CL(0, "ENTER LAUNCH RAMP"), "OR HYPERSPACE KICKER", "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
					PuPEvent 23
				    pupDMDDisplay "splash4a"," ", "", 6, 0, 10
                    If MusicMode = 1 Then
                        PlaySong "mu_GE_SecurityBreach"
                    End If	
                    Mission(CurrentPlayer) = 16
                    MissionHits = 25
                    Addscore 30000 '30000 points to accept mission
                    li071.State = 2
                    li073.State = 2
					mli016.State = 2
					StartFlicker FlasherFienna1, FlasherFienna2, FlasherFienna1a, FlasherFienna2a
					Soldier1.visible=1
					Soldier2.visible=1
                case 17 'Maelstrom
                    DMD CL(0, "MISSION ACCEPTED"), CL(1, "MAELSTROM"), "", eNone, eNone, eNone, 1500, True, ""
                    DMD CL(0, "STARTING MAELSTROM"), CL(1, "DESTROY 3 DRONES"), "", eNone, eScrollLeft, eNone, 8000, True, ""
					PuPEvent 26
					pupDMDDisplay "splash","^^DESTROY 3 DRONES", "", 8, 0, 10
                    If MusicMode = 1 Then
                        PlaySong "mu_GE_Maelstrom"
                    End If	
                    Mission(CurrentPlayer) = 17
                    MissionHits = 3
                    Addscore 30000 '30000 points to accept mission
                    li079.State = 2
                    li084.State = 2
                    li088.State = 2
					mli017.State = 2
					Fienna3.visible=1
					Drone1.visible=1
					Drone2.visible=1
					Drone3.visible=1
                case 18 'Time Warp
                    DMD CL(0, "?"), CL(1, ""), "", eNone, eNone, eNone, 1500, True, ""
                    DMD CL(0, "ENTER 3"), CL(1, "WORMHOLES IN ORDER"), "", eScrollLeft, eScrollLeft, eNone, 1500, True, ""
					PuPEvent 24	
					PupEvent 700
				    pupDMDDisplay "splash4a"," ", "", 15, 0, 10
                    If MusicMode = 1 Then
                        PlaySong "mu_Retro-Theme"
                    End If	
                    Mission(CurrentPlayer) = 18
                    Addscore 30000 '30000 points to accept mission
                    'prepare the yellow Wormhole
                    li144.State = 0
                    li143.State = 0
                    li142.State = 2
                    li080.State = 2
					mli018.State = 2
					StartFlicker FlasherTimeWarp1, FlasherTimeWarp2, FlasherTimeWarp1a, FlasherTimeWarp2a
        End Select
End Sub




Dim MissionLookup(18) ' Stores Mission Case per MissionStatusLight

Sub InitializeMissionLookup()
    Dim i
    For i = 1 To 18
        MissionLookup(i) = ((i - 1) Mod 4) + 1 ' Cycle through cases 1-4
    Next
End Sub




' MissionStatusLightStates for each player and each mission
Dim MissionStatusLightStates(4, 18) ' For 4 players and 18 missions

Sub UpdateMissionStatusLights()
    For i = 0 To 17 ' Loop from 0 to 17 for zero-based array
        If MissionStatusLightStates(CurrentPlayer, i + 1) = 1 Then ' Adjust index for 1-based state array
            MissionStatusLights(i).State = 1 ' Turn on mission light for the current player
        Else
            MissionStatusLights(i).State = 0 ' Turn off the mission light for the current player
        End If
    Next
End Sub


Sub CheckMission                    'check if mission objectives are completed
DMDFlush
PuPEvent 504
    Select Case Mission(CurrentPlayer)
        Case 1                      'Launch Training
            If Missionhits = 0 then 'win the mission
                DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(500000)), "", eNone, eScrollLeft, eNone, 1500, True, ""
                PuPEvent 30
				pupDMDDisplay "splash4a"," ^500,000", "", 3, 0, 10
                Addscore 500000     '500k for finishing the mission
                If mli017.State <> 1 Then 
					AddProgress 6       'add 6 progress lights
				End If
                LightEffect 2
				'mli001.State = 1
				MissionStatusLightStates(CurrentPlayer, 1) = 1 
				UpdateMissionStatusLights ' Update the mission lights for the current player
                PrepareForMission   'ready for the next mission
				ResetMissionObjects


            Else
                PlaySfx
            End If
        Case 2                      'Re-entry Training
            If Missionhits = 0 then 'win the mission
                DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(500000)), "", eNone, eScrollLeft, eNone, 2000, True, ""
                PuPEvent 30
				pupDMDDisplay "splash4a"," ^500,000", "", 3, 0, 10
                Addscore 500000     '500k for finishing the mission
                If mli017.State <> 1 Then 
					AddProgress 6       'add 6 progress lights
				End If
                LightEffect 2
				mli002.State = 1
				MissionStatusLightStates(CurrentPlayer, 2) = 1 
				UpdateMissionStatusLights ' Update the mission lights for the current player
                PrepareForMission   'ready for the next mission
				ResetMissionObjects
            Else
                PlaySfx
            End If
        Case 3                      'Target Practice
            If Missionhits = 0 then 'win the mission
                DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(500000)), "", eNone, eScrollLeft, eNone, 6000, True, ""
                PuPEvent 29
                pupDMDDisplay "splash4a","", "", 6, 0, 10
                Addscore 500000     '500k for finishing the mission
                If mli017.State <> 1 Then 
					AddProgress 6       'add 6 progress lights
				End If
                LightEffect 2
				MissionStatusLightStates(CurrentPlayer, 3) = 1 
				UpdateMissionStatusLights ' Update the mission lights for the current player
                PrepareForMission   'ready for the next mission
				EndFlicker FlasherFienna1, FlasherFienna2, FlasherFienna1a, FlasherFienna2a
				'ResetMissionObjects
				Decoy.visible=0
            Else
                PlaySfx
            End If
        Case 4                      'Satellite Deployment
            If Missionhits = 0 then 'win the mission
                DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(750000)), "", eNone, eScrollLeft, eNone, 2000, True, ""
                PuPEvent 32
                pupDMDDisplay "splash4a"," ^750,000", "",3, 0, 10
                Addscore 750000     '750k for finishing the mission
                If mli017.State <> 1 Then 
					AddProgress 7       'add 7 progress lights
				End If
                LightEffect 2
				mli004.State = 1
				MissionStatusLightStates(CurrentPlayer, 4) = 1 
				UpdateMissionStatusLights ' Update the mission lights for the current player
                PrepareForMission   'ready for the next mission
				ResetMissionObjects
            Else
                PlaySfx
            End If
        Case 5                      'Bug Hunt Mission
            If Missionhits = 0 then 'win the mission
                DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(750000)), "", eNone, eScrollLeft, eNone, 2000, True, ""
                PuPEvent 31
                pupDMDDisplay "splash4a"," ^750,000", "",3, 0, 10
                Addscore 750000     '750k for finishing the mission
                If mli017.State <> 1 Then 
					AddProgress 7       'add 7 progress lights
				End If
                LightEffect 2
				mli005.State = 1
				MissionStatusLightStates(CurrentPlayer, 5) = 1 
				UpdateMissionStatusLights ' Update the mission lights for the current player
                PrepareForMission   'ready for the next mission
				ResetMissionObjects
            Else
                PlaySfx
            End If
        Case 6 'Rescue Mission
            If bFlagsUpgraded Then
				PupEvent 43
                DMD CL(0, "ENTER THE"), CL(1, "HYPERSPACE KICKER"), "", eNone, eNone, eNone, 3000, True, ""
		        pStatus "ENTER HYPERSPACE", 2000
                Mission(CurrentPlayer) = 61
                li079.State = 0
                PlaySfx
                li123.State = 0
				ResetMissionObjects

            End If
        Case 61               'Rescue Mission
            DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(750000)), "", eNone, eScrollLeft, eNone, 2000, True, ""
			PuPEvent 33
            pupDMDDisplay "splash4a"," ^750,000", "",3, 0, 10              
            Addscore 750000   '750k for finishing the mission
                If mli017.State <> 1 Then 
					AddProgress 7       'add 7 progress lights
				End If
            LightEffect 2
			mli006.State = 1
				MissionStatusLightStates(CurrentPlayer, 6) = 1 
				UpdateMissionStatusLights ' Update the mission lights for the current player
            PrepareForMission 'ready for the next mission

        Case 7                'Nexus Probe Mission
            DMD CL(0, "HIT THE ATTACK"), CL(1, "BUMPERS 8X"), "", eNone, eNone, eNone, 1500, True, ""
	        pStatus "HIT ATTACK BUMPERS 8 TIMES", 2000
            Mission(CurrentPlayer) = 71
            PlaySfx
            Missionhits = 8
            li091.State = 0
            li085.State = 2
        case 71                     'Nexus Probe Mission
            If Missionhits = 0 then 'win the mission
                DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(750000)), "", eNone, eScrollLeft, eNone, 1500, True, ""
                PuPEvent 32
                pupDMDDisplay "splash4a"," ^750,000", "",3, 0, 10
                Addscore 750000     '750k for finishing the mission
                If mli017.State <> 1 Then 
					AddProgress 7       'add 7 progress lights
				End If
                LightEffect 2
				mli007.State = 1
				MissionStatusLightStates(CurrentPlayer, 7) = 1 
				UpdateMissionStatusLights ' Update the mission lights for the current player
                PrepareForMission   'ready for the next mission
				ResetMissionObjects
            Else
                PlaySfx
            End If
        Case 8 'Andromeda
            DMD " 1ST TASK COMPLETED", " HIT RED WORMHOLE", "", eNone, eScrollLeft, eNone, 1500, True, ""
            pStatus "HIT THE RED WORMHOLE", 2000
            PlaySfx
            'prepare the red Wormhole
            li144.State = 0
            li143.State = 2
            li142.State = 0
            li080.State = 0
            Mission(CurrentPlayer) = 81
            li081.State = 2
        Case 81
            DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(1500000)), "", eNone, eScrollLeft, eNone, 2000, True, ""
                PuPEvent 32
                pupDMDDisplay "splash4a"," ^", "",4, 0, 10
            Addscore 1500000  '1,5 Mill for finishing the mission
                If mli017.State <> 1 Then 
					AddProgress 10       'add 10 progress lights
				End If
            LightEffect 2
            PrepareForMission 'ready for the next mission
            li082.State = 0
            li144.State = 0
            li143.State = 0
            li142.State = 0
            li081.State = 0
			mli008.State = 1
			MissionStatusLightStates(CurrentPlayer, 8) = 1 
			UpdateMissionStatusLights ' Update the mission lights for the current player
            PrepareForMission 'ready for the next mission
        Case 9 'Interceptor Mission
            DMD "", CL(1, "1ST TASK COMPLETED"), "", eNone, eNone, eNone, 1000, True, ""
            DMD CL(0, "FIRE A"), CL(1, "PHOTON BLAST"), "", eNone, eNone, eNone, 2500, True, ""
				PuPEvent 41
			pStatus "HIT PHOTON SPINNER", 2000
            PlaySfx
            Mission(CurrentPlayer) = 91
            li093.State = 0
            li134.State = 2
        Case 91
            DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(1500000)), "", eNone, eScrollLeft, eNone, 5000, True, ""
                PuPEvent 34              
				pupDMDDisplay "splash4a"," ^1,000,000", "",3, 0, 10
            Addscore 1000000  '1 Mill for finishing the mission
                If mli017.State <> 1 Then 
					AddProgress 8       'add 8 progress lights
				End If
            LightEffect 2
			mli009.State = 1
			MissionStatusLightStates(CurrentPlayer, 9) = 1 
			UpdateMissionStatusLights ' Update the mission lights for the current player
            PrepareForMission 'ready for the next mission
			ResetMissionObjects
        Case 10               'Destroyer Mission
            DMD " 1ST TASK COMPLETED", " ENTER ANY WORMHOLE", "", eNone, eScrollLeft, eNone, 4000, True, ""
                PuPEvent 42
			pStatus "ESCAPE THROUGH WORMHOLE", 2000
            PlaySfx
            Mission(CurrentPlayer) = 101
            li086.State = 0
            li080.State = 2
            li081.State = 2
            li082.State = 2
        Case 101              'Destroyer Mission
            DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(1500000)), "", eNone, eScrollLeft, eNone, 1500, True, ""
                PuPEvent 35
                pupDMDDisplay "splash4a"," ^1,000,000", "",3, 0, 10
            Addscore 1000000  '1 Mill for finishing the mission
                If mli017.State <> 1 Then 
					AddProgress 8       'add 8 progress lights
				End If
            LightEffect 2
			MissionStatusLightStates(CurrentPlayer, 10) = 1 
			UpdateMissionStatusLights ' Update the mission lights for the current player
            PrepareForMission 'ready for the next mission
            li080.State = 0
            li081.State = 0
            li082.State = 0
			mli010.State = 1
            ChangeArrowColor 'in case the wormholes were activated
			ResetMissionObjects
        Case 11              'Black Hole Mission
            DMD " 1ST TASK COMPLETED", "ENTER THE BLACK HOLE", "", eNone, eScrollLeft, eNone, 1500, True, ""
            PlaySfx
            Mission(CurrentPlayer) = 111
            li078.State = 0
            li083.State = 2
        Case 111                    'Black Hole Mission
            DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(1500000)), "", eNone, eScrollLeft, eNone, 2000, True, ""
                PuPEvent 31
                pupDMDDisplay "splash4a"," ^1,000,000", "",3, 0, 10
            Addscore 1000000        '1 Mill for finishing the mission
                If mli017.State <> 1 Then 
					AddProgress 8       'add 8 progress lights
				End If
            LightEffect 2
			mli011.State = 1
			MissionStatusLightStates(CurrentPlayer, 11) = 1 
			UpdateMissionStatusLights ' Update the mission lights for the current player
            PrepareForMission       'ready for the next mission
        Case 12                     'Hail All Fleets
            If Missionhits = 0 then 'win the mission
                DMD " 1ST TASK COMPLETED", "  HIT SPACE WARP", "", eNone, eScrollLeft, eNone, 1500, True, ""
				pStatus "HIT SPACE WARP", 2000
                Mission(CurrentPlayer) = 121
                li077.State = 0
                li134.State = 0
                li087.State = 2
            End If
        Case 121                    'Hail All Fleets
            DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(1750000)), "", eNone, eScrollLeft, eNone, 2000, True, ""
                PuPEvent 39
                pupDMDDisplay "splash4a"," ^1,750,000", "",6, 0, 10
            Addscore 1750000        '1 Mill for finishing the mission
			If Rank(CurrentPlayer) <> 8 Then  
                If mli017.State <> 1 Then 
					AddProgress 11       'add 11 progress lights
				End If
			End If
            LightEffect 2
			mli012.State = 1
			MissionStatusLightStates(CurrentPlayer, 12) = 1 
			UpdateMissionStatusLights ' Update the mission lights for the current player
            PrepareForMission       'ready for the next mission
			ResetMissionObjects
        Case 13                     'Satellite Repair Mission
            If Missionhits = 0 then 'win the mission
                DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(1250000)), "", eNone, eScrollLeft, eNone, 2000, True, ""
                PuPEvent 33
                pupDMDDisplay "splash4a"," ^1,250,000", "",3, 0, 10
                Addscore 1250000    '1,25 Mill for finishing the mission
                If mli017.State <> 1 Then 
					AddProgress 9       'add 9 progress lights
				End If
                LightEffect 2
				mli013.State = 1
				MissionStatusLightStates(CurrentPlayer, 13) = 1 
				UpdateMissionStatusLights ' Update the mission lights for the current player
                PrepareForMission   'ready for the next mission
				ResetMissionObjects
            Else
                PlaySfx
            End If
        Case 14                     'Recon Mission
            If Missionhits = 0 then 'win the mission
                DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(1250000)), "", eNone, eScrollLeft, eNone, 2000, True, ""
                PuPEvent 36
                pupDMDDisplay "splash4a"," ^1,250,000", "",3, 0, 10
                Addscore 1250000    '1,25 Mill for finishing the mission
                If mli017.State <> 1 Then 
					AddProgress 9       'add 9 progress lights
				End If
                LightEffect 2
				mli014.State = 1
				MissionStatusLightStates(CurrentPlayer, 14) = 1 
				UpdateMissionStatusLights ' Update the mission lights for the current player
                PrepareForMission   'ready for the next mission
				'ResetMissionObjects
				EndFlicker FlasherNexus1, FlasherNexus2, FlasherNexus1a, FlasherNexus2a

            Else
                PlaySfx
            End If
        Case 15                     'Evacuation Mission
            If Missionhits = 0 then 'win the mission
                DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(1250000)), "", eNone, eScrollLeft, eNone, 2000, True, ""
                PuPEvent 32
                pupDMDDisplay "splash4a"," ^1,250,000", "",3, 0, 10
                Addscore 1250000    '1,25 Mill for finishing the mission
                If mli017.State <> 1 Then 
					AddProgress 9       'add 9 progress lights
				End If
                LightEffect 2
				mli015.State = 1
				MissionStatusLightStates(CurrentPlayer, 15) = 1 
				UpdateMissionStatusLights ' Update the mission lights for the current player
                PrepareForMission   'ready for the next mission
            Else
                PlaySfx
            End If
        Case 16                     'Security Breach Mission
            If Missionhits = 0 then 'win the mission
                DMD " 1ST TASK COMPLETED", "", "", eNone, eNone, eNone, 1500, True, ""
                DMD "HIT THE LAUNCH RAMP", " OR HYPERSPACE", "", eNone, eNone, eNone, 1500, True, ""
				pStatus "HIT RAMP OR HYPERSPACE", 2000
                PuPEvent 44
                Mission(CurrentPlayer) = 161
                li071.State = 0
                li073.State = 0
                li078.State = 2
                li134.State = 2
                li097.State = 2
                li100.State = 2
				Soldier1.visible=0
				Soldier2.visible=0
            Else
                PlaySfx
            End If
        Case 161             'Security Breach Mission
            DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(2000000)), "", eNone, eScrollLeft, eNone, 2000, True, ""
                PuPEvent 37
                pupDMDDisplay "splash4a"," ^2,000,000", "",6, 0, 10
            Addscore 2000000 '2 Mill for finishing the mission
                If mli017.State <> 1 Then 
					AddProgress 12       'add 12 progress lights
				End If
            LightEffect 2
            li097.State = 0
            li100.State = 0
            li034.State = 0
            li078.State = 0
			mli016.State = 1
			EndFlicker FlasherFienna1, FlasherFienna2, FlasherFienna1a, FlasherFienna2a
			MissionStatusLightStates(CurrentPlayer, 16) = 1 
			UpdateMissionStatusLights ' Update the mission lights for the current player
            PrepareForMission       'ready for the next mission
        Case 17  ' Maelstrom "DROPTARGETS LEFT " & MissionHits
            If Missionhits = 0 then '
                DMD CL(0, "2ND TASK"), "HIT 3 SPOT TARGETS", "", eNone, eScrollLeft, eNone, 1500, True, ""
				pupDMDDisplay "splash"," ^2ND TASK^HIT 3 SPOT TARGETS", "", 3, 0, 10
                li079.State = 0
                li084.State = 0
                li088.State = 0
                Mission(CurrentPlayer) = 171
                Missionhits = 3
                li076.State = 2
                li086.State = 2
                li090.State = 2
                li093.State = 2
				Drone1.visible=0
				Drone2.visible=0
				Drone3.visible=0
				Interceptor.visible=1
				Drone6.visible=1
				Drone4.visible=1
				PupEvent 83
            Else
                PlaySfx
            End If
        Case 171 '"SPOT TARGETS LEFT " & MissionHits
            If Missionhits = 0 then
                DMD CL(0, "3RD TASK"), "HIT 5 LANES", "", eNone, eScrollLeft, eNone, 1500, True, ""
				pupDMDDisplay "splash3a"," ^3RD TASK^HIT 5 LANES", "", 3, 0, 10
                li076.State = 0
                li086.State = 0
                li090.State = 0
                li093.State = 0
                Mission(CurrentPlayer) = 172
                Missionhits = 5
                li091.State = 2
                li027.State = 2
                li070.State = 2
                li069.State = 2
                li074.State = 2
                li075.State = 2
				Interceptor.visible=0
				Drone6.visible=0
				Drone4.visible=0
				PupEvent 84
            Else
                PlaySfx
            End If
        Case 172 '"LANES LEFT " & MissionHits
            If Missionhits = 0 then
                DMD CL(0, "4TH TASK"), " HIT THE FUEL CHUTE", "", eNone, eScrollLeft, eNone, 1500, True, ""
				pupDMDDisplay "splash"," ^4TH TASK^HIT FUEL CHUTE", "", 3, 0, 10
                li091.State = 0
                li027.State = 0
                li070.State = 0
                li069.State = 0
                li074.State = 0
                li075.State = 0
                Mission(CurrentPlayer) = 173
                li077.State = 2
				ShakeHit "Fienna3"
				PupEvent 85
            Else
                PlaySfx
            End If
        Case 173 '" FUEL CHUTE HIT"
            DMD CL(0, " ^ 5TH TASK"), " HIT THE LAUNCH RAMP", "", eNone, eScrollLeft, eNone, 1500, True, ""
			pupDMDDisplay "splash"," ^5TH TASK^HIT LAUNCH RAMP", "", 3, 0, 10
            Mission(CurrentPlayer) = 174
            li077.State = 0
            li078.State = 2
			Destroyer.visible=1
			PupEvent 86

        Case 174 '"LAUNCH RAMP HIT"
            DMD CL(0, "6TH TASK"), " HIT SPINNERS", "", eNone, eScrollLeft, eNone, 1500, True, ""
			pupDMDDisplay "splash"," ^6TH TASK^HIT SPINNERS", "", 3, 0, 10
            Mission(CurrentPlayer) = 175
            li078.State = 0
            li077.State = 2
            li134.State = 2
			Destroyer.visible=0
			PupEvent 87
        Case 175 '"SPINNERS HIT"
            DMD CL(0, "7TH TASK"), "  ENTER A WORMHOLE", "", eNone, eScrollLeft, eNone, 1500, True, ""
			pupDMDDisplay "splash"," ^7TH TASK^ENTER A WORMHOLE", "", 3, 0, 10
            Mission(CurrentPlayer) = 176
            li077.State = 0
            li134.State = 0
            li080.State = 2
            li081.State = 2
            li082.State = 2
			PupEvent 88
        Case 176 '"ENTER A WORMHOLE"
            DMD CL(0, "8TH TASK"), "  ENTER HYPERSPACE", "", eNone, eScrollLeft, eNone, 1500, True, ""
			pupDMDDisplay "splash"," ^8TH TASK^ENTER HYPERSPACE", "", 3, 0, 10
            Mission(CurrentPlayer) = 177
            li080.State = 0
            li081.State = 0
            li082.State = 0
            li134.State = 2
            li099.State = 1
			If Rank(CurrentPlayer) <> 8 Then
				Rank(CurrentPlayer) = 8
				UpdateRankLights
			End If
			PupEvent 89
        Case 177 '"HYPERSPACE ENTERED"
            DMD CL(0, "MAELSTROM COMPLETED"), CL(1, FormatScore(5000000)), "", eNone, eScrollLeft, eNone, 7000, True, ""
            Addscore 5000000 '5 Mill for finishing the mission
            AddProgress 0   'add 12 progress lights
            LightEffect 2         
			pupDMDDisplay "splash4a"," ^", "",83, 0, 10
			mli017.State = 1
			EnableVictory
			PupEvent 250
			MissionStatusLightStates(CurrentPlayer, 17) = 1 
			UpdateMissionStatusLights ' Update the mission lights for the current player
            PrepareForMission 'ready for the next mission

		Case 18 'Time Warp
            DMD " 1ST TASK COMPLETED", " HIT RED WORMHOLE", "", eNone, eScrollLeft, eNone, 1500, True, ""
			PuPEvent 45
            pupDMDDisplay "splash4a"," ^", "",9, 0, 10
            PlaySfx
            'prepare the red Wormhole
            li144.State = 0
            li143.State = 2
            li142.State = 0
            li080.State = 0
            Mission(CurrentPlayer) = 181
            li081.State = 2
        Case 181
            DMD " 2ND TASK COMPLETED", "HIT GREEN WORMHOLE", "", eNone, eScrollLeft, eNone, 1500, True, ""
			PuPEvent 46
			pupDMDDisplay "splash4a"," ^", "",7, 0, 10
            PlaySfx
            'prepare the green Wormhole
            li144.State = 2
            li143.State = 0
            li142.State = 0
            Mission(CurrentPlayer) = 182
            li081.State = 0
            li082.State = 2
        Case 182
            DMD CL(0, "MISSION COMPLETED"), CL(1, FormatScore(1500000)), "", eNone, eScrollLeft, eNone, 2000, True, ""
                PuPEvent 38
				PupEvent 7
                pupDMDDisplay "splash4a"," ^", "",15, 0, 10
            Addscore 1500000  '1,5 Mill for finishing the mission
                If mli017.State <> 1 Then 
					AddProgress 10       'add 10 progress lights
				End If
            LightEffect 2
            PrepareForMission 'ready for the next mission
            li082.State = 0
            li144.State = 0
            li143.State = 0
            li142.State = 0
			mli018.State = 1
			EndFlicker FlasherTimeWarp1, FlasherTimeWarp2, FlasherTimeWarp1a, FlasherTimeWarp2a
			MissionStatusLightStates(CurrentPlayer, 18) = 1 
			UpdateMissionStatusLights ' Update the mission lights for the current player
            PrepareForMission 'ready for the next mission
		Case 19 'Post Credit
            DMD " ", " ", "", eNone, eScrollLeft, eNone, 1500, True, ""
			EnableVictory
			PuPEvent 252
            pupDMDDisplay "splash4a"," ^", "",48, 0, 10
            li134.State = 0
            mli019.State = 1

    End Select
DMDScore()
End Sub


Dim MissionStatusLights
MissionStatusLights = Array(mli001, mli002, mli003, mli004, mli005, mli006, mli007, mli008, mli009, mli010, _
                      mli011, mli012, mli013, mli014, mli015, mli016, mli017, mli018)

Sub StopMissions 'this will stop all missions. Called  at the end of the ball or when the fuel runs out
    If Fuel = 0 Then
        DMD "", CL(1, "MISSION ABORTED"), "", eNone, eNone, eNone, 1500, True, "vo_Mission Aborted"
		pStatus "MISSION ABORTED", 2000
    End If
    RotateProgressLights.Enabled = 0:UpdateProgressLights
    RotateRankLights.Enabled = 0:UpdateRankLights
    for each i in aArrows:i.State = 0:Next
    Mission(CurrentPlayer) = 0
    MissionSelected = 0
    li123.State = 0
    Dim light
    For Each light In MissionStatusLights
        If light.State = 2 Then
            light.State = 0
        End If
    Next
End Sub


Dim FlickerDuration
Dim IsMissionEnding
Dim ActiveFlasher1, ActiveFlasher2, ActiveFlasher1a, ActiveFlasher2a

Sub StartFlicker(flasher1, flasher2, flasher1a, flasher2a)
    ' Reset flicker duration and initialize states
    FlickerDuration = 0
    IsMissionEnding = False

    ' Set the flashers for flickering
    Set ActiveFlasher1 = flasher1
    Set ActiveFlasher2 = flasher2
    Set ActiveFlasher1a = flasher1a
    Set ActiveFlasher2a = flasher2a

    ' Start flicker: Flashers 1 and 2 flicker for 2 seconds, then transition to steady state with 1a and 2a
    ActiveFlasher1.Visible = 1
    ActiveFlasher2.Visible = 1
    ActiveFlasher1a.Visible = 0
    ActiveFlasher2a.Visible = 0

    ' Set timer for flicker effect
    FlickerTimer.Interval = 200
    FlickerTimer.Enabled = True
End Sub

Sub FlickerTimer_Timer()
    FlickerDuration = FlickerDuration + FlickerTimer.Interval

    If IsMissionEnding Then
        ' Flicker during mission end
        ActiveFlasher1.Visible = Not ActiveFlasher1.Visible
        ActiveFlasher2.Visible = Not ActiveFlasher2.Visible

        ' Stop flicker after 2 seconds
        If FlickerDuration >= 2000 Then
            ActiveFlasher1.Visible = 0
            ActiveFlasher2.Visible = 0
            FlickerTimer.Enabled = False
        End If
    Else
        ' Flicker during mission start
        ActiveFlasher1.Visible = Not ActiveFlasher1.Visible
        ActiveFlasher2.Visible = Not ActiveFlasher2.Visible

        ' Transition to steady state after 2 seconds
        If FlickerDuration >= 2000 Then
            ActiveFlasher1.Visible = 0
            ActiveFlasher2.Visible = 0
            ActiveFlasher1a.Visible = 1
            ActiveFlasher2a.Visible = 1
            FlickerTimer.Enabled = False
        End If
    End If
End Sub

Sub EndFlicker(flasher1, flasher2, flasher1a, flasher2a)
    ' Reset flicker duration and indicate mission end
    FlickerDuration = 0
    IsMissionEnding = True

    ' Set the flashers for flickering
    Set ActiveFlasher1 = flasher1
    Set ActiveFlasher2 = flasher2
    Set ActiveFlasher1a = flasher1a
    Set ActiveFlasher2a = flasher2a

    ' Start flicker: Flashers 1 and 2 flicker for 2 seconds, then turn everything off
    ActiveFlasher1.Visible = 1
    ActiveFlasher2.Visible = 1
    ActiveFlasher1a.Visible = 0
    ActiveFlasher2a.Visible = 0

    ' Set timer for flicker effect
    FlickerTimer.Interval = 200
    FlickerTimer.Enabled = True
End Sub



Dim CurrentShakingObject
Dim ShakeAmount

Sub ShakeHit(ObjectName)
    ' Initialize the shake effect based on the object
    If ObjectName = "Fienna3" Then
        Set CurrentShakingObject = Fienna3
    ElseIf ObjectName = "Drone8" Then
        Set CurrentShakingObject = Drone8
    ElseIf ObjectName = "Interceptor" Then
        Set CurrentShakingObject = Interceptor
    Else
        MsgBox "Object '" & ObjectName & "' is not recognized or does not support shaking."
        Exit Sub
    End If

    ' Initialize shake effect
    ShakeAmount = 6
    ShakeTimer.Enabled = True
End Sub

Sub ShakeTimer_Timer()
    ' Check if the object reference is valid
    If CurrentShakingObject Is Nothing Then
        MsgBox "No object assigned to shake!"
        ShakeTimer.Enabled = False
        Exit Sub
    End If

    ' Apply the shake effect
    On Error Resume Next  ' Handle runtime errors gracefully
    CurrentShakingObject.Transy = ShakeAmount / 2
    If Err.Number <> 0 Then
        MsgBox "Error applying shake effect: " & Err.Description
        Err.Clear
        ShakeTimer.Enabled = False
        Exit Sub
    End If

    ' Stop shaking when done
    If ShakeAmount = 0 Then
        ShakeTimer.Enabled = False
        Exit Sub
    End If

    ' Alternate shake direction and reduce intensity
    If ShakeAmount < 0 Then
        ShakeAmount = Abs(ShakeAmount) - 0.1
    Else
        ShakeAmount = -ShakeAmount + 0.1
    End If
End Sub




'********************* START OF PUPDMD FRAMEWORK v1.0 *************************
'******************** DO NOT MODIFY STUFF BELOW   THIS LINE!!!! ***************
'******************************************************************************
'*****   Create a PUPPack within PUPPackEditor for layout config!!!  **********
'******************************************************************************
'


Const HasPuP = True   'dont set to false as it will break pup

Const pTopper=0
Const pDMD=5
Const pBackglass=2
Const pBackground=-1
Const pPlayfield=3
Const pMusic=4
Const pCallouts=6
Const pBackglass2=7
Const pTopper2=8
Const pPopUP=9
Const pPopUP2=10



'pages
Const pDMDBlank=0
Const pScores=1
Const pBigLine=2
Const pThreeLines=3
Const pTwoLines=4
Const pTargerLetters=5

'dmdType
Const pDMDTypeLCD=0
Const pDMDTypeReal=1
Const pDMDTypeFULL=2




Dim PuPlayer
dim PUPDMDObject  'for realtime mirroring.
Dim pDMDlastchk: pDMDLastchk= -1    'performance of updates
Dim pDMDCurPage: pDMDCurPage= 0    'default page is empty.
Dim pInAttract : pInAttract=false   'pAttract mode




'*************  starts PUP system,  must be called AFTER b2s/controller running so put in last line of table1_init
Sub PuPInit

Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")   
PuPlayer.B2SInit "", pGameName

'if (PuPDMDDriverType=pDMDTypeReal) and (useRealDMDScale=1) Then 
'       PuPlayer.setScreenEx pDMD,0,0,128,32,0  'if hardware set the dmd to 128,32
'End if

PuPlayer.LabelInit pDMD


'if PuPDMDDriverType=pDMDTypeReal then
'Set PUPDMDObject = CreateObject("PUPDMDControl.DMD") 
'PUPDMDObject.DMDOpen
'PUPDMDObject.DMDPuPMirror
'PUPDMDObject.DMDPuPTextMirror
'PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 1, ""FN"":33 }"             'set pupdmd for mirror and hide behind other pups
'PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 1, ""FN"":32, ""FQ"":3 }"   'set no antialias on font render if real
'END IF


pSetPageLayouts

pDMDSetPage(pDMDBlank)   'set blank text overlay page.
'pDMDStartUP				 ' firsttime running for like an startup video..
		pAttractStart
	    PupEvent 7 ' Start the backglass
		PuPEvent 2
		'IntroTimer.Enabled=1

End Sub 'end PUPINIT



'PinUP Player DMD Helper Functions

Sub pDMDLabelHide(labName)
PuPlayer.LabelSet pDMD,labName,"",0,""   
end sub




Sub pDMDScrollBig(msgText,timeSec,mColor)
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
end sub

Sub pDMDScrollBigV(msgText,timeSec,mColor)
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'yps':1,'ype':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
end sub


Sub pDMDSplashScore(msgText,timeSec,mColor)
PuPlayer.LabelSet pDMD,"MsgScore",msgText,0,"{'mt':1,'at':1,'fq':250,'len':"& (timeSec*1000) &",'fc':" & mColor & "}"
end Sub

Sub pDMDSplashScoreScroll(msgText,timeSec,mColor)
PuPlayer.LabelSet pDMD,"MsgScore",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':"& (timeSec*1000) &", 'mlen':"& (timeSec*1000) &",'tt':0, 'fc':" & mColor & "}"
end Sub

Sub pDMDZoomBig(msgText,timeSec,mColor)  'new Zoom
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':3,'hstart':5,'hend':80,'len':" & (timeSec*1000) & ",'mlen':" & (timeSec*500) & ",'tt':5,'fc':" & mColor & "}"
end sub

Sub pDMDTargetLettersInfo(msgText,msgInfo, timeSec)  'msgInfo = '0211'  0= layer 1, 1=layer 2, 2=top layer3.
'this function is when you want to hilite spelled words.  Like B O N U S but have O S hilited as already hit markers... see example.
PuPlayer.LabelShowPage pDMD,5,timeSec,""  'show page 5
Dim backText
Dim middleText
Dim flashText
Dim curChar
Dim i
Dim offchars:offchars=0
Dim spaces:spaces=" "  'set this to 1 or more depends on font space width.  only works with certain fonts
                          'if using a fixed font width then set spaces to just one space.

For i=1 To Len(msgInfo)
    curChar="" & Mid(msgInfo,i,1)
    if curChar="0" Then
            backText=backText & Mid(msgText,i,1)
            middleText=middleText & spaces
            flashText=flashText & spaces          
            offchars=offchars+1
    End If
    if curChar="1" Then
            backText=backText & spaces
            middleText=middleText & Mid(msgText,i,1)
            flashText=flashText & spaces
    End If
    if curChar="2" Then
            backText=backText & spaces
            middleText=middleText & spaces
            flashText=flashText & Mid(msgText,i,1)
    End If   
Next 

if offchars=0 Then 'all litup!... flash entire string
   backText=""
   middleText=""
   FlashText=msgText
end if  

PuPlayer.LabelSet pDMD,"Back5"  ,backText  ,1,""
PuPlayer.LabelSet pDMD,"Middle5",middleText,1,""
PuPlayer.LabelSet pDMD,"Flash5" ,flashText ,0,"{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) & "}"   
end Sub


Sub pDMDSetPage(pagenum)    
    PuPlayer.LabelShowPage pDMD,pagenum,0,""   'set page to blank 0 page if want off
    PDMDCurPage=pagenum
end Sub

Sub pHideOverlayText(pDisp)
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& pDisp &", ""FN"": 34 }"             'hideoverlay text during next videoplay on DMD auto return
end Sub



Sub pDMDShowLines3(msgText,msgText2,msgText3,timeSec)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,3,timeSec,""
PuPlayer.LabelSet pDMD,"Splash3a",msgText,vis,pLine1Ani
PuPlayer.LabelSet pDMD,"Splash3b",msgText2,vis,pLine2Ani
PuPlayer.LabelSet pDMD,"Splash3c",msgText3,vis,pLine3Ani
end Sub


Sub pDMDShowLines2(msgText,msgText2,timeSec)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,4,timeSec,""
PuPlayer.LabelSet pDMD,"Splash4a",msgText,vis,pLine1Ani
PuPlayer.LabelSet pDMD,"Splash4b",msgText2,vis,pLine2Ani
end Sub

Sub pDMDShowCounter(msgText,msgText2,msgText3,timeSec)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,6,timeSec,""
PuPlayer.LabelSet pDMD,"Splash6a",msgText,vis, pLine1Ani
PuPlayer.LabelSet pDMD,"Splash6b",msgText2,vis,pLine2Ani
PuPlayer.LabelSet pDMD,"Splash6c",msgText3,vis,pLine3Ani
end Sub


Sub pDMDShowBig(msgText,timeSec, mColor)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,vis,pLine1Ani
end sub


Sub pDMDShowHS(msgText,msgText2,msgText3,timeSec) 'High Score
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,7,timeSec,""
PuPlayer.LabelSet pDMD,"Splash7a",msgText,vis,pLine1Ani
PuPlayer.LabelSet pDMD,"Splash7b",msgText2,vis,pLine2Ani
PuPlayer.LabelSet pDMD,"Splash7c",msgText3,vis,pLine3Ani
end Sub


Sub pDMDSetBackFrame(fname)
  PuPlayer.playlistplayex pDMD,"PUPFrames",fname,0,1    
end Sub

Sub pDMDStartBackLoop(fPlayList,fname)
  PuPlayer.playlistplayex pDMD,fPlayList,fname,0,1
  PuPlayer.SetBackGround pDMD,1
end Sub

Sub pDMDStopBackLoop
  PuPlayer.SetBackGround pDMD,0
  PuPlayer.playstop pDMD
end Sub


Dim pNumLines

'Theme Colors for Text (not used currenlty,  use the |<colornum> in text labels for colouring.
Dim SpecialInfo
Dim pLine1Color : pLine1Color=2041045  
Dim pLine2Color : pLine2Color=2041045
Dim pLine3Color :  pLine3Color=8454143
Dim curLine1Color: curLine1Color=pLine1Color  'can change later
Dim curLine2Color: curLine2Color=pLine2Color  'can change later
Dim curLine3Color: curLine3Color=pLine3Color  'can change later


Dim pDMDCurPriority: pDMDCurPriority =-1
Dim pDMDDefVolume: pDMDDefVolume = 0   'default no audio on pDMD

Dim pLine1
Dim pLine2
Dim pLine3
Dim pLine1Ani
Dim pLine2Ani
Dim pLine3Ani

Dim PriorityReset:PriorityReset=-1
DIM pAttractReset:pAttractReset=-1
DIM pAttractBetween: pAttractBetween=2000 '1 second between calls to next attract page
DIM pDMDVideoPlaying: pDMDVideoPlaying=false


'************************ where all the MAGIC goes,  pretty much call this everywhere  ****************************************
'*************************                see docs for examples                ************************************************
'****************************************   DONT TOUCH THIS CODE   ************************************************************

Sub pupDMDDisplay(pEventID, pText, VideoName,TimeSec, pAni,pPriority)
' pEventID = reference if application,  
' pText = "text to show" separate lines by ^ in same string
' VideoName "gameover.mp4" will play in background  "@gameover.mp4" will play and disable text during gameplay.
' also global variable useDMDVideos=true/false if user wishes only TEXT
' TimeSec how long to display msg in Seconds
' animation if any 0=none 1=Flasher
' also,  now can specify color of each line (when no animation).  "sometext|12345"  will set label to "sometext" and set color to 12345

DIM curPos
if pDMDCurPriority>pPriority then Exit Sub  'if something is being displayed that we don't want interrupted.  same level will interrupt.
pDMDCurPriority=pPriority
if timeSec=0 then timeSec=1 'don't allow page default page by accident


pLine1=""
pLine2=""
pLine3=""
pLine1Ani=""
pLine2Ani=""
pLine3Ani=""

curPos=InStr(pText,"^")   'Lets break apart the string if needed
if curPos>0 Then 
   pLine1=Left(pText,curPos-1) 
   pText=Right(pText,Len(pText) - curPos)
   
   curPos=InStr(pText,"^")   'Lets break apart the string
   if curPOS>0 Then
      pLine2=Left(pText,curPos-1) 
      pText=Right(pText,Len(pText) - curPos)

      curPos=InStr("^",pText)   'Lets break apart the string   
      if curPos>0 Then
         pline3=Left(pText,curPos-1) 
      Else 
        if pText<>"" Then pline3=pText 
      End if 
   Else 
      if pText<>"" Then pLine2=pText
   End if    
Else 
  pLine1=pText  'just one line with no break 
End if


'lets see how many lines to Show
pNumLines=0
if pLine1<>"" then pNumLines=pNumlines+1
if pLine2<>"" then pNumLines=pNumlines+1
if pLine3<>"" then pNumLines=pNumlines+1

if pDMDVideoPlaying Then 
			PuPlayer.playstop pDMD
			pDMDVideoPlaying=False
End if


if (VideoName<>"") and (useDMDVideos) Then  'we are showing a splash video instead of the text.
    
    PuPlayer.playlistplayex pDMD,"DMDSplash",VideoName,pDMDDefVolume,pPriority  'should be an attract background (no text is displayed)
    pDMDVideoPlaying=true
end if 'if showing a splash video with no text




if StrComp(pEventID,"shownum",1)=0 Then              'check eventIDs
    pDMDShowCounter pLine1,pLine2,pLine3,timeSec
Elseif StrComp(pEventID,"target",1)=0 Then              'check eventIDs
    pDMDTargetLettersInfo pLine1,pLine2,timeSec
Elseif StrComp(pEventID,"highscore",1)=0 Then              'check eventIDs
    pDMDShowHS pLine1,pLine2,pline3,timeSec
Elseif (pNumLines=3) Then                'depends on # of lines which one to use.  pAni=1 will flash.
    pDMDShowLines3 pLine1,pLine2,pLine3,TimeSec
Elseif (pNumLines=2) Then
    pDMDShowLines2 pLine1,pLine2,TimeSec
Elseif (pNumLines=1) Then
    pDMDShowBig pLine1,timeSec, curLine1Color
Else
    pDMDShowBig pLine1,timeSec, curLine1Color
End if

PriorityReset=TimeSec*1000
End Sub 'pupDMDDisplay message

Sub pupDMDupdate_Timer()
	pUpdateScores

    if PriorityReset>0 Then  'for splashes we need to reset current prioirty on timer
       PriorityReset=PriorityReset-pupDMDUpdate.interval
       if PriorityReset<=0 Then 
            pDMDCurPriority=-1            
            if pInAttract then pAttractReset=pAttractBetween ' pAttractNext  call attract next after 1 second
			pDMDVideoPlaying=false			
			End if
    End if

    if pAttractReset>0 Then  'for splashes we need to reset current prioirty on timer
       pAttractReset=pAttractReset-pupDMDUpdate.interval
       if pAttractReset<=0 Then 
            pAttractReset=-1            
            if pInAttract then pAttractNext
			End if
    end if 
End Sub

Sub PuPEvent(EventNum)
    if hasPUP=false then Exit Sub

    Select Case EventNum
        Case 502
            PlayMusic "./pupvideos/" & pGameName & "/Callouts/mu_GE_Maelstrom-Re-attract.mp3", 0.8
        Case 503
            EndMusic
    End Select

    PuPlayer.B2SData "E"&EventNum,1  'send event to puppack driver  
End Sub


'********************* END OF PUPDMD FRAMEWORK v1.0 *************************
'******************** DO NOT MODIFY STUFF ABOVE THIS LINE!!!! ***************
'****************************************************************************

'*****************************************************************
'   **********  PUPDMD  MODIFY THIS SECTION!!!  ***************
'PUPDMD Layout for each Table1
'Setup Pages.  Note if you use fonts they must be in FONTS folder of the pupVideos\tablename\FONTS  "case sensitive exact naming fonts!"
'*****************************************************************

Sub pSetPageLayouts

DIM dmddef
DIM dmdalt
DIM dmdscr
DIM dmdfixed

DIM dmdscreenscale

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

	dmdalt="Komu A"    
    dmdfixed="Komu A"
	dmdscr="Komu A" 'main score font
	dmddef="Exima Geometric"
    dmdscreenscale = 0.7

	'Page 1 (default score display)
		PuPlayer.LabelNew pDMD,"Credits"    ,dmddef,7 * dmdscreenscale,2041045   ,0,1,2,0,0,1,0
		PuPlayer.LabelNew pDMD,"Playlabel"  ,dmddef,5 * dmdscreenscale,2041045   ,448,1,0,10,22,1,0
		PuPlayer.LabelNew pDMD,"Play1"      ,dmddef,18 * dmdscreenscale,14111599   ,1,1,0,5,0,1,0
		PuPlayer.LabelNew pDMD,"Ball"       ,dmddef,18 * dmdscreenscale,14111599   ,1,1,0,95,0,1,0
		PuPlayer.LabelNew pDMD,"Balllabel"  ,dmddef,5 * dmdscreenscale,2041045   ,3160,1,0,92,15,1,0
        PuPlayer.LabelNew pDMD, "pupStatus" ,dmdscr,20 * dmdscreenscale, 14111599, 0, 1, 0, 0, 65, 1, 0
		PuPlayer.LabelNew pDMD,"CurScore"   ,dmdscr,28 * dmdscreenscale,2530743   ,0,1,0, 0,10,1,0	

	'Page 2 (default Text Splash 1 Big Line)
		PuPlayer.LabelNew pDMD,"Splash"  ,dmdalt,32 * dmdscreenscale,2041045,0,1,1,0,0,2,0

	'Page 3 (default Text 3 Lines)
		PuPlayer.LabelNew pDMD,"Splash3a",dmddef,14 * dmdscreenscale,2041045,0,1,0,0,8,3,0
		PuPlayer.LabelNew pDMD,"Splash3b",dmddef,14 * dmdscreenscale,2041045,0,1,0,0,24,3,0
		PuPlayer.LabelNew pDMD,"Splash3c",dmdalt,18 * dmdscreenscale,14111599,0,1,0,0,65,3,0

	'Page 4 (default Text 2 Line)
		PuPlayer.LabelNew pDMD,"Splash4a",dmddef,16 * dmdscreenscale,2041045,0,1,0,0,22,4,0
		PuPlayer.LabelNew pDMD,"Splash4b",dmdalt,22 * dmdscreenscale,14111599,0,1,2,0,90,4,0

	'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
		PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,20 * dmdscreenscale,8421504,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,20 * dmdscreenscale,65535  ,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,20 * dmdscreenscale,65535  ,0,1,1,0,0,5,0

	'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
		PuPlayer.LabelNew pDMD,"Splash6a",dmddef,18 * dmdscreenscale,65280,0,0,0,15,1,6,0
		PuPlayer.LabelNew pDMD,"Splash6b",dmddef,18 * dmdscreenscale,33023,0,1,0,60,0,6,0
		PuPlayer.LabelNew pDMD,"Splash6c",dmddef,18 * dmdscreenscale,33023,0,1,0,60,50,6,0

	'Page 7 (Show High Scores Fixed Fonts)
		PuPlayer.LabelNew pDMD,"Splash7a",dmddef,20 * dmdscreenscale,2041045,0,1,0,0,8,7,0
		PuPlayer.LabelNew pDMD,"Splash7b",dmdfixed,28 * dmdscreenscale,14111599,0,1,0,0,30,7,0
		PuPlayer.LabelNew pDMD,"Splash7c",dmdfixed,28 * dmdscreenscale,14111599,0,1,0,0,58,7,0

	'Page 8 (High Score Entry)
		PuPlayer.LabelNew pDMD,"HighScore",dmdalt,24 * dmdscreenscale,14111599,0,1,0,0,32,8,0
		PuPlayer.LabelNew pDMD,"HighScore2",dmdalt,22 * dmdscreenscale,14111599,0,1,0,28,50,8,0
		PuPlayer.LabelNew pDMD,"HighScore3",dmddef,16 * dmdscreenscale,2041045,0,1,0,0,16,8,0
		PuPlayer.LabelNew pDMD,"HighScore4",dmdalt,22 * dmdscreenscale,2530743,0,1,0,0,60,8,0

END Sub 'page Layouts



'*****************************************************************
'        PUPDMD Custom SUBS/Events for each Table1
'     **********    MODIFY THIS SECTION!!!  ***************
'*****************************************************************
'
'
'  we need to somewhere in code if applicable
'
'   call pDMDStartGame,pDMDStartBall,pGameOver,pAttractStart
'


Sub pDMDStartGame
pInAttract=False
pDMDSetPage(pScores)   'set blank text overlay page.
    PuPlayer.playlistplayex pCallouts,"Callouts","vo_Gravity Normalized.ogg",0,0
	PuPEvent 503
    PuPEvent 4
    PuPEvent 1


end Sub


Sub pDMDStartBall

end Sub

Sub pDMDGameOver
pAttractStart
end Sub

Sub pAttractStart
pDMDSetPage(pDMDBlank)   'set blank text overlay page
pCurAttractPos=0
pInAttract=True          'Startup in AttractMode
pAttractNext
end Sub

DIM pCurAttractPos: pCurAttractPos=0


'********************** gets called auto each page next and timed already in DMD_Timer.  make sure you use pupDMDDisplay or it wont advance auto.
Sub pAttractNext
pCurAttractPos=pCurAttractPos+1

  Select Case pCurAttractPos

  Case 1
            PuPEvent 502
            If score(currentplayer) > 0 Then    
				pupDMDDisplay "GAMEOVER", " ^Last Score - " & FormatNumber(Score(CurrentPlayer),0), "", 2, 0, 10
				pupDMDDisplay "GAMEOVER", " ^Last Score - " & FormatNumber(Score(CurrentPlayer),0),  "", 2, 0, 10
				PuPEvent 501
				PuPEvent 2	
			Else
				pupDMDDisplay "attract", "", "", 3, 0, 10
				PuPEvent 501				
			end if 
  Case 2    pupDMDDisplay "attract","","",18,0,10 
				PuPEvent 500
  Case 3 
            if Credits = 0 then    
				pupDMDDisplay "attract", "CREDITS 0^INSERT COIN", "", 3, 1, 10
			Else    
				pupDMDDisplay "attract", "CREDITS "&(Credits)&"^PRESS START", "", 3, 0, 10
			End If
  Case 4 pupDMDDisplay "attract", "ROM VERSION "&(myVersion), "", 2, 0, 10

  Case 5 pupDMDDisplay "highscore","LeaderBoard^1. " & HighScoreName(0)& "  " & FormatNumber(HighScore(0), 0) & "^2. " & HighScoreName(1) & "  " & FormatNumber(HighScore(1), 0), "", 3, 0, 10
  Case 6 pupDMDDisplay "highscore","LeaderBoard^3. " & HighScoreName(2)& "  " & FormatNumber(HighScore(2),0)&"^4. " & HighScoreName(3) & "  " & FormatNumber(HighScore(3),0) , "", 3, 0, 10  

  Case 7 pupDMDDisplay "attract","","",51,0,10 
				PuPEvent 503
				PuPEvent 0
				PuPEvent 3

  Case 8 pupDMDDisplay "attract","","",150,0,10 
				PuPEvent 5

  Case Else
    pCurAttractPos=0
    pAttractNext 'reset to beginning
  end Select

end Sub

'************************ called during gameplay to update Scores ***************************
Dim lastScore : lastScore = -1
Dim lastCredits : lastCredits = -1
Dim lastCurrentPlayer : lastCurrentPlayer = -1
Dim lastBalls : lastBalls = -1

Sub pUpdateScores()
	if pDMDCurPage <> pScores then
        lastScore = -1
        lastCredits = -1
        lastCurrentPlayer = -1
        lastBalls = -1
        Exit Sub
    End if

	If (lastScore <> Score(CurrentPlayer)) Then
        lastScore = Score(CurrentPlayer)
        puPlayer.LabelSet pDMD,"CurScore","" & FormatNumber(lastScore,0) ,1,""
    End If

	If (lastCredits <> Credits) Then
        lastCredits = Credits
    	puPlayer.LabelSet pDMD,"Credits","CREDITS:" & ""& lastCredits ,1,""
    End If

	If (lastCurrentPlayer <> CurrentPLayer) Then
        lastCurrentPlayer = CurrentPLayer
    	puPlayer.LabelSet pDMD,"Play1","" & lastCurrentPlayer,1,""
    End If

	If (lastBalls <> balls) Then
        lastBalls = balls
    	puPlayer.LabelSet pDMD,"Ball","" & ""& lastBalls ,1,""
    	puPlayer.LabelSet pDMD,"PlayLabel","Player",1,""
    	puPlayer.LabelSet pDMD,"BallLabel","Ball",1,""
    End if
end Sub



'********************  pretty much only use pupDMDDisplay all over ************************   
' Sub pupDMDDisplay(pEventID, pText, VideoName,TimeSec, pAni,pPriority)
' pEventID = reference if application,  
' pText = "text to show" separate lines by ^ in same string
' VideoName "gameover.mp4" will play in background  "@gameover.mp4" will play and disable text during gameplay.
' also global variable useDMDVideos=true/false if user wishes only TEXT
' TimeSec how long to display msg in Seconds
' animation if any 0=none 1=Flasher
' also,  now can specify color of each line (when no animation).  "sometext|12345"  will set label to "sometext" and set color to 12345
'Samples
'pupDMDDisplay "shoot", "SHOOT AGAIN!", ", 3, 1, 10 
'pupDMDDisplay "default", "DATA GADGET LIT", "@DataGadgetLit.mp4", 3, 1, 10
'pupDMDDisplay "shoot", "SHOOT AGAIN!", "@shootagain.mp4", 3, 1, 10   
'pupDMDDisplay "balllock", "Ball^Locked|16744448", "", 5, 1, 10             '  5 seconds,  1=flash, 10=priority, ball is first line, locked on second and locked has custom color |
'pupDMDDisplay "balllock","Ball 2^is^Locked", "balllocked2.mp4",3, 1,10     '  3 seconds,  1=flash, play balllocked2.mp4 from dmdsplash folder, 
'pupDMDDisplay "balllock","Ball^is^Locked", "@balllocked.mp4",3, 1,10       '  3 seconds,  1=flash, play @balllocked.mp4 from dmdsplash folder, because @ text by default is hidden unless useDmDvideos is disabled.


'pupDMDDisplay "shownum", "3^More To|616744448^GOOOO", "", 5, 1, 10         ' "shownum" is special.  layout is line1=BIG NUMBER and line2,line3 are side two lines.  "4^Ramps^Left"

'pupDMDDisplay "target", "POTTER^110120", "blank.mp4", 10, 0, 10            ' 'target'...  first string is line,  second is 0=off,1=already on, 2=flash on for each character in line (count must match)

'pupDMDDisplay "highscore", "High Score^AAA   2451654^BBB   2342342", "", 5, 0, 10            ' highscore is special  line1=text title like highscore, line2, line3 are fixed fonts to show AAA 123,123,123
'pupDMDDisplay "highscore", "High Score^AAA   2451654|616744448^BBB   2342342", "", 5, 0, 10  ' sames as above but notice how we use a custom color for text |



'----- VR Room Auto-Detect -----
Dim VR_Obj, VRRoom

VRRoom = 1 ' Default to Minimal

Sub Table1_OptionEvent(ByVal eventId)
    ' VR Room
    VRRoom = Table1.Option("VR Room", 1, 3, 1, VRRoomChoice, 0, Array("Minimal", "Space", "MR Mode"))
    SetupVRRoom
End Sub

Sub SetupVRRoom()
	Room360.Visible = 0
    If RenderingMode = 2 Then
        For Each VR_Obj in Walls : VR_Obj.sidevisible = 0 : Next
        If VRRoom = 2 Then
            ' Space Room
            For Each VR_Obj in VR_MinimalRoom : VR_Obj.Visible = 0 : Next
            For Each VR_Obj in VR_Space : VR_Obj.Visible = 1 : Next
			For Each VR_Obj in VR_Table : VR_Obj.Visible = 1 : Next
			Wall040.SideVisible = 0
			Wall042.SideVisible = 0
        ElseIf VRRoom = 3 Then
            ' MR Mode
            For Each VR_Obj in VR_MinimalRoom : VR_Obj.Visible = 0 : Next
            For Each VR_Obj in VR_Space : VR_Obj.Visible = 0 : Next
			For Each VR_Obj in VR_Table : VR_Obj.Visible = 1 : Next
			Room360.Visible = 1
			Wall040.SideVisible = 0
			Wall042.SideVisible = 0
        Else
            ' Minimal Room
            For Each VR_Obj in VR_MinimalRoom : VR_Obj.Visible = 1 : Next
            For Each VR_Obj in VR_Space : VR_Obj.Visible = 0 : Next
        For Each VR_Obj in VR_Table : VR_Obj.Visible = 1 : Next
        End If
        lrail.Visible = 0
        rrail.Visible = 0
        lrail1.Visible = 0
        rrail1.Visible = 0
		Wall040.Visible = 0
		Wall042.Visible = 0
    Else
        VRRoom = 0
        For Each VR_Obj in VR_MinimalRoom : VR_Obj.Visible = 0 : Next
        For Each VR_Obj in VR_Space : VR_Obj.Visible = 0 : Next
        For Each VR_Obj in VR_Table : VR_Obj.Visible = 0 : Next
		Wall040.SideVisible = 1
		Wall042.SideVisible = 1
        If Table1.ShowDT Then
            lrail.Visible = 1
            rrail.Visible = 1
            lrail1.Visible = 1
            rrail1.Visible = 1

        Else
            lrail.Visible = 0
            rrail.Visible = 0
            lrail1.Visible = 0
            rrail1.Visible = 0

        End If
    End If
End Sub



' VR PLUNGER ANIMATION
'
' Code needed to animate the plunger. If you pull the plunger it will move in VR.
' IMPORTANT: there are two numeric values in the code that define the postion of the plunger and the 
' range in which it can move. The first numeric value is the actual y position of the plunger primitive
' and the second is the actual y position + 100 to determine the range in which it can move.
'
' You need to to select the VR_Primary_plunger primitive you copied from the
' template and copy the value of the Y position 
' (e.g. 2130) into the code. The value that determines the range of the plunger is always the y 
' position + 100 (e.g. 2230).
'

Sub TimerPlunger_Timer

  If VR_Primary_plunger.Y < -79.9933 then
      VR_Primary_plunger.Y = VR_Primary_plunger.Y + 5
  End If
End Sub

Sub TimerPlunger2_Timer
	VR_Primary_plunger.Y = -214.933 + (5* Plunger.Position) -20
End Sub

'VR Stuff Below.. ****************************************************************************************************

Dim move1:move1=0.02   'For VR Skybox

Sub Sky_Timer()
Sky.ObjRotZ=Sky.ObjRotZ+move1
End Sub


Dim move2:move2=0.06   'For VR Meteors
Dim move3:move3=0.08
Dim MoveShip: MoveShip = 0.2
Dim MoveShip2: MoveShip2 = 0.25

Sub Rock1Timer_Timer()
Rock1.ObjRotZ=Rock1.ObjRotZ+move2
Rock1.ObjRotX=Rock1.ObjRotX+move3

Rock2.ObjRotZ=Rock2.ObjRotZ+move2
Rock2.ObjRotX=Rock2.ObjRotX+move3

Rock4.ObjRotZ=Rock4.ObjRotZ+move3
Rock4.ObjRotX=Rock4.ObjRotX+move2

Rock7.ObjRotZ=Rock7.ObjRotZ+move3
Rock7.ObjRotX=Rock7.ObjRotX+move2

FallingStar1.ObjRotZ=FallingStar1.ObjRotZ+move3
FallingStar1.ObjRotX=FallingStar1.ObjRotX+move3

FallingStar2.ObjRotZ=FallingStar2.ObjRotZ+move3
FallingStar2.ObjRotX=FallingStar2.ObjRotX+move3


Moon.ObjRotZ=Moon.ObjRotZ+move2

End Sub


Dim move4:move4=22
Dim move5:move5=-24

Sub StarTimer_Timer()

FallingStar1.Y=FallingStar1.Y+move4
FallingStar2.Y=FallingStar2.Y+move5

If FallingStar1.Y>=50000 then Randomize (21): FallingStar1.Y = -42000: FallingStar1.X = 15000 + rnd(1)*-25000 : FallingStar1.Size_x = 1300 * rnd(1) +320 : FallingStar1.Size_y = 1300 * rnd(1) +320: FallingStar1.Size_z = 1300 * rnd(1) +550' Randomize X position, x,y,z size here
If FallingStar2.Y<=-40000 then Randomize (5): FallingStar2.Y = 50000: FallingStar2.X = 15000 + rnd(1)*-25000 : FallingStar2.Size_x = 1300 * rnd(1) +320 : FallingStar2.Size_y = 1300 * rnd(1) +320: FallingStar2.Size_z = 1300 * rnd(1) +550' Randomize X position, x,y,z size here

End Sub

'******
' Stars Timer
'******

Const StarsCounterMax = 299

TimerStars.enabled = True

Dim StarsCounter: StarsCounter = 1

Sub TimerStars_Timer()

if StarsCounter < 10 then 
        Stars.Image = "" & "000" & StarsCounter
		Stars001.Image = "" & "000" & StarsCounter
		Stars002.Image = "" & "000" & StarsCounter
    elseif StarsCounter < 100 then
        Stars.Image = "" & "00" & StarsCounter 
		Stars001.Image = "" & "00" & StarsCounter
		Stars002.Image = "" & "00" & StarsCounter
    elseif StarsCounter < 300  then
        Stars.Image = "" & "0" & StarsCounter 
		Stars001.Image = "" & "0" & StarsCounter
		Stars002.Image = "" & "0" & StarsCounter
    else 
        Stars.image = "" & StarsCounter
		Stars001.Image = "" & StarsCounter
		Stars002.Image = "" & StarsCounter
    end if

    StarsCounter = StarsCounter + 1 

    If StarsCounter > StarsCounterMax Then
        StarsCounter = 1
   End If
End Sub


'DOF Update it by Outhere - Search the word > outhere < for DOF I Changed or Added
'101 Left Flipper
'102 Right Flipper
'103 Left Slingshot
'104 Right Slingshot
'105
'106 Right Slingshot Shake
'107 Top Bumper Left
'108 Top Bumper Center
'109 Top Bumper Right
'110 Lower Bumper Left
'111 Lower Bumper CenterLeft
'112 Lower Bumper Right
'113 Very Top Bumper Left
'114 Wormhole 1
'115 Wormhole 2
'116 Wormhole 3
'117 Drop Targets Left Reset
'118 GI on / off
'119 Drop Targets Center Reset
'120 Drop Targets Right Reset
'121
'122 Knocker
'123 Ball Release
'124 Left Kick Back
'125
'126 sc_droptarget
'127
'128 Right Kick Back
'129 BlackHole Kick Out
'130 HyperSpaceHole Kick Out
'131 Upper Left Slingshot
'132 Upper Right Slingshot
'133 Shaker
'134 Beacon - BallLock
'135
'136
'137
'138
'139
'140
'141
'142
'143 AutoPlunger
'144
'146
'147
'148
'149
'150
'151
'152
'153
'154
'155