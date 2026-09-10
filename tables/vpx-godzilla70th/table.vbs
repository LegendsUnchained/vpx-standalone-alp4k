Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="godzilla",UseSolenoids=1,UseLamps=0,UseGI=0, SCoin="coin"
Const BallSize = 50			' Ball size must be 50
Const BallMass = 1			' Ball mass must be 1

LoadVPM "01000000","SEGA.VBS",3.10

Dim DesktopMode: DesktopMode = Table1.ShowDT
Dim mMagnaSave2


If DesktopMode = True Then 'Show Desktop components
Ramp16.visible=1
Ramp15.visible=1
Primitive13.visible=0
Else
Ramp16.visible=1
Ramp15.visible=1
Primitive13.visible=0
End if

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


'**********************************************************************************************************
'Kickers
'**********************************************************************************************************

Sub Tflash_hit()
FlashForMs f7, 2000, 50, 0
FlashForMs f8, 92500, 50, 0
FlashForMs f9, 3000, 50, 0
FlashForMs f10, 3500, 50, 0
FlashForMs f11, 4000, 50, 0
FlashForMs f12, 4500, 50, 0
End Sub

sub K5_hit()
k5.destroyball
FlashForMs f1, 2000, 50, 0
FlashForMs f2, 2500, 50, 0
FlashForMs f3, 3000, 50, 0
FlashForMs f4, 3500, 50, 0
FlashForMs f5, 4000, 50, 0
FlashForMs f6, 4500, 50, 0
k6.createball
k6.kick 250,7
PlaySound "kicker"
end Sub

sub K2_hit()
k2.destroyball
k3.createball
k3.kick 265,7
end Sub

sub K9_hit()
k9.destroyball
Timer009.enabled=1
PlaySound "combo"
end Sub

sub timer009_timer
	Timer009.enabled=0
Timer010.enabled=1
end Sub

sub timer010_timer
Timer010.enabled=0
k10.createball
k10.kick 200,20,80
FlashForMs Laser, 2000, 50, 0
FlashForMs laser1, 2000, 50, 0
PlaySound "13"
'Addscore 5000
end Sub


'**********************************************************************************************************
'Sonnar
'**********************************************************************************************************

ringspin.enabled = 1

Sub ringspin_timer
	ringplate.RotZ = ringplate.RotZ + 0.3
    ufometal.RotY = ufometal.RotY + 0.5
end Sub

'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************

 SolCallBack(1) = "SolRelease"
 SolCallBack(2) = "Auto_Plunger"
SolCallback(3) = "solTopMagnet"
SolCallback(4) = "solMiddleMagnet"
SolCallback(5) = "solBottomMagnet"
SolCallback(6) 	= "SolShake"
SolCallBack(7)="SetLamp 107, "
SolCallback(14) = "solOrbitMagnet"
SolCallback(17) = "SolRampDiverter"
SolCallBack(18)="SetLamp 118, "
SolCallBack(19)="SetLamp 119, "
SolCallBack(20)="SetLamp 120, "
SolCallBack(25)="SetLamp 125, " 'F1
SolCallBack(26)="SetLamp 126, " 'F2
SolCallBack(27)="SetLamp 127, " 'F3
SolCallBack(28)="SetLamp 128, " 'F4
SolCallBack(29)="SetLamp 129, " 'F5
SolCallBack(30)="SetLamp 130, " 'F6
SolCallBack(31)="SetLamp 131, " 'F7
SolCallBack(32)="SetLamp 132, " 'F8

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors):LeftFlipper.RotateToEnd
         PlaySound SoundFX("fx_Flipperup",DOFContactors):LeftFlipperH.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):LeftFlipper.RotateToStart
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):LeftFlipperH.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFContactors):RightFlipper.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFContactors):RightFlipper.RotateToStart
     End If
End Sub
'**********************************************************************************************************

'Solenoid Controlled toys
'**********************************************************************************************************

Sub SolRelease(Enabled)
	If Enabled Then
	bsTrough.ExitSol_On
	vpmTimer.PulseSw 15
	End If
End Sub
 
Sub Auto_Plunger(Enabled)
	If Enabled Then
	PlungerIM.AutoFire
	End If
End Sub

Sub SolRampDiverter(enabled)
	If enabled then
		Diverter1.IsDropped = 1
		Diverter2.IsDropped = 0
        pont.roty=-7
 		PlaySound "Diverter"
	Else	
		Diverter1.IsDropped = 0
		Diverter2.IsDropped = 1
        pont.roty=33
		PlaySound "Diverter"
	End If
End Sub

Sub SolShake(enabled)
	If enabled Then
	    ShakeTimer.Enabled = 1
		playsound SoundFX("Motor",DOFContactors)
	Else
    	ShakeTimer.Enabled = 0
	End If
End Sub

Sub ShakeTimer_Timer()
	Nudge 0,1
	Nudge 90,1
	Nudge 180,1
	Nudge 270,1
End Sub 

Sub solTopMagnet(enabled)
    If enabled Then 
		mTopMagnet.MagnetOn = 1
	Else 
		mTopMagnet.MagnetOn = 0
End If
End Sub

Sub solMiddleMagnet(enabled)
    If enabled Then 
		mMiddleMagnet.MagnetOn = 1
	Else 
		mMiddleMagnet.MagnetOn = 0
End If
End Sub

Sub solBottomMagnet(enabled)
    If enabled Then 
		mBottomMagnet.MagnetOn = 1
	Else 
		mBottomMagnet.MagnetOn = 0
End If
End Sub

Sub solOrbitMagnet(enabled)
    If enabled Then 
		mOrbitMagnet.MagnetOn = 1
	Else 
		mOrbitMagnet.MagnetOn = 0
End If
End Sub

set GICallback = GetRef("UpdateGI")
Sub UpdateGI(no, Enabled)
	If Enabled Then
		dim xx
		For each xx in GI:xx.State = 1:	Next
        PlaySound "fx_relay"

	Else For each xx in GI:xx.State = 0: Next
        PlaySound "fx_relay"

	End If
End Sub

'***************************
'   LUT - Darkness control 
'***************************

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

Sub NextLUT:LUTImage = (LUTImage + 1)MOD 15:UpdateLUT:SaveLUT:Lutbox.text = "level of darkness " & LUTImage:End Sub

Sub UpdateLUT
    Select Case LutImage
        Case 0:table1.ColorGradeImage = "LUT0":GiIntensity = 1:ChangeGIIntensity 1
        Case 1:table1.ColorGradeImage = "LUT1":GiIntensity = 1.05:ChangeGIIntensity 1
        Case 2:table1.ColorGradeImage = "LUT2":GiIntensity = 1.1:ChangeGIIntensity 1
        Case 3:table1.ColorGradeImage = "LUT3":GiIntensity = 1.15:ChangeGIIntensity 1
        Case 4:table1.ColorGradeImage = "LUT4":GiIntensity = 1.2:ChangeGIIntensity 1
        Case 5:table1.ColorGradeImage = "LUT5":GiIntensity = 1.25:ChangeGIIntensity 1
        Case 6:table1.ColorGradeImage = "LUT6":GiIntensity = 1.3:ChangeGIIntensity 1
        Case 7:table1.ColorGradeImage = "LUT7":GiIntensity = 1.35:ChangeGIIntensity 1
        Case 8:table1.ColorGradeImage = "LUT8":GiIntensity = 1.4:ChangeGIIntensity 1
        Case 9:table1.ColorGradeImage = "LUT9":GiIntensity = 1.45:ChangeGIIntensity 1
        Case 10:table1.ColorGradeImage = "LUT10":GiIntensity = 1.5:ChangeGIIntensity 1
        Case 11:table1.ColorGradeImage = "LUT11":GiIntensity = 1.55:ChangeGIIntensity 1
        Case 12:table1.ColorGradeImage = "LUT12":GiIntensity = 1.6:ChangeGIIntensity 1
        Case 13:table1.ColorGradeImage = "LUT13":GiIntensity = 1.65:ChangeGIIntensity 1
        Case 14:table1.ColorGradeImage = "LUT14":GiIntensity = 1.7:ChangeGIIntensity 1
    End Select
End Sub

Dim GiIntensity
GiIntensity = 1               'used for the LUT changing to increase the GI lights when the table is darker

Sub ChangeGIIntensity(factor) 'changes the intensity scale
    Dim bulb
    For each bulb in GI
        bulb.IntensityScale = GiIntensity * factor
    Next
End Sub

'******************************************************
'		FLIPPER CORRECTION INITIALIZATION
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity()
   dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 80
		x.DebugOn=False ' prints some info in debugger


        x.AddPt "Polarity", 0, 0, 0
        x.AddPt "Polarity", 1, 0.05, - 2.7
        x.AddPt "Polarity", 2, 0.16, - 2.7
        x.AddPt "Polarity", 3, 0.22, - 0
        x.AddPt "Polarity", 4, 0.25, - 0
        x.AddPt "Polarity", 5, 0.3, - 1
        x.AddPt "Polarity", 6, 0.4, - 2
        x.AddPt "Polarity", 7, 0.5, - 2.7
        x.AddPt "Polarity", 8, 0.65, - 1.8
        x.AddPt "Polarity", 9, 0.75, - 0.5
        x.AddPt "Polarity", 10, 0.81, - 0.5
        x.AddPt "Polarity", 11, 0.88, 0
        x.AddPt "Polarity", 12, 1.3, 0

		x.AddPt "Velocity", 0, 0, 0.85
		x.AddPt "Velocity", 1, 0.15, 0.85
		x.AddPt "Velocity", 2, 0.2, 0.9
		x.AddPt "Velocity", 3, 0.23, 0.95
		x.AddPt "Velocity", 4, 0.41, 0.95
		x.AddPt "Velocity", 5, 0.53, 0.95 '0.982
		x.AddPt "Velocity", 6, 0.62, 1.0
		x.AddPt "Velocity", 7, 0.702, 0.968
		x.AddPt "Velocity", 8, 0.95,  0.968
		x.AddPt "Velocity", 9, 1.03,  0.945
		x.AddPt "Velocity", 10, 1.5,  0.945

	Next

	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
    LF.SetObjects "LF", LeftFlipper, TriggerLF
    RF.SetObjects "RF", RightFlipper, TriggerRF
    LF.SetObjects "RF", LeftFlipperH, TriggerLF1
End Sub

'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

' modified 2023 by nFozzy
' Removed need for 'endpoint' objects
' Added 'createvents' type thing for TriggerLF / TriggerRF triggers.
' Removed AddPt function which complicated setup imo
' made DebugOn do something (prints some stuff in debugger)
'   Otherwise it should function exactly the same as before\
' modified 2024 by rothbauerw
' Added Reprocessballs for flipper collisions (LF.Reprocessballs Activeball and RF.Reprocessballs Activeball must be added to the flipper collide subs
' Improved handling to remove correction for backhand shots when the flipper is raised

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt		'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay		'delay before trigger turns off and polarity is disabled
	Private Flipper, FlipperStart, FlipperEnd, FlipperEndY, LR, PartialFlipCoef, FlipStartAngle
	Private Balls(20), balldata(20)
	Private Name
	
	Dim PolarityIn, PolarityOut
	Dim VelocityIn, VelocityOut
	Dim YcoefIn, YcoefOut
	Public Sub Class_Initialize
		ReDim PolarityIn(0)
		ReDim PolarityOut(0)
		ReDim VelocityIn(0)
		ReDim VelocityOut(0)
		ReDim YcoefIn(0)
		ReDim YcoefOut(0)
		Enabled = True
		TimeDelay = 50
		LR = 1
		Dim x
		For x = 0 To UBound(balls)
			balls(x) = Empty
			Set Balldata(x) = new SpoofBall
		Next
	End Sub
	
	Public Sub SetObjects(aName, aFlipper, aTrigger)
		
		If TypeName(aName) <> "String" Then MsgBox "FlipperPolarity: .SetObjects error: first argument must be a String (And name of Object). Found:" & TypeName(aName) End If
		If TypeName(aFlipper) <> "Flipper" Then MsgBox "FlipperPolarity: .SetObjects error: Second argument must be a flipper. Found:" & TypeName(aFlipper) End If
		If TypeName(aTrigger) <> "Trigger" Then MsgBox "FlipperPolarity: .SetObjects error: third argument must be a trigger. Found:" & TypeName(aTrigger) End If
		If aFlipper.EndAngle > aFlipper.StartAngle Then LR = -1 Else LR = 1 End If
		Name = aName
		Set Flipper = aFlipper
		FlipperStart = aFlipper.x
		FlipperEnd = Flipper.Length * Sin((Flipper.StartAngle / 57.295779513082320876798154814105)) + Flipper.X ' big floats for degree to rad conversion
		FlipperEndY = Flipper.Length * Cos(Flipper.StartAngle / 57.295779513082320876798154814105)*-1 + Flipper.Y
		
		Dim str
		str = "Sub " & aTrigger.name & "_Hit() : " & aName & ".AddBall ActiveBall : End Sub'"
		ExecuteGlobal(str)
		str = "Sub " & aTrigger.name & "_UnHit() : " & aName & ".PolarityCorrect ActiveBall : End Sub'"
		ExecuteGlobal(str)
		
	End Sub
	
	' Legacy: just no op
	Public Property Let EndPoint(aInput)
		
	End Property
	
	Public Sub AddPt(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out)
		Select Case aChooseArray
			Case "Polarity"
				ShuffleArrays PolarityIn, PolarityOut, 1
				PolarityIn(aIDX) = aX
				PolarityOut(aIDX) = aY
				ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity"
				ShuffleArrays VelocityIn, VelocityOut, 1
				VelocityIn(aIDX) = aX
				VelocityOut(aIDX) = aY
				ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef"
				ShuffleArrays YcoefIn, YcoefOut, 1
				YcoefIn(aIDX) = aX
				YcoefOut(aIDX) = aY
				ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
	End Sub
	
	Public Sub AddBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If IsEmpty(balls(x)) Then
				Set balls(x) = aBall
				Exit Sub
			End If
		Next
	End Sub
	
	Private Sub RemoveBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If TypeName(balls(x) ) = "IBall" Then
				If aBall.ID = Balls(x).ID Then
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
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x)) Then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next
	End Property
	
	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x)) Then
				balldata(x).Data = balls(x)
			End If
		Next
		FlipStartAngle = Flipper.currentangle
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub

	Public Sub ReProcessBalls(aBall) 'save data of balls in flipper range
		If FlipperOn() Then
			Dim x
			For x = 0 To UBound(balls)
				If Not IsEmpty(balls(x)) Then
					if balls(x).ID = aBall.ID Then
						If isempty(balldata(x).ID) Then
							balldata(x).Data = balls(x)
						End If
					End If
				End If
			Next
		End If
	End Sub

	'Timer shutoff for polaritycorrect
	Private Function FlipperOn()
		If GameTime < FlipAt+TimeDelay Then
			FlipperOn = True
		End If
	End Function
	
	Public Sub PolarityCorrect(aBall)
		If FlipperOn() Then
			Dim tmp, BallPos, x, IDX, Ycoef, BalltoFlip, BalltoBase, NoCorrection, checkHit
			Ycoef = 1
			
			'y safety Exit
			If aBall.VelY > -8 Then 'ball going down
				RemoveBall aBall
				Exit Sub
			End If
			
			'Find balldata. BallPos = % on Flipper
			For x = 0 To UBound(Balls)
				If aBall.id = BallData(x).id And Not IsEmpty(BallData(x).id) Then
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					BalltoFlip = DistanceFromFlipperAngle(BallData(x).x, BallData(x).y, Flipper, FlipStartAngle)
					If ballpos > 0.65 Then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)								'find safety coefficient 'ycoef' data
				End If
			Next
			
			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				If ballpos > 0.65 Then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)												'find safety coefficient 'ycoef' data
				NoCorrection = 1
			Else
				checkHit = 50 + (20 * BallPos) 

				If BalltoFlip > checkHit or (PartialFlipCoef < 0.5 and BallPos > 0.22) Then
					NoCorrection = 1
				Else
					NoCorrection = 0
				End If
			End If
			
			'Velocity correction
			If Not IsEmpty(VelocityIn(0) ) Then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
				
				'If partialflipcoef < 1 Then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
				
				If Enabled Then aBall.Velx = aBall.Velx*VelCoef
				If Enabled Then aBall.Vely = aBall.Vely*VelCoef
			End If
			
			'Polarity Correction (optional now)
			If Not IsEmpty(PolarityIn(0) ) Then
				Dim AddX
				AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				
				If Enabled and NoCorrection = 0 Then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef*VelCoef)
			End If
			If DebugOn Then debug.print "PolarityCorrect" & " " & Name & " @ " & GameTime & " " & Round(BallPos*100) & "%" & " AddX:" & Round(AddX,2) & " Vel%:" & Round(VelCoef*100)
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	Dim x, aCount
	aCount = 0
	ReDim a(UBound(aArray) )
	For x = 0 To UBound(aArray)		'Shuffle objects in a temp array
		If Not IsEmpty(aArray(x) ) Then
			If IsObject(aArray(x)) Then
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	If offset < 0 Then offset = 0
	ReDim aArray(aCount-1+offset)		'Resize original array
	For x = 0 To aCount-1				'set objects back into original array
		If IsObject(a(x)) Then
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
	BallSpeed = Sqr(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)		'Set up line via two points, no clamping. Input X, output Y
	Dim x, y, b, m
	x = input
	m = (Y2 - Y1) / (X2 - X1)
	b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius
	Public Property Let Data(aBall)
		With aBall
			x = .x
			y = .y
			z = .z
			velx = .velx
			vely = .vely
			velz = .velz
			id = .ID
			mass = .mass
			radius = .radius
		End With
	End Property
	Public Sub Reset()
		x = Empty
		y = Empty
		z = Empty
		velx = Empty
		vely = Empty
		velz = Empty
		id = Empty
		mass = Empty
		radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	Dim y 'Y output
	Dim L 'Line
	'find active line
	Dim ii
	For ii = 1 To UBound(xKeyFrame)
		If xInput <= xKeyFrame(ii) Then
			L = ii
			Exit For
		End If
	Next
	If xInput > xKeyFrame(UBound(xKeyFrame) ) Then L = UBound(xKeyFrame)		'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )
	
	If xInput <= xKeyFrame(LBound(xKeyFrame) ) Then Y = yLvl(LBound(xKeyFrame) )		 'Clamp lower
	If xInput >= xKeyFrame(UBound(xKeyFrame) ) Then Y = yLvl(UBound(xKeyFrame) )		'Clamp upper
	
	LinearEnvelope = Y
End Function

'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************

Dim bsTrough, mTopMagnet, mMiddleMagnet, mBottomMagnet, mOrbitMagnet

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Godzilla limited edition"&chr(13)&"have fun"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.hidden = 0
        .Games(cGameName).Settings.Value("sound")=1
		.PuPHide = 1
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With
     On Error Goto 0

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = true
	vpmNudge.TiltSwitch = swTilt

	vpmNudge.Sensitivity = 2
	vpmNudge.TiltObj=Array(Bumper2,LeftSlingshot,RightSlingshot)

	Set bsTrough = new cvpmBallStack
		bsTrough.InitSw 0,14,13,12,11,0,0,0
		bsTrough.Balls = 4
		bsTrough.InitKick BallRelease,45,8
        bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)

	Set mTopMagnet = New cvpmMagnet
		mTopMagnet.InitMagnet TopMagnet, 100
		mTopMagnet.Solenoid = 3
		mTopMagnet.GrabCenter = True
		mTopMagnet.CreateEvents "mTopMagnet"
	
	Set mMiddleMagnet = New cvpmMagnet 
		mMiddleMagnet.InitMagnet MiddleMagnet, 2 
		mMiddleMagnet.Solenoid = 4
		mMiddleMagnet.GrabCenter = False
		mMiddleMagnet.CreateEvents "mMiddleMagnet"

	Set mBottomMagnet = New cvpmMagnet 
		mBottomMagnet.InitMagnet BottomMagnet, 2
		mBottomMagnet.Solenoid = 5
		mBottomMagnet.GrabCenter = False
		mBottomMagnet.CreateEvents "mBottomMagnet"

	Set mOrbitMagnet = New cvpmMagnet
		mOrbitMagnet.InitMagnet OrbitMagnet, 100
		mOrbitMagnet.Solenoid = 14
		mOrbitMagnet.GrabCenter = True
		mOrbitMagnet.CreateEvents "mOrbitMagnet"

    Kicker1.CreateBall
    Kicker1.Kick 0,1
    Kicker2.CreateBall
    Kicker2.Kick 0,1
    Kicker3.CreateBall
    Kicker3.Kick 0,1

	Diverter1.IsDropped = 0
	Diverter2.IsDropped = 1

End Sub


Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub
sub Table1_Exit:Controller.Stop:end sub

'**********************************************************************************************************
'magma
'**********************************************************************************************************

Set mMagnaSave2 = New cvpmMagnet : With mMagnaSave2
	.InitMagnet Magna2, 15
End With

Sub Magna2_Hit():mMagnaSave2.AddBall ActiveBall: End Sub
Sub Magna2_UnHit(): mMagnaSave2.RemoveBall ActiveBall: End Sub

sub tr12_hit()
    PlaySound "magnet"
	mMagnaSave2.MagnetOn = 1 
	magnettimer002.enabled=1 
end sub

sub magnettimer002_timer()
	magnettimer002.enabled=0
	mMagnaSave2.MagnetOn = 0
end Sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)
    If keycode = LeftMagnaSave Then bLutActive = True
    If keycode = RightMagnaSave Then
        If bLutActive Then NextLUT:End If
    End If

	If KeyDownHandler(keycode) Then Exit Sub
	If KeyCode=PlungerKey Then Controller.Switch(53)=1

End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If KeyUpHandler(keycode) Then Exit Sub
	If KeyCode=PlungerKey Then Controller.Switch(53)=0
End Sub

     ' Impulse Plunger
	Dim PlungerIM
    Const IMPowerSetting = 55
    Const IMTime = 0.6
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 0.3
        .InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
        .CreateEvents "plungerIM"
    End With

'**********************************************************************************************************

 ' Drain hole and kickers
Sub Drain_Hit:bsTrough.addball me : playsound"drain" : End Sub

 'Stand Up Targets
Sub sw17_hit:vpmTimer.pulseSw 17 : playsound"targethaut" : End Sub
Sub sw18_hit:vpmTimer.pulseSw 18 : playsound"targethaut" : End Sub
Sub sw19_hit:vpmTimer.pulseSw 19 : playsound"targethaut" : End Sub

Sub sw21_hit:vpmTimer.pulseSw 21 : playsound"trigger" : End Sub
Sub sw22_hit:vpmTimer.pulseSw 22 : playsound"trigger" : End Sub
Sub sw23_hit:vpmTimer.pulseSw 23 : playsound"trigger" : End Sub

Sub sw29_hit:vpmTimer.pulseSw 29 : End Sub
Sub sw30_hit:vpmTimer.pulseSw 30 : End Sub
Sub sw31_hit:vpmTimer.pulseSw 31 : End Sub
Sub sw32_hit:vpmTimer.pulseSw 32 : End Sub

Sub sw20_Hit:vpmTimer.PulseSw 20 : sw20p.TransX = -10 : sw20.TimerEnabled = 1 : playsound"Target" : End Sub
Sub sw20_Timer : sw20p.TransX = 0 : sw20.TimerEnabled = 0 : End Sub
Sub sw24_Hit:vpmTimer.PulseSw 24 : sw24p.TransX = -10 : sw24.TimerEnabled = 1 : playsound"Target" : End Sub
Sub sw24_Timer : sw24p.TransX = 0 : sw24.TimerEnabled = 0 : End Sub
Sub sw33_Hit:vpmTimer.PulseSw 20 : sw33p.TransX = -10 : sw33.TimerEnabled = 1 : playsound"magnet" : End Sub
Sub sw33_Timer : sw20p.TransX = 0 : sw33.TimerEnabled = 0 : End Sub

'Wire Triggers
Sub sw16_Hit : Controller.Switch(16)=1 : playsound"rollover" : End Sub 
Sub sw16_Unhit : Controller.Switch(16)=0:End Sub
Sub sw41_Hit : Controller.Switch(41)=1 : playsound"rollover" : End Sub 
Sub sw41_Unhit : Controller.Switch(41)=0:End Sub
Sub sw42_Hit : Controller.Switch(42)=1 : playsound"rollover" : End Sub 
Sub sw42_Unhit : Controller.Switch(42)=0:End Sub
Sub sw43_Hit : Controller.Switch(43)=1 : playsound"rollover" : End Sub 	
Sub sw43_Unhit : Controller.Switch(43)=0:End Sub
Sub sw44_Hit : Controller.Switch(44)=1 : playsound"rollover" : End Sub 
Sub sw44_Unhit : Controller.Switch(44)=0:End Sub
Sub sw47_Hit : Controller.Switch(47)=1 : playsound"rollover" : End Sub 
Sub sw47_Unhit : Controller.Switch(47)=0:End Sub
Sub sw48_Hit : Controller.Switch(48)=1 : playsound"rollover" : End Sub 
Sub sw48_Unhit : Controller.Switch(48)=0:End Sub
Sub sw57_Hit : Controller.Switch(57)=1 : playsound"explosion" : End Sub 
Sub sw57_Unhit : Controller.Switch(57)=0:End Sub
Sub sw58_Hit : Controller.Switch(58)=1 : playsound"explosion1" : End Sub 
Sub sw58_Unhit : Controller.Switch(58)=0:End Sub
Sub sw60_Hit : Controller.Switch(60)=1 : playsound"explosion" : End Sub 
Sub sw60_Unhit : Controller.Switch(60)=0:End Sub
Sub sw61_Hit : Controller.Switch(61)=1 : playsound"explosion1" : End Sub 
Sub sw61_Unhit : Controller.Switch(61)=0:End Sub

'Ramp Gate Triggers
Sub sw26_Hit:vpmTimer.PulseSw 26 : End Sub 
Sub sw28_Hit:vpmTimer.PulseSw 28 : End Sub 

' Gate Triggers
Sub sw45_Hit:vpmTimer.PulseSw 45 : End Sub 

'Spinners
Sub sw46_Spin:vpmTimer.PulseSw 46 : playsound"fx_spinner" : End Sub
Sub sw62_Spin:vpmTimer.PulseSw 62 : playsound"fx_spinner" : End Sub
Sub sw63_Spin:vpmTimer.PulseSw 62 : playsound"fx_spinner" : End Sub

'Bumpers
Sub Bumper2_Hit : vpmTimer.PulseSw(50) : ringcap.transx = -10 : godcap.transx = -10 : Me.TimerEnabled = 1 : playsound SoundFX("fx_bumper1",DOFContactors): End Sub
Sub Bumper2_Timer : ringcap.transx = 0 : godcap.transx = 0 : Me.TimerEnabled = 0 : End Sub

'Generic Sounds
Sub Trigger1_Hit:PlaySound "fx_ballrampdrop":End Sub
Sub Trigger2_Hit:PlaySound "fx_ballrampdrop":End Sub
Sub Trigger3_Hit:PlaySound "fx_ballrampdrop":End Sub
Sub Trigger4_Hit:PlaySound "fx_ballrampdrop":End Sub
Sub Trigger4a_Hit:PlaySound "fx_ballrampdrop":End Sub
Sub TriggerrampD_Hit:PlaySound "fx_metalrolling":End Sub
Sub TriggerrampD1_Hit:PlaySound "fx_metalrolling":End Sub
Sub TriggerrampG_Hit:PlaySound "fx_metalrolling":End Sub
Sub TriggerrampGB_Hit:PlaySound "fx_metalrolling":End Sub


'***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************

Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200)

InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
LampTimer.Interval = 5 'lamp fading speed
LampTimer.Enabled = 1

' Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
            FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4 'actual fading step
        Next
    End If
    UpdateLamps
End Sub

Sub UpdateLamps
 NFadeL 1, l1
 NFadeL 2, l2
 NFadeL 3, l3
 NFadeL 4, l4
 NFadeLm 5, l5a
 NFadeL 5, l5
 NFadeL 6, l6
 NFadeL 7, l7
 'NFadeL 8, l8 'Launch Button
 NFadeL 9, l9
 NFadeL 10, l10
 NFadeL 11, l11
 NFadeL 12, l12
 NFadeL 13, l13
 NFadeL 14, l14
 NFadeL 15, l15
 NFadeL 16, l16
 NFadeL 17, l17
 NFadeL 18, l18
 NFadeL 19, l19
 NFadeL 20, l20
 NFadeL 21, l21
 NFadeL 22, l22
 NFadeL 23, l23
 NFadeL 24, l24
 NFadeL 25, l25
 NFadeL 26, l26
 NFadeL 27, l27
 NFadeLm 28, l28 'PF GI
 NFadeL 28, l28a
 NFadeL 29, l29
 NFadeL 30, l30
 NFadeL 31, l31
 NFadeLm 32, l32 'PF GI
 NFadeL 32, l32a
 NFadeL 33, l33
 NFadeL 34, l34
 NFadeL 35, l35
 NFadeL 36, l36 
 NFadeL 37, l37 
 NFadeL 38, l38
 NFadeL 39, l39

 NFadeL 41, l41
 NFadeL 42, l42
 NFadeL 43, l43
 NFadeL 44, l44
 NFadeLm 45, l45 'Bumper
 NFadeL 45, l45a
 NFadeLm 46, l46 'Bumper
 NFadeL 46, l46a
 NFadeLm 47, l47 'Bumper
 NFadeL 47, l47a

 NFadeL 49, l49
 NFadeL 50, l50
 NFadeL 51, l51
 NFadeL 52, l52
 NFadeL 53, l53
 NFadeL 54, l54
 NFadeL 55, l55
 NFadeL 56, l56

'Solenoid Controlled Lamps

 NFadeLm 107, S107
 NFadeLm 107, S107a
 NFadeL 107, S107b

 NFadeObjm 118, P118, "dome2_0_greenON", "dome2_0_green" 
 NFadeL 118, S118
 
 NFadeObjm 119, P119, "dome2_0_greenON", "dome2_0_green"
 NFadeL 119, S119
 
 NFadeL 120, S120

 NFadeLm 125, S125
 NFadeLm 125, S125a
 NFadeLm 125, S125b
 NFadeL 125, S125c

 NFadeObjm 126, P126, "dome2_0_yellowON", "dome2_0_yellow"
 NFadeObjm 126, P126a, "dome2_0_yellowON", "dome2_0_yellow"
 NFadeLm 126, S126
 NFadeL 126, S126a

 NFadeL 127, S127

 NFadeL 128, S128

 NFadeL 129, S129

 NFadeL 130, S130

 NFadeLm 131, S131
 NFadeL 131, S131a

 NFadeObjm 132, P132, "dome2_0_redON", "dome2_0_red"
 NFadeLm 132, S132
 NFadeL 132, S132a

End Sub

' div lamp subs

Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0        ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4      ' used to track the fading state
        FlashSpeedUp(x) = 0.4   ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.2 ' slower speed when turning off the flasher
        FlashMax(x) = 1         ' the maximum value when on, usually 1
        FlashMin(x) = 0         ' the minimum value when off, usually 0
        FlashLevel(x) = 0       ' the intensity of the flashers, usually from 0 to 1
    Next
End Sub

Sub AllLampsOff
    Dim x
    For x = 0 to 200
        SetLamp x, 0
    Next
End Sub

Sub SetLamp(nr, value)
    If value <> LampState(nr) Then
        LampState(nr) = abs(value)
        FadingLevel(nr) = abs(value) + 4
    End If
End Sub

' Lights: used for VP10 standard lights, the fading is handled by VP itself

Sub NFadeL(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.state = 0:FadingLevel(nr) = 0
        Case 5:object.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeLm(nr, object) ' used for multiple lights
    Select Case FadingLevel(nr)
        Case 4:object.state = 0
        Case 5:object.state = 1
    End Select
End Sub

'Lights, Ramps & Primitives used as 4 step fading lights
'a,b,c,d are the images used from on to off

Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.image = a:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1             'wait
        Case 9:object.image = c:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1         'wait
        Case 13:object.image = d:FadingLevel(nr) = 0                  'Off
    End Select
End Sub

Sub FadeObjm(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
        Case 9:object.image = c
        Case 13:object.image = d
    End Select
End Sub

Sub NFadeObj(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 0 'off
        Case 5:object.image = a:FadingLevel(nr) = 1 'on
    End Select
End Sub

Sub NFadeObjm(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
    End Select
End Sub

' Flasher objects

Sub Flash(nr, object)
    Select Case FadingLevel(nr)
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it just sets the flashlevel
    Object.IntensityScale = FlashLevel(nr)
End Sub


'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 62
    PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    godzilla.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10:godzilla.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0:godzilla.TransZ = 0:
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 59
    PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    godzilla.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10:godzilla.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0:godzilla.TransZ = 0:
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
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function


'********************************************************************
'      JP's VP10 Rolling Sounds (+rothbauerw's Dropping Sounds)
'********************************************************************

Const tnob = 7 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingTimer_Timer()
    Dim BOT, bb
    BOT = GetBalls

    ' stop the sound of deleted balls
    For bb = UBound(BOT) + 1 to tnob
        rolling(bb) = False
        StopSound("fx_ballrolling" & bb)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

   For bb = 0 to UBound(BOT)
        ' play the rolling sound for each ball
        If BallVel(BOT(bb) ) > 1 AND BOT(bb).z < 30 Then
            rolling(bb) = True
           PlaySound("fx_ballrolling" & bb), -1, Vol(BOT(bb)), AudioPan(BOT(bb)), 0, Pitch(BOT(bb)), 1, 0, AudioFade(BOT(bb))
        Else
           If rolling(bb) = True Then
               StopSound("fx_ballrolling" & bb)
                rolling(bb) = False
           End If
        End If

        ' play ball drop sounds
       If BOT(bb).VelZ < -1 and BOT(bb).z < 55 and BOT(bb).z > 27 Then 'height adjust for ball drop sounds
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
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperLSh1.RotZ = LeftFlipperH.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle

    LFLogo.RotY = LeftFlipper.CurrentAngle
    LFLogoH.RotY = LeftFlipperH.CurrentAngle
    RFlogo.RotY = RightFlipper.CurrentAngle

    sw28p.RotY = sw28.CurrentAngle

End Sub

'*****************************************
'	ninuzzu's	BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6,BallShadow7)

Sub BallShadowUpdate_timer()
    Dim BOT, bd
    BOT = GetBalls
    ' hide shadow of deleted balls
    If UBound(BOT)<(tnob-1) Then
        For bd = (UBound(BOT) + 1) to (tnob-1)
            BallShadow(bd).visible = 0
       Next
    End If
    ' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
    ' render the shadow for each ball
    For bd = 0 to UBound(BOT)
        If BOT(bd).X < Table1.Width/2 Then
          BallShadow(bd).X = ((BOT(bd).X) - (Ballsize/5) + ((BOT(bd).X - (Table1.Width/2))/7)) + 6
        Else
           BallShadow(bd).X = ((BOT(bd).X) + (Ballsize/5) + ((BOT(bd).X - (Table1.Width/2))/7)) - 6
        End If
        ballShadow(bd).Y = BOT(bd).Y + 12
       If BOT(bd).Z > 20 Then
           BallShadow(bd).visible = 1
        Else
           BallShadow(bd).visible = 0
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

Sub LeftFlipperH_Collide(parm)
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
