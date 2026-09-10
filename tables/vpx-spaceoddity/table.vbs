'Space Oddity by BALUTITO
'
' Space Station / IPD No.2261 / December, 1987 / 4 Players
' http://www.ipdb.org/machine.cgi?id=2261
' VP9.16rev626 by MaX
' VP10 by nFozzy
' Version 1.21

'1.21 (1.21 minor hotfix, changed some minor hotfixes)
'Floating Text scores! Implementation of Toxie's idea here http://www.vpforums.org/index.php?showtopic=39255
'New env lighting. Tweaked table lighting, physics. Removed physics hacks.
'New GI color options, GI settings saved to VPReg.stg
'More detailed graphics options (see options at top of table script)

'1.11
'Fixed ball reflections, updated a few scripts

'1.1 changelog
'different GI (and alternative incandescent GI)
'significant memory optimization
'new physics
'Ball shadow routine by Ninuzzu


Option explicit
Randomize

dim FloatingScores
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

'**********************Options********************************

'Graphics options

Const GraphicsLevel = 3	'Between 0 and 4, (Default: 3)
'0 - The barest minimum (will be really dark, turn up day/night slider)
'1 - Low / balanced flashers and GI elements
'2 - All Flasher and GI Elements
'3 - All + GI Ball Reflections + backglass flasher glow + GI compensation
'4 - All That + Habit rail Shadow

const VPXdisplay = 0 'Enable/Disable VPX display. Disable for performance. (Default: 1)

FloatingScores = Table1.ShowDT 'Enable/Disable floating text scores  (Default: Table1.ShowDT)
'Does NOT play nicely with B2S at the moment.
'(In a multiplayer game, floating text will only appear for player 1)

Const OldSidewalls = 0 'Restores the old diffuse GI sidewall reflections. More perspective safe. (Default: 0)

Const LoadColorsOverride = 0 'If 'Loadcolors' is erroring for some reason, set this to 1 (Default: 0)

'More options:

const SingleScreenFS = 0 '1 = VPX display 2 = Vpinmame display rotated (Default: 0)
const BackGlassFlasher = 0 'Enable / Disable the backglass rocket flasher in DT mode (Default: 0)
SoundLevelMult = 1 'Increase table SFX (normalization)

'you can switch GI types in-game by pressing the right magnasave with the left magnasave held down






'Debug boxes off
Sub InitTextBoxes()	'turn off debug boxes
	dim x : for each x in textboxes : x.visible = 0 : next
End Sub






'Floating text
'*************

ScoreBox.TimerEnabled = FloatingScores


dim FTlow, FTmed, FThigh
InitFloatingText
Sub InitFloatingText()
	Set FTlow = New FloatingText
	with FTlow 
		.Sprites(0) = Array(FtLow1_1, FtLow1_2, FtLow1_3, FtLow1_4, FtLow1_5, FTlow1_6)
		.Sprites(1) = Array(FtLow2_1, FtLow2_2, FtLow2_3, FtLow2_4, FtLow2_5, FtLow2_6)
		.Sprites(2) = Array(FtLow3_1, FtLow3_2, FtLow3_3, FtLow3_4, FtLow3_5, FtLow3_6)
		.Sprites(3) = Array(FtLow4_1, FtLow4_2, FtLow4_3, FtLow4_4, FtLow4_5, FtLow4_6)
		.Sprites(4) = Array(FtLow5_1, FtLow5_2, FtLow5_3, FtLow5_4, FtLow5_5, FtLow5_6)
		
		.Prefix = "Font_"
		.Size = 29/2
		.FadeSpeedUp = 1/800
		.RotX = -37

	end With

	Set FTmed = New FloatingText
	With FTmed
		.Sprites(0) = Array(FtMed1_1, FtMed1_2, FtMed1_3, FtMed1_4, FtMed1_5, FtMed1_6)
		.Sprites(1) = Array(FtMed2_1, FtMed2_2, FtMed2_3, FtMed2_4, FtMed2_5, FtMed2_6)
		.Sprites(2) = Array(FtMed3_1, FtMed3_2, FtMed3_3, FtMed3_4, FtMed3_5, FtMed3_6)
		.Sprites(3) = Array(FtMed4_1, FtMed4_2, FtMed4_3, FtMed4_4, FtMed4_5, FtMed4_6)
		.Sprites(4) = Array(FtMed5_1, FtMed5_2, FtMed5_3, FtMed5_4, FtMed5_5, FtMed5_6)
		.Prefix = "Font_"
		.Size = 29
		.FadeSpeedUp = 1/1500
		.RotX = -37
	End With

	Set FThigh = New FloatingText
	With FThigh
		.Sprites(0) = Array(FtHi1_1, FtHi1_2, FtHi1_3, FtHi1_4, FtHi1_5, FtHi1_6)
		.Sprites(1) = Array(FtHi2_1, FtHi2_2, FtHi2_3, FtHi2_4, FtHi2_5, FtHi2_6)
		.Sprites(2) = Array(FtHi3_1, FtHi3_2, FtHi3_3, FtHi3_4, FtHi3_5, FtHi3_6)
		.Prefix = "Font_"
		.Size = 29*4
		.FadeSpeedUp = 1/3500
		.RotX = -37
	End With

End Sub

'A special keyframe animation called with big scores
dim aLutBurst : Set aLutBurst = New cAnimation
with aLutBurst
	.AddPoint 0, 0, 0
	.AddPoint 1, 130, 20 'up
	.AddPoint 2, 150, 14 'hold
	.AddPoint 3, 180, 20 'hold
	.AddPoint 4, 210, 14 'hold
	.AddPoint 5, 240, 20 'hold
	.AddPoint 6, 270, 14 'hold
	.AddPoint 7, 290, 20 'hold
	.AddPoint 8, 320, 14 'hold
	.AddPoint 9, 350, 20 'hold
	.AddPoint 10,350+130, 0 'down
	.Callback = "animLutBurst"
End With
Sub animLutBurst(aLVL)
	table1.ColorGradeImage = "RedLut_" & Round(aLVL)
ENd Sub
animlutburst 0

Const UseVPMNVRAM = true
dim LastSwitch : Set LastSwitch = Sw10
dim LastScore : LastScore = 0

Sub ScoreBox_Timer()
	Dim NVRAM : NVRAM = Controller.NVRAM
	dim str : str = _
	ConvertBCD(NVRAM(CInt("&h200"))) & _
	ConvertBCD(NVRAM(CInt("&h201"))) & _
	ConvertBCD(NVRAM(CInt("&h202"))) & _
	ConvertBCD(NVRAM(CInt("&h203")))		'sys 11 current score
	str = round(str)

	dim PointGain
	PointGain = Str - LastScore	
	LastScore = str

	if PointGain >= 90000 Then	'hi point scores
		PlaceFloatingTextHi PointGain, LastSwitch.x, LastSwitch.y	
		aLutBurst.Play
	elseif pointgain >= 2500 then 'medium point scores	
		ftmed.TextAt PointGain, Lastswitch.x, Lastswitch.Y
	elseif pointgain > 0 then	'low point scores
		ftlow.TextAt PointGain, Lastswitch.x, Lastswitch.Y
	end if
	'if debugstr <> "" then 
	'	if tb.text <> debugstr then tb.text = debugstr
	'end if

	FTlow.Update2
	FTmed.Update2

	FThigh.Update2
	aLutBurst.Update2
End Sub

Function ConvertBCD(v)
	ConvertBCD = "" & ((v AND &hF0) / 16) & (v AND &hF)
End Function

'Helper placer sub
Sub PlaceFloatingTextHi(aPointGain, ByVal aX, ByVal aY)	'center text a bit for the big scores
	aX = (aX + (table1.width/2))/2
	aY = (aY + (table1.Height/2))/2
	FThigh.TextAt aPointGain, aX, aY
End Sub

'Switch location handling, or at least the method I use
Sub aSwitches_Hit(aIDX)
	Set LastSwitch = aSwitches(aIDX)
End Sub

'Walls don't have x/y coords so a spoof object is used for slingshots.
'Set LastSwitch = LeftSlingPos under 'Sub LeftSlingShot_SlingShot' 
Dim LeftSlingPos : Set LeftSlingPos = New WallSwitchPos
Dim RightSlingPos : Set RightSlingPos = New WallSwitchPos
Dim LeftLockPos : Set LeftLockPos = New WallSwitchPos

Class WallSwitchPos : Public x,y,name : End Class
'
LeftLockPos.Name = "LeftLock"
LeftLockPos.x = leftlock.x + 180
LeftLockPos.y = LeftLock.y


LeftSlingPos.Name = "LeftSlingShot"
LeftSlingPos.X = 160+25
LeftSlingPos.Y = 1480

RightSlingPos.Name = "RightSlingShot"
RightSlingPos.X = 700-25
RightSlingPos.Y = 1480

'Some debug boxes
'tb.timerinterval = 2200
'tb.Timerenabled = 1
'Sub tb_timer()
'	FTlow.TextAt "1234567", 600, 1000
'End Sub

'Sub tbDB_Timer()
'	dim str,x
'	for x = 0 to ftlow.count
'		str = str & "L&L " & x & ": " & abs(Ftlow.Lock(x)) & "" & abs(Ftlow.loaded(x)) & vbnewline '& " " & FtLow.Text(x)
'	Next
'	if me.text <> str then me.text = str end if
'End Sub




'End Floating text
'*************




dim BallMass : BallMass = 1
dim BallSize : BallSize = 50
'kas 90, 0.1 : ka k, 419.4706, 945' : coef=1
dim SoundLevelMult, ReflectColor(2), FastFlips, DesktopMode : DesktopMode = Table1.ShowDT



SetLocale(1033)	'Important for lamp routine to work correctly



LoadVPM "01120100", "S11.VBS", 3.65


'*************************************************************
'Debug stuff
Dim DebugFlippers : DebugFlippers = False
dim aDebugBoxes : aDebugBoxes = array(TbBounces, tbpl,tbWR, TbFlipper)
Sub DebugF(input)
	dim x
	if input = 5 then for each x in aDebugBoxes : x.visible = cBool(input) : next : exit sub
	input = cbool(input)
	if IsObject(fastflips) then FastFlips.Debug = input
'	TiltSol input
	destroyer.enabled = input
	Sw10.enabled = not input 'space station specific, disable trough 
	if not input then Sw10.kick -10, 45 'space station specific
	for each x in aDebugBoxes : x.visible = input : next 
	for each x in aDebugBoxes : x.TimerEnabled = input : next 
end sub
debugf 0
'debugf 5 'boxes

sub GimmeF() : kl.createsizedballwithmass 25, ballmass : kl.kick 0, 5 : debugf 1 : end Sub
sub Gimmep() : kp.createsizedballwithmass 25, ballmass : kp.kick 0, 5 : debugf 1 : end Sub
sub Gimme() : kr.createsizedballwithmass 25, ballmass : kr.kick 0, 5 : debugf 1 : end Sub


'******************************************************************************
'     _______.  ______    __    __  .__   __.  _______      _______ ___   ___ 
'    /       | /  __  \  |  |  |  | |  \ |  | |       \    |   ____|\  \ /  / 
'   |   (----`|  |  |  | |  |  |  | |   \|  | |  .--.  |   |  |__    \  V  /  
'    \   \    |  |  |  | |  |  |  | |  . `  | |  |  |  |   |   __|    >   <   
'.----)   |   |  `--'  | |  `--'  | |  |\   | |  '--'  |   |  |      /  .  \  
'|_______/     \______/   \______/  |__| \__| |_______/    |__|     /__/ \__\ 
'                                                                             
'******************************************************************************


'10.4 playsound args - name,loopcount,volume,pan,randompitch,pitch,UseExisting,Restart,Fade


'*********Sounds with falloffs*********
Sub Targets_Hit (idx)
	PlaySound SoundFX("target",DOFTargets), 0, LVL(0.2), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, Fade(ActiveBall)
	PlaySound SoundFX("targethit",0), 0, LVL(Vol(ActiveBall)*18 ), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, Fade(ActiveBall)
End Sub

Sub zCol_Sleeves_Hit() : RandomSoundRubber : End sub

Sub Posts_Hit(idx)
	PlaySound RandomPost, 0, LVL(Vol(ActiveBall)*80 ), Pan(ActiveBall)*50, 0, Pitch(ActiveBall), 1, 0, Fade(ActiveBall)
End Sub

Sub Pegs_Hit(idx)
	PlaySound RandomPost, 0, LVL(Vol(ActiveBall)*80 ), Pan(ActiveBall)*50, 0, Pitch(ActiveBall), 1, 0, Fade(ActiveBall)
End Sub



'*********Other Sounds*********
Sub RampEntry2_Hit()	'Left habitrail out of space station
	'Playsound "WireRamp", 0, LVL(0.3)
	WireRampOff 'Off of the plastic platform first
	WireRampOn False	'Quick hack arg :False = Habitrail, True = Plastic Ramp
End Sub

Sub RampSoundPlunge1_hit() : WireRampOn  False : End Sub
Sub RampSoundPlunge2_hit() : WireRampOff : WireRampOn True : End Sub	'Exit Habitrail, onto Mini PF 
Sub RampSoundPlunge3_hit() : WireRampOn True : End Sub	'Exit Vuk, onto Mini PF 

Sub RampEntry_Hit()
 	If activeball.vely < -8 then 
		WireRampOn True
		'PlaySound "plasticrolling2", 0, LVL(1), Pan(ActiveBall), 0, Pitch(ActiveBall)*10, 1, 0
		PlaySound "ramp_hit2", 0, LVL(Vol(ActiveBall)), Pan(ActiveBall), 0, Pitch(ActiveBall)*10, 0, 0, Fade(activeball)
	Elseif activeball.vely > 3 then 
		'StopSound "plasticrolling2"
		PlaySound "PlayfieldHit", 0, LVL(Vol(ActiveBall) ), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, Fade(activeball)
	End If
End Sub

'Sub RampSound0_Hit()	'mid plunge
'	If Activeball.velx > 0 then
'		Playsound "pmaReriW", 0, LVL(1)
'	Else
'		Playsound "WireRamp1", 0, LVL(1)
'	End If
'End Sub

Sub zApron_Hit (idx)
	PlaySound "woodhitaluminium", 0, LVL((Vol(ActiveBall))*10), Pan(ActiveBall)/4, 0, Pitch(ActiveBall), 1, 0,Fade(ActiveBall)
End Sub

Sub zPlastic_Hit (idx)
	PlaySound "Rubber_Hit_2", 0, LVL(Vol(ActiveBall)*30), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0,Fade(ActiveBall)
End Sub

Sub zInlanes_Hit (idx)
	PlaySound "MetalHit2", 0, LVL(Vol(ActiveBall)*5), Pan(ActiveBall)*55, 0, Pitch(ActiveBall), 1, 0,Fade(ActiveBall)
End Sub

Sub RampHit2_Hit()	
	PlaySound "ramp_hit3", 0, LVL(Vol(ActiveBall) ), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0,Fade(ActiveBall)
End Sub

Sub Metals_Hit (idx)
	PlaySound "metalhit_medium", 0, LVL(Vol(ActiveBall)*30 ), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0,Fade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate", 0, LVL(Vol(ActiveBall) ), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0,Fade(ActiveBall)
End Sub

'Ramp drops using collision events
Sub col_UPF_Fall_hit():if FallSFX1.Enabled = 0 then playsound "drop_mono", 0, LVL(0.5), 0,0,0,0,0,Fade(ActiveBall) :FallSFX1.Enabled = 1	:end if :end sub'name,loopcount,volume,pan,randompitch
Sub FallSFX1_Timer():me.Enabled = 0:end sub

Sub Col_Rramp_Fall_Hit():if FallSFX2.Enabled = 0 then playsound "drop_mono", 0, LVL(0.5), 0.05,0,0,0,0,Fade(ActiveBall) :FallSFX2.Enabled = 1	:end if :end sub'name,loopcount,volume,pan,randompitch
Sub FallSFX2_Timer():me.Enabled = 0:end sub

Sub col_Lramp_Fall_Hit():if FallSFX3.Enabled = 0 then playsound "drop_mono", 0, LVL(0.5), -0.05,0,0,0,0,Fade(ActiveBall) :FallSFX3.Enabled = 1	:end if :end sub'name,loopcount,volume,pan,randompitch
Sub FallSFX3_Timer():me.Enabled = 0:end sub


Sub LeftSlingShot_Hit(): Playsound RandomBand, 0, LVL(Vol(ActiveBall)*100 ), Pan(ActiveBall)*50, 0, Pitch(ActiveBall), 1, 0,Fade(ActiveBall) : End Sub
Sub RightSlingShot_Hit(): Playsound RandomBand, 0, LVL(Vol(ActiveBall)*100 ), Pan(ActiveBall)*50, 0, Pitch(ActiveBall), 1, 0,Fade(ActiveBall) : End Sub

Sub Bands_Hit(idx) : Playsound RandomBand, 0, LVL(Vol(ActiveBall)*100 ), Pan(ActiveBall)*50, 0, Pitch(ActiveBall), 1, 0,Fade(ActiveBall) : End Sub
Sub RandomSoundRubber() : PlaySound RandomPost, 0, LVL(Vol(ActiveBall)*100 ), Pan(ActiveBall)*50, 0, Pitch(ActiveBall), 1, 0, Fade(ActiveBall) : End Sub



'SFX string functions -SFX - Posts 1-5, Bands 1-4 and 11,22,33,44
Function RandomPost() : RandomPost = "Post" & rndnum(1,5) : End Function

Function RandomBand()
		dim x : x = rndnum(1,4)
		if BallVel(activeball) > 30 then 
			RandomBand = "Rubber" & x & x	'ex. Playsound "Band44" 
		else
			RandomBand = "Rubber" & x	'ex. Playsound "Band4"
		End If
End Function

'Flipper collide sound
Sub LeftFlipper_Collide(parm) : RandomSoundFlipper() : End Sub
Sub RightFlipper_Collide(parm) : RandomSoundFlipper() : End Sub
Sub RandomSoundFlipper()
	dim x : x = RndNum(1,3) 
	PlaySound "flip_hit_" & x, 0, LVL(Vol(ActiveBall)*50 ), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0,Fade(ActiveBall)
End Sub

' Ball Collision Sound
Sub OnBallBallCollision(ball1, ball2, velocity) : PlaySound("fx_collide"), 0, LVL(Csng(velocity) ^2 / 2000), Pan(ball1), 0, Pitch(ball1), 0, 0,Fade(ball1) : End Sub

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************
Const tnob = 9 ' total number of balls
ReDim rolling(tnob)
InitRolling : Sub InitRolling : Dim i : For i = 0 to tnob : rolling(i) = False : Next : End Sub

Sub RollingTimer_Timer
    Dim BOT, b : BOT = GetBalls

	For b = UBound(BOT) + 1 to tnob	' stop the sound of deleted balls
		rolling(b) = False
		StopSound("tablerolling" & b)
	Next

	If UBound(BOT) = -1 Then Exit Sub	' exit the sub if no balls on the table

	For b = 0 to UBound(BOT)	' play the rolling sound for each ball
		If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
			rolling(b) = True
			PlaySound("tablerolling" & b), -1, Vol(BOT(b) )^0.8, Pan(BOT(b) )*3, 0, BallPitch(BOT(b)), 1, 0,Fade(BOT(b))*3
		Else
			If rolling(b) = True Then
                StopSound("tablerolling" & b)
				rolling(b) = False
			End If
		End If
	Next
End Sub
Sub StopAllRolling() 	'call this at table pause!!!
	dim b : for b = 0 to tnob 
		StopSound("tablerolling" & b)
		StopSound("RampLoop" & b)
		StopSound("wireloop" & b)
	next
end sub

'=====================================
'		Ramp Rolling SFX updates nf
'=====================================
'Ball tracking ramp SFX 1.0
'	Usage:
'- Setup hit events with WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'- To stop tracking ball, use WireRampoff
'--	Otherwise, the ball will auto remove if it's below 30 vp units

'Example, from Space Station:
'Sub RampSoundPlunge1_hit() : WireRampOn  False : End Sub						'Enter metal habitrail
'Sub RampSoundPlunge2_hit() : WireRampOff : WireRampOn True : End Sub			'Exit Habitrail, enter onto Mini PF 
'Sub RampEntry_Hit() : If activeball.vely < -10 then WireRampOn True : End Sub 	'Ramp enterance
dim RampMinLoops : RampMinLoops = 4
dim RampBalls(6,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

dim RampType(6)	'Slapped together support for multiple ramp types... False = Wire Ramp, True = Plastic Ramp

Sub WireRampOn(input)  : Waddball ActiveBall, input : RampRollUpdate: End Sub
Sub WireRampOff() : WRemoveBall ActiveBall.ID	: End Sub

Sub Waddball(input, RampInput)	'Add ball	
	dim x : for x = 1 to uBound(RampBalls)	'Check, don't add balls twice
		if RampBalls(x, 1) = input.id then 
			if Not IsEmpty(RampBalls(x,1) ) then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next
	
	For x = 1 to uBound(RampBalls)
		if IsEmpty(RampBalls(x, 1)) then 
			Set RampBalls(x, 0) = input
			RampBalls(x, 1)	= input.ID
			RampType(x) = RampInput
			RampBalls(x, 2)	= 0
			'exit For
			RampBalls(0,0) = True
			RampRoll.Enabled = 1	 'Turn on timer
			'RampRoll.Interval = RampRoll.Interval 'reset timer
			exit Sub
		End If
		if x = uBound(RampBalls) then 	'debug
			Debug.print "WireRampOn error, ball queue is full: " & vbnewline & _
						 RampBalls(0, 0) & vbnewline & _
						 Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & "type:" & RampType(1) & vbnewline & _
						 Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & "type:" & RampType(2) & vbnewline & _
						 Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & "type:" & RampType(3) & vbnewline & _
						 Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & "type:" & RampType(4) & vbnewline & _
						 Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & "type:" & RampType(5) & vbnewline & _
						 " "
		End If
	next
End Sub

Sub WRemoveBall(ID)		'Remove ball
	dim ballcount : ballcount = 0
	dim x : for x = 1 to Ubound(RampBalls)
		if ID = RampBalls(x, 1) then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end If
		'if RampBalls(x,1) = Not IsEmpty(Rampballs(x,1) then ballcount = ballcount + 1
		if not IsEmpty(Rampballs(x,1)) then ballcount = ballcount + 1
	next
	if BallCount = 0 then RampBalls(0,0) = False	'if no balls in queue, disable timer update
End Sub

Sub RampRoll_Timer():RampRollUpdate:End Sub

Sub RampRollUpdate()		'Timer update
	dim x : for x = 1 to uBound(RampBalls)
		if Not IsEmpty(RampBalls(x,1) ) then 
			if BallVel(RampBalls(x,0) ) > 1 then ' if ball is moving, play rolling sound
				If RampType(x) then 
					PlaySound("RampLoop" & x), -1, Vol(RampBalls(x,0) )*10, Pan(RampBalls(x,0) )*3, 0, BallPitchV(RampBalls(x,0) ), 1, 0,Fade(RampBalls(x,0) )'*3
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), -1, Vol(RampBalls(x,0) )*10, Pan(RampBalls(x,0) )*3, 0, BallPitch(RampBalls(x,0) ), 1, 0,Fade(RampBalls(x,0) )'*3
				End If
				RampBalls(x, 2)	= RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
			end if
			if RampBalls(x,0).Z < 30 and RampBalls(x, 2) > RampMinLoops then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end if
	next
	if not RampBalls(0,0) then RampRoll.enabled = 0

End Sub


Sub tbWR_Timer()	'debug textbox
	me.text =	"on? " & RampBalls(0, 0) & " timer: " & RampRoll.Enabled & vbnewline & _
				 "1 " & Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & " type:" & RampType(1) & " Loops:" & RampBalls(1, 2) & vbnewline & _
				 "2 " & Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & " type:" & RampType(2) & " Loops:" & RampBalls(2, 2) & vbnewline & _
				 "3 " & Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & " type:" & RampType(3) & " Loops:" & RampBalls(3, 2) & vbnewline & _
				 "4 " & Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & " type:" & RampType(4) & " Loops:" & RampBalls(4, 2) & vbnewline & _
				 "5 " & Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & " type:" & RampType(5) & " Loops:" & RampBalls(5, 2) & vbnewline & _
				 "6 " & Typename(RampBalls(6, 0)) & " ID:" & RampBalls(6, 1) & " type:" & RampType(6) & " Loops:" & RampBalls(6, 2) & vbnewline & _
				 " "
End Sub

' *********************************************************************
'                      Ball & Sound Functions
' *********************************************************************
'10.4 playsound args - name,loopcount,volume,pan,randompitch,pitch,UseExisting,Restart,Fade

'**************** 3D Audio Vp10.4 Functions ****************
Function Fade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / table1.height-1
    If tmp > 0 Then
		Fade = Csng(tmp ^10)
    Else
        Fade = Csng(-((- tmp) ^10) )
    End If
End Function

Function FadeY(Y) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = y * 2 / table1.height-1
    If tmp > 0 Then
		FadeY = Csng(tmp ^10)
    Else
        FadeY = Csng(-((- tmp) ^10) )
    End If
End Function

'**************** Other sound functions ****************
Function RndNum(min, max)
    RndNum = Int(Rnd() * (max-min + 1) ) + min ' Sets a random number between min and max
End Function

Function LVL(input) : LVL = Input * SoundLevelMult : End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Vol2(ball1, ball2) ' Calculates the Volume of the sound based on the speed of two balls
    Vol2 = (Vol(ball1) + Vol(ball2) ) / 2
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp : tmp = ball.x * 2 / Table1.width-1
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
    BallVel = INT(SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2) )
End Function

Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

'new
Function BallPitch(ball) ' Calculates the pitch of the sound based on the ball speed
    BallPitch = pSlope(BallVel(ball), 1, -1000, 60, 10000)
End Function
Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
    BallPitchV = pSlope(BallVel(ball), 1, -4000, 60, 7000)
End Function
'EOStimer (just switches elast falloff)
'============

'LFHM physics by wrd1972 and Rothbauer
dim EOSAngle,ElastFalloffUp,ElastFalloffDown

ElastFalloffup = LeftFlipper.ElasticityFalloff
ElastFalloffdown = 0.7'0.7

'EOS angle
EOSAngle = 4

'Flipper EOS timer (HMLF)
dim LastAngleL, LastAngleR	'
Sub eostimer_Timer()	'use -1 timer interval for this?
	If LeftFlipper.CurrentAngle <> LastAngleL then	'slight optimization
		'If LeftFlipper.CurrentAngle < LeftFlipper.EndAngle + EOSAngle Then
		If LeftFlipper.CurrentAngle = LeftFlipper.EndAngle or _
		LeftFlipper.CurrentAngle = LeftFlipper.StartAngle Then
			LeftFlipper.ElasticityFalloff = ElastFalloffup
		Else
			LeftFlipper.ElasticityFalloff = ElastFalloffdown
		End If
	End If
	If RightFlipper.CurrentAngle <> LastAngleR then
		'If RightFlipper.CurrentAngle > RightFlipper.EndAngle - EOSAngle Then
		If RightFlipper.CurrentAngle = RightFlipper.EndAngle or _
		RightFlipper.CurrentAngle = RightFlipper.StartAngle Then
			RightFlipper.ElasticityFalloff = ElastFalloffup
		Else
			RightFlipper.ElasticityFalloff = ElastFalloffdown
		End If
	End If
	'dim str : str = LeftFlipper.ElasticityFalloff
	'if tb.text <>str then tb.text = str
	LastAngleL = LeftFlipper.CurrentAngle
	LastAngleR = RightFLipper.CurrentAngle
End Sub	

'**************************************



Siderails.visible = Table1.ShowDT
F31.visible = Table1.ShowDT
'F31.Visible = BackGlassFlasher

P_InstructionsFS.visible = Not Table1.ShowDT : P_InstructionsDT.visible = Table1.ShowDT

Const cGameName = "spstn_l5"

Const UseSolenoids = 1 'Don't touch -nf
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0
Const HandleMech = 0

' Standard Sounds
Const SCoin = "fx_Coin"

Dim bsTrough, bsLeftBallPopper, bsRightBallPopper, bsRightLock, bsLeftLock, dtdrop, dt3bank, mufo
Dim musicNum
musicNum = int ( rnd * 6)
NextTrack


Sub Table1_Init
	vpmInit Me
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine = "Space Station (Williams 1987)"
'		.Games(cGameName).Settings.Value("rol") = 0
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.HandleMechanics = 0
		.Hidden = 0
		.SetDisplayPosition 0,0,GetPlayerHWnd 'if you can't see the DMD then uncomment this line
		.Games("spstn_l5").Settings.Value("sound") = 0
		.Games("spstn_l5").Settings.Value("samples") = 0
		On Error Resume Next
	'	Controller.SolMask(0) = 0
	'	vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the timer to renable all the solenoids after 2 seconds
		Controller.Run GetPlayerHwnd
'        .DIP(0) = &H00		'Taxi
		If Err Then MsgBox Err.Description
		On Error Goto 0
	End With

	If SingleScreenFS = 2 and not Table1.ShowDT then 
		Controller.Games(cGameName).Settings.Value("rol") = 1	
	Else
		Controller.Games(cGameName).Settings.Value("rol") = 0
		Controller.Hidden = 1
	End If
	If VPXdisplay = 0 then 
		Controller.Hidden = 0
	End If

  On Error Goto 0


	' Nudging
	vpmNudge.TiltSwitch = 9
	vpmNudge.Sensitivity = 0.25
    vpmNudge.TiltObj = Array(LeftSlingshot, RightSlingshot, bumper1, bumper2)',Bumper3)	



	'sw41 Left Ball Popper (Top of Table)
	Set bsLeftBallPopper = New cvpmSaucer
	with bsLeftBallPopper
		.InitKicker LeftBallPopper, 41, 85, 85, 100 '82 'switch, direction, force, Zforce	'65
'		.InitExitVariance 2, 2
    End With

	'sw42 Right Ball Popper (Mega VUK)
	Set bsRightBallPopper = New cvpmSaucer
	with bsRightBallPopper
		.InitKicker RightBallPopper, 42, 85, 95, 100 'switch, direction, force, Zforce
		.InitExitVariance 0, 5
    End With

	'sw45 Right Lock
	Set bsRightLock = New cvpmSaucer
	with bsRightLock
		.InitKicker RightLock, 45, 0, 32, 0 'switch, direction, force, Zforce	'28
		.InitExitVariance 1, 2
    End With

	'sw47 Left Lock
	Set bsLeftLock = New cvpmSaucer
	with bsLeftLock
		'.InitKicker LeftLock, 47, 0, 32, 0 'switch, direction, force, Zforce
		.InitKicker LeftLock, 47, 0, 30, 0 'switch, direction, force, Zforce
		.InitExitVariance 5, 2
    End With

	'sw33 Drop Target 1-bank
'	Set dtDrop = New cvpmDropTarget
'	with dtDrop
'		.InitDrop Array(sw33), Array(33)
'		.InitSnd "fx_droptarget", "resetdrop"
''		.CreateEvents "dtDrop"
'    End With

	'Drop Target 3-bank
	Set dt3bank = New cvpmDropTarget
	with dt3bank
		.InitDrop Array(sw57, sw58, sw59), Array(57, 58, 59)
		.InitSnd "", SoundFX("resetdrop",DOFDropTargets)
    End With
	
     ' Main Timer init
     PinMAMETimer.Interval = PinMAMEInterval
     PinMAMETimer.Enabled = 1
	 LampTimer.Enabled = 1

	' Init Kickback
    KickBack.Pullback

	'start trough
	sw13.CreateSizedBallWithMass 25, BallMass
	sw12.CreateSizedBallWithMass 25, BallMass
	sw11.CreateSizedBallWithMass 25, BallMass

	'Init animations
	LeftSling.Showframe 100
	LeftSlingArm.Showframe 100
	RightSling.Showframe 100
	RightSlingArm.Showframe 100
	BumperRing1.Showframe 100
	BumperRing2.Showframe 100
	BumperRing3.Showframe 100
	RubberAnim_1.Showframe 100
	RubberAnim_2.Showframe 100

	BallSearch	'init switches


	if GraphicsLevel > 2 then 
		dim x
		for each x in ReflectionsGI
			x.state = 1	
			x.visible = 1	
		Next

		For each x in ReflectionsGIB
			x.state = 0	
			x.visible = 1
		next
	End If


	if cbool(OldSidewalls) then
		gi_left.imagea = "GILv"
		gi_Right.imagea = "GIRv"
		gig_left.imagea = "GIGL"
		gig_Right.imagea = "GIGR"
	End If



	InitAnimations
	InitLampsNF



	if GraphicsLevel > 3 then 
		shadowtest.IntensityScale = 1 
		shadowtestp.IntensityScale = 1
	end if

	'GI color pulled from VPReg.stg

	gic.name = "SpaceStationNF" 'Name (Public) 	'String input. Sets Name of game in VPReg.stg. IE 'SpaceStationNF'
	gic.Value = "GIcolor" 		'Value (Public) 'String input. Sets Key for color in VPReg.stg.IE 'GIcolor'
	If Not cBool(LoadColorsOverride) then gic.LoadColors


End Sub
Sub table1_Paused:Controller.Pause = 1: StopAllRolling:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub Table1_exit():gic.SaveColors : Controller.Pause = False:Controller.Stop:End Sub 'Save color to VPReg.stg
Sub Destroyer_Hit:Me.destroyball : End Sub

Sub zCol_Band_Anim1_Hit():RubberAnim_1.Playanim 0, (3*Lampz.FrameTime/500):End Sub
Sub zCol_BandUPF_Hit():RubberAnim_2.Playanim 0, (3*Lampz.FrameTime/500):End Sub

Sub Gate6_Hit(): activeball.velx = activeball.velx*0.95 : activeball.vely = activeball.vely*0.95 : End Sub	'switch and gate slow ball to left vuk
Sub Gate5_Hit(): activeball.velx = activeball.velx*0.95 : activeball.vely = activeball.vely*0.95 : End Sub


dim CatchInput(1) : CatchInput(0) = 0 : CatchInput(1) = 0
Sub Table1_KeyDown(ByVal keycode)
	If vpmKeyDown(keycode) Then Exit Sub
    If keycode = PlungerKey Then PlaySound SoundFx("plungerpull",0),0,LVL(1),0.25,0.25:Plunger.Pullback:End If
	If KeyCode = LeftFlipperKey then FastFlips.FlipL True' :  FastFlips.FlipUL True
	If KeyCode = RightFlipperKey then FastFlips.FlipR True' :  FastFlips.FlipUR True
	if Keycode = KeyRules then 
		If DesktopMode Then
			P_InstructionsDT.PlayAnim 0, 3*Lampz.FrameTime/500
		Else
			P_InstructionsFS.PlayAnim 0, 3*Lampz.FrameTime/500
		End If
		Exit Sub
	End If
	if keycode = LeftMagnaSave then catchinput(0) = True
	if keycode = RightMagnaSave then catchinput(1) = True : if catchinput(0) and lampz.state(109) > 0 then gic.changeGI : playsound "fx_relay_on", 0, LVL(0.1)
''	if keycode = 31 then Testtu 'test backhand shot
'
'	if keycode = 203 then TestL 'test backhand shot	'leftarrow
'	if keycode = 200 then TestM 'test backhand shot	'Uparrow
'	if keycode = 205 then TestR 'test backhand shot	'Right Arrow

'	If keycode = LeftFlipperKey Then :FlippersEnabled = True: flipnf 0, 1: exit sub:End If	'debug always-on flippers
'	If keycode = RightFlipperKey Then :FlippersEnabled = True: flipnf 1, 1: exit sub:End If	'debug always-on flippers
End Sub

Sub Table1_KeyUp(ByVal keycode)
    If vpmKeyUp(keycode) Then Exit Sub
    If keycode = PlungerKey Then 
		Plunger.Fire
		if BallInPlunger then 
			PlaySound SoundFX("plunger3",0),0,LVL(1),0.05,0.02,0,0,0,FadeY(1900)
		Else
			PlaySound SoundFX("plunger",0),0,LVL(0.8),0.05,0.02,0,0,0,FadeY(1900)
		end if
	End If
	if Keycode = KeyRules then 
		If DesktopMode Then
			P_InstructionsDT.ShowFrame 0
		Else
			P_InstructionsFs.ShowFrame 0
		End If
		Exit Sub
	End If
	if keycode = LeftMagnaSave then catchinput(0) = False
	if keycode = RightMagnaSave then catchinput(1) = False

	If KeyCode = LeftFlipperKey then FastFlips.FlipL False' :  FastFlips.FlipUL False
	If KeyCode = RightFlipperKey then FastFlips.FlipR False' :  FastFlips.FlipUR False
End Sub

'******************************
'*     HiRez00: Music Mod     *
'******************************


Sub NextTrack

If musicNum = 0 Then PlayMusic "bowie1.mp3" End If
If musicNum = 1 Then PlayMusic "bowie2.mp3" End If
If musicNum = 2 Then PlayMusic "bowie3.mp3" End If
If musicNum = 3 Then PlayMusic "bowie4.mp3" End If
If musicNum = 4 Then PlayMusic "bowie5.mp3" End If
If musicNum = 5 Then PlayMusic "bowie6.mp3" End If

musicNum = (musicNum + 1) mod 6
End Sub

Sub Table1_MusicDone
        NextTrack
End Sub
'***********



'VP10.2 Style OBJ Animations note

'VP10 animation OBJ squences only play back properly at 60FPS. 
'So playback speed must be compensated for in script to play back at the proper speed regardless of framerate.
'x = Lampz.FrameTime y = Playback Speed
'y = 3x/50


Sub TestAnims
	dim x
	x = 3*Lampz.FrameTime'/500
	LeftSling.playanim 0, x
	LeftSlingArm.playanim 0, x
	RightSling.playanim 0, x
	RightSlingArm.playanim 0, x
	BumperRing1.playanim 0, x
	BumperRing2.playanim 0, x
	BumperRing3.playanim 0, x
	RubberAnim_1.Playanim 0, x
	RubberAnim_2.Playanim 0, x
'	lefta.playanim 0, (3*Lampz.FrameTime/500)
End Sub

'Trough Handling
'==============
SolCallback(1)	   = "TroughIn"       ' Drain
SolCallback(2)     = "TroughOut"      ' Ball Release
Sub TroughIn(enabled)
	if Enabled then 
		sw10.Kick 60, 16 : 
		Tgate.Open = True
		If sw10.BallCntOver > 0 Then Playsound SoundFX("Trough1",DOFcontactors), 0, LVL(0.5), 0.01 : End If
	Else 
		Tgate.Open = False
		BallSearch
	End If 
end Sub
Sub TroughOut(enabled): if Enabled then sw11.Kick 58, 8 :Playsound SoundFX("BallRelease",DOFcontactors), 0, LVL(0.4), 0.02:End If: end Sub

'Trough Switches
'Sub sw10_hit():controller.Switch(10) = 1 : Playsound "Trough2", 0, 0.2, 0: End Sub	'Drain
Sub sw10_hit()
	Set LastSwitch = L44	'floatingtext 
	if sw11.BallCntOver + sw12.BallCntOver + Sw13.BallCntOver + _
	PlungerLane.BallcntOver + LeftLock.BallCntOver + _
	SafetyL.BallCntOver + safetyR.ballCntOver + _
	LeftBallPopper.BallCntOver + RightLock.BallCntOver >= 3 then me.destroyball : exit sub

	controller.Switch(10) = 1 
End Sub	'Drain
Sub TroughSFX_Hit(): Playsound "Trough2", 0, LVL(0.2), 0:: End Sub
Sub Sw13_hit():controller.Switch(13) = 1 : UpdateTrough : End Sub
Sub Sw12_hit():controller.Switch(12) = 1 : UpdateTrough : End Sub
Sub Sw11_hit():controller.Switch(11) = 1 : UpdateTrough : End Sub

Sub sw10_UnHit():controller.Switch(10) = 0 : UpdateTrough : End Sub
Sub Sw13_UnHit():controller.Switch(13) = 0 : UpdateTrough : End Sub
Sub Sw12_UnHit():controller.Switch(12) = 0 : UpdateTrough : End Sub
Sub Sw11_UnHit():controller.Switch(11) = 0 : UpdateTrough : End Sub

Sub UpdateTrough: TroughTimer.enabled = 1 : TroughTimer.Interval = 200: end sub

Sub TroughTimer_Timer()
	If sw11.BallCntOver = 0 then sw12.kick 58, 12
	If sw12.BallCntOver = 0 then sw13.kick 58, 12
End Sub

Sub BallSearch()	'In case of hard pinmame reset. Called by PF solenoids firing empty.
	if Sw10.BallCntOver > 0 then controller.Switch(10) = 1 else controller.Switch(10) = 0
	if Sw13.BallCntOver > 0 then controller.Switch(13) = 1 else controller.Switch(13) = 0
	if Sw12.BallCntOver > 0 then controller.Switch(12) = 1 else controller.Switch(12) = 0
	if Sw11.BallCntOver > 0 then controller.Switch(11) = 1 else controller.Switch(11) = 0
	If MotorPos > 180 then Controller.Switch(52) = 1 Else Controller.Switch(52) = 0 End If
	If MotorPos > 90 and  MotorPos < 270 then Controller.Switch(53) = 1 Else Controller.Switch(53) = 0 End If
	UpdateUFO MotorPos
End Sub


'MECH SCRIPTING
SolCallback(16) = "SolMotor"

Dim MotorOn : MotorOn = False
dim MotorPos : MotorPos = 90
dim MotorSpeed : MotorSpeed = 0.054
dim MotorSoundON:MotorSoundON = 0

Sub SolMotor(aOn)
	Select Case aOn
		Case True : MotorOn = True
		Case False : MotorOn = False : MotorSound False
	End Select
End Sub

Sub MotorUpdate()	'Should go counter-clockwise...
	If MotorOn then
		MotorSound True
		MotorPos = MotorPos + MotorSpeed * Lampz.FrameTime
		If MotorPos > 360 then MotorPos = MotorPos - 360
		If MotorPos < 0 then MotorPos = MotorPos + 360

		If MotorPos > 180 then Controller.Switch(52) = 1 Else Controller.Switch(52) = 0 End If
		If MotorPos > 90 and  MotorPos < 270 then Controller.Switch(53) = 1 Else Controller.Switch(53) = 0 End If
		UpdateUFO MotorPos
		'TBM.text = "Motorpos: " & MotorPos & vbnewline & _ 
		'			"Speed: " & MotorSpeed & vbnewline & _ 
		'			"Switch1: " & Controller.Switch(52) & VbNewline & _
		'			"Switch2: " & Controller.Switch(53) & vbnewline & _
		'			MotorOn & " " & gametime
	End If
End Sub

Sub MotorSound(aEnabled)
	If aEnabled Then
		If Not MotorSoundON then Playsound SoundFX("Motor",DOFgear), 0, LVL(1), Pan(SpaceStationToy), 0, 0, 1, 0, Fade(SpaceStationToy)
	Else
		Stopsound SoundFX("Motor",DOFgear)

	End If
	MotorSoundON = aEnabled
End Sub

' Update UFO
Sub UpdateUFO(aNewPos)
	SpaceStationToy.RotZ = (anewpos * -1)
	l41p.RotZ = (anewpos * -1)
	if AnewPos > 350 then GoLeft : Exit Sub
	If AnewPos < 10 Then GoLeft : Exit Sub
	if anewpos > 80 and anewpos < 100 then GoRight : Exit Sub
	if AnewPos > 170 and AnewPos < 190 then GoLeft : Exit Sub
	if AnewPos > 260 and AnewPos < 280 then GoRight : Exit Sub
	If ANewPos > 290 then GoRight : Exit Sub
	ToyClosed.isdropped = 0
End Sub

Sub GoLeft()
	toy1.isdropped=1:toy1a.isdropped=1:toy1b.isdropped=1:toy2.isdropped=0:toy2a.isdropped=0:toy2b.isdropped=0
'	tbm.text = "Go Left (0 180)"
	ToyClosed.isdropped = 1
End Sub

Sub GoRight()
	toy1.isdropped=0:toy1a.isdropped=0:toy1b.isdropped=0:toy2.isdropped=1:toy2a.isdropped=1:toy2b.isdropped=1
'	tbm.text = "Go Right (90 270)"
	ToyClosed.isdropped = 1
End Sub

'==============SWITCHES==============
'====================================
'====================================


'Sweep Bank Handling
Sub	sw57w_Hit : dt3bank.hit 1 : Sw57p.IsDropped = True : Sw57.IsDropped = True : Me.Enabled = 0 : Playsound SoundFX("DropTarget",DOFDropTargets), 0, LVL(1), 0.005: End Sub
Sub	sw58w_Hit : dt3bank.hit 2 : Sw58p.IsDropped = True : Sw58.IsDropped = True : Me.Enabled = 0 : Playsound SoundFX("DropTarget",DOFDropTargets), 0, LVL(1), 0.005: End Sub
Sub	sw59w_Hit : dt3bank.hit 3 : Sw59p.IsDropped = True : Sw59.IsDropped = True : Me.Enabled = 0 : Playsound SoundFX("DropTarget",DOFDropTargets), 0, LVL(1), 0.005: End Sub

'SolCallback(6)	   = "dt3bank.SolDropUp"       ' 3 Drop Target Bank Reset
SolCallback(6)	   = "ResetDTbank"       ' 3 Drop Target Bank Reset
Sub ResetDTbank(Enabled)
	dt3bank.SolDropUp Enabled 
		if Enabled then 
'			tb.text = "DT RESET"
			sw57p.Isdropped = False 
			sw58p.IsDropped = False 
			sw59p.IsDropped = False 
			sw57.Isdropped = False 
			sw58.IsDropped = False 
			sw59.IsDropped = False 
			sw57w.Enabled = True
			sw58w.Enabled = True
			sw59w.Enabled = True
		End If 
End Sub

'Upper Rollovers
Sub sw14_Hit:Controller.Switch(14) = 1:End Sub
Sub sw14_UnHit:Controller.Switch(14) = 0:End Sub
Sub sw15_Hit:Controller.Switch(15) = 1:End Sub
Sub sw15_UnHit:Controller.Switch(15) = 0:End Sub
Sub sw16_Hit:Controller.Switch(16) = 1:End Sub
Sub sw16_UnHit:Controller.Switch(16) = 0:End Sub

'Outlane Switches
Sub sw17_Hit:Controller.Switch(17) = 1:End Sub
Sub sw17_UnHit:Controller.Switch(17) = 0:End Sub
Sub sw32_Hit:Controller.Switch(32) = 1:End Sub
Sub sw32_UnHit:Controller.Switch(32) = 0:End Sub

'Targets
Sub sw18_Hit:vpmTimer.PulseSw 18:End Sub
Sub sw19_Hit:vpmTimer.PulseSw 19:End Sub
Sub sw20_Hit:vpmTimer.PulseSw 20:End Sub
Sub sw21_Hit:vpmTimer.PulseSw 21:End Sub
Sub sw22_Hit:vpmTimer.PulseSw 22:End Sub
Sub sw23_Hit:vpmTimer.PulseSw 23:End Sub
Sub sw24_Hit:vpmTimer.PulseSw 24:End Sub
Sub sw25_Hit:vpmTimer.PulseSw 25:End Sub
Sub sw26_Hit:vpmTimer.PulseSw 26:End Sub
Sub sw27_Hit:vpmTimer.PulseSw 27:End Sub
Sub sw28_Hit:vpmTimer.PulseSw 28:End Sub
Sub sw29_Hit:vpmTimer.PulseSw 29:End Sub
Sub sw30_Hit:vpmTimer.PulseSw 30:End Sub
Sub sw31_Hit:vpmTimer.PulseSw 31:End Sub

'Big target
Sub sw35_Hit:vpmTimer.PulseSw 35:PlaySound "target", 0, LVL(0.1), 0.3:End Sub

'Rollunder
Sub sw37_Hit:Controller.Switch(37) = 1:End Sub
Sub sw37_UnHit:Controller.Switch(37) = 0:End Sub

'USA rollovers
Sub sw38_Hit:Controller.Switch(38) = 1:End Sub
Sub sw38_UnHit:Controller.Switch(38) = 0:End Sub
Sub sw39_Hit:Controller.Switch(39) = 1:End Sub
Sub sw39_UnHit:Controller.Switch(39) = 0:End Sub
Sub sw40_Hit:Controller.Switch(40) = 1:End Sub
Sub sw40_UnHit:Controller.Switch(40) = 0:End Sub

'Shooter Lane
Sub sw43_Hit:Controller.Switch(43) = 1:End Sub
Sub sw43_UnHit:Controller.Switch(43) = 0:End Sub

dim BallInPlunger :BallInPlunger = False
sub PlungerLane_hit():ballinplunger = True: End Sub
Sub PlungerLane_unhit():BallInPlunger = False: End Sub

'Right Lock Entry
Sub sw46_Hit:Controller.Switch(46) = 1:End Sub
Sub sw46_UnHit:Controller.Switch(46) = 0:End Sub

'Left Lock
Sub sw47_Hit:Controller.Switch(47) = 1:End Sub
Sub sw47_UnHit:Controller.Switch(47) = 0:End Sub

'Left Lock Entry (On the Ramp)
Sub sw48_Hit:Controller.Switch(48) = 1:End Sub
Sub sw48_UnHit:Controller.Switch(48) = 0:End Sub

'sw50 10 point (Top right)
Sub zCol_Band_Sw50_Hit():vpmTimer.PulseSw 50:End Sub

'sw54 10 point (Lwr right)
Sub zCol_Band_Sw54_Hit():vpmTimer.Pulsesw 54:End Sub

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 60:PlaySound SoundFX("LeftBumper_Hit",DOFContactors), 	0, LVL(1), -0.01, 0.25:	BumperRing1.playanim 0, (3*Lampz.FrameTime/500) * 1.5:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 61:PlaySound SoundFX("RightBumper_Hit",DOFContactors), 0, LVL(1), 0.01, 0.25:	BumperRing2.playanim 0, (3*Lampz.FrameTime/500) * 1.5:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 62:PlaySound SoundFX("TopBumper_Hit",DOFContactors), 	0, LVL(1), 0, 0.25:		BumperRing3.playanim 0, (3*Lampz.FrameTime/500) * 1.5:End Sub

'One bank Drop-Target
'Sub sw33_Hit():dtDrop.Hit 1: Playsound SoundFX("DropTarget",DOFDropTargets), 0, LVL(1), 0.02,0,0,0,0,FadeY(1900):End Sub

dim aDTup : set aDTup = new cAnimation
dim aDTDown : set aDTDown = new cAnimation

Sub InitAnimations()	'call with Table1_Init()
	with adTup
		.AddPoint 0, 0, -35	'rest -35	'mod me if in a different spot...
		.AddPoint 1, 21, -30 'up
		.AddPoint 2, 100, -30 'hold
		.AddPoint 3, 133, -35 '(rest)
	End With
	aDTup.Callback = "animDTup"
	aDTup.play
	'aDTup.debugOn = True
	with aDTdown
		.AddPoint 0, 0, -35	'rest -35
		.AddPoint 1, 33, -100 'Down
	End With
	aDTDown.Callback = "animDTdown"
End Sub

Sub animDTup(Value) : sw33.z = Value : End Sub
Sub animDTdown(Value) : sw33.z = Value : End Sub

Sub Sw33_Hit()
	set lastswitch = me
	Playsound SoundFX("DropTarget",DOFDropTargets), 0, LVL(1), 0.02,0,0,0,0,FadeY(1900)
	Controller.Switch(33) = 1
	aDTdown.ModPoint 0, 0, sw33.z	'move start key of animation to wherever drop target currently is
	aDTdown.Play
	Sw33.Collidable = False
	Sw33Back.IsDropped = True
End Sub

SolCallback(8)	  = "DropTargetUp"		' Single Drop Target Reset

Sub DropTargetUp(aOn)
	if aOn then
		Playsound SoundFX("resetdrop", DOFdroptargets), 0, LVL(1), 0.02, 0,0,0,0,FadeY(1900)
		aDTup.ModPoint 0, 0, sw33.z	'move start key of animation to wherever drop target currently is
		aDTup.Play
		Controller.Switch(33) = 0
		Sw33.Collidable = True
		Sw33Back.IsDropped = False
	end If
End Sub




'Sling Shots
Sub LeftSlingShot_Slingshot
	Set LastSwitch = LeftSlingPos	'floatingtext
	vpmTimer.PulseSw 63
    PlaySound SoundFX("LeftSlingShot",DOFContactors),0,LVL(1),-0.01,0.05
	LeftSling.playanim 0, (3*Lampz.FrameTime/500) * 1.25
	LeftSlingArm.playanim 0, (3*Lampz.FrameTime/500) * 1.25
End Sub

Sub RightSlingShot_Slingshot
	Set LastSwitch = RightSlingPos
	vpmTimer.PulseSw 64
    PlaySound SoundFX("RightSlingShot",DOFContactors), 0, LVL(1), 0.01, 0.05
	RightSling.playanim 0, (3*Lampz.FrameTime/500) * 1.25
	RightSlingArm.playanim 0, (3*Lampz.FrameTime/500) * 1.25
End Sub

'*********
'Solenoids

'SolCallback(8)	   = "dtdrop.SolDropUp"		' Single Drop Target Reset
SolCallback(13)    = "SolKickBack"    ' Left Re-Entry Kickback

SolCallback(3)     = "LeftVuk"	' Left  Ball Popper
SolCallback(4)	   = "RightVuk"	' Right Ball Popper

Sub LeftVuk(enabled)
	If Enabled then
		If Toy2.isdropped then 
			bsLeftBallPopper.InitKicker LeftBallPopper, 41, 85, 65, 100 '82 'switch, direction, force, Zforce	'65
		Else
			bsLeftBallPopper.InitKicker LeftBallPopper, 41, 85, 85, 100 '82 'switch, direction, force, Zforce	'65
		End If
		bsLeftBallPopper.ExitSol_On
		if LeftBallPopper.BallCntOver > 0 Then
			Playsound SoundFX("fx_VukOut2",DOFContactors), 1, LVL(5), 0
		Else
			Playsound SoundFX("DiverterLeft",DOFContactors), 1, LVL(0.25), 0
			BallSearch
		End If
	End If
End Sub

Sub RightVuk(enabled)
	If Enabled then 
		bsRightBallPopper.ExitSol_On
		if RightBallPopper.BallCntOver > 0 Then
			Playsound SoundFX("fx_VukOut",DOFContactors), 1, LVL(5), 0.01
		Else
			Playsound SoundFX("DiverterRight",DOFContactors), 1, LVL(0.25), 0.1
			BallSearch
		End If
	End If
End Sub

SolCallback(17)    = "RightLockOut"   ' Right lock kickback 
SolCallback(32)    = "LeftLockOut"	' Left lock Kickback

Sub LeftLockOut(enabled)
	If Enabled then 
		bsLeftLock.ExitSol_On
		SafetyL.Enabled = 0: SafetyL.Kick 0, 22 	'bugfix - prevent 2 balls from getting stuck in this lock
		if LeftLock.BallCntOver > 0 Then
			Playsound SoundFX("Kicker_Release",DOFContactors), 1, LVL(1), -0.02 
		Else
			Playsound SoundFX("DiverterLeft",DOFContactors), 1, LVL(0.25), -0.02 
			BallSearch
		End If
	End If
End Sub

Sub RightLockOut(enabled)
	If Enabled then 
		bsRightLock.ExitSol_On
		SafetyR.Enabled = 0: SafetyR.Kick 0, 22	'bugfix - prevent 2 balls from getting stuck in this lock
		if RightLock.BallCntOver > 0 Then
			Playsound SoundFX("Kicker_Release",DOFContactors), 1, LVL(1), 0.01
		Else
			Playsound SoundFX("DiverterLeft",DOFContactors), 1, LVL(0.25), 0.01
			BallSearch
		End If
	End If
End Sub


'PlaySound "name",loopcount,volume,pan,randompitch
Sub LeftBallPopper_Hit():bsLeftBallPopper.Addball me : Playsound SoundFX("Kicker_Hit",DOFContactors), 1, LVL(0.5), 0,0,0,0,0,FadeY(70): End Sub
Sub RightBallPopper_Hit():bsRightBallPopper.AddBall me : Playsound SoundFX("Kicker_Hit",DOFContactors), 1, LVL(0.5), 0.005,0,0,0,0,FadeY(70) : End Sub

Sub LeftLock_Hit()
	set lastswitch = LeftLockPos
	bsLeftLock.AddBall me : Playsound SoundFX("TroughLock",DOFContactors), 1, LVL(0.8), -0.02,0,0,0,0,Fade(me) : SafetyL.Enabled = True 
End Sub
Sub RightLock_Hit():bsRightLock.AddBall me : Playsound SoundFX("TroughLock",DOFContactors), 1, LVL(0.8), 0.01,0,0,0,0,Fade(me) : SafetyR.Enabled = True : End Sub

SolCallback(7) = "KnockerSol"
Sub KnockerSol(enabled) : If Enabled then Playsound SoundFX("Knocker",DOFKnocker), 0, LVL(0.5) : End If : End Sub

Sub SolKickBack(enabled)
    If enabled Then
       Kickback.Fire
       PlaySound SoundFX("DiverterLeft",DOFContactors), 0, LVL(1), -0.02
    Else
       KickBack.PullBack
    End If
End Sub



'*********
' FastFlips NF 'Pre-solid state flippers lag reduction
'*********
SolCallback(23)		= "FastFlips.Flippers_On"


'********************
' Special JP Flippers 'Legacy Flippers using callbacks
'********************
'SolCallback(sLRFlipper) = "SolRFlipper"
'SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
	If Enabled Then
		playsound SoundFX("FlipperUpLeft",DOFFlippers), 0, LVL(1), -0.01	'flip
		LeftFlipper.RotateToEnd
		lf.ProcessBalls
	Else
		LeftFlipper.RotateToStart
		if LeftFlipper.CurrentAngle = LeftFlipper.StartAngle then
			playsound SoundFX("FlipperDown",DOFFlippers), 0, 0
		else
			playsound SoundFX("FlipperDown",DOFFlippers), 0, LVL(1), 0.01	'return
		end if
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		playsound SoundFX("FlipperUpLeft",DOFFlippers), 0, LVL(1), 0.01	'flip
		RightFlipper.RotateToEnd
		rf.ProcessBalls
	Else
		RightFlipper.RotateToStart
		if RightFlipper.CurrentAngle = RightFlipper.StartAngle then
			playsound SoundFX("FlipperDown",DOFFlippers), 0, 0
		else
			playsound SoundFX("FlipperDown",DOFFlippers), 0, LVL(1), 0.01	'return
		end if
			
	End If
End Sub




InitSideWalls
Sub InitSideWalls
	F26w.y = 1004.5
	F26w.x = -5.87

	'F15w.y = 0.1
	'F15w.x = 478.2149658

	F28w.y = 0.1
	F28w.x = 478.2149658

	F29w.y = 0.1
	F29w.x = 478.2149658

	F27w.y = 1004.5
	F27w.x = 958.82

	GI_Left.y = 1004.5
	GI_Left.x = -5.87

	GI_Back.y = 0.1
	GI_Back.x = 478.2149658

	GI_Right.y = 1004.5
	GI_Right.x = 958.82

	GIG_Left.y = 1004.5
	GIG_Left.x = -5.87

	GIG_Back.y = 0.1
	GIG_Back.x = 478.2149658

	GIG_Right.y = 1004.5
	GIG_Right.x = 958.82
End Sub


'New lamp handling 

dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader

Sub LampTimer_Timer()
	dim x, chglamp
	chglamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
		next
	End If
	Lampz.Update2	'update (Both fading and object updates)
	MotorUpdate
	UpdateBallShadow
	
	aDTup.Update2
	aDTdown.update2

End Sub

dim CapOrange : CapOrange = "Flashers_Orange"	'Strings have to be excuted this way, declared in global scope

Sub InitLampsNF()

	lampz.filter = "LampFilter"

	dim x
	'Inserts
	for x = 0 to uBound(lampz.obj)
		lampz.fadespeedup(x) = 1/100
		lampz.fadespeedDown(x) = 1/126
	Next

	'gi
	for x = 109 to 111
		lampz.fadespeedup(x) = 1/100
		lampz.fadespeedDown(x) = 1/100
	Next

	'flashers
	for x = 115 to uBound(lampz.obj)
		lampz.fadespeedup(x) = 1/77
		lampz.fadespeedDown(x) = 1/96
	Next

	'Lamp Assignments
	dim LampNumber
	for each x in ColInserts	 'Simple map lamps (maplamps)
		if TypeName(x) = "Flasher" or TypeName(x) = "Light" then 
			On Error Resume Next
				LampNumber = cInt(mid(x.name,2,2))
				'debug.print x.name & " -> " & LampNumber
				Set lampz.obj(LampNumber) = x
				LampNumber = Empty
			On Error Goto 0
		End If
	Next	
	lampz.massassign 41, L41
	lampz.massassign 41, L41t

	'Flasher Assignments 1 - Image swaps and bare essentials

	Lampz.Callback(115) = "OrangeImageSwapV f15p," '	FlashOBJm 115, f15p, "Flashers_Orange", 5'12 '(nr, object, imgseq, steps)
	Lampz.MassAssign 125, f25							'Relaunch + "ON" Flashers 6		definitely correct

	Lampz.Callback(126) = "OrangeImageSwap f26p,"  ' 	FlashOBJm 126, f26p, "Flashers_Orange", 12

	Lampz.Callback(127) = "OrangeImageSwap f27p,"  ' 	FlashOBJm 127, f27p, "Flashers_Orange", 12

	Lampz.Callback(128) = "BlueOrangeCombo" ' 	FlashOBJm 128, f28p2, "Flashers_Orange", 12

	Lampz.Callback(129) = "RedImageSwap F29p,"

	'Lampz.MassAssign 130, f30								 'Flame + "TI" Flashers 5


	'Flasher Assignments 2 - Ambients
	if GraphicsLevel > 0 Then
		Lampz.MassAssign 115, f15wNew 'Pfd Top Panel Flashers(3)
		Lampz.MassAssign 115, f15wr
		Lampz.MassAssign 126, f26 							'Left Side + "SP" Flashers 1
		Lampz.MassAssign 127, f27a								 'Right Side + "AC" Flashers 2
		Lampz.MassAssign 127, f27b
		Lampz.MassAssign 128, f28a 							'Top Upper Playfield + "ES" Flashers 3
		Lampz.MassAssign 128, f28b
		Lampz.MassAssign 129, f29 							'Playfield Top Panel + "TA" Flashers 4
	End If

	'Flasher Assignments 3 - Wall Reflections
	If GraphicsLevel > 1 Then 
		Lampz.MassAssign 126, f26w
		Lampz.MassAssign 127, f27w
		Lampz.MassAssign 128, f28w
	End If

	'Flasher Assignments 4 - Ambients from backglass
	if GraphicsLevel > 2 then
		Lampz.MassAssign 125, fbg6
		Lampz.MassAssign 125, fbg2	'formerly 127
		Lampz.MassAssign 126, fbg1	
		Lampz.MassAssign 128, fbg3
		Lampz.MassAssign 129, fbg4
		Lampz.MassAssign 130, fbg5
	End If

	'special flasher
	if cBool(backglassflasher) and table1.showdt then Lampz.MassAssign 131, f31 'Station Flashers 

'	'GI Assignments

	Lampz.MassAssign 109, Array(Gi_White, GiBlue, Gi_whitep)
	Lampz.MassAssign 110, Array(Gi_Green, Gi_GreenP)
	Lampz.MassAssign 111, Array(Gi_bumpers, GI_BumpersT)

	'Conditional GI assignments
	If GraphicsLevel > 0 Then
		Lampz.MassAssign 109, Array(Gi_Ambient)
		Lampz.MassAssign 110, Array(Gig_ambient)		
	End If

	If GraphicsLevel > 1 Then
		Lampz.MassAssign 109, Array(GI_Left, Gi_Back, Gi_Right, Gi_Upf1, Gi_Upf2, Gi_Upf3, Gi_Upf4, Gi_Sticker, Gi_UPF, Gi_Shooter, Gi_BumperShine)
		Lampz.MassAssign 110, Array(Gig_Left, Gig_Back, Gig_Right, Gig_Sticker, Gig_UPF, Gig_Shooter, Gig_BumperShine)		
	End If

	If GraphicsLevel > 2 Then
		Lampz.MassAssign 109, ColToArray(ReflectionsGIB)
		Lampz.Callback(109) = "GIupdates"
		Lampz.Callback(110) = "GIupdates"
	End If

	Lampz.State(109) = 1	'GI white
	Lampz.State(110) = 0	'GI Green
	Lampz.State(111) = 1	'Gi Bumpers

	Lampz.Update2

	for each x in getelements : if typename(x) = "Flasher" or typename(x) = "Light" then x.IntensityScale = 0 end if : next

	if GraphicsLevel >2 then for each x in ReflectionsGI : x.Intensityscale = 1 : Next end if

	InitDigits

	'(do this last)
	lampz.init	'This auto turns states on, also initializes objects so they don't stutter when they first appear

	dim tmp : tmp = Array(Gi_White, Gi_Whitep, GI_Left, Gi_Back, _
	Gi_Right, Gi_Sticker, Gi_UPF, Gi_Shooter, Gi_BumperShine)	'no giblue, no giambient, no upperpf
	GIc.Assign tmp
	GIc.Callback = "GIcolorupdate" 'call, not execute, no arguments

	GIc.ColorAssign(0) = ARRAY(255,110,13)	'Warm White (LED)
	GIc.ColorAssign(1) = Array(255,255,255)	'white
	GIc.ColorAssign(2) = ARRAY(15,45,255)	'Blue high sat
	GIc.ColorAssign(3) = Array(85,13,255)	'95% sat Violet
	GIc.ColorAssign(4) = ARRAY(650,25,2)	'Extremely warm white (>255 abuses lum function, as long as it's <1000 it should continue to work)

End Sub

dim GIambientLvl : GIambientLvl = Gi_Ambient.Opacity
dim giaSat : giasat = 0.75 'saturation of the ambient flasher

Sub GIcolorUpdate()
	dim x : for each x in ReflectionsGI : x.Color = GiC.ColorRGB : Next	
	if Not IsArray(GIc.Color) then exit Sub

	dim tmp : tmp = GIc.Color
	dim DeSatColor 
	DeSatColor = DeSat(Array(tmp(0), tmp(1), tmp(2)), giasat)
	Gi_Ambient.Color = RGB(DeSatColor(0), DeSatColor(1), DeSatColor(2) )
	Gi_Ambient.Opacity = GIambientLvl * Gic.Lum(DeSatColor)

	'tb.text = "gicolorupdate " & gametime
End Sub

function DeSat(ByVal aRGB, ByVal aSat)	'simple desaturation function
	dim r, g, b, L
	L = 0.3*aRGB(0) + 0.59*aRGB(1) + 0.1*aRGB(2)
	r = aRGB(0) + aSat * (L - aRGB(0))
	g = aRGB(1) + aSat * (L - aRGB(1))
	b = aRGB(2) + aSat * (L - aRGB(2))
	desat = array(r,g,b)	'return array 
	'desat = rgb(r,g,b)		'return rgb
End Function


dim GIscale : GIscale = 1.75
dim GIFlasherScale : GIFlasherScale = 1.5

Sub GIupdates(ByVal aLvl)
	if Lampz.LVL(109) < Lampz.LVL(110) then aLvl = Lampz.LVL(110) else aLvl = Lampz.LVL(109) end if
	aLvl = 0.952381*aLvl^2 + 0.047619*aLvl
	'aLvl = 1.95238*aLvl - 0.952381*aLvl^2
	dim Offset
	Offset = (GiScale-1) * (ABS(aLvl-1 )  ) + 1	'invert
	dim x : for x = 0 to 92
		lampz.Modulate(x) = Offset
	Next
	lampz.Modulate(115) = Offset
	Offset = (GiFlasherScale-1) * (ABS(aLvl-1 )  ) + 1	'invert
	for x = 116 to 131
		if x <> 126 and x <> 115 then lampz.Modulate(x) = Offset
	Next
End Sub





Sub BlueOrangeCombo(aLVL)
	BlueImageSwap F28p1, aLvl
	OrangeImageSwap F28p2, aLvl
End Sub

Sub OrangeImageSwap(aObj, aLVL)
	dim ImageName : ImageName = "Flashers_Orange"
	dim ImageCount : ImageCount = 9
	aObj.blenddisablelighting = aLvl
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)
	if aLvl > 0.1 then 
		if aObj.Material <> "DomeOrangeOn" then aObj.Material = "DomeOrangeOn"	'switch to a non-metal material
	Else
		if aObj.Material <> "DomeOrange" then aObj.Material = "DomeOrange"
	end if
	dim OutImage
	OutImage = cInt((ImageCount) * aLvl)
	OutImage = ImageName & OutImage

	if aObj.Image <> OutImage then aObj.Image = OutImage' : 	tb.text = OutImage
End Sub

Sub OrangeImageSwapV(aObj, aLVL)
	dim ImageName : ImageName = "Flashers_Orange"
	dim ImageCount : ImageCount = 9
	aObj.blenddisablelighting = aLvl*0.3 * lampz.Modulate(115)
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)
	if aLvl > 0.1 then 
		if aObj.Material <> "DomeOrangeOn" then aObj.Material = "DomeOrangeOn"	'switch to a non-metal material
	Else
		if aObj.Material <> "DomeOrange" then aObj.Material = "DomeOrange"
	end if
	dim OutImage
	OutImage = cInt((ImageCount) * aLvl)
	OutImage = ImageName & OutImage

	if aObj.Image <> OutImage then aObj.Image = OutImage' : 	tb.text = OutImage
End Sub

Sub BlueImageSwap(aObj, aLVL)
	dim ImageName : ImageName = "Flashers"
	dim ImageCount : ImageCount = 9
	aObj.blenddisablelighting = aLvl * lampz.Modulate(128)
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)
	if aLvl > 0.1 then 
		aObj.Material = "DomeBlueOn"	'switch to a non-metal material
	Else
		aObj.Material = "DomeBlue"
	end if
	dim OutImage
	OutImage = cInt((ImageCount) * aLvl)
	OutImage = ImageName & OutImage

	if aObj.Image <> OutImage then aObj.Image = OutImage' : 	tb.text = OutImage
End Sub

Sub RedImageSwap(aObj, aLVL)
	dim ImageName : ImageName = "Flashers"
	dim ImageCount : ImageCount = 9
	aObj.blenddisablelighting = aLvl*0.9 * lampz.Modulate(129)
	'debug.print aObj.BlendDisableLighting
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)
	if aLvl > 0.1 then 
		aObj.Material = "DomeRedOn"	'switch to a non-metal material
	Else
		aObj.Material = "DomeRed"
	end if
	dim OutImage
	OutImage = cInt((ImageCount) * aLvl)
	OutImage = ImageName & OutImage

	if aObj.Image <> OutImage then aObj.Image = OutImage' : 	tb.text = OutImage
End Sub




'Lamp Filter
Function LampFilter(aLvl)	
	'LampFilter = aLvl^1.8	'exponential curve?
	'LampFilter = -5.20833*aLvl^4 + 7.8125*aLvl^3 - 1.97917*aLvl^2 + 0.375*aLvl
	LampFilter = 0.952381*aLvl^2 + 0.047619*aLvl
End Function





'Helper function
Function ColtoArray(aDict)	'converts a collection to an indexed array. Indexes will come out random probably.
	redim a(999)
	dim countr : countr = 0
	dim x  : for each x in aDict : set a(Countr) = x : countr = countr + 1 : Next
	redim preserve a(countr-1) : ColtoArray = a
End Function


'*******************************
'Intermediate Solenoid Procedures (Setlamp, etc)
'********************************
'Solenoid pipeline looks like this:
'Pinmame Controller -> UseSolenoids -> Solcallback -> intermediate subs (here) -> ModLampz dynamiclamps object -> object updates / more callbacks

'Lamps, for reference:
'Pinmame Controller -> LampTimer -> Lampz Fading Object -> Object Updates / callbacks


'Flasher Sols
SolCallBack(15) = "Lampz.SetLamp 115," 'Pfd Top Panel Flashers(3)

SolCallBack(25) = "Lampz.SetLamp 125," 'Relaunch + "ON" Flashers
SolCallBack(26) = "Lampz.SetLamp 126," 'Left Side + "SP" Flashers
SolCallBack(27) = "Lampz.SetLamp 127," 'Right Side + "AC" Flashers
SolCallBack(28) = "Lampz.SetLamp 128," 'Top Upper Playfield + "ES" Flashers
SolCallBack(29) = "Lampz.SetLamp 129," 'Playfield Top Panel + "TA" Flashers
SolCallBack(30) = "Lampz.SetLamp 130," 'Flame + "TI" Flashers
SolCallBack(31) = "Lampz.SetLamp 131," 'Station Flashers 


'Gi Sols
SolCallback(9) = "GItoggler"		'PF
SolCallback(10) = "GiSelect"		'Green toggle
SolCallback(11) = "GIbumpers"		'Bumpers

dim CodeGreen : CodeGreen = False
Sub GIselect(value)
	CodeGreen = value' : git2.text = value
	dim x
	if Value Then
		if Lampz.state(109) + Lampz.state(110) > 0 then 
			GIon 1
			for each x in ReflectionsGI : x.Color = RGB(0,255,0) : Next	
		End If
	else
		if Lampz.state(109) + Lampz.state(110) > 0 then 
			GIon 1
			for each x in ReflectionsGI : x.Color = GiC.ColorRGB : Next	
		End If
	End If

End Sub

Sub GIbumpers(aOff)	'GI relay. Enabled = Off
	Select Case aOff
		Case False	: Lampz.state(111) = 1
		Case True	: Lampz.state(111) = 0
	End Select 
End Sub

Sub GIToggler(value)
	if Lampz.State(109) + Lampz.State(110) = 0 then 
		GIon 1
		Playsound "Fx_Relay_On", 0, LVL(0.1)
	else
		GIon 0
		Playsound "Fx_Relay_Off", 0, LVL(0.1)
	End If
End Sub

Sub GIon(aEnabled)
	dim x
	Select Case aEnabled
		Case 0
			Lampz.State(110) = 0
			Lampz.State(109) = 0
			for each x in ReflectionsGI : x.state = 0 : Next
		Case 1
			for each x in ReflectionsGI : x.state = 1 : Next
			If CodeGreen then 
				Lampz.State(110) = 1 : for each x in ReflectionsGI : x.Color = RGB(0,255,0) : Next	
				Lampz.State(109) = 0		
			Else
				Lampz.State(110) = 0
				Lampz.State(109) = 1 : for each x in ReflectionsGI : x.Color = GiC.ColorRGB : Next	
			End If
	End Select
End Sub












'Display Output

dim DisplayColor, DisplayColorG
InitDisplay
Sub InitDisplay()
	dim x
	for each x in Display
		x.Color = RGB(1,1,1)
	next
	Displaytimer.enabled = 1
	DisplayColor = dx.Color
	DisplayColorG = dxG.Color 
	If DesktopMode then 
		for each x in Display
			x.height = 380 + 70
			x.x = x.x - 956 + 28+52
			x.y = x.y -32
			x.rotx = -42
			x.visible = 1
		next
		for each x in Display2
			x.x = x.x +100-22
			x.y = x.y + 1
		next
		for each x in Display3
			x.height = 411
			x.x = x.x +37
			x.y = x.y +30
		next
		for each x in Display4
			x.height = 411
			x.x = x.x +115-22
			x.y = x.y +30
		next
	Else
		Select Case SingleScreenFS
			Case 1
				Displaytimer.enabled = 1
				for each x in Display
					x.visible = 1
				next
			case 0, 2
				Displaytimer.enabled = 0
				for each x in Display
					x.visible = 0
				next		
		End Select
	End If

	if VPXdisplay = 0 Then
		Displaytimer.enabled = 0
		for each x in Display
			x.visible = 0
		next
	End If
End Sub

'Sub tbd_Timer():me.text = Dstate(0,0) & vbnewline & Dstate(0,1) & vbnewline & Dstate(0,2) & _
'				vbnewline & Dstate(0,3) & vbnewline & Dstate(0,4) & vbnewline & Dstate(0,5) & _ 
'				vbnewline & Dstate(0,6) & vbnewline & Dstate(0,7) & vbnewline & Dstate(0,8) & _ 
'				vbnewline & Dstate(0,9) & vbnewline & Dstate(0,10) & vbnewline & Dstate(0,11) & _
'				vbnewline & Dstate(0,12) & vbnewline & Dstate(0,13) & vbnewline & Dstate(0,14) & vbnewline & Dstate(0,15)
'End Sub

'num = digits 
'chg = LEDs changed since last call
' stat = state

Sub Displaytimer_Timer
	Dim ii, jj, obj, b, x
	Dim ChgLED,num, chg, stat
	ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
		If Not IsEmpty(ChgLED) Then
			For ii=0 To UBound(chgLED)
				num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
				For Each obj In Digits(num)
					If chg And 1 Then FadeDisplay obj, stat And 1	
					chg=chg\2 : stat=stat\2
				Next
			Next
		End If
End Sub

Sub FadeDisplay(object, onoff)
	If OnOff = 1 Then
		object.color = DisplayColor
	Else
		Object.Color = RGB(1,1,1)
	End If
End Sub

Dim Digits(32)
Digits(0)=Array(D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,D12,D13,D14,D15)
Digits(1)=Array(D16,D17,D18,D19,D20,D21,D22,D23,D24,D25,D26,D27,D28,D29,D30)
Digits(2)=Array(D31,D32,D33,D34,D35,D36,D37,D38,D39,D40,D41,D42,D43,D44,D45)
Digits(3)=Array(D46,D47,D48,D49,D50,D51,D52,D53,D54,D55,D56,D57,D58,D59,D60)
Digits(4)=Array(D61,D62,D63,D64,D65,D66,D67,D68,D69,D70,D71,D72,D73,D74,D75)
Digits(5)=Array(D76,D77,D78,D79,D80,D81,D82,D83,D84,D85,D86,D87,D88,D89,D90)
Digits(6)=Array(D91,D92,D93,D94,D95,D96,D97,D98,D99,D100,D101,D102,D103,D104,D105)
Digits(7)=Array(D106,D107,D108,D109,D110,D111,D112,D113,D114,D115,D116,D117,D118,D119,D120)
Digits(8)=Array(D121,D122,D123,D124,D125,D126,D127,D128,D129,D130,D131,D132,D133,D134,D135)
Digits(9)=Array(D136,D137,D138,D139,D140,D141,D142,D143,D144,D145,D146,D147,D148,D149,D150)
Digits(10)=Array(D151,D152,D153,D154,D155,D156,D157,D158,D159,D160,D161,D162,D163,D164,D165)
Digits(11)=Array(D166,D167,D168,D169,D170,D171,D172,D173,D174,D175,D176,D177,D178,D179,D180)
Digits(12)=Array(D181,D182,D183,D184,D185,D186,D187,D188,D189,D190,D191,D192,D193,D194,D195)
Digits(13)=Array(D196,D197,D198,D199,D200,D201,D202,D203,D204,D205,D206,D207,D208,D209,D210)
Digits(14)=Array(D211,D212,D213,D214,D215,D216,D217,D218)
Digits(15)=Array(D219,D220,D221,D222,D223,D224,D225,D226)
Digits(16)=Array(D227,D228,D229,D230,D231,D232,D233,D234)
Digits(17)=Array(D235,D236,D237,D238,D239,D240,D241,D242)
Digits(18)=Array(D243,D244,D245,D246,D247,D248,D249,D250)
Digits(19)=Array(D251,D252,D253,D254,D255,D256,D257,D258)
Digits(20)=Array(D259,D260,D261,D262,D263,D264,D265,D266)
Digits(21)=Array(D267,D268,D269,D270,D271,D272,D273,D274)
Digits(22)=Array(D275,D276,D277,D278,D279,D280,D281,D282)
Digits(23)=Array(D283,D284,D285,D286,D287,D288,D289,D290)
Digits(24)=Array(D291,D292,D293,D294,D295,D296,D297,D298)
Digits(25)=Array(D299,D300,D301,D302,D303,D304,D305,D306)
Digits(26)=Array(D307,D308,D309,D310,D311,D312,D313,D314)
Digits(27)=Array(D315,D316,D317,D318,D319,D320,D321,D322)


Sub InitDigits()
	if not cBool(VPXdisplay) then displaytimer.enabled = 0 : exit Sub
	dim tmp, x, xx
	for x = 0 to uBound(Digits)
		if IsArray(Digits(x) ) then
			tmp = Digits(x)
			for each xx in tmp : xx.IntensityScale = 1 : next
		end If
	Next
End Sub

' Lights: used for VP10 standard lights, the fading is handled by VP itself




f27a.x = 827.3388
f27a.y = 1340.21
f27a.height = 135

f27b.x = 784.011
f27b.y = 628.534
f27b.height = 121.206
f27b.rotx = -9.1734

f28b.x = 516.72336052
f28b.y = 312.80047786
f28b.height = 111

f28a.x = 101.46988373
f28a.y = 46.54971291
f28a.height = 199


Gi_Ambient.x = 476.470
Gi_Ambient.y = 1004.902
Gig_Ambient.x = 476.470
Gig_Ambient.y = 1004.902

GIblue.x = 395
GIblue.y = 400

GI_Right.y = 1004.5
GI_Right.x = 958.82

GI_Left.y = 1004.5
GI_Left.x = -5.87

GI_Back.y = 0.1
GI_Back.x = 478.2149658

Gi_Sticker.x = 559
Gi_Sticker.y = 183.75
Gig_Sticker.x = 559
Gig_Sticker.y = 183.75

Gi_Upf.x = 290
Gi_Upf.y = 100
GiG_Upf.x = 290
GiG_Upf.y = 100

Gi_Shooter.x = 834.69
Gi_Shooter.y = 1313.123
GiG_Shooter.x = 834.69
GiG_Shooter.y = 1313.123


Gi_BumperShine.x = 357.5
Gi_BumperShine.y = 428
GiG_BumperShine.x = 357.5
GiG_BumperShine.y = 428

l42.x = 429.7
l42.y = 1578.7
f25.x = 429.7
f25.y = 1578.7

l4.x = 303.5
l4.y = 1419.5

l35.x = 802.25
l35.y = 1107.5

dim lastinput : lastinput = 0
Sub GItype(input)
	dim xg, xp, x, s, rnd
	Select Case input
		case 1	'2700k RGB
			xg = "GI" : xp = "GIP"
			gi_white.opacity = 75000*3
			gi_whiteP.opacity = 2500*3

			gi_left.imagea = "gil"
			gi_back.imagea = "gib"
			gi_right.imagea = "gir"
			gi_left.opacity = 250000 : gi_back.opacity = gi_left.opacity : gi_right.opacity = gi_left.opacity	'38000

			gi_upf.imagea = "gi_lvl2"
			gi_upf.opacity = 1750'1200
			gi_sticker.imagea = "gi_sticker"
			gi_sticker.opacity = 3500'1500
			gi_bumpershine.imagea = "gi_bumpershine"
'			gi_bumpershine.opacity = 1500
			gi_shooter.imagea = "gi_shooter"
'			gi_shooter.opacity = 1500

			ReflectColor(0) = 255
			ReflectColor(1) = 127
			ReflectColor(2) = 0
			for each x in ReflectionsGI : x.Color = RGB(ReflectColor(0),ReflectColor(1),ReflectColor(2) ) : next
'			for x = 0 to (gi2.count -1)
'				On Error Resume Next
'				GI2(x).color = save2700k(x, 0)
'				GI2(x).colorfull = save2700k(x, 1)
'				GI2(x).intensity = save2700k(x, 2)
'			Next
'			for x = 200 to 203		'GI relay on / off	Fading Speeds
'				FlashSpeedUp(x) = 0.01
'				FlashSpeedDown(x) = 0.008
'			Next
'			for x = 300 to 303		'GI	8 step modulation
'				FlashSpeedUp(x) = 0.01
'				FlashSpeedDown(x) = 0.008
'			Next
		case 2	'White
			xg = "GIv" : xp = "GIvP"
			gi_white.opacity = 75000
			gi_whiteP.opacity = 2500'4000

			gi_left.imagea = "gilv"
			gi_back.imagea = "gibv"
			gi_right.imagea = "girv"
'			gi_left.opacity = 38000 : gi_back.opacity = gi_left.opacity : gi_right.opacity = gi_left.opacity
			gi_left.opacity = 150000 : gi_back.opacity = gi_left.opacity : gi_right.opacity = gi_left.opacity

			gi_upf.imagea = "giv_upf"	'oddly named
			gi_upf.opacity = 1200
			gi_sticker.imagea = "giv_sticker"
			gi_sticker.opacity = 1500
			gi_bumpershine.imagea = "giv_bumpershine"
'			gi_bumpershine.opacity = 1500
			gi_shooter.imagea = "giv_shooter"
'			gi_shooter.opacity = 1500

			ReflectColor(0) = 255
			ReflectColor(1) = 255
			ReflectColor(2) = 255
			for each x in ReflectionsGI : x.Color = RGB(ReflectColor(0),ReflectColor(1),ReflectColor(2) ) : next


'			for each x in GI2
'				On Error Resume Next
'				s = mid(x.name, 3, 1)
'				if s = "t" then 'transmit
'					x.Color = RGB(44, 58, 77)
'					x.ColorFull = RGB(188, 188, 155)
'				end If
'			Next
'			git3.colorfull = rgb(255,239,232)	'bulbs
'			git3.color = rgb(253,227,151)
'			git4.colorfull = rgb(255,239,232)
'			git4.color = rgb(253,227,151)
'			git5.colorfull = rgb(255,239,232)
'			git5.color = rgb(253,227,151)
'			for x = 200 to 203		'GI relay on / off
'				FlashSpeedUp(x) = 0.014
'				FlashSpeedDown(x) = 0.014
'			Next
'			for x = 300 to 303		'GI	8 step modulation
'				FlashSpeedUp(x) = 0.014
'				FlashSpeedDown(x) = 0.014
'			Next
		case 0	'Random
			rnd = rndnum(1, 2)
			if rnd <> lastinput then GItype rnd else gitype 0 end if
			Exit Sub
		Case 5 'Sequential
			If LastInput > 1 then LastInput = 0
			gitype (Lastinput+1)
			Exit Sub
		Case Else
			Gitype 0 : exit sub
	End Select
'	dim temp
'	temp = lastinput
	lastinput = input
	GI_white.ImageA = xg
	GI_whitep.ImageA = xp

'	tb.text = temp & " " & input	'debug
End Sub

'Ballshadow routine by Ninuzzu

Dim BallShadow
BallShadow = Array (BallShadow1, BallShadow2, BallShadow3, BallShadow4, BallShadow5, BallShadow6)

Sub UpdateBallShadow()	'called by -1 lamptimer
	On Error Resume Next
    Dim BOT, b
    BOT = GetBalls
	dim CenterPoint : CenterPoint = 425'Table1.Width/2

	' render the shadow for each ball
    For b = 0 to UBound(BOT)
		If BOT(b).X < CenterPoint Then
			BallShadow(b).X = ((BOT(b).X) - (50/6) + ((BOT(b).X - (CenterPoint))/7)) + 10
		Else
			BallShadow(b).X = ((BOT(b).X) + (50/6) + ((BOT(b).X - (CenterPoint))/7)) - 10
		End If

			BallShadow(b).Y = BOT(b).Y + 20
			BallShadow(b).Z = 1
		If BOT(b).Z > 20 Then
			BallShadow(b).visible = 1
		Else
			BallShadow(b).visible = 0
		End If
	Next
End Sub




	'Fastflips Pinmame callback bypass
	Set FastFlips = new cFastFlips
	with FastFlips
		.CallBackL = "SolLflipper"	'set flipper sub callbacks
		.CallBackR = "SolRflipper"	'...
		'.CallBackUL = "SolULflipper"'...(upper flippers, if needed)
		'.CallBackUR = "SolURflipper"'...
		.TiltObjects = True 'Optional, if True calls vpmnudge.solgameon automatically
		.Delay = 0			'Optional, if > 0 adds some compensation for solenoid jitter (occasional problem on Bram Stoker's Dracula)
		.Debug = False		'Debug, always-on flippers. Call FastFlips.Debug True or False in debugger to enable/disable.
	end with



'cFastFlips by nFozzy
'Bypasses pinmame callback for faster and more responsive flippers
'Version 0.1
'todo replace delay timer with vpmtimer

'Flipper / game-on Solenoid # reference:
'Williams System 11: Sol23 or 24
'Gottlieb System 3: Sol32
'Data East (pre-whitestar): Sol23 or 24
'WPC 90, 92', WPC Security : Sol31

'********************Setup*******************:

'....top of script....
'dim FastFlips

'....init....
'Set FastFlips = new cFastFlips
'with FastFlips
'	.CallBackL = "SolLflipper"	'set flipper sub callbacks
'	.CallBackR = "SolRflipper"	'...
'	.CallBackUL = "SolULflipper"'...(upper flippers, if needed)
'	.CallBackUR = "SolURflipper"'...
'	.TiltObjects = True 'Optional, if True calls vpmnudge.solgameon automatically
'	.Delay = 0			'Optional, if > 0 adds some compensation for solenoid jitter (occasional problem on Bram Stoker's Dracula)
'	.Debug = False		'Debug, always-on flippers. Call FastFlips.Debug True or False in debugger to enable/disable.
'end with

'IF USING DELAY > 0 create a timer object called FastFlipsTimer and add this additional bit of script anywhere:
'Sub FastFlipsTimer_Timer() 
'	FastFlips.CutFlippers False
'	me.Enabled = False
'End Sub

'...keydown section... (comment out the upper flippers as needed)
' If KeyCode = LeftFlipperKey then FastFlips.FlipL True :  FastFlips.FlipUL True
' If KeyCode = RightFlipperKey then FastFlips.FlipR True :  FastFlips.FlipUR True
'(Do not use Exit Sub, this script does not handle switch handling at all!)

'...keyUp section...
' If KeyCode = LeftFlipperKey then FastFlips.FlipL False :  FastFlips.FlipUL False
' If KeyCode = RightFlipperKey then FastFlips.FlipR False :  FastFlips.FlipUR False

'...Flipper Callbacks....
'if pinmame flipper callbacks are in use, comment them out (for example 'SolCallback(sLRFlipper)	)
'Use these subs (the ones defined in CallBackL / CallBackR) to handle flipper rotation and sounds

'...Solenoid...
'SolCallBack(31) = "FastFlips.Flippers_On"
'//////for a reference of solenoid numbers, see top /////

'*************************************************

Class cFastFlips
	Public Delay, TiltObjects, Debug
	Private SubL, SubUL, SubR, SubUR, FlippersEnabled
	
	Private Sub Class_Initialize()
		Delay = 0 : FlippersEnabled = False : Debug = False
	End Sub
	
	'set callbacks
	Public Property Let CallBackL(aInput)  : Set SubL  = GetRef(aInput) : End Property
	Public Property Let CallBackUL(aInput) : Set SubUL = GetRef(aInput) : End Property
	Public Property Let CallBackR(aInput)  : Set SubR  = GetRef(aInput) : End Property
	Public Property Let CallBackUR(aInput) : Set SubUR = GetRef(aInput) : End Property

	'call callbacks
	Public Sub FlipL(aEnabled)
		if not FlippersEnabled and not debug then Exit Sub
		subL aEnabled
	End Sub
	
	Public Sub FlipR(aEnabled)
		if not FlippersEnabled and not debug then Exit Sub
		subR aEnabled
	End Sub

	Public Sub FlipUL(aEnabled)
		if not FlippersEnabled and not debug then Exit Sub
		subUL aEnabled
	End Sub	

	Public Sub FlipUR(aEnabled)
		if not FlippersEnabled and not debug then Exit Sub
		subUR aEnabled
	End Sub	
	
	Public Sub Flippers_On(aEnabled)	'Handle solenoid
		if delay > 0 and not aEnabled then 	'handle delay
			FastFlipsTimer.Interval = delay
			FastFlipsTimer.Enabled = Not aEnabled
		else
			CutFlippers(aEnabled)
		end if
	End Sub
	
	Public Sub CutFlippers(aEnabled)
		FlippersEnabled = aEnabled
		if TiltObjects then vpmnudge.solgameon aEnabled
		If Not aEnabled then 'todo delay
			subL False
			subR False
			if not IsEmpty(subUL) then subUL False
			if not IsEmpty(subUR) then subUR False
		End If		
	End Sub
	
End Class





















'FlipperPolarity 0.10

'No longer includes overall speedhack because it was redundant with the velocity correction stuff. 
'If you are experiencing flipper lag please rig up solenoids on a 1-interval timer, or use a flipper solenoid if possible!!


'Setup -                                                             
'Triggers tight to the flippers TriggerLF and TriggerRF. Timers as low as possible
'Debug box TBpl (for .debug = True)


dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity
InitPolarity
Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		'safety coefficient (diminishes polarity correction only)
		'x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 0	'don't mess with these
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1	'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1

		x.enabled = True
		'x.DebugOn = True : stickL.visible = True : tbpl.visible = True : fastflips.Debug = True
		x.TimeDelay = 44
	Next
	
	'Space Station 1.12D
	'rf.report "Polarity"
	AddPt "Polarity", 0, 0, 0
	AddPt "Polarity", 1, 0.368, -4
	AddPt "Polarity", 2, 0.451, -3.7
	AddPt "Polarity", 3, 0.493, -3.88
	AddPt "Polarity", 4, 0.65, -2.3
	AddPt "Polarity", 5, 0.71, -2
	AddPt "Polarity", 6, 0.785,-1.8
	AddPt "Polarity", 7, 1.18, -1
	AddPt "Polarity", 8, 1.2, 0


	'rf.report "Velocity"
	addpt "Velocity", 0, 0, 	1
	addpt "Velocity", 1, 0.16, 1.06
	addpt "Velocity", 2, 0.41, 	1.05
	addpt "Velocity", 3, 0.53, 	1'0.982
	addpt "Velocity", 4, 0.702, 0.968
	addpt "Velocity", 5, 0.95,  0.968
	addpt "Velocity", 6, 1.03, 	0.945

	'rf.print "Polarity"




	LF.Object = LeftFlipper	
	LF.EndPoint = EndPointLp	'you can use just a coordinate, or an object with a .x property. Using a couple of simple primitive objects
	RF.Object = RightFlipper
	RF.EndPoint = EndPointRp
End Sub

Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub


Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub















'Methods:
'.TimeDelay - Delay before trigger shuts off automatically. Default = 80 (ms)
'.AddPoint - "Polarity", "Velocity", "Ycoef" coordinate points. Use one of these 3 strings, keep coordinates sequential. x = %position on the flipper, y = output
'.Object - set to flipper reference. Optional.
'.StartPoint - set start point coord. Unnecessary, if .object is used.

'Called with flipper - 
'ProcessBalls - catches ball data. 
' - OR - 
'.Fire - fires flipper.rotatetoend automatically + processballs. Requires .Object to be set to the flipper.


'Trigger Hit - .AddBall activeball
'Trigger UnHit - .PolarityCorrect activeball

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt	'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay	'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart, FlipperEnd, LR, PartialFlipCoef
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
	Public Property Let EndPoint(aInput) : if IsObject(aInput) then FlipperEnd = aInput.x else FlipperEnd = aInput : end if : End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property
	
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
				if DebugOn then StickL.visible = True : StickL.x = balldata(x).x		'debug TODO
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function	'Timer shutoff for polaritycorrect
	
	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then 
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1
			dim teststr : teststr = "Cutoff"
			tmp = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
			if tmp < 0.1 then 'if real ball position is behind flipper, exit Sub to prevent stucks	'Disabled 1.03, I think it's the Mesh that's causing stucks, not this
				if DebugOn then TestStr = "real pos < 0.1 ( " & round(tmp,2) & ")" : tbpl.text = Teststr 
				'RemoveBall aBall
				'Exit Sub
			end if

			'y safety Exit
			if aBall.VelY > -8 then 'ball going down
				if DebugOn then teststr = "y velocity: " & round(aBall.vely, 3) & "exit sub" : tbpl.text = teststr
				RemoveBall aBall
				exit Sub
			end if
			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					'TB.TEXT = balldata(x).id & " " & BALLDATA(X).X & VBNEWLINE & FLIPPERSTART & " " & FLIPPEREND
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)				'find safety coefficient 'ycoef' data
				end if
			Next

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				if DebugOn then set tmp = new spoofball : tmp.data = aBall : End If
				if IsEmpty(BallData(idx).id) and aBall.VelY < -12 then 'if tip hit with no collected data, do vel correction anyway
					if PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1) > 1.1 then 'adjust plz
						VelCoef = LinearEnvelope(5, VelocityIn, VelocityOut)
						if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
						if Enabled then aBall.Velx = aBall.Velx*VelCoef'VelCoef
						if Enabled then aBall.Vely = aBall.Vely*VelCoef'VelCoef
						if DebugOn then teststr = "tip protection" & vbnewline & "velcoef: " & round(velcoef,3) & vbnewline & round(PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1),3) & vbnewline
						'debug.print teststr
					end if
				Else
		 : 			VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
					if Enabled then aBall.Velx = aBall.Velx*VelCoef
					if Enabled then aBall.Vely = aBall.Vely*VelCoef
				end if
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1	'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
			'debug
			if DebugOn then
				TestStr = teststr & "%pos:" & round(BallPos,2)
				if IsEmpty(PolarityOut(0) ) then 
					teststr = teststr & vbnewline & "(Polarity Disabled)" & vbnewline
				else 
					teststr = teststr & "+" & round(1 *(AddX*ycoef*PartialFlipcoef),3)
					if BallPos >= PolarityOut(uBound(PolarityOut) ) then teststr = teststr & "(MAX)" & vbnewline else teststr = teststr & vbnewline end if	
					if Ycoef < 1 then teststr = teststr &  "ycoef: " & ycoef & vbnewline
					if PartialFlipcoef < 1 then teststr = teststr & "PartialFlipcoef: " & round(PartialFlipcoef,4) & vbnewline				
				end if

				teststr = teststr & vbnewline & "Vel: " & round(BallSpeed(tmp),2) & " -> " & round(ballspeed(aBall),2) & vbnewline
				teststr = teststr & "%" & round(ballspeed(aBall) / BallSpeed(tmp),2)
				tbpl.text = TestSTR
			end if
		Else
			'if DebugOn then tbpl.text = "td" & timedelay
		End If
		RemoveBall aBall
	End Sub
End Class

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

Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub

'Variable Envelope. Infinite amount of points!
'Keep keyframes in order, bounds equal, and all that.
'	L1			L2		  L3	    L4		L5... etc...	  L(uBound(xKeyFrame)
'          .
'        /  \
'      /      \				   .
'    /          \           _/  \__
'  /              \       _/       \__
'/                  \ . /             \__
'0.........1..........2........3.........4..........etc........uBound(xKeyFrame)


'in animation's case, xInput = MS
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)	'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	'Clamp if on the boundry lines
	'if L=1 and Y < yLvl(LBound(yLvl) ) then Y = yLvl(lBound(yLvl) )
	'if L=uBound(xKeyFrame) and Y > yLvl(uBound(yLvl) ) then Y = yLvl(uBound(yLvl) )
	'clamp 2.0
	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper

	LinearEnvelope = Y
End Function

Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function


'======================================

















'====================
'Class jungle nf
'=============

'No-op object instead of adding more conditionals to the main loop
'It also prevents errors if empty lamp numbers are called, and it's only one object
'should be g2g?

Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class

'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs

Class LampFader
	Public FadeSpeedDown(140), FadeSpeedUp(140)
	Private Lock(140), Loaded(140), OnOff(140)
	Public UseFunction
	Private cFilter
	Private UseCallback(140), cCallback(140)
	Public Lvl(140), Obj(140)
	Private Mult(140)
	Public FrameTime
	Private InitFrame

	Sub Class_Initialize()
		InitFrame = 0
		dim x : for x = 0 to uBound(OnOff) 	'Set up fade speeds
			FadeSpeedDown(x) = 1/100	'fade speed down
			FadeSpeedUp(x) = 1/80		'Fade speed up
			UseFunction = False
			lvl(x) = 0
			OnOff(x) = False
			Lock(x) = True : Loaded(x) = False
			Mult(x) = 1
		Next

		for x = 0 to uBound(OnOff) 		'clear out empty obj
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		Next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property		'debug.print Lampz.Locked(100)	'debug
	Public Property Get state(idx) : state = OnOff(idx) : end Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property

	Public Property Let state(ByVal idx, input) 'Major update path
		if Input <> OnOff(idx) then  'discard redundant updates
			OnOff(idx) = input
			Lock(idx) = False 
			Loaded(idx) = False
		End If
	End Property

	'Mass assign, Builds arrays where necessary
	Sub MassAssign(aIdx, aInput)
		If typename(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
			if IsArray(aInput) then
				obj(aIdx) = aInput
			Else
				Set obj(aIdx) = aInput
			end if
		Else
			Obj(aIdx) = AppendArray(obj(aIdx), aInput)
		end if	
	end Sub

	Sub SetLamp(aIdx, aOn) : state(aIdx) = aOn : End Sub	'Solenoid Handler

	Public Sub TurnOnStates()	'If obj contains any light objects, set their states to 1 (Fading is our job!)
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then 
				'debugstr = debugstr & "array found at " & idx & "..."
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x)' : debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					tmp(x).intensityscale = 0.0001 ' this can prevent init stuttering
				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx)' : debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				obj(idx).intensityscale = 0.0001 ' this can prevent init stuttering				
			end if
		Next
		'debug.print debugstr
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'Just runs TurnOnStates right now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property 

	Public Sub Update1()	 'Handle all boolean numeric fading. If done fading, Lock(x) = True. Update on a '1' interval Timer!
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx : for x = 0 to uBound(OnOff)
			if not Loaded(x) then
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then 
						for each xx in obj(x) : xx.IntensityScale = cFilter(Lvl(x)*Mult(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x)*Mult(x) : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then 
						obj(x).Intensityscale = cFilter(Lvl(x)*Mult(x))
					Else
						obj(x).Intensityscale = Lvl(x)
					End If
				end if
				If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x))	'Callback
				If Lock(x) Then
					if Lvl(x) = 1 or Lvl(x) = 0 then Loaded(x) = True	'finished fading
				end if
			end if
		Next
	End Sub
End Class


'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs

Class DynamicLamps 'Lamps that fade up and down. GI and Flasher handling
	Public Loaded(50), FadeSpeedDown(50), FadeSpeedUp(50)
	Private Lock(50), SolModValue(50)
	Private UseCallback(50), cCallback(50)
	Public Lvl(50)
	Public Obj(50)
	Private UseFunction, cFilter
	private Mult(50)

	Public FrameTime
	Private InitFrame

	Private Sub Class_Initialize() 
		InitFrame = 0
		dim x : for x = 0 to uBound(Obj) 
			FadeSpeedup(x) = 0.01 
			FadeSpeedDown(x) = 0.01 
			lvl(x) = 0.0001 : SolModValue(x) = 0
			Lock(x) = True : Loaded(x) = False
			mult(x) = 1
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property
	Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property

	Public Property Let State(idx,Value)
		'If Value = SolModValue(idx) Then Exit Property ' Discard redundant updates
		If Value <> SolModValue(idx) Then ' Discard redundant updates
			SolModValue(idx) = Value
			Lock(idx) = False : Loaded(idx) = False
		End If
	End Property
	Public Property Get state(idx) : state = SolModValue(idx) : end Property

	'Mass assign, Builds arrays where necessary
	Sub MassAssign(aIdx, aInput)
		If typename(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
			if IsArray(aInput) then
				obj(aIdx) = aInput
			Else
				Set obj(aIdx) = aInput
			end if
		Else
			Obj(aIdx) = AppendArray(obj(aIdx), aInput)
		end if	
	end Sub

	'solcallback (solmodcallback) handler 
	Sub SetLamp(aIdx, aInput) : state(aIdx) = aInput : End Sub	'0->1 Input
	Sub SetModLamp(aIdx, aInput) : state(aIdx) = aInput/255 : End Sub	'0->255 Input
	Sub SetGI(aIdx, ByVal aInput) : if aInput = 8 then aInput = 7 end if : state(aIdx) = aInput/7 : End Sub	'0->8 WPC GI input 

	Public Sub TurnOnStates()	'If obj contains any light objects, set their states to 1 (Fading is our job!)
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then 
				'debugstr = debugstr & "array found at " & idx & "..."
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x) ': debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					
				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx) ': debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				
			end if
		Next
		'debug.print debugstr
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'just call turnonstates for now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property 

	Public Sub Update1()	 'Handle all numeric fading. If done fading, Lock(x) = True
		'dim stringer
		dim x : for x = 0 to uBound(Lvl)
			'stringer = "Locked @ " & SolModValue(x)
			if not Lock(x) then 'and not Loaded(x) then
				If lvl(x) < SolModValue(x) then '+
					'stringer = "Fading Up " & lvl(x) & " + " & FadeSpeedUp(x)
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) >= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				ElseIf Lvl(x) > SolModValue(x) Then '-
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					'stringer = "Fading Down " & lvl(x) & " - " & FadeSpeedDown(x)
					if Lvl(x) <= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True					
				End If
			end if
		Next
		'tbF.text = stringer
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		dim x : for x = 0 to uBound(Lvl)
			if not Lock(x) then 'and not Loaded(x) then
				If lvl(x) < SolModValue(x) then '+
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				ElseIf Lvl(x) > SolModValue(x) Then '-
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					if Lvl(x) <= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True					
				End If
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx
		for x = 0 to uBound(Lvl)
			if not Loaded(x) then
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then 
						for each xx in obj(x) : xx.IntensityScale = cFilter(Lvl(x)*mult(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x)*mult(x) : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then 
						obj(x).Intensityscale = cFilter(Lvl(x)*mult(x))
					Else
						obj(x).Intensityscale = Lvl(x)*mult(x)
					End If
				end if
				If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x)*mult(x))	'Callback
				If Lock(x) Then
					Loaded(x) = True
				end if
			end if
		Next
	End Sub
End Class


'Helper function 
Function AppendArray(ByVal aArray, aInput)	'append one value, object, or Array onto the end of a 1 dimensional array
	if IsArray(aInput) then 'Input is an array...
		dim tmp : tmp = aArray
		If not IsArray(aArray) Then	'if not array, create an array
			tmp = aInput
		Else						'Append existing array with aInput array
			Redim Preserve tmp(uBound(aArray) + uBound(aInput)+1)	'If existing array, increase bounds by uBound of incoming array
			dim x : for x = 0 to uBound(aInput)
				if isObject(aInput(x)) then 
					Set tmp(x+uBound(aArray)+1 ) = aInput(x)
				Else
					tmp(x+uBound(aArray)+1 ) = aInput(x)
				End If		
			Next
		AppendArray = tmp	 'return new array
		End If
	Else 'Input is NOT an array...
		If not IsArray(aArray) Then	'if not array, create an array
			aArray = Array(aArray, aInput)
		Else
			Redim Preserve aArray(uBound(aArray)+1)	'If array, increase bounds by 1
			if isObject(aInput) then 
				Set aArray(uBound(aArray)) = aInput
			Else
				aArray(uBound(aArray)) = aInput
			End If
		End If
		AppendArray = aArray 'return new array
	End If
End Function





'Flipper debug scripts (T2)



'Physics debug helpers
'Physics debug helpers
'Physics debug helpers
'Physics debug helpers
'======================


'Sub PlungerTrigger_Hit() : if activeball.vely < 0 then TBPP.text = round(BallSpeed(activeball),2) : End If : End Sub

Sub TBPprint_Timer() : me.timerenabled = False : me.text = "" : end Sub
'www.shodor.org/interactivate/activities/SimplePlot/

dim tdir, tforce : tdir = 0 : tforce = 0
sub kas(aDir, aForce) : tdir = aDir : tforce = aForce : end Sub 'sets tdir and tforce

'kas 90, 0.1 : ka k, 426.8965, 1869 : coef=1
'kas 90, 0.15 : ka k, 426.8965, 1869 : coef=0.95	'shitty middle post slow
'kas 90, 0.15 : ka k, 309.4953, 502.0567 : coef=1	'drop test

'kas 270, 15 : ka k, 439.8656, 563.9827 : coef=1	'bumper kick
'kas 270, 15 : ka k, 460.7, 823.1 : coef=1	'1h4350

'Sub Bumpers_Hit(idx) : cortest activeball, "Bumper" : End Sub

sub ka(aObj, aX, aY) 'drop a ball from this specific position
	if not TypeName(aObj) = "Kicker" then exit sub 
	aObj.createsizedballwithmass ballsize/2, ballmass 
	dim b : set b = aObj.LastCapturedBall 
	if aX <> 0 then b.x = aX end if : if aY <> 0 then b.y = aY end if
	b.ReflectionEnabled = False
	aObj.kick tDir, tForce
end Sub


sub kap(aDelay)	'flip after this MS delay, Left
	TBPF.text = "Ldelay " & aDelay
	k.createsizedballwithmass ballsize/2, ballmass 
	dim b : set b = k.LastCapturedBall 
	b.x = leftflipper.x+17 : b.y = 1740 : k.kick 0, 0
	SolLFlipper True

	nftimer.addtimer 2300, "SolLFlipper False'"
	nftimer.addtimer 2300+aDelay, "SolLFlipper True : exclamate TBPF :"
	nftimer.addtimer 2400+aDelay, "SolLFlipper False'"
End Sub

sub kapf(aDelay)	'flip after this MS delay, Right
	TBPF1.text = "Rdelay " & aDelay
	k.createsizedballwithmass ballsize/2, ballmass 
	dim b : set b = k.LastCapturedBall 
	b.x = RightFlipper.x-17 : b.y = 1740 : k.kick 0, 0
	SolRFlipper True

	nftimer.addtimer 2100, "SolRflipper False" 
	nftimer.addtimer 2100+aDelay, "SolRFlipper True : exclamate TBPF1"
	nftimer.addtimer 2200+aDelay, "SolRFlipper False"
End Sub

Sub exclamate(aTB) : aTb.text = aTb.text & "!" : End Sub

'kapp 49 '=50 pretty random
'kapp 50.06 '=48
'kappf 71.5 'DT	vel 51?
'kapp 87.25 '% <50 (ramp) ill...
'kapp 94 '% =46 (late ramp)
'kapp 102.5 '% =41 (gate)

dim kappStart, kappPos
Sub Kapp(aPos)			'flip at this specific flipper position, left
	kappPos = aPos/100
	if IsEmpty(kappstart) then 
		kappstart = gametime
		TBPF.text = "Lpos " & Round(aPos,3) & "%"
		k.createsizedballwithmass ballsize/2, ballmass 
		dim b : set b = k.LastCapturedBall 
		b.x = leftflipper.x+17 : b.y = 1740 : k.kick 0, 0
		SolLFlipper True
		nftimer.addtimer 2100, "SolLflipper False" 
		kappt.Enabled = 1
	Else
		tbpf.text = "Lpos wait a minute"
	end If
End Sub
Sub kappt_timer()
	if gametime <= kappstart + 2000 Then
		tbpf.text = lf.pos
	Elseif gametime <= kappstart + 3000 Then
		if lf.pos >= kappPos then
			SolLflipper True
			nftimer.addtimer 100, "SolLflipper False" 
			tbpf.text = round(lf.pos,2) & ">" & vbnewline & round(kapppos,2) & "!"
			kappstart = Empty : me.Enabled = 0
		end If
	Else
		kappstart = Empty : me.Enabled = 0
	end If
End Sub

dim kappfStart, kappfPos
Sub Kappf(aPos)	'put in a percentage flipper position, right
	kappfPos = aPos/100
	if IsEmpty(kappfstart) then 
		kappfstart = gametime
		TBPF1.text = "Rpos " & round(aPos,3) & "%"
		k.createsizedballwithmass ballsize/2, ballmass 
		dim b : set b = k.LastCapturedBall 
		b.x = Rightflipper.x-17 : b.y = 1740 : k.kick 0, 0
		SolRFlipper True
		nftimer.addtimer 2100, "SolRflipper False" 
		kapptf.Enabled = 1
	Else
		tbpf1.text = "Rpos wait a minute"
	end If
End Sub
Sub kapptf_timer()
	if gametime <= kappfstart + 2000 Then
		tbpf.text = lf.pos
	elseif gametime <= kappfstart + 4000 Then
		if rf.pos >= kappfPos then
			SolRflipper True
			nftimer.addtimer 100, "SolRflipper False" 
			tbpf1.text = round(Rf.pos,2) & ">" & vbnewline & round(kappFpos,2) & "!"
			kappfstart = Empty : me.Enabled = 0
		end If
	Else
		kappfstart = Empty : me.Enabled = 0
	end If
End Sub


Sub nftimertimer_Timer():NFtimer.Update : End Sub

Class TimerEntry 
	public command, starttime, endtime 
	Public Sub Fire : execute command : Reset : end Sub
	public sub Reset : command = 0 : starttime = 0 : endtime = 0 : End Sub
End Class
dim NFtimer : set NFtimer = new cNFtimer
Class cNFtimer	'fast update timer object similar to cvpmtimer (used for debug only)
	Public PulseDuration	'pulseSw duration
	private queue(99), count
	private sub Class_Initialize : PulseDuration = 60 : count=0 : dim x : for x = 1 to uBound(queue) : set queue(x) = new TimerEntry : next : End Sub

	Public Sub AddTimer(aDelay, aStr)
		queue(Count+1).command = aStr : queue(count+1).StartTime = gametime : queue(count+1).EndTime = gametime + aDelay
		count = count + 1
	End Sub

	Public Sub PulseSw(aSw) : Controller.Switch(aSw) = 1 : AddTimer PulseDuration, "Controller.Switch(" & aSw & ") = 0" : End Sub

	Public Sub Update()
		if count then 
			dim i : for i = 1 to 99
				if queue(i).EndTime > 0 and queue(i).EndTime <= gametime then queue(i).Fire  : count = count - 1
			next
		end If
		'debugger
	End Sub

	Private Sub Debugger
		dim str,x
		for x = 1 to uBound(queue)
			if queue(x).endtime <> 0 then str = str & x & ":" & queue(x).endtime - queue(x).starttime & " " & queue(x).command & vbnewline
		Next
		tbtimer.text = "count " & count & vbnewline & str	'resource heavy
	End Sub
End Class
'TBTimer.Text = ""







'Keyframe Animation Class

'Setup
'.Update1 - update logic. Use 1 interval
'.Update - update objects. recommended -1 interval
'.Update2 - TODO alternative, updates both on -1 TODO

'Properties
'.State - returns if animation state (true or False)
'.Addpoint - Add keyframes. 3 argument sub : Keyframe#, Time Value, Output Value. Keep keyframes sequential, and timeline straight.
'.Modpoint - Modify an existing point
'.Debug - display debug animation (set before .addpoint to get full debug info)
'.Play - Play Animation
'.Pause - Pause mid-animation
'.Callback - string. Sub to call when animation is updated, with one argument sending the interpolated animation info

'Events
'.Callback(argument) - whatever you set callback to. Manually attach animation to this value - ie Showframe, Height, RotX, RotY, whatever...

Class cAnimation
	Public DebugOn
	Private KeyTemp(99,1)
	Private Lock, Loaded, StopAnim, UpdateSub
	Private ms, lvl, KeyStep, KeyLVL 'make these private later
	private LoopAnim

	Private Sub Class_Initialize : redim KeyStep(99) : redim KeyLVL(99) : Lock = True : Loaded = True : ms = 0: End Sub

	Public Property Get State : State = not Lock : End Property
	Public Property Let CallBack(String) : UpdateSub = String : End Property

	public Sub AddPoint(aKey, aMS, aLVL)
		KeyTemp(aKey, 0) = aMS : KeyTemp(aKey, 1) = aLVL
		Shuffle aKey
	End Sub

	'  v  v   v    keyframes IDX / (0)
	'	  .
	'	 / \lvl (1)
	'___/	 \___
	'-----MS--------->

	'in -> AddPoint(KeyFrame#, 0) = KeyFrame(Time) 
	'in -> AddPoint(KeyFrame#, 1) = KeyFrame(LVL) 
	'	(1d array conversion)
	'into -> KeyStep(99)
	'into -> KeyLvl(99)
	Private Sub Shuffle(aKey) 'shuffle down keyframe data into 1d arrays 'this sucks, it does't actually shuffle anything
		redim preserve KeyStep(99) : redim preserve KeyLvl(99)
		dim str : str = "shuffling @ " & akey & vbnewline
		dim x : for x = 0 to uBound(KeyTemp)
			if KeyTemp(x,0) <> "" Then
				KeyStep(x) = KeyTemp(x,0) : KeyLvl(x) = KeyTemp(x,1)
			Else
				if x = 0 then msgbox "cAnimation error: Please start at keyframe 0!" : exit Sub
				redim preserve KeyStep(x-1) : redim preserve KeyLvl(x-1) : Exit For
			end If
		Next
		str = str & "uBound step:" & uBound(keystep) & vbnewline & "uBound KeyLvl:" & uBound(KeyLvl) & vbnewline
		If DebugOn then TBanima.text = str & "printing steps:" & vbnewline & PrintArray(keystep) & vbnewline & "printing step values:" & vbnewline & PrintArray(keylvl)
	End Sub

	Private function PrintArray(aArray)	'debug
		dim str, x : for x = 0 to uBound(aArray) : str = str & x & ":" & aArray(x) & vbnewline : Next : printarray = str
	end Function

	Public Sub ModPoint(idx, aMs, aLvl) : KeyStep(idx) = aMs : KeyLVL(idx) = aLvl : End Sub  'modify a point after it's set

	Public Sub Play()	: StopAnim = False : Lock = False : Loaded = False : LoopAnim = False :  End Sub 'play animation
	Public Sub PlayLoop()	: StopAnim = False : Lock = False : Loaded = False : LoopAnim = True: End Sub 'play animation
	Public Sub Pause()	: StopAnim = True : end Sub	'pause animation


	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		'FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		if not lock then
			if ms > keystep(uBound(keystep)) then 
				If LoopAnim then ms = 0 else StopAnim = True : ms = 0	'No looping
			End If
			if StopAnim then Lock = True	'if stopped by script or by end of animation
			if Not Lock Then ms = ms + 1*lampz.FrameTime : lvl = LinearEnvelope(ms, KeyStep, KeyLVL)
		end if
		Update
	End Sub

	Public Sub Update1()	'update logic
		if not lock then
			if ms > keystep(uBound(keystep)) then 
				If LoopAnim then ms = 0 else StopAnim = True : ms = 0	'No looping
			End If
			if StopAnim then Lock = True	'if stopped by script or by end of animation
			if Not Lock Then ms = ms + 1 : lvl = LinearEnvelope(ms, KeyStep, KeyLVL)
		end if
	End Sub

	Public Sub Update() 	'Update object
		if Not Loaded then
			if Lock then Loaded = True
			if DebugOn then dim str : str = "ms:" & ms & vbnewline & "lvl:" & lvl & vbnewline & _
									Lock & " " & loaded & vbnewline :	tbanim.text = str
			proc UpdateSub, lvl
		end if
	End Sub

End Class 

Sub Proc(string, Callback)	'proc using a string and one argument
	'On Error Resume Next
	dim p : Set P = GetRef(String) 
	P Callback
	If err.number = 13 then  msgbox "Proc error! No such procedure: " & vbnewline & string
	if err.number = 424 then msgbox "Proc error! No such Object"
End Sub







sub buttface()
	tb.text = "buttface " & gametime
End Sub




Dim GIc : set GIc = New GIcolorswapper

'GIcolorswapper class by nfozzy (0.01a)
'Changes GI colors. Does automatic luminance correction. 
'Be aware: this will change GI Intensity/Opacity values!

'Designed primarily for flashers with script-based fading routines.
'This doesn't handle Light objects and color/colorfull stuff well. 


'Methods:

' - Init - 
'Assign (Sub) 			'Array input. Assigns GI objects. If collection, use ColToArray function to conver to indexed array

' - Usage - 
'Color (Property)			'Input: RGB values in an array. Primary method of changing GI color. IE 'GIc.Color = Array(255,255,255)'
' - Alt Usage - 
'ColorAssign (Property)	'Assign colors for automatic color switching. Setup like this: GIc.ColorAssign(0) = Array(255,5,5)
'changeGI (Sub)			'No Arguments, swaps through the colors defined in ColorAssign sequentially


' - Extra features - 
'ColorRGB (Property) 	'Returns RGB value of last rgb

' - Saving and Loading - 
'Name (Public) 		'String input. Sets name of game in VPReg.stg. IE 'SpaceStationNF'
'Value (Public) 	'String input. Sets Key for color in VPReg.stg.IE 'LastGIcolor'
'SaveColors (Sub)	'No arguments. Call on Table1_exit() sub
'LoadColors (Sub)	'No arguments. Call on Table1_Init() sub


Class GIcolorswapper

	Public ObjArray, BaseOpacity, ColorsArray 'set private
	Public ColorSeq	'set private
	Public Name, Value 'save/load stuff
	Private LastColor	'gets saved on me.SaveColors
	Private cCallback 'will call this sub (just call, not execute!) when color updates


	Private Sub Class_Initialize
		Redim ColorsArray(0)
		ColorSeq = 0
	End Sub

	Public Sub Assign(aArray)
		if not isarray(aArray) then msgbox "GIcolorswapper 'assign' error, input must be an array" : exit Sub
		dim idx, a : a = aArray
		Redim BaseOpacity((uBound(a)))
		for idx = 0 to uBound(a) : 
			if typename(a(idx)) = "Flasher" then 
				BaseOpacity(idx) = a(idx).opacity 
			elseif typename(a(idx) ) = "Light" then
				BaseOpacity(idx) = a(idx).Intensity 
			end if 
		Next
		ObjArray = a
	End Sub


	Public Property Let ColorAssign(aIdx, aArray)
		if aIdx > uBound(ColorsArray) Then
			Redim Preserve ColorsArray(aIDX)
		end If
		if not IsArray(aArray) then msgbox "ColorAssign error, RGB input must be an array" & vbnlewline & " IE: ColorAssign(0) = Array(255,255,255)"
		ColorsArray(aIDX) = aArray '1d within 1d

	End Property
	Public Property Get ColorAssign(aIDX) : ColorAssign = Colorsarray(aIDX) : End Property
	Public Property Get ColorRGB	'return last RGB color 
		if IsArray(LastColor) Then ColorRGB = RGB(LastColor(0),LastColor(1),LastColor(2)) else ColorRGB = RGB(255,255,255) End If 
	End Property

	Public Property Let Color(aRGB)	'in - Array(R, G, B) (integers within array)
		if Not IsArray(aRGB) then debug.print "use an array, not an RGB function idiot!" : Exit Property
		UpdateColors aRGB
	End Property
	Public Property Get Color : Color = LastColor : End Property


	Public Property Let Callback(aStr) : Set cCallback = GetRef(aStr) : End Property

	Public Sub changeGI()	'swap through all colors in ColorAssign
		if ColorSeq = uBound(ColorsArray) then ColorSeq = 0 else ColorSeq = ColorSeq + 1
		dim tmp : tmp = ColorsArray(ColorSeq)	'pick the color array out of ColorsArray (1d within 1d)
		if IsEmpty(tmp) then msgbox "changeGI error: Index '" & ColorSeq & "' is empty" 
		UpdateColors tmp
	End Sub


	Private Sub UpdateColors(aRGB)
		LastColor = aRGB	' for SaveColors
		dim x: for x = 0 to uBound(ObjArray)
			if typename(ObjArray(x) ) = "Flasher" then 
				ObjArray(x).Opacity = BaseOpacity(x) * Lum(aRGB)
				'tb.text = "set" & ObjArray(x).name & "to " & BaseOpacity(x) & " * " & round(Lum(aRGB),4) & vbnewline & round(Lum(aRGB),3)
				ObjArray(x).color = rgb(argb(0), aRGB(1), aRGB(2) )
'				if ObjArray(x).name = "GIsideL" or ObjArray(x).name = "GIsideR" then	'T2 specific thing
'					if rgb(argb(0), aRGB(1), aRGB(2)) = 16777215 then   	'if white, slightly less boring reflection color
'						ObjArray(x).color = rgb(65,127,255)
'					End If
'				End If
			elseif Typename(ObjArray(x) ) = "Light" Then
				ObjArray(x).Intensity = BaseOpacity(x) * Lum(aRGB)
				if ObjArray(x).colorfull then ObjArray(x).colorfull = rgb(argb(0), aRGB(1), aRGB(2) )			
				if ObjArray(x).color then ObjArray(x).color = rgb(argb(0), aRGB(1), aRGB(2) )			
			end If
		Next
		if not IsEmpty(cCallback) then cCallback
		'tb.text = round(lumincoef,3)
	End Sub


	Public Function Lum(aRgb)	'Luminance. input: array, output: value between 0 and 1
		Lum = 255/(argb(0)*0.3 + argb(1)*0.59 + argb(2)*0.11)/1
	End Function



	'Save GI colors to VPReg.stg

	Public Sub SaveColors()
		if IsEmpty(Name) then msgbox "SaveColors error, 'name' is undefined" : Exit Sub
		if IsEmpty(Value) then msgbox "LoadColors error, 'Value' is undefined" : Exit Sub
		if IsEmpty(LastColor) then exit Sub
		SaveValue Name,Value, formatRGB(LastColor)
		'tb.text = "saving:" & vbnewline & formatRGB(GiColorL) & vbnewline & FormatRGB(GiColorR)
	End Sub

	Public Sub LoadColors()
		if IsEmpty(Name) then msgbox "LoadColors error, 'name' is undefined" : Exit Sub
		if IsEmpty(Value) then msgbox "LoadColors error, 'Value' is undefined" : Exit Sub
		if LoadValue(Name, Value) = "" then exit sub
		dim tmp : tmp = LoadValue(Name, Value)
		UpdateColors Array(mid(tmp, 1, 3),mid(tmp, 4, 3),mid(tmp, 7, 3))

	End Sub

	Private Function FormatRGB(Byval aArray)
		dim idx: for idx = 0 to 2 
			if aArray(idx) < 10 and len(aArray(idx)) = 1 then 
				aArray(idx) = "00" & aArray(idx)
			elseif aArray(idx) < 100 and Len(aArray(idx)) = 2 then
				'debug.print "array" & x & "(" & aArray(x) & ") < 100, adding a 0 before it"
				aArray(idx) = "0"  & aArray(idx)
			end if
		Next
		FormatRGB = aArray(0) & aArray(1) & aArray(2)
	End Function
'
'	function DeSat(ByVal aRGB, ByVal aSat)	'simple desaturation function (returns rgb)
'		dim r, g, b, L
'		L = 0.3*aRGB(0) + 0.59*aRGB(1) + 0.1*aRGB(2)
'		r = aRGB(0) + aSat * (L - aRGB(0))
'		g = aRGB(1) + aSat * (L - aRGB(1))
'		b = aRGB(2) + aSat * (L - aRGB(2))
'		'desat = array(r,g,b)	'return array 
'		desat = rgb(r,g,b)		'return rgb
'	End Function
'
'	dim GiColorL, GiColorR 'Save GIcolor in memory
'	Sub GIcolor(input)	'can input rgb in array form
'	'	dim aL   : aL = Array(gil, gilp, gihand)	'cut image swap stuff
'	'	dim aL1 : aL1 = Array("gil", "gilp" ,"gihand")
'	'	dim aR   : aR = Array(gir, girp1, girp2, girp3, girp_corner, girp_corner1, gihand1)
'	'	dim aR1 : aR1 = Array("gir", "girp1", "girp2","girp3","girp_corner","girp_corner","gihand")
'		dim x, c : c = Array(255,255,255)
'		if isarray(input) then 
'			c = input 
'		else
'			select case input
'				'gicolor array(255, 15, 100)	'pink meh
'				case 0	: c = Array(255, 162, 37)'(255, 127, 37)	'Warm White (LED)
'				case 1  : c = Array(255,255,255)	'white
'				case 2	: c = Array(255, 15, 3)		'Red flood 
'				'case 3	: c = Array(133, 11, 255)	'purple Flood
'				case 3	: c = Array(85,13,255)		'95% sat Violet
'				case 4	: c = Array(36, 54, 255)	'Blue
'				case 5	: c = Array(20, 255, 12)	'Green
'				case 6	: c = Array(5, 255, 127)	'aqua
'			end Select	'gicolor array(255,45,0) 'orange
'		end if
'		MatchColorWithLuminance Flashers.obj(2), c, gilevels1 : GiColorL = c
'		if not catchinput(1) then MatchColorWithLuminance Flashers.obj(4), c, gilevels2 : GiColorR = c
'	'	for x = 0 to ubound(aL)
'	'		if aL(x).imagea <> aL1(x) then aL(x).imagea = aL1(x)
'	'	Next
'	'	for x = 0 to ubound(aR)
'	'		if aR(x).imagea <> aR1(x) then aR(x).imagea = aR1(x)
'	'	Next
'	End Sub
		
	
	
End Class
















'Floating Text Class 0.01a by nFozzy

'--Setup--
'Sprites(idx)	- Input Array of flasher objects. Overfilled text will be cut off. 
'(Please add this first, and only add indexes sequentially. The more arrays indexed, the more text frames can be displayed)

'Size (Public)  - Adjusts the type spacing. (Default 30)
'RotX (Property)- Adjust RotX. 
'FadeSpeedUp 	- Adjust scrolling speed

'--Methods--
'TextAt	(Sub)	- Input String, X coord, Y Coord. Primary method. Displays text at this coordinate.

'---Fading updates--
'Update2 - Handles all fading. REQUIRES SCRIPT FRAMETIME CALCULATION!


Class FloatingText
	Private Count, Prfx
	public Size
	Public Frame, Text, lock, loaded, lvl, z 'arrays
	Public FadeSpeedUp

	Private Sub Class_Initialize 
		Redim Frame(0), Text(0), lock(0), loaded(0), lvl(0), z(0)
		FadeSpeedUp = 1/1500 
		lvl(0) = 0 : loaded(0) = 1
		Count = 0 : size = 30
	end sub

	Public Property Let RotX(aInput) 
		'dim debugstr
		dim tmp, x, xx : for each x in Frame 
			tmp = x
			if IsArray(tmp) then 
				for each xx in tmp
					xx.RotX = aInput
					'debugstr = debugstr & xx.name & ".rotX = " & aInput & "..." & vbnewline
				next
			Else
				'debugstr = debugstr & "...not any array..." & vbnewline
			end If
		Next
		'if tb.text <> debugstr then tb.text = debugstr
	End Property

	Public Property Let Sprites(aIdx, aArray)
		if IsArray(aArray) Then
			Count = aIdx
			Redim Preserve Frame(aIdx)
			Redim Preserve Text(aIdx)
			Redim Preserve lock(aIdx)
			Redim Preserve loaded(aIdx)
			Redim Preserve lvl(aIdx)
			Redim Preserve z(aIdx)
			
			Lvl(aIdx) = 0 : Loaded(aIDX) = 1
			Frame(aIDX) = aArray	'Char contains sprites in 1d array. Use local variables to access sprites.
			z(aIDX) = aArray(0).height
			'msgbox "assigning " & aidx & vbnewline & ubound(mask)
		Else
			msgbox "FloatingText Error, 'Sprites' must be an array!"
		End If

	End Property

	Public Property Get Sprites(aIDX) : Sprites = Frame(aIDX) : End Property

	Public Property Let Prefix(aStr) : Prfx = aStr : End Property
	Public Property Get Prefix : Prefix = prfx : End Property

	Private Function MaxIDX(byval aArray, byref index)	'max, but also returns Index number of highest
		dim idx, MaxItem', str
		for idx = 0 to uBound(aArray)
			if IsEmpty(MaxItem) then 
				if not IsEmpty(aArray(idx)) then 
					MaxItem = aArray(idx)
					index = idx
				end If
			end if
			if not IsEmpty(aArray(idx) ) then 
				If aArray(idx) > MaxItem then MaxItem = aArray(idx) : index = idx
			end If
		Next
		MaxIDX = MaxItem
	End Function 

	Public Sub TextAt(aStr, aX, aY)		'Position text
		dim idx, xx, tmp

		'Choose a frame to assign
		dim ChosenFrame 
		'Find the highest value in Lvl and return it as ChosenFrame
		Call MaxIDX(Lvl, ChosenFrame)

		'Update Position
		'0 '1 '2
		'a(0) = aX
		'a(1) = aX + Size * index
		Text(ChosenFrame) = aStr 
		tmp = Frame(ChosenFrame)		' tmp = Sprite array contained by char array
		for xx = 0 to uBound(tmp)
			tmp(xx).x = aX + (Size * xx) - (Len(aStr)*Size)/2	'len part centers text 
			tmp(xx).y = aY
		Next'

		'Update Text
		for idx = 0 to uBound(tmp)
			xx = Mid(aStr, idx+1, 1)
			if xx <> "" then
				tmp(idx).visible = True
				tmp(idx).ImageA = Prfx & xx
				tmp(idx).ImageB = ""
			Else
				tmp(idx).visible = False
			end If
		Next
		If TypeName(aStr) <> "String" then FormatNumbers aStr, tmp
	
		'start fading / floating up
		lock(ChosenFrame) = False : Loaded(chosenframe) = False : lvl(chosenframe) = 0
	End Sub

	Private Sub FormatNumbers(aStr, aArray)
		If Len(aStr) >12 then Commalate len(aStr)-12,aArray
		If Len(aStr) > 9 then Commalate len(aStr)-9, aArray
		If Len(aStr) > 6 then Commalate len(aStr)-6, aArray
		If Len(aStr) > 3 Then Commalate len(aStr)-3, aArray
	End Sub
		
	Private Sub Commalate(aIDX, aArray)
		if aIdx-1 > uBound(aArray) then Exit Sub
		aArray(aIdx-1).ImageB = Prfx & "Comma"
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		dim x : for x = 0 to Count
			if not Lock(x) then
				Lvl(x) = Lvl(x) + FadeSpeedUp * lampz.frametime	'TODO this requires frametime
				if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
			end if
		next
		Update
	End Sub

	Private Sub Update()	'Handle object updates
		dim x : for x = 0 to Count
			if not Loaded(x) then
				dim opacitycurve	'TODO section this off and make it a function or something
				if lvl(x) > 0.5 then 
					opacitycurve = pSlope(lvl(x), 0, 1, 1, 0)
				Else
					opacitycurve = 1
				end If

				dim xx
				for each xx in Frame(x)
					xx.height = z(x) + (lvl(x) * 100) 
					xx.IntensityScale = opacitycurve
				Next
				If Lock(x) Then
					if Lvl(x) = 1 then Loaded(x) = True	'finished fading
				end if
			end if
		next
	End Sub

End Class

'******************
' Rom sound ON wwhen close table
'******************
Sub Table1_Exit()
    Controller.Pause = False
    Controller.Stop
    Controller.Games("spstn_l5").Settings.Value("sound") = 1
    Controller.Games("spstn_l5").Settings.Value("samples") = 1
End Sub

