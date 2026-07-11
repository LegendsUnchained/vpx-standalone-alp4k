'**********************
'  Bad Cats(1989)
' VPX table by unclewilly, Brad1X, Clark Kent, Dark
' version 1.0
'**********************

Option Explicit
Randomize
Dim FlipLag
FlipLag = 1  'Enable/Disable FlipperLag Fix  0 is disable 1 is enable
Dim xx, DNS
Dns = table1.NightDay

If DNS <= 5 or DNS >= 75 then
	For each xx in aGiLights:xx.intensity = xx.intensity *(1-(DNS/100)):Next
	For each xx in aAllFlashers:xx.opacity = xx.opacity *(1-(DNS/100)):Next
	For each xx in AllLamps:xx.intensity = xx.intensity *(1-(DNS/100)):Next
	For each xx in TargetDropGi:xx.intensity = xx.intensity *(1-(DNS/100)):Next
else 
	If DNS <= 40 or DNS >= 80 Then
		For each xx in aGiLights:xx.intensity = xx.intensity *(.5-(DNS/100)):Next
		For each xx in aAllFlashers:xx.opacity = xx.opacity *(.5-(DNS/100)):Next
		For each xx in AllLamps:xx.intensity = xx.intensity *(.5-(DNS/100)):Next
	For each xx in TargetDropGi:xx.intensity = xx.intensity *(.5-(DNS/100)):Next
	else
		For each xx in aGiLights:xx.intensity = xx.intensity *(.7-(DNS/100)):Next
		For each xx in aAllFlashers:xx.opacity = xx.opacity *(.7-(DNS/100)):Next
		For each xx in AllLamps:xx.intensity = xx.intensity *(.7-(DNS/100)):Next
	For each xx in TargetDropGi:xx.intensity = xx.intensity *(.7-(DNS/100)):Next
	end if
end if



solGI 0

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01550000", "S11.vbs", 3.26

Dim bsTrough, bsDog, bsTrash, dtBird, dtMilk
Const cGameName = "bcats_l5"

Const UseSolenoids = 1
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 1
Const HandleMech = 0

'Standard Sounds
Const SSolenoidOn = "fx_Solenoid"
Const SSolenoidOff = ""
Const SCoin = "fx_Coin"
BSize = 25.5
BMass = 1.7

'************
' Table init.
'************
dim HiddenVar
If Table1.ShowDT = False then 
	HiddenVar = 0
Else
	HiddenVar = 0
end If

Sub Table1_Init
    vpmInit me

    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "BadCats, Williams 1989" & vbNewLine & "VPX table by unclewilly v.1.0"
        .Games(cGameName).Settings.Value("rol") = 0 '1= rotated display, 0= normal
        .HandleMechanics = 0
        .HandleKeyboard = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .ShowTitle = 0
        .Hidden = HiddenVar
        If Err Then MsgBox Err.Description
    End With
    On Error Goto 0
    Controller.Run

    'Nudging
    vpmNudge.TiltSwitch = swTilt
    vpmNudge.Sensitivity = 2
    'vpmNudge.TiltObj = Array(sw60, sw61, sw62, LeftSlingshot, RightSlingShot)

    'Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 0, 10, 0, 0, 0, 0, 0, 0
        .InitKick ballrelease, 90, 4
        .InitEntrySnd "fx_Solenoid", "fx_Solenoid"
        .InitExitSnd SoundFX("fx_ballrel", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .IsTrough = True
        .Balls = 1
    End With

    'Dog House hole
    Set bsDog = New cvpmBallStack
    With bsDog
        .InitSw 0, 22, 0, 0, 0, 0, 0, 0
        .InitKick Ralfie, 180, 25
        .InitEntrySnd "fx_kicker_enter", "fx_Solenoid"
        .InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .IsTrough = False
    End With

    'Trash hole
    Set bsTrash = New cvpmBallStack
    With bsTrash
        .InitSaucer Bin, 24, 79, 22
        .KickForceVar = 3
        .KickAngleVar = 2
        .InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_kicker", DOFContactors)
        .InitAddSnd SoundFX("fx_kicker_enter", DOFContactors)
        .CreateEvents "bsTrash", Bin
    End With

    'Droptargets
    set dtBird = new cvpmdroptarget
    With dtBird
        .InitDrop Array(sw25,sw26,sw27,sw28,sw29), Array(25, 26, 27, 28, 29)
        .Initsnd SoundFX("fx_droptarget", DOFContactors), SoundFX("fx_resetdrop", DOFContactors)
    End With

    set dtMilk = new cvpmdroptarget
    With dtMilk
        .InitDrop Array(sw37,sw38,sw39), Array(37, 38, 39)
        .Initsnd SoundFX("fx_droptarget", DOFContactors), SoundFX("fx_resetdrop", DOFContactors)
    End With

    ' Seafood Wheel
    Dim mSFWheelMech
    Set mSFWheelMech = New cvpmMech
    With mSFWheelMech
        .MType = vpmMechStepSol + vpmMechCircle + vpmMechLinear + vpmMechFast
        .Sol1 = 16
        .Sol2 = 15
        .Length = 200
        .Steps = 200
        .AddSw 44, 0, 99
        .Callback = GetRef("UpdateWheel")
        .Start
    End With

'Init VariTarget
    sw19w21.IsDropped = 1
    sw19w31.IsDropped = 1
    sw19w41.IsDropped = 1

    'Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1


If Table1.ShowDT = False then
	For each xx in SideRails:xx.Visible = False:Next
End If
End Sub

Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_exit:Controller.Stop:End Sub
'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
        If keycode = LeftFlipperKey Then
			If FlipLag = 1 then flipnf 0, 1
        end if
        If keycode = RightFlipperKey Then
            If FlipLag = 1 then flipnf 1, 1
        end if
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If keycode = PlungerKey Then PlaySound "fx_PlungerPull", 0, 1, 0.1, 0.05:Plunger.Pullback
    If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub table1_KeyUp(ByVal Keycode)
        If keycode = LeftFlipperKey Then
            If FlipLag = 1 then flipnf 0, 0
        end if
        If keycode = RightFlipperKey Then
            If FlipLag = 1 then flipnf 1, 0
        end if
    If keycode = PlungerKey Then PlaySound "fx_plunger", 0, 1, 0.1, 0.05:Plunger.Fire
    If vpmKeyUp(keycode) Then Exit Sub
End Sub

'*********
' Switches
'*********

'Slings & Rubbers
Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    PlaySound SoundFX("fx_slingshot_left", DOFContactors), 0, 1, -0.05, 0.05
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    vpmTimer.PulseSw 63
    LeftSlingShot.TimerEnabled = 1
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
    PlaySound SoundFX("fx_slingshot_right", DOFContactors), 0, 1, 0.05, 0.05
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 64
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing2.Visible = 0:Remk.RotX = -10:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

'Rubbers

Sub sw40_Hit():PlaySound "fx_Rubber", 0, 1, -0.1, 0.15::vpmTimer.PulseSw 40:End Sub
Sub sw33_Hit():PlaySound "fx_Rubber", 0, 1, 0.1, 0.15::vpmTimer.PulseSw 33:End Sub
Sub sw34_Hit():PlaySound "fx_Rubber", 0, 1, 0.1, 0.15::vpmTimer.PulseSw 34:End Sub


' Bumpers
Sub sw60_Hit:vpmTimer.PulseSw 60:PlaySound SoundFX("fx_bumper1", DOFContactors), 0, 1, -0.1, 0.15:End Sub
Sub sw61_Hit:vpmTimer.PulseSw 61:PlaySound SoundFX("fx_bumper3", DOFContactors), 0, 1, 0.1, 0.15:End Sub
Sub sw62_Hit:vpmTimer.PulseSw 62:PlaySound SoundFX("fx_bumper2", DOFContactors), 0, 1, 0, 0.15:End Sub


'Rollover & Ramp Switches
Sub sw11_Hit:Controller.Switch(11) = 1:PlaySound "fx_sensor", 0, 1, -0.1, 0.15:End Sub
Sub sw11_UnHit:Controller.Switch(11) = 0:End Sub

Sub sw12_Hit:Controller.Switch(12) = 1:PlaySound "fx_sensor", 0, 1, 0, 0.15:End Sub
Sub sw12_UnHit:Controller.Switch(12) = 0:End Sub

Sub sw13_Hit:Controller.Switch(13) = 1:PlaySound "fx_sensor", 0, 1, 0, 0.15:End Sub
Sub sw13_UnHit:Controller.Switch(13) = 0:End Sub

Sub sw14_Hit:Controller.Switch(14) = 1:PlaySound "fx_sensor", 0, 1, 0, 0.15:End Sub
Sub sw14_UnHit:Controller.Switch(14) = 0:End Sub

Sub sw36_Hit:Controller.Switch(36) = 1:PlaySound "fx_sensor", 0, 1, -0.1, 0.15:End Sub
Sub sw36_UnHit:Controller.Switch(36) = 0:End Sub

Sub sw30_Hit:Controller.Switch(30) = 1:PlaySound "fx_sensor", 0, 1, -0.1, 0.15:End Sub
Sub sw30_UnHit:Controller.Switch(30) = 0:End Sub

Sub sw31_Hit:Controller.Switch(31) = 1:PlaySound "fx_sensor", 0, 1, 0.1, 0.15:End Sub
Sub sw31_UnHit:Controller.Switch(31) = 0:End Sub

Sub sw35_Hit:Controller.Switch(35) = 1:PlaySound "fx_sensor", 0, 1, -0.1, 0.15:End Sub
Sub sw35_UnHit:Controller.Switch(35) = 0:End Sub

Sub sw41_Hit:Controller.Switch(41) = 1:sw41.Timerenabled = 1:PlaySound "fx_sensor", 0, 1, 0.1, 0.15:End Sub
Sub sw41_UnHit:Controller.Switch(41) = 0:End Sub
dim sw41Dir
sw41Dir = -1
Sub sw41_Timer()
	If sw41P.ObjRotZ = 60 then sw41Dir = 5
	If sw41P.ObjRotZ = 90 then sw41Dir = -5
	sw41P.ObjRotZ = sw41P.ObjRotZ + sw41Dir
	If sw41P.ObjRotZ = 90 then sw41.timerenabled = 0
End Sub

Sub sw43_Hit:Controller.Switch(43) = 1:sw43.Timerenabled = 1:PlaySound "fx_sensor", 0, 1, 0.1, 0.15:End Sub
Sub sw43_UnHit:Controller.Switch(43) = 0:End Sub
dim sw43Dir
sw43Dir = -1
Sub sw43_Timer()
	If sw43P.ObjRotZ = 60 then sw43Dir = 5
	If sw43P.ObjRotZ = 90 then sw43Dir = -5
	sw43P.ObjRotZ = sw43P.ObjRotZ + sw43Dir
	If sw43P.ObjRotZ = 90 then sw43.timerenabled = 0
End Sub

'Ramp Gates
Sub sw16_Hit:vpmTimer.PulseSw 16:End Sub

Sub sw23_Hit:vpmTimer.PulseSw 23:End Sub

' Linear Fish Target

Sub sw19_1_Hit:vpmTimer.PulseSw 19:PlaySound SoundFX("fx_target", DOFContactors), 0, 1, -0.1, 0.15:Fisht.transY = -50:sw19w11.IsDropped = 1:sw19w21.IsDropped = 0:End Sub

Sub sw19_2_Hit:vpmTimer.PulseSw 19:PlaySound SoundFX("fx_target", DOFContactors), 0, 1, -0.1, 0.15:Fisht.transY = -95:sw19w21.IsDropped = 1:sw19w31.IsDropped = 0:End Sub

Sub sw19_3_Hit:vpmTimer.PulseSw 19:PlaySound SoundFX("fx_target", DOFContactors), 0, 1, -0.1, 0.15:Fisht.transY = -135:sw19w31.IsDropped = 1:sw19w41.IsDropped = 0:End Sub

Sub sw19_4_Hit:vpmTimer.PulseSw 19:PlaySound SoundFX("fx_target", DOFContactors), 0, 1, -0.1, 0.15:End Sub

Sub sw19_1_UnHit
    If ActiveBall.VelY > 0 Then
		
        sw19w11.IsDropped = 0
        sw19w21.IsDropped = 1
    End If
End Sub

Sub sw19_2_UnHit
    If ActiveBall.VelY > 0 Then

        sw19w21.IsDropped = 0
        sw19w31.IsDropped = 1
    End If
End Sub

Sub sw19_3_UnHit
    If ActiveBall.VelY > 0 Then

        sw19w31.IsDropped = 0
        sw19w41.IsDropped = 1
    End If
End Sub

Sub sw19_4_UnHit
End Sub

Sub Fanimation_UnHit():FTTimer.Enabled = 1:End Sub

Sub FTTimer_Timer()
	If FishT.TransY < 0 Then
		FishT.TransY =FishT.TransY + 5
	Else
		FTTimer.enabled = 0
	end If
End Sub

'Droptargets VPX
Sub sw25_Hit:PlaySound SoundFX("fx_droptarget", DOFContactors), 0, 1, -0.1, 0.15:End Sub 'hit event only for the sound
Sub sw25_Dropped:dtBird.hit 1:If GIState=1 then:sw25l.State = 1:end if: sw25.Image = "BirdTD": End Sub

Sub sw26_Hit:PlaySound SoundFX("fx_droptarget", DOFContactors), 0, 1, -0.1, 0.15:End Sub 'hit event only for the sound
Sub sw26_Dropped:dtBird.hit 2:If GIState=1 then:sw26l.State = 1:end if:sw26.Image = "BirdTD": End Sub

Sub sw27_Hit:PlaySound SoundFX("fx_droptarget", DOFContactors), 0, 1, -0.1, 0.15:End Sub 'hit event only for the sound
Sub sw27_Dropped:dtBird.hit 3:If GIState=1 then:sw27l.State = 1:end if:sw27.Image = "BirdTD": End Sub

Sub sw28_Hit:PlaySound SoundFX("fx_droptarget", DOFContactors), 0, 1, -0.1, 0.15:End Sub 'hit event only for the sound
Sub sw28_Dropped:dtBird.hit 4:If GIState=1 then:sw28l.State = 1:end if:sw28.Image = "BirdTD": End Sub

Sub sw29_Hit:PlaySound SoundFX("fx_droptarget", DOFContactors), 0, 1, -0.1, 0.15:End Sub 'hit event only for the sound
Sub sw29_Dropped:dtBird.hit 5:sw29.Image = "BirdTD": End Sub

Sub sw37_Hit:PlaySound SoundFX("fx_droptarget", DOFContactors), 0, 1, 0.1, 0.15:End Sub 'hit event only for the sound
Sub sw37_Dropped:dtMilk.hit 1:If GIState=1 then:sw37l.State = 1:end if:sw37.Image = "MilkD": End Sub

Sub sw38_Hit:PlaySound SoundFX("fx_droptarget", DOFContactors), 0, 1, 0.1, 0.15:End Sub 'hit event only for the sound
Sub sw38_Dropped:dtMilk.hit 2:If GIState=1 then:sw38l.State = 1:end if:sw38.Image = "MilkD": End Sub

Sub sw39_Hit:PlaySound SoundFX("fx_droptarget", DOFContactors), 0, 1, 0.1, 0.15:End Sub 'hit event only for the sound
Sub sw39_Dropped:dtMilk.hit 3:If GIState=1 then:sw39l.State = 1:end if:sw39.Image = "MilkD": End Sub

' Drain & holes
Sub Bin_Hit():BsTrash.AddBall 0:End Sub
Sub Drain_Hit:Playsound "fx_drain":bsTrough.AddBall Me:End Sub
Sub Ralfie_Hit:Playsound "fx_scoop", 0, 1, 0.05, 0.05:bsDog.AddBall Me:End Sub

'  Ramp Helpers
Sub LHelp_Hit():Playsound "fx_balldrop", 0, 1, -0.05, 0.05:end Sub

Sub RHelp_Hit():Playsound "fx_balldrop", 0, 1, 0.05, 0.05:end Sub

Sub WireRampSound_Hit():Playsound "WireRamp", 0, 1, 0, 0.35:end Sub
'***********
' Solenoids
'***********
' from pacdudes script
SolCallback(1) = "bsTrough.SolIn"
SolCallback(1) = "bsTrough.SolOut"
SolCallback(2) = "vpmsolsound SoundFX(""fx_knocker"",DOFKnocker),"
SolCallback(3) = "SolDogOut"
SolCallback(4) = "solMT"  'dtMilk.SolDropUp
SolCallback(5) = "bsTrash.SolOut"
SolCallback(6) = "SolBT"    'dtBird.SolDropUp
SolCallback(10)= "SolGIBlink"
SolCallBack(23)= "SolGION"  'check to see if 10 works

SolCallback(15) = "SolSFW1"
SolCallback(16) = "SolSFW"
'Flashers
SolCallback(25) = "flash125" 
SolCallback(26) = "flash126"
SolCallback(27) = "flash127"
SolCallback(28) = "flash128"
SolCallback(29) = "flash129"
SolCallback(30) = "flash130"
SolCallback(31) = "flash131"
SolCallback(32) = "flash132"
'Solenoid Subs

Sub SolDogOut(enabled)
	If Enabled Then
		bsDog.ExitSol_On
		SetLamp 190, 0
	End If
End Sub

Sub SolSFW(enabled)
  If enabled Then
	SetLamp 190, 1
  Else
	SetLamp 190, 0
  end If



end Sub

Sub SolSFW1(enabled)

  If enabled Then
	SetLamp 190, 1
  Else
	SetLamp 190, 0
  end If

end Sub

Sub solMT(enabled)
  If enabled Then
	dtMilk.DropSol_On
	For each xx in MTGi:xx.State = 0:next
	For each xx in MT:xx.Image = "Milk":next
  Else
  end If
end Sub

Sub solBT(enabled)
  If enabled Then
	dtBird.DropSol_On
	For each xx in BTGi:xx.State = 0:next
	For each xx in BT:xx.Image = "BirdT":next
  Else
  end If
end Sub

Sub Flash127(enabled)
  If enabled Then
	Setlamp 127, 1
  Else
	SetLamp 127, 0
  end If
end Sub

Sub Flash125(enabled)
  If enabled Then
	Setlamp 125, 1
  Else
	SetLamp 125, 0
  end If
end Sub

Sub Flash126(enabled)
  If enabled Then
	Setlamp 126, 1
  Else
	SetLamp 126, 0
  end If
end Sub

Sub Flash128(enabled)
  If enabled Then
	Setlamp 128, 1
  Else
	SetLamp 128, 0
  end If
end Sub

Sub Flash129(enabled)
  If enabled Then
	Setlamp 129, 1
  Else
	SetLamp 129, 0
  end If
end Sub

Sub Flash130(enabled)
  If enabled Then
	Setlamp 130, 1
  Else
	SetLamp 130, 0
  end If
end Sub

Sub Flash131(enabled)
  If enabled Then
	Setlamp 131, 1
  Else
	SetLamp 131, 0
  end If
end Sub

Sub Flash132(enabled)
  If enabled Then
	Setlamp 132, 1
  Else
	SetLamp 132, 0
  end If
end Sub

Sub ACRelay(enabled)
    vpmNudge.SolGameOn enabled
End Sub

'**************
' Flipper Subs
'**************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
	If FlipLag = 0 then 
        PlaySound SoundFX("fx_flipperup", DOFContactors), 0, 1, -0.1, 0.25
        LeftFlipper.RotateToEnd
	end If
    Else
	If FlipLag = 0 then 
        PlaySound SoundFX("fx_flipperdown", DOFContactors), 0, 1, -0.1, 0.25
        LeftFlipper.RotateToStart
	end if
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
	If FlipLag = 0 then 
        PlaySound SoundFX("fx_flipperup", DOFContactors), 0, 1, 0.1, 0.25
        RightFlipper.RotateToEnd
	end if
    Else
	If FlipLag = 0 then 
        PlaySound SoundFX("fx_flipperdown", DOFContactors), 0, 1, 0.1, 0.25
        RightFlipper.RotateToStart
	end if
    End If
End Sub

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, -0.1, 0.15
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, 0.1, 0.15
End Sub

'SeaFoodWheel Based on jp's script based on cyclone script
Dim SFWSpin
SFWSpin = 0
Sub UpdateWheel(aNewPos, aSpeed, aLastPos)
    ' 360/200= 1.8
    If aNewPos <> aLastPos then
        SFWheel.ObjRotZ = aNewPos * 1.8
    End If
End Sub

'*********
' Special Flippers
'*********
dim FlippersEnabled



sub flipnf(LR, DU)
    if LR = 0 Then        'left flipper
        if DU = 1 then
            If FlippersEnabled = True then
                leftflipper.rotatetoend
                LeftFlipperSound 1
            end if
            controller.Switch(swLLFlip) = True
        Elseif DU = 0 then
            If FlippersEnabled = True then
                leftflipper.rotatetoStart
                LeftFlipperSound 0
            end if
            controller.Switch(swLLFlip) = False                            
        end if
    elseif LR = 1 then        ''right flipper
        if DU = 1 then
            If FlippersEnabled = True then
                RightFlipper.rotatetoend
                RightFlipperSound 1
            end if
            controller.Switch(swLRFlip) = True
        Elseif DU = 0 then
            If FlippersEnabled = True then
                RightFlipper.rotatetoStart
                RightFlipperSound 0
            end if
            controller.Switch(swLRFlip) = False                        
        end if
    end if
end sub

sub LeftFlipperSound(updown)'called along with the flipper, so feel free to add stuff, EOStorque tweaks, animation updates, upper flippers, whatever.
    if updown = 1 Then
        PlaySound SoundFX("fx_flipperup", DOFContactors), 0, 1, -0.1, 0.25    'flip
    Else
        PlaySound SoundFX("fx_flipperdown", DOFContactors), 0, 1, -0.1, 0.25'return
    end if
end sub
sub RightFlipperSound(updown)
    if updown = 1 Then
        PlaySound SoundFX("fx_flipperup", DOFContactors), 0, 1, 0.1, 0.25    'flip
    Else
        PlaySound SoundFX("fx_flipperdown", DOFContactors), 0, 1, 0.1, 0.25'return
    end if
end sub
'************GI Subs

 Dim GIActive,GIState
 GIActive=0:GIState=0
 Sub SolGION(enabled)
    FlippersEnabled = Enabled
 	If enabled then
 		GIActive=1:SolGI 1
 	else
 		GIActive=0:SolGI 0
        if leftflipper.startangle > leftflipper.endangle Then
            if leftflipper.currentangle < leftflipper.startangle then leftflipper.rotatetostart : leftflippersound 0 : end if
        elseif leftflipper.startangle < leftflipper.endangle Then
            if leftflipper.currentangle > leftflipper.startangle then leftflipper.rotatetostart : leftflippersound 0 : end If
        end If
        if rightflipper.startangle > rightflipper.endangle Then
            if rightflipper.currentangle < rightflipper.startangle then rightflipper.rotatetostart : rightflippersound 0 : end if
        elseif rightflipper.startangle < rightflipper.endangle Then
            if rightflipper.currentangle > rightflipper.startangle then rightflipper.rotatetostart : rightflippersound 0 : end If
        end If
 	end if
 End Sub

 Sub SolGIBlink(enabled)
 	If GIActive=1 then:SolGI Not enabled:end if
 End Sub

 'GI Lights

  Sub SolGI(Enabled)
 	If enabled then
	Playsound "fx_relay_on"								'ninuzzu - added relay click sound
	Table1.ColorGradeImage = "ColorGrade_8"				'ninuzzu - added LUT color grade---->this will light the whole table when GI is on
	For each xx in aGiLights:xx.State = 1:next
	If Sw28.IsDropped = 1 then: sw28l.State = 1: End if
	If Sw27.IsDropped = 1 then: sw27l.State = 1: End if
	If Sw26.IsDropped = 1 then: sw26l.State = 1: End if
	If Sw25.IsDropped = 1 then: sw25l.State = 1: End if
	If Sw39.IsDropped = 1 then: sw39l.State = 1: End if
	If Sw38.IsDropped = 1 then: sw38l.State = 1: End if
	If Sw37.IsDropped = 1 then: sw37l.State = 1: End if
	GIState=1
	SetLamp 190, 0
 	else
	Playsound "fx_relay_off"							'ninuzzu - added relay click sound
		Table1.ColorGradeImage = "ColorGrade_1"			'ninuzzu - added LUT color grade---->this will darken the whole table when GI is off
	For each xx in aGiLights:xx.State = 0:next
	For each xx in TargetDropGi:xx.State = 0:next
	GIState=0
 	end if
 End Sub

'******************************************************
'        JP's VP10 Fading Lamps & Flashers
'  very reduced, mostly for rom activated flashers
' if you need to turn a light on or off then use:
'	LightState(lightnumber) = 0 or 1
'        Based on PD's Fading Light System
'******************************************************

Dim LightState(200), FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200)

InitFlashers() ' turn off the lights and flashers and reset them to the default parameters

LampTimer.Interval = 50 'lamp fading speed
LampTimer.Enabled = 1

Sub LampTimer_timer()
    Dim chgLamp, x
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For x = 0 To UBound(chgLamp)
            LightState(chgLamp(x, 0) ) = chgLamp(x, 1) 'light state as set by the rom
        Next
    End If
    ' Lights & Flashers
    LightX 1, l1
    LightX 2, l2
    LightX 3, l3
    LightX 4, l4
    LightX 5, l5
    LightX 6, l6
    LightX 7, l7
    LightX 8, l8
    Flash 9, l9
    Flash 10, l10
    Flash 11, l11
    Flash 12, l12
    Flash 13, l13
    LightX 14, l14
    LightX 15, l15
    LightX 16, l16
    Flash 17, l17
    Flash 18, l18
    Flash 19, l19

    LightX 21, l21
    LightX 22, l22
    LightX 23, l23
    LightX 24, l24
    LightX 25, l25
    LightX 26, l26
    LightX 27, l27
    LightX 28, l28
    LightX 29, l29
    LightX 30, l30
    LightX 31, l31
    LightX 33, l33
    LightX 34, l34
    LightX 35, l35
    LightX 36, l36
    LightX 37, l37
    LightX 38, l38
    LightX 39, l39
    LightX 40, l40
    LightX 41, l41
    LightX 42, l42
    LightX 43, l43
    LightX 44, l44
    LightX 45, l45
    LightX 46, l46
    LightX 47, l47
    LightX 48, l48
    LightX 49, l49
    LightX 50, l50
    LightX 51, l51
    LightX 52, l52
    LightX 53, l53
'    LightX 54, l54 'Lamp shed backglass
'    LightX 55, l55  'bbq bg
'    LightX 56, l56  'candle bg
'    LightX 57, l57  '57-64 bg jackpot 1000000 - 8000000
'    LightX 58, l58
'    LightX 59, l59
'    LightX 60, l60
'    Flashm 61, Diode3
'    Flash 62, Diode4
'    LightX 63, l63
'    LightXm 64, l69a
	LightXm 125, f25a
	LightX 125, f25
	LightXm 126, f26a
	LightXm 126, f26
	Flash 126, f26b
	LightXm 127, f27
	lightXm 127, f27a
	Flash 127, f27b
	LightXm 128, f28a
	LightXm 128, f28
	Flash 128, f28b
	LightXm 129, f29a
	LightXm 129, f29
	Flash 129, f29b
	LightXm 130, f30
	Flash 130, f30a
	LightXm 131, f31a
	LightXm 131, f31
	Flash 131, f31b
	LightXm 132, f32
	Flash 132, f32a

	Flash 190, SFWL
End Sub

Sub SetLamp(nr, value)
    If value <> LightState(nr) Then
        LightState(nr) = value
    End If
End Sub

' div lamp subs

Sub InitFlashers()
    Dim x
    For x = 0 to 200
        LightState(x) = 0     	 ' light state: 0=off, 1=on, -1=no change (on or off)
        FlashSpeedUp(x) = 0.5    ' Fade Speed Up
        FlashSpeedDown(x) = 0.25 ' Fade Speed Down
        FlashMax(x) = 1          ' the maximum intensity when on, usually 1
        FlashMin(x) = 0          ' the minimum intensity when off, usually 0
        FlashLevel(x) = 0        ' the intensity/fading of the flashers
    Next
End Sub

' VPX Lights, just turn them on or off

Sub LightX(nr, object)
    Select Case LightState(nr)
        Case 0, 1:object.state = LightState(nr):LightState(nr) = -1
    End Select
End Sub

Sub LightXm(nr, object) 'multiple lights
    Select Case LightState(nr)
        Case 0, 1:object.state = LightState(nr)
    End Select
End Sub

' VPX Flashers, changes the intensity

Sub Flash(nr, object)
    Select Case LightState(nr)
        Case 0 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
                LightState(nr) = -1 'completely off, so stop the fading loop
            End if
            Object.IntensityScale = FlashLevel(nr)
        Case 1 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                LightState(nr) = -1 'completely on, so stop the fading loop
            End if
            Object.IntensityScale = FlashLevel(nr)
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it just sets the intensity
    Object.IntensityScale = FlashLevel(nr)
End Sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 500)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
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

'Const tnob = 5 ' total number of balls in this table is 4, but always use a higher number here because of the timing

Const tnob = 1 'ninuzzu - why 5 balls? Bad Cats has only one ball
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

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

    ' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
        If BallVel(BOT(b) ) > 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b) )
            Else
                ballpitch = Pitch(BOT(b) ) * 100
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, ballpitch, 1, 0
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
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 500, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub

'******************
' RealTime Updates
'******************

Set MotorCallback = GetRef("RealTimeUpdates")

Sub RealTimeUpdates
    RollingUpdate
	BallShadowUpdate								'ninuzzu - added ballshadow routine
	FlipperL.RotZ=LeftFlipper.currentangle			'ninuzzu - move flipper primitive in sync with VP flipper object
	FlipperR.RotZ=RightFlipper.currentangle			'ninuzzu - move flipper primitive in sync with VP flipper object
	FlipperLSh.RotZ=LeftFlipper.currentangle		'ninuzzu - move flipper shadow primitive in sync with VP flipper object
	FlipperRSh.RotZ=RightFlipper.currentangle		'ninuzzu - move flipper shadow primitive in sync with VP flipper object

End Sub

'*****************************************
'	Ball Shadow
'*****************************************

Dim BallShadow
BallShadow = Array (BallShadow1)																		'ninuzzu - let's create an array of primitives, the number of primitives is equal to tnob
Dim ShadowSFW
ShadowSFW = 0	

Sub shadowTrig_Hit:	ShadowSFW = 1: End Sub																								'ninuzzu- so in this case only one primitive, for 3 ball it will be BallShadow = Array (BallShadow1,BallShadow2,BallShadow3)
Sub shadowTrig_UnHit: ShadowSFW = 0: End Sub

Sub BallShadowUpdate()
    Dim BOT, b
    BOT = GetBalls																						'ninuzzu- this will return an array , the balls array, this is updated in real time

	' render the shadow for each ball						
    For b = 0 to UBound(BOT)																			'ninuzzu - now let's link the ball array with the array of primitives; so for each ball in the array, do this
		If BOT(b).X < Table1.Width/2 Then
			BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 10		'ninuzzu - the shadow array will move left or right depending on the ball X position in the table
		Else
			BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 10
		End If
		BallShadow(b).Y = BOT(b).Y + 20																	'ninuzzu - the shadow Y is at ball Y + 20 units lower
		BallShadow(b).Z = 1																				'ninuzzu - the shadow Z is 1
	
		If (BOT(b).Z > 20 and ShadowSFW = 0)  Then																			'ninuzzu - if the ball is falling through a hole, e.g. a subway, the shadow is not visible.
			BallShadow(b).visible = 1
		Else
			BallShadow(b).visible = 0
		End If
	Next
End Sub

'******************************
' Diverse Collection Hit Sounds
'******************************

Sub aMetal_Wires_Hit(idx):PlaySound "fx_metalhit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Bands_Hit(idx):PlaySound "fx_rubber_band", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Posts_Hit(idx):PlaySound "fx_rubber", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Pins_Hit(idx):PlaySound "fx_postrubber", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aPlastics_Hit(idx):PlaySound "fx_PlasticHit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aGates_Hit(idx):PlaySound "fx_Gate", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aWoods_Hit(idx):PlaySound "fx_Woodhit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub

