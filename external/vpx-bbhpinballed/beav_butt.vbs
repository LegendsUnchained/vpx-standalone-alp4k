'Last Updated in VBS v3.50

Option Explicit
LoadCore
Private Sub LoadCore
	On Error Resume Next
	If VPBuildVersion < 0 Or Err Then
		Dim fso : Set fso = CreateObject("Scripting.FileSystemObject") : Err.Clear
		ExecuteGlobal fso.OpenTextFile("core.vbs", 1).ReadAll    : If Err Then MsgBox "Can't open ""core.vbs""" : Exit Sub
		ExecuteGlobal fso.OpenTextFile("VPMKeys.vbs", 1).ReadAll : If Err Then MsgBox "Can't open ""vpmkeys.vbs""" : Exit Sub
	Else
		ExecuteGlobal GetTextFile("core.vbs")    : If Err Then MsgBox "Can't open ""core.vbs"""    : Exit Sub
		ExecuteGlobal GetTextFile("VPMKeys.vbs") : If Err Then MsgBox "Can't open ""vpmkeys.vbs""" : Exit Sub
	End If
End Sub

'-------------------------
' GTS3 Data
'-------------------------
' FrontDoor switches
Const swCoin1       = 00
Const swCoin2       = 01
Const swCoin3       = 02
' Cabinet switches
Const swStartButton = 03
Const swLeftAdvance = 04
Const swRightAdvance = 05
Const swLRFlip = 141'06
Const swLLFlip = 143'07

Const swDiagnostic  = -8
Const swTilt        = -7
Const swSlamTilt    = -6

' Help window
vpmSystemHelp = "Class Of 1812 keys:" & vbNewLine &_
  vpmKeyName(StartGameKey)  & vbTab & "Start Gane" & vbNewLine &_ 
  vpmKeyName(keyInsertCoin1)  & vbTab & "Left Coin Chute" & vbNewLine &_
  vpmKeyName(keyInsertCoin2)  & vbTab & "Right Coin Chute" & vbNewLine &_
  vpmKeyName(keyInsertCoin3)  & vbTab & "Center Coin Chute" & vbNewLine &_
  vpmKeyName(keySelfTest)     & vbTab & "Diagnostic"     & vbNewLine &_
  vpmKeyName(keyDown)  & vbTab & "Left Advance Button" & vbNewLine &_
  vpmKeyName(keyUp)  & vbTab & "Right Advance Button" & vbNewLine &_
  vpmKeyName(keySlamDoorHit)  & vbTab & "Slam Tilt"

' Option Menu (No Dips)
Private Sub Class1812ShowDips
	If Not IsObject(vpmDips) Then ' First time
		Set vpmDips = New cvpmDips
		With vpmDips
	  	.AddForm  80, 0, "Option Menu"
		.AddLabel 0,0,250,20,"No Options In This Table At This Time"
		End With
	End If
	vpmDips.ViewDips
End Sub
Set vpmShowDips = GetRef("Class1812ShowDips")
Private vpmDips

' Keyboard handlers
Function vpmKeyDown(ByVal keycode)
	On Error Resume Next
	vpmKeyDown = True ' assume we handle the key
	With Controller
		Select Case keycode
			Case RightFlipperKey .Switch(swLRFlip) = True
      Case LeftFlipperKey  .Switch(swLLFlip) = True
			Case keyInsertCoin1  vpmTimer.AddTimer 750,"vpmTimer.PulseSw swCoin1'" : Playsound SCoin
			Case keyInsertCoin2  vpmTimer.AddTimer 750,"vpmTimer.PulseSw swCoin2'" : Playsound SCoin
			Case keyInsertCoin3  vpmTimer.AddTimer 750,"vpmTimer.PulseSw swCoin3'" : Playsound SCoin
			Case StartGameKey    .Switch(swStartButton) = True
			Case keySelfTest     .Switch(swDiagnostic)  = True
			Case keySlamDoorHit  .Switch(swSlamTilt)    = True
			Case keyDown         .Switch(swLeftAdvance) = True
			Case keyUp           .Switch(swRightAdvance) = True
			Case keyBangBack     vpmNudge.DoNudge   0,6
			Case LeftTiltKey     vpmNudge.DoNudge  75,2
			Case RightTiltKey    vpmNudge.DoNudge 285,2
			Case CenterTiltKey   vpmNudge.DoNudge   0,2
			Case keyVPMVolume    vpmVol
			Case Else            vpmKeyDown = False
		End Select
	End With
	On Error Goto 0
End Function

Function vpmKeyUp(ByVal keycode)
	On Error Resume Next
	vpmKeyUp = True ' assume we handle the key
	With Controller
		Select Case keycode
			Case RightFlipperKey .Switch(swLRFlip) = False
      Case LeftFlipperKey  .Switch(swLLFlip) = False
			Case keyDown         .Switch(swLeftAdvance) = False
			Case keyUp           .Switch(swRightAdvance) = False
			Case StartGameKey    .Switch(swStartButton) = False
			Case keySelfTest     .Switch(swDiagnostic)  = False
			Case keySlamDoorHit  .Switch(swSlamTilt)    = False
			Case keyShowOpts     .Pause = True : .ShowOptsDialog GetPlayerHWnd : .Pause = False
			Case keyShowKeys     .Pause = True : vpmShowHelp : .Pause = False
			Case keyShowDips     If IsObject(vpmShowDips) Then .Pause = True : vpmShowDips : .Pause = False
			Case keyAddBall      .Pause = True : vpmAddBall  : .Pause = False
			Case keyReset        .Stop : BeginModal : .Run : vpmTimer.Reset : EndModal
			Case keyFrame        .LockDisplay = Not .LockDisplay
			Case keyDoubleSize   .DoubleSize  = Not .DoubleSize
			Case Else            vpmKeyUp = False
		End Select
	End With
	On Error Goto 0
End Function
