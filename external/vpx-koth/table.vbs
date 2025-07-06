' ****************************************************************
'                       VISUAL PINBALL X
'                		PINBALL_TABLE_NAME
'                       Version 1.0.0
'						started 
' ****************************************************************


'⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣀⢀⣠⣴⣾⣴⣾⣿⣿⣶⣿⣿⣶⣾⣿⣷⣦⣿⣷⣦⣤⣶⣤⣄⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
'⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣾⡿⣿⡿⠛⠋⠙⠋⠁⠀⠈⠉⠀⠀⠉⠉⠀⠈⠉⠋⠉⠉⠛⠋⠙⠻⢿⣶⣦⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀
'⠀⠀⠀⠀⠀⠀⠀⣰⣾⣾⡿⠋⠀⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠻⣿⣄⠀⠀⠀⠀⠀⠀⠀⠀
'⠀⠀⠀⠀⠀⠀⣰⣿⠏⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⠿⣿⣆⠀⠀⠀⠀⠀⠀
'⠀⠀⠀⠀⢠⣾⣿⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⣿⣆⠀⠀⠀⠀⠀
'⠀⠀⠀⢀⣾⡟⠁⠀⠀⠀⠀⠀⠀⠀⣄⡀⠀⣄⠀⠀⣀⡀⣀⡀⠀⢀⡀⠀⣤⠄⣠⡀⢀⣄⠀⠀⣀⠀⠀⠀⠀⠀⠀⠙⣿⡆⠀⠀⠀⠀
'⠀⠀⠀⣼⣿⠁⠀⠀⠀⠀⣹⡶⠶⠶⠟⠋⠉⠉⠙⠛⠋⠳⠾⠳⠶⠋⠷⠞⠉⠋⠉⠛⠉⠙⠚⠛⠷⣄⠀⠀⠀⠀⠀⠀⠸⣿⡄⠀⠀⠀
'⠀⠀⢰⣿⠃⠀⠀⠀⣶⠟⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠲⡆⠀⠀⠀⠀⢿⣷⠀⠀⠀
'⠀⠀⢸⣿⠀⠀⠀⣰⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠹⣆⠀⠀⠀⢸⣿⠀⠀⠀
'⠀⠀⢸⣿⠀⠀⢰⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⡤⠶⠶⠶⢤⣄⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⢧⠀⠀⢸⣿⠀⠀⠀
'⠀⠀⢸⣿⠀⢠⡏⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⠋⠁⠀⠀⠀⠀⠀⠀⠈⠙⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⣧⠀⢸⣿⠀⠀⠀
'⠀⠀⢸⣿⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣠⡤⠤⠤⣄⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⠀⢸⣿⠀⠀⠀
'⠀⠀⢸⣿⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⠤⠴⠚⠋⠁⠀⠀⠀⠀⠉⠙⠓⠒⠦⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⠀⢸⣿⠀⠀⠀
'⠀⠀⢸⣿⠀⢸⡇⠀⢀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⠀⠀⠀⠀⠀⠀⠀⢀⣀⣀⣀⣀⣀⣀⣀⣠⣄⣤⣤⣤⣀⠀⢸⠀⢸⣿⠀⠀⠀
'⠀⠀⢸⣿⠀⢸⡇⢰⣿⠿⠿⠛⠛⠛⠛⠛⠿⣟⠛⠛⠿⣿⡄⠀⣀⡀⠀⢰⣿⠛⠛⠛⢻⣛⠙⠉⠉⠋⠉⠛⠛⣿⣧⣸⠀⢸⣿⡆⠀⠀
'⠀⠀⣸⡿⠀⢸⣷⣿⡿⠀⠀⠀⠀⠤⠤⠖⠒⣠⠗⠀⠀⢿⣿⣿⣿⣿⣷⣿⣏⠀⠀⠲⣤⣉⡛⠒⠶⠤⠄⠀⠀⢿⡿⢿⣧⡘⣿⣧⡀⠀
'⢀⣼⣿⣇⣠⣾⡟⢹⡇⠀⠀⢠⠖⠒⣒⣛⠉⠓⣄⠀⠀⢸⡏⠁⠀⠀⠈⢻⣏⠀⠀⣤⠞⠉⢉⣉⠉⠓⢦⠀⠀⣸⡇⢸⡟⡿⣏⠻⣿⡶
'⢸⡿⢡⣾⡿⣽⡇⢸⡇⠀⠀⣏⠀⠘⢿⡯⠀⠀⣸⠇⠀⢸⡇⠀⠀⠀⠀⢸⣿⠀⠀⢿⡀⠀⢻⢧⠄⠀⢘⡧⠀⣿⡇⠘⠛⢠⡏⠁⢻⣿
'⢸⡇⠀⢹⣧⠈⠀⢸⣿⠀⠀⠈⠒⠶⠤⠤⠖⠋⠁⠀⢀⣾⡇⠀⠀⠀⠀⢸⣿⡀⠀⠀⠉⠓⠒⠶⠒⠒⠋⠀⠀⣿⡇⠀⠀⡟⠀⠀⢸⣷
'⢸⣿⡀⠀⣿⠆⡀⠀⠻⣷⣄⣀⠀⠀⠀⠀⠀⠀⣀⣠⣾⡿⠀⠀⠀⠀⠀⠀⢻⣷⣄⣀⣀⣀⣀⣀⣀⣀⣀⣠⣾⠟⠁⢰⡀⢻⠀⢀⣾⡇
'⠀⣿⣧⠀⠹⢼⡇⠀⠀⠈⠉⠛⠻⠿⠿⠿⠿⠟⠛⠋⠉⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠛⠛⠛⠛⠛⠛⠛⠉⠉⠁⠀⠀⠸⡇⠋⠀⣸⣿⠀
'⠀⠸⣿⡆⠀⢸⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⡖⠀⠀⠀⠀⠀⠀⠘⢦⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢷⠀⢀⣿⡇⠀
'⠀⠀⠹⣿⣦⣼⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⡾⠋⠀⠀⠀⠀⠀⠀⠀⠀⠈⠻⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣼⣶⣿⠏⠀⠀
'⠀⠀⠀⠀⢹⣿⡆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠠⣿⠠⠶⠿⢦⣄⠀⠀⢀⡴⠟⠛⠃⣨⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⡏⠀⠀⠀⠀
'⠀⠀⠀⠀⠸⣿⡇⠀⠠⣄⡀⠀⠀⢀⣠⡤⠚⠀⠈⠓⠀⠀⠀⠉⠛⠛⠋⠀⠀⠀⠀⠋⠀⠉⠳⢤⣀⣀⠀⣀⣠⠴⠂⠀⣿⡇⠀⠀⠀⠀
'⠀⠀⠀⠀⠀⣿⣧⠀⠀⠀⠉⠛⠛⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠉⠁⠀⠀⠀⢸⣿⠃⠀⠀⠀⠀
'⠀⠀⠀⠀⠀⢻⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⠀⠀⠀⠀⠀
'⠀⠀⠀⠀⠀⢸⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠠⣄⣀⣀⣀⡤⠴⠶⠤⢤⠤⠴⠶⠦⠤⢤⣄⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⠀⠀⠀⠀⠀
'⠀⠀⠀⠀⠀⠸⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠉⢀⣀⠀⠀⠀⠀⠀⠀⠀⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⠀⠀⠀⠀⠀
'⠀⠀⠀⠀⠀⠀⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠛⠓⠒⠒⠒⠒⠋⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣼⣿⠀⠀⠀⠀⠀
'⠀⠀⠀⠀⠀⠀⣿⣧⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⠀⠀⠀⠀⠀
'⠀⠀⠀⠀⠀⠀⠸⣿⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⡟⠀⠀⠀⠀⠀
'⠀⠀⠀⠀⠀⠀⠀⣿⣧⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣼⣿⠃⠀⠀⠀⠀⠀
'⠀⠀⠀⠀⠀⠀⠀⠘⣿⡗⠄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠰⠋⣿⡟⠀⠀⠀⠀⠀⠀
'⠀⠀⠀⠀⠀⠀⠀⠀⣿⣇⠀⠀⠀⠀⠀⠀⠀⠳⣤⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣠⠴⠃⠀⠀⠀⠀⠀⠀⢠⣿⠁⠀⠀⠀⠀⠀⠀
'⠀⠀⠀⠀⠀⠀⠀⠀⠘⣿⡀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠙⠛⠓⠒⠒⠒⠖⠚⠛⠋⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⣾⣿⠀⠀⠀⠀⠀⠀⠀
'⠀⠀⠀⠀⠀⠀⠀⠀⠀⢿⣧⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⡇⠀⠀⠀⠀⠀⠀⠀
'⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⡇⠀⠀⠀⠀⠀⠀⠀
'⠀⠀⠀⠀⠀⠀⠀⠀⠀⣼⡿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣿⣿⠀⠀⠀⠀⠀⠀⠀
'⠀⠀⠀⠀⠀⠀⠀⠀⠀⠻⢿⣶⣤⣄⣀⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⣀⣤⣤⣶⡿⠿⠛⠉⠀⠀⠀⠀⠀⠀⠀
'⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⢛⠛⠻⠿⠿⢶⣶⣶⣶⣶⣶⣶⣶⣶⣶⣶⣶⠶⠶⠿⠿⠛⠛⢋⠉⡁⢀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀


'*****************************************************************************************************************************************
' DOF IDs
'*****************************************************************************************************************************************

'E101 0/1 LeftFlipper
'E102 0/1 RightFlipper
'E103 2 Leftslingshot
'E104 2 Rightslingshot
'E105 2 Bumper001

'E111 2 NewBall

'E114 2 OpenCooler
'E115 2 PropaneSpinner
'E116 2 Spinner001
'E117 2 Spinner002

'E118 2 Peggy Kicker

'E119 2 FootballSpinner

'E120 0/1 Start Button
'E121 2 AutoFire
'E122 2 Gates
'E123 2 Gates Wire

'E125 0/1 Add Credit
'E127 0/1 Gi


'E128 2 TLeftOutlane
'E129 2 TLeftInlane
'E130 2 TLeftInlane2
'E131 2 TRightOutlane
'E132 2 TRightInlane
'E133 2 TRightInlane2
 

'E134 2 Extraball

'E136 2 Start Modes
'E137 2 Propane - Multipliers

'E138 2 Targets

'E140 2 Ramps
'E141 2 Win Modes
'E142 2 Plunger Release

'E243 2 End of Ball
'E265 0/1 ???

'E290 0/1 GameOver



'*****************************************************************************************************************************************
'14D
' MerlinRTP Notes
' Music removed duck updates it was causing weird hiccups
' Added some basic data structures for modes
' Intend to use light seq for each mode string of lights -- think T-E-R-R-I-F-I-E-R lights 
' KING HILL lights cannot be included in any light seq except Attract
' light King Extra ball lit 
' light Hill Extra ball lit
' if one of the other modes was already compelted then instead of extraball lit, wizard mode is kicked off automatically

'15 Added pupDMD framework, added missing triggers, added light sequence for KOTH, added updatelights routine, added all arrays for lighting of modes, added 4 new modes
' added logic to handle all progress updates including lighting, began to add mode code for hank,beer,peggy modes, added code for multiplier pupDMD, lights on PF.
' tractor moves along with Lawn Mower hits

' 16 Added light sequences for all modes, shots, began adding code for start, stop, win modes

' 17 added code for the modes, including light sequences for shot placements
' added queue mgnt code ?
' add pupDMD tags for splash messages

' 18 Added code to handle ball locking for tractor which can be stolen. if at least one ball locked, opponent can steal your locked Balls
' added code for lock popup and value

'18T - Added modetimer.enabled = 0 to win/stop for modes with timers

'19D - added all mode messaging with updatemodemessages timer and data structures
'19G - All modes tested for start, win/stop , added temp jackpot/superjackpot audio callouts, add DMDSCreen variable so it works for JoeP
'19H - Commented out Flex DMD code , addded highscore code for PUPDMD

'	Select Case Mode(CurrentPlayer,0)
'		Case 1
'		Case 2
'		Case 3
'		Case 4
'		Case 5
'		Case 6
'		Case 7
'		Case 8
'		Case 9
'		Case 10
'		Case 11
'		Case 12
'		Case 13
'		Case 14
'		Case 15
'		Case 16
'		Case 17
'		Case 18
'		Case 19
'	End Select


Option Explicit
Randomize




'***************** This is where you turn on SCORBIT *******
Const ScorbitActive	= 0 	' Is Scorbit Active, Change to 1	
'***********************************************************







'****** PuP Variables ******

Dim usePUP: Dim cPuPPack: Dim PuPlayer: Dim PUPStatus: PUPStatus=false ' dont edit this line!!!

Dim pupPackScreenFile
Dim pupPackSizeFile
Dim ObjFso
Dim ObjFile
Dim PupType
Dim bAlreadyPlayed

Dim DMDType, bIsMiniGameActive

'*************************** PuP Settings for this table ********************************

usePUP   = true               ' enable Pinup Player functions for this table
cPuPPack = "kingofthehill"    ' name of the PuP-Pack / PuPVideos folder for this table
Const bEnablePuP = True
'//////////////////// PINUP PLAYER: STARTUP & CONTROL SECTION //////////////////////////

' This is used for the startup and control of Pinup 
'************ PuP-Pack Startup **************


Sub PuPEvent(EventNum)
    if (usePUP=false or PUPStatus=false) then Exit Sub
    PuPlayer.B2SData "E"&EventNum,1  'send event to Pup-Pack
End Sub

'Dim VR_MessageSize: VR_MessageSize = 6	' controls the size of the mode messages and progress in VR DMD
'Dim VR_BonusSize: VR_BonusSize = 4.5  ' controls the size of the multiplier values in VR DMD
'Dim VR_BallValue: VR_BallValue = 3	' controls the size of the ball value message in VR DMD
'Dim VR_Score:VR_Score = 9			' controls the size of the main score in VR DMD



'**********************************************************************************************************************************

Const BallSize = 50   ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1
Const SongVolume = 0.1 ' 1 is full volume. Value is from 0 to 1



' Load the core.vbs for supporting Subs and functions

On Error Resume Next
ExecuteGlobal GetTextFile("core.vbs")
If Err Then MsgBox "Can't open core.vbs"
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0


' Define any Constants
Const TableName = "kingofthehill"
Const cGameName = "kingofthehill"
Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height
Dim lightCtrl : Set lightCtrl = new LStateController

Const myVersion = "1.0.0"
Const MaxPlayers = 4     ' from 1 to 4
Const BallSaverTime = 20 ' in seconds
Const MaxMultiplier = 5  ' limit to 3x in this game, both bonus multiplier and playfield multiplier
Dim BallsPerGame: BallsPerGame = 3   ' usually 3 or 5
Const MaxMultiballs = 6  ' max number of balls during multiballs

Const Special1 = 1000000  ' High score to obtain an extra ball/game
Const Special2 = 3000000
Const Special3 = 5000000

'----- General Sound Options -----
Dim VolumeDial			  'Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume		  'Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume		  'Level of ramp rolling volume. Value between 0 and 1

'----- Shadow Options -----
Const DynamicBallShadowsOn = 0	  '0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 0	   '0 = Static shadow under ball ("flasher" image, like JP's), 1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that behaves like a true shadow!, 2 = flasher image shadow, but it moves like ninuzzu's

' Do not change anything below this line unless you know what the fuck you are doing

Const     ScorbitShowClaimQR	= 1 	' If Scorbit is active this will show a QR Code  on ball 1 that allows player to claim the active player from the app
Const     ScorbitUploadLog		= 0 	' Store local log and upload after the game is over 
Const     ScorbitAlternateUUID  = 0 	' Force Alternate UUID from Windows Machine and saves it in VPX Users directory (C:\Visual Pinball\User\ScorbitUUID.dat)


'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
''''' SCORING '''''''''''
Const SCORE_SUPER_JACKPOT = 500000
Const SCORE_JACKPOT = 200000
Const SCORE_BUMPER = 1000
Const SCORE_SLINGSHOT = 1500
Const SCORE_TARGET = 2500
Const SCORE_SPINNER = 1000
Const SCORE_KICKER = 1000
Const SCORE_LANES = 1000
Const SCORE_LANES_COMPLETED = 60000
Const SCORE_RAMP = 1000
Const SCORE_MODE_START = 10000
Const SCORE_MODE_PROGRESS = 5000
Const SCORE_MODE_COMPLETED = 100000
Const SCORE_SMALL_TARGET = 3000
Const SCORE_BBQFEAST = 1000000
Const SCORE_SKILLSHOT = 200000
Const SCORE_ORBIT = 1500
'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


Dim BIPL							'Ball in plunger lane
BIPL = False

Dim VRRoomChoice : VRRoomChoice = 0				'0 - VR Room Off, 1 - Cab Only, 2 - Minimal, 3 - Mega
Dim VRTest : VRTest = False


'----- Music Options -----
Dim fMusicVolume  ' 			'Music volume. 0 = no music, 1 = full volume
Const fAttractVolume = 0.3			'Attract mode music volume. 0 = no music, 1 = full volume
Const fIntroVolume = 0.008			' Intro Music Volume
Dim fDuckfactor	: fDuckfactor = .2			'Duck music to 40% (multiplicative with fMusicVolume) during videos
Const nVideoVolume = 90
Dim fCalloutVolume ' 0.5 Default	' Volume for Voice Callouts


Const bNoMusic = False
Const ChaseTime = 6000
Const LargeSeqInterval = 5
Const LargeSeqUpdate = 120
Dim nBallsPerGame 		' used for tweak menu
Dim stagedFlipper
Dim TrustOpt 
Dim bSupressModeMessages
Dim nCard
Dim bRuleCards
Dim bOnTheFirstBallScorbit
Dim GameModeStrTmp
Dim AttractTimerCount
Dim nBallSaveCounter
Dim bMowerNotStarted
Dim MasterMowerLocation	' used to keep track of the most advanced mower
Dim bReadyDrinkAlamo
Dim bGameReady

' EOB Tracked Items
Dim RampCount
Dim OrbitCount
Dim TargetCount
Dim BumperCount

' Define Global Variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim BumperHits
Dim bumperHitCount(4)
Dim BonusPoints(4)
Dim bBonusHeld
Dim CompletedModes(4)
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Special1Awarded(4)
Dim Special2Awarded(4)
Dim Special3Awarded(4)
Dim Score(4)
Dim MultiplierRank(4)
Dim nBonusX(4)
Dim nPlayfieldX(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim Mode(4,19)  ' 4 players 10 modes

Dim asModeMessagesL1
asModeMessagesL1 = Array("", "Shoot", "Hit Moving Target", "Luanne , Bobby", "Hit Peggy For", "Hit Boom For", "Hit Bill For", "Peggy Scoop", "Alternate", "Shoot" , "Shoot Behind", "Chase", "Avoid Bobby", "Shoot", "Shoot", "Shoot", "MULTIBALL", "Ladybird", "Shoot")

Dim asModeMessagesL2
asModeMessagesL2 = Array("","Orbits & Ramps", "Times", " Hank, Ladybird", "Super Jackpot", "Super Jackpot", "Super Jackpot", "Times", "Orbits and Ramps", "Targets", "Left Flipper", "Lights", " Hits Left", "Spinners", "Ramps", "Orbits", "", "10X Value", "Ramps" )

' Used for number of shots for each mode to compelte
Dim anModeProgress
anModeProgress = Array("","10","3","4","0","0","0","5","6","10","0","6","3","100","6","6","0","0","15","0")

' 0 - Base No Mode -- used to work towards all the other modes
' 1 - PEG First mission
' 2 - PEG Second mission
' 3 - PEG Third Mission (K)
' 4 - Luanne 			(I)
' 5 - Boomhauer 		(N)
' 6 - Bill 				(G)
' 7 - Hank First Mission	
' 8 - Hank Second Mission	
' 9 - Hank Third Mission(H)
' 10 - Dale				(I)
' 11 - Cotton			(L)
' 12 - Bobby			(L)
' 13 - Beer First 
' 14 - Beer Second
' 15 = Beer Third
' 16 - Lawn Mower
' 17 - LadyBird Mode
' 18 - BBQ Mode
' 19 - Wizard

Dim anBeer(4)	' array used for beer Mode
Dim BobbyTargetCount
Dim bPeggyReady
Dim bDaleReady
Dim bBoomReady
Dim bBillReady
Dim bBobbyReady
Dim bLuanneReady
Dim nYepReady
Dim bEBReady
Dim nTimerCount
Dim nNewCottonShot
Dim nCottonShots(10)
Dim nPeggyMode1Progress(11)
Dim nPeggyMode2Progress
Dim nPeggyMode3Progress(4)
Dim nHankMode1Progress
Dim bHankMode2Ramp
Dim bHankMode2Orbit
Dim nHankMode2Progress
Dim nHankMode3Progress
Dim nBBQModeProgress
Dim nCottonModeProgress
Dim nCottonActive
Dim nBeerMode1Progress
Dim nBeerMode2Progress
Dim nBeerMode3Progress
Dim nMowerProgress(10)
Dim PeggyLightsCount ' Total count of all lights
Dim anPeggyLights(4,3)
Dim BillLightsCount ' Total count of all lights
Dim anBillLights(4,9)
Dim BobbyLightsCount ' Total count of all lights
Dim anBobbyLights(4,15)
Dim BoomLightsCount ' Total count of all lights
Dim anBoomLights(4,9)
Dim CottonLightsCount ' Total count of all lights
Dim anCottonLights(4,6)
Dim DaleLightsCount ' Total count of all lights
Dim anDaleLights1(4)
Dim anDaleLights2(4)
Dim anDaleLights3(4)
Dim HankLightsCount ' Total count of all lights
Dim anHankLights(4,5)
Dim LuanneLightsCount ' Total count of all lights
Dim anLuanneLights(4,6)
Dim anBeerLights(4,5)
Dim bHillBall(4)
Dim bKingBall(4)

' non mode lights
Dim anDrinkAlamo(4,10)
Dim anLadybird(4,8)
Dim anLawnMower(4,5)
Dim anPropane(4,7)
Dim anYEP(4,3)
Dim anBBQFeast(4,5)

Dim bWizardMode
Dim bLockEngaged
Dim nLockedBalls
Dim bReleasingBalls

' Lane Lights
Dim bConnie	' Used to limit to 1 add a ball per played ball

Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim bAttractMode
Dim mBalls2Eject
Dim bAutoPlunger

' Define Game Control Variables
Dim BallsOnPlayfield
Dim BallsInLock
Dim BallsInHole

' Define Game Flags
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim bBallSaverReady
Dim bMultiBallMode
'Dim Multiball
Dim bMusicOn
Dim bJustStarted
Dim bJackpot
Dim plungerIM
Dim LastSwitchHit
Dim PTime
Dim bSqueal
Dim bSkillShotReady

Dim BallHandlingQueue : Set BallHandlingQueue = New vpwQueueManager
Dim EOBQueue : Set EOBQueue = New vpwQueueManager
Dim DMDQueue : Set DMDQueue = New vpwQueueManager
Dim HSQueue : Set HSQueue = New vpwQueueManager
Dim AudioQueue : Set AudioQueue = New vpwQueueManager
Dim GeneralPupQueue: Set GeneralPupQueue = New vpwQueueManager

Dim nTextPriority ' a low number is a high priority, 1 means top priority
				  ' 99 means "only display if otherwise idle"
Dim nTextDuration ' Time in ms of long to display the current message
Dim oEventQueue			' Global event queue
Dim oCurrentEvent		' The event currently being handled
Dim nTimeLastPuP		' GameTime when the last PuP event was handled
Dim IndexTmp

'Dim nDMDTextDisplayTime ' Time in ms the current text has been displayed
Dim nTimePupStartPlay	' Timestamp of when the current video started playing. 0 = no video
'Dim DMDTextEffect


Dim DrainTracker(41)
Dim BallLockedTracker(18)
Dim GameOverTracker(11)
Dim BallSavedTracker(18)

' core.vbs variables

'==================================================================================================================================
' Called when options are tweaked by the player. 
' - 0: game has started, good time to load options and adjust accordingly
' - 1: an option has changed
' - 2: options have been reseted
' - 3: player closed the tweak UI, good time to update staticly prerendered parts

' Table1.Option arguments are: 
' - option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional arry of literal strings

Dim dspTriggered : dspTriggered = False
Sub Table1_OptionEvent(ByVal eventId)
	If eventId = 1 And Not dspTriggered Then dspTriggered = True : DisableStaticPreRendering = True : End If

    ' VRRoom
	If RenderingMode = 2 or VRTest = True Then
		VRRoomChoice = Table1.Option("VR Room", 0, 3, 1, 3, 0, Array("OFF","Cab", "Minimal", "Mega"))
		LoadVRRoom
	End If
    ' Sound volumes
    VolumeDial = Table1.Option("Mech Volume", 0, 1, 0.01, 0.8, 1)
    BallRollVolume = Table1.Option("Ball Roll Volume", 0, 1, 0.01, 0.8, 1)
	RampRollVolume = Table1.Option("Ramp Roll Volume", 0, 1, 0.01, 0.5, 1)
	fCalloutVolume = Table1.Option("Callout Volume - Default 50%", 0, 1, 0.01, 0.5, 1)
	fMusicVolume = Table1.Option("Music Volume - Default 35%", 0, 1, 0.001, 0.35, 1)
	SetMusicVolumes

	'Balls Per Game
	nBallsPerGame = Table1.Option("Balls Per Game", 0, 2, 1, 0, 0, Array("3 (Default)", "4", "5"))
	SetBallsPerGame nBallsPerGame

	' Display rulecards, change with magna save keys
	bRulecards = Table1.Option("Rules on Backglass", 0, 1, 1, 1, 0, Array("False", "True (Default)"))

	'Staged Flipper Style
	stagedFlipper = Table1.Option("Staged Flipper", 0, 1, 1, 0, 0, Array("0 (Off - Default)", "1 (Magna Saves)"))

	'Freeplay
	bFreeplay = Table1.Option("Freeplay On", 0, 1, 1, 1, 0, Array("False", "True (Default)"))

	TrustOpt = Table1.Option("Trust Post", 0, 1, 1, 1, 0, Array("False", "True (Default)"))
	SetTrustPost TrustOpt
	

	If eventId = 3 And dspTriggered Then dspTriggered = False : DisableStaticPreRendering = False : End If

End Sub
'================================================================================================================================================================
Sub SetBallsPerGame(Opt)
	Select Case Opt
		Case 0: nBallsPerGame = 3
		Case 1:	nBallsPerGame = 4
		Case 2:	nBallsPerGame = 5
	End Select
End Sub

Sub SetStagedFlippers(Opt)
	Select Case Opt
		Case 0: stagedFlipper = 0
		Case 1:	stagedFlipper = 1
	End Select
End Sub

Sub SetTrustPost(Opt)
	Select Case Opt
		Case 0:
			zCol_Rubber_Peg005.collidable = 0
			pin004.Visible = 0
			Primitive008.visible= 0
		Case 1:
			zCol_Rubber_Peg005.collidable = 1
			pin004.Visible = 1
			Primitive008.visible=1
	End Select
End Sub


' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
	Dim i


	'Randomize
	bAlreadyPlayed = True
	bGameReady = True

	Dim x,y
	For Each x In aLights : Y = Y + 1 : x.timerinterval = y : Next

	lightCtrl.RegisterLights "VPX"

	lighttimer.enabled = 1

	PTime = 0 ' used for plunger timer sound effects
	bSqueal = True

	'SwitchMusic "M_End"		' moved to start attract
	CottonModeChase.Interval = ChaseTime


	Debug.print "Starting Game"


'Impulse Plunger as autoplunger
    Const IMPowerSetting = 36 ' Plunger Power
    Const IMTime = 1.1        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFXDOF("fx_kicker", 142, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 142, DOFPulse, DOFContactors)
        .CreateEvents "plungerIM"
    End With



    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init


    ' load saved values, highscore, names, jackpot
    Loadhs

    'Init main variables
    For i = 1 To MaxPlayers
        Score(i) = 0
        BonusPoints(i) = 0
        nBonusX(i) = 1
		MultiplierRank(i) = 0
        nPlayfieldX(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
    Next


	' Reset Tracker Arrays
	ResetBallLockedTracker
	ResetBallSavedTracker
	ResetGameOverTracker
	ResetDrainTracker

	' Turn off the bumper lights
	FlBumperFadeTarget(1) = 0

    ' freeplay or coins
    'bFreePlay = True 'we do not want coins

    'if bFreePlay = false Then DOF 125, DOFOn

    ' Init main variables and any other flags
	
    bAttractMode = False
    bOnTheFirstBall = False
    bBallInPlungerLane = False
    bBallSaverActive = False
    bBallSaverReady = False
    bGameInPlay = False
    bMusicOn = True
    BallsOnPlayfield = 0
	bMultiBallMode = False
	'Multiball=false
	bAutoPlunger = False
    BallsInLock = 0
    BallsInHole = 0
	LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bJustStarted = True
    ' set any lights for the attract mode
    GiOff
	MVTarget.collidable = False
	PrepareBobbyMovement

	pupinit

	wallsdown
	
	DOF 120,DOFOn  'start button on

	if Not ScorbitActive Then
		StartAttractMode
	End If


End Sub

Sub loadBG
	if DMDType = 0 Then
		PuPlayer.playlistplayex pBackglass,"PuPOverlays","defaultDMD.png",0,1
		'if renderingmode = 2 then VR_CabBackglass.image = "Card0"
	Else
		PuPlayer.playlistplayex pBackglass,"PuPOverlays","card1.png",0,1
		if renderingmode = 2 then VR_CabBackglass.image = "card1"
		PuPlayer.playlistplayex pDMD,"PuPOverlays","defaultDMD.png",0,1
	End If

	if DMDType > 0 And bAlreadyPlayed Then
		PuPlayer.playlistplayex pBackglass,"PuPOverlays","Card0.png",0,1
		if renderingmode = 2 then VR_CabBackglass.image = "Card0"
	End If


	PuPlayer.playevent pDMDVideo,"Background","DefaultBG.mp4",0,20,6,0,""

End Sub

Sub ResetOverlay
	PuPlayer.playlistplayex pDMD,"PuPOverlays","defaultDMD.png",0,1
End Sub

Sub ResetBackglass
	if DMDType = 0 Then
		PuPlayer.playlistplayex pBackglass,"PuPOverlays","defaultDMD.png",0,1
	Else
		PuPlayer.playlistplayex pBackglass,"PuPOverlays","card1.png",0,1
		if renderingmode = 2 then VR_CabBackglass.image = "card1"
	End If
End Sub
'****************************************
' Real Time updatess using the GameTimer
'****************************************
'used for all the real time updates

Sub GameTimer_Timer
    RollingUpdate
	Cor.Update
    ' add any other real time update subs, like gates or diverters
    FlipperLSh.Rotz = LeftFlipper.CurrentAngle
    FlipperRSh.Rotz = RightFlipper.CurrentAngle
    FlipperLSh2.Rotz = LeftFlipper2.CurrentAngle
    FlipperRSh2.Rotz = RightFlipper2.CurrentAngle
	LF1Logo.RotZ = LeftFlipper.CurrentAngle
	RF1logo.RotZ = RightFlipper.CurrentAngle
	LF2Logo.RotZ = LeftFlipper2.CurrentAngle
	RF2Logo.RotZ = RightFlipper2.CurrentAngle
	TruckW.TransZ = Plunger.Position *15
	Truck.TransZ = Plunger.Position *15
    Tank.Roty = PropaneSpinner.CurrentAngle+110
    Football.Roty = FootballSpinner.CurrentAngle+110
End Sub

Sub lighttimer_Timer
	lightCtrl.Update()
End Sub

sub TestRTP
'TractorKicker2.Kick 160, 1
'Bumper001_hit
exit sub
anLawnMower(currentplayer,0) = 4
anLawnMower(currentplayer,1) = 1
anLawnMower(currentplayer,2) = 1
anLawnMower(currentplayer,3) = 1
anLawnMower(currentplayer,4) = 1
'anLawnMower(currentplayer,5) = 1

exit sub
Mode(1,0) = 6
TurnOffModeLights
lighteffect 41

exit sub
	bBobbyReady = True
	StartBobby
end Sub

sub WallsDown
	Debugwall1.isdropped = 1
	Debugwall2.isdropped = 1
	Debugwall3.isdropped = 1
End Sub

sub WallsUp
	Debugwall1.isdropped = 0
	Debugwall2.isdropped = 0
	Debugwall3.isdropped = 0
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)

    If Keycode = AddCreditKey Then
		if bFreeplay Then
			Exit Sub
		Else
			Credits = Credits + 1
        'if bFreePlay = False Then
            DOF 125, DOFOn
            If(Tilted = False) Then
			ClearTwoLines
			pDMDSplashTwoLines "ADD CREDITS", "CREDITS " &credits, 3, cPurple
			DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
            End If
        End If
    End If

	If keycode = LeftFlipperKey and Renderingmode = 2 then VR_CabLeftFlipperButton.X = VR_CabLeftFlipperButton.X + 8
	If keycode = RightFlipperKey and Renderingmode = 2 then VR_CabRightFlipperButton.X = VR_CabRightFlipperButton.X - 8



	'   If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then 'Use this for ROM based games
	If keycode = AddCreditKey Or keycode = AddCreditKey2 Then
		if bFreeplay Then
			Exit Sub
		Else
			Select Case Int(Rnd * 3)
				Case 0
					PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
				Case 1
					PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
				Case 2
					PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
			End Select
		End If
	End If

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

	If keycode = PlungerKey Then
		Plunger.Pullback
		SoundPlungerPull

		if BallsOnPlayfield < 1 OR not BIPL  Then Exit Sub
        
		Playsound "Engine_during_burnout", -1, 1
		plungertimer.enabled = 1
		
	End If


	If (LeftMagnaSave = keycode And bRulecards) Then 
		if DMDType = 0 Then Exit Sub
		nCard = nCard - 1
		if nCard < 0 Then nCard = 2
		PuPlayer.playlistplayex pBackglass,"PuPOverlays","card"&nCard&".png",0,1
		if renderingmode = 2 Then VR_CabBackglass.image = "card"&nCard
	End If

	If (RightMagnaSave = keycode And bRulecards) Then 
		if DMDType = 0 Then Exit Sub
		nCard = nCard + 1		
		if nCard > 2 Then nCard = 0
		PuPlayer.playlistplayex pBackglass,"PuPOverlays","card"&nCard&".png",0,1
		if renderingmode = 2 Then VR_CabBackglass.image = "card"&nCard
	End If

    ' Normal flipper action


    If bGameInPlay AND NOT Tilted Then

        'If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
        'If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
        'If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt

		If keycode = LeftTiltKey Then
			Nudge 90, 1
			SoundNudgeLeft
			CheckTilt
		End If
		If keycode = RightTiltKey Then
			Nudge 270, 1
			SoundNudgeRight
			CheckTilt
		End If
		If keycode = CenterTiltKey Then
			Nudge 0, 1
			SoundNudgeCenter
			CheckTilt
		End If
		If keycode = MechanicalTilt Then
			SoundNudgeCenter() 'Send the Tilting command to the ROM (usually by pulsing a Switch), or run the tilting code for an orginal table
			CheckTilt
		End If


 '       If keycode = LeftFlipperKey Then SolLFlipper 1:Healer_Left
 '       If keycode = RightFlipperKey Then SolRFlipper 1:Healer_Right

		If 1 = stagedFlipper Then	
			If (LeftMagnaSave = keycode) Then
				FlipperActivate LeftFlipper2, LFPress
				SolLFlipper2 True
			End If

			If (RightMagnaSave = keycode) Then
				FlipperActivate RightFlipper2, RFPress
				SolRFlipper2 True
			End If
		End If



		If (keycode = RightFlipperKey) Then
			FlipperActivate RightFlipper, RFPress
			Healer_Right
			SolRFlipper True						'This would be called by the solenoid callbacks if using a ROM
			If stagedFlipper = 0 Then
				SolRFlipper2 True
			End If
		End If


		If (keycode = LeftFlipperKey) Then
			FlipperActivate LeftFlipper, LFPress
			Healer_Left
			SolLFlipper True						'This would be called by the solenoid callbacks if using a ROM
			If stagedFlipper = 0 Then
				SolLFlipper2 True
			End If
		End If


        If keycode = StartGameKey Then
            If((PlayersPlayingGame <MaxPlayers) AND(bOnTheFirstBall = True) ) Then

                If(bFreePlay) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
					DMDUpdatePlayerName
                Else
                    If(Credits> 0) then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
						DMDUpdatePlayerName
                        If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
                        Else 
						debug.print "WTF MAN"
							pDMDSplashTwoLines "ADD CREDITS", "CREDITS 0", 3, cRed
							DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
                    End If
                End If
            End If
        End If
        Else ' If (GameInPlay)

            If keycode = StartGameKey And bGameReady Then
                If(bFreePlay) Then
                    If(BallsOnPlayfield <= 0) Then
                        ResetForNewGame()
						Playsound "pressstartcarstart"
                    End If
                Else
                    If(Credits> 0) Then
                        If(BallsOnPlayfield <= 0) Then
                            Credits = Credits - 1
                            If Credits <1 And bFreePlay = 0 Then DOF 125, DOFOff
							Playsound "pressstartcarstart"
                            ResetForNewGame()
                        End If
                    Else
						debug.print "WTF MAN2 " &bFreeplay
						pDMDSplashTwoLines "ADD CREDITS", "CREDITS 0", 3, cRed
						DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
                    End If
                End If
            End If
    End If ' If (GameInPlay)

'table keys
'If keycode = RightMagnaSave or keycode = LeftMagnasave Then ShowPost 
End Sub

Sub Table1_KeyUp(ByVal keycode)

	if keycode = LeftFlipperKey And RenderingMode = 2 then VR_CabLeftFlipperButton.X = VR_CabLeftFlipperButton.X - 8
	if keycode = RightFlipperKey And RenderingMode = 2 then VR_CabRightFlipperButton.X = VR_CabRightFlipperButton.X + 8

	If KeyCode = PlungerKey Then
		plungertimer.enabled = 0
		if BallsOnPlayfield < 1 OR not BIPL then SoundPlungerReleaseNoBall():Plunger.Fire:Exit Sub


		if PTime > 10 Then
			StopSound "IdleTruck"
			StopSound "tiresqueal"
			StopSound "Engine_during_burnout"
			Playsound "launchtruck"
			Plunger.Fire 
			DuckResume
		Else
			StopSound "Engine_during_burnout"
			Playsound "launchtruck"
			Plunger.Fire
			DuckResume
		End If
		PTime = 0  '  Reset PTime

		If BIPL = 1 Then
			SoundPlungerReleaseBall()   'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall() 'Plunger release sound when there is no ball in shooter lane
		End If

	End If

    If hsbModeActive Then
        Exit Sub
    End If






    ' Table specific




    If bGameInPLay AND NOT Tilted Then
		If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
		If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress

        If keycode = LeftFlipperKey Then
            SolLFlipper 0
			if stagedFlipper <> 1 Then SolLFlipper2 0
        End If
        If keycode = RightFlipperKey Then
            SolRFlipper 0
			if stagedFlipper <> 1 Then SolRFlipper2 0
        End If


		If 1 = stagedFlipper Then
			If (LeftMagnaSave = keycode)  Then
				FlipperDeActivate LeftFlipper2, LFPress
				SolLFlipper2 False
			End If

			If (RightMagnaSave = keycode) Then
				FlipperDeActivate RightFlipper2, RFPress
				SolRFlipper2 False
			End If
		End If

    End If
End Sub

	Sub TimerPlunger_Timer
		VR_CabPlunger.Y = 2 + (5* Plunger.Position) -20
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
    If B2SOn = true Then Controller.Stop
End Sub

'********************
'     Flippers
'********************

Sub SolLFlipper2(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
        'LeftFlipper.RotateToEnd
        LeftFlipper2.RotateToEnd
'		Flipper1.RotateToEnd 'Adds To End Movement for Flipper1
		RotateLaneLightsLeft
		'RotateLaneLightsLeft2
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
        'LeftFlipper.RotateToStart
        LeftFlipper2.RotateToStart
'		Flipper1.RotateToStart 'Adds To End Movement for Flipper1
    End If
End Sub

Sub SolRFlipper2(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
        'RightFlipper.RotateToEnd
        RightFlipper2.RotateToEnd
		RotateLaneLightsRight
		'RotateLaneLightsRight2
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
       ' RightFlipper.RotateToStart
        RightFlipper2.RotateToStart
    End If
End Sub


Const ReflipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled) 'Left flipper solenoid callback
	If Enabled Then
		LF.Fire  'leftflipper.rotatetoend
       ' LeftFlipper2.RotateToEnd
		RotateLaneLightsLeft
		
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then
			RandomSoundReflipUpLeft LeftFlipper
		Else
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If
	Else
		LeftFlipper.RotateToStart
   '     LeftFlipper2.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled) 'Right flipper solenoid callback
	If Enabled Then
		RF.Fire 'rightflipper.rotatetoend
       ' RightFlipper2.RotateToEnd
		RotateLaneLightsRight
		
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		RightFlipper.RotateToStart
   '     RightFlipper2.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub



'**************************************************
'        Flipper Collision Subs
'NOTE: COpy and overwrite collision sound from original collision subs over
'RandomSoundFlipper()' below
'**************************************************'

Sub LeftFlipper_Collide(parm)
'	LastSwitchHit = "LF"
    CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
    LeftFlipperCollide parm   'This is the Fleep code
End Sub

Sub LeftFlipper2_Collide(parm)
'	LastSwitchHit = "LF2"
    'CheckLiveCatch Activeball, LeftFlipper2, LFCount, parm
    LeftFlipperCollide parm   'This is the Fleep code
End Sub

Sub RightFlipper_Collide(parm)
'	LastSwitchHit = "RF"
    CheckLiveCatch Activeball, RightFlipper, RFCount, parm
    RightFlipperCollide parm  'This is the Fleep code
End Sub

Sub RightFlipper2_Collide(parm)
'	LastSwitchHit = "RF2"
    'CheckLiveCatch Activeball, RightFlipper2, RFCount, parm
    RightFlipperCollide parm  'This is the Fleep code
End Sub


Sub RotateLaneLightsLeft
    Dim TempState
    TempState = LeftOutlane.State
    LeftOutlane.State = LeftInlane.State
    LeftInlane.State = RightInlane.State
    RightInlane.State = RightOutlane.State
    RightOutlane.State = TempState
End Sub

Sub RotateLaneLightsRight
    Dim TempState
    TempState = RightOutlane.State
    RightOutlane.State = RightInlane.State
    RightInlane.State = LeftInlane.State
    LeftInlane.State = LeftOutlane.State
    LeftOutlane.State = TempState
End Sub

'Sub RotateLaneLightsLeft2
'    Dim TempState
'    TempState = li016.State
'    li016.State = li017.State
'    li017.State = li018.State
'    li018.State = li019.State
'	li019.State = li020.state
'    li020.state = TempState
'End Sub

'Sub RotateLaneLightsRight2
'    Dim TempState
'    TempState = li020.State
'    li020.State = li019.State
'    li019.State = li018.State
'    li018.State = li017.State
'	li017.State = li016.state
'    li016.state = TempState
'End Sub

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                    'Called when table is nudged
    Tilt = Tilt + TiltSensitivity                'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt> TiltSensitivity) AND(Tilt <15) Then 'show a warning
'        DMD "_", CL(1, "CAREFUL!"), "", eNone, eBlinkFast, eNone, 500, True, ""

	pDMDSplashTwoLines "TILT", "WARNING", 3, cYellow
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
    End if
    If Tilt> 15 Then 'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
'        DMDFlush
'        DMD "", "", "TILT", eNone, eNone, eBlink, 200, False, ""

		pDMDSplashTwoLines "TILT", "BALL LOST", 3, cRed
		DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
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
'       Bumper1.Force = 0
'       Bumper2.Force = 0
'		Bumper3.Force = 0
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
'        Bumper1.Force = 8
'        Bumper2.Force = 8
'		Bumper3.Force = 8
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
        'clean up the buffer display
'        DMDFlush
    End If
End Sub



Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield <= 0) Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        EndOfBall()
        TiltRecoveryTimer.Enabled = False
    End If
' else retry (checks again in another second or so)
End Sub

'********************
' Play random quotes
'********************

Sub PlayQuote
    Dim tmp
    tmp = INT(RND * 123) + 1
    PlaySound "HIT_" &tmp
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
    For each bulb in aGILights
        SetLightColor bulb, col, -1
    Next
End Sub

Sub GIUpdateTimer_Timer
    Dim tmp, obj
    tmp = Getballs
    If UBound(tmp) <> OldGiState Then
        OldGiState = Ubound(tmp)
        If UBound(tmp) = 1 Then 'we have 2 captive balls on the table (-1 means no balls, 0 is the first ball, 1 is the second..)
            GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
        Else
            Gion
        End If
    End If
End Sub

Sub GiOn
    DOF 127, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
    For each bulb in aBumperLights
        bulb.State = 1
    Next
' table1.ColorGradeImage = "ColorGradeLUT256x16_HalfSat"
End Sub

Sub GiOff
    DOF 127, DOFOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
    For each bulb in aBumperLights
        bulb.State = 0
    Next
' table1.ColorGradeImage = "ColorGradeLUT256x16_HalfSat-dark"
End Sub

' GI, light & flashers sequence effects

Sub GiEffect(n)
    Dim ii
    Select Case n
        Case 0 'all off
            LightSeqGi.Play SeqAlloff
        Case 1 'all blink
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 15, 10
        Case 2 'random
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 10, 10
        Case 4 'all blink once
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 4, 1
    End Select
End Sub



Sub LightEffect(n)
	StopKOTHSequences

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
        Case 4 'up 1 time
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 8, 1
        Case 5 'up 2 times
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 8, 2
        Case 6 'down 1 time
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqDownOn, 8, 1
        Case 7 'down 2 times
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqDownOn, 8, 2

		Case 11
			'StopKOTHSequences
			LightSeqKOTH1.UpdateInterval = 40
			LightSeqKOTH1.Play SeqRightOn, 40,1000
		Case 12
			'StopKOTHSequences
			LightSeqKOTH2.UpdateInterval = 40
			LightSeqKOTH2.Play SeqRightOn, 40,1000
		Case 13
			'StopKOTHSequences
			LightSeqKOTH3.UpdateInterval = 40
			LightSeqKOTH3.Play SeqRightOn, 40,1000
		Case 14
			'StopKOTHSequences
			LightSeqKOTH4.UpdateInterval = 40
			LightSeqKOTH4.Play SeqRightOn, 40,1000
		Case 15
			'StopKOTHSequences
			LightSeqKOTH5.UpdateInterval = 40
			LightSeqKOTH5.Play SeqRightOn, 40,1000
		Case 16
			'StopKOTHSequences
			LightSeqKOTH6.UpdateInterval = 40
			LightSeqKOTH6.Play SeqRightOn, 40,1000
		Case 17
			'StopKOTHSequences
			LightSeqKOTH7.UpdateInterval = 40
			LightSeqKOTH7.Play SeqRightOn, 40,1000
		Case 18
			'StopKOTHSequences
			LightSeqKOTH8.UpdateInterval = 40
			LightSeqKOTH8.Play SeqRightOn, 40,1000
		Case 19 ' Hank 2b Ramps
	        LightSeqBeer2.UpdateInterval = LargeSeqInterval
            LightSeqBeer2.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqLawnmower.UpdateInterval = LargeSeqInterval
            LightSeqLawnmower.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqHank2.UpdateInterval = LargeSeqInterval
            LightSeqHank2.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqPropane.UpdateInterval = LargeSeqInterval
            LightSeqPropane.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqBBQ2.UpdateInterval = LargeSeqInterval
            LightSeqBBQ2.Play SeqUpOn, LargeSeqUpdate, 1000
		Case 20	' Bill Mode
            LightSeqBill.UpdateInterval = LargeSeqInterval
            LightSeqBill.Play SeqUpOn, LargeSeqUpdate, 1000
		Case 21	' Dale Mode
            LightSeqHank2.UpdateInterval = LargeSeqInterval
            LightSeqHank2.Play SeqUpOn, LargeSeqUpdate, 1000
		Case 22	' Boom Mode
            LightSeqBoom.UpdateInterval = LargeSeqInterval
            LightSeqBoom.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqHank2.UpdateInterval = LargeSeqInterval
            LightSeqHank2.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqBill.UpdateInterval = LargeSeqInterval
            LightSeqBill.Play SeqUpOn, LargeSeqUpdate, 1000
		Case 23 ' Beer 1
            LightSeqLadybird.UpdateInterval = LargeSeqInterval
            LightSeqLadybird.Play SeqUpOn, LargeSeqUpdate, 1000
            LightSeqLuanne.UpdateInterval = LargeSeqInterval
            LightSeqLuanne.Play SeqUpOn, LargeSeqUpdate, 1000
		Case 24 ' Beer 2
	        LightSeqBeer2.UpdateInterval = LargeSeqInterval
            LightSeqBeer2.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqLawnmower.UpdateInterval = LargeSeqInterval
            LightSeqLawnmower.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqHank2.UpdateInterval = LargeSeqInterval
            LightSeqHank2.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqPropane.UpdateInterval = LargeSeqInterval
            LightSeqPropane.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqBBQ2.UpdateInterval = LargeSeqInterval
            LightSeqBBQ2.Play SeqUpOn, LargeSeqUpdate, 1000
		Case 25 ' Beer 3
            LightSeqBoom.UpdateInterval = LargeSeqInterval
            LightSeqBoom.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqLadybird.UpdateInterval = LargeSeqInterval
            LightSeqLadybird.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqBill.UpdateInterval = LargeSeqInterval
            LightSeqBill.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqCotton.UpdateInterval = LargeSeqInterval
            LightSeqCotton.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqLuanne.UpdateInterval = LargeSeqInterval
            LightSeqLuanne.Play SeqUpOn, LargeSeqUpdate, 1000
		Case 26 ' LadyBird
            LightSeqLadybird.UpdateInterval = LargeSeqInterval
            LightSeqLadybird.Play SeqUpOn, LargeSeqUpdate, 1000
		Case 27 ' Lawn Mower
            LightSeqBoom.UpdateInterval = LargeSeqInterval
            LightSeqBoom.Play SeqUpOn, LargeSeqUpdate, 1000

	        LightSeqBeer2.UpdateInterval = LargeSeqInterval
            LightSeqBeer2.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqLadybird.UpdateInterval = LargeSeqInterval
            LightSeqLadybird.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqLawnmower.UpdateInterval = LargeSeqInterval
            LightSeqLawnmower.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqHank2.UpdateInterval = LargeSeqInterval
            LightSeqHank2.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqPropane.UpdateInterval = LargeSeqInterval
            LightSeqPropane.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqBill.UpdateInterval = LargeSeqInterval
            LightSeqBill.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqBBQ2.UpdateInterval = LargeSeqInterval
            LightSeqBBQ2.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqCotton.UpdateInterval = LargeSeqInterval
            LightSeqCotton.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqLuanne.UpdateInterval = LargeSeqInterval
            LightSeqLuanne.Play SeqUpOn, LargeSeqUpdate, 1000
		Case 28 ' Hank 1
            LightSeqPeggy.UpdateInterval = LargeSeqInterval
            LightSeqPeggy.Play SeqUpOn, LargeSeqUpdate, 1000
		Case 29 ' Hank 2a Orbits
            LightSeqBoom.UpdateInterval = LargeSeqInterval
            LightSeqBoom.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqLadybird.UpdateInterval = LargeSeqInterval
            LightSeqLadybird.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqBill.UpdateInterval = LargeSeqInterval
            LightSeqBill.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqCotton.UpdateInterval = LargeSeqInterval
            LightSeqCotton.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqLuanne.UpdateInterval = LargeSeqInterval
            LightSeqLuanne.Play SeqUpOn, LargeSeqUpdate, 1000

		Case 30 ' Hank 3
	        LightSeqYEP.UpdateInterval = LargeSeqInterval
            LightSeqYEP.Play SeqUpOn, LargeSeqUpdate, 1000		

	        LightSeqDrinkAlamo.UpdateInterval = LargeSeqInterval
            LightSeqDrinkAlamo.Play SeqUpOn, LargeSeqUpdate, 1000	

			Light109.State = 2 ' Football

			Light095.State = 2  ' CIA
			Light096.State = 2  ' NSA
			Light097.State = 2  ' FBI

			Light098.State = 2  'P
			Light099.State = 2  'E
			Light100.State = 2  'G
		Case 31 ' Propane
				'NA
		Case 32 ' BBQ
            LightSeqBBQ2.UpdateInterval = LargeSeqInterval
            LightSeqBBQ2.Play SeqUpOn, LargeSeqUpdate, 1000
		Case 35 ' Cotton
			' Chase the Sequence Mode
		Case 36 ' Luanne
            LightSeqHank2.UpdateInterval = LargeSeqInterval
            LightSeqHank2.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqPeggy.UpdateInterval = LargeSeqInterval
            LightSeqPeggy.Play SeqUpOn, LargeSeqUpdate, 1000

		Case 37 ' Bobby
            LightSeqBoom.UpdateInterval = LargeSeqInterval
            LightSeqBoom.Play SeqUpOn, LargeSeqUpdate, 1000

	        LightSeqBeer2.UpdateInterval = LargeSeqInterval
            LightSeqBeer2.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqLadybird.UpdateInterval = LargeSeqInterval
            LightSeqLadybird.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqLawnmower.UpdateInterval = LargeSeqInterval
            LightSeqLawnmower.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqHank2.UpdateInterval = LargeSeqInterval
            LightSeqHank2.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqPropane.UpdateInterval = LargeSeqInterval
            LightSeqPropane.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqBill.UpdateInterval = LargeSeqInterval
            LightSeqBill.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqBBQ2.UpdateInterval = LargeSeqInterval
            LightSeqBBQ2.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqCotton.UpdateInterval = LargeSeqInterval
            LightSeqCotton.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqLuanne.UpdateInterval = LargeSeqInterval
            LightSeqLuanne.Play SeqUpOn, LargeSeqUpdate, 1000
		Case 38 ' DrinkAlamo
	        LightSeqBeer2.UpdateInterval = LargeSeqInterval
            LightSeqBeer2.Play SeqUpOn, LargeSeqUpdate, 1000
		Case 39 ' Peggy 1
            LightSeqBoom.UpdateInterval = LargeSeqInterval
            LightSeqBoom.Play SeqUpOn, LargeSeqUpdate, 1000

	        LightSeqBeer2.UpdateInterval = LargeSeqInterval
            LightSeqBeer2.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqLadybird.UpdateInterval = LargeSeqInterval
            LightSeqLadybird.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqLawnmower.UpdateInterval = LargeSeqInterval
            LightSeqLawnmower.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqHank2.UpdateInterval = LargeSeqInterval
            LightSeqHank2.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqPropane.UpdateInterval = LargeSeqInterval
            LightSeqPropane.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqBill.UpdateInterval = LargeSeqInterval
            LightSeqBill.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqBBQ2.UpdateInterval = LargeSeqInterval
            LightSeqBBQ2.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqCotton.UpdateInterval = LargeSeqInterval
            LightSeqCotton.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqLuanne.UpdateInterval = LargeSeqInterval
            LightSeqLuanne.Play SeqUpOn, LargeSeqUpdate, 1000

		Case 40 ' Peggy 2
			'NA
		Case 41 ' Peggy 3
            LightSeqLuanne.UpdateInterval = LargeSeqInterval
            LightSeqLuanne.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqHank2.UpdateInterval = LargeSeqInterval
            LightSeqHank2.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqBobby.UpdateInterval = LargeSeqInterval
            LightSeqBobby.Play SeqUpOn, LargeSeqUpdate, 1000

            LightSeqLadybird.UpdateInterval = LargeSeqInterval
            LightSeqLadybird.Play SeqUpOn, LargeSeqUpdate, 1000
    End Select
End Sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else1qqqqqq
        Pan = Csng(-((- tmp) ^10))
    End If
End Function


'********************************************
'   Fleep Rolling Sounds
'********************************************

Const tnob = 10 ' total number of balls
Const lob = 0   'number of locked balls

'==================
'******************************************************
'	ZBRL:  BALL ROLLING AND DROP SOUNDS
'******************************************************

' Be sure to call RollingUpdate in a timer with a 10ms interval see the GameTimer_Timer() sub

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
		' Comment the next line if you are not implementing Dyanmic Ball Shadows
'		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next
	
	' exit the sub if no balls on the table
	If UBound(BOT) =  - 1 Then Exit Sub
	
	' play the rolling sound for each ball
	For b = 0 To UBound(BOT)
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
		
		' "Static" Ball Shadows
		' Comment the next If block, if you are not implementing the Dynamic Ball Shadows
'		If AmbientBallShadowOn = 0 Then
'			If BOT(b).Z > 30 Then
'				BallShadowA(b).height = BOT(b).z - BallSize / 4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
'			Else
'				BallShadowA(b).height = 0.1
'			End If
'			BallShadowA(b).Y = BOT(b).Y + offsetY
'			BallShadowA(b).X = BOT(b).X + offsetX
'			BallShadowA(b).visible = 1
'		End If
	Next
End Sub



'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************

'******************************
' Diverse Collection Hit Sounds
'******************************

'Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
'Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
'Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
'Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
'Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
'Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
'Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub

Sub RHelp1_Hit()
    StopSound "fx_metalrolling"
    PlaySoundAtBall "fx_ballrampdrop"
End Sub

Sub RHelp2_Hit()
    StopSound "fx_metalrolling"
    PlaySoundAtBall"fx_ballrampdrop"
End Sub


'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

'****** INSTRUCTIONS please read ******

' The "DynamicBSUpdate" sub should be called with an interval of -1 (framerate)
' Place a toggleable variable (DynamicBallShadowsOn) in user options at the top of the script
' Import the "bsrtx7" and "ballshadow" images
' Import the shadow materials file (3 sets included) (you can also export the 3 sets from this table to create the same file)
' Copy in the sets of primitives named BallShadow#, RtxBallShadow#, and RtxBall2Shadow#, with at least as many objects each as there can be balls
'
' Create a collection called DynamicSources that includes all light sources you want to cast ball shadows
'***These must be organized in order, so that lights that intersect on the table are adjacent in the collection***
' This is because the code will only project two shadows if they are coming from lights that are consecutive in the collection
' The easiest way to keep track of this is to start with the group on the left slingshot and move clockwise around the table
'	For example, if you use 6 lights: A & B on the left slingshot and C & D on the right, with E near A&B and F next to C&D, your collection would look like EBACDF
'
'																E
'	A		 C													B
'	 B		D			your collection should look like		A		because E&B, B&A, etc. intersect; but B&D or E&F do not
'  E		  F													C
'																D
'																F
'
'Update shadow options in the code to fit your table and preference

'****** End Instructions ******

' *** Example timer sub

' The frame timer interval is -1, so executes at the display frame rate
Sub FrameTimer_Timer()
	If DynamicBallShadowsOn=1 Then DynamicBSUpdate 'update ball shadows
End Sub

' *** These are usually defined elsewhere (ballrolling), but activate here if necessary

'Const tnob = 10 ' total number of balls
'Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done

' *** Example "Top of Script" User Option
'Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow, 1 = enable dynamic ball shadow

' *** Shadow Options ***
Const fovY					= -2	'Offset y position under ball to account for layback or inclination (more pronounced need further back, -2 seems best for alignment at slings)
Const DynamicBSFactor 		= 0.99	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.7	'0 to 1, higher is darker
Const Wideness				= 20	'Sets how wide the shadows can get (20 +5 thinness should be most realistic)
Const Thinness				= 5		'Sets minimum as ball moves away from source
' ***				 ***

Dim sourcenames, currentShadowCount

sourcenames = Array ("","","","","","","","","","","","")
currentShadowCount = Array (0,0,0,0,0,0,0,0,0,0,0,0)


dim objrtx1(20), objrtx2(20)
dim objBallShadow(20)
DynamicBSInit

sub DynamicBSInit()
	Dim iii

	for iii = 0 to tnob									'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = iii/1000 + 0.01
		objrtx1(iii).visible = 0
		'objrtx1(iii).uservalue=0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = (iii)/1000 + 0.02
		objrtx2(iii).visible = 0
		'objrtx2(iii).uservalue=0
		currentShadowCount(iii) = 0
		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		objBallShadow(iii).Z = iii/1000 + 0.04
	Next
end sub


Sub DynamicBSUpdate
	Dim falloff:	falloff = 150			'Max distance to light sources, can be changed if you have a reason
	Const AmbientShadowOn = 1				'Toggle for just the moving shadow primitive (ninuzzu's)
	Dim ShadowOpacity, ShadowOpacity2 
	Dim s, Source, LSd, b, currentMat, AnotherSource, BOT
	BOT = GetBalls

	'Hide shadow of deleted balls
	For s = UBound(BOT) + 1 to tnob
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
	Next

	If UBound(BOT) = lob - 1 Then Exit Sub		'No balls in play, exit

'The Magic happens here
	For s = lob to UBound(BOT)

' *** Normal "ambient light" ball shadow
		If AmbientShadowOn = 1 Then
			If BOT(s).X < tablewidth/2 Then
				objBallShadow(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/13)) + 5
			Else
				objBallShadow(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/13)) - 5
			End If
			objBallShadow(s).Y = BOT(s).Y + fovY

			If BOT(s).Z < 30 Then 'or BOT(s).Z > 105 Then		'Defining when (height-wise) you want ambient shadows
				objBallShadow(s).visible = 1
	'			objBallShadow(s).Z = BOT(s).Z - 25 + s/1000 + 0.04		'Uncomment if you want to add shadows to an upper/lower pf
			Else
				objBallShadow(s).visible = 0
			end if
		End If
' *** Dynamic shadows
		For Each Source in DynamicSources
			LSd=DistanceFast((BOT(s).x-Source.x),(BOT(s).y-Source.y))	'Calculating the Linear distance to the Source
			If BOT(s).Z < 30 Then 'Or BOT(s).Z > 105 Then				'Defining when (height-wise) you want dynamic shadows
				If LSd < falloff and Source.state=1 Then	    		'If the ball is within the falloff range of a light and light is on
					currentShadowCount(s) = currentShadowCount(s) + 1	'Within range of 1 or 2
					if currentShadowCount(s) = 1 Then					'1 dynamic shadow source
						sourcenames(s) = source.name
						currentMat = objrtx1(s).material
						objrtx2(s).visible = 0 : objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01							'Uncomment if you want to add shadows to an upper/lower pf
						objrtx1(s).rotz = AnglePP(Source.x, Source.y, BOT(s).X, BOT(s).Y) + 90
						ShadowOpacity = (falloff-LSd)/falloff									'Sets opacity/darkness of shadow by distance to light
						objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness						'Scales shape of shadow with distance/opacity
						UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^2,RGB(0,0,0),0,0,False,True,0,0,0,0
						'debug.print "update1" & source.name & " at:" & ShadowOpacity

						currentMat = objBallShadow(s).material
						UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-ShadowOpacity),RGB(0,0,0),0,0,False,True,0,0,0,0

					Elseif currentShadowCount(s) = 2 Then
																'Same logic as 1 shadow, but twice
						currentMat = objrtx1(s).material
						set AnotherSource = Eval(sourcenames(s))
						objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01							'Uncomment if you want to add shadows to an upper/lower pf
						objrtx1(s).rotz = AnglePP(AnotherSource.x, AnotherSource.y, BOT(s).X, BOT(s).Y) + 90
						ShadowOpacity = (falloff-(((BOT(s).x-AnotherSource.x)^2+(BOT(s).y-AnotherSource.y)^2)^0.5))/falloff
						objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness
						UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0

						currentMat = objrtx2(s).material
						objrtx2(s).visible = 1 : objrtx2(s).X = BOT(s).X : objrtx2(s).Y = BOT(s).Y + fovY
'						objrtx2(s).Z = BOT(s).Z - 25 + s/1000 + 0.02							'Uncomment if you want to add shadows to an upper/lower pf
						objrtx2(s).rotz = AnglePP(Source.x, Source.y, BOT(s).X, BOT(s).Y) + 90
						ShadowOpacity2 = (falloff-LSd)/falloff
						objrtx2(s).size_y = Wideness*ShadowOpacity2+Thinness
						UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
						'debug.print "update2: " & source.name & " at:" & ShadowOpacity & " and "  & Eval(sourcenames(s)).name & " at:" & ShadowOpacity2

						currentMat = objBallShadow(s).material
						UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
					end if
				Else
					currentShadowCount(s) = 0
				End If
			Else									'Hide dynamic shadows everywhere else
				objrtx2(s).visible = 0 : objrtx1(s).visible = 0
			End If
		Next
	Next
End Sub


Function DistanceFast(x, y)
	dim ratio, ax, ay
	'Get absolute value of each vector
	ax = abs(x)
	ay = abs(y)
	'Create a ratio
	ratio = 1 / max(ax, ay)
	ratio = ratio * (1.29289 - (ax + ay) * ratio * 0.29289)
	if ratio > 0 then
		DistanceFast = 1/ratio
	Else
		DistanceFast = 0
	End if
end Function

Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function
							'Enable these functions if they are not already present elswhere in your table
Dim PI: PI = 4*Atn(1)


'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************


' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
    Dim i,j

	if bRuleCards = False Then DMDQueue.Add "ResetBackglass","ResetBackglass",95,2100,0,0,0,False

	PTime = 0 ' used for plunger timer sound effects
	bSqueal = True
    bGameInPLay = True

	nBallSaveCounter = RndNbr(4)

    'resets the score display, and turn off attract mode
    StopAttractMode
    GiOn

	pDMDSetPage(pScores)

    TotalGamesPlayed = TotalGamesPlayed + 1
    CurrentPlayer = 1
    PlayersPlayingGame = 1
    bOnTheFirstBall = True
	bPeggyReady = False
	bDaleReady = False
	bBoomReady = False
	bBillReady = False
	bLuanneReady = False
	bBobbyReady = False
	bEBReady = False
	nYepReady = 0
	MasterMowerLocation = 0
	bLockEngaged = False
	bReleasingBalls = False
	'Multiball=false	
    For i = 1 To MaxPlayers
        Score(i) = 0
        BonusPoints(i) = 0
		'BonusHeldPoints(i) = 0
		CompletedModes(i) = 0
		bHillBall(i) = 0
		bKingBall(i) = 0
		anBeer(i) = 0
        nBonusX(i) = 1
		MultiplierRank(i) = 0
        nPlayfieldX(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
		BumperHitCount(i) = 0
        Special1Awarded(i) = False
        Special2Awarded(i) = False
        Special3Awarded(i) = False
    Next

	'Reset Modes
	For i = 1 to MaxPlayers
		for j = 0 to 19
			Mode(i,j) = 0
		Next
	Next

	
	ResetAllProgress
	DMDUpdateAll
	RemoveCollisionsBobby

    ' initialise any other flags
    Tilt = 0

	'reset variables
	bumperHits = 100

    'UpdateMusic = 0
    'UpdateMusic = UpdateMusic + 6
    'UpdateMusicNow
	PlayModeMusic
	MVTargetDescend.Enabled = 1

    ' initialise Game variables
    Game_Init()
	ResetLockedBalls
	UpdateLockKickers
	
    ' you may wish to start some music, play a sound, do whatever at this point
StopAllMusic

    vpmtimer.addtimer 1500, "FirstBall '"
End Sub

Sub ResetLockedBalls
	nLockedBalls = 0
End Sub


Sub UpdateLockKickers

	' Reset from add a ball with ball lock
	BallHandlingQueue.Add "bAutoplunger = False","bAutoplunger = False",70,600,0,0,0,False

	if bLockEngaged Then
		' Need to turn on the kicker in front of the currently locked ball
		Select Case nLockedBalls
			Case 0
				TractorKicker1.Enabled = True
				TractorKicker2.Enabled = False
				TractorKicker3.Enabled = False
				TractorKicker4.Enabled = False
				TractorKicker5.Enabled = False
			Case 1
				TractorKicker1.Enabled = True
				TractorKicker2.Enabled = True
				TractorKicker3.Enabled = False
				TractorKicker4.Enabled = False
				TractorKicker5.Enabled = False
			Case 2
				TractorKicker1.Enabled = True
				TractorKicker2.Enabled = True
				TractorKicker3.Enabled = True
				TractorKicker4.Enabled = False
				TractorKicker5.Enabled = False
			Case 3
				TractorKicker1.Enabled = True
				TractorKicker2.Enabled = True
				TractorKicker3.Enabled = True
				TractorKicker4.Enabled = True
				TractorKicker5.Enabled = False
			Case 4
				TractorKicker1.Enabled = True
				TractorKicker2.Enabled = True
				TractorKicker3.Enabled = True
				TractorKicker4.Enabled = True
				TractorKicker5.Enabled = True
			Case 5
				TractorKicker1.Enabled = True
				TractorKicker2.Enabled = True
				TractorKicker3.Enabled = True
				TractorKicker4.Enabled = True
				TractorKicker5.Enabled = True
		End Select

	Else
		TractorKicker1.Enabled = False
		TractorKicker2.Enabled = False
		TractorKicker3.Enabled = False
		TractorKicker4.Enabled = False
		TractorKicker5.Enabled = False
	End If


End Sub

Sub ResetBallLocks
	TractorKicker1.Enabled = False
	TractorKicker2.Enabled = False
	TractorKicker3.Enabled = False
	TractorKicker4.Enabled = False
	TractorKicker5.Enabled = False
End Sub


' This is used to delay the start of a game to allow any attract sequence to

' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
    ' reset the table for a new ball
    ResetForNewPlayerBall()
    ' create a new ball in the shooters lane
    CreateNewBall()
	bAlreadyPlayed = True
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))

Sub ResetForNewPlayerBall()
    ' make sure the correct display is upto date
    AddScore 0

    ' set the current players bonus multiplier back down to 1X
    nBonusX(CurrentPlayer) = 1
    'UpdateBonusXLights
	
' reset any drop targets, lights, game Mode etc..
    
   'This is a new ball, so activate the ballsaver
    bBallSaverReady = True
	bSkillShotReady = True
	bMowerNotStarted = True
	bWizardMode = False
	bConnie = True   ' restricts one add a ball per played ball
    'Reset any table specific
	nCottonActive = 0
	BumperBonus = 0
	HoleBonus = 0
	ALLRampBonus = 0
	RampBonus1 = 0
	RampBonus2 = 0
	RampBonus3 = 0
	MulitballBonus = 0
    ResetNewBallVariables


	AudioQueue.Add "UpdateMusicNow","UpdateMusicNow",65,50,0,0,0,False
	

    ResetNewBallLights()
	DMDUpdateAll
	UpdateKOTHLights
	UpdateModeCompleteLights
	ResetYEP(CurrentPlayer)	
	CloseCooler
	
	DisplayLockPopUp
	'Multiball=false	
End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()
    
	LightSeqAttract.StopPlay

	' create a ball in the plunger lane kicker.
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' kick it out..
    'PlaySoundAt SoundFXDOF("fx_Ballrel", 123, DOFPulse, DOFContactors), BallRelease
	DOF 111,DOFPulse
	RandomSoundBallRelease ballrelease
    BallRelease.Kick 90, 4

	'only this tableDrain / Plunger Functions
	'ChangeBallImage

    If BallsOnPlayfield> 1 Then
        bMultiBallMode = True
        bAutoPlunger = True
        'ChangeSong
	Else
		debug.print"BallSaverTime: " &BallSaverTime
		if bBallSaverReady Then Playsound "IdleTruck", -1, 1
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

	Dim Msg1
Sub DisplayEOB1
	if DMDType = 5 Then
		Msg1 = RampCount &" RAMPS * " & nPlayfieldX(CurrentPlayer) & "X = "&AwardPoints(1)
		PuPlayer.LabelSet pDMD,"Event5A",msg1,1,"{'mt':2,'fonth':6}"	
	Else
		Msg1 = RampCount &" RAMPS * " & SCORE_RAMP & " * " & nPlayfieldX(CurrentPlayer) & "X = "&AwardPoints(1)
		PuPlayer.LabelSet pDMD,"Event5A",msg1,1,"" 
	End If
 
End Sub

	Dim Msg2
Sub DisplayEOB2
	if DMDType = 5 Then
		Msg2 = OrbitCount &" ORBITS * " & nPlayfieldX(CurrentPlayer)& "X = "&AwardPoints(2)
		PuPlayer.LabelSet pDMD,"Event5B",msg2,1,"{'mt':2,'fonth':6}"
	Else
		Msg2 = OrbitCount &" ORBITS * " & SCORE_ORBIT & " * " & nPlayfieldX(CurrentPlayer)& "X = "&AwardPoints(2)
		PuPlayer.LabelSet pDMD,"Event5B",msg2,1,""  
	End If

End Sub

	Dim Msg3
Sub DisplayEOB3
	if DMDType = 5 Then
		Msg3 = TargetCount &" TARGETS * " & nPlayfieldX(CurrentPlayer)& "X = "&AwardPoints(3)
		PuPlayer.LabelSet pDMD,"Event5C",msg3,1,"{'mt':2,'fonth':6}" 
	Else
		Msg3 = TargetCount &" TARGETS * " & SCORE_TARGET & " * " & nPlayfieldX(CurrentPlayer)& "X = "&AwardPoints(3)
		PuPlayer.LabelSet pDMD,"Event5C",msg3,1,""  
	End If

End Sub

	Dim Msg4

Sub DisplayEOB4
	if DMDType = 5 Then
		Msg4 = BumperCount &" BUMPERS * " & nPlayfieldX(CurrentPlayer)& "X = "&AwardPoints(4)
		PuPlayer.LabelSet pDMD,"Event5D",msg4,1,"{'mt':2,'fonth':6}" 
	Else
		Msg4 = BumperCount &" BUMPERS * " & SCORE_BUMPER & " * " & nPlayfieldX(CurrentPlayer)& "X = "&AwardPoints(4)
		PuPlayer.LabelSet pDMD,"Event5D",msg4,1,""  
	End If

End Sub

	Dim Msg4b
Sub DisplayEOB4b
	if DMDType = 5 Then
		Msg4b = "MODE BONUSES = " &AwardPoints(5)
		PuPlayer.LabelSet pDMD,"Event5E",msg4b,1,"{'mt':2,'fonth':6}"
	Else
		Msg4b = "MODE BONUSES " & BonusPoints(CurrentPlayer) & " * " & nBonusX(CurrentPlayer)& "X = "&AwardPoints(5)
		PuPlayer.LabelSet pDMD,"Event5E",msg4b,1,""  
	End If

End Sub

Dim AwardPoints(6)
Dim TmpPoints 
Dim CurrRound
Dim STEPCOUNT : STEPCOUNT = 10
Sub DisplayEOB5(Round)

	EOB_Step = 1
	EOB_Bonus.Enabled = 1
	CurrRound = Round

	Debug.print "ROUND:"&Round
	TmpPoints = AwardPoints(Round)/STEPCOUNT

  
End Sub



' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded
Dim TotalBonus
Sub EndOfBall()
	Dim BonusDelayTime
	' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)

	'LightSeqAttract.Play SeqBlinking, , 5, 150

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{red}:Ball "&Balls & " Lost"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If


	pDMDLabelhide "LockValue"
	pDMDLabelhide "LockPopup"
	' stop all mode sequences
	WipeAllQueues
	ExitModetimer
	StopAllModeSeq
	StopAnyModes

	'make sure lights reset -- should never happen
	if coolerlight.state = 1 Then ResetDrinkAlamo(CurrentPlayer)

	sMusicTrack = ""
	StopAllMusic
	'bonuscheckie

	ClearTwoLines
	PuPDrain

    TotalBonus = 0

    'If NOT Tilted Then
	If(Tilted = False) Then

		AwardPoints(1) = RampCount*SCORE_RAMP*nPlayfieldX(CurrentPlayer)
		AwardPoints(2) = OrbitCount*SCORE_RAMP*nPlayfieldX(CurrentPlayer)
		AwardPoints(3) = TargetCount*SCORE_RAMP*nPlayfieldX(CurrentPlayer)
		AwardPoints(4) = BumperCount*SCORE_RAMP*nPlayfieldX(CurrentPlayer)
		AwardPoints(5) = BonusPoints(CurrentPlayer) * nBonusX(CurrentPlayer)
		
		EOBQueue.Add "Line1", "DisplayEOB1", 50, 100, 0,0,0,False 
		'EOBQueue.Add "Line1b","Addscore (RampCount*SCORE_RAMP*nPlayfieldX(CurrentPlayer))" , 50, 10, 0,0,0,False 
		EOBQueue.Add "Line6a", "DisplayEOB5 1 ", 150, 400, 0,0,0,False 

		EOBQueue.Add "Line2", "DisplayEOB2", 50, 1000, 0,0,0,False 
		'EOBQueue.Add "Line2b","Addscore (OrbitCount*SCORE_ORBIT*nPlayfieldX(CurrentPlayer))" , 50, 1000, 0,0,0,False 
		EOBQueue.Add "Line6b", "DisplayEOB5 2", 50, 1400, 0,0,0,False 

		EOBQueue.Add "Line3", "DisplayEOB3", 50, 2000, 0,0,0,False 
		'EOBQueue.Add "Line3b","Addscore (TargetCount*SCORE_TARGET*nPlayfieldX(CurrentPlayer))" , 50, 2000, 0,0,0,False 
		EOBQueue.Add "Line6c", "DisplayEOB5 3", 50, 2400, 0,0,0,False 


		EOBQueue.Add "Line4", "DisplayEOB4", 50, 3000, 0,0,0,False 
		'EOBQueue.Add "Line4b", "Addscore (BumperCount*SCORE_TARGET*nPlayfieldX(CurrentPlayer))", 50, 3000, 0,0,0,False 
		EOBQueue.Add "Line6d", "DisplayEOB5 4", 50, 3400, 0,0,0,False 

		EOBQueue.Add "Line5", "DisplayEOB4b", 50, 4000, 0,0,0,False 
		'EOBQueue.Add "Line5b", "Score(CurrentPlayer) = Score(CurrentPlayer) + TotalBonus", 50, 4000, 0,0,0,False 
		EOBQueue.Add "Line6e", "DisplayEOB5 5", 50, 4400, 0,0,0,False 


		' add a bit of a delay to allow for the bonus points to be shown & added up
		EOBQueue.Add "Line6","ClearEOBDMD", 50, 5300, 0,0,0,False 
		EOBQueue.Add "Line6f","EndOfBall2", 50, 5400, 0,0,0,False 

    Else 'Si hay falta simplemente espera un momento y va directo a la segunta parte después de perder la bola
		BonusDelayTime = 100
		EndOfBall2
    End If
	'vpmtimer.addtimer BonusDelayTime, "EndOfBall2 '"
End Sub

Dim EOB_Step
EOB_BONUS.Interval = 55
Sub EOB_Bonus_Timer
	Dim Msg5
	
	'debug.print CurrRound &": Tmp points:" &TmpPoints
	Addscore TmpPoints
	

	Select Case CurrRound
		Case 1
			Msg1 = RampCount &" RAMPS * " & SCORE_RAMP & " * " & nPlayfieldX(CurrentPlayer) & "X = "&AwardPoints(1) - TmpPoints*EOB_Step
			PuPlayer.LabelSet pDMD,"Event5A",msg1,1,""  
		Case 2
			Msg2 = OrbitCount &" ORBITS * " & SCORE_ORBIT & " * " & nPlayfieldX(CurrentPlayer)& "X = "&AwardPoints(2) - TmpPoints*EOB_Step
			PuPlayer.LabelSet pDMD,"Event5B",msg2,1,""  
		Case 3
			Msg3 = TargetCount &" TARGETS * " & SCORE_TARGET & " * " & nPlayfieldX(CurrentPlayer)& "X = "&AwardPoints(3) - TmpPoints*EOB_Step
			PuPlayer.LabelSet pDMD,"Event5C",msg3,1,""  
		Case 4
			Msg4 = BumperCount &" BUMPERS * " & SCORE_BUMPER & " * " & nPlayfieldX(CurrentPlayer)& "X = "&AwardPoints(4) - TmpPoints*EOB_Step
			PuPlayer.LabelSet pDMD,"Event5D",msg4,1,""  
		Case 5
			Msg4b = "MODE BONUSES " & BonusPoints(CurrentPlayer) & " * " & nBonusX(CurrentPlayer)& "X = "&AwardPoints(5) - TmpPoints*EOB_Step
			PuPlayer.LabelSet pDMD,"Event5E",msg4b,1,""  
	End Select

	TotalBonus = TotalBonus + TmpPoints
	Msg5 = "TOTAL BONUS " & FormatScore(TotalBonus)
	PuPlayer.LabelSet pDMD,"Event5F",msg5,1,""

	EOB_Step = EOB_Step + 1
	if EOB_Step > 10 Then EOB_Bonus.Enabled = 0
End Sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the CurrentPlayer)
'
Sub EndOfBall2()
    ' if were tilted, reset the internal tilted flag (this will also
    ' set TiltWarnings back to zero) which is useful if we are changing player LOL
    'UpdateMusic = UpdateMusic + 1
	'PlayModeMusic	
    Tilted = False
    Tilt = 0
    DisableTable False 'enable again bumpers and slingshots

    ' has the player won an extra-ball ? (might be multiple outstanding)
    If(ExtraBallsAwards(CurrentPlayer) <> 0) Then
        'debug.print "Extra Ball"

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1
		HideExtraBall

        ' if no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurrentPlayer) = 0) Then
            LightShootAgain.State = 0
        End If


        ' reset the playfield for the new ball
        ResetForNewPlayerBall()
		
		' set the dropped wall for bonus

		
        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls
		' Move the tractor back in place
		'MoveLawnMowerReverse

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
	bSqueal = True  ' reset for new ball

    debug.print "EndOfBall - Complete"

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
		StopAllMusic

		'20H RTP changed from 13000 to 3000
        vpmtimer.addtimer 1000, "EndOfGame() '"

    ' you may wish to put a Game Over message on the desktop/backglass

    Else
        ' set the next player
        CurrentPlayer = NextPlayer

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

        ' AND create a new ball
        CreateNewBall()

        ' play a sound if more than 1 player
        If PlayersPlayingGame> 1 Then
            PlaySound "vo_player" &CurrentPlayer
        End If
    End If
End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()

	Overhead_GIoff
	LetEmGo	' release all locked balls

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Red}:Game Over"
		Scorbit.SetGameMode(GameModeStrTmp)
		StopScorbit
	End If

    LightSeqAttract.StopPlay
	'debug.print "End Of Game"
    bGameInPLay = False
    ' just ended your game then play the end of game tune
    If NOT bJustStarted Then
        'UpdateMusicNow
		'PlayModeMusic
		'SwitchMusic "M_End"
    End If


    bJustStarted = False
    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0

	MoveLawnMowerReverse

	ClearTwoLines
	PupGameOver
	'pDMDSplashTwoLines "GAME", "OVER", 4, cPurple
	'DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,4100,0,0,0,True
    ' terminate all Mode - eject locked balls
    ' most of the Mode/timers terminate at the end of the ball

    ' set any lights for the attract mode
    GiOff
	DMDQueue.Add "StartAttractMode","StartAttractMode",95,4300,0,0,0,True
    
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
    BallsOnPlayfield = BallsOnPlayfield - 1 
	'If BallsOnPlayfield<2 Then
	'Multiball=false
	'end if
	
    ' pretend to knock the ball into the ball storage mech
    RandomSoundDrain Drain
    'if Tilted then end Ball Mode
    If Tilted Then
        StopEndOfBallMode
    End If

    ' if there is a game in progress AND it is not Tilted
    If(bGameInPLay = True) AND(Tilted = False) Then

        ' is the ball saver active,
        If(bBallSaverActive = True) Then
			if ballsonplayfield <= 0 Then 
				PupBallSave
				pDMDSplashTwoLines " BALL ", " SAVED ", 3, cRed
				DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True'
			End If
			'AddMultiball 1
			bAutoPlunger = True
			DMDQueue.Add "AddMultiball-"&gametime,"AddMultiball 1",65,500,0,0,0,False

			if Scorbit.bSessionActive then
				GameModeStrTmp="BL{green}:Ball Saved"
				Scorbit.SetGameMode(GameModeStrTmp)
			End If
           
        Else

			If(BallsOnPlayfield > 1)Then
				Select Case Mode(CurrentPlayer,0)
					Case 1
						'if Mode(CurrentPlayer,1) = 2 Then 
					Case 2
						'if Mode(CurrentPlayer,2) = 2 Then 
					Case 3
						'if Mode(CurrentPlayer,3) = 2 Then 
					Case 4
						if Mode(CurrentPlayer,4) = 2 Then PupFailLuanne
					Case 5
						if Mode(CurrentPlayer,5) = 2 Then PupFailBoom
					Case 6
						if Mode(CurrentPlayer,6) = 2 Then PupFailBill
					Case 7
						'if Mode(CurrentPlayer,7) = 2 Then 
					Case 8
						'if Mode(CurrentPlayer,8) = 2 Then
					Case 9
						'if Mode(CurrentPlayer,9) = 2 Then
					Case 10
						'if Mode(CurrentPlayer,10) = 2 Then
					Case 11
						'if Mode(CurrentPlayer,11) = 2 Then
					Case 12
						if Mode(CurrentPlayer,12) = 2 Then PupFailBobby
					Case 13
						'if Mode(CurrentPlayer,13) = 2 Then
					Case 14
						'if Mode(CurrentPlayer,14) = 2 Then
					Case 15
						'if Mode(CurrentPlayer,15) = 2 Then
					Case 16
						'if Mode(CurrentPlayer,16) = 2 Then
					Case 17
						'if Mode(CurrentPlayer,17) = 2 Then
					Case 18
						'if Mode(CurrentPlayer,18) = 2 Then
					Case 19
						'if Mode(CurrentPlayer,19) = 2 Then
				End Select
			End If

			If(BallsOnPlayfield = 1)Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True)then
                    ' not in multiball mode any more
                    bMultiBallMode = False
                    ' you may wish to change any music over at this point and
                    ' turn off any multiball specific lights
				'UpdateMusicNow
					StopMB
					PlayModeMusic
                End If
            End If
            ' was that the last ball on the playfield
            If(BallsOnPlayfield <= 0) Then

                ' End Mode and timers
				StopAllMusic

				DOF 243, DOFPulse
				DOF 265, DOFOff
				
				'vpmtimer.addtimer 3000, "ChangeSong '"
                ' Show the end of ball animation
                ' and continue with the end of ball
                ' DMD something?
                StopEndOfBallMode
                vpmtimer.addtimer 200, "EndOfBall '" 'the delay is depending of the animation of the end of ball, since there is no animation then move to the end of ball
            End If
        End If
    End If
End Sub

Sub StopMB
	if Mode(CurrentPlayer,4) = 2 Then WinLuanne
	if Mode(CurrentPlayer,5) = 2 Then WinBoom
	if Mode(CurrentPlayer,6) = 2 Then WinBill
	if Mode(CurrentPlayer,10) = 2 Then WinDale
	if Mode(CurrentPlayer,12) = 2 Then WinBobby
	if Mode(CurrentPlayer,16) = 2 Then stoplawnmower
End Sub



Sub UpdateModeCompleteLights
' 3 - PEG Third Mission (K)
' 4 - Luanne 			(I)
' 5 - Boomhauer 		(N)
' 6 - Bill 				(G)
' 7 - Hank First Mission	
' 8 - Hank Second Mission	
' 9 - Hank Third Mission(H)
' 10 - Dale				(I)
' 11 - Cotton			(L)
' 12 - Bobby			(L)

	if Mode(CurrentPlayer,3) = 1 Then Light143.State = 1
	if Mode(CurrentPlayer,4) = 1 Then Light148.State = 1
	if Mode(CurrentPlayer,5) = 1 Then Light145.State = 1
	if Mode(CurrentPlayer,6) = 1 Then Light146.State = 1

	if Mode(CurrentPlayer,9) = 1 Then Light147.State = 1
	if Mode(CurrentPlayer,10) = 1 Then Light144.State = 1
	if Mode(CurrentPlayer,11) = 1 Then Light149.State = 1
	if Mode(CurrentPlayer,12) = 1 Then Light150.State = 1
End Sub


Sub CloseCooler
	coolerlight.state = 0
	CoolerTop.objRotx = 0
End Sub

Sub OpenCooler
DOF 114,DOFPulse
	coolerlight.state = 1
	CoolerTop.objRotx = 40
End Sub


' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.

Sub Trigger1_Hit()
	If bAutoPlunger Then
        'debug.print "autofire the ball"
        PlungerIM.AutoFire
        DOF 121, DOFPulse
       ' PlaySoundAt "fx_fire", Trigger1
		SoundPlungerReleaseBall
        bAutoPlunger = False
    End If	

'StopSong
    bBallInPlungerLane = True
    If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
        'EnableBallSaver BallSaverTime  -- handled in gate001_hit routine
    Else
        ' show the message to shoot the ball in case the player has fallen sleep
 '       Trigger1.TimerEnabled = 1
    End If
End Sub

' The ball is released from the plunger

Sub Trigger1_UnHit()
    
    'LightEffect 4
	'ChangeSong
End Sub


Sub Trigger1_Timer
'	pDMDSplashTwoLines "SHOOT", "THE BALL", 3, cRed
'	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
'    trigger1.TimerEnabled = 0
End Sub

Sub EnableBallSaver(seconds)
    'debug.print "Ballsaver started"
    ' set our game flag
    bBallSaverActive = True
    bBallSaverReady = False
    ' start the timer
    BallSaverTimer.Interval = 1000 * seconds
    BallSaverTimer.Enabled = True
    BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
    BallSaverSpeedUpTimer.Enabled = True
    ' if you have a ball saver light you might want to turn it on at this point (or make it flash)
    LightShootAgain.BlinkInterval = 160
    LightShootAgain.State = 2
End Sub

' The ball saver timer has expired.  Turn it off AND reset the game flag
'
Sub BallSaverTimer_Timer()
    'debug.print "Ballsaver ended"
    BallSaverTimer.Enabled = False
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

	if points = SCORE_JACKPOT Then playsound "Jackpot1"
	if points = SCORE_SUPER_JACKPOT Then playsound "Jackpot3"

    ' add the points to the current players score variable
    Score(CurrentPlayer) = Score(CurrentPlayer) + (points*nPlayfieldX(CurrentPlayer))

    ' play a sound for each score
'	PlaySound "tone"&points

    ' you may wish to check to see if the player has gotten an extra ball by a high score
 '   If Score(CurrentPlayer) >= Special1 AND Special1Awarded(CurrentPlayer) = False Then
 '       AwardExtraBall
 '       Special1Awarded(CurrentPlayer) = True
 '   End If
 '   If Score(CurrentPlayer) >= Special2 AND Special2Awarded(CurrentPlayer) = False Then
 '       AwardExtraBall
 '       Special2Awarded(CurrentPlayer) = True
 '   End If
 '   If Score(CurrentPlayer) >= Special3 AND Special3Awarded(CurrentPlayer) = False Then
 '       AwardExtraBall
 '       Special3Awarded(CurrentPlayer) = True
 '   End If

	if PlayersPlayingGame = 1 Then
		PuPlayer.LabelSet pDMD,"CurrScore"," " & FormatScore(Score(CurrentPlayer)) & " ",1,"{'mt':2,'fonth':14,'xpos':50,'ypos':89.5}"	
	Else
		PuPlayer.LabelSet pDMD,"CurrScore"," " & FormatScore(Score(CurrentPlayer)) & " ",1,"{'mt':2,'fonth':9,'xpos':50,'ypos':87.0}"
	End If
End Sub

' Add bonus to the bonuspoints AND update the score board
Sub AddBonus(points) 'not used in this table, since there are many different bonus items.
    If(Tilted = False) Then
        ' add the bonus to the current players bonus variable
       ' BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + (points*nBonusX(CurrentPlayer)) 
        BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
    End if
End Sub

Sub AwardExtraBall()
    PlaySound SoundFXDOF("fx_knocker", 134, DOFPulse, DOFKnocker)
    ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
	Light008.State = 0
	bEBReady = False
	DisplayExtraBall

	pDMDSplashTwoLines "EXTRA BALL", "AWARDED", 3, cBlue
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True


    LightShootAgain.State = 1
    LightEffect 2
End Sub

'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
    Dim x
    x = LoadValue(TableName, "HighScore1")
    If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 100000 End If
    x = LoadValue(TableName, "HighScore1Name")
    If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If
    x = LoadValue(TableName, "HighScore2")
    If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 100000 End If
    x = LoadValue(TableName, "HighScore2Name")
    If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If
    x = LoadValue(TableName, "HighScore3")
    If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 100000 End If
    x = LoadValue(TableName, "HighScore3Name")
    If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If
    x = LoadValue(TableName, "HighScore4")
    If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 100000 End If
    x = LoadValue(TableName, "HighScore4Name")
    If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If
    x = LoadValue(TableName, "Credits")
    If(x <> "") then Credits = CInt(x) Else Credits = 0:If bFreePlay = 0 Then DOF 125, DOFOff:End If
    x = LoadValue(TableName, "TotalGamesPlayed")
    If(x <> "") then TotalGamesPlayed = CInt(x) Else TotalGamesPlayed = 0 End If
End Sub

Sub Savehs
    SaveValue TableName, "HighScore1", HighScore(0)
    SaveValue TableName, "HighScore1Name", HighScoreName(0)
    SaveValue TableName, "HighScore2", HighScore(1)
    SaveValue TableName, "HighScore2Name", HighScoreName(1)
    SaveValue TableName, "HighScore3", HighScore(2)
    SaveValue TableName, "HighScore3Name", HighScoreName(2)
    SaveValue TableName, "HighScore4", HighScore(3)
    SaveValue TableName, "HighScore4Name", HighScoreName(3)
    SaveValue TableName, "Credits", Credits
    SaveValue TableName, "TotalGamesPlayed", TotalGamesPlayed
End Sub

Sub Reseths
    HighScoreName(0) = "RTP"
    HighScoreName(1) = "JOE"
    HighScoreName(2) = "GMA"
    HighScoreName(3) = "ZAC"
    HighScore(0) = 2000000
    HighScore(1) = 1500000
    HighScore(2) = 1000000
    HighScore(3) = 750000
    Savehs
End Sub

Sub HighScoreHelper(lOne,lTwo,lTime)
	'PuPEvent 815
	'load highscore video or image
	ClearHighScoreTwoLine

	'pDMDSetPage(4)
	PuPlayer.LabelSet pDMD,"HSLine1",lOne,1,"{'mt':2,'color':" & cPurple & "}"
	PuPlayer.LabelSet pDMD,"HSLine2",lTwo,1,"{'mt':2,'color':" & cPurple & "}"
	'DMDQueue.Add "pDMDSetPage-1","pDMDSetPage(1)",95,lTime,0,0,0,True
End Sub

sub ClearHighScoreTwoLine
	PuPlayer.LabelSet pDMD,"HSLine1","",0,""
	PuPlayer.LabelSet pDMD,"HSLine2","",0,""
End Sub

' ***********************************************************
'  High Score Initals Entry Functions - based on Black's code
' ***********************************************************

Dim hsbModeActive
Dim hsEnteredName
Dim hsEnteredDigits(3)
Dim hsCurrentDigit
Dim hsValidLetters
Dim hsCurrentLetter
Dim hsLetterFlash

Sub CheckHighscore()
    Dim tmp
	if bEnablePup Then
		'pDMDSetPage(4)
		'dbg "in Check high score"
		tmp = Score(CurrentPlayer)

		If tmp > HighScore(0)Then 'add 1 credit for beating the highscore
			Credits = Credits + 1
		End If

		If tmp > HighScore(3)Then
			HighScore(3) = tmp
			HighScoreEntryInit()
		Else
			 EndOfBallComplete()  
		End If

	End If
End Sub

Sub HighScoreEntryInit()
	if bEnablePuP Then

		ClearTwoLines
		'pDMDSetPage(4)
		'dbg "++++++++++++++++    In High Score Entry Init "
		'PuPlayer.LabelSet pDMD,"CurrScore","",0,""   ' Clear current score
		hsbModeActive = True
		hsLetterFlash = 0

		hsEnteredDigits(0) = " "
		hsEnteredDigits(1) = " "
		hsEnteredDigits(2) = " "
		hsCurrentDigit = 0

		hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789<" ' < is back arrow
		hsCurrentLetter = 1
		ClearHighScoreTwoLine
		HighScoreHelper "YOUR NAME:", " <A  > ", 9999
		HighScoreDisplayNameNow()

		HighScoreFlashTimer.Interval = 250
		HighScoreFlashTimer.Enabled = True
	End If
    HighScoreDisplayName()
End Sub

Sub HighScoreDisplayNameNow()
	'dbg "in HS display name now"
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub EnterHighScoreKey(keycode)
    If keycode = LeftFlipperKey Then
        Playsound "fx_Previous"
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0)then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayName()
    End If

    If keycode = RightFlipperKey Then
        Playsound "fx_Next"
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter > len(hsValidLetters))then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayName()
    End If

    If keycode = PlungerKey OR keycode = StartGameKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<")then
            playsound "fx_Enter"
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3)then
                HighScoreCommitName()
            else
                HighScoreDisplayName()
            end if
        else
            playsound "fx_Esc"
            hsEnteredDigits(hsCurrentDigit) = " "
            if(hsCurrentDigit > 0)then
                hsCurrentDigit = hsCurrentDigit - 1
            end if
            HighScoreDisplayName()
        end if
    end if
End Sub

Sub HighScoreDisplayName()
    Dim i, TempStr
	if bEnablePuP Then
		'dbg "in HS display name"
	'	pDMDSetPage(4)

		TempStr = " >"
		if(hsCurrentDigit> 0)then TempStr = TempStr & hsEnteredDigits(0)
		if(hsCurrentDigit> 1)then TempStr = TempStr & hsEnteredDigits(1)
		if(hsCurrentDigit> 2)then TempStr = TempStr & hsEnteredDigits(2)

		if(hsCurrentDigit <> 3)then
			if(hsLetterFlash <> 0)then
				TempStr = TempStr & "_"
			else
				TempStr = TempStr & mid(hsValidLetters, hsCurrentLetter, 1)
			end if
		end if

		if(hsCurrentDigit <1)then TempStr = TempStr & hsEnteredDigits(1)
		if(hsCurrentDigit <2)then TempStr = TempStr & hsEnteredDigits(2)

		TempStr = TempStr & "< "
		HighScoreHelper "YOUR NAME:", Mid(TempStr, 2, 5), 9999
	End If
End Sub

Sub HighScoreCommitName()
	if bEnablePup Then
		'pDMDSetPage(4)
		'dbg " %%%%%%%%%%%  HighScore Commited"
		HighScoreFlashTimer.Enabled = False
		hsbModeActive = False

		hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
		if(hsEnteredName = "   ")then
			hsEnteredName = "YOU"
		end if
		HighScoreName(3) = hsEnteredName
		SortHighscore
		ClearInitials
	End If
    EndOfBallComplete()
End Sub

Sub ClearInitials
	ClearHighScoreTwoLine
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


Function FormatScore(ByVal Num) 'it returns a string with commas (as in Black's original font)
    dim i
    dim NumString

	if Num=0 then 
		FormatScore="0"
		Exit Function
	End if 

    NumString = CStr(abs(Num))

    For i = Len(NumString)-3 to 1 step -3
        if IsNumeric(mid(NumString, i, 1))then
            NumString = left(NumString, i) & "," & right(NumString, Len(NumString)-i)   ' ANDREW
		   'NumString = left(NumString, i-1) & chr(asc(mid(NumString, i, 1)) + 48) & right(NumString, Len(NumString)- i)
        end if
    Next
    FormatScore = NumString
End function

Function CL(id, NumString)
    Dim Temp, TempStr
    Temp = (dCharsPerLine(id) - Len(NumString) ) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CL = TempStr
End Function

Function RL(id, NumString)
    Dim Temp, TempStr
    Temp = dCharsPerLine(id) - Len(NumString)
    TempStr = Space(Temp) & NumString
    RL = TempStr
End Function


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

' #####################################
' ###### Flashers flupper #####
' #####################################

Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1   		' *** change this, if your table has another name       			***
FlasherLightIntensity = 1		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 1		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherOffBrightness = 0.5		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objlight(20)

'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
'InitFlasher 1, "green" : InitFlasher 2, "red" : InitFlasher 3, "white"
'InitFlasher 4, "green" : InitFlasher 5, "red" : InitFlasher 6, "white"
'InitFlasher 7, "green" : InitFlasher 8, "red"
'InitFlasher 9, "blue" ': InitFlasher 10, "red" : InitFlasher 11, "white" 
' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 4,17 : RotateFlasher 5,0 : RotateFlasher 6,90
'RotateFlasher 7,0 : RotateFlasher 8,0 
'RotateFlasher 9,-45 : RotateFlasher 10,90 : RotateFlasher 11,90

Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 60
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	objbase(nr).BlendDisableLighting = FlasherOffBrightness
	' set the texture and color of all objects
	select case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objlight(nr).intensity = 5000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub

Sub FlashFlasher(nr)
	If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objlit(nr).visible = 1 : End If
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objlit(nr).visible = 0 : End If
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub 
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub 
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub 
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub 
Sub FlasherFlash5_Timer() : FlashFlasher(5) : End Sub 
Sub FlasherFlash6_Timer() : FlashFlasher(6) : End Sub 
Sub FlasherFlash7_Timer() : FlashFlasher(7) : End Sub
Sub FlasherFlash8_Timer() : FlashFlasher(8) : End Sub
Sub FlasherFlash9_Timer() : FlashFlasher(9) : End Sub
Sub FlasherFlash10_Timer() : FlashFlasher(10) : End Sub
Sub FlasherFlash11_Timer() : FlashFlasher(11) : End Sub

' ###################################
' ###### copy script until here #####
' ###################################

' ***      script for demoing flashers					***
' *** you should not need this in your table			***
' *** in your table start a flash with :				***
' *** ObjLevel(xx) = 1 : FlasherFlashxx_Timer			***
' *** for modulated flashers use 0-1 for ObjLevel(xx)	***

'dim countr : Randomize

'Sub Timer1_Timer
'	If TestFlashers = 0 Then
'		countr = countr + 1 : If Countr > 11 then Countr = 3 : end If
'		If rnd(1) < 0.04 Then
'			PlaySound "fx_relay_on",0,1
'			select case countr
				'case 1 : Objlevel(1) = 1 : FlasherFlash1_Timer
				'case 2 : Objlevel(2) = 1 : FlasherFlash2_Timer
				'case 3 : ObjLevel(3) = 1 : FlasherFlash3_Timer
				'case 4 : ObjLevel(4) = 1 : FlasherFlash4_Timer
				'case 5 : ObjLevel(5) = 1 : FlasherFlash5_Timer
				'case 6 : ObjLevel(6) = 1 : FlasherFlash6_Timer
				'case 7 : ObjLevel(7) = 1 : FlasherFlash7_Timer
				'case 8 : ObjLevel(8) = 1 : FlasherFlash8_Timer
'				case 9 : ObjLevel(9) = 1 : FlasherFlash9_Timer
				'case 10 : ObjLevel(10) = 1 : FlasherFlash10_Timer
				'case 11 : ObjLevel(11) = 1 : FlasherFlash11_Timer
'			end Select
'		End If
'	End If
'end Sub

' ********************************
'   Table info & Attract Mode
' ********************************

Sub ShowTableInfo
    Dim ii
    'info goes in a loop only stopped by the credits and the startkey
    If Score(1) Then
'        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 1" &FormatScore(Score(1) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(2) Then
'        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 2 " &FormatScore(Score(2) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(3) Then
'        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 3 " &FormatScore(Score(3) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(4) Then
'        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 4 " &FormatScore(Score(4) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
'     DMD CL(0, "GAME OVER"), CL(1, "TRY AGAIN"), "", eNone, eBlink, eNone, 2000, True, ""
    If bFreePlay Then
'        DMD "", CL(1, "FREE PLAY"), "", eNone, eNone, eNone, 2000, False, ""
    Else
        If Credits> 0 Then
'            DMD CL(0, "CREDITS " & Credits), CL(1, "PRESS START"), "", eNone, eBlink, eNone, 2000, False, ""
        Else
 '           DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
        End If
    End If
	'   Put here your intro DMD

End Sub

Sub StartAttractMode
    'PlayModeMusic
	SwitchMusic "M_End"
    StartLightSeq
'    DMDFlush
    'ShowTableInfo

	debug.print "Attract Mode On"
	'pDMDSetPage pScores

	AttractTimerCount = 0
	AttractTimer.Enabled = 1
	'pDMDsetPage 88
End Sub

Sub StopAttractMode
	debug.print "Attract Mode Off"
	ClearPupAttractMessages
	AttractTimer.Enabled = 0
	loadBG
	'pDMDsetPage pScores
	LightSeqAttract.StopPlay
End Sub

Sub StartLightSeq()
    'lights sequences
    LightSeqAttract.UpdateInterval = 25
    LightSeqAttract.Play SeqBlinking, , 5, 150
    LightSeqAttract.Play SeqRandom, 40, , 4000
    LightSeqAttract.Play SeqAllOff
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 40, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 40, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqRightOn, 30, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqLeftOn, 30, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqStripe1VertOn, 50, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
End Sub

Sub LightSeqAttract_PlayDone()
    StartLightSeq()
End Sub

Sub LightSeqTilt_PlayDone()
    LightSeqTilt.Play SeqAllOff
End Sub



Sub UpdateLights
	
'	debug.print "INSIDE UPDATE LIGHTS"
	'Bill
	'Light106.State = 1		' Main Light

	Light065.State = anBillLights(CurrentPlayer,1)
	Light064.State = anBillLights(CurrentPlayer,2)
	Light063.State = anBillLights(CurrentPlayer,3)
	Light062.State = anBillLights(CurrentPlayer,4)
	Light061.State = anBillLights(CurrentPlayer,5)
	Light060.State = anBillLights(CurrentPlayer,6)
	Light059.State = anBillLights(CurrentPlayer,7)
	Light058.State = anBillLights(CurrentPlayer,8)
	Light057.State = anBillLights(CurrentPlayer,9)

	'Bobby
			' No Main Light
	Light127.State = anBobbyLights(CurrentPlayer,1)
	Light126.State = anBobbyLights(CurrentPlayer,2)
	Light125.State = anBobbyLights(CurrentPlayer,3)
	Light124.State = anBobbyLights(CurrentPlayer,4)
	Light110.State = anBobbyLights(CurrentPlayer,5)
	Light114.State = anBobbyLights(CurrentPlayer,6)
	Light115.State = anBobbyLights(CurrentPlayer,7)
	Light116.State = anBobbyLights(CurrentPlayer,8)
	Light117.State = anBobbyLights(CurrentPlayer,9)
	Light118.State = anBobbyLights(CurrentPlayer,10)
	Light119.State = anBobbyLights(CurrentPlayer,11)
	Light120.State = anBobbyLights(CurrentPlayer,12)
	Light121.State = anBobbyLights(CurrentPlayer,13)
	Light122.State = anBobbyLights(CurrentPlayer,14)
	Light123.State = anBobbyLights(CurrentPlayer,15)

	'Boomhauer
	'Light102.State = 1		' Main Light

	Light019.State = anBoomLights(CurrentPlayer,1)
	Light020.State = anBoomLights(CurrentPlayer,2)
	Light021.State = anBoomLights(CurrentPlayer,3)
	Light022.State = anBoomLights(CurrentPlayer,4)
	Light023.State = anBoomLights(CurrentPlayer,5)
	Light024.State = anBoomLights(CurrentPlayer,6)
	Light025.State = anBoomLights(CurrentPlayer,7)
	Light026.State = anBoomLights(CurrentPlayer,8)
	Light027.State = anBoomLights(CurrentPlayer,9)

	'Cotton
	'Light107.State = 1		' Main Light

	Light072.State = anCottonLights(CurrentPlayer,1)
	Light071.State = anCottonLights(CurrentPlayer,2)
	Light070.State = anCottonLights(CurrentPlayer,3)
	Light069.State = anCottonLights(CurrentPlayer,4)
	Light068.State = anCottonLights(CurrentPlayer,5)
	Light067.State = anCottonLights(CurrentPlayer,6)

	'Dale
	if bDaleReady Then
		Light112.State = 2		' Main Light -- light when modes are ready
	Else
		Light112.State = 0		' Main Light -- light when modes are ready
	End If

	Select Case anDaleLights1(CurrentPlayer)
		Case 1
			Light134.State = 1
		Case 2
			Light134.State = 1
			Light135.State = 1
		Case 3
			Light134.State = 1
			Light135.State = 1
			Light136.State = 1
			Light095.State = 1
		Case Else
			Light134.State = 0
			Light135.State = 0
			Light136.State = 0
			Light095.State = 0
	End Select

	Select Case anDaleLights2(CurrentPlayer)
		Case 1
			Light137.State = 1
		Case 2
			Light137.State = 1
			Light140.State = 1
		Case 3
			Light137.State = 1
			Light140.State = 1
			Light138.State = 1
			Light096.State = 1
		Case Else
			Light137.State = 0
			Light140.State = 0
			Light138.State = 0
			Light096.State = 0
	End Select

	Select Case anDaleLights3(CurrentPlayer)
		Case 1
			Light141.State = 1
		Case 2
			Light141.State = 1
			Light142.State = 1
		Case 3
			Light141.State = 1
			Light142.State = 1
			Light139.State = 1
			Light097.State = 1
		Case Else
			Light141.State = 0
			Light142.State = 0
			Light139.State = 0
			Light097.State = 0
	End Select



	Light028.State = anBeerLights(CurrentPlayer,1)
	Light029.State = anBeerLights(CurrentPlayer,2)
	Light030.State = anBeerLights(CurrentPlayer,3)
	Light031.State = anBeerLights(CurrentPlayer,4)
	Light032.State = anBeerLights(CurrentPlayer,5)

	'BEER
	Light001.State = Mode(CurrentPlayer,13)
	Light002.State = Mode(CurrentPlayer,14)
	Light003.State = Mode(CurrentPlayer,15)


	'Hank
	Light006.State = Mode(CurrentPlayer,7)
	Light005.State = Mode(CurrentPlayer,8)
	Light004.State = Mode(CurrentPlayer,9)


	Light033.State = anHankLights(CurrentPlayer,1)
	Light034.State = anHankLights(CurrentPlayer,2)
	Light035.State = anHankLights(CurrentPlayer,3)
	Light036.State = anHankLights(CurrentPlayer,4)
	Light037.State = anHankLights(CurrentPlayer,5)

	'Peggy
			' No Main Light
	Light101.State = Mode(CurrentPlayer,1)
	Light128.State = Mode(CurrentPlayer,2)
	Light129.State = Mode(CurrentPlayer,3)

	Light098.State = anPeggyLights(CurrentPlayer,1)
	Light099.State = anPeggyLights(CurrentPlayer,2)
	Light100.State = anPeggyLights(CurrentPlayer,3)

	'Luanne
	'Light108.State = 1		' Main Light

	Light047.State = anLuanneLights(CurrentPlayer,1)
	Light043.State = anLuanneLights(CurrentPlayer,2)
	Light044.State = anLuanneLights(CurrentPlayer,3)
	Light045.State = anLuanneLights(CurrentPlayer,4)
	Light046.State = anLuanneLights(CurrentPlayer,5)
	Light048.State = anLuanneLights(CurrentPlayer,6)

	'YEP
	if nYepReady = 1 Then 
		Light113.State = 2		' Main Light
		' Start light sequence of YEP lights ?
	Else
		Light113.State = 0		' Main Light
	End If

	if nYepReady = 2 Then
		Light007.State = 2
	Else
		Light007.State = 0
	End If
	
	Light131.State = anYEP(CurrentPlayer,1)
	Light133.State = anYEP(CurrentPlayer,2)
	Light132.State = anYEP(CurrentPlayer,3)

	'Drink Alamo
			' No Main Light
	Light094.State = anDrinkAlamo(CurrentPlayer,1)
	Light093.State = anDrinkAlamo(CurrentPlayer,2)
	Light092.State = anDrinkAlamo(CurrentPlayer,3)
	Light091.State = anDrinkAlamo(CurrentPlayer,4)
	Light090.State = anDrinkAlamo(CurrentPlayer,5)
	Light089.State = anDrinkAlamo(CurrentPlayer,6)
	Light088.State = anDrinkAlamo(CurrentPlayer,7)
	Light087.State = anDrinkAlamo(CurrentPlayer,8)
	Light086.State = anDrinkAlamo(CurrentPlayer,9)
	Light085.State = anDrinkAlamo(CurrentPlayer,10)


	'LadyBird
	'Light103.State = 1		' Main Light

	Light018.State = anLadybird(CurrentPlayer,1)
	Light017.State = anLadybird(CurrentPlayer,2)
	Light016.State = anLadybird(CurrentPlayer,3)
	Light015.State = anLadybird(CurrentPlayer,4)
	Light014.State = anLadybird(CurrentPlayer,5)
	Light013.State = anLadybird(CurrentPlayer,6)
	Light012.State = anLadybird(CurrentPlayer,7)
	Light011.State = anLadybird(CurrentPlayer,8)

	'Lawn Mower
	'Light104.State = 1		' Main Light
	
	Light077.State = anLawnMower(CurrentPlayer,1)
	Light076.State = anLawnMower(CurrentPlayer,2)
	Light075.State = anLawnMower(CurrentPlayer,3)
	Light074.State = anLawnMower(CurrentPlayer,4)
	Light073.State = anLawnMower(CurrentPlayer,5)

	'Propane
	'Light105.State = 1		' Main Light

	Light078.State = anPropane(CurrentPlayer,1)
	Light079.State = anPropane(CurrentPlayer,2)
	Light080.State = anPropane(CurrentPlayer,3)
	Light081.State = anPropane(CurrentPlayer,4)
	Light082.State = anPropane(CurrentPlayer,5)
	Light083.State = anPropane(CurrentPlayer,6)
	Light084.State = anPropane(CurrentPlayer,7)

	'Collect Multiplier area
	'Light109.State = 1		' Main Light

	'BBQ Feast
	Light038.State = anBBQFeast(CurrentPlayer,1)
	Light039.State = anBBQFeast(CurrentPlayer,2)
	Light040.State = anBBQFeast(CurrentPlayer,3)
	Light041.State = anBBQFeast(CurrentPlayer,4)
	Light042.State = anBBQFeast(CurrentPlayer,5)


End Sub

Sub UpdateArrows_Timer()
'	if Mode(CurrentPlayer,0) <> 0 Then
'		StopAllModeSeq
'	Else
'		Exit Sub
'	End If
'	UpdateLights

	if Mode(CurrentPlayer,0) <> 0 Then Exit Sub
	UpdateLights
End Sub

Sub TurnOffModeLights
	Dim TL
	For each TL in aModeLights
		TL.State = 0
	Next
End Sub

Sub StopModeRampSeq
	if Mode(CurrentPlayer,8) = 2 Then
		bHankMode2Orbit = True
		bHankMode2Ramp = False
	End If
	LightSeqBeer2.StopPlay
	LightSeqHank2.StopPlay
	LightSeqLawnMower.StopPlay
	LightSeqBBQ2.StopPlay
	LightSeqPropane.StopPlay

End Sub

Sub StopModeOrbitSeq
	if Mode(CurrentPlayer,8) = 2 Then
		bHankMode2Orbit = False
		bHankMode2Ramp = True
	End If
	LightSeqBoom.StopPlay
	LightSeqLadyBird.StopPlay
	LightSeqBill.StopPlay
	LightSeqCotton.StopPlay
	LightSeqLuanne.StopPlay
End Sub

Sub StopAllModeSeq
	LightSeqBill.StopPlay
	LightSeqBobby.StopPlay
	LightSeqBoom.StopPlay
	LightSeqBeer.StopPlay
	LightSeqBeer2.StopPlay
	LightSeqBBQ.StopPlay
	LightSeqBBQ2.StopPlay
	LightSeqCotton.StopPlay
	LightSeqDrinkAlamo.StopPlay
	'LightSeqDale.StopPlay
	LightSeqHank.StopPlay
	LightSeqHank2.StopPlay
	LightSeqLadyBird.StopPlay
	LightSeqLawnMower.StopPlay
	LightSeqLuanne.StopPlay
	LightSeqPeggy.StopPlay
	LightSeqPropane.StopPlay
	LightSeqYEP.StopPlay
End Sub

'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' droptargets, animations, etc
Sub VPObjects_Init
End Sub

' tables variables and Mode init
Dim HoleBonus, BumperBonus, ALLRampBonus, RampBonus1, RampBonus2, RampBonus3, MulitballBonus, TargetBonus    

Sub Game_Init() 'called at the start of a new game
    Dim i, j
	Overhead_GIon
	PlayModeMusic
	vpmtimer.addtimer 200, "DuckMusic '"
	TargetBonus = 0
	'bumperHits = 100
	BumperBonus = 0
	ALLRampBonus = 0
	RampBonus1 = 0
	RampBonus2 = 0
	RampBonus3 =0
	MulitballBonus = 0
	'BallInHole = 0
    TurnOffPlayfieldLights()
	

		debug.print "Starting Game"

	if ScorbitActive = 1 And (Scorbit.bNeedsPairing) = False Then 
		debug.print "Starting Scorbit"
		Scorbit.StartSession()
		GameModeStrTmp="BL{Red}: Starting Game"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
	bOnTheFirstBallScorbit = True
	bSupressModeMessages = False

	DOF 290, DOFOff  ' call to clear gameover DOF if its running
	

End Sub

Sub StopEndOfBallMode()     'this sub is called after the last ball is drained
End Sub

Sub SupressModeMessages(mTime)
	bSupressModeMessages = True
	DMDQueue.Add "bSupressModeMessages = False","bSupressModeMessages = False",1,mTime,0,0,0,False
End Sub

Sub ResetEOBValues
	RampCount = 0
	TargetCount = 0
	OrbitCount = 0
	BumperCount = 0
	BonusPoints(CurrentPlayer) = 0
End Sub

Sub ResetNewBallVariables() 'reset variables for a new ball or player
Dim i
	TargetBonus = 0
	bBallSaverReady = True
	Mode(CurrentPlayer,0) = 0
	DMDUpdateBallNumber Balls
	DisplayAllProgress
	DMDUpdatePlayerName
	ForceSetLawnMower
	ResetEOBValues
End Sub

Sub ResetNewBallLights()    'turn on or off the needed lights before a new ball is released
    'TurnOffPlayfieldLights
    'li025.State = 1
    'li021.State = 1
    'li022.State = 1
    'li023.State = 1
    'li024.State = 1
	'li033.state = 1
	if Mode(CurrentPlayer,3) <> 1 Then Light143.State = 0
	if Mode(CurrentPlayer,4) <> 1 Then Light144.State = 0
	if Mode(CurrentPlayer,5) <> 1 Then Light145.State = 0
	if Mode(CurrentPlayer,6) <> 1 Then Light146.State = 0

	if Mode(CurrentPlayer,9) <> 1 Then Light147.State = 0
	if Mode(CurrentPlayer,10) <> 1 Then Light148.State = 0
	if Mode(CurrentPlayer,11) <> 1 Then Light149.State = 0
	if Mode(CurrentPlayer,12) <> 1 Then Light150.State = 0
	
	gi1.state = 1
	gi2.state = 1
	gi3.state = 1
	gi4.state = 1
	'LoadModeLights
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub



'******************************
'   Light Helper Subs
'******************************


' *********************************************************************
'                        Table Object Hit Events
'
' Any target hit Sub should do something like this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/Mode this trigger is a member of
' - set the "LastSwitchHit" variable in case it is needed later
' *********************************************************************

'************
' Slingshots
'************

Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
'    If li001.State=1 then 
'	AddScore 210
'	end if
'	If li002.State=1 then 
'	AddScore 420
	addscore SCORE_SLINGSHOT
'	end if
	'PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
	DOF 104,DOFPulse
	RandomSoundSlingshotRight Sling1
    RSling.Visible = 0:RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	gi1.State = 0
	Gi2.State = 0	
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10:gi1.State = 0:Gi2.State = 0
		Case 1:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 5:gi1.State = 0:Gi2.State = 0
        Case 2:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:gi1.State = 1:Gi2.State = 1:RightSlingShot.TimerEnabled = False
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
'	If li001.State=1 then 
'	AddScore 210
'	end if
'	If li002.State=1 then 
'	AddScore 420
	addscore SCORE_SLINGSHOT
'	end if
    'PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
	DOF 103,DOFPulse
	RandomSoundSlingshotLeft Sling2
    LSling.Visible = 0:LSling1.Visible = 1
    sling2.rotx = 20
	 LStep = 0
    LeftSlingShot.TimerEnabled = 1
	gi3.State = 0
	Gi4.State = 0
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10:gi3.State = 0:Gi4.State = 0
        Case 2:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 5:gi3.State = 0:Gi4.State = 0
        Case 3:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:gi3.State = 1:Gi4.State = 1:LeftSlingShot.TimerEnabled = False
    End Select
    LStep = LStep + 1
End Sub





'*****************
'triggers
'*****************



'************************** 
'Bumpers 
'************************** 


Sub Bumper001_hit()
	Dim tmpHC
	'PlaySoundAt"fx_bumper1", Bumper001
	DOF 105, DOFPulse
	RandomSoundBumperMiddle Bumper001
	FlBumperFadeTarget(1) = 1		'Flupper bumper demo
	Bumper001.timerenabled = True
	'ObjLevel(9) = 1 : FlasherFlash9_Timer
	BumperCount = BumperCount + 1
	'addscore SCORE_BUMPER

	'bumperHitCount(CurrentPlayer) = bumperHitCount(CurrentPlayer) + 1

	Select Case Mode(CurrentPlayer,0)
		Case 0
			anBobbyLights(CurrentPlayer,0) = anBobbyLights(CurrentPlayer,0) + 1
			if anBobbyLights(CurrentPlayer,0) > 15 Then anBobbyLights(CurrentPlayer,0) = 15 : bBobbyReady = True : StartBobby
			'if bumperHitCount(CurrentPlayer) > 15 Then bumperHitCount(CurrentPlayer) = 15
			tmpHC = anBobbyLights(CurrentPlayer,0)
			anBobbyLights(CurrentPlayer,tmpHC) = 1
			DisplayBobbyProgress
		Case 1

		Case 3
			if Mode(CurrentPlayer,3) = 2 Then
				if nPeggyMode3Progress(1) = 0 Then
					nPeggyMode3Progress(1) = 1
					addscore SCORE_MODE_PROGRESS
					LightSeqBobby.StopPlay()
					CheckPeggy3Progress
				End If
			End If

		Case 12

	End Select

End sub

Sub Bumper001_timer
	FlBumperFadeTarget(1) = 0
End Sub

Sub CheckBumpers()
    If bumperHits <= 0 Then
        BumperBonus = BumperBonus + 1
        bumperHits = 100
    ' do something more
    End If
End Sub

Sub ResetBumpers()
    bumperHits = 100
End Sub

' #####################################
' ###### copy script from here on #####
' #####################################

' prepare some global vars to dim/brighten objects when using day-night slider
Dim DayNightAdjust , DNA30, DNA45, DNA90
If NightDay < 10 Then
	DNA30 = 0 : DNA45 = (NightDay-10)/20 : DNA90 = 0 : DayNightAdjust = 0.4
Else
	DNA30 = (NightDay-10)/30 : DNA45 = (NightDay-10)/45 : DNA90 = (NightDay-10)/90 : DayNightAdjust = NightDay/25
End If

Dim FlBumperFadeActual(6), FlBumperFadeTarget(6), FlBumperColor(6), FlBumperTop(6), FlBumperSmallLight(6), Flbumperbiglight(6)
Dim FlBumperDisk(6), FlBumperBase(6), FlBumperBulb(6), FlBumperscrews(6), FlBumperActive(6), FlBumperHighlight(6)
Dim cnt : For cnt = 1 to 6 : FlBumperActive(cnt) = False : Next

' colors available are red, white, blue, orange, yellow, green, purple and blacklight

FlInitBumper 1, "red"

' ### uncomment the statement below to change the color for all bumpers ###
' Dim ind : For ind = 1 to 5 : FlInitBumper ind, "green" : next

Sub FlInitBumper(nr, col)
	FlBumperActive(nr) = True
	' store all objects in an array for use in FlFadeBumper subroutine
	FlBumperFadeActual(nr) = 1 : FlBumperFadeTarget(nr) = 1.1: FlBumperColor(nr) = col
	Set FlBumperTop(nr) = Eval("bumpertop" & nr) : FlBumperTop(nr).material = "bumpertopmat" & nr
	Set FlBumperSmallLight(nr) = Eval("bumpersmalllight" & nr) : Set Flbumperbiglight(nr) = Eval("bumperbiglight" & nr)
	Set FlBumperDisk(nr) = Eval("bumperdisk" & nr) : Set FlBumperBase(nr) = Eval("bumperbase" & nr)
	Set FlBumperBulb(nr) = Eval("bumperbulb" & nr) : FlBumperBulb(nr).material = "bumperbulbmat" & nr
	Set FlBumperscrews(nr) = Eval("bumperscrews" & nr): FlBumperscrews(nr).material = "bumperscrew" & col
	Set FlBumperHighlight(nr) = Eval("bumperhighlight" & nr)
	' set the color for the two VPX lights
	select case col
		Case "red"
			FlBumperSmallLight(nr).color = RGB(255,4,0) : FlBumperSmallLight(nr).colorfull = RGB(255,24,0)
			FlBumperBigLight(nr).color = RGB(255,32,0) : FlBumperBigLight(nr).colorfull = RGB(255,32,0)
			FlBumperHighlight(nr).color = RGB(64,255,0)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.98
			FlBumperSmallLight(nr).TransmissionScale = 0
		Case "blue"
			FlBumperBigLight(nr).color = RGB(32,80,255) : FlBumperBigLight(nr).colorfull = RGB(32,80,255)
			FlBumperSmallLight(nr).color = RGB(0,80,255) : FlBumperSmallLight(nr).colorfull = RGB(0,80,255)
			FlBumperSmallLight(nr).TransmissionScale = 0 : MaterialColor "bumpertopmat" & nr, RGB(8,120,255)
			FlBumperHighlight(nr).color = RGB(255,16,8)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "green"
			FlBumperSmallLight(nr).color = RGB(8,255,8) : FlBumperSmallLight(nr).colorfull = RGB(8,255,8)
			FlBumperBigLight(nr).color = RGB(32,255,32) : FlBumperBigLight(nr).colorfull = RGB(32,255,32)
			FlBumperHighlight(nr).color = RGB(255,32,255) : MaterialColor "bumpertopmat" & nr, RGB(16,255,16) 
			FlBumperSmallLight(nr).TransmissionScale = 0.005
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "orange"
			FlBumperHighlight(nr).color = RGB(255,130,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).color = RGB(255,130,0) : FlBumperSmallLight(nr).colorfull = RGB (255,90,0)
			FlBumperBigLight(nr).color = RGB(255,190,8) : FlBumperBigLight(nr).colorfull = RGB(255,190,8)
		Case "white"
			FlBumperBigLight(nr).color = RGB(255,230,190) : FlBumperBigLight(nr).colorfull = RGB(255,230,190)
			FlBumperHighlight(nr).color = RGB(255,180,100) : 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.99
		Case "blacklight"
			FlBumperBigLight(nr).color = RGB(32,32,255) : FlBumperBigLight(nr).colorfull = RGB(32,32,255)
			FlBumperHighlight(nr).color = RGB(48,8,255) : 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "yellow"
			FlBumperSmallLight(nr).color = RGB(255,230,4) : FlBumperSmallLight(nr).colorfull = RGB(255,230,4)
			FlBumperBigLight(nr).color = RGB(255,240,50) : FlBumperBigLight(nr).colorfull = RGB(255,240,50)
			FlBumperHighlight(nr).color = RGB(255,255,220)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1 
			FlBumperSmallLight(nr).TransmissionScale = 0
		Case "purple"
			FlBumperBigLight(nr).color = RGB(80,32,255) : FlBumperBigLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).color = RGB(80,32,255) : FlBumperSmallLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).TransmissionScale = 0 : 
			FlBumperHighlight(nr).color = RGB(32,64,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
	end select
End Sub

Sub FlFadeBumper(nr, Z)
	FlBumperBase(nr).BlendDisableLighting = 0.5 * DayNightAdjust
'	UpdateMaterial(string, float wrapLighting, float roughness, float glossyImageLerp, float thickness, float edge, float edgeAlpha, float opacity,
'               OLE_COLOR base, OLE_COLOR glossy, OLE_COLOR clearcoat, VARIANT_BOOL isMetal, VARIANT_BOOL opacityActive,
'               float elasticity, float elasticityFalloff, float friction, float scatterAngle) - updates all parameters of a material
	FlBumperDisk(nr).BlendDisableLighting = (0.5 - Z * 0.3 )* DayNightAdjust	

	select case FlBumperColor(nr)

		Case "blue" :
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(38-24*Z,130 - 98*Z,255), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20  + 500 * Z / (0.5 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 5000 * (0.03 * Z +0.97 * Z^3)
			Flbumperbiglight(nr).intensity = 45 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 10000 * (Z^3) / (0.5 + DNA90)

		Case "green"	
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(16 + 16 * sin(Z*3.14),255,16 + 16 * sin(Z*3.14)), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 10 + 150 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 2 * DayNightAdjust + 20 * Z
			FlBumperBulb(nr).BlendDisableLighting = 7 * DayNightAdjust + 6000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 6000 * (Z^3) / (1 + DNA90)
		
		Case "red" 
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 16 - 11*Z + 16 * sin(Z*3.14),0), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 100 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 18 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 20 * DayNightAdjust + 9000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,20 + Z*4,8-Z*8)
		
		Case "orange"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 100 - 22*z  + 16 * sin(Z*3.14),Z*32), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 250 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 2500 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 4000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,100 + Z*50, 0)

		Case "white"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255,230 - 100 * Z, 200 - 150 * Z), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 180 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 5 * DayNightAdjust + 30 * Z
			FlBumperBulb(nr).BlendDisableLighting = 18 * DayNightAdjust + 3000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 14 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z^3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255,255 - 20*Z,255-65*Z) : FlBumperSmallLight(nr).colorfull = RGB(255,255 - 20*Z,255-65*Z)
			MaterialColor "bumpertopmat" & nr, RGB(255,235 - z*36,220 - Z*90)

		Case "blacklight"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 1, RGB(30-27*Z^0.03,30-28*Z^0.01, 255), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 900 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 60 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 30000 * Z^3
			Flbumperbiglight(nr).intensity = 40 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z^3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255-240*(Z^0.1),255 - 240*(Z^0.1),255) : FlBumperSmallLight(nr).colorfull = RGB(255-200*z,255 - 200*Z,255)
			MaterialColor "bumpertopmat" & nr, RGB(255-190*Z,235 - z*180,220 + 35*Z)

		Case "yellow"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 180 + 40*z, 48* Z), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 200 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 40 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 2000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,200, 24 - 24 * z)

		Case "purple" :
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(128-118*Z - 32 * sin(Z*3.14), 32-26*Z ,255), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 15  + 200 * Z / (0.5 + DNA30) 
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 10000 * (0.03 * Z +0.97 * Z^3)
			Flbumperbiglight(nr).intensity = 50 * Z / (1 + DNA45) 
			FlBumperHighlight(nr).opacity = 4000 * (Z^3) / (0.5 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(128-60*Z,32,255)


	end select
End Sub

Sub BumperTimer_Timer
	dim nr
	For nr = 1 to 6
		If FlBumperFadeActual(nr) < FlBumperFadeTarget(nr) and FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.8
			If FlBumperFadeActual(nr) > 0.99 Then FlBumperFadeActual(nr) = 1 : End If
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
		If FlBumperFadeActual(nr) > FlBumperFadeTarget(nr) and FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.4 / (FlBumperFadeActual(nr) + 0.1)
			If FlBumperFadeActual(nr) < 0.01 Then FlBumperFadeActual(nr) = 0 : End If
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
	next
End Sub

'' ###################################
'' ###### copy script until here #####
'' ###################################


'*****************
'Targets
'*****************

'*****************
'Gates
'*****************
sub Gate_Hit()
'ObjLevel(1) = 1 : FlasherFlash9_Timer
End Sub
'*****************
'Kickers
'*****************

Sub Kicker001_Hit()
	'PlaySoundAt "fx_kicker_enter",Kicker001
	SoundSaucerLock
	'DOF 118, DOFPulse
    Kicker001.TimerInterval = 1000
	Kicker001.TimerEnabled = True

	If Tilted Then Exit Sub

	if Mode(CurrentPlayer,4) = 2 Then addscore SCORE_SUPER_JACKPOT : Exit Sub' Luanne Mode

	if bPeggyReady And Mode(CurrentPlayer,0) = 0 Then
		Light130.State = 0 ' Turn off mode light
		bPeggyReady = False
		PuPlayer.LabelSet pDMD, "PeggyProgress", "ProgressBars\\peg4.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"

			if Mode(CurrentPlayer,3) <> 1 Then
				Mode(CurrentPlayer,3) = 2
				StartPeggy3' swapped as first mission is too hard
			Elseif Mode(CurrentPlayer,2) <> 1 Then
				Mode(CurrentPlayer,2) = 2
				StartPeggy2
			Elseif Mode(CurrentPlayer,1) <> 1 Then
				Mode(CurrentPlayer,1) = 2
				StartPeggy1' swapped as first mission is too hard
			End If

	End If

	if Mode(CurrentPlayer,7) = 2 Then 
		nTimerCount = nTimerCount + 20
		nHankMode1Progress = nHankMode1Progress + 1
		CheckHankMode1Progress
	End If

End Sub

Sub Kicker001_Timer()
	Kicker001.Kick 150, 10
	DOF 118, DOFPulse
	'PlaySoundAt "fx_kicker", Kicker001
	SoundSaucerKick 1, Kicker001
	'PlaySoundAt "Dash2", TLoop
	Kicker001.TimerEnabled = False
End Sub
'*****************
'Spinners
'*****************

Sub Spinner001_Spin
    DOF 116, DOFPulse
    SoundSpinner Spinner001
	if Mode(CurrentPlayer,17) = 2 Then 
		addscore SCORE_SPINNER*10	' 10x multiplier for Ladybird mode  
	Else
		addscore SCORE_SPINNER
	End If

	 if mode(currentplayer,13) = 2 Then nBeerMode1Progress = nBeerMode1Progress + 1 :  CheckBeerMode1

	LastTriggerHit = "Spinner001"
End Sub 

Sub Spinner002_Spin
    DOF 117, DOFPulse
    SoundSpinner Spinner002
	addscore SCORE_SPINNER

	if mode(currentplayer,13) = 2 Then nBeerMode1Progress = nBeerMode1Progress + 1 :  CheckBeerMode1
End Sub 

'Sub Spinner003_Spin
'    PlaySoundAt "fx_spinner", Spinner002
'End Sub 

Sub footballSpinner_Spin
    PlaySoundAt "footballspin", FootballSpinner
DOF 119, DOFPulse

End Sub 

Sub propanegate_Hit()
    PlaySoundAt "gasleak", propanegate
End Sub 

Sub propaneSpinner_Spin
DOF 115, DOFPulse
End Sub

'******************************************************
' 	ZFLE:  FLEEP MECHANICAL SOUNDS
'******************************************************

' This part in the script is an entire block that is dedicated to the physics sound system.
' Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for the TOM table

' Many of the sounds in this package can be added by creating collections and adding the appropriate objects to those collections.
' Create the following new collections:
'	 Metals (all metal objects, metal walls, metal posts, metal wire guides)
'	 Apron (the apron walls and plunger wall)
'	 Walls (all wood or plastic walls)
'	 Rollovers (wire rollover triggers, star triggers, or button triggers)
'	 Targets (standup or drop targets, these are hit sounds only ... you will want to add separate dropping sounds for drop targets)
'	 Gates (plate gates)
'	 GatesWire (wire gates)
'	 Rubbers (all rubbers including posts, sleeves, pegs, and bands)
' When creating the collections, make sure "Fire events for this collection" is checked.
' You'll also need to make sure "Has Hit Event" is checked for each object placed in these collections (not necessary for gates and triggers).
' Once the collections and objects are added, the save, close, and restart VPX.
'
' Many places in the script need to be modified to include the correct sound effect subroutine calls. The tutorial videos linked below demonstrate
' how to make these updates. But in summary the following needs to be updated:
'	- Nudging, plunger, coin-in, start button sounds will be added to the keydown and keyup subs.
'	- Flipper sounds in the flipper solenoid subs. Flipper collision sounds in the flipper collide subs.
'	- Bumpers, slingshots, drain, ball release, knocker, spinner, and saucers in their respective subs
'	- Ball rolling sounds sub
'
' Tutorial vides by Apophis
' Audio : Adding Fleep Part 1				https://youtu.be/rG35JVHxtx4
' Audio : Adding Fleep Part 2				https://youtu.be/dk110pWMxGo
' Audio : Adding Fleep Part 3				https://youtu.be/ESXWGJZY_EI


'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1					  'volume level; range [0, 1]
NudgeLeftSoundLevel = 1				 'volume level; range [0, 1]
NudgeRightSoundLevel = 1				'volume level; range [0, 1]
NudgeCenterSoundLevel = 1			   'volume level; range [0, 1]
StartButtonSoundLevel = 0.1			 'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr   'volume level; range [0, 1]
PlungerPullSoundLevel = 1			   'volume level; range [0, 1]
RollingSoundFactor = 1.1 / 5

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010		'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635		'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0					   'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45					'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel		'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel	   'sound helper; not configurable
SlingshotSoundLevel = 0.95					  'volume level; range [0, 1]
BumperSoundFactor = 4.25						'volume multiplier; must not be zero
KnockerSoundLevel = 1						   'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2		  'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055 / 5			 'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075 / 5			   'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075 / 5			'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025		   'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025		   'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8	  'volume level; range [0, 1]
WallImpactSoundFactor = 0.075				   'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075 / 3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5 / 5			'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10	 'volume multiplier; must not be zero
DTSoundLevel = 0.25				 'volume multiplier; must not be zero
RolloverSoundLevel = 0.25		   'volume level; range [0, 1]
SpinnerSoundLevel = 0.5			 'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor

DrainSoundLevel = 0.8				   'volume level; range [0, 1]
BallReleaseSoundLevel = 1			   'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2	'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015	 'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025 / 5			 'volume multiplier; must not be zero

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
	PlaySound playsoundparams, - 1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
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
	PlaySound playsoundparams, - 1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
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
	PlaySound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	PlaySound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
	PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.y * 2 / tableheight - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioFade = CSng(tmp ^ 10)
	Else
		AudioFade = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.x * 2 / tablewidth - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioPan = CSng(tmp ^ 10)
	Else
		AudioPan = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Vol = CSng(BallVel(ball) ^ 2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = CSng((ball.velz) ^ 2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
	BallVel = Int(Sqr((ball.VelX ^ 2) + (ball.VelY ^ 2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * CSng(BallVel(ball) ^ 3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	PitchPlayfieldRoll = BallVel(ball) ^ 2 * 15
End Function

Function RndInt(min, max) ' Sets a random number integer between min and max
	RndInt = Int(Rnd() * (max - min + 1) + min)
End Function

Function RndNum(min, max) ' Sets a random number between min and max
	RndNum = Rnd() * (max - min) + min
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////

Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeLeftSoundLevel * VolumeDial, - 0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
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
	PlaySoundAtLevelStatic ("Drain_" & Int(Rnd * 11) + 1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
	PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd * 7) + 1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundSlingshotLeft(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd * 10) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd * 8) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

'/////////////////////////////  SPINNER SOUNDS  ////////////////////////////

Sub SoundSpinner(spinnerswitch)
	PlaySoundAtLevelStatic ("fx_Spinner"), SpinnerSoundLevel, spinnerswitch
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
	PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd * 7) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd * 8) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm / 10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm / 10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd * 7) + 1), parm * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////

Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd * 4) + 1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////

Sub Rubbers_Hit(idx)
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 5 Then
		RandomSoundRubberStrong 1
	End If
	If finalspeed <= 5 Then
		RandomSoundRubberWeak()
	End If
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////

Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd * 10) + 1
		Case 1
			PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 2
			PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 3
			PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 4
			PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 5
			PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 6
			PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 7
			PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 8
			PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 9
			PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 10
			PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6 * voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////

Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd * 9) + 1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////

Sub Walls_Hit(idx)
	RandomSoundWall()
End Sub

Sub RandomSoundWall()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 5) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 4) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 3) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////

Sub RandomSoundMetal()
	PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd * 13) + 1), Vol(ActiveBall) * MetalImpactSoundFactor
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
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Bounce_" & Int(Rnd * 2) + 1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////

Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd * 3) + 1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
	If Abs(cor.ballvelx(ActiveBall.id) < 4) And cor.ballvely(ActiveBall.id) > 7 Then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////

Sub RandomSoundFlipperBallGuide()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd * 3) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd * 7) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////

Sub RandomSoundTargetHitStrong()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 10 Then
		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft ActiveBall
	Else
		RandomSoundTargetHitWeak()
	End If
End Sub

Sub Targets_Hit (idx)
	PlayTargetSound
	'addscore SCORE_TARGET
	TargetCount = TargetCount + 1
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////

Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd * 9) + 1
		Case 1
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd * 7) + 1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////

Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd * 5) + 1
		Case 1
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd * 2) + 1), GateSoundLevel, ActiveBall
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, ActiveBall
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
	DOF 122, DOFPulse
End Sub

Sub GatesWire_hit(idx)
	SoundPlayfieldGate
	DOF 123, DOFPulse
End Sub

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub Arch1_hit()
	If ActiveBall.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If ActiveBall.velx <  - 8 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_hit()
	If ActiveBall.velx < 1 Then SoundPlayfieldGate
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
	If ActiveBall.velx > 10 Then
		RandomSoundLeftArch
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd * 2) + 1), SaucerLockSoundLevel, ActiveBall
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0
			PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1
			PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////

Sub OnBallBallCollision(ball1, ball2, velocity)
	Dim snd
	Select Case Int(Rnd * 7) + 1
		Case 1
			snd = "Ball_Collide_1"
		Case 2
			snd = "Ball_Collide_2"
		Case 3
			snd = "Ball_Collide_3"
		Case 4
			snd = "Ball_Collide_4"
		Case 5
			snd = "Ball_Collide_5"
		Case 6
			snd = "Ball_Collide_6"
		Case 7
			snd = "Ball_Collide_7"
	End Select
	
	PlaySound (snd), 0, CSng(velocity) ^ 2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
	PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd * 6) + 1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
	PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd * 6) + 1), 200, obj
End Sub

'/////////////////////////////  GI AND FLASHER RELAYS  ////////////////////////////

Const RelayFlashSoundLevel = 0.315  'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05	  'volume level; range [0, 1];

Sub Sound_GI_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_GI_On"), 0.025 * RelayGISoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_GI_Off"), 0.025 * RelayGISoundLevel, obj
	End Select
End Sub

Sub Sound_Flash_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_Flash_On"), 0.025 * RelayFlashSoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_Flash_Off"), 0.025 * RelayFlashSoundLevel, obj
	End Select
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'****  FLEEP MECHANICAL SOUNDS
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

'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************

Sub swplunger_Hit()
	BIPL = True
	debug.print"BIPL True"
End Sub

Sub swplunger_UnHit
	'BIPL = False
End Sub

Sub RampTrigger001_Hit() ' Beer Ramp on Left
	WireRampOn True	 'Play Plastic Ramp Sound
	WobbleBeerCan.Enabled = 1
End Sub

Sub RampTrigger002_Hit()	' Main Center Ramp
	WireRampOn True	 'Play Plastic Ramp Sound
	debug.print "HANK Ramp 1"
End Sub

Sub RampTrigger003_Hit()	' propane Ramp
	WireRampOn True	 'Play Plastic Ramp Sound
End Sub

Sub RampTrigger004_Hit() ' Ramp entry to grill on Right
	WireRampOn True	 'Play Plastic Ramp Sound
End Sub



Sub RampTrigger002a_Hit()
	'WireRampOff	 'Turn off the Plastic Ramp Sound
End Sub



Sub RampTrigger003c_Hit()
	WireRampOn False	'On Wire Ramp, Play Wire Ramp Sound
End Sub

Sub RampTrigger003d_Hit()
	WireRampOff	 'Turn off the Plastic Ramp Sound
End Sub

Sub RampTrigger004a_Hit()
	WireRampOff	 'Turn off the Plastic Ramp Sound
End Sub

Sub RampTrigger005_Hit() ' Beer Ramp on Left
	WireRampOn True 'Play Plastic Ramp Sound
End Sub


Sub RampTrigger001b_Hit()
	WireRampOff	 'Turn off the Plastic Ramp Sound
End Sub

Sub RampTrigger002b_Hit()
	WireRampOff	 'Turn off the Plastic Ramp Sound

	' StartMode & Reset Hank Lights
End Sub



Sub RampTrigger003b_Hit()
	WireRampOff	 'Turn off the Plastic Ramp Sound

	if anLawnMower(CurrentPlayer,0) > 4 And bLockengaged = False Then DisplayLockMessage

	if anLawnMower(CurrentPlayer,0) > 4 And bMowerNotStarted Then bLockEngaged = True
	bReleasingBalls = False
End Sub

Sub DisplayLockMessage
	if bMowerNotStarted = False Then Exit Sub ' exit if in tracot rMball
	pDMDSplashTwoLines " BALL LOCK ", " READY ", 4, cGreen
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,4100,0,0,0,True
End Sub



Sub RampTrigger001a_Unhit()
	WireRampOn False	'On Wire Ramp, Play Wire Ramp Sound
End Sub

Sub RampTrigger003a_Unhit()
	WireRampOn False	'On Wire Ramp, Play Wire Ramp Sound
End Sub

Sub RampTrigger005a_Unhit()
	WireRampOn False	'On Wire Ramp, Play Wire Ramp Sound
End Sub

Sub RampTrigger001b_Unhit()
	PlaySoundAt "WireRamp_Stop", RampTrigger001b
End Sub

Sub RampTrigger003b_Unhit()
	PlaySoundAt "WireRamp_Stop", RampTrigger003b
End Sub


Sub IncreaseMulitpliers
	nYepReady = 0
	MultiplierRank(CurrentPlayer) = MultiplierRank(CurrentPlayer) + 1
	Playsound "Jackpot5"

	DOF 137, DOFPulse
	
	Select Case MultiplierRank(CurrentPlayer)
		Case 0:
			' Do nothing
		Case 1:
			nPlayfieldX(CurrentPlayer) = 2
			DisplayPlayfieldValue
		Case 2:
			nBonusX(CurrentPlayer) = 2
			DisplayBonusValue
		Case 3:
			nPlayfieldX(CurrentPlayer) = 3
			DisplayPlayfieldValue
		Case 4:
			nBonusX(CurrentPlayer) = 3
			DisplayBonusValue
		Case 5:
			nPlayfieldX(CurrentPlayer) = 4
			DisplayPlayfieldValue
		Case 6:
			nBonusX(CurrentPlayer) = 4
			DisplayBonusValue
		Case 7:
			nPlayfieldX(CurrentPlayer) = 5
			DisplayPlayfieldValue
		Case Else:
			' done with multipliers
			nBonusX(CurrentPlayer) = 5
			DisplayBonusValue

	End Select

	ResetPropane(CurrentPlayer)

End Sub


Dim MowerSpeed
MowerSpeed = 1

Dim CurrX, CurrY, CurrZ, Curr_RotY
Dim NewX, NewY, NewZ, New_RotY
Dim Steps, tmpX, tmpY, tmpZ,tmpRotY


Sub MoveLawnMowerForward
Debug.print "Move Forward"
	if lawnmowerforward.enabled = True Then vpmtimer.addtimer 1100, "MoveLawnMowerForward '": Exit Sub
	Playsound "LawnMowerWAV"
	
	if  MasterMowerLocation >= anLawnMower(CurrentPlayer,0)  Then Exit Sub

	Select Case anLawnMower(CurrentPlayer,0)
		Case 1
			Steps = 100
			CurrX = 430
			CurrY = 696
			CurrZ = 124
			Curr_RotY = -112

			NewX = 476
			NewY = 790
			NewZ = 130
			New_RotY = -118
 
			tmpX = (NewX-CurrX)/Steps
			tmpY = (NewY-CurrY)/Steps
			tmpZ = (NewZ-CurrZ)/Steps
			tmpRotY = (New_RotY-Curr_RotY)/Steps
			MasterMowerLocation = 1

			vpmtimer.addtimer 1000, "LawnMowerForward.Enabled = True ' "
		Case 2
			Steps = 100
			CurrX = 476
			CurrY = 790
			CurrZ = 130
			Curr_RotY = -118

			NewX = 526
			NewY = 884
			NewZ = 136
			New_RotY = -122
 
			tmpX = (NewX-CurrX)/Steps
			tmpY = (NewY-CurrY)/Steps
			tmpZ = (NewZ-CurrZ)/Steps
			tmpRotY = (New_RotY-Curr_RotY)/Steps
			MasterMowerLocation = 2

'			Debug.print "X:" &tmpX
'			Debug.print "Y:" &tmpY
'			Debug.print "Z:" &tmpZ
'			Debug.print "Rot Y:" &tmpRotY

			vpmtimer.addtimer 1000, "LawnMowerForward.Enabled = True ' "
		Case 3
			Steps = 100
			CurrX = 526
			CurrY = 884
			CurrZ = 136
			Curr_RotY = -122

			NewX = 582
			NewY = 980
			NewZ = 142
			New_RotY = -123
 
			tmpX = (NewX-CurrX)/Steps
			tmpY = (NewY-CurrY)/Steps
			tmpZ = (NewZ-CurrZ)/Steps
			tmpRotY = (New_RotY-Curr_RotY)/Steps
			MasterMowerLocation = 3

			vpmtimer.addtimer 1000, "LawnMowerForward.Enabled = True ' "
		Case 4
			Steps = 100
			CurrX = 582
			CurrY = 980
			CurrZ = 142
			Curr_RotY = -123

			NewX = 638
			NewY = 1075
			NewZ = 146
			New_RotY = -124
 
			tmpX = (NewX-CurrX)/Steps
			tmpY = (NewY-CurrY)/Steps
			tmpZ = (NewZ-CurrZ)/Steps
			tmpRotY = (New_RotY-Curr_RotY)/Steps
			MasterMowerLocation = 4

			vpmtimer.addtimer 1000, "LawnMowerForward.Enabled = True ' "
		Case 5
			Steps = 100
			CurrX = 638
			CurrY = 1075
			CurrZ = 142
			Curr_RotY = -124

			NewX = 700
			NewY = 1170
			NewZ = 150
			New_RotY = -125
 
			tmpX = (NewX-CurrX)/Steps
			tmpY = (NewY-CurrY)/Steps
			tmpZ = (NewZ-CurrZ)/Steps
			tmpRotY = (New_RotY-Curr_RotY)/Steps
			MasterMowerLocation = 5

			vpmtimer.addtimer 1000, "LawnMowerForward.Enabled = True ' "
		Case Else
	End Select
End Sub

Sub MoveLawnMowerReverse
	LawnMowerReverse.Enabled = True
	Select Case anLawnMower(CurrentPlayer,0)
		Case 1
			Steps = 100
			
			CurrX = 430
			CurrY = 696
			CurrZ = 124
			Curr_RotY = -112

			NewX = 476
			NewY = 790
			NewZ = 130
			New_RotY = -118
 
			tmpX = (NewX-CurrX)/Steps
			tmpY = (NewY-CurrY)/Steps
			tmpZ = (NewZ-CurrZ)/Steps
			tmpRotY = (New_RotY-Curr_RotY)/Steps

		Case 2
			Steps = 100

			CurrX = 476
			CurrY = 790
			CurrZ = 130
			Curr_RotY = -118

			NewX = 526
			NewY = 884
			NewZ = 136
			New_RotY = -122
 
			tmpX = (NewX-CurrX)/Steps
			tmpY = (NewY-CurrY)/Steps
			tmpZ = (NewZ-CurrZ)/Steps
			tmpRotY = (New_RotY-Curr_RotY)/Steps

		Case 3
			Steps = 100

			CurrX = 526
			CurrY = 884
			CurrZ = 136
			Curr_RotY = -122

			NewX = 582
			NewY = 980
			NewZ = 142
			New_RotY = -123
 
			tmpX = (NewX-CurrX)/Steps
			tmpY = (NewY-CurrY)/Steps
			tmpZ = (NewZ-CurrZ)/Steps
			tmpRotY = (New_RotY-Curr_RotY)/Steps

		Case 4
			Steps = 100

			CurrX = 582
			CurrY = 980
			CurrZ = 142
			Curr_RotY = -123

			NewX = 638
			NewY = 1075
			NewZ = 146
			New_RotY = -124
 
			tmpX = (NewX-CurrX)/Steps
			tmpY = (NewY-CurrY)/Steps
			tmpZ = (NewZ-CurrZ)/Steps
			tmpRotY = (New_RotY-Curr_RotY)/Steps

		Case 5
			Steps = 100

			CurrX = 638
			CurrY = 1075
			CurrZ = 142
			Curr_RotY = -124

			NewX = 700
			NewY = 1170
			NewZ = 150
			New_RotY = -125
 
			tmpX = (NewX-CurrX)/Steps
			tmpY = (NewY-CurrY)/Steps
			tmpZ = (NewZ-CurrZ)/Steps
			tmpRotY = (New_RotY-Curr_RotY)/Steps

		Case Else
			ResetLawnMowerAll
			LawnMowerReverse.Enabled = False
	End Select
	anLawnMower(CurrentPlayer,0) = anLawnMower(CurrentPlayer,0) - 1
End Sub


Sub LawnMowerForward_Timer()


	primitive152.x = currX + tmpX
	CurrX = CurrX + TmpX

	primitive152.y = currY + tmpY
	CurrY = CurrY + TmpY

	primitive152.z = currZ + tmpZ
	CurrZ = CurrZ + TmpZ

	primitive152.Roty = curr_RotY + tmpRotY
	Curr_RotY = Curr_RotY + TmpRotY

	Steps = Steps - 1
	if Steps < 0 Then LawnMowerForward.Enabled = 0
End Sub

Sub LawnMowerReverse_Timer()
'	Debug.print "Y:" &tmpY

	primitive152.x = NewX - tmpX
	NewX = NewX - TmpX

	primitive152.y = NewY - tmpY
	NewY = NewY - TmpY

	primitive152.z = NewZ - tmpZ
	NewZ = NewZ - TmpZ

	primitive152.Roty = New_RotY - tmpRotY
	New_RotY = New_RotY - TmpRotY

	Steps = Steps - 1
	if Steps < 0 Then LawnMowerReverse.Enabled = 0:MoveLawnMowerReverse
End Sub



'******************************************************
' 	ZBOU: VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1	  '0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.7	 'Level of bounces. Recommmended value of 0.7

Sub TargetBouncer(aBall,defvalue)
	Dim zMultiplier, vel, vratio
	If TargetBouncerEnabled = 1 And aball.z < 30 Then
		'   debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		vel = BallSpeed(aBall)
		If aBall.velx = 0 Then vratio = 1 Else vratio = aBall.vely / aBall.velx
		Select Case Int(Rnd * 6) + 1
			Case 1
				zMultiplier = 0.2 * defvalue
			Case 2
				zMultiplier = 0.25 * defvalue
			Case 3
				zMultiplier = 0.3 * defvalue
			Case 4
				zMultiplier = 0.4 * defvalue
			Case 5
				zMultiplier = 0.45 * defvalue
			Case 6
				zMultiplier = 0.5 * defvalue
		End Select
		aBall.velz = Abs(vel * zMultiplier * TargetBouncerFactor)
		aBall.velx = Sgn(aBall.velx) * Sqr(Abs((vel ^ 2 - aBall.velz ^ 2) / (1 + vratio ^ 2)))
		aBall.vely = aBall.velx * vratio
		'   debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		'   debug.print "conservation check: " & BallSpeed(aBall)/vel
	End If
End Sub

'Add targets or posts to the TargetBounce collection if you want to activate the targetbouncer code from them
Sub TargetBounce_Hit(idx)
	TargetBouncer ActiveBall, 1
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
'######################### Adjust these values to increase or lessen the elasticity

Dim FlippersD
Set FlippersD = New Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False
FlippersD.addpoint 0, 0, 1.1
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn   'tbpOut.text
	Public name, Threshold  'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
	End Sub
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If GameTime > 100 Then Report
	End Sub
	
	Public Sub Dampen(aBall)
		If threshold Then
			If BallSpeed(aBall) < threshold Then Exit Sub
		End If
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If debugOn Then str = name & " In vel:" & Round(cor.ballvel(aBall.id),2 ) & vbNewLine & "desired cor: " & Round(desiredcor,4) & vbNewLine & _
		"actual cor: " & Round(realCOR,4) & vbNewLine & "ballspeed coef: " & Round(coef, 3) & vbNewLine
		If Print Then Debug.print Round(cor.ballvel(aBall.id),2) & ", " & Round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef
		aBall.vely = aBall.vely * coef
		If debugOn Then TBPout.text = str
	End Sub
	
	Public Sub Dampenf(aBall, parm) 'Rubberizer is handle here
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If Abs(aball.velx) < 2 And aball.vely < 0 And aball.vely >  - 3.75 Then
			aBall.velx = aBall.velx * coef
			aBall.vely = aBall.vely * coef
		End If
	End Sub
	
	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		Dim x
		For x = 0 To UBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x) * aCoef
		Next
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
End Class

'******************************************************
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

Dim cor
Set cor = New CoRTracker

Class CoRTracker
	Public ballvel, ballvelx, ballvely
	
	Private Sub Class_Initialize
		ReDim ballvel(0)
		ReDim ballvelx(0)
		ReDim ballvely(0)
	End Sub
	
	Public Sub Update()	'tracks in-ball-velocity
		Dim str, b, AllBalls, highestID
		allBalls = GetBalls
		
		For Each b In allballs
			If b.id >= HighestID Then highestID = b.id
		Next
		
		If UBound(ballvel) < highestID Then ReDim ballvel(highestID)	'set bounds
		If UBound(ballvelx) < highestID Then ReDim ballvelx(highestID)	'set bounds
		If UBound(ballvely) < highestID Then ReDim ballvely(highestID)	'set bounds
		
		For Each b In allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

' Note, cor.update must be called in a 10 ms timer. The example table uses the GameTimer for this purpose, but sometimes a dedicated timer call RDampen is used.
'
'Sub RDampen_Timer
'	Cor.Update
'End Sub

'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************


'******************************************************
'****  GNEREAL ADVICE ON PHYSICS
'******************************************************
'
' It's advised that flipper corrections, dampeners, and general physics settings should all be updated per these 
' examples as all of these improvements work together to provide a realistic physics simulation.
'
' Tutorial videos provided by Bord
' Flippers: 	https://www.youtube.com/watch?v=FWvM9_CdVHw
' Dampeners: 	https://www.youtube.com/watch?v=tqsxx48C6Pg
' Physics: 		https://www.youtube.com/watch?v=UcRMG-2svvE
'
'
' Note: BallMassFix must be set to 1. BallSizeFix should be set to 50 (in other words the ball radius is 25) 
'
' Recommended Table Physics Settings
' | Gravity Constant             | 0.97      |
' | Playfield Friction           | 0.15-0.25 |
' | Playfield Elasticity         | 0.25      |
' | Playfield Elasticity Falloff | 0         |
' | Playfield Scatter            | 0         |
' | Default Element Scatter      | 2         |
'
' Bumpers
' | Force         | 9.5-10.5 |
' | Hit Threshold | 1.6-2    |
' | Scatter Angle | 2        |
' 
' Slingshots
' | Hit Threshold      | 2    |
' | Slingshot Force    | 4-5  |
' | Slingshot Theshold | 2-3  |
' | Elasticity         | 0.85 |
' | Friction           | 0.8  |
' | Scatter Angle      | 1    |



'******************************************************
'****  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
' There are several steps for taking advantage of nFozzy's flipper solution.  At a high level well need the following:
'	1. flippers with specific physics settings
'	2. custom triggers for each flipper (TriggerLF, TriggerRF)
'	3. an object or point to tell the script where the tip of the flipper is at rest (EndPointLp, EndPointRp)
'	4. and, special scripting
'
' A common mistake is incorrect flipper length.  A 3-inch flipper with rubbers will be about 3.125 inches long.  
' This translates to about 147 vp units.  Therefore, the flipper start radius + the flipper length + the flipper end 
' radius should  equal approximately 147 vp units. Another common mistake is is that sometimes the right flipper
' angle was set with a large postive value (like 238 or something). It should be using negative value (like -122).
'
' The following settings are a solid starting point for various eras of pinballs.
' |                    | EM's           | late 70's to mid 80's | mid 80's to early 90's | mid 90's and later |
' | ------------------ | -------------- | --------------------- | ---------------------- | ------------------ |
' | Mass               | 1              | 1                     | 1                      | 1                  |
' | Strength           | 500-1000 (750) | 1400-1600 (1500)      | 2000-2600              | 3200-3300 (3250)   |
' | Elasticity         | 0.88           | 0.88                  | 0.88                   | 0.88               |
' | Elasticity Falloff | 0.15           | 0.15                  | 0.15                   | 0.15               |
' | Fricition          | 0.8-0.9        | 0.9                   | 0.9                    | 0.9                |
' | Return Strength    | 0.11           | 0.09                  | 0.07                   | 0.055              |
' | Coil Ramp Up       | 2.5            | 2.5                   | 2.5                    | 2.5                |
' | Scatter Angle      | 0              | 0                     | 0                      | 0                  |
' | EOS Torque         | 0.3            | 0.3                   | 0.275                  | 0.275              |
' | EOS Torque Angle   | 4              | 4                     | 6                      | 6                  |
'


'******************************************************
' Flippers Polarity (Select appropriate sub based on era) 
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity



'*******************************************
' Early 90's and after

Sub InitPolarity()
	Dim x, a
	a = Array(LF, RF)
	For Each x In a
		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 60
		x.DebugOn=False ' prints some info in debugger
		
		x.AddPt "Polarity", 0, 0, 0
		x.AddPt "Polarity", 1, 0.05, -5.5
		x.AddPt "Polarity", 2, 0.4, -5.5
		x.AddPt "Polarity", 3, 0.6, -5.0
		x.AddPt "Polarity", 4, 0.65, -4.5
		x.AddPt "Polarity", 5, 0.7, -4.0
		x.AddPt "Polarity", 6, 0.75, -3.5
		x.AddPt "Polarity", 7, 0.8, -3.0
		x.AddPt "Polarity", 8, 0.85, -2.5
		x.AddPt "Polarity", 9, 0.9,-2.0
		x.AddPt "Polarity", 10, 0.95, -1.5
		x.AddPt "Polarity", 11, 1, -1.0
		x.AddPt "Polarity", 12, 1.05, -0.5
		x.AddPt "Polarity", 13, 1.1, 0
		x.AddPt "Polarity", 14, 1.3, 0
		
		x.AddPt "Velocity", 0, 0,	   1
		x.AddPt "Velocity", 1, 0.160, 1.06
		x.AddPt "Velocity", 2, 0.410, 1.05
		x.AddPt "Velocity", 3, 0.530, 1'0.982
		x.AddPt "Velocity", 4, 0.702, 0.968
		x.AddPt "Velocity", 5, 0.95,  0.968
		x.AddPt "Velocity", 6, 1.03,  0.945
	Next
	
	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
	LF.SetObjects "LF", LeftFlipper, TriggerLF
	RF.SetObjects "RF", RightFlipper, TriggerRF
End Sub




'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

' modified 2023 by nFozzy
' Removed need for 'endpoint' objects
' Added 'createvents' type thing for TriggerLF / TriggerRF triggers.
' Removed AddPt function which complicated setup imo
' made DebugOn do something (prints some stuff in debugger)
'   Otherwise it should function exactly the same as before

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt		'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay		'delay before trigger turns off and polarity is disabled
	Private Flipper, FlipperStart, FlipperEnd, FlipperEndY, LR, PartialFlipCoef
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
			If Not IsEmpty(balls(x) ) Then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next
	End Property
	
	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x) ) Then
				balldata(x).Data = balls(x)
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub
	'Timer shutoff for polaritycorrect
	Private Function FlipperOn()
		If GameTime < FlipAt+TimeDelay Then
			FlipperOn = True
		End If
	End Function
	
	Public Sub PolarityCorrect(aBall)
		If FlipperOn() Then
			Dim tmp, BallPos, x, IDX, Ycoef
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
					If ballpos > 0.65 Then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)								'find safety coefficient 'ycoef' data
				End If
			Next
			
			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				If ballpos > 0.65 Then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)												'find safety coefficient 'ycoef' data
			End If
			
			'Velocity correction
			If Not IsEmpty(VelocityIn(0) ) Then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
				
				If partialflipcoef < 1 Then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
				
				If Enabled Then aBall.Velx = aBall.Velx*VelCoef
				If Enabled Then aBall.Vely = aBall.Vely*VelCoef
			End If
			
			'Polarity Correction (optional now)
			If Not IsEmpty(PolarityIn(0) ) Then
				Dim AddX
				AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				
				If Enabled Then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
			If DebugOn Then debug.print "PolarityCorrect" & " " & Name & " @ " & GameTime & " " & Round(BallPos*100) & "%" & " AddX:" & Round(AddX,2) & " Vel%:" & Round(VelCoef*100)
		End If
		RemoveBall aBall
	End Sub
End Class





'******************************************************
'  SLINGSHOT CORRECTION FUNCTIONS
'******************************************************
' To add these slingshot corrections:
' 	- On the table, add the endpoint primitives that define the two ends of the Slingshot
'	- Initialize the SlingshotCorrection objects in InitSlingCorrection
' 	- Call the .VelocityCorrect methods from the respective _Slingshot event sub


dim LS : Set LS = New SlingshotCorrection
dim RS : Set RS = New SlingshotCorrection
dim RS2 : Set RS2 = New SlingshotCorrection

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
'
'Function RotPoint(x,y,angle)
'    dim rx, ry
'    rx = x*dCos(angle) - y*dSin(angle)
'    ry = x*dSin(angle) + y*dCos(angle)
'    RotPoint = Array(rx,ry)
'End Function

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


'******************************************************
'  FLIPPER TRICKS
'******************************************************
' To add the flipper tricks you must
'	 - Include a call to FlipperCradleCollision from within OnBallBallCollision subroutine
'	 - Include a call the CheckLiveCatch from the LeftFlipper_Collide and RightFlipper_Collide subroutines

RightFlipper.timerinterval = 1
Rightflipper.timerenabled = True

Sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
End Sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b
	   Dim BOT
	   BOT = GetBalls
	
	If Flipper1.currentangle = Endangle1 And EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'   debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					Exit Sub
				End If
			Next
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
				End If
			Next
		End If
	Else
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 Then EOSNudge1 = 0
	End If
End Sub


Dim FCCDamping: FCCDamping = 0.4

Sub FlipperCradleCollision(ball1, ball2, velocity)
	if velocity < 0.7 then exit sub		'filter out gentle collisions
    Dim DoDamping, coef
    DoDamping = false
    'Check left flipper
    If LeftFlipper.currentangle = LFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, LeftFlipper) OR FlipperTrigger(ball2.x, ball2.y, LeftFlipper) Then DoDamping = true
    End If
    'Check right flipper
    If RightFlipper.currentangle = RFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, RightFlipper) OR FlipperTrigger(ball2.x, ball2.y, RightFlipper) Then DoDamping = true
    End If
    If DoDamping Then
		coef = FCCDamping
        ball1.velx = ball1.velx * coef: ball1.vely = ball1.vely * coef: ball1.velz = ball1.velz * coef
        ball2.velx = ball2.velx * coef: ball2.vely = ball2.vely * coef: ball2.velz = ball2.velz * coef
    End If
End Sub

'*****************
' Maths
'*****************


Function dSin(degrees)
	dsin = Sin(degrees * Pi / 180)
End Function

Function dCos(degrees)
	dcos = Cos(degrees * Pi / 180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		End If
	ElseIf dx = 0 Then
		If dy = 0 Then
			Atn2 = 0
		Else
			Atn2 = Sgn(dy) * pi / 2
		End If
	End If
End Function

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function

'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = Sqr((ax - bx) ^ 2 + (ay - by) ^ 2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) 'Distance between a point and a line where point Is px,py
	DistancePL = Abs((by - ay) * px - (bx - ax) * py + bx * ay - by * ax) / Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI / 180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax)) * 180 / PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle + 90)) + Flipper.x, Sin(Radians(Flipper.currentangle + 90)) + Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle = Abs(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360
	
	If DistanceFromFlipper(ballx,bally,Flipper) < 48 And DiffAngle <= 90 And Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If
End Function

Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy) 'Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
	Dim AB, BC, CD, DA
	AB = (bx * py) - (by * px) - (ax * py) + (ay * px) + (ax * by) - (ay * bx)
	BC = (cx * py) - (cy * px) - (bx * py) + (by * px) + (bx * cy) - (by * cx)
	CD = (dx * py) - (dy * px) - (cx * py) + (cy * px) + (cx * dy) - (cy * dx)
	DA = (ax * py) - (ay * px) - (dx * py) + (dy * px) + (dx * ay) - (dy * ax)
	
	If (AB <= 0 And BC <= 0 And CD <= 0 And DA <= 0) Or (AB >= 0 And BC >= 0 And CD >= 0 And DA >= 0) Then
		InRect = True
	Else
		InRect = False
	End If
End Function

Function InRotRect(ballx,bally,px,py,angle,ax,ay,bx,by,cx,cy,dx,dy)
	Dim rax,ray,rbx,rby,rcx,rcy,rdx,rdy
	Dim rotxy
	rotxy = RotPoint(ax,ay,angle)
	rax = rotxy(0) + px
	ray = rotxy(1) + py
	rotxy = RotPoint(bx,by,angle)
	rbx = rotxy(0) + px
	rby = rotxy(1) + py
	rotxy = RotPoint(cx,cy,angle)
	rcx = rotxy(0) + px
	rcy = rotxy(1) + py
	rotxy = RotPoint(dx,dy,angle)
	rdx = rotxy(0) + px
	rdy = rotxy(1) + py
	
	InRotRect = InRect(ballx,bally,rax,ray,rbx,rby,rcx,rcy,rdx,rdy)
End Function

Function RotPoint(x,y,angle)
	Dim rx, ry
	rx = x * dCos(angle) - y * dSin(angle)
	ry = x * dSin(angle) + y * dCos(angle)
	RotPoint = Array(rx,ry)
End Function

'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

Dim LFPress, RFPress, LFCount, RFCount, LFPress2, RFPress2
Dim LFState, RFState
Dim EOST, EOSA,Frampup, FElasticity,FReturn
Dim RFEndAngle, LFEndAngle

Const FlipperCoilRampupMode = 0 '0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
'Const EOSTnew = 1.5 'EM's to late 80's - new recommendation by rothbauerw (previously 1)
Const EOSTnew = 1.2 '90's and later - new recommendation by rothbauerw (previously 0.8)
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode
	Case 0
		SOSRampup = 2.5
	Case 1
		SOSRampup = 6
	Case 2
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'   Const EOSReturn = 0.055  'EM's
'   Const EOSReturn = 0.045  'late 70's to mid 80's
'   Const EOSReturn = 0.035  'mid 80's to early 90's
Const EOSReturn = 0.025  'mid 90's and later

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
	Flipper.eostorque = EOST * EOSReturn / FReturn
	
	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b, BOT
				BOT = GetBalls
		
		For b = 0 To UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= - 0.4 Then BOT(b).vely =  - 0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState)
	Dim Dir
	Dir = Flipper.startangle / Abs(Flipper.startangle) '-1 for Right Flipper
	
	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup
			Flipper.endangle = FEndAngle - 3 * Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) And FlipperPress = 1 Then
		If FCount = 0 Then FCount = GameTime
		
		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	ElseIf Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 And FlipperPress = 1 Then
		If FState <> 3 Then
			Flipper.eostorque = EOST
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If
	End If
End Sub

Const LiveDistanceMin = 30  'minimum distance In vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114 'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
	Dim Dir
	Dir = Flipper.startangle / Abs(Flipper.startangle)	'-1 for Right Flipper
	Dim LiveCatchBounce																														'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime
	CatchTime = GameTime - FCount
	
	If CatchTime <= LiveCatch And parm > 6 And Abs(Flipper.x - ball.x) > LiveDistanceMin And Abs(Flipper.x - ball.x) < LiveDistanceMax Then
		If CatchTime <= LiveCatch * 0.5 Then												'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		Else
			LiveCatchBounce = Abs((LiveCatch / 2) - CatchTime)		'Partial catch when catch happens a bit late
		End If
		
		If LiveCatchBounce = 0 And ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx = 0
		ball.angmomy = 0
		ball.angmomz = 0
	Else
		If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf ActiveBall, parm
	End If
End Sub

'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************

Sub plungertimer_Timer()
Ptime = PTime + 1
	if PTime > 12 And bSqueal Then
		Playsound "tiresqueal", -1, 1
		bSqueal = False
		StartCharge
	End If
End Sub

Sub Gate001_Hit()
	BIPL = False
	bBallInPlungerLane = False

	if bOnTheFirstBall Then PuPGameLaunch

	DelayQRClaim.Enabled=False
	if ScorbitActive And bOnTheFirstBallScorbit Then
		bOnTheFirstBallScorbit = False
		HideScorbit
	End If

    If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
        EnableBallSaver BallSaverTime
		bBallSaverReady = False
    End If

	Stopsound "IdleTruck"
	vpmtimer.addtimer 1700, "StopCharge '"
End Sub

'********TRUCK SMOKE********

Dim	Charge1Pos, ChargeFrames 
ChargeFrames = Array( "CH-011", "CH-012", "CH-013", "CH-014", "CH-015", "CH-016", "CH-017", "CH-018", "CH-019", "CH-020",_
"CH-021", "CH-022", "CH-023", "CH-024", "CH-025", "CH-026", "CH-027", "CH-028", "CH-029", "CH-030", "CH-031", "CH-032", "CH-033", "CH-034", "CH-035", "CH-036", "CH-037", "CH-038", "CH-039", "CH-040", "CH-041",_
"CH-042", "CH-043", "CH-044", "CH-045", "CH-046", "CH-047", "CH-048", "CH-049", "CH-050", "CH-051", "CH-052", "CH-053", "CH-054", "CH-055", "CH-056", "CH-057", "CH-058", "CH-059", "CH-060", "CH-061", "CH-062",_
"CH-063", "CH-064", "CH-065", "CH-066", "CH-067", "CH-068", "CH-069", "CH-070", "CH-071", "CH-072", "CH-073", "CH-074", "CH-075", "CH-076", "CH-077", "CH-078", "CH-079", "CH-080", "CH-081", "CH-082", "CH-083",_
"CH-084", "CH-085", "CH-086", "CH-087", "CH-088", "CH-089", "CH-090", "CH-091", "CH-092", "CH-093", "CH-94", "CH-095","CH-096", "CH-097","CH-098", "CH-099", "CH-0100", "CH-0101", "CH-0102", "CH-0103", "CH-0104",_
"CH-0105", "CH-0106", "CH-0107", "CH-0108", "CH-0109", "CH-0110", "CH-0111", "CH-0112", "CH-0113", "CH-0114", "CH-0115", "CH-0116", "CH-0117", "CH-0118", "CH-0119", "CH-0120", "CH-0121", "CH-0122", "CH-0123",_
"CH-0124", "CH-0125", "CH-0126", "CH-0127", "CH-0128", "CH-0129", "CH-0130", "CH-0131", "CH-0132", "CH-0133", "CH-0134", "CH-0135", "CH-0136", "CH-0137", "CH-0138", "CH-0139", "CH-0140", "CH-0141", "CH-0142",_
"CH-0143", "CH-0144", "CH-0145", "CH-0146", "CH-0147", "CH-0148", "CH-0149", "CH-0150", "CH-0151", "CH-0152", "CH-0153", "CH-0154", "CH-0155", "CH-0156", "CH-0157", "CH-0158", "CH-0159", "CH-0160", "CH-0161",_
"CH-0162", "CH-0163", "CH-0164", "CH-0165", "CH-0166", "CH-0167", "CH-0168", "CH-0169", "CH-0170", "CH-0171", "CH-0172", "CH-0173", "CH-0174", "CH-0175", "CH-0176", "CH-0177", "CH-0178", "CH-0179")

Sub StartCharge
	debug.print"PRE-Pos:" &Charge1Pos
	Charge1Pos = 0
	debug.print"Pos:" &Charge1Pos
	ChargeFlash.visible = 1
	ChargeTimer.Enabled = 1
End Sub
 
Sub ChargeTimer_timer
	ChargeFlash.ImageA = ChargeFrames(Charge1Pos)
	Charge1Pos = (Charge1Pos + 1) MOD 169
End Sub
 
Sub StopCharge
	debug.print"Called StopCHARGE"
	ChargeFlash.visible = 0
	Charge1Pos = 0
	ChargeTimer.Enabled = 0
End Sub

'***********************************************************



Sub Gate003_Hit()
	BIPL = True
	Debug.print "BIPL True"
End Sub



'***********************************************************************************************************************
' Lights State Controller - 0.9.1
'  
' A light state controller for original vpx tables.
'
' Documentation: https://github.com/mpcarr/vpx-light-controller
'
'***********************************************************************************************************************


Class LStateController

    Private m_currentFrameState, m_on, m_off, m_seqRunners, m_lights, m_seqs, m_vpxLightSyncRunning, m_vpxLightSyncClear, m_vpxLightSyncCollection, m_tableSeqColor, m_tableSeqOffset, m_tableSeqSpeed, m_tableSeqDirection, m_tableSeqFadeUp, m_tableSeqFadeDown, m_frametime, m_initFrameTime, m_pulse, m_pulseInterval, m_lightmaps, m_seqOverrideRunners, m_pauseMainLights, m_pausedLights, m_minX, m_minY, m_maxX, m_maxY, m_width, m_height, m_centerX, m_centerY, m_coordsX, m_coordsY, m_angles, m_radii

    Private Sub Class_Initialize()
        Set m_lights = CreateObject("Scripting.Dictionary")
        Set m_on = CreateObject("Scripting.Dictionary")
        Set m_off = CreateObject("Scripting.Dictionary")
        Set m_seqRunners = CreateObject("Scripting.Dictionary")
        Set m_seqOverrideRunners = CreateObject("Scripting.Dictionary")
        Set m_currentFrameState = CreateObject("Scripting.Dictionary")
        Set m_seqs = CreateObject("Scripting.Dictionary")
        Set m_pulse = CreateObject("Scripting.Dictionary")
        Set m_on = CreateObject("Scripting.Dictionary")
        m_vpxLightSyncRunning = False
        m_vpxLightSyncCollection = Null
        m_initFrameTime = 0
        m_frameTime = 0
        m_pulseInterval = 26
        m_vpxLightSyncClear = False
        m_tableSeqColor = Null
        m_tableSeqFadeUp = Null
        m_tableSeqFadeDown = Null
        m_pauseMainLights = False
        Set m_pausedLights = CreateObject("Scripting.Dictionary")
        Set m_lightmaps = CreateObject("Scripting.Dictionary")
        m_minX = 1000000
        m_minY = 1000000
        m_maxX = -1000000
        m_maxY = -1000000
        m_centerX = Round(tableWidth/2)
        m_centerY = Round(tableHeight/2)
    End Sub

    Private Sub AssignStateForFrame(key, state)
        If m_currentFrameState.Exists(key) Then
            m_currentFrameState.Remove key
        End If
        m_currentFrameState.Add key, state
    End Sub

    Public Sub LoadLightShows()
        Dim oFile
        Dim oFSO : Set oFSO = CreateObject("Scripting.FileSystemObject")
        Dim objFileToWrite : Set objFileToWrite = CreateObject("Scripting.FileSystemObject").OpenTextFile(cGameName & "_LightShows/lights-out.txt",2,true)
        For Each oFile In oFSO.GetFolder(cGameName & "_LightShows").Files
            If LCase(oFSO.GetExtensionName(oFile.Name)) = "yaml" And Not Left(oFile.Name,6) = "lights" Then
                Dim textStream : Set textStream = oFSO.OpenTextFile(oFile.Path, 1)
                Dim show : show = textStream.ReadAll
                Dim fileName : fileName = "lSeq" & Replace(oFSO.GetFileName(oFile.Name), "."&oFSO.GetExtensionName(oFile.Name), "")
                Dim lcSeq : lcSeq = "Dim " & fileName & " : Set " & fileName & " = New LCSeq"&vbCrLf
                lcSeq = lcSeq + fileName & ".Name = """&fileName&""""&vbCrLf
                Dim seq : seq = ""
                Dim re : Set re = New RegExp
                With re
                    .Pattern    = "- time:.*?\n"
                    .IgnoreCase = False
                    .Global     = True
                End With
                Dim matches : Set matches = re.execute(show)
                Dim steps : steps = matches.Count
                Dim match, nextMatchIndex, uniqueLights
                Set uniqueLights = CreateObject("Scripting.Dictionary")
                nextMatchIndex = 1
                For Each match in matches
                    Dim lightStep
                    If Not nextMatchIndex < steps Then
                        lightStep = Mid(show, match.FirstIndex, Len(show))
                    Else
                        lightStep = Mid(show, match.FirstIndex, matches(nextMatchIndex).FirstIndex - match.FirstIndex)
                        nextMatchIndex = nextMatchIndex + 1
                    End If

                    Dim re1 : Set re1 = New RegExp
                    With re1
                        .Pattern        = ".*:?: '([A-Fa-f0-9]{6})'"
                        .IgnoreCase     = True
                        .Global         = True
                    End With

                    Dim lightMatches : Set lightMatches = re1.execute(lightStep)
                    If lightMatches.Count > 0 Then
                        Dim lightMatch, lightStr, lightSplit
                        lightStr = "Array("
                        lightSplit = 0
                        For Each lightMatch in lightMatches
                            Dim sParts : sParts = Split(lightMatch.Value, ":")
                            Dim lightName : lightName = Trim(sParts(0))
                            Dim color : color = Trim(Replace(sParts(1),"'", ""))
                            If color = "000000" Then
                                lightStr = lightStr + """"&lightName&"|0|000000"","
                            Else
                                lightStr = lightStr + """"&lightName&"|100|"&color&""","
                            End If

                            If Len(lightStr)+20 > 2000 And lightSplit = 0 Then                           
                                lightSplit = Len(lightStr)
                            End If

                            uniqueLights(lightname) = 0
                        Next
                        lightStr = Left(lightStr, Len(lightStr) - 1)
                        lightStr = lightStr & ")"
                        
                        If lightSplit > 0 Then
                            lightStr = Left(lightStr, lightSplit) & " _ " & vbCrLF & Right(lightStr, Len(lightStr)-lightSplit)
                        End If

                        seq = seq + lightStr & ", _"&vbCrLf
                    Else
                        seq = seq + "Array(), _"&vbCrLf
                    End If

                    
                    Set re1 = Nothing
                Next
                
                lcSeq = lcSeq + filename & ".Sequence = Array( " & Left(seq, Len(seq) - 5) & ")"&vbCrLf
                'lcSeq = lcSeq + seq & vbCrLf
                lcSeq = lcSeq + fileName & ".UpdateInterval = 20"&vbCrLf
                lcSeq = lcSeq + fileName & ".Color = Null"&vbCrLf
                lcSeq = lcSeq + fileName & ".Repeat = False"&vbCrLf

                'MsgBox(lcSeq)
                objFileToWrite.WriteLine(lcSeq)
                ExecuteGlobal lcSeq
                Set re = Nothing

                textStream.Close
            End if
        Next
        'Clean up
        objFileToWrite.Close
        Set objFileToWrite = Nothing
        Set oFile = Nothing
        Set oFSO = Nothing
    End Sub

    Public Sub CompileLights(collection, name)
        Dim light
        Dim lights : lights = "light:" & vbCrLf
        For Each light in collection
            lights = lights + light.name & ":"&vbCrLf
            lights = lights + "   x: "& light.x/tablewidth & vbCrLf
            lights = lights + "   y: "& light.y/tableheight & vbCrLf
        Next
        Dim objFileToWrite : Set objFileToWrite = CreateObject("Scripting.FileSystemObject").OpenTextFile(cGameName & "_LightShows/lights-"&name&".yaml",2,true)
        objFileToWrite.WriteLine(lights)
        objFileToWrite.Close
        Set objFileToWrite = Nothing
        Debug.print("Lights YAML File saved to: " & cGameName & "LightShows/lights-"&name&".yaml")
    End Sub

    Dim leds
    Dim ledGrid()
    Dim lightsToLeds

    Sub PrintLEDs
        Dim light
        Dim lights : lights = ""
    
        Dim row,col,value,i
        For row = LBound(ledGrid, 1) To UBound(ledGrid, 1)
            For col = LBound(ledGrid, 2) To UBound(ledGrid, 2)
                ' Access the array element and do something with it
                value = ledGrid(row, col)
                lights = lights + cstr(value) & vbTab
            Next
            lights = lights + vbCrLf
        Next

        Dim objFileToWrite : Set objFileToWrite = CreateObject("Scripting.FileSystemObject").OpenTextFile(cGameName & "_LightShows/led-grid.txt",2,true)
        objFileToWrite.WriteLine(lights)
        objFileToWrite.Close
        Set objFileToWrite = Nothing
        Debug.print("Lights File saved to: " & cGameName & "LightShows/led-grid.txt")


        lights = ""
        For i = 0 To UBound(leds)
            value = leds(i)
            If IsArray(value) Then
                lights = lights + "Index: " & cstr(value(0)) & ". X: " & cstr(value(1)) & ". Y:" & cstr(value(2)) & ". Angle:" & cstr(value(3)) & ". Radius:" & cstr(value(4)) & ". CoordsX:" & cstr(value(5)) & ". CoordsY:" & cstr(value(6)) & ". Angle256:" & cstr(value(7)) &". Radii256:" & cstr(value(8)) &","
            End If
            lights = lights + vbCrLf
            'lights = lights + cstr(value) & ","
            
        Next

        
        Set objFileToWrite = CreateObject("Scripting.FileSystemObject").OpenTextFile(cGameName & "_LightShows/coordsX.txt",2,true)
        objFileToWrite.WriteLine(lights)
        objFileToWrite.Close
        Set objFileToWrite = Nothing
        Debug.print("Lights File saved to: " & cGameName & "LightShows/coordsX.txt")


    End Sub

    Public Sub RegisterLights(mode)

        Dim idx,tmp,vpxLight,lcItem
    
            vpmMapLights aLights
            Dim colCount : colCount = Round(tablewidth/20)
            Dim rowCount : rowCount = Round(tableheight/20)
                
            dim ledIdx : ledIdx = 0
            redim leds(UBound(Lights)-1)
            redim lightsToLeds(UBound(Lights)-1)
            ReDim ledGrid(rowCount,colCount)
            For idx = 0 to UBound(Lights)
                vpxLight = Null
                Set lcItem = new LCItem
                debug.print("TRYING TO REGISTER IDX: " & idx)
                If IsArray(Lights(idx)) Then
                    tmp = Lights(idx)
                    Set vpxLight = tmp(0)
                    debug.print("TEMP LIGHT NAME for idx:" & idx & ", light: " & vpxLight.name)
                ElseIf IsObject(Lights(idx)) Then
                    Set vpxLight = Lights(idx)
                End If
                If Not IsNull(vpxLight) Then
                    Debug.print("Registering Light: "& vpxLight.name)


                    Dim r : r = Round(vpxLight.y/20)
                    Dim c : c = Round(vpxLight.x/20)
                    If r < rowCount And c < colCount And r >= 0 And c >= 0 Then
                        If Not ledGrid(r,c) = "" Then
                            MsgBox(vpxLight.name & " is too close to another light")
                        End If
                        leds(ledIdx) = Array(ledIdx, c, r, 0,0,0,0,0,0) 'index, row, col, angle, radius, x256, y256, angle256, radius256
                        lightsToLeds(idx) = ledIdx
                        ledGrid(r,c) = ledIdx
                        ledIdx = ledIdx + 1
                        If (c < m_minX) Then : m_minX = c
                        if (c > m_maxX) Then : m_maxX = c
                
                        if (r < m_minY) Then : m_minY = r
                        if (r > m_maxY) Then : m_maxY = r
                    End If
                    Dim e, lmStr: lmStr = "lmArr = Array("    
                    For Each e in GetElements()
                        If InStr(e.Name, "_" & vpxLight.Name & "_") Or InStr(e.Name, "_" & vpxLight.UserValue & "_") Then
                            Debug.Print(e.Name)
                            lmStr = lmStr & e.Name & ","
                        End If
                    Next
                    lmStr = lmStr & "Null)"
                    lmStr = Replace(lmStr, ",Null)", ")")
                    ExecuteGlobal "Dim lmArr : "&lmStr
                    m_lightmaps.Add vpxLight.Name, lmArr
                    Debug.print("Registering Light: "& vpxLight.name) 
                    lcItem.Init idx, vpxLight.BlinkInterval, Array(vpxLight.color, vpxLight.colorFull), vpxLight.name, vpxLight.x, vpxLight.y
                    m_lights.Add vpxLight.Name, lcItem
                    m_seqRunners.Add "lSeqRunner" & CStr(vpxLight.name), new LCSeqRunner
                End If
            Next
            'ReDim Preserve leds(ledIdx)
            m_width = m_maxX - m_minX + 1
            m_height = m_maxY - m_minY + 1
            m_centerX = m_width / 2
            m_centerY = m_height / 2
            GenerateLedMapCode()
    End Sub

    Private Sub GenerateLedMapCode()

        Dim minX256, minY256, minAngle, minAngle256, minRadius, minRadius256
        Dim maxX256, maxY256, maxAngle, maxAngle256, maxRadius, maxRadius256
        Dim i, led
        minX256 = 1000000
        minY256 = 1000000
        minAngle = 1000000
        minAngle256 = 1000000
        minRadius = 1000000
        minRadius256 = 1000000

        maxX256 = -1000000
        maxY256 = -1000000
        maxAngle = -1000000
        maxAngle256 = -1000000
        maxRadius = -1000000
        maxRadius256 = -1000000

        For i = 0 To UBound(leds)
            led = leds(i)
            If IsArray(led) Then
                
                Dim x : x = led(1)
                Dim y : y = led(2)
            
                Dim radius : radius = Sqr((x - m_centerX) ^ 2 + (y - m_centerY) ^ 2)
                Dim radians: radians = Atn2(m_centerY - y, m_centerX - x)
                        dim angle 
                angle = radians * (180 / 3.141592653589793)
                Do While angle < 0
                    angle = angle + 360
                Loop
                Do While angle > 360
                    angle = angle - 360
                Loop
            
                If angle < minAngle Then
                    minAngle = angle
                End If
                If angle > maxAngle Then
                    maxAngle = angle
                End If
            
                If radius < minRadius Then
                    minRadius = radius
                End If
                If radius > maxRadius Then
                    maxRadius = radius
                End If
            
                led(3) = angle
                led(4) = radius
                leds(i) = led
            End If
        Next

        For i = 0 To UBound(leds)
            led = leds(i)
            If IsArray(led) Then
                Dim x256 : x256 = MapNumber(led(1), m_minX, m_maxX, 0, 255)
                Dim y256 : y256 = MapNumber(led(2), m_minY, m_maxY, 0, 255)
                Dim angle256 : angle256 = MapNumber(led(3), 0, 360, 0, 255)
                Dim radius256 : radius256 = MapNumber(led(4), 0, maxRadius, 0, 255)
            
                led(5) = Round(x256)
                led(6) = Round(y256)
                led(7) = Round(angle256)
                led(8) = Round(radius256)
            
                If x256 < minX256 Then minX256 = x256
                If x256 > maxX256 Then maxX256 = x256
            
                If y256 < minY256 Then minY256 = y256
                If y256 > maxY256 Then maxY256 = y256
            
                If angle256 < minAngle256 Then minAngle256 = angle256
                If angle256 > maxAngle256 Then maxAngle256 = angle256
            
                If radius256 < minRadius256 Then minRadius256 = radius256
                If radius256 > maxRadius256 Then maxRadius256 = radius256
                leds(i) = led
            End If
        Next

        reDim m_coordsX(UBound(leds)-1)
        reDim m_coordsY(UBound(leds)-1)
        reDim m_angles(UBound(leds)-1)
        reDim m_radii(UBound(leds)-1)
        
        For i = 0 To UBound(leds)
            led = leds(i)
            If IsArray(led) Then
                m_coordsX(i)    =  leds(i)(5) 'x256
                m_coordsY(i)    =  leds(i)(6) 'y256
                m_angles(i)     =  leds(i)(7) 'angle256
                m_radii(i)      =  leds(i)(8) 'radius256
            End If
        Next

    End Sub

    Private Function MapNumber(l, inMin, inMax, outMin, outMax)
        If (inMax - inMin + outMin) = 0 Then
            MapNumber = 0
        Else
            MapNumber = ((l - inMin) * (outMax - outMin)) / (inMax - inMin) + outMin
        End If
    End Function

    Private Function ReverseArray(arr)
        Dim i, upperBound
        upperBound = UBound(arr)

        ' Create a new array of the same size
        Dim reversedArr()
        ReDim reversedArr(upperBound)

        ' Fill the new array with elements in reverse order
        For i = 0 To upperBound
            reversedArr(i) = arr(upperBound - i)
        Next

        ReverseArray = reversedArr
    End Function

    Private Function Atn2(dy, dx)
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

    Private Function ColtoArray(aDict)	'converts a collection to an indexed array. Indexes will come out random probably.
        redim a(999)
        dim count : count = 0
        dim x  : for each x in aDict : set a(Count) = x : count = count + 1 : Next
        redim preserve a(count-1) : ColtoArray = a
    End Function

    Function IncrementUInt8(x, increment)
    If x + increment > 255 Then
        IncrementUInt8 = x + increment - 256
    Else
        IncrementUInt8 = x + increment
    End If
    End Function

    Public Sub AddLight(light, idx)
        If m_lights.Exists(light.name) Then
            Exit Sub
        End If
        Dim lcItem : Set lcItem = new LCItem
        lcItem.Init idx, light.BlinkInterval, Array(light.color, light.colorFull), light.name, light.x, light.y
        m_lights.Add light.Name, lcItem
        m_seqRunners.Add "lSeqRunner" & CStr(light.name), new LCSeqRunner
    End Sub

    Public Sub LightState(light, state)
        m_lightOff(light.name)
        If state = 1 Then
            m_lightOn(light.name)
        ElseIF state = 2 Then
            Blink(light)
        End If
    End Sub

    Public Sub LightOn(light)
        m_LightOn(light.name)
    End Sub

    Public Sub LightOnWithColor(light, color)
        m_LightOnWithColor light.name, color
    End Sub

    Public Sub FadeLightToColor(light, color, fadeSpeed)
        If m_lights.Exists(light.name) Then
            dim lightColor, steps
            steps = Round(fadeSpeed/20)
            If steps < 10 Then
                steps = 10
            End If
            lightColor = m_lights(light.name).Color
            Dim seq : Set seq = new LCSeq
            seq.Name = light.name & "Fade"
            seq.Sequence = FadeRGB(light.name, lightColor(0), color, steps)
            seq.Color = Null
            seq.UpdateInterval = 20
            seq.Repeat = False
            m_lights(light.name).Color = color
            m_seqRunners("lSeqRunner"&CStr(light.name)).AddItem seq
            If color = RGB(0,0,0) Then
                m_lightOff(light.name)
            End If
        End If
    End Sub

    Public Sub FlickerOn(light)
        Dim name : name = light.name
        If m_lights.Exists(name) Then
            m_lightOn(name)

            If m_pulse.Exists(name) Then 
                Exit Sub
            End If
            m_pulse.Add name, (new PulseState)(m_lights(name), Array(37,100,24,0,70), 0, m_pulseInterval, 1, null)
        End If
    End Sub  
    
    Public Sub LightColor(light, color)

        If m_lights.Exists(light.name) Then
            m_lights(light.name).Color = color
            'Update internal blink seq for light
            If m_seqs.Exists(light.name & "Blink") Then
                m_seqs(light.name & "Blink").Color = color
            End If

        End If
    End Sub

    Private Sub m_LightOn(name)
        
        If m_lights.Exists(name) Then
            
            If m_off.Exists(name) Then 
                m_off.Remove(name)
            End If
            If m_seqs.Exists(name & "Blink") Then
                m_seqRunners("lSeqRunner"&CStr(name)).RemoveItem m_seqs(name & "Blink")
            End If
            If m_on.Exists(name) Then 
                Exit Sub
            End If
            m_on.Add name, m_lights(name)
        End If
    End Sub

    Private Sub m_LightOnWithColor(name, color)
        If m_lights.Exists(name) Then
            m_lights(name).Color = color
            If m_off.Exists(name) Then 
                m_off.Remove(name)
            End If

            If m_seqs.Exists(name & "Blink") Then
                m_seqRunners("lSeqRunner"&CStr(name)).RemoveItem m_seqs(name & "Blink")
            End If

            If m_on.Exists(name) Then 
                Exit Sub
            End If
            m_on.Add name, m_lights(name)
        End If
    End Sub

    Public Sub LightOff(light)
        m_lightOff(light.name)
    End Sub

    Private Sub m_lightOff(name)
        If m_lights.Exists(name) Then
            If m_on.Exists(name) Then 
                m_on.Remove(name)
            End If

            If m_seqs.Exists(name & "Blink") Then
                m_seqRunners("lSeqRunner"&CStr(name)).RemoveItem m_seqs(name & "Blink")
            End If

            If m_off.Exists(name) Then 
                Exit Sub
            End If
            m_off.Add name, m_lights(name)
        End If
    End Sub

    Public Sub UpdateBlinkInterval(light, interval)
        If m_lights.Exists(light.name) Then
            light.BlinkInterval = interval
            If m_seqs.Exists(light.name & "Blink") Then
                m_seqs.Item(light.name & "Blink").UpdateInterval = interval
            End If
        End If
    End Sub


    Public Sub Pulse(light, repeatCount)
        Dim name : name = light.name
        If m_lights.Exists(name) Then
            If m_off.Exists(name) Then 
                m_off.Remove(name)
            End If
            If m_pulse.Exists(name) Then 
                Exit Sub
            End If
            'Array(100,94,32,13,6,3,0)
            m_pulse.Add name, (new PulseState)(m_lights(name), Array(37,100,24,0,70,100,12,0), 0, m_pulseInterval, repeatCount, null)
        End If
    End Sub

    Public Sub PulseWithColor(light, color, repeatCount)
        Dim name : name = light.name
        If m_lights.Exists(name) Then
            If m_off.Exists(name) Then 
                m_off.Remove(name)
            End If
            If m_pulse.Exists(name) Then 
                Exit Sub
            End If
            'Array(100,94,32,13,6,3,0)
            m_pulse.Add name, (new PulseState)(m_lights(name), Array(37,100,24,0,70,100,12,0), 0, m_pulseInterval, repeatCount,  Array(color,null))
        End If
    End Sub

    Public Sub PulseWithProfile(light, profile, repeatCount)
        Dim name : name = light.name
        If m_lights.Exists(name) Then
            If m_off.Exists(name) Then 
                m_off.Remove(name)
            End If
            If m_pulse.Exists(name) Then 
                Exit Sub
            End If
            m_pulse.Add name, (new PulseState)(m_lights(name), profile, 0, m_pulseInterval, repeatCount, null)
        End If
    End Sub       

    Public Sub PulseWithState(pulse)
        
        If m_lights.Exists(pulse.Light) Then
            If m_off.Exists(pulse.Light) Then 
                m_off.Remove(pulse.Light)
            End If
            If m_pulse.Exists(pulse.Light) Then 
                Exit Sub
            End If
            m_pulse.Add name, pulse
        End If
    End Sub

    Public Sub LightLevel(light, lvl)
        If m_lights.Exists(light.name) Then
            m_lights(light.name).Level = lvl

            If m_seqs.Exists(light.name & "Blink") Then
                m_seqs(light.name & "Blink").Sequence = m_buildBlinkSeq(light.name, light.BlinkPattern)
            End If
        End If
    End Sub


    Public Sub AddShot(name, light, color)
        If m_lights.Exists(light.name) Then
            If m_seqs.Exists(name & light.name) Then
                m_seqs(name & light.name).Color = color
                m_seqRunners("lSeqRunner"&CStr(light.name)).AddItem m_seqs(name & light.name)
            Else
                Dim stateOn : stateOn = light.name&"|100"
                Dim stateOff : stateOff = light.name&"|0"
                Dim seq : Set seq = new LCSeq
                seq.Name = name
                seq.Sequence = Array(stateOn, stateOff,stateOn, stateOff)
                seq.Color = color
                seq.UpdateInterval = light.BlinkInterval
                seq.Repeat = True

                m_seqRunners("lSeqRunner"&CStr(light.name)).AddItem seq
                m_seqs.Add name & light.name, seq
            End If
            If m_on.Exists(light.name) Then
                m_on.Remove light.name
            End If
        End If
    End Sub

    Public Sub RemoveShot(name, light)
        If m_lights.Exists(light.name) And m_seqs.Exists(name & light.name) Then
            m_seqRunners("lSeqRunner"&CStr(light.name)).RemoveItem m_seqs(name & light.name)
            If IsNUll(m_seqRunners("lSeqRunner"&CStr(light.name)).CurrentItem) Then
            LightOff(light)
            End If
        End If
    End Sub

    Public Sub RemoveAllShots()
        Dim light
        For Each light in m_lights.Keys()
            m_seqRunners("lSeqRunner"&CStr(light)).RemoveAll
            AssignStateForFrame light, (new FrameState)(0, Null, m_lights(light).Idx)
        Next
    End Sub

    Public Sub RemoveShotsFromLight(light)
        If m_lights.Exists(light.name) Then
            m_seqRunners("lSeqRunner"&CStr(light.name)).RemoveAll   
            m_lightOff(light.name)  
        End If
    End Sub

    Public Sub Blink(light)
        If m_lights.Exists(light.name) Then

            If m_seqs.Exists(light.name & "Blink") Then
                m_seqs(light.name & "Blink").ResetInterval
                m_seqs(light.name & "Blink").CurrentIdx = 0
                m_seqRunners("lSeqRunner"&CStr(light.name)).AddItem m_seqs(light.name & "Blink")
            Else
                Dim seq : Set seq = new LCSeq
                seq.Name = light.name & "Blink"
                seq.Sequence = m_buildBlinkSeq(light.name, light.BlinkPattern)
                seq.Color = Null
                seq.UpdateInterval = light.BlinkInterval
                seq.Repeat = True

                m_seqRunners("lSeqRunner"&CStr(light.name)).AddItem seq
                m_seqs.Add light.name & "Blink", seq
            End If
            If m_on.Exists(light.name) Then
                m_on.Remove light.name
            End If
        End If
    End Sub

    Public Sub AddLightToBlinkGroup(group, light)
        If m_lights.Exists(light.name) Then

            If m_seqs.Exists(group & "BlinkGroup") Then

                Dim i, pattern, buff : buff = Array()
                pattern = m_seqs(group & "BlinkGroup").Pattern
                ReDim buff(Len(pattern)-1)
                For i = 0 To Len(pattern)-1
                    Dim lightInSeq, ii, p, buff2
                    buff2 = Array()
                    If Mid(pattern, i+1, 1) = 1 Then
                        p=1
                    Else
                        p=0
                    End If
                    ReDim buff2(UBound(m_seqs(group & "BlinkGroup").LightsInSeq)+1)
                    ii=0
                    For Each lightInSeq in m_seqs(group & "BlinkGroup").LightsInSeq
                        If p = 1 Then
                            buff2(ii) = lightInSeq & "|100"
                        Else
                            buff2(ii) = lightInSeq & "|0"
                        End If
                        ii = ii + 1
                    Next
                    If p = 1 Then
                        buff2(ii) = light.name & "|100"
                    Else
                        buff2(ii) = light.name & "|0"
                    End If
                    buff(i) = buff2
                Next
                m_seqs(group & "BlinkGroup").Sequence = buff
            Else
                Dim seq : Set seq = new LCSeq
                seq.Name = group & "BlinkGroup"
                seq.Sequence = Array(Array(light.name & "|100"), Array(light.name & "|0"))
                seq.Color = Null
                seq.Pattern = "10"
                seq.UpdateInterval = light.BlinkInterval
                seq.Repeat = True
                m_seqRunners.Add "lSeqRunner" & group & "BlinkGroup", new LCSeqRunner
                m_seqs.Add group & "BlinkGroup", seq
            End If
        End If
    End Sub

    Public Sub RemoveLightFromBlinkGroup(group, light)
        If m_lights.Exists(light.name) Then

            If m_seqs.Exists(group & "BlinkGroup") Then

                Dim i, pattern, buff : buff = Array()
                pattern = m_seqs(group & "BlinkGroup").Pattern
                ReDim buff(Len(pattern)-1)
                For i = 0 To Len(pattern)-1
                    Dim lightInSeq, ii, p, buff2
                    buff2 = Array()
                    If Mid(pattern, i+1, 1) = 1 Then
                        p=1
                    Else
                        p=0
                    End If
                    ReDim buff2(UBound(m_seqs(group & "BlinkGroup").LightsInSeq)-1)
                    ii=0
                    For Each lightInSeq in m_seqs(group & "BlinkGroup").LightsInSeq
                        If Not lightInSeq = light.name Then
                            If p = 1 Then
                                buff2(ii) = lightInSeq & "|100"
                            Else
                                buff2(ii) = lightInSeq & "|0"
                            End If
                            ii = ii + 1
                        End If
                    Next
                    buff(i) = buff2
                Next
                AssignStateForFrame light.name, (new FrameState)(0, Null, m_lights(light.name).Idx)
                m_seqs(group & "BlinkGroup").Sequence = buff
            End If
        End If
    End Sub

    Public Sub UpdateBlinkGroupPattern(group, pattern)
        If m_seqs.Exists(group & "BlinkGroup") Then

            Dim i, buff : buff = Array()
            m_seqs(group & "BlinkGroup").Pattern = pattern
            ReDim buff(Len(pattern)-1)
            For i = 0 To Len(pattern)-1
                Dim lightInSeq, ii, p, buff2
                buff2 = Array()
                If Mid(pattern, i+1, 1) = 1 Then
                    p=1
                Else
                    p=0
                End If
                ReDim buff2(UBound(m_seqs(group & "BlinkGroup").LightsInSeq))
                ii=0
                For Each lightInSeq in m_seqs(group & "BlinkGroup").LightsInSeq
                    If p = 1 Then
                        buff2(ii) = lightInSeq & "|100"
                    Else
                        buff2(ii) = lightInSeq & "|0"
                    End If
                    ii = ii + 1
                Next
                buff(i) = buff2
            Next
            m_seqs(group & "BlinkGroup").Sequence = buff
        End If
    End Sub

    Public Sub UpdateBlinkGroupInterval(group, interval)
        If m_seqs.Exists(group & "BlinkGroup") Then
            m_seqs(group & "BlinkGroup").UpdateInterval = interval
        End If 
    End Sub
    
    Public Sub StartBlinkGroup(group)
        If m_seqs.Exists(group & "BlinkGroup") Then
            AddLightSeq "lSeqRunner" & group & "BlinkGroup", m_seqs(group & "BlinkGroup")
        End If
    End Sub

    Public Sub StopBlinkGroup(group)
        If m_seqs.Exists(group & "BlinkGroup") Then
            RemoveLightSeq "lSeqRunner" & group & "BlinkGroup", m_seqs(group & "BlinkGroup")
        End If
    End Sub


    Public Function GetLightState(light)
        GetLightState = 0
        If(m_lights.Exists(light.name)) Then
            If m_on.Exists(light.name) Then
                GetLightState = 1
            Else
                If m_seqs.Exists(light.name & "Blink") Then
                    GetLightState = 2
                End If
            End If
        End If
    End Function

    Public Function IsShotLit(name, light)
        IsShotLit = False
        If(m_lights.Exists(light.name)) Then
            If m_seqRunners("lSeqRunner"&CStr(light.name)).HasSeq(name) Then
                IsShotLit = True
            End If
        End If
    End Function

    Public Sub CreateSeqRunner(name)
        If m_seqRunners.Exists(name) Then
            Exit Sub
        End If
        Dim seqRunner : Set seqRunner = new LCSeqRunner
        seqRunner.Name = name
        m_seqRunners.Add name, seqRunner
    End Sub

    Private Sub CreateOverrideSeqRunner(name)
        If m_seqOverrideRunners.Exists(name) Then
            Exit Sub
        End If
        Dim seqRunner : Set seqRunner = new LCSeqRunner
        seqRunner.Name = name
        m_seqOverrideRunners.Add name, seqRunner
    End Sub

    Public Sub AddLightSeq(lcSeqRunner, lcSeq)
        If Not m_seqRunners.Exists(lcSeqRunner) Then
            Exit Sub
        End If

        m_seqRunners(lcSeqRunner).AddItem lcSeq
    End Sub

    Public Sub RemoveLightSeq(lcSeqRunner, lcSeq)
        If Not m_seqRunners.Exists(lcSeqRunner) Then
            Exit Sub
        End If

        Dim light
        For Each light in lcSeq.LightsInSeq
            If(m_lights.Exists(light)) Then
                    AssignStateForFrame light, (new FrameState)(0, Null, m_lights(light).Idx)
            End If
        Next

        m_seqRunners(lcSeqRunner).RemoveItem lcSeq
    End Sub

    Public Sub RemoveAllLightSeq(lcSeqRunner)
        If Not m_seqRunners.Exists(lcSeqRunner) Then
            Exit Sub
        End If
        Dim lcSeqKey, light, seqs, lcSeq
        Set seqs = m_seqRunners(lcSeqRunner).Items()
        For Each lcSeqKey in seqs.Keys()
            Set lcSeq = seqs(lcSeqKey)
            For Each light in lcSeq.LightsInSeq
                If(m_lights.Exists(light)) Then
                    AssignStateForFrame light, (new FrameState)(0, Null, m_lights(light).Idx)
                End If
            Next
        Next

        m_seqRunners(lcSeqRunner).RemoveAll
    End Sub

    Public Sub AddTableLightSeq(name, lcSeq)
        CreateOverrideSeqRunner(name)

        Dim seqOverride, hasOverride
        hasOverride = False
        For Each seqOverride In m_seqOverrideRunners.Keys()
            If Not IsNull(m_seqOverrideRunners(seqOverride).CurrentItem) Then
                hasOverride = True
            End If
        Next
        If hasOverride = False Then
            Dim light
            For Each light in m_lights.Keys()
                AssignStateForFrame light, (new FrameState)(0, Null, m_lights(light).Idx)
            Next
        End If
        m_seqOverrideRunners(name).AddItem lcSeq
    End Sub

    Public Sub RemoveTableLightSeq(name, lcSeq)
        If Not m_seqOverrideRunners.Exists(name) Then
            Exit Sub
        End If
        m_seqOverrideRunners(name).RemoveItem lcSeq
        Dim seqOverride, hasOverride
        hasOverride = False
        For Each seqOverride In m_seqOverrideRunners.Keys()
            If Not IsNull(m_seqOverrideRunners(seqOverride).CurrentItem) Then
                hasOverride = True
            End If
        Next
        If hasOverride = False Then
            Dim light
            For Each light in m_lights.Keys()
                AssignStateForFrame light, (new FrameState)(0, Null, m_lights(light).Idx)
            Next
        End If
    End Sub

    Public Sub RemoveAllTableLightSeqs()
        Dim light, runner
        For Each runner in m_seqOverrideRunners.Keys()
            m_seqOverrideRunners(runner).RemoveAll()
        Next
        For Each light in m_lights.Keys()
            AssignStateForFrame light, (new FrameState)(0, Null, m_lights(light).Idx)
        Next
    End Sub

Public Sub SyncLightMapColors()
        dim light,lm
        For Each light in m_lights.Keys()
            If m_lightmaps.Exists(light) Then
                For Each lm in m_lightmaps(light)
                    dim color : color = m_lights(light).Color
                    If not IsNull(lm) Then
                        lm.Color = color(0)
                    End If
                Next
            End If
        Next
    End Sub

    Public Sub SyncWithVpxLights(lightSeq)
        m_vpxLightSyncCollection = ColToArray(eval(lightSeq.collection))
        m_vpxLightSyncRunning = True
        m_tableSeqSpeed = Null
        m_tableSeqOffset = 0
        m_tableSeqDirection = Null
    End Sub

    Public Sub StopSyncWithVpxLights()
        m_vpxLightSyncRunning = False
        m_vpxLightSyncClear = True
        m_tableSeqColor = Null
        m_tableSeqFadeUp = Null
        m_tableSeqFadeDown = Null
        m_tableSeqSpeed = Null
        m_tableSeqOffset = 0
        m_tableSeqDirection = Null
    End Sub

    Public Sub SetVpxSyncLightColor(color)
        m_tableSeqColor = color
    End Sub
    
    Public Sub SetVpxSyncLightsPalette(palette, direction, speed)
        m_tableSeqColor = palette
        Select Case direction:
            Case "BottomToTop": 
                m_tableSeqDirection = m_coordsY
                m_tableSeqColor = ReverseArray(palette)
            Case "TopToBottom": 
                m_tableSeqDirection = m_coordsY
            Case "RightToLeft": 
                m_tableSeqDirection = m_coordsX
            Case "LeftToRight": 
                m_tableSeqDirection = m_coordsX
                m_tableSeqColor = ReverseArray(palette)       
            Case "RadialOut": 
                m_tableSeqDirection = m_radii      
            Case "RadialIn": 
                m_tableSeqDirection = m_radii
                m_tableSeqColor = ReverseArray(palette) 
            Case "Clockwise": 
                m_tableSeqDirection = m_angles
            Case "AntiClockwise": 
                m_tableSeqDirection = m_angles
                m_tableSeqColor = ReverseArray(palette) 
        End Select  

        m_tableSeqSpeed = speed
    End Sub

    Public Sub SetTableSequenceFade(fadeUp, fadeDown)
        m_tableSeqFadeUp = fadeUp
        m_tableSeqFadeDown = fadeDown
    End Sub

    Public Function GetLightIdx(light)
        dim syncLight : syncLight = Null
        If m_lights.Exists(light.name) Then
            'found a light
            Set syncLight = m_lights(light.name)
        End If
        If Not IsNull(syncLight) Then
            'Found a light to sync.
            GetLightIdx = lightsToLeds(syncLight.Idx)
        Else
            GetLightIdx = Null
        End If
        
    End Function

    Private Function m_buildBlinkSeq(lightName, pattern)
        Dim i, buff : buff = Array()
        ReDim buff(Len(pattern)-1)
        For i = 0 To Len(pattern)-1
            
            If Mid(pattern, i+1, 1) = 1 Then
                buff(i) = lightName & "|100"
            Else
                buff(i) = lightName & "|0"
            End If
        Next
        m_buildBlinkSeq=buff
    End Function

    Private Function GetTmpLight(idx)
        If IsArray(Lights(idx) ) Then	'if array
            Set GetTmpLight = Lights(idx)(0)
        Else
            Set GetTmpLight = Lights(idx)
        End If
    End Function

    Public Sub ResetLights()
        Dim light
        For Each light in m_lights.Keys()
            m_seqRunners("lSeqRunner"&CStr(light)).RemoveAll
            m_lightOff(light) 
            AssignStateForFrame light, (new FrameState)(0, Null, m_lights(light).Idx)
        Next
        RemoveAllTableLightSeqs()
        Dim k
        For Each k in m_seqRunners.Keys()
            Dim lsRunner: Set lsRunner = m_seqRunners(k)
            lsRunner.RemoveAll
        Next

    End Sub

    Public Sub PauseMainLights
        If m_pauseMainLights = False Then
            m_pauseMainLights = True
            m_pausedLights.RemoveAll
            Dim pon
            Set pon = CreateObject("Scripting.Dictionary")
            Dim poff : Set poff = CreateObject("Scripting.Dictionary")
            Dim ppulse : Set ppulse = CreateObject("Scripting.Dictionary")
            Dim pseqs : Set pseqs = CreateObject("Scripting.Dictionary")
            Dim lightProps : Set lightProps = CreateObject("Scripting.Dictionary")
            'Add State in
            Dim light, item
            For Each item in m_on.Keys()
                pon.add item, m_on(Item)
            Next
            For Each item in m_off.Keys()
                poff.add item, m_off(Item)
            Next
            For Each item in m_pulse.Keys()
                ppulse.add item, m_pulse(Item)
            Next
            For Each item in m_seqRunners.Keys()
                dim tmpSeq : Set tmpSeq = new LCSeqRunner
                dim seqItem
                For Each seqItem in m_seqRunners(Item).Items.Items()
                    tmpSeq.AddItem seqItem
                Next
                tmpSeq.CurrentItemIdx = m_seqRunners(Item).CurrentItemIdx
                pseqs.add item, tmpSeq
            Next
            
            Dim savedProps(1,3)
            
            For Each light in m_lights.Keys()
                    
                savedProps(0,0) = m_lights(light).Color
                savedProps(0,1) = m_lights(light).Level
                If m_seqs.Exists(light & "Blink") Then
                    savedProps(0,2) = m_seqs.Item(light & "Blink").UpdateInterval
                Else
                    savedProps(0,2) = Empty
                End If
                lightProps.add light, savedProps
            Next
            m_pausedLights.Add "on", pon
            m_pausedLights.Add "off", poff
            m_pausedLights.Add "pulse", ppulse
            m_pausedLights.Add "runners", pseqs
            m_pausedLights.Add "lightProps", lightProps
            m_on.RemoveAll
            m_off.RemoveAll
            m_pulse.RemoveAll
            For Each item in m_seqRunners.Items()
                item.removeAll
            Next			
        End If
    End Sub

    Public Sub ResumeMainLights
        If m_pauseMainLights = True Then
            m_pauseMainLights = False
            m_on.RemoveAll
            m_off.RemoveAll
            m_pulse.RemoveAll
            Dim light, item
            For Each light in m_lights.Keys()
                AssignStateForFrame light, (new FrameState)(0, Null, m_lights(light).Idx)
            Next
            For Each item in m_seqRunners.Items()
                item.removeAll
            Next
            'Add State in
            For Each item in m_pausedLights("on").Keys()
                m_on.add item, m_pausedLights("on")(Item)
            Next
            For Each item in m_pausedLights("off").Keys()
                m_off.add item, m_pausedLights("off")(Item)
            Next			
            For Each item in m_pausedLights("pulse").Keys()
                m_pulse.add item, m_pausedLights("pulse")(Item)
            Next
            For Each item in m_pausedLights("runners").Keys()
                
                
                Set m_seqRunners(Item) = m_pausedLights("runners")(Item)
            Next
            For Each item in m_pausedLights("lightProps").Keys()
                LightColor Eval(Item), m_pausedLights("lightProps")(Item)(0,0)
                LightLevel Eval(Item), m_pausedLights("lightProps")(Item)(0,1)
                If Not IsEmpty(m_pausedLights("lightProps")(Item)(0,2)) Then
                    UpdateBlinkInterval Eval(Item), m_pausedLights("lightProps")(Item)(0,2)
                End If
            Next			
            m_pausedLights.RemoveAll
        End If
    End Sub

    Public Sub Update()

        m_frameTime = gametime - m_initFrameTime : m_initFrameTime = gametime
        Dim x
        Dim lk
        dim color
        dim idx
        Dim lightKey
        Dim lcItem
        Dim tmpLight
        Dim seqOverride, hasOverride
        hasOverride = False
        For Each seqOverride In m_seqOverrideRunners.Keys()
            If Not IsNull(m_seqOverrideRunners(seqOverride).CurrentItem) Then
                RunLightSeq m_seqOverrideRunners(seqOverride)
                hasOverride = True
            End If
        Next
        If hasOverride = False Then
        
            If HasKeys(m_on) Then   
                For Each lightKey in m_on.Keys()
                    Set lcItem = m_on(lightKey)
                    AssignStateForFrame lightKey, (new FrameState)(lcItem.level, m_on(lightKey).Color, m_on(lightKey).Idx)
                Next
            End If

            If HasKeys(m_off) Then
                For Each lightKey in m_off.Keys()
                    Set lcItem = m_off(lightKey)
                    AssignStateForFrame lightKey, (new FrameState)(0, Null, lcItem.Idx)
                Next
            End If

        
            If HasKeys(m_seqRunners) Then
                Dim k
                For Each k in m_seqRunners.Keys()
                    Dim lsRunner: Set lsRunner = m_seqRunners(k)
                    If Not IsNull(lsRunner.CurrentItem) Then
                            RunLightSeq lsRunner
                    End If
                Next
            End If

            If HasKeys(m_pulse) Then   
                For Each lightKey in m_pulse.Keys()
                    Dim pulseColor : pulseColor = m_pulse(lightKey).Color
                    If IsNull(pulseColor) Then
                        AssignStateForFrame lightKey, (new FrameState)(m_pulse(lightKey).PulseAt(m_pulse(lightKey).idx), m_pulse(lightKey).Light.Color, m_pulse(lightKey).light.Idx)
                    Else
                        AssignStateForFrame lightKey, (new FrameState)(m_pulse(lightKey).PulseAt(m_pulse(lightKey).idx), m_pulse(lightKey).Color, m_pulse(lightKey).light.Idx)
                    End If						
                    
                    Dim pulseUpdateInt : pulseUpdateInt = m_pulse(lightKey).interval - m_frameTime
                    Dim pulseIdx : pulseIdx = m_pulse(lightKey).idx
                    If pulseUpdateInt <= 0 Then
                        pulseUpdateInt = m_pulseInterval
                        pulseIdx = pulseIdx + 1
                    End If
                    
                    Dim pulses : pulses = m_pulse(lightKey).pulses
                    Dim pulseCount : pulseCount = m_pulse(lightKey).Cnt
                    
                    
                    If pulseIdx > UBound(m_pulse(lightKey).pulses) Then
                        m_pulse.Remove lightKey    
                        If pulseCount > 0 Then
                            pulseCount = pulseCount - 1
                            pulseIdx = 0
                            m_pulse.Add lightKey, (new PulseState)(m_lights(lightKey),pulses, pulseIdx, pulseUpdateInt, pulseCount, pulseColor)
                        End If
                    Else
                        m_pulse.Remove lightKey
                        m_pulse.Add lightKey, (new PulseState)(m_lights(lightKey),pulses, pulseIdx, pulseUpdateInt, pulseCount, pulseColor)
                    End If
                Next
            End If

            If m_vpxLightSyncRunning = True Then
                Dim lx
                If Not IsNull(m_vpxLightSyncCollection) Then
                    For Each lx in m_vpxLightSyncCollection
                        'sync each light being ran by the vpx LS
                        dim syncLight : syncLight = Null
                        If m_lights.Exists(lx.name) Then
                            'found a light
                            Set syncLight = m_lights(lx.name)
                        End If
                        If Not IsNull(syncLight) Then
                            'Found a light to sync.
                            
                            Dim lightState

                            If IsNull(m_tableSeqColor) Then
                                color = syncLight.Color
                            Else
                                If Not IsArray(m_tableSeqColor) Then
                                    color = Array(m_tableSeqColor, Null)
                                Else
                                    
                                    If Not IsNull(m_tableSeqSpeed) And Not m_tableSeqSpeed = 0 Then

                                        Dim colorPalleteIdx : colorPalleteIdx = IncrementUInt8(m_tableSeqDirection(lightsToLeds(syncLight.Idx)),m_tableSeqOffset)
                                        If gametime mod m_tableSeqSpeed = 0 Then
                                            m_tableSeqOffset = m_tableSeqOffset + 1
                                            If m_tableSeqOffset > 255 Then
                                                m_tableSeqOffset = 0
                                            End If	
                                        End If
                                        If colorPalleteIdx < 0 Then 
                                            colorPalleteIdx = 0
                                        End If
                                        color = Array(m_TableSeqColor(Round(colorPalleteIdx)), Null)
                                        'color = syncLight.Color
                                    Else
                                        color = Array(m_TableSeqColor(m_tableSeqDirection(lightsToLeds(syncLight.Idx))), Null)
                                    End If
                                    
                                End If
                            End If
                        
                    
                            AssignStateForFrame syncLight.name, (new FrameState)(lx.GetInPlayState*100,color, syncLight.Idx)                     
                        End If
                    Next
                End If
            End If

            If m_vpxLightSyncClear = True Then  
                If Not IsNull(m_vpxLightSyncCollection) Then
                    For Each lk in m_vpxLightSyncCollection
                        'sync each light being ran by the vpx LS
                        dim syncClearLight : syncClearLight = Null
                        If m_lights.Exists(lk.name) Then
                            'found a light
                            Set syncClearLight = m_lights(lk.name)
                        End If
                        If Not IsNull(syncClearLight) Then
                            AssignStateForFrame syncClearLight.name, (new FrameState)(0, Null, syncClearLight.idx) 
                        End If
                    Next
                End If
            
                m_vpxLightSyncClear = False
            End If
        End If
        

        If HasKeys(m_currentFrameState) Then
            
            Dim frameStateKey
            For Each frameStateKey in m_currentFrameState.Keys()
                idx = m_currentFrameState(frameStateKey).idx
                
                Dim newColor : newColor = m_currentFrameState(frameStateKey).colors
                Dim bUpdate

                If Not IsNull(newColor) Then
                    'Check current color is the new color coming in, if not, set the new color.
                    
                    Set tmpLight = GetTmpLight(idx)

                    Dim c, cf
                    c = newColor(0)
                    cf= newColor(1)

                    If Not IsNull(c) Then
                        If Not CStr(tmpLight.Color) = CStr(c) Then
                            bUpdate = True
                        End If
                    End If

                    If Not IsNull(cf) Then
                        If Not CStr(tmpLight.ColorFull) = CStr(cf) Then
                            bUpdate = True
                        End If
                    End If
                End If

               
                Dim lm
                If IsArray(Lights(idx)) Then
                    For Each x in Lights(idx)
                        If bUpdate Then 
                            If Not IsNull(c) Then
                                x.color = c
                            End If
                            If Not IsNull(cf) Then
                                x.colorFull = cf
                            End If
                            If m_lightmaps.Exists(x.Name) Then
                                For Each lm in m_lightmaps(x.Name)
                                    If Not IsNull(lm) Then
                                        lm.Color = c
                                    End If
                                Next
                            End If
                        End If
                        x.State = m_currentFrameState(frameStateKey).level/100
                    Next
                Else
                    If bUpdate Then    
                        If Not IsNull(c) Then
                            Lights(idx).color = c
                        End If
                        If Not IsNull(cf) Then
                            Lights(idx).colorFull = cf
                        End If
                        If m_lightmaps.Exists(Lights(idx).Name) Then
                            For Each lm in m_lightmaps(Lights(idx).Name)
                                If Not IsNull(lm) Then
                                    lm.Color = c
                                End If
                            Next
                        End If
                    End If
                    Lights(idx).State = m_currentFrameState(frameStateKey).level/100
                End If
            Next
        End If
        m_currentFrameState.RemoveAll
        m_off.RemoveAll

    End Sub

    Private Function HexToInt(hex)
        HexToInt = CInt("&H" & hex)
    End Function

    Function RGBToHex(r, g, b)
        RGBToHex = Right("0" & Hex(r), 2) & _
            Right("0" & Hex(g), 2) & _
            Right("0" & Hex(b), 2)
    End Function

    Function FadeRGB(light, color1, color2, steps)

    
        Dim r1, g1, b1, r2, g2, b2
        Dim i
        Dim r, g, b
        color1 = clng(color1)
        color2 = clng(color2)
        ' Extract RGB values from the color integers
        r1 = color1 Mod 256
        g1 = (color1 \ 256) Mod 256
        b1 = (color1 \ (256 * 256)) Mod 256

        r2 = color2 Mod 256
        g2 = (color2 \ 256) Mod 256
        b2 = (color2 \ (256 * 256)) Mod 256

        ' Resize the output array
        ReDim outputArray(steps - 1)

        ' Generate the fade
        For i = 0 To steps - 1
            ' Calculate RGB values for this step
            r = r1 + (r2 - r1) * i / (steps - 1)
            g = g1 + (g2 - g1) * i / (steps - 1)
            b = b1 + (b2 - b1) * i / (steps - 1)

            ' Convert RGB to hex and add to output
            outputArray(i) = light & "|100|" & RGBToHex(CInt(r), CInt(g), CInt(b))
        Next
        FadeRGB = outputArray
    End Function

    Public Function CreateColorPalette(startColor, endColor, steps)
    Dim colors()
    ReDim colors(steps)
    
    Dim startRed, startGreen, startBlue, endRed, endGreen, endBlue
    startRed = HexToInt(Left(startColor, 2))
    startGreen = HexToInt(Mid(startColor, 3, 2))
    startBlue = HexToInt(Right(startColor, 2))
    endRed = HexToInt(Left(endColor, 2))
    endGreen = HexToInt(Mid(endColor, 3, 2))
    endBlue = HexToInt(Right(endColor, 2))
    
    Dim redDiff, greenDiff, blueDiff
    redDiff = endRed - startRed
    greenDiff = endGreen - startGreen
    blueDiff = endBlue - startBlue
    
    Dim i
    For i = 0 To steps
        Dim red, green, blue
        red = startRed + (redDiff * (i / steps))
        green = startGreen + (greenDiff * (i / steps))
        blue = startBlue + (blueDiff * (i / steps))
        colors(i) = RGB(red,green,blue)'IntToHex(red, 2) & IntToHex(green, 2) & IntToHex(blue, 2)
    Next
    
    CreateColorPalette = colors
    End Function


    Function CreateColorPaletteWithStops(startColor, endColor, stopPositions, stopColors)

    Dim colors(255)

    Dim fStop : fStop = CreateColorPalette(startColor, stopColors(0), stopPositions(0))
    Dim i, istep
    For i = 0 to stopPositions(0)
        colors(i) = fStop(i)
    Next
    For i = 1 to Ubound(stopColors)
        Dim stopStep : stopStep = CreateColorPalette(stopColors(i-1), stopColors(i), stopPositions(i))
        Dim ii
    ' MsgBox(stopPositions(i) - stopPositions(i-1))
        istep = 0
        For ii = stopPositions(i-1)+1 to stopPositions(i)
        '  MsgBox(ii)
        colors(ii) = stopStep(iStep)
        iStep = iStep + 1
        Next
    Next
    ' MsgBox("Here")
    Dim eStop : eStop = CreateColorPalette(stopColors(UBound(stopColors)), endColor, 255-stopPositions(UBound(stopPositions)))
    'MsgBox(UBound(eStop))
    iStep = 0
    For i = 255-(255-stopPositions(UBound(stopPositions))) to 254
        colors(i) = eStop(iStep)
        iStep = iStep + 1
    Next

    CreateColorPaletteWithStops = colors
    End Function

    Private Function HasKeys(o)
        If Ubound(o.Keys())>-1 Then
            HasKeys = True
        Else
            HasKeys = False
        End If
    End Function

    Private Sub RunLightSeq(seqRunner)

        Dim lcSeq: Set lcSeq = seqRunner.CurrentItem
        dim lsName, isSeqEnd
        If UBound(lcSeq.Sequence)<lcSeq.CurrentIdx Then
            isSeqEnd = True
        Else
            isSeqEnd = False
        End If

        dim lightInSeq
        For each lightInSeq in lcSeq.LightsInSeq
        
            If isSeqEnd Then

                

            'Needs a guard here for something, but i've forgotten. 
            'I remember: Only reset the light if there isn't frame data for the light. 
            'e.g. a previous seq has affected the light, we don't want to clear that here on this frame
                If m_lights.Exists(lightInSeq) = True AND NOT m_currentFrameState.Exists(lightInSeq) Then
                AssignStateForFrame lightInSeq, (new FrameState)(0, Null, m_lights(lightInSeq).Idx)
                End If
            Else
                


                If m_currentFrameState.Exists(lightInSeq) Then

                    
                    'already frame data for this light.
                    'replace with the last known state from this seq
                    If Not IsNull(lcSeq.LastLightState(lightInSeq)) Then
                        AssignStateForFrame lightInSeq, lcSeq.LastLightState(lightInSeq)
                    End If
                End If

            End If
        Next

        If isSeqEnd Then
            lcSeq.CurrentIdx = 0
            seqRunner.NextItem()
        End If

        If Not IsNull(seqRunner.CurrentItem) Then
            Dim framesRemaining, seq, color
            Set lcSeq = seqRunner.CurrentItem
            seq = lcSeq.Sequence
            

            Dim name
            Dim ls, x
            If IsArray(seq(lcSeq.CurrentIdx)) Then
                For x = 0 To UBound(seq(lcSeq.CurrentIdx))
                    lsName = Split(seq(lcSeq.CurrentIdx)(x),"|")
                    name = lsName(0)
                    If m_lights.Exists(name) Then
                        Set ls = m_lights(name)
                        
                        color = lcSeq.Color

                        If IsNull(color) Then
                            color = ls.Color
                        End If
                        
                        If Ubound(lsName) = 2 Then
                            If lsName(2) = "" Then
                                AssignStateForFrame name, (new FrameState)(lsName(1), color, ls.Idx)
                            Else
                                AssignStateForFrame name, (new FrameState)(lsName(1), Array( RGB( HexToInt(Left(lsName(2), 2)), HexToInt(Mid(lsName(2), 3, 2)), HexToInt(Right(lsName(2), 2)) ), RGB(0,0,0)), ls.Idx)
                            End If
                        Else
                            AssignStateForFrame name, (new FrameState)(lsName(1), color, ls.Idx)
                        End If
                        lcSeq.SetLastLightState name, m_currentFrameState(name) 
                    End If
                Next       
            Else
                lsName = Split(seq(lcSeq.CurrentIdx),"|")
                name = lsName(0)
                If m_lights.Exists(name) Then
                    Set ls = m_lights(name)
                    
                    color = lcSeq.Color
                    If IsNull(color) Then
                        color = ls.Color
                    End If
                    If Ubound(lsName) = 2 Then
                        If lsName(2) = "" Then
                            AssignStateForFrame name, (new FrameState)(lsName(1), color, ls.Idx)
                        Else
                            AssignStateForFrame name, (new FrameState)(lsName(1), Array( RGB( HexToInt(Left(lsName(2), 2)), HexToInt(Mid(lsName(2), 3, 2)), HexToInt(Right(lsName(2), 2)) ), RGB(0,0,0)), ls.Idx)
                        End If
                    Else
                        AssignStateForFrame name, (new FrameState)(lsName(1), color, ls.Idx)
                    End If
                    lcSeq.SetLastLightState name, m_currentFrameState(name) 
                End If
            End If

            framesRemaining = lcSeq.Update(m_frameTime)
            If framesRemaining < 0 Then
                lcSeq.ResetInterval()
                lcSeq.NextFrame()
            End If
            
        End If
    End Sub

End Class

Class FrameState
    Private m_level, m_colors, m_idx

    Public Property Get Level(): Level = m_level: End Property
    Public Property Let Level(input): m_level = input: End Property

    Public Property Get Colors(): Colors = m_colors: End Property
    Public Property Let Colors(input): m_colors = input: End Property

    Public Property Get Idx(): Idx = m_idx: End Property
    Public Property Let Idx(input): m_idx = input: End Property

    Public default function init(level, colors, idx)
        m_level = level
        m_colors = colors
        m_idx = idx 

        Set Init = Me
    End Function

    Public Function ColorAt(idx)
        ColorAt = m_colors(idx) 
    End Function
End Class

Class PulseState
    Private m_light, m_pulses, m_idx, m_interval, m_cnt, m_color

    Public Property Get Light(): Set Light = m_light: End Property
    Public Property Let Light(input): Set m_light = input: End Property

    Public Property Get Pulses(): Pulses = m_pulses: End Property
    Public Property Let Pulses(input): m_pulses = input: End Property

    Public Property Get Idx(): Idx = m_idx: End Property
    Public Property Let Idx(input): m_idx = input: End Property

    Public Property Get Interval(): Interval = m_interval: End Property
    Public Property Let Interval(input): m_interval = input: End Property

    Public Property Get Cnt(): Cnt = m_cnt: End Property
    Public Property Let Cnt(input): m_cnt = input: End Property

    Public Property Get Color(): Color = m_color: End Property
    Public Property Let Color(input): m_color = input: End Property		

    Public default function init(light, pulses, idx, interval, cnt, color)
        Set m_light = light
        m_pulses = pulses
        'debug.Print(Join(Pulses))
        m_idx = idx 
        m_interval = interval
        m_cnt = cnt
        m_color = color

        Set Init = Me
    End Function

    Public Function PulseAt(idx)
        PulseAt = m_pulses(idx) 
    End Function
End Class

Class LCItem
    
    Private m_Idx, m_State, m_blinkSeq, m_color, m_name, m_level, m_x, m_y

        Public Property Get Idx()
            Idx=m_Idx
        End Property

        Public Property Get Color()
            Color=m_color
        End Property

        Public Property Let Color(input)
            If IsNull(input) Then
                m_Color = Null
            Else
                If Not IsArray(input) Then
                    input = Array(input, null)
                End If
                m_Color = input
            End If
        End Property

        Public Property Let Level(input)
            m_level = input
        End Property

        Public Property Get Level()
            Level=m_level
        End Property

        Public Property Get Name()
            Name=m_name
        End Property

        Public Property Get X()
            X=m_x
        End Property

        Public Property Get Y()
            Y=m_y
        End Property

        Public Property Get Row()
            Row=Round(m_x/40)
        End Property

        Public Property Get Col()
            Col=Round(m_y/40)
        End Property

        Public Sub Init(idx, intervalMs, color, name, x, y)
            m_Idx = idx
            If Not IsArray(color) Then
                m_color = Array(color, null)
            Else
                m_color = color
            End If
            m_name = name
            m_level = 100
            m_x = x
            m_y = y
        End Sub

End Class

Class LCSeq
    
    Private m_currentIdx, m_sequence, m_name, m_image, m_color, m_updateInterval, m_Frames, m_repeat, m_lightsInSeq, m_lastLightStates, m_palette, m_pattern

    Public Property Get CurrentIdx()
        CurrentIdx=m_currentIdx
    End Property

    Public Property Let CurrentIdx(input)
        m_lastLightStates.RemoveAll()
        m_currentIdx = input
    End Property

    Public Property Get LightsInSeq()
        LightsInSeq=m_lightsInSeq.Keys()
    End Property

    Public Property Get Sequence()
        Sequence=m_sequence
    End Property
    
    Public Property Let Sequence(input)
        m_sequence = input
        dim item, light, lightItem
        m_lightsInSeq.RemoveAll
        for each item in input
            If IsArray(item) Then
                for each light in item
                    lightItem = Split(light,"|")
                    If Not m_lightsInSeq.Exists(lightItem(0)) Then
                        m_lightsInSeq.Add lightItem(0), True
                    End If    
                next
            Else
                lightItem = Split(item,"|")
                If Not m_lightsInSeq.Exists(lightItem(0)) Then
                    m_lightsInSeq.Add lightItem(0), True
                End If
            End If
        next
    End Property

    Public Property Get LastLightState(light)
        If m_lastLightStates.Exists(light) Then
            dim c : Set c = m_lastLightStates(light)
            Set LastLightState = c
        Else
            LastLightState = Null
        End If
    End Property

    Public Property Let LastLightState(light, input)
        If m_lastLightStates.Exists(light) Then
            m_lastLightStates.Remove light
        End If
        If input.level > 0 Then
            m_lastLightStates.Add light, input
        End If
    End Property

    Public Sub SetLastLightState(light, input)	
        If m_lastLightStates.Exists(light) Then	
            m_lastLightStates.Remove light	
        End If	
        If input.level > 0 Then	
                m_lastLightStates.Add light, input	
        End If	
    End Sub

    Public Property Get Color()
        Color=m_color
    End Property
    
    Public Property Let Color(input)
        If IsNull(input) Then
            m_Color = Null
        Else
            If Not IsArray(input) Then
                input = Array(input, null)
            End If
            m_Color = input
        End If
    End Property

    Public Property Get Palette()
        Palette=m_palette
    End Property
    
    Public Property Let Palette(input)
        If IsNull(input) Then
            m_palette = Null
        Else
            If Not IsArray(input) Then
                m_palette = Null
            Else
                m_palette = input
            End If
        End If
    End Property

    Public Property Get Name()
        Name=m_name
    End Property
    
    Public Property Let Name(input)
        m_name = input
    End Property        

    Public Property Get UpdateInterval()
        UpdateInterval=m_updateInterval
    End Property

    Public Property Let UpdateInterval(input)
        m_updateInterval = input
        'm_Frames = input
    End Property

    Public Property Get Repeat()
        Repeat=m_repeat
    End Property

    Public Property Let Repeat(input)
        m_repeat = input
    End Property

    Public Property Get Pattern()
        Pattern=m_pattern
    End Property

    Public Property Let Pattern(input)
        m_pattern = input
    End Property    

    Private Sub Class_Initialize()
        m_currentIdx = 0
        m_color = Array(Null, Null)
        m_updateInterval = 180
        m_repeat = False
        m_Frames = 180
        m_pattern = Null
        Set m_lightsInSeq = CreateObject("Scripting.Dictionary")
        Set m_lastLightStates = CreateObject("Scripting.Dictionary")
    End Sub

    Public Property Get Update(framesPassed)
        m_Frames = m_Frames - framesPassed
        Update = m_Frames
    End Property

    Public Sub NextFrame()
        m_currentIdx = m_currentIdx + 1
    End Sub

    Public Sub ResetInterval()
        m_Frames = m_updateInterval
        Exit Sub
    End Sub

End Class

Class LCSeqRunner
    
    Private m_name, m_items,m_currentItemIdx

    Public Property Get Name()
        Name=m_name
    End Property
    
    Public Property Let Name(input)
        m_name = input
    End Property

    Public Property Get Items()
        Set Items = m_items
    End Property

    Public Property Get CurrentItemIdx()
        CurrentItemIdx = m_currentItemIdx
    End Property

    Public Property Let CurrentItemIdx(input)
        m_currentItemIdx = input
    End Property

    Public Property Get CurrentItem()
        Dim items: items = m_items.Items()
        If m_currentItemIdx > UBound(items) Then
            m_currentItemIdx = 0
        End If
        If UBound(items) = -1 Then       
            CurrentItem  = Null
        Else
            Set CurrentItem = items(m_currentItemIdx)                
        End If
    End Property

    Private Sub Class_Initialize()    
        Set m_items = CreateObject("Scripting.Dictionary")
        m_currentItemIdx = 0
    End Sub

    Public Sub AddItem(item)
        If Not IsNull(item) Then
            If Not m_items.Exists(item.Name) Then
                m_items.Add item.Name, item
            End If
        End If
    End Sub

    Public Sub RemoveAll()
        Dim item
        For Each item in m_items.Keys()
            m_items(item).ResetInterval
            m_items(item).CurrentIdx = 0
            m_items.Remove item
        Next
    End Sub

    Public Sub RemoveItem(item)
        If Not IsNull(item) Then
            If m_items.Exists(item.Name) Then
                    item.ResetInterval
                    item.CurrentIdx = 0
                    m_items.Remove item.Name
            End If
        End If
    End Sub

    Public Sub NextItem()
        Dim items: items = m_items.Items
        If items(m_currentItemIdx).Repeat = False Then
            RemoveItem(items(m_currentItemIdx))
        Else
            m_currentItemIdx = m_currentItemIdx + 1
        End If
        
        If m_currentItemIdx > UBound(m_items.Items) Then   
            m_currentItemIdx = 0
        End If
    End Sub

    Public Function HasSeq(name)
        If m_items.Exists(name) Then
            HasSeq = True
        Else
            HasSeq = False
        End If
    End Function

    End Class





' ** Reset All **
Dim Healer(6)

'things to Reset
ResetLights
Sub ResetLights


	dim x
' fixing make collection all inserts
' For Each x In AllLights : x.state = 0 : Next

	for x = 0 To 6 : Healer(x) = 0 : Next			' Healer    might not reset these Lights in missions
	LeftOutlane.state = 0
	LeftInlane.state = 0
	LeftInlane2.state = 0
	RightOutlane.state = 0
	RightInlane.state = 0
	RightInlane2.state = 0
End Sub


' ** TRIGGERS **
' jp under left fliupper
' outsidemission 1 mb then 10-15 sec BallSaver
' in mission addaball then pooints
' save position outside mission and turn off when missions Start
' ask flux for blinkieraction

Sub CheckHealer
	dim x

	addscore SCORE_LANES

	If Healer(1) + Healer(2) + Healer(3) + Healer(4) + Healer(5) + Healer(6) = 6 then
		LeftOutlane.state = 0
		lightCtrl.Pulse LeftOutlane,8
		LeftInlane.state = 0
		lightCtrl.Pulse LeftInlane,8
		LeftInlane2.state = 0
		lightCtrl.Pulse LeftInlane2,8
		RightOutlane.state = 0
		lightCtrl.Pulse RightOutlane,8
		RightInlane.state = 0
		lightCtrl.Pulse RightInlane,8
		RightInlane.state = 0
		lightCtrl.Pulse RightInlane,8
		For x = 1 to 6 : Healer(x) = 0 : Next
		
		if bMultiBallMode = False And bConnie Then 
			bautoplunger = true
			EnableBallSaver 20
			'BallSaverTimer.Interval= BallSaverTimer.Interval + 10000	
			'if BallSaverTimer.Interval > 20000 Then BallSaverTimer.Interval = 20000
			AddMultiball 1
			bConnie = False
		Else
			AddBonus SCORE_LANES_COMPLETED
			Playsound "Jackpot4"
			'pDMDSplashTwoLines "LANE BONUS", SCORE_LANES_COMPLETED, 3, cLightBlue
			'DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
		End if
	End If
End Sub


Sub TLeftOutlane_Hit
	If Healer(1) = 0 Then
		DOF 128, DOFPulse
		lightCtrl.Pulse LeftOutlane,4
		Healer(1) = 1 : CheckHealer
		lightCtrl.LightOn LeftOutlane
	Else
		lightCtrl.Pulse LeftOutlane,1
	End If
DEBUG.PRINT "leftoutlanehit    "  & Healer(1)
End Sub

Sub TLeftInlane_hit
	If Healer(3) = 0 Then
		DOF 129, DOFPulse
		lightCtrl.Pulse LeftInlane,4
		Healer(3) = 1 : CheckHealer
		lightCtrl.LightOn LeftInlane
	Else
		lightCtrl.Pulse LeftInlane,1
	End If
End Sub

Sub TLeftInlane2_hit
	If Healer(2) = 0 Then
		DOF 130, DOFPulse
		lightCtrl.Pulse LeftInlane2,4
		Healer(2) = 1 : CheckHealer
		lightCtrl.LightOn LeftInlane2
	Else
		lightCtrl.Pulse LeftInlane2,1
	End If
End Sub

Sub TRightOutlane_hit
	If Healer(6) = 0 Then
		DOF 131, DOFPulse
		lightCtrl.Pulse RightOutlane,4
		Healer(6) = 1 : CheckHealer
		lightCtrl.LightOn RightOutlane
	Else
		lightCtrl.Pulse RightOutlane,1
	End If
End Sub

Sub TRightInlane_hit
	If Healer(4) = 0 Then
		DOF 132, DOFPulse
		lightCtrl.Pulse RightInlane,4
		Healer(4) = 1 : CheckHealer
		lightCtrl.LightOn RightInlane
	Else
		lightCtrl.Pulse RightInlane,1
	End If
End Sub

Sub TRightInlane2_hit
	If Healer(5) = 0 Then
		DOF 133, DOFPulse
		lightCtrl.LightOn RightInlane2
		lightCtrl.Pulse RightInlane2,4
		Healer(5) = 1 : CheckHealer
	Else
		lightCtrl.Pulse RightInlane2,1
	End If
End Sub

Sub Healer_Left
	dim x
	x = Healer(1)
	healer(1) = Healer(2)
	healer(2) = Healer(3)
	healer(3) = Healer(4)
	healer(4) = Healer(5)
	healer(5) = Healer(6)
	healer(6) = x
	updatehealer
End Sub

Sub Healer_Right
	dim x
	x = Healer(6)
	healer(6) = Healer(5)
	healer(5) = Healer(4)
	healer(4) = Healer(3)
	healer(3) = Healer(2)
	healer(2) = Healer(1)
	healer(1) = x
	updatehealer
End Sub

Sub updatehealer
	If Healer(1) = 1 Then lightCtrl.LightOn LeftOutlane Else lightCtrl.LightOff LeftOutlane
	If Healer(2) = 1 Then lightCtrl.LightOn LeftInlane2 Else lightCtrl.LightOff LeftInlane2
	If Healer(3) = 1 Then lightCtrl.LightOn LeftInlane Else lightCtrl.LightOff LeftInlane
	If Healer(4) = 1 Then lightCtrl.LightOn RightInlane Else lightCtrl.LightOff RightInlane
	If Healer(5) = 1 Then lightCtrl.LightOn RightInlane2 Else lightCtrl.LightOff RightInlane2
	If Healer(6) = 1 Then lightCtrl.LightOn RightOutlane Else lightCtrl.LightOff RightOutlane
End Sub

'*******************************************
'  ZMUS - Music
'*******************************************
Dim fCurrentMusicVol 
Dim sMusicTrack : sMusicTrack = ""
Dim fSongVolume : fSongVolume = 1
Dim newSong
Dim CurrSong
Sub SetMusicVolumes
	if sMusicTrack <> "" Then PlaySound sMusicTrack,-1,fMusicVolume,0,0,0,1,0,0
End Sub

'PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)
Sub SwitchMusic(sTrack)

	Debug.print "ST:"&sTrack
	Debug.print "MT:" &sMusicTrack
	If sTrack <> sMusicTrack Then
		if sMusicTrack <> "" Then StopSound sMusicTrack
		sMusicTrack = sTrack
		'Debug.print "WTH Man:" &sTrack
		PlaySound sTrack,-1,fMusicVolume,0,0,0,1,0,0
		fCurrentMusicVol = fMusicVolume
	End If
End Sub

Sub PlayModeMusic
Exit Sub
	fSongVolume = 1

	If bNoMusic Then
		StopAllMusic
		Exit Sub
	Else
		Select Case sMusicTrack
			Case "Music1":SwitchMusic "Music1"
			Case "Music2":SwitchMusic "Music2"
			Case "Music3":SwitchMusic "Music3"
			Case "Music4":SwitchMusic "Music4"
			Case "Music5":SwitchMusic "Music5"
			Case "Music6":SwitchMusic "Music6"
			Case "Music7":SwitchMusic "Music7"
			Case "Music8":SwitchMusic "Music8"
			Case Else		
				UpdateMusicNow		
		End Select
	End If
	
End Sub

Sub UpdateMusicNow
	StopAllMusic
	newSong = RndNbr(6)
	'newSong = 1
    Select Case newSong
        Case 1:SwitchMusic "Music1"' : currsong = "Music2"
        Case 2:SwitchMusic "Music2"' : currsong = "Music3"
        Case 3:SwitchMusic "Music3"' : currsong = "Music4"
        Case 4:SwitchMusic "Music4"' : currsong = "Music5"
        Case 5:SwitchMusic "Music5"' : currsong = "Music6"
        Case 6:SwitchMusic "Music6"' : currsong = "Music7"
    End Select
end sub

Sub StopAllMusic
	'sMusicTrack = ""
	StopSound "M_End"
	StopSound "Music1"
	StopSound "Music2"
	StopSound "Music3"
	StopSound "Music4"
	StopSound "Music5"
	StopSound "Music6"
	StopSound "Music7"
	StopSound "Music8"

	StopSound "Mode1"
	StopSound "Mode2"
	StopSound "Mode3"
	StopSound "Mode4"
	StopSound "Mode5"
	StopSound "Mode6"
	StopSound "Mode7"
	StopSound "Mode8"
	StopSound "Mode9"
	StopSound "Mode10"
	StopSound "Mode11"
	StopSound "Mode12"
	StopSound "Mode13"
	StopSound "Mode14"
	StopSound "Mode15"
	StopSound "Mode16"
	StopSound "WizardMode"
End Sub

Sub DuckMusic2
	PlaySound sMusicTrack, -1, 0.4,0,0,0,1,0,0
End Sub

Sub DuckResume
	DuckTimer.Enabled = 1
	'PlaySound sMusicTrack, -1, fMusicVolume,0,0,0,1,0,0
End Sub

Sub DuckMusic
	PlaySound sMusicTrack, -1, fMusicVolume * fDuckFactor,0,0,0,1,0,0
	fCurrentMusicVol = fMusicVolume * fDuckfactor
	Dbg "Ducking Music with Volume: " &fCurrentMusicVol
end sub

Sub DuckUpdate
	Dim bDuck, fGoalVolume
	bDuck = False
	
	Dbg "Current Volume: " &fCurrentMusicVol
'	If Not oCurrentEvent is Nothing Then
'		If oCurrentEvent.pupTrigger <> 0 Then
'			bDuck = True
'		Else
'			bDuck = False
'		End If
'	Else
'		bDuck = False
'	End If

	If (Not bDuck) And sMusicTrack <> "" Then
		If sMusicTrack = "music-attract" then
			fGoalVolume = fAttractVolume
		Else
			fGoalVolume = fMusicVolume
			'Dbg "Goal Volume: " &fGoalVolume
		end If

		If fCurrentMusicVol < fGoalVolume Then
			' fade in is 6 updates of 17 ms
			fCurrentMusicVol = fCurrentMusicVol + ((fGoalVolume - fDuckfactor) / 6)
			If fCurrentMusicVol >= fGoalVolume Then
				fCurrentMusicVol = fGoalVolume
			End If
			PlaySound sMusicTrack, -1, fCurrentMusicVol,0,0,0,1,0,0
		Else
			Dbg "Leaving with Volume: " &fCurrentMusicVol
			DuckTimer.Enabled = 0
		End If
	End If
end sub

Sub DuckTimer_Timer()
exit sub
	DuckUpdate
End Sub

Dim objIEDebugWindow
Sub Dbg( myDebugText )
' Uncomment the next line to turn off debugging
Exit Sub

If Not IsObject( objIEDebugWindow ) Then
Set objIEDebugWindow = CreateObject( "InternetExplorer.Application" )
objIEDebugWindow.Navigate "about:blank"
objIEDebugWindow.Visible = True
objIEDebugWindow.ToolBar = False
objIEDebugWindow.Width = 600	
objIEDebugWindow.Height = 900
objIEDebugWindow.Left = 2100
objIEDebugWindow.Top = 100
Do While objIEDebugWindow.Busy
Loop
objIEDebugWindow.Document.Title = "My Debug Window"
objIEDebugWindow.Document.Body.InnerHTML = "<b>King Of the Hill Debug Window -TimeStamp: " & GameTime& "</b></br>"
End If

objIEDebugWindow.Document.Body.InnerHTML = objIEDebugWindow.Document.Body.InnerHTML & myDebugText & " --TimeStamp:<b> " & GameTime & "</b><br>" & vbCrLf
End Sub



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
'Note:  for *Future Pinball* "pupDMDupdate_Timer()" timer needs to be renamed to "pupDMDupdate_expired()"  and then all is good.
'       and for future pinball you need to add the follow lines near top
'Need to use BAM and have com idll enabled.
'				Dim icom : Set icom = xBAM.Get("icom") ' "icom" is name of "icom.dll" in BAM\Plugins dir
'				if icom is Nothing then MSGBOX "Error cannot run without icom.dll plugin"
'				Function CreateObject(className)       
'   					Set CreateObject = icom.CreateObject(className)   
'				End Function


'**************************
'   PinUp Player USER Config
'**************************

Dim pGameName       : pGameName="kingofthehill"  'pupvideos foldername, probably set to cGameName in realworld


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

Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")   
PuPlayer.B2SInit "", pGameName

PuPType = 0

	pupPackScreenFile = PuPlayer.GetRoot & "kingofthehill\ScreenType.txt"
	Set ObjFso = CreateObject("Scripting.FileSystemObject")
	Set ObjFile = ObjFso.OpenTextFile(pupPackScreenFile)
	DMDType = ObjFile.ReadLine

	pupPackSizeFile = PuPlayer.GetRoot & "kingofthehill\PupSize.txt"
	Set ObjFso = CreateObject("Scripting.FileSystemObject")
	Set ObjFile = ObjFso.OpenTextFile(pupPackSizeFile)
	PuPType = ObjFile.ReadLine

'	if DMDType = 3 Then 
'		pDMDVideo = 5
'	Else
		pDMDVideo = 11
'	End If

	if DMDType > 1 Then
		pDMD = 5
	Else
		pDMD = 2
	End If

	CheckPupVersion
	PuPlayer.LabelInit pDMD
	PuPlayer.LabelInit pDMDVideo
	PuPlayer.LabelInit pBackglass

	if DMDType = 99 Then 
		BatReminder.Visible = True
	Else
		BatReminder.Visible = False
	End If

pSetPageLayouts

pDMDSetPage(pDMDBlank)   'set blank text overlay page.

	if ScorbitActive Then
		pBackglassSetPage 1
		dbg "Delaying Pup startup"
		BallHandlingQueue.Add "pDMDStartUP","pDMDStartUP",24,2000,0,0,0,False
	Else
		pDMDStartUP
	End If

	if Scorbitactive then 
		if Scorbit.DoInit(4391, "PupOverlays", Version, "opdb-koth") then 	' Prod
			tmrScorbit.Interval=2000
			tmrScorbit.UserValue = 0
			tmrScorbit.Enabled=True 
			Scorbit.UploadLog = ScorbitUploadLog
		End if 
	End if 



BallHandlingQueue.Add "LoadBG","LoadBG",24,1000,0,0,0,False
BallHandlingQueue.Add "UpdateModeMessages.Enabled = 1","UpdateModeMessages.Enabled = 1",24,2000,0,0,0,False



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
Const pScores = 1
Const pAttract = 2
Const pPrevScores = 3
Const pCredits = 4
Const pSlotMachine = 5
Const pBonus = 6
Const pEvent = 7
Const pHighScore = 8


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

Sub pSetPageLayouts
	Dim i

	pDMDAlwaysPAD		'we pad all text with space before and after for shadow clipping/etc


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

if DMDType = 0 Then
	pupCreateLabelImage "ScorbitQRicon1","PuPOverlays\\QRcodeS.png",50,30,34,60,77,0
	pupCreateLabelImage "ScorbitQR1","PuPOverlays\\QRcode.png",50,30,34,60,77,0

	pupCreateLabelImage "ScorbitQRicon2","PuPOverlays\\QRcodeB.png",50,30,34,60,77,0
	pupCreateLabelImage "ScorbitQR2","PuPOverlays\\QRclaim.png",50,30,34,60,77,0
Else
	pupCreateLabelImageBG "ScorbitQRicon1","PuPOverlays\\QRcodeS.png",50,30,34,60,1,0
	pupCreateLabelImageBG "ScorbitQR1","PuPOverlays\\QRcode.png",50,30,34,60,1,0

	pupCreateLabelImageBG "ScorbitQRicon2","PuPOverlays\\QRcodeB.png",50,30,34,60,1,0
	pupCreateLabelImageBG "ScorbitQR2","PuPOverlays\\QRclaim.png",50,30,34,60,1,0
End If



	pupCreateLabelImage "DMDOverlay", "PuPOverlays\\DefaultDMD.png",0,0,100,100,1,0

	pupCreateLabelImage "TimerPopUp","PNG\\timer.png",0,0,100,100,1,0

	pupCreateLabelImage "LockPopUp","PNG\\lock.png",0,0,100,100,1,0

	pupCreateLabelImage "FireworkRed","GIFs\\Firework-Red.gif",0,0,10,20,1,0

	pupCreateLabelImage "Star","PNG\\star.png",10,10,4,5.6,1,0




	pupCreateLabelImage "AttractCredits","Attract\\Full.png",0,0,100,100,1,0
	pupCreateLabelImage "BM_Image","Multipliers\\b2x.png",0,0,100,100,1,0
	pupCreateLabelImage "PFM_Image","Multipliers\\pf2x.png",0,0,100,100,1,0


	pupCreateLabelImage "ExtraBall_Image","PNG\\extraball.png",0,0,100,100,1,0

	'pupCreateLabelImage "PeggyProgress","ProgressBars\\peg0.png",0,0,100,100,1,1	' 4
	pupCreateLabelImage "PeggyProgress","ProgressBars\\peg0.png",0,0,100,100,1,1	' 4
	pupCreateLabelImage "BillProgress","ProgressBars\\bill0.png",0,0,100,100,1,1	' 9
	pupCreateLabelImage "BobbyProgress","ProgressBars\\bobby0.png",0,0,100,100,1,1	' 15
	pupCreateLabelImage "BoomProgress","ProgressBars\\boom0.png",0,0,100,100,1,1 '8
	pupCreateLabelImage "CottonProgress","ProgressBars\\cotton0.png",0,0,100,100,1,1 ' 6
	pupCreateLabelImage "DaleProgress","ProgressBars\\dale0.png",0,0,100,100,1,1	'10
	pupCreateLabelImage "HankProgress","ProgressBars\\hank0.png",0,0,100,100,1,1	' 5
	pupCreateLabelImage "LuanneProgress","ProgressBars\\luanne0.png",0,0,100,100,1,1	'6


	' USED FOR DEBUG ONLY
	PuPlayer.LabelNew pDMD, "Line1b",dmdDef,			15,cGold	,0,1,1, 50,32,	pScores,0
	PuPlayer.LabelNew pDMD, "Line2b",dmdDef, 			15,cGold	,0,1,1, 50,55,	pScores,0

'page 4
	PuPlayer.LabelNew pDMD,"HSLine1",	             dmddef,16,cPurple   ,0,1,1,50,40,1,0
	PuPlayer.LabelNew pDMD,"HSLine2",	             dmddef,16,cPurple   ,0,1,1,50,55,1,0
	pDMDLabelSetBorder "HSLine1",cBlack,6,6,1
	pDMDLabelSetBorder "HSLine2",cBlack,6,6,1

'page 88
	PuPlayer.LabelNew pDMD, "Attract2a", dmddef, 12, cWhite, 0, 1, 1, 50, 40, 1, 0
	PuPlayer.LabelNew pDMD, "Attract2b", dmddef, 12, cWhite, 0, 1, 1, 50, 56, 1, 0
	pDMDLabelSetBorder "Attract2a",cBlack,6,6,1
	pDMDLabelSetBorder "Attract2b",cBlack,6,6,1


	'USED FOR TEMP UPDATES
	PuPlayer.LabelNew pDMD, "Splash", dmddef, 18, cWhite, 0, 1, 1, 50, 32, pScores, 0
	PuPlayer.LabelNew pDMD, "Splash2a", dmddef, 12, cGold, 0, 1, 1, 50, 34, pScores, 0
	PuPlayer.LabelNew pDMD, "Splash2b", dmddef, 12, cGold, 0, 1, 1, 50, 50, pScores, 0
	PuPlayer.LabelNew pDMD, "Splash3a", dmddef, 10, cGold, 0, 1, 1, 50, 30, pScores, 0
	PuPlayer.LabelNew pDMD, "Splash3b", dmddef, 10, cGold, 0, 1, 1, 50, 42, pScores, 0
	PuPlayer.LabelNew pDMD, "Splash3c", dmddef, 10, cGold, 0, 1, 1, 50, 54, pScores, 0

	pDMDLabelSetBorder "Splash",cBlack,6,6,1
	pDMDLabelSetBorder "Splash2A",cBlack,6,6,1
	pDMDLabelSetBorder "Splash2B",cBlack,6,6,1
	pDMDLabelSetBorder "Splash3a",cBlack,6,6,1
	pDMDLabelSetBorder "Splash3b",cBlack,6,6,1
	pDMDLabelSetBorder "Splash3c",cBlack,6,6,1

	'USED FOR LONG TERM DISPLAYS
	PuPlayer.LabelNew pDMD, "Event3A", dmddef, 10, cWhite, 0, 1, 1, 50, 31, pScores, 0
	PuPlayer.LabelNew pDMD, "Event3B", dmddef, 10, cWhite, 0, 1, 1, 50, 45, pScores, 0
	PuPlayer.LabelNew pDMD, "Event3C", dmddef, 10, cWhite, 0, 1, 1, 50, 59, pScores, 0

	pDMDLabelSetBorder "Event3A",cBlack,6,6,1
	pDMDLabelSetBorder "Event3B",cBlack,6,6,1
	pDMDLabelSetBorder "Event3C",cBlack,6,6,1

	'USED FOR JACKPOTS
	PuPlayer.LabelNew pDMD, "SplashJP2a", dmddef, 12, cRed, 0, 1, 1, 50, 34, pScores, 0
	PuPlayer.LabelNew pDMD, "SplashJP2b", dmddef, 12, cRed, 0, 1, 1, 50, 50, pScores, 0


	PuPlayer.LabelNew pDMD,"CurrScore",         dmddef,5,cWhite   ,0,1,1, 50,88.5,1,0
	PuPlayer.LabelNew pDMD,"CurrName",	             dmddef,5,cWhite   ,0,0,0,50,79.5,1,0
	PuPlayer.LabelNew pDMD,"Position1Name",	             dmddef,5,cBlack   ,0,0,0,0,89,1,0
	PuPlayer.LabelNew pDMD,"Position1Score",	             dmddef,5,cBlack   ,0,0,1,33,79.5,1,0
	PuPlayer.LabelNew pDMD,"Position2Name",	             dmddef,5,cBlack   ,0,0,0,20,89,1,0
	PuPlayer.LabelNew pDMD,"Position2Score",	             dmddef,5,cBlack  ,0,0,1,20,94,1,0
	PuPlayer.LabelNew pDMD,"Position3Name",	             dmddef,5,cBlack   ,0,0,0,50,89,1,0
	PuPlayer.LabelNew pDMD,"Position3Score",	             dmddef,5,cBlack   ,0,0,1,50,94,1,0
	PuPlayer.LabelNew pDMD,"Position4Name",	             dmddef,5,cBlack   ,0,0,0,80,89,1,0
	PuPlayer.LabelNew pDMD,"Position4Score",	             dmddef,5,cBlack   ,0,0,1,80,94,1,0

	pDMDLabelSetBorder "CurrScore",cBlack,10,10,1
	pDMDLabelSetBorder "CurrName",cBlack,10,10,1

	pDMDLabelSetBorder "Position1Score",cBlack,10,10,1
	pDMDLabelSetBorder "Position2Score",cBlack,10,10,1
	pDMDLabelSetBorder "Position3Score",cBlack,10,10,1
	pDMDLabelSetBorder "Position4Score",cBlack,10,10,1
	pDMDLabelSetBorder "Position1Name",cBlack,10,10,1
	pDMDLabelSetBorder "Position2Name",cBlack,10,10,1
	pDMDLabelSetBorder "Position3Name",cBlack,10,10,1
	pDMDLabelSetBorder "Position4Name",cBlack,10,10,1

	' EOB Sequence
	PuPlayer.LabelNew pDMD, "Event5A", dmddef, 6, cRed, 0, 1, 1, 50, 10, pScores, 0
	PuPlayer.LabelNew pDMD, "Event5B", dmddef, 6, cRed, 0, 1, 1, 50, 20, pScores, 0
	PuPlayer.LabelNew pDMD, "Event5C", dmddef, 6, cRed, 0, 1, 1, 50, 30, pScores, 0
	PuPlayer.LabelNew pDMD, "Event5D", dmddef, 6, cRed, 0, 1, 1, 50, 40, pScores, 0
	PuPlayer.LabelNew pDMD, "Event5E", dmddef, 6, cWhite, 0, 1, 1, 50, 50, pScores, 0
	PuPlayer.LabelNew pDMD, "Event5F", dmddef, 6, cBlue, 0, 1, 1, 50, 60, pScores, 0

	pDMDLabelSetBorder "Event5A",cBlack,6,6,1
	pDMDLabelSetBorder "Event5B",cBlack,6,6,1
	pDMDLabelSetBorder "Event5C",cBlack,6,6,1
	pDMDLabelSetBorder "Event5D",cBlack,6,6,1
	pDMDLabelSetBorder "Event5E",cBlack,6,6,1
	pDMDLabelSetBorder "Event5F",cBlack,6,6,1


	PuPlayer.LabelNew pDMD,"BallValue",	             dmddef,6,cWhite   ,0,1,1,65,77.1,pScores,0
	pDMDLabelSetBorder "BallValue",cBlack,4,4,1

	PuPlayer.LabelNew pDMD,"MainModeTimerValue",	             dmddef,8,cRed   ,0,1,1,3.8,76,1,0
	pDMDLabelSetBorder "MainModeTimerValue",cBlack,10,10,1


	PuPlayer.LabelNew pDMD,"BMValue",	             dmddef,12,cWhite   ,0,1,1,21.75,87.5,1,0
	pDMDLabelSetBorder "BMValue",cBlack,4,4,1

	PuPlayer.LabelNew pDMD,"PFMValue",	             dmddef,12,cWhite   ,0,1,1,78.75,87.5,1,0
	pDMDLabelSetBorder "PFMValue",cBlack,4,4,1

	PuPlayer.LabelNew pDMD,"TimerValue",	             dmddef,12,cRed   ,0,1,1,78.75,87.5,1,0
	pDMDLabelSetBorder "TimerValue",cBlack,4,4,1

	PuPlayer.LabelNew pDMD,"LockValue",	             dmddef,12,cRed   ,0,1,1,78.75,87.5,1,0
	pDMDLabelSetBorder "LockValue",cBlack,4,4,1



End Sub




Sub pDMDStartUP
'do stuff fancy pants on first run
'	pInAttract = True
	pDMDSetPage(pScores)

	if ScorbitActive Then
		dbg "Calling SCORBIT check pairing"
		BallHandlingQueue.Add "CheckPairing","CheckPairing",24,2000,0,0,0,False
	End If
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


Sub DisplayTimerPopUp
	PuPlayer.LabelSet pDMD, "TimerPopup", "PNG\\timer.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"

'	if nTimerCount > 99 Then
'		PuPlayer.LabelSet pDMD,"TimerValue",nTimerCount,1,"{'mt':2,'fonth':7,'xalign':1,'yalign':1,'xpos':26.75,'ypos':10.5}"
'	Else
'		PuPlayer.LabelSet pDMD,"TimerValue",nTimerCount,1,"{'mt':2,'fonth':8.5,'xalign':1,'yalign':1,'xpos':27,'ypos':10.5}"
'	End If
End Sub

Sub HideTimerPopUp
	DMDQueue.Add "Hide Timer Value","pDMDLabelhide ""TimerValue"" ",1,10,0,0,0,False
	DMDQueue.Add "Hide Timer Popup","pDMDLabelhide ""TimerPopup"" ",1,20,0,0,0,False
End Sub

Sub DisplayLockPopUp
	if bReleasingBalls or nLockedBalls = 0 Then exit sub

	PuPlayer.LabelSet pDMD, "LockPopup", "PNG\\lock.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pDMD, "LockValue",nLockedBalls,1,"{'mt':2,'fonth':8.5,'xalign':1,'yalign':1,'xpos':73.25,'ypos':10.5}"
End Sub
 
Sub PupLockBallDisplay
	if bReleasingBalls or nLockedBalls = 0 or bMowerNotStarted = False Then exit sub

	DisplayLockPopUp

	PuPBallLock
	pDMDSplashTwoLines "BALL "& nLockedBalls, "LOCKED", 3, cRed
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{green}:Ball "&nLockedBalls & " Locked"
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub HideLockedBallPopUp
	DMDQueue.Add "Hide Locked Balls Value","pDMDLabelhide ""LockValue"" ",1,10,0,0,0,False
	DMDQueue.Add "Hide Locked Balls Popup","pDMDLabelhide ""LockPopup"" ",1,20,0,0,0,False
End Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
''''' 		Peggy
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
Sub CalcPeggy
	dim i
	anPeggyLights(CurrentPlayer,0) = 0

	for i = 1 to 3
		if anPeggyLights(CurrentPlayer,i) = 1 Then anPeggyLights(CurrentPlayer,0) = anPeggyLights(CurrentPlayer,0) + 1
	Next

End Sub

Sub DisplayPeggyProgress
	CalcPeggy

	Select Case anPeggyLights(CurrentPlayer,0)
		Case 0
			PuPlayer.LabelSet pDMD, "PeggyProgress", "ProgressBars\\peg0.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 1
			PuPlayer.LabelSet pDMD, "PeggyProgress", "ProgressBars\\peg1.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 2
			PuPlayer.LabelSet pDMD, "PeggyProgress", "ProgressBars\\peg2.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 3				
			PuPlayer.LabelSet pDMD, "PeggyProgress", "ProgressBars\\peg3.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 4
			PuPlayer.LabelSet pDMD, "PeggyProgress", "ProgressBars\\peg4.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
	End Select
End Sub

Sub ResetPeggyLights(CP)
	dim i
	for i = 0 to 3
		anPeggyLights(CP,i) = 0
	Next
End Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
''''' 		Bill
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
Sub CalcBill
	dim i
	anBillLights(CurrentPlayer,0) = 0

	for i = 1 to 9
		if anBillLights(CurrentPlayer,i) = 1 Then anBillLights(CurrentPlayer,0) = anBillLights(CurrentPlayer,0) + 1
	Next

End Sub

Sub DisplayBillProgress
	'CalcBill

	Select Case anBillLights(CurrentPlayer,0)
		Case 0
			PuPlayer.LabelSet pDMD, "BillProgress", "ProgressBars\\Bill0.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 1
			PuPlayer.LabelSet pDMD, "BillProgress", "ProgressBars\\Bill1.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 2
			PuPlayer.LabelSet pDMD, "BillProgress", "ProgressBars\\Bill2.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 3
			PuPlayer.LabelSet pDMD, "BillProgress", "ProgressBars\\Bill3.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 4
			PuPlayer.LabelSet pDMD, "BillProgress", "ProgressBars\\Bill4.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 5
			PuPlayer.LabelSet pDMD, "BillProgress", "ProgressBars\\Bill5.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 6
			PuPlayer.LabelSet pDMD, "BillProgress", "ProgressBars\\Bill6.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 7
			PuPlayer.LabelSet pDMD, "BillProgress", "ProgressBars\\Bill7.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 8
			PuPlayer.LabelSet pDMD, "BillProgress", "ProgressBars\\Bill8.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 9
			PuPlayer.LabelSet pDMD, "BillProgress", "ProgressBars\\Bill9.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
	End Select
End Sub

Sub ResetBillLights(CP)
	dim i
	for i = 0 to 9
		anBillLights(CP,i) = 0
	Next
End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
''''' 		Bobby
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
Sub CalcBobby
	dim i
	anBobbyLights(CurrentPlayer,0) = 0

	for i = 1 to 15
		if anBobbyLights(CurrentPlayer,i) = 1 Then anBobbyLights(CurrentPlayer,0) = anBobbyLights(CurrentPlayer,0) + 1
	Next

End Sub

Sub DisplayBobbyProgress
	'CalcBobby

	Select Case anBobbyLights(CurrentPlayer,0)
		Case 0
			PuPlayer.LabelSet pDMD, "BobbyProgress", "ProgressBars\\Bobby0.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 1
			PuPlayer.LabelSet pDMD, "BobbyProgress", "ProgressBars\\Bobby1.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 2
			PuPlayer.LabelSet pDMD, "BobbyProgress", "ProgressBars\\Bobby2.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 3
			PuPlayer.LabelSet pDMD, "BobbyProgress", "ProgressBars\\Bobby3.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 4
			PuPlayer.LabelSet pDMD, "BobbyProgress", "ProgressBars\\Bobby4.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 5
			PuPlayer.LabelSet pDMD, "BobbyProgress", "ProgressBars\\Bobby5.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 6
			PuPlayer.LabelSet pDMD, "BobbyProgress", "ProgressBars\\Bobby6.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 7
			PuPlayer.LabelSet pDMD, "BobbyProgress", "ProgressBars\\Bobby7.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 8
			PuPlayer.LabelSet pDMD, "BobbyProgress", "ProgressBars\\Bobby8.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 9
			PuPlayer.LabelSet pDMD, "BobbyProgress", "ProgressBars\\Bobby9.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 10
			PuPlayer.LabelSet pDMD, "BobbyProgress", "ProgressBars\\Bobby10.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 11
			PuPlayer.LabelSet pDMD, "BobbyProgress", "ProgressBars\\Bobby11.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 12
			PuPlayer.LabelSet pDMD, "BobbyProgress", "ProgressBars\\Bobby12.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 13
			PuPlayer.LabelSet pDMD, "BobbyProgress", "ProgressBars\\Bobby13.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 14
			PuPlayer.LabelSet pDMD, "BobbyProgress", "ProgressBars\\Bobby14.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 15
			PuPlayer.LabelSet pDMD, "BobbyProgress", "ProgressBars\\Bobby15.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
	End Select
End Sub

Sub ResetBobbyLights(CP)
	dim i
	for i = 0 to 15
		anBobbyLights(CP,i) = 0
	Next
End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
''''' 		Boomhauer
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
Sub CalcBoom
	dim i
	anBoomLights(CurrentPlayer,0) = 0

	for i = 1 to 9
		if anBoomLights(CurrentPlayer,i) = 1 Then anBoomLights(CurrentPlayer,0) = anBoomLights(CurrentPlayer,0) + 1
	Next

End Sub

Sub DisplayBoomProgress
	'CalcBoom

	Select Case anBoomLights(CurrentPlayer,0)
		Case 0
			PuPlayer.LabelSet pDMD, "BoomProgress", "ProgressBars\\Boom0.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 1
			PuPlayer.LabelSet pDMD, "BoomProgress", "ProgressBars\\Boom1.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 2
			PuPlayer.LabelSet pDMD, "BoomProgress", "ProgressBars\\Boom2.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 3
			PuPlayer.LabelSet pDMD, "BoomProgress", "ProgressBars\\Boom3.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 4
			PuPlayer.LabelSet pDMD, "BoomProgress", "ProgressBars\\Boom4.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 5
			PuPlayer.LabelSet pDMD, "BoomProgress", "ProgressBars\\Boom5.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 6
			PuPlayer.LabelSet pDMD, "BoomProgress", "ProgressBars\\Boom6.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 7
			PuPlayer.LabelSet pDMD, "BoomProgress", "ProgressBars\\Boom7.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 8
			PuPlayer.LabelSet pDMD, "BoomProgress", "ProgressBars\\Boom8.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
	End Select
End Sub

Sub ResetBoomLights(CP)
	dim i
	for i = 0 to 9
		anBoomLights(CP,i) = 0
	Next
End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
''''' 		Cotton
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
Sub CalcCotton

	if Mode(CurrentPlayer,0) <> 0 Then Exit Sub

	dim i
	anCottonLights(CurrentPlayer,0) = 0

	for i = 1 to 6
		if anCottonLights(CurrentPlayer,i) = 1 Then anCottonLights(CurrentPlayer,0) = anCottonLights(CurrentPlayer,0) + 1
	Next

	CheckCottonProgress

End Sub

Sub DisplayCottonProgress

	if Mode(CurrentPlayer,0) <> 0 Then Exit Sub
	CalcCotton

	Select Case anCottonLights(CurrentPlayer,0)
		Case 0
			PuPlayer.LabelSet pDMD, "CottonProgress", "ProgressBars\\Cotton0.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 1
			PuPlayer.LabelSet pDMD, "CottonProgress", "ProgressBars\\Cotton1.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 2
			PuPlayer.LabelSet pDMD, "CottonProgress", "ProgressBars\\Cotton2.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 3
			PuPlayer.LabelSet pDMD, "CottonProgress", "ProgressBars\\Cotton3.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 4
			PuPlayer.LabelSet pDMD, "CottonProgress", "ProgressBars\\Cotton4.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 5
			PuPlayer.LabelSet pDMD, "CottonProgress", "ProgressBars\\Cotton5.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 6
			PuPlayer.LabelSet pDMD, "CottonProgress", "ProgressBars\\Cotton6.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
	End Select
End Sub

Sub ResetCottonLights(CP)
	dim i
	for i = 0 to 6
		anCottonLights(CP,i) = 0
	Next
End Sub

Sub CheckCottonProgress
	if anCottonLights(CurrentPlayer,0) > 5 Then StartCotton
End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
''''' 		Dale
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
Sub CalcDale
	DaleLightsCount = 0
	DaleLightsCount = anDaleLights1(CurrentPlayer) + anDaleLights2(CurrentPlayer) + anDaleLights3(CurrentPlayer)

	Debug.print "Dale: "&DaleLightsCount
	if DaleLightsCount = 9 Then bDaleReady = True
End Sub

Sub DisplayDaleProgress
	CalcDale

	Select Case DaleLightsCount
		Case 0
			PuPlayer.LabelSet pDMD, "DaleProgress", "ProgressBars\\Dale0.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 1
			PuPlayer.LabelSet pDMD, "DaleProgress", "ProgressBars\\Dale1.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 2
			PuPlayer.LabelSet pDMD, "DaleProgress", "ProgressBars\\Dale2.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 3
			PuPlayer.LabelSet pDMD, "DaleProgress", "ProgressBars\\Dale3.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 4
			PuPlayer.LabelSet pDMD, "DaleProgress", "ProgressBars\\Dale4.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 5
			PuPlayer.LabelSet pDMD, "DaleProgress", "ProgressBars\\Dale5.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 6
			PuPlayer.LabelSet pDMD, "DaleProgress", "ProgressBars\\Dale6.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 7
			PuPlayer.LabelSet pDMD, "DaleProgress", "ProgressBars\\Dale7.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 8
			PuPlayer.LabelSet pDMD, "DaleProgress", "ProgressBars\\Dale8.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 9
			PuPlayer.LabelSet pDMD, "DaleProgress", "ProgressBars\\Dale9.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 10
			PuPlayer.LabelSet pDMD, "DaleProgress", "ProgressBars\\Dale10.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
	End Select
End Sub

Sub ResetDaleLights(CP)
'	dim i
	DaleLightsCount = 0



'	for i = 0 to 3
		anDaleLights1(CP) = 0
		anDaleLights2(CP) = 0
		anDaleLights3(CP) = 0
'	Next
End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
''''' 		Hank
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
Sub CalcHank
exit sub
	dim i
	anHankLights(CurrentPlayer,0) = 0

	for i = 1 to 5
		if anHankLights(CurrentPlayer,i) = 1 Then anHankLights(CurrentPlayer,0) = anHankLights(CurrentPlayer,0) + 1
	Next

End Sub

Sub DisplayHankProgress
	'CalcHank

	Select Case anHankLights(CurrentPlayer,0)
		Case 0
			PuPlayer.LabelSet pDMD, "HankProgress", "ProgressBars\\Hank0.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 1
			PuPlayer.LabelSet pDMD, "HankProgress", "ProgressBars\\Hank1.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 2
			PuPlayer.LabelSet pDMD, "HankProgress", "ProgressBars\\Hank2.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 3
			PuPlayer.LabelSet pDMD, "HankProgress", "ProgressBars\\Hank3.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 4
			PuPlayer.LabelSet pDMD, "HankProgress", "ProgressBars\\Hank4.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 5
			PuPlayer.LabelSet pDMD, "HankProgress", "ProgressBars\\Hank5.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
	End Select
End Sub

Sub ResetHankLights(CP)
	dim i
	for i = 0 to 5
		anHankLights(CP,i) = 0
	Next

	Light004.State = 0
	Light005.State = 0
	Light006.State = 0
End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
''''' 		Luanne
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
Sub CalcLuanne
	dim i
	anLuanneLights(CurrentPlayer,0) = 0

	for i = 1 to 6
		if anLuanneLights(CurrentPlayer,i) = 1 Then anLuanneLights(CurrentPlayer,0) = anLuanneLights(CurrentPlayer,0) + 1
	Next

End Sub

Sub DisplayLuanneProgress
	'CalcLuanne

	Select Case anLuanneLights(CurrentPlayer,0)
		Case 0
			PuPlayer.LabelSet pDMD, "LuanneProgress", "ProgressBars\\Luanne0.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 1
			PuPlayer.LabelSet pDMD, "LuanneProgress", "ProgressBars\\Luanne1.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 2
			PuPlayer.LabelSet pDMD, "LuanneProgress", "ProgressBars\\Luanne2.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 3
			PuPlayer.LabelSet pDMD, "LuanneProgress", "ProgressBars\\Luanne3.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 4
			PuPlayer.LabelSet pDMD, "LuanneProgress", "ProgressBars\\Luanne4.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 5
			PuPlayer.LabelSet pDMD, "LuanneProgress", "ProgressBars\\Luanne5.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
		Case 6
			PuPlayer.LabelSet pDMD, "LuanneProgress", "ProgressBars\\Luanne6.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0,'yalign':0,'ypos':0,'xpos':0}"
	End Select
End Sub

Sub ResetLuanneLights(CP)
	dim i
	for i = 0 to 6
		anLuanneLights(CP,i) = 0
	Next
End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
''''' 		Misc Modes
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

Sub ResetPropane(CP)
	dim i
	for i = 0 to 7
		anPropane(CP,i) = 0
	Next
End Sub

Sub ResetLawnMowerAll
	dim i,j
	for j = 1 to 4
		for i = 0 to 5
			anLawnMower(j,i) = 0
		Next
	Next

End Sub

Sub ResetLadyBird(CP)
	dim i
	for i = 0 to 8
		anLadyBird(CP,i) = 0
	Next
End Sub

Sub ResetDrinkAlamo(CP)
	dim i
	for i = 0 to 10
		anDrinkAlamo(CP,i) = 0
	Next
	CloseCooler
End Sub

Sub ResetYEP(CP)
	dim i
	for i = 0 to 3
		anYEP(CP,i) = 0
	Next
End Sub

Sub ResetBBQFeast(CP)
	dim i
	for i = 0 to 5
		anBBQFeast(CP,i) = 0
	Next
End Sub

Sub ResetBeerLights(CP)
	dim i
	for i = 0 to 5
		anBeerLights(CP,i) = 0
	Next
	Light001.State = 0
	Light002.State = 0
	Light003.State = 0
End Sub


Sub DisplayAllProgress
	DisplayBillProgress
	DisplayBobbyProgress
	DisplayBoomProgress
	DisplayCottonProgress
	DisplayDaleProgress
	DisplayHankProgress
	DisplayPeggyProgress
	DisplayLuanneProgress
End Sub

Sub ResetAllProgress
Dim c
	for c = 1 to 4
		ResetBillLights(c)
		ResetBobbyLights(c)
		ResetBoomLights(c)
		ResetCottonLights(c)
		ResetDaleLights(c)
		ResetHankLights(c)
		ResetLuanneLights(c)
		ResetPeggyLights(c)
		ResetPropane(c)
		ResetLadyBird(c)
		ResetDrinkAlamo(c)
		ResetYEP(c)
		ResetBeerLights(c)
		ResetBBQFeast(c)
	Next

ResetLawnMowerAll
End Sub



Sub DisplayExtraBall
	PuPlayer.LabelSet pDMD, "Extraball_Image", "balls\\extraball.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0, 'yalign':0,'ypos':0,'xpos':0}"
End Sub

Sub HideExtraBall
	PuPlayer.LabelSet pDMD, "Extraball_Image", "balls\\extraball.png",0,""
End Sub

Sub CheckKingExtraBall
	if bKingBall(CurrentPlayer) = 1 Then Exit Sub
	Dim EB, i
	EB = True

	for i = 3 to 6
		if Mode(CurrentPlayer,i) <> 1 Then EB = False
	Next

	if EB Then bEBReady = True : Light008.State = 2 : bKingBall(CurrentPlayer) = 1

	if bHillBall(CurrentPlayer) And bKingBall(CurrentPlayer) Then WizardMode

End Sub

Sub CheckHillExtraBall

	if bHillBall(CurrentPlayer) = 1 Then Exit Sub
	Dim EB, i
	EB = True

	for i = 9 to 12
		if Mode(CurrentPlayer,i) <> 1 Then EB = False
	Next

	if EB Then bEBReady = True : Light008.State = 2	: bHillBall(CurrentPlayer) = 1

	if bHillBall(CurrentPlayer) And bKingBall(CurrentPlayer) Then WizardMode

End Sub


Sub DMDUpdateBallNumber(nBallNr)

'	if VRROOM > 0 Then
'			PuPlayer.LabelSet pDMD,"BallValue","Ball " &nBallNr,1,"{'mt':2,'fonth':"& VR_BallValue &",'xalign':0,'yalign':0,'ypos':76.6,'xpos':62.25}"
'	Else
		PuPlayer.LabelSet pDMD,"BallValue","Ball " &nBallNr,1,"{'mt':2,'fonth':4,'xalign':0,'yalign':0,'ypos':76,'xpos':65}"
'	End If

exit sub

	Select Case nBallNr
		Case 1
			PuPlayer.LabelSet pDMD, "Ball3_Image", "balls\\ball3.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0, 'yalign':0,'ypos':0,'xpos':0}"
			PuPlayer.LabelSet pDMD, "Ball2_Image", "balls\\ball2.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0, 'yalign':0,'ypos':0,'xpos':0}"
			PuPlayer.LabelSet pDMD, "Ball1_Image", "balls\\ball1.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0, 'yalign':0,'ypos':0,'xpos':0}"
		Case 2
			PuPlayer.LabelSet pDMD, "Ball3_Image", "balls\\ball3.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0, 'yalign':0,'ypos':0,'xpos':0}"
			PuPlayer.LabelSet pDMD, "Ball2_Image", "balls\\ball2.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0, 'yalign':0,'ypos':0,'xpos':0}"
			PuPlayer.LabelSet pDMD, "Ball1_Image", "balls\\ball1.png",0,""
		Case 3
			PuPlayer.LabelSet pDMD, "Ball3_Image", "balls\\ball3.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0, 'yalign':0,'ypos':0,'xpos':0}"
			PuPlayer.LabelSet pDMD, "Ball2_Image", "balls\\ball2.png",0,""
			PuPlayer.LabelSet pDMD, "Ball1_Image", "balls\\ball1.png",0,""
	End Select


End Sub

Sub DMDClearPlayerName
	pDMDLabelHide "CurrName"
	PuPlayer.LabelSet pDMD,"Position1Score","",1,""
	PuPlayer.LabelSet pDMD,"Position2Score","",1,""
	PuPlayer.LabelSet pDMD,"Position3Score","",1,""
	PuPlayer.LabelSet pDMD,"Position4Score","",1,""
End Sub

Sub DMDUpdatePlayerName
	DMDClearPlayerName

'	if VRRoom > 0 Then
'		PuPlayer.LabelSet pDMD,"CurrScore"," " & FormatScore(Score(nplayer)) & " ",1,"{'mt':2,'fonth':"& VR_Score &",'xpos':50,'ypos':87.0}"
'	Elseif Score(nPlayer) > 999999999 Then 
'		PuPlayer.LabelSet pDMD,"CurrScore"," " & FormatScore(Score(nplayer)) & " ",1,"{'mt':2,'fonth':8,'xpos':50,'ypos':87.0}"
'	Elseif nPlayersinGame < 3 Then
'		PuPlayer.LabelSet pDMD,"CurrScore"," " & FormatScore(Score(nplayer)) & " ",1,"{'mt':2,'fonth':10,'xpos':50,'ypos':88.5}"
'	Else

	if PlayersPlayingGame = 1 Then
		PuPlayer.LabelSet pDMD,"CurrScore"," " & FormatScore(Score(CurrentPlayer)) & " ",1,"{'mt':2,'fonth':14,'xpos':50,'ypos':89.5}"	
	Else
		PuPlayer.LabelSet pDMD,"CurrScore"," " & FormatScore(Score(CurrentPlayer)) & " ",1,"{'mt':2,'fonth':9,'xpos':50,'ypos':87.0}"
	End If
'	End If


	PuPlayer.LabelSet pDMD,"CurrName","Player " &CurrentPlayer,1,"{'mt':2,'color': "&cWhite&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':79.0}"

	Select Case CurrentPlayer
		Case 1
			
			if PlayersPlayingGame > 1 Then
'				PuPlayer.LabelSet pDMD, "Bullet2", "PuPOverlays\\P2.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':76.5,'xpos':26}"
				PuPlayer.LabelSet pDMD,"Position2Score",FormatScore(Score(2)),1,"{'mt':2,'color': "&cWhite&" ,'xalign':0,'yalign':1,'xpos':29,'ypos':79.1}"  
			End If

			if PlayersPlayingGame > 2 Then
'				PuPlayer.LabelSet pDMD, "Bullet3", "PuPOverlays\\P3.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':90.5,'xpos':26}"
				PuPlayer.LabelSet pDMD,"Position3Score",FormatScore(Score(3)),1,"{'mt':2,'color': "&cWhite&" ,'xalign':0,'yalign':1,'xpos':29,'ypos':94.5}"  
			End If

			if PlayersPlayingGame > 3 Then
'				PuPlayer.LabelSet pDMD, "Bullet4", "PuPOverlays\\P4.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':90.5,'xpos':70.25}"
				PuPlayer.LabelSet pDMD,"Position4Score",FormatScore(Score(4)),1,"{'mt':2,'color': "&cWhite&" ,'xalign':2,'yalign':1,'xpos':71,'ypos':94.5}" 
			End If
		Case 2 

'			PuPlayer.LabelSet pDMD, "Bullet1", "PuPOverlays\\P1.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':76.5,'xpos':26}"
			PuPlayer.LabelSet pDMD,"Position1Score",FormatScore(Score(1)),1,"{'mt':2,'color': "&cWhite&" ,'xalign':0,'yalign':1,'xpos':29,'ypos':79.1}"  

			if PlayersPlayingGame > 2 Then
'				PuPlayer.LabelSet pDMD, "Bullet3", "PuPOverlays\\P3.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':90.5,'xpos':26}"
				PuPlayer.LabelSet pDMD,"Position3Score",FormatScore(Score(3)),1,"{'mt':2,'color': "&cWhite&" ,'xalign':0,'yalign':1,'xpos':29,'ypos':94.5}"  
			End If

			if PlayersPlayingGame > 3 Then
'				PuPlayer.LabelSet pDMD, "Bullet4", "PuPOverlays\\P4.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':90.5,'xpos':70.25}"
				PuPlayer.LabelSet pDMD,"Position4Score",FormatScore(Score(4)),1,"{'mt':2,'color': "&cWhite&" ,'xalign':2,'yalign':1,'xpos':71,'ypos':94.5}"
			End If

		Case 3
'			PuPlayer.LabelSet pDMD, "Bullet1", "PuPOverlays\\P1.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':76.5,'xpos':26}"
			PuPlayer.LabelSet pDMD,"Position1Score",FormatScore(Score(1)),1,"{'mt':2,'color': "&cWhite&" ,'xalign':0,'yalign':1,'xpos':29,'ypos':79.1}"  

'			PuPlayer.LabelSet pDMD, "Bullet2", "PuPOverlays\\P2.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':90.5,'xpos':26}"
			PuPlayer.LabelSet pDMD,"Position2Score",FormatScore(Score(2)),1,"{'mt':2,'color': "&cWhite&" ,'xalign':0,'yalign':1,'xpos':29,'ypos':94.5}" 

			if PlayersPlayingGame > 3 Then
'				PuPlayer.LabelSet pDMD, "Bullet4", "PuPOverlays\\P4.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':90.5,'xpos':70.25}"
				PuPlayer.LabelSet pDMD,"Position4Score",FormatScore(Score(4)),1,"{'mt':2,'color': "&cWhite&" ,'xalign':2,'yalign':1,'xpos':71,'ypos':94.5}"
			End If
		Case 4
'			PuPlayer.LabelSet pDMD, "Bullet1", "PuPOverlays\\P1.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':76.5,'xpos':26}"
			PuPlayer.LabelSet pDMD,"Position1Score",FormatScore(Score(1)),1,"{'mt':2,'color': "&cWhite&" ,'xalign':0,'yalign':1,'xpos':29,'ypos':79.1}"  

'			PuPlayer.LabelSet pDMD, "Bullet2", "PuPOverlays\\P2.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':90.5,'xpos':26}"
			PuPlayer.LabelSet pDMD,"Position2Score",FormatScore(Score(2)),1,"{'mt':2,'color': "&cWhite&" ,'xalign':0,'yalign':1,'xpos':29,'ypos':94.5}"  

'			PuPlayer.LabelSet pDMD, "Bullet3", "PuPOverlays\\P3.png",1,"{'mt':2,'width':4, 'height':6,'xalign':0,'yalign':0,'ypos':90.5,'xpos':70.25}"
			PuPlayer.LabelSet pDMD,"Position3Score",FormatScore(Score(3)),1,"{'mt':2,'color': "&cWhite&" ,'xalign':2,'yalign':1,'xpos':71,'ypos':94.5}" 
	End Select
End Sub

Sub DMDUpdateAll
	DMDUpdatePlayerName
	DMDUpdateBallNumber Balls
	DisplayBonusValue
	DisplayPlayfieldValue
End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
''''' 		Grill Images
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
Sub ResetBXLights
	B2XLight.State = 0
	B3XLight.State = 0
	B4XLight.State = 0
	B5XLight.State = 0
End Sub

Sub DisplayBonusValue
	ResetBXLights
	Select Case nBonusX(CurrentPlayer)
		Case 2
			PuPlayer.LabelSet pDMD, "BM_Image", "Multipliers\\b2x.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0, 'yalign':0,'ypos':0,'xpos':0}"
			B2XLight.State = 1
		Case 3
			PuPlayer.LabelSet pDMD, "BM_Image", "Multipliers\\b3x.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0, 'yalign':0,'ypos':0,'xpos':0}"
			B3XLight.State = 1
		Case 4
			PuPlayer.LabelSet pDMD, "BM_Image", "Multipliers\\b4x.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0, 'yalign':0,'ypos':0,'xpos':0}"
			B4XLight.State = 1
		Case 5
			PuPlayer.LabelSet pDMD, "BM_Image", "Multipliers\\b5x.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0, 'yalign':0,'ypos':0,'xpos':0}"	
			B5XLight.State = 1
		Case Else
			PuPlayer.LabelSet pDMD, "BM_Image", "Multipliers\\Clear.png",0,"{'mt':2,'width':100, 'height':100,'xalign':0, 'yalign':0,'ypos':0,'xpos':0}"
	End Select

'	if VRRoom > 0 Then
'		PuPlayer.LabelSet pDMD,"BMValue",nBonusX(nPlayer) &"X ",1,"{'mt':2,'fonth':"& VR_BonusSize &", 'color':" & cGold &"}"
'	Else
		PuPlayer.LabelSet pDMD,"BMValue",nBonusX(CurrentPlayer) &"X ",1,"{'mt':2,'fonth':14,'ypos':84,'xpos':11.5}"
'	End If

	if nBonusX(CurrentPlayer) = 1 Then Exit Sub
		
	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{blue}:Bonus Increased: "&nBonusX(CurrentPlayer) &"X "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If



End Sub

Sub ResetPFMLights
	P2XLight.State = 0
	P3XLight.State = 0
	P4XLight.State = 0
	P5XLight.State = 0
End Sub

Sub DisplayPlayfieldValue
	ResetPFMLights
	Select Case nPlayfieldX(CurrentPlayer)
		Case 2
			PuPlayer.LabelSet pDMD, "PFM_Image", "Multipliers\\pf2x.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0, 'yalign':0,'ypos':0,'xpos':0}"
			P2xLight.State = 1
		Case 3
			PuPlayer.LabelSet pDMD, "PFM_Image", "Multipliers\\pf3x.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0, 'yalign':0,'ypos':0,'xpos':0}"
			P3xLight.State = 1
		Case 4
			PuPlayer.LabelSet pDMD, "PFM_Image", "Multipliers\\pf4x.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0, 'yalign':0,'ypos':0,'xpos':0}"
			P4xLight.State = 1
		Case 5
			PuPlayer.LabelSet pDMD, "PFM_Image", "Multipliers\\pf5x.png",1,"{'mt':2,'width':100, 'height':100,'xalign':0, 'yalign':0,'ypos':0,'xpos':0}"	
			P5xLight.State = 1
		Case Else
			PuPlayer.LabelSet pDMD, "PFM_Image", "Multipliers\\Clear.png",0,"{'mt':2,'width':100, 'height':100,'xalign':0, 'yalign':0,'ypos':0,'xpos':0}"
	End Select


'	if VRRoom > 0 Then
'		PuPlayer.LabelSet pDMD,"PFMValue",nPlayfieldX(nPlayer) &"X ",1,	"{'mt':2,'fonth':"& VR_BonusSize &", 'color':" & cGold &"}"
'	Else
		PuPlayer.LabelSet pDMD,"PFMValue",nPlayfieldX(CurrentPlayer) &"X ",1,"{'mt':2,'fonth':14,'ypos':84,'xpos':92}"
'	End If

	if nPlayfieldX(CurrentPlayer) = 1 Then Exit Sub

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{green}:PFX Increased: "&nPlayfieldX(CurrentPlayer) &"X "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If

End Sub

'%%%%%%%%%%%%%%%%
'  Bobby Code   '
'%%%%%%%%%%%%%%%%
Sub MVTarget_Hit()
	'Debug.print "Ball: " &activeball.vely
	if activeball.vely < 0 Then debug.print "DIDNT COUNT" : Exit Sub ' dont count if ball is dropping from above
	if Mode(CurrentPlayer,2) = 2 Then BobbyTargetCount = BobbyTargetCount + 1
	if Mode(CurrentPlayer,12) = 2 Then BobbyTargetCount = BobbyTargetCount + 1
	'Debug.print "Total Bobby Hits: " & BobbyTargetCount
	CheckBobbyTargetCount
End Sub

Sub CheckBobbyTargetCount
	Select Case BobbyTargetCount
		Case 1:
			if Mode(CurrentPlayer,2) = 2 Then nPeggyMode2Progress = 1:PupProgressPeggy2
			MVTargetDescend.Enabled = 1
			vpmtimer.addtimer 2000, "MVTargetRise.Enabled = 1 '"
		Case 2:
			if Mode(CurrentPlayer,2) = 2 Then nPeggyMode2Progress = 2
			MVTargetDescend.Enabled = 1
			vpmtimer.addtimer 2000, "MVTargetRise.Enabled = 1 '"
		Case 3:
			if Mode(CurrentPlayer,2) = 2 Then WinPeggy2
			if Mode(CurrentPlayer,12) = 2 Then Mode(CurrentPlayer,12) = 1 : WinBobby
			MVTargetMove.Enabled = 0
			MVTargetDescend.Enabled = 1
	End Select
End Sub

Const BobbySpeed = 50
MVTargetDescend.Interval = 2
MVTargetRise.Interval = 2


Sub MVTargetDescend_Timer()
	MVTarget.z = MVTarget.z - 1 
	if MVTarget.z < -199 Then MVTargetDescend.Enabled = 0
End Sub

Sub MVTargetRise_Timer()
	MVTarget.z = MVTarget.z + 1
	if MVTarget.z > -13 Then MVTargetRise.Enabled = 0
End Sub

Sub PrepareBobbyMovement
	Dim currPos
	CurrPos = MVTarget.rotY
	MVTargetRise.Enabled = 1

	if MVTarget.rotY > 180 Then
		bMovingLeft = True
	Else
		bMovingRight = True
	End If
	MVTargetMove.Interval = BobbySpeed
	if Mode(CurrentPlayer,12) = 2 Then MVTargetMove.Interval = 64
	if Mode(CurrentPlayer,2) = 2 Then MVTargetMove.Interval = 40

	MVTargetMove.Enabled = 1
End Sub

Dim bMovingRight, bMovingLeft
Sub MVTargetMove_Timer

	MapBobbyTarget
	if bMovingRight Then
		if mvTarget.rotY > 208 Then 
			'Debug.print "Start moving left"
			bMovingRight = False
			bMovingLeft = True
		Else
			MVTarget.rotY = MVTarget.rotY + 1
		End If
	Else
		if mvTarget.rotY < 154 Then
			'Debug.print "Started to move Right"
			bMovingRight = True
			bMovingLeft = False
		Else
			MVTarget.rotY = MVTarget.rotY - 1
		End If
	End If
End Sub

Sub MapBobbyTarget
	if MVTargetMove.Enabled = 0 Then Exit Sub

	Select Case MVTarget.rotY
		Case 150,151,152,153,154,155,156:
'			Debug.print "Zone 1: "&MVTarget.rotY
			p10.collidable = False
			p1.collidable = True
			p2.collidable = False
		Case 157,158,159,160,161,162:
'			Debug.print "Zone 2: " &MVTarget.rotY
			p1.collidable = False
			p2.collidable = True
			p3.collidable = False
		Case 163,164,165,166,167,168:
'			Debug.print "Zone 3: " &MVTarget.rotY
			p2.collidable = False
			p3.collidable = True
			p4.collidable = False
		Case 169,170,171,172,173,174:
'			Debug.print "Zone 4: " &MVTarget.rotY
			p3.collidable = False
			p4.collidable = True
			p5.collidable = False
		Case 175,176,177,178,179,180:
'			Debug.print "Zone 5: " &MVTarget.rotY
			p4.collidable = False
			p5.collidable = True
			p6.collidable = False
		Case 181,182,183,184,185,186:
'			Debug.print "Zone 6: " &MVTarget.rotY
			p5.collidable = False
			p6.collidable = True
			p7.collidable = False
		Case 187,188,189,190,191,192:
'			Debug.print "Zone 7: " &MVTarget.rotY
			p6.collidable = False
			p7.collidable = True
			p8.collidable = False
		Case 193,194,195,196,197,198:
'			Debug.print "Zone 8: " &MVTarget.rotY
			p7.collidable = False
			p8.collidable = True
			p9.collidable = False
		Case 199,200,201,202,203,204:
'			Debug.print "Zone 9: " &MVTarget.rotY
			p8.collidable = False
			p9.collidable = True
			p10.collidable = False
		Case 205,206,207,208,209,210:
'			Debug.print "Zone 10: " &MVTarget.rotY
			p9.collidable = False
			p10.collidable = True
			p1.collidable = False
		Case Else
'			Debug.print "Zone Else: "&MVTarget.rotY
	End Select
End Sub

Sub RemoveCollisionsBobby
	MVTargetMove.Enabled = 0
	p1.collidable = False
	p2.collidable = False
	p3.collidable = False
	p4.collidable = False
	p5.collidable = False
	p6.collidable = False
	p7.collidable = False
	p8.collidable = False
	p9.collidable = False
	p10.collidable = False
End Sub

Sub aBobbyTargets_Hit(Index)
'	Debug.print "HIT BOBBY : " &Index

	MVTarget_Hit
'	PuPlayer.LabelSet pDMD,"Line1b","HIT BOBBY :" &BobbyTargetCount,1,"{'mt':2,'color': "&cWhite&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':35.0}"
'	PuPlayer.LabelSet pDMD,"Line2b","At POSITION: " &Index,1,"{'mt':2,'color': "&cWhite&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':65.0}"
'	vpmtimer. addtimer 3000, "clearbobbymsg '"
End Sub

Sub clearbobbymsg
	PuPlayer.LabelSet pDMD,"Line1b","",0,"{'mt':2,'color': "&cWhite&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':50.0}"
	PuPlayer.LabelSet pDMDVideo,"Line2b","",0,"{'mt':2,'color': "&cWhite&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':50.0}"
End Sub

'%%%%%%%%%%%%%%%%%
' END Bobby Code '
'%%%%%%%%%%%%%%%%%
'C1 - Boomhauer
'C2 - Beer
'C3 - Ladybird
'C4 - LawnMower
'C5 - Hank
'C6 - Propane
'C7 - Bill
'C8 - BBQ
'C9 - Cotton
'C10 - Luanne
'#######################################################################
''''''''''''       START OF MAIN SHOTS    ''''''''''''''''''''''''''''''
'#######################################################################
'-------------------
'      DALE LANE
'-------------------
Sub Target004_Hit()
	if Tilted Then Exit Sub

	Select Case Mode(CurrentPlayer,0)
		Case 0
			anDaleLights1(CurrentPlayer) = anDaleLights1(CurrentPlayer) + 1
			if anDaleLights1(CurrentPlayer) > 2 Then anDaleLights1(CurrentPlayer) = 3 
			DisplayDaleProgress
		Case 9
			if Mode(CurrentPlayer,9) = 2 Then nHankMode3Progress = nHankMode3Progress + 1 : CheckHankMode3Progress
		Case 12


	End Select

	LastSwitchHit = "Target004"
Debug.print "Last Switch: "&LastSwitchHit
End Sub

Sub Target005_Hit()
	if Tilted Then Exit Sub

	Select Case Mode(CurrentPlayer,0)
		Case 0
			anDaleLights2(CurrentPlayer) = anDaleLights2(CurrentPlayer) + 1
			if anDaleLights2(CurrentPlayer) > 2 Then anDaleLights2(CurrentPlayer) = 3
			DisplayDaleProgress
		Case 9
			if Mode(CurrentPlayer,9) = 2 Then nHankMode3Progress = nHankMode3Progress + 1 : CheckHankMode3Progress

		Case 12

	End Select

	LastSwitchHit = "Target005"
Debug.print "Last Switch: "&LastSwitchHit
End Sub

Sub Target006_Hit()
	if Tilted Then Exit Sub

	Select Case Mode(CurrentPlayer,0)
		Case 0
			anDaleLights3(CurrentPlayer) = anDaleLights3(CurrentPlayer) + 1
			if anDaleLights3(CurrentPlayer) > 2 Then anDaleLights3(CurrentPlayer) = 3 
			DisplayDaleProgress
		Case 9
			if Mode(CurrentPlayer,9) = 2 Then nHankMode3Progress = nHankMode3Progress + 1 : CheckHankMode3Progress
		Case 12

	End Select

	LastSwitchHit = "Target006"
Debug.print "Last Switch: "&LastSwitchHit
End Sub

Sub DaleTrigger_Hit()
	if Tilted Then Exit Sub
	OrbitCount = OrbitCount + 1

	Select Case Mode(CurrentPlayer,0)
		Case 0
			if bSkillshotReady And LastTriggerHit = "PlungerTrigger" Then AwardSkillshot
			if bDaleReady Then StartDale			
		Case 5
			if Mode(CurrentPlayer,5) = 2 Then addscore SCORE_JACKPOT ' Boom Mode
		Case 10
			if Mode(CurrentPlayer,10) = 2 Then addscore SCORE_SUPER_JACKPOT ' Dale Mode
		Case 19	
			if bWizardMode Then
				addscore SCORE_SUPER_JACKPOT
			end If
	End Select


	LastSwitchHit = "DaleTrigger"
Debug.print "Last Switch: "&LastSwitchHit
End Sub

'-------------------
'  BOOMHAUER Orbit
'-------------------
Sub BoomPrepTrigger_Hit()
	LastTriggerHit = "BoomPrepTrigger"
End Sub
Sub BoomTrigger_Hit()
	if Tilted Then Exit Sub

	Debug.print "Ball Info Boom:" &activeball.vely
'	if LastSwitchHit = "LuanneTrigger" Then
'	if activeball.vely < -1  Then 
	if LastTRiggerHit = "BoomPrepTrigger" Then
	OrbitCount = OrbitCount + 1

	LastTriggerHit = "BoomTrigger"
	Debug.print "Last Switch Boom: "&LastTriggerHit
		Select Case Mode(CurrentPlayer,0)
			Case 0
				anBoomLights(CurrentPlayer,0) = anBoomLights(CurrentPlayer,0) + 1
				if anBoomLights(CurrentPlayer,0) > 9 Then anBoomLights(CurrentPlayer,0) = 9 : bBoomReady = True : StartBoom' Start Mode	
				anBoomLights(CurrentPlayer,anBoomLights(CurrentPlayer,0)) = 1
				DisplayBoomProgress
			Case 1
				if Mode(CurrentPlayer,1) = 2 Then
					if nPeggyMode1Progress(1) = 0 Then
						nPeggyMode1Progress(1) = 1
						addscore SCORE_MODE_PROGRESS
						LightSeqBoom.StopPlay()
						CheckPeggyProgress
					End If
				End If
			Case 8
				if Mode(CurrentPlayer,8) = 2 And bHankMode2Orbit Then 
					nHankMode2Progress = nHankMode2Progress + 1
					CheckHankMode2Progress
					AddScore SCORE_MODE_PROGRESS
					StopModeOrbitSeq
					LightEffect 19
				End If
			Case 11
				if Mode(CurrentPlayer,11) = 2 And nCottonActive = 1 Then 
					nCottonShots(1) = 1
					nCottonModeProgress = nCottonModeProgress + 1
					nNewCottonShot = rndNumNot(1,10,nCottonActive)
					ChaseLights
					ChaseTimerReset
				End If
			Case 12
				if Mode(CurrentPlayer,12) = 2 Then 
					if BobbyTargetCount = 0 Then
						addscore SCORE_SUPER_JACKPOT ' Bobby Mode
					Elseif BobbyTargetCount < 2 Then
						addscore SCORE_JACKPOT ' Bobby Mode
					End If
				End If
			Case 15
			Case 16
				if Mode(CurrentPlayer,16) = 2 Then
					if nMowerProgress(1) = 0 Then
						nMowerProgress(1) = 1
						LightSeqBoom.StopPlay()
						CheckMowerProgress
					End If
				End If
				if mode(currentplayer,15) = 2 Then nBeerMode3Progress = nBeerMode3Progress + 1 :  CheckBeerMode3
			Case 19	
				if bWizardMode Then
					addscore SCORE_SUPER_JACKPOT
				end If
		End Select
'		End If
	End If

'	LastSwitchHit = "BoomTrigger"
'Debug.print "Last Switch: "&LastSwitchHit
End Sub

'------------------
'    YEP Target
'------------------
Sub Target008_Hit()
	if Tilted Then Exit Sub

	DOF 138, DOFPulse

	playsound "yep"
	light113.State = 2
	BallHandlingQueue.Add "light113.State = 0","Light113.State = 0",1,1500,0,0,0,False
	Select Case Mode(CurrentPlayer,0)
		Case 0
			'anYEP(CurrentPlayer,0) = anYEP(CurrentPlayer,0) + 1
			'if anYEP(CurrentPlayer,0) > 2 Then anYEP(CurrentPlayer,0) = 3
			'anYEP(CurrentPlayer,anYEP(CurrentPlayer,0)) = 1

			if nYepReady = 1 Then 
				nYepReady = 2
				anYEP(CurrentPlayer,1) = 0
				anYEP(CurrentPlayer,2) = 0
				anYEP(CurrentPlayer,3) = 0
			End If
		Case 1

		Case 9
			if Mode(CurrentPlayer,9) = 2 Then nHankMode3Progress = nHankMode3Progress + 1 : CheckHankMode3Progress
		Case 12


	End Select



	LastSwitchHit = "Target008"
Debug.print "Last Switch: "&LastSwitchHit
	'CheckYEP
End Sub

'------------------
'    BEER Ramp
'------------------
Sub RampTrigger001a_Hit()
	WireRampOff	 'Turn off the Plastic Ramp Sound

    If Tilted Then Exit Sub

	DOF 140, DOFPulse

	RampCount = RampCount + 1

	Select Case Mode(CurrentPlayer,0)
		Case 0
			anBeerLights(CurrentPlayer,0) = anBeerLights(CurrentPlayer,0) + 1
			if anBeerLights(CurrentPlayer,0) > 4 Then anBeerLights(CurrentPlayer,0) = 5 :CheckBeer ' Start Mode	
			anBeerLights(CurrentPlayer,anBeerLights(CurrentPlayer,0)) = 1
		Case 1
			if Mode(CurrentPlayer,1) = 2 Then
					if nPeggyMode1Progress(2) = 0 Then
						nPeggyMode1Progress(2) = 1
						addscore SCORE_MODE_PROGRESS
						LightSeqBeer2.StopPlay()
						CheckPeggyProgress
					End If
			End If
		Case 8
			if Mode(CurrentPlayer,8) = 2 And bHankMode2Ramp Then 
				nHankMode2Progress = nHankMode2Progress + 1
				CheckHankMode2Progress
				AddScore SCORE_MODE_PROGRESS
				StopModeRampSeq
				LightEffect 29
			End If
		Case 11
			if Mode(CurrentPlayer,11) = 2 And nCottonActive = 2 Then 
				nCottonShots(2) = 1
				nCottonModeProgress = nCottonModeProgress + 1
				nNewCottonShot = rndNumNot(1,10,nCottonActive)
				ChaseLights
				ChaseTimerReset
			End If
		Case 12
			if Mode(CurrentPlayer,12) = 2 Then 
				if BobbyTargetCount = 0 Then
					addscore SCORE_SUPER_JACKPOT ' Bobby Mode
				Elseif BobbyTargetCount < 3 Then
					addscore SCORE_JACKPOT ' Bobby Mode
				End If
			End If
		Case 14
			if mode(currentplayer,14) = 2 Then nBeerMode2Progress = nBeerMode2Progress + 1 :  CheckBeerMode2
		Case 16
			if Mode(CurrentPlayer,16) = 2 Then
				if nMowerProgress(2) = 0 Then
					nMowerProgress(2) = 1
					LightSeqBeer2.StopPlay()
					CheckMowerProgress
				End If
			End If
		Case 18
			if Mode(CurrentPlayer,18) = 2 Then 	
				nBBQModeProgress = nBBQModeProgress + 1
				CheckBBQProgress
			End If
		Case 19
			if Mode(CurrentPlayer,19) = 2 Then addscore SCORE_SUPER_JACKPOT
	End Select
	LastSwitchHit = "BeerRamp"
Debug.print "Last Switch: "&LastSwitchHit
End Sub

Sub CheckBeer
	if Mode(CurrentPlayer,0) <> 0 Then Exit Sub  ' Only one mode can be active at a time

	if Mode(CurrentPlayer,13) = 0 Then 
		StartBeer1
	Elseif Mode(CurrentPlayer,14) = 0 Then
		StartBeer2
	Elseif Mode(CurrentPlayer,15) = 0 Then
		StartBeer3
	End If
End Sub
'-------------------
' DRINK ALAMO Target
'-------------------

Sub Target001_Hit()
	if Tilted Then Exit Sub

	DOF 138, DOFPulse

	anDrinkAlamo(CurrentPlayer,0) = anDrinkAlamo(CurrentPlayer,0) + 1
	if anDrinkAlamo(CurrentPlayer,0) > 9 Then anDrinkAlamo(CurrentPlayer,0) = 10 : bReadyDrinkAlamo = True : StartDrinkAlamo ' Start Mode	
	anDrinkAlamo(CurrentPlayer,anDrinkAlamo(CurrentPlayer,0)) = 1

	Select Case Mode(CurrentPlayer,0)
		Case 0
'			anDrinkAlamo(CurrentPlayer,0) = anDrinkAlamo(CurrentPlayer,0) + 1
'			if anDrinkAlamo(CurrentPlayer,0) > 9 Then anDrinkAlamo(CurrentPlayer,0) = 10 : StartDrinkAlamo ' Start Mode	
'			anDrinkAlamo(CurrentPlayer,anDrinkAlamo(CurrentPlayer,0)) = 1
		Case 1

		Case 9
			if Mode(CurrentPlayer,9) = 2 Then nHankMode3Progress = nHankMode3Progress + 1 : CheckHankMode3Progress
		Case 12

	End Select

	LastSwitchHit = "Target011"
Debug.print "Last Switch: "&LastSwitchHit
End Sub

'------------------
'  LADYBIRD trigger
'------------------
Sub LadyBirdTrigger_Hit()
	if Tilted Then Exit Sub

	DOF 140, DOFPulse

	if LastTriggerHit = "Spinner001" Then WobbleBeerCan.Enabled = 1

	if LastTriggerHit <> "LuannePrepTrigger" Then 	OrbitCount = OrbitCount + 1

		Select Case Mode(CurrentPlayer,0)
			Case 0
				if LastTriggerHit <> "LuannePrepTrigger" Then
					anLadybird(CurrentPlayer,0) = anLadybird(CurrentPlayer,0) + 1
					if anLadybird(CurrentPlayer,0) > 7 Then anLadybird(CurrentPlayer,0) = 8 : StartLadybird' Start Mode	
					anLadybird(CurrentPlayer,anLadybird(CurrentPlayer,0)) = 1
				End If
				if LastTriggerHit = "LuannePrepTrigger" Then
					anLuanneLights(CurrentPlayer,0) = anLuanneLights(CurrentPlayer,0) + 1
					if anLuanneLights(CurrentPlayer,0) > 6 Then anLuanneLights(CurrentPlayer,0) = 6 : bLuanneReady = True : StartLuanne' Start Mode
					anLuanneLights(CurrentPlayer,anLuanneLights(CurrentPlayer,0)) = 1
					DisplayLuanneProgress
				End If
			Case 1
				if Mode(CurrentPlayer,1) = 2 And LastTriggerHit <> "LuannePrepTrigger" Then
					if nPeggyMode1Progress(3) = 0 Then
						nPeggyMode1Progress(3) = 1
						addscore SCORE_MODE_PROGRESS
						LightSeqLadybird.StopPlay()
						CheckPeggyProgress
					End If
				End If
				if Mode(CurrentPlayer,1) = 2 And LastTriggerHit = "LuannePrepTrigger" Then
					if nPeggyMode1Progress(10) = 0 Then
						nPeggyMode1Progress(10) = 1
						addscore SCORE_MODE_PROGRESS
						LightSeqLuanne.StopPlay()
						CheckPeggyProgress
					End If
				End If
			Case 3
				if Mode(CurrentPlayer,3) = 2 And LastTriggerHit <> "LuannePrepTrigger" Then
					if nPeggyMode3Progress(3) = 0 Then
						nPeggyMode3Progress(3) = 1
						addscore SCORE_MODE_PROGRESS
						LightSeqLadybird.StopPlay()
						CheckPeggy3Progress
					End If
				End If
				if Mode(CurrentPlayer,3) = 2 And LastTriggerHit = "LuannePrepTrigger" Then
					if nPeggyMode3Progress(4) = 0 Then
						nPeggyMode3Progress(4) = 1
						addscore SCORE_MODE_PROGRESS
						LightSeqLuanne.StopPlay()
						CheckPeggy3Progress
					End If
				End If
			Case 8
				if Mode(CurrentPlayer,8) = 2 And LastTriggerHit = "LuannePrepTrigger" Then
					if bHankMode2Orbit Then
						nHankMode2Progress = nHankMode2Progress + 1
						Addscore SCORE_MODE_PROGRESS
						CheckHankMode2Progress
						StopModeOrbitSeq
						LightEffect 19
					End If
				End If
			Case 11
				if Mode(CurrentPlayer,11) = 2 And LastTriggerHit <> "LuannePrepTrigger"Then 
					if nCottonActive = 3 Then 
						nCottonShots(3) = 1
						nCottonModeProgress = nCottonModeProgress + 1
						nNewCottonShot = rndNumNot(1,10,nCottonActive)
						ChaseLights
						ChaseTimerReset
					End If
				End If
			Case 15
				if mode(currentplayer,15) = 2 Then nBeerMode3Progress = nBeerMode3Progress + 1 :  CheckBeerMode3
			Case 16
				if Mode(CurrentPlayer,16) = 2 And LastTriggerHit <> "LuannePrepTrigger" Then
					if nMowerProgress(3) = 0 Then
						nMowerProgress(3) = 1
						LightSeqLadybird.StopPlay()
						CheckMowerProgress
					End If
				End If
			Case 17
				if Mode(CurrentPlayer,17) = 2 And LastTriggerHit <> "LuannePrepTrigger" Then playsound "Jackpot1"
			Case 19
				if Mode(CurrentPlayer,19) = 2 And LastTriggerHit <> "LuannePrepTrigger" Then addscore SCORE_SUPER_JACKPOT
				
		End Select

'	End If

	LastSwitchHit = "LadybirdTrigger"
Debug.print "Last Switch: "&LastSwitchHit
End Sub

'------------------
'  LAWNMOWER Ramp
'------------------
Sub MowerPrepTrigger_Hit

	LastSwitchHit = "MowerPrepTrigger"
	WireRampOn False	 'Play Plastic Ramp Sound

End Sub

Sub RampTrigger005a_Hit()	
'	WireRampOff	 'Turn off the Plastic Ramp Sound

    If Tilted Then Exit Sub

	DOF 140, DOFPulse
	'if activeball.vely > 20 Then Activeball.vely = 8  ' slow the ball down
	RampCount = RampCount + 1

	'if LastSwitchHit = "MowerPrepTrigger" Then


	'if activeball.vely > 0 Then
		Select Case Mode(CurrentPlayer,0)
			Case 0
				anLawnMower(CurrentPlayer,0) = anLawnMower(CurrentPlayer,0) + 1
				if anLawnMower(CurrentPlayer,0) > 5 Then anLawnMower(CurrentPlayer,0) = 5 ' Start Mode	
				anLawnMower(CurrentPlayer,anLawnMower(CurrentPlayer,0)) = 1
		if anLawnMower(CurrentPlayer,0) = 5 Or bLockEngaged Then updateLockKickers

		if bLockEngaged Then
			' need to remove the locked ball from playfield Count
			ballsonplayfield = Ballsonplayfield - 1
			Debug.print "###############################"
			Debug.print "SHOULD BE " &ballsonplayfield & "Balls in play"
			Debug.print "###############################"
		Else
			Debug.Print "Moving Mower"
			MoveLawnMowerForward
		End If

			Case 1
				if Mode(CurrentPlayer,1) = 2 Then
					if nPeggyMode1Progress(4) = 0 Then
						nPeggyMode1Progress(4) = 1
						addscore SCORE_MODE_PROGRESS
						LightSeqLawnmower.StopPlay()
						CheckPeggyProgress
					End If
				End If
			Case 8
				if Mode(CurrentPlayer,8) = 2 And bHankMode2Ramp Then 
					nHankMode2Progress = nHankMode2Progress + 1
					CheckHankMode2Progress
					AddScore SCORE_MODE_PROGRESS
					StopModeRampSeq
					LightEffect 29
				End If
			Case 11
				if Mode(CurrentPlayer,11) = 2 And nCottonActive = 4 Then 
					nCottonShots(4) = 1
					nCottonModeProgress = nCottonModeProgress + 1
					nNewCottonShot = rndNumNot(1,10,nCottonActive)
					ChaseLights
					ChaseTimerReset
				End If
			Case 14
				if mode(currentplayer,14) = 2 Then nBeerMode2Progress = nBeerMode2Progress + 1 :  CheckBeerMode2
			Case 16
				if Mode(CurrentPlayer,16) = 2 Then
					if nMowerProgress(4) = 0 Then
						nMowerProgress(4) = 1
						LightSeqLawnmower.StopPlay()
						CheckMowerProgress
					End If
				End If
			Case 18
				if Mode(CurrentPlayer,18) = 2 Then 	
					nBBQModeProgress = nBBQModeProgress + 1
					CheckBBQProgress
				End If
			Case 19
				if Mode(CurrentPlayer,19) = 2 Then addscore SCORE_SUPER_JACKPOT

		End Select
	'End If

	LastSwitchHit = "MowerRamp"

End Sub

'------------------
'    HANK Ramp
'------------------
Sub RampTrigger002c_Hit()
	if Tilted Then Exit Sub

'	if mode(currentplayer,14) = 2 Then nBeerMode2Progress = nBeerMode2Progress + 1 :  CheckBeerMode2

	LastSwitchHit = "HankRamp"
Debug.print "Last Switch: "&LastSwitchHit
End Sub

Sub CheckHank
	if Mode(CurrentPlayer,0) <> 0 Then Exit Sub  ' Only one mode can be active at a time

	if Mode(CurrentPlayer,8) = 2 And bHankMode2Ramp Then 
		nHankMode2Progress = nHankMode2Progress + 1
		CheckHankMode2Progress
		AddScore SCORE_MODE_PROGRESS
		StopModeRampSeq
		LightEffect 29
	End If

	if Mode(CurrentPlayer,7) = 0 Then 
		StartHank1
	Elseif Mode(CurrentPlayer,8) = 0 Then
		StartHank2
	Elseif Mode(CurrentPlayer,9) = 0 Then
		StartHank3
	End If



End Sub

'------------------
'    PROPANE Ramp
'------------------
Sub RampTrigger003a_Hit()
	WireRampOff	 'Turn off the Plastic Ramp Sound

    If Tilted Then Exit Sub


	DOF 140, DOFPulse

	RampCount = RampCount + 1

	Select Case Mode(CurrentPlayer,0)
		Case 0
			anPropane(CurrentPlayer,0) = anPropane(CurrentPlayer,0) + 1
			' Hit Yep target to light collect multiplier and set nYEPReady = 2
			if anPropane(CurrentPlayer,0) > 6 Then 
				anPropane(CurrentPlayer,0) = 7
				nYepReady = 1
				anYEP(CurrentPlayer,1) = 1
				anYEP(CurrentPlayer,2) = 1
				anYEP(CurrentPlayer,3) = 1
			End If
			anPropane(CurrentPlayer,anPropane(CurrentPlayer,0)) = 1
		Case 1
			if Mode(CurrentPlayer,1) = 2 Then
				if nPeggyMode1Progress(6) = 0 Then
					nPeggyMode1Progress(6) = 1
					addscore SCORE_MODE_PROGRESS
					LightSeqPropane.StopPlay()
					CheckPeggyProgress
				End If
			End If
		Case 8
			if Mode(CurrentPlayer,8) = 2 And bHankMode2Ramp Then 
				nHankMode2Progress = nHankMode2Progress + 1
				CheckHankMode2Progress
				AddScore SCORE_MODE_PROGRESS
				StopModeRampSeq
				LightEffect 29
			End If
		Case 11
			if Mode(CurrentPlayer,11) = 2 And nCottonActive = 6 Then 
				nCottonShots(6) = 1
				nCottonModeProgress = nCottonModeProgress + 1
				nNewCottonShot = rndNumNot(1,10,nCottonActive)
				ChaseLights
				ChaseTimerReset
			End If
		Case 12
			if Mode(CurrentPlayer,12) = 2 Then 
				if BobbyTargetCount = 0 Then
					addscore SCORE_SUPER_JACKPOT ' Bobby Mode
				Elseif BobbyTargetCount < 3 Then
					addscore SCORE_JACKPOT ' Bobby Mode
				End If
			End If
		Case 14
			if mode(currentplayer,14) = 2 Then nBeerMode2Progress = nBeerMode2Progress + 1 :  CheckBeerMode2
		Case 16
			if Mode(CurrentPlayer,16) = 2 Then
				if nMowerProgress(6) = 0 Then
					nMowerProgress(6) = 1
					LightSeqPropane.StopPlay()
					CheckMowerProgress
				End If
			End If
		Case 18
			if Mode(CurrentPlayer,18) = 2 Then 	
				nBBQModeProgress = nBBQModeProgress + 1
				CheckBBQProgress
			End If
		Case 19
			if Mode(CurrentPlayer,19) = 2 Then addscore SCORE_SUPER_JACKPOT
	End Select
	LastSwitchHit = "PropaneRamp"
Debug.print "Last Switch: "&LastSwitchHit
End Sub

'-------------------
'      BILL Orbit
'-------------------
Sub BillTrigger_Hit()
	if Tilted Then Exit Sub

	DOF 140, DOFPulse


	debug.print "LS:" &LastSwitchHit
	if LastSwitchHit <> "CottonTrigger" Then
		OrbitCount = OrbitCount + 1

		Select Case Mode(CurrentPlayer,0)
			Case 0
				anBillLights(CurrentPlayer,0) = anBillLights(CurrentPlayer,0) + 1
				if anBillLights(CurrentPlayer,0) > 9 Then anBillLights(CurrentPlayer,0) = 9 : bBillReady = True : StartBill ' Start Mode	
				anBillLights(CurrentPlayer,anBillLights(CurrentPlayer,0)) = 1
				DisplayBillProgress
			Case 1
				if Mode(CurrentPlayer,1) = 2 Then
					if nPeggyMode1Progress(7) = 0 Then
						nPeggyMode1Progress(7) = 1
						addscore SCORE_MODE_PROGRESS
						LightSeqBill.StopPlay()
						CheckPeggyProgress
					End If
				End If
			Case 5
				if Mode(CurrentPlayer,5) = 2 Then addscore SCORE_JACKPOT ' Boom Mode
			Case 6
				if Mode(CurrentPlayer,6) = 2 Then addscore SCORE_SUPER_JACKPOT
			Case 8
				if Mode(CurrentPlayer,8) = 2 And bHankMode2Orbit Then 
					nHankMode2Progress = nHankMode2Progress + 1
					CheckHankMode2Progress
					AddScore SCORE_MODE_PROGRESS
					StopModeOrbitSeq
					LightEffect 19
				End If
			Case 11
				if Mode(CurrentPlayer,11) = 2 And nCottonActive = 7 Then 
					nCottonShots(7) = 1
					nCottonModeProgress = nCottonModeProgress + 1
					nNewCottonShot = rndNumNot(1,10,nCottonActive)
					ChaseLights
					ChaseTimerReset
				End If
			Case 12
				if Mode(CurrentPlayer,12) = 2 Then 
					if BobbyTargetCount = 0 Then
						addscore SCORE_SUPER_JACKPOT ' Bobby Mode
					Elseif BobbyTargetCount < 2 Then
						addscore SCORE_JACKPOT ' Bobby Mode
					End If
				End If
			Case 15
				if mode(currentplayer,15) = 2 Then nBeerMode3Progress = nBeerMode3Progress + 1 :  CheckBeerMode3
			Case 16
				if Mode(CurrentPlayer,16) = 2 Then
					if nMowerProgress(7) = 0 Then
						nMowerProgress(7) = 1
						LightSeqBill.StopPlay()
						CheckMowerProgress
					End If
				End If
			Case 19
				if Mode(CurrentPlayer,19) = 2 Then addscore SCORE_SUPER_JACKPOT
		End Select



	End If


	LastSwitchHit = "BillTrigger"
Debug.print "Last Switch: "&LastSwitchHit

	BallHandlingQueue.Add "LastSwitchHit = Empty","LastSwitchHit = ""Empty""",50,400,0,0,0,False
	
End Sub

'------------------
'  BBQ FEAST Ramp
'------------------
Sub RampTrigger004b_Hit()   ' Ramp into Grill
	WireRampOff	 'Turn off the Plastic Ramp Sound

    If Tilted Then Exit Sub

	DOF 140, DOFPulse

	RampCount = RampCount + 1

	if bEBReady Then AwardExtraBall

	Select Case Mode(CurrentPlayer,0)
		Case 0
			anBBQFeast(CurrentPlayer,0) = anBBQFeast(CurrentPlayer,0) + 1
			if anBBQFeast(CurrentPlayer,0) > 4 Then anBBQFeast(CurrentPlayer,0) = 5 : CheckBBQFeast ' Start Mode	
			anBBQFeast(CurrentPlayer,anBBQFeast(CurrentPlayer,0)) = 1

			if nYepReady = 2 Then IncreaseMulitpliers

		Case 1
			if Mode(CurrentPlayer,1) = 2 Then
				if nPeggyMode1Progress(8) = 0 Then
					nPeggyMode1Progress(8) = 1
					addscore SCORE_MODE_PROGRESS
					LightSeqBBQ2.StopPlay()
					CheckPeggyProgress
				End If
			End If
		Case 8
			if Mode(CurrentPlayer,8) = 2 And bHankMode2Ramp Then 
				nHankMode2Progress = nHankMode2Progress + 1
				CheckHankMode2Progress
				AddScore SCORE_MODE_PROGRESS
				StopModeRampSeq
				LightEffect 29
			End If
		Case 11
			if Mode(CurrentPlayer,11) = 2 And nCottonActive = 8 Then 
				nCottonShots(8) = 1
				nCottonModeProgress = nCottonModeProgress + 1
				nNewCottonShot = rndNumNot(1,10,nCottonActive)
				ChaseLights
				ChaseTimerReset
			End If
		Case 12
			if Mode(CurrentPlayer,12) = 2 Then 
				if BobbyTargetCount = 0 Then
					addscore SCORE_SUPER_JACKPOT ' Bobby Mode
				Elseif BobbyTargetCount < 3 Then
					addscore SCORE_JACKPOT ' Bobby Mode
				End If
			End If
		Case 14
			if mode(currentplayer,14) = 2 Then nBeerMode2Progress = nBeerMode2Progress + 1 :  CheckBeerMode2
		Case 16
			if Mode(CurrentPlayer,16) = 2 Then
				if nMowerProgress(8) = 0 Then
					nMowerProgress(8) = 1
					LightSeqBBQ2.StopPlay()
					CheckMowerProgress
				End If
			End If
		Case 18
			if Mode(CurrentPlayer,18) = 2 Then 	
				nBBQModeProgress = nBBQModeProgress + 1
				CheckBBQProgress
			End If
		Case 19
			if Mode(CurrentPlayer,19) = 2 Then addscore SCORE_SUPER_JACKPOT
	End Select

	LastSwitchHit = "BBQRamp"
Debug.print "Last Switch: "&LastSwitchHit
End Sub

Sub CheckBBQFeast
	Light009.State = 2
	StartBBQFeast
End Sub



'-------------------
'   COTTON Orbit
'-------------------
Sub CottonTrigger_Hit()
	if Tilted Then Exit Sub

	DOF 140, DOFPulse

	if LastSwitchHit <> "BillTrigger" Then
		OrbitCount = OrbitCount + 1

		Select Case Mode(CurrentPlayer,0)
			Case 0
				anCottonLights(CurrentPlayer,0) = anCottonLights(CurrentPlayer,0) + 1
				if anCottonLights(CurrentPlayer,0) > 6 Then anCottonLights(CurrentPlayer,0) = 6 ' Start Mode	
				anCottonLights(CurrentPlayer,anCottonLights(CurrentPlayer,0)) = 1
				DisplayCottonProgress
			Case 1
				if Mode(CurrentPlayer,1) = 2 Then
					if nPeggyMode1Progress(9) = 0 Then
						nPeggyMode1Progress(9) = 1
						addscore SCORE_MODE_PROGRESS
						LightSeqCotton.StopPlay()
						CheckPeggyProgress
					End If
				End If
			Case 8
				if Mode(CurrentPlayer,8) = 2 And bHankMode2Orbit Then 
					nHankMode2Progress = nHankMode2Progress + 1
					CheckHankMode2Progress
					AddScore SCORE_MODE_PROGRESS
					StopModeOrbitSeq
					LightEffect 19
				End If
			Case 11
				if Mode(CurrentPlayer,11) = 2 And nCottonActive = 9 Then 
					nCottonShots(9) = 1
					nCottonModeProgress = nCottonModeProgress + 1
					nNewCottonShot = rndNumNot(1,10,nCottonActive)
					ChaseLights
					ChaseTimerReset
				End If
			Case 12
				if Mode(CurrentPlayer,12) = 2 And BobbyTargetCount = 0 Then addscore SCORE_SUPER_JACKPOT ' Bobby Mode
			Case 15
				if mode(currentplayer,15) = 2 Then nBeerMode3Progress = nBeerMode3Progress + 1 :  CheckBeerMode3
			Case 16
				if Mode(CurrentPlayer,16) = 2 Then
					if nMowerProgress(9) = 0 Then
						nMowerProgress(9) = 1
						LightSeqCotton.StopPlay()
						CheckMowerProgress
					End If
				End If
			Case 19
				if Mode(CurrentPlayer,19) = 2 Then addscore SCORE_SUPER_JACKPOT
		End Select
	End If


	LastSwitchHit = "CottonTrigger"
	Debug.print "Last Switch: "&LastSwitchHit
	BallHandlingQueue.Add "LastSwitchHit = Empty","LastSwitchHit = ""Empty""",50,400,0,0,0,False
End Sub

'-------------------
'   LUANNE Orbit
'-------------------
Dim LastTriggerHit
Sub RightOrbitTrigger_Hit()
	LastTriggerHit = "RightOrbitTrigger"
End Sub

Sub LuannePrepTrigger_Hit
	WireRampOff	 'Turn off the Plastic Ramp Sound
	LastTriggerHit = "LuannePrepTrigger"
	Debug.print "Last Switch Luanne: "&LastTriggerHit
End Sub

Sub LuanneTrigger_Hit()
	if Tilted Then Exit Sub

	DOF 140, DOFPulse
	Debug.print "Ball Info Luanne:" &activeball.vely
	if LastTriggerHit = "LuannePrepTrigger" Then
		OrbitCount = OrbitCount + 1
		LastTriggerHit = "LuanneTrigger"
		Debug.print "Last Switch Luanne: "&LastTriggerHit

		Select Case Mode(CurrentPlayer,0)
			Case 0
				anLuanneLights(CurrentPlayer,0) = anLuanneLights(CurrentPlayer,0) + 1
				if anLuanneLights(CurrentPlayer,0) > 6 Then anLuanneLights(CurrentPlayer,0) = 6 : bLuanneReady = True : StartLuanne' Start Mode
				anLuanneLights(CurrentPlayer,anLuanneLights(CurrentPlayer,0)) = 1
				DisplayLuanneProgress
			Case 1
				if Mode(CurrentPlayer,1) = 2 Then
					if nPeggyMode1Progress(10) = 0 Then
						nPeggyMode1Progress(10) = 1
						addscore SCORE_MODE_PROGRESS
						LightSeqLuanne.StopPlay()
						CheckPeggyProgress
					End If
				End If
			Case 3
				if Mode(CurrentPlayer,3) = 2 Then
					if nPeggyMode3Progress(4) = 0 Then
						nPeggyMode3Progress(4) = 1
						addscore SCORE_MODE_PROGRESS
						LightSeqLuanne.StopPlay()
						CheckPeggy3Progress
					End If
				End If
			Case 8
				if Mode(CurrentPlayer,8) = 2 And bHankMode2Orbit Then 
					nHankMode2Progress = nHankMode2Progress + 1
					CheckHankMode2Progress
					AddScore SCORE_MODE_PROGRESS
					StopModeOrbitSeq
					LightEffect 19
				End If
			Case 11
				if Mode(CurrentPlayer,11) = 2 And nCottonActive = 10 Then 
					nCottonShots(10) = 1
					nCottonModeProgress = nCottonModeProgress + 1
					nNewCottonShot = rndNumNot(1,10,nCottonActive)
					ChaseLights
					ChaseTimerReset
				End If
			Case 15
				if mode(currentplayer,15) = 2 Then nBeerMode3Progress = nBeerMode3Progress + 1 :  CheckBeerMode3
			Case 16
				if Mode(CurrentPlayer,16) = 2 Then
					if nMowerProgress(10) = 0 Then
						nMowerProgress(10) = 1
						LightSeqLuanne.StopPlay()
						CheckMowerProgress
					End If
				End If
			Case 19
				if Mode(CurrentPlayer,19) = 2 Then addscore SCORE_SUPER_JACKPOT

		End Select
	End If

End Sub


'-------------------
' PEGGY Targets
'-------------------
Sub Target009_Hit()
	if bPeggyReady or Tilted Then Exit Sub

	DOF 138, DOFPulse
	Select Case Mode(CurrentPlayer,0)
		Case 0
			anPeggyLights(CurrentPlayer,1) = 1	
			CheckPeggy
		Case 1

		Case 9
			if Mode(CurrentPlayer,9) = 2 Then nHankMode3Progress = nHankMode3Progress + 1 : CheckHankMode3Progress
		Case 12

	End Select
	LastSwitchHit = "Target009"
End Sub

Sub Target002_Hit()
	if bPeggyReady or Tilted Then Exit Sub

	DOF 138, DOFPulse
	Select Case Mode(CurrentPlayer,0)
		Case 0
			anPeggyLights(CurrentPlayer,2) = 1	
			CheckPeggy
		Case 1

		Case 9
			if Mode(CurrentPlayer,9) = 2 Then nHankMode3Progress = nHankMode3Progress + 1 : CheckHankMode3Progress
		Case 12

	End Select

	LastSwitchHit = "Target002"
End Sub

Sub Target003_Hit()
	if bPeggyReady or Tilted Then Exit Sub

	DOF 138, DOFPulse
	Select Case Mode(CurrentPlayer,0)
		Case 0
			anPeggyLights(CurrentPlayer,3) = 1	
			CheckPeggy
		Case 1

		Case 9
			if Mode(CurrentPlayer,9) = 2 Then nHankMode3Progress = nHankMode3Progress + 1 : CheckHankMode3Progress
		Case 12

	End Select
	LastSwitchHit = "Target003"
End Sub

Sub CheckPeggy
	DisplayPeggyProgress
	if anPeggyLights(CurrentPlayer,1) + anPeggyLights(CurrentPlayer,2) + anPeggyLights(CurrentPlayer,3) = 3 Then
		'ResetPeggyLights(CurrentPlayer)
		anPeggyLights(CurrentPlayer,0) = anPeggyLights(CurrentPlayer,0) + 1
		bPeggyReady = True
		Light130.State = 2
		' Start Peggy Mode
	End If
End Sub


Sub ClearEOBDMD
	PuPlayer.LabelSet pDMD,"Event5A","",0,""
	PuPlayer.LabelSet pDMD,"Event5B","",0,""
	PuPlayer.LabelSet pDMD,"Event5C","",0,""
	PuPlayer.LabelSet pDMD,"Event5D","",0,""
	PuPlayer.LabelSet pDMD,"Event5E","",0,""
	PuPlayer.LabelSet pDMD,"Event5F","",0,""
End Sub



'#######################################################################
'''''''''''' END OF MAIN SHOTS    ''''''''''''''''''''''''''''''''''''''
'#######################################################################
Const FontSizeEvents = 10
Sub UpdateModeMessages_Timer


	if bSupressModeMessages Then 
		PuPlayer.LabelSet pDMD,"Event3A","",0,""
		PuPlayer.LabelSet pDMD,"Event3B","",0,""
		PuPlayer.LabelSet pDMD,"Event3C","",0,""
		Exit Sub
	End If
'asModeMessagesL1
'asModeMessagesL1 = Array("", "Shoot", "Shoot Moving Target", "Luanne , Bobby", "Peggy", "Boomhauer", "Bill", "Peggy Scoop", _
'"Alternate", "Shoot" , "Shoot Behind", "Chase", "Orbits and Ramps", "Shoot 100", "Shoot", "Shoot", "MULTIBALL", "Ladybird", "Shoot")

'Dim asModeMessagesL2
'asModeMessagesL2 = Array("","Orbits & Ramps", "3 Times", " Hank, Ladybird", "Super Jackpot", "Super Jackpot", "Super Jackpot", _
'"5 Times", "Orbits and Ramps", "10 Targets", "Left Flipper", "Lights", "Super Jackpot", "Spinners", "Ramps", "Orbits", "", "10X Value", "15 Ramps" )

' Used for number of shots for each mode to compelte
'Dim anModeProgress = Array("",10,3,4,0,0,0,5,5,10,0,6,3,100,6,6,0,0,15,0)


' 0 - Base No Mode -- used to work towards all the other modes
' 1 - PEG First mission
' 2 - PEG Second mission
' 3 - PEG Third Mission (K)
' 4 - Luanne 			(I)
' 5 - Boomhauer 		(N)
' 6 - Bill 				(G)
' 7 - Hank First Mission	
' 8 - Hank Second Mission	
' 9 - Hank Third Mission(H)
' 10 - Dale				(I)
' 11 - Cotton			(L)
' 12 - Bobby			(L)
' 13 - Beer First 
' 14 - Beer Second
' 15 = Beer Third
' 16 - Lawn Mower
' 17 - LadyBird Mode
' 18 - BBQ Mode
' 19 - Wizard

	ShowStar

	Select Case Mode(CurrentPlayer,0)
		Case 0
			PuPlayer.LabelSet pDMD,"Event3A","",0,""
			PuPlayer.LabelSet pDMD,"Event3B","",0,""
			PuPlayer.LabelSet pDMD,"Event3C","",0,""
		Case 1
			PuPlayer.LabelSet pDMD,"Event3A","' PEGGY '",1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':31.0}"
			PuPlayer.LabelSet pDMD,"Event3B",asModeMessagesL1(1),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':45.0}"
			PuPlayer.LabelSet pDMD,"Event3C",anModeProgress(1) - nPeggyMode1Progress(0)& " "& asModeMessagesL2(1),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':59.0}"
		Case 2
			PuPlayer.LabelSet pDMD,"Event3A","' PEGGY '",1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':31.0}"
			PuPlayer.LabelSet pDMD,"Event3B",asModeMessagesL1(2),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':45.0}"
			PuPlayer.LabelSet pDMD,"Event3C",anModeProgress(2) - nPeggyMode2Progress& " "& asModeMessagesL2(2),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':59.0}"
		Case 3
			PuPlayer.LabelSet pDMD,"Event3A","' PEGGY '",1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':31.0}"
			PuPlayer.LabelSet pDMD,"Event3B",asModeMessagesL1(3),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':45.0}"
			PuPlayer.LabelSet pDMD,"Event3C",asModeMessagesL2(3),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':59.0}"
		Case 4
			PuPlayer.LabelSet pDMD,"Event3A","' LUANNE '",1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':31.0}"
			PuPlayer.LabelSet pDMD,"Event3B",asModeMessagesL1(4),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':45.0}"
			PuPlayer.LabelSet pDMD,"Event3C",asModeMessagesL2(4),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':59.0}"
		Case 5
			PuPlayer.LabelSet pDMD,"Event3A","' BOOMHAUER '",1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':31.0}"
			PuPlayer.LabelSet pDMD,"Event3B",asModeMessagesL1(5),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':45.0}"
			PuPlayer.LabelSet pDMD,"Event3C",asModeMessagesL2(5),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':59.0}"
		Case 6
			PuPlayer.LabelSet pDMD,"Event3A","' BILL '",1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':31.0}"
			PuPlayer.LabelSet pDMD,"Event3B",asModeMessagesL1(6),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':45.0}"
			PuPlayer.LabelSet pDMD,"Event3C",asModeMessagesL2(6),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':59.0}"
		Case 7
			PuPlayer.LabelSet pDMD,"Event3A","' HANK '",1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':31.0}"
			PuPlayer.LabelSet pDMD,"Event3B",asModeMessagesL1(7),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':45.0}"
			PuPlayer.LabelSet pDMD,"Event3C",anModeProgress(7) - nHankMode1Progress& " "& asModeMessagesL2(7),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':59.0}"
		Case 8
			PuPlayer.LabelSet pDMD,"Event3A","' HANK '",1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':31.0}"
			PuPlayer.LabelSet pDMD,"Event3B",asModeMessagesL1(8),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':45.0}"
			PuPlayer.LabelSet pDMD,"Event3C",anModeProgress(8) - nHankMode2Progress& " "& asModeMessagesL2(8),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':59.0}"
		Case 9
			PuPlayer.LabelSet pDMD,"Event3A","' HANK '",1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':31.0}"
			PuPlayer.LabelSet pDMD,"Event3B",asModeMessagesL1(9),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':45.0}"
			PuPlayer.LabelSet pDMD,"Event3C",anModeProgress(9) - nHankMode3Progress& " "& asModeMessagesL2(9),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':59.0}"
		Case 10
			PuPlayer.LabelSet pDMD,"Event3A","' DALE '",1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':31.0}"
			PuPlayer.LabelSet pDMD,"Event3B",asModeMessagesL1(10),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':45.0}"
			PuPlayer.LabelSet pDMD,"Event3C",asModeMessagesL2(10),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':59.0}"
		Case 11
			PuPlayer.LabelSet pDMD,"Event3A","' COTTON '",1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':31.0}"
			PuPlayer.LabelSet pDMD,"Event3B",asModeMessagesL1(11),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':45.0}"
			PuPlayer.LabelSet pDMD,"Event3C",anModeProgress(11) - nCottonModeProgress& " "& asModeMessagesL2(11),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':59.0}"
		Case 12
			PuPlayer.LabelSet pDMD,"Event3A","' BOBBY '",1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':31.0}"
			PuPlayer.LabelSet pDMD,"Event3B",asModeMessagesL1(12),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':45.0}"
			PuPlayer.LabelSet pDMD,"Event3C",anModeProgress(12) - BobbyTargetCount& " "& asModeMessagesL2(12),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':59.0}"
		Case 13
			PuPlayer.LabelSet pDMD,"Event3A","' BEER 1 '",1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':31.0}"
			PuPlayer.LabelSet pDMD,"Event3B",asModeMessagesL1(13),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':45.0}"
			PuPlayer.LabelSet pDMD,"Event3C",anModeProgress(13) - nBeerMode1Progress& " "& asModeMessagesL2(13),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':59.0}"
		Case 14
			PuPlayer.LabelSet pDMD,"Event3A","' BEER 2 '",1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':31.0}"
			PuPlayer.LabelSet pDMD,"Event3B",asModeMessagesL1(14) &" " &anModeProgress(14) - nBeerMode2Progress,1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':45.0}"
			PuPlayer.LabelSet pDMD,"Event3C",asModeMessagesL2(14),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':59.0}"
		Case 15
			PuPlayer.LabelSet pDMD,"Event3A","' BEER 3 '",1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':31.0}"
			PuPlayer.LabelSet pDMD,"Event3B",asModeMessagesL1(15),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':45.0}"
			PuPlayer.LabelSet pDMD,"Event3C",anModeProgress(15) - nBeerMode3Progress& " "& asModeMessagesL2(15),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':59.0}"
		Case 16
			PuPlayer.LabelSet pDMD,"Event3A","' TRACTOR '",1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':31.0}"
			PuPlayer.LabelSet pDMD,"Event3B",asModeMessagesL1(16),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':45.0}"
			PuPlayer.LabelSet pDMD,"Event3C",asModeMessagesL2(16),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':59.0}"
		Case 17
			PuPlayer.LabelSet pDMD,"Event3A","' LADYBIRD '",1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':31.0}"
			PuPlayer.LabelSet pDMD,"Event3B","10X SPINNER",1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':45.0}"
			PuPlayer.LabelSet pDMD,"Event3C","VALUE",1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':59.0}"

		Case 18
			PuPlayer.LabelSet pDMD,"Event3A","' BBQ FEAST '",1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':31.0}"
			PuPlayer.LabelSet pDMD,"Event3B",asModeMessagesL1(18),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':45.0}"
			PuPlayer.LabelSet pDMD,"Event3C",anModeProgress(18) - nBBQModeProgress & " "& asModeMessagesL2(18),1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':59.0}"
		Case 19
			PuPlayer.LabelSet pDMD,"Event3A","' WIZARD '",1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':31.0}"
			PuPlayer.LabelSet pDMD,"Event3B","' MODE '",1,"{'mt':2,'fonth': "&FontSizeEvents&" ,'xalign':1,'yalign':1,'xpos':50,'ypos':45.0}"
		Case Else

	End Select
End Sub


'*****************
''' START MODES ''
'*****************
sub ShowStar

	if Mode(currentplayer,0) = 0 Then
		ModeFlasher.Visible = False
	Else
		ModeFlasher.Visible = True
	End If

	Select Case Mode(CurrentPlayer,0)
		Case 0
			PuPlayer.LabelSet pDMD,"Star","`u`",0,"{'mt':2,'xpos':20,'ypos':20}"	
		Case 1
			PuPlayer.LabelSet pDMD,"Star","`u`",1,"{'mt':2,'xpos':19.25,'ypos':3}"
			ModeFlasher.ImageA = "FlashPeggy"
		Case 2
			PuPlayer.LabelSet pDMD,"Star","`u`",1,"{'mt':2,'xpos':19.25,'ypos':3}"
			ModeFlasher.ImageA = "FlashPeggy"
		Case 3
			PuPlayer.LabelSet pDMD,"Star","`u`",1,"{'mt':2,'xpos':19.25,'ypos':3}"
			ModeFlasher.ImageA = "FlashPeggy"
		Case 4
			PuPlayer.LabelSet pDMD,"Star","`u`",1,"{'mt':2,'xpos':19.2,'ypos':25.5}"	
			ModeFlasher.ImageA = "FlashLuanne"
		Case 5
			PuPlayer.LabelSet pDMD,"Star","`u`",1,"{'mt':2,'xpos':80.75,'ypos':25.5}"		
			ModeFlasher.ImageA = "FlashBoom"	
		Case 6
			PuPlayer.LabelSet pDMD,"Star","`u`",1,"{'mt':2,'xpos':98.5,'ypos':25.5}"
			ModeFlasher.ImageA = "FlashBill"			
		Case 7
			PuPlayer.LabelSet pDMD,"Star","`u`",1,"{'mt':2,'xpos':1.5,'ypos':3}"
			ModeFlasher.ImageA = "FlashHank"
		Case 8
			PuPlayer.LabelSet pDMD,"Star","`u`",1,"{'mt':2,'xpos':1.5,'ypos':3}"
			ModeFlasher.ImageA = "FlashHank"
		Case 9
			PuPlayer.LabelSet pDMD,"Star","`u`",1,"{'mt':2,'xpos':1.5,'ypos':3}"
			ModeFlasher.ImageA = "FlashHank"
		Case 10
			PuPlayer.LabelSet pDMD,"Star","`u`",1,"{'mt':2,'xpos':98.5,'ypos':3}"
			ModeFlasher.ImageA = "FlashDale"
		Case 11
			PuPlayer.LabelSet pDMD,"Star","`u`",1,"{'mt':2,'xpos':80.75,'ypos':3}"
			ModeFlasher.ImageA = "FlashCotton"
		Case 12
			PuPlayer.LabelSet pDMD,"Star","`u`",1,"{'mt':2,'xpos':1.5,'ypos':25.5}"		
			ModeFlasher.ImageA = "FlashBobby"	
		Case 13
			PuPlayer.LabelSet pDMD,"Star","`u`",0,"{'mt':2,'xpos':50,'ypos':70}"	
			ModeFlasher.ImageA = "FlashBeer"			
		Case 14
			PuPlayer.LabelSet pDMD,"Star","`u`",0,"{'mt':2,'xpos':50,'ypos':70}"
			ModeFlasher.ImageA = "FlashBeer"						
		Case 15
			PuPlayer.LabelSet pDMD,"Star","`u`",0,"{'mt':2,'xpos':50,'ypos':70}"		
			ModeFlasher.ImageA = "FlashBeer"	
		Case 16
			PuPlayer.LabelSet pDMD,"Star","`u`",0,"{'mt':2,'xpos':50,'ypos':70}"
			ModeFlasher.ImageA = "FlashTractor"		
		Case 17
			PuPlayer.LabelSet pDMD,"Star","`u`",0,"{'mt':2,'xpos':50,'ypos':70}"	
			ModeFlasher.ImageA = "FlashLadybird"	
		Case 18
			PuPlayer.LabelSet pDMD,"Star","`u`",0,"{'mt':2,'xpos':50,'ypos':70}"	
			ModeFlasher.ImageA = "FlashBBQ"		
		Case 19
			PuPlayer.LabelSet pDMD,"Star","`u`",0,"{'mt':2,'xpos':50,'ypos':70}"	
			ModeFlasher.Visible = False	
	End Select
End Sub


Sub StartBill
	if Tilted Then Exit Sub

	DOF 136, DOFPulse

	ClearTwoLines
	if Mode(CurrentPlayer,0) <> 0 Then Exit Sub
	' 3 ball multiball inside orbit hit 9 + 1 times

'	if Mode(CurrentPlayer,6) = 2 Then addscore SCORE_SUPER_JACKPOT ' Bill Mode

	if bBillReady Then
		StopAllMusic
		SwitchMusic "Mode4"
		

		bBillReady = False
		Mode(CurrentPlayer,0) = 6
		Mode(CurrentPlayer,6) = 2
		PuPStartMultiball
		AddMultiball 2
		TurnOffModeLights
		lighteffect 20
		EnableBallSaver 20

		if Scorbit.bSessionActive then
			GameModeStrTmp="BL{Yellow}:Start Bill "
			Scorbit.SetGameMode(GameModeStrTmp)
		End If
	End If
End Sub

Sub StartDale
	if Tilted Then Exit Sub

	DOF 136, DOFPulse

	ClearTwoLines

	StopAllMusic
	SwitchMusic "Mode6"

	BallHandlingQueue.Add "bDaleReady = False","bDaleReady = False",50,100,0,0,0,False
	
	Mode(CurrentPlayer,0) = 10
	Mode(CurrentPlayer,10) = 2
	PuPStartMultiball
	AddMultiball 4
	EnableBallSaver 20
	TurnOffModeLights
	lighteffect 21

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Yellow}:Start Dale "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub StartBoom
	if Tilted Then Exit Sub

	if Mode(CurrentPlayer,0) <> 0 Then Exit Sub
	' 5 ball multiball once all 9 CIA,FBI,NSA lights lit

	DOF 136, DOFPulse

	ClearTwoLines
	'PuPStartMultiball
	PupStartBoom
	StopAllMusic
	SwitchMusic "Mode3"

	if Mode(CurrentPlayer,5) = 2 Then addscore SCORE_SUPER_JACKPOT ' Boom Mode

	if bBoomReady Then
		bBoomReady = False
		Mode(CurrentPlayer,0) = 5
		Mode(CurrentPlayer,5) = 2
		AddMultiball 3
		EnableBallSaver 20
		TurnOffModeLights
		LightEffect 22

		if Scorbit.bSessionActive then
			GameModeStrTmp="BL{Yellow}:Start Boom "
			Scorbit.SetGameMode(GameModeStrTmp)
		End If
	End If
End Sub

sub StartBeer1
	if Tilted Then Exit Sub

	DOF 136, DOFPulse

	ClearTwoLines
	PupStartBeer
	StopAllMusic
	SwitchMusic "Mode9"

	nBeerMode1Progress = 0
	Mode(CurrentPlayer,0) = 13
	Mode(CurrentPlayer,13) = 2
	TurnOffModeLights
	LightEffect 23

	DisplayTimerPopUp
	nTimerCount = 150
	ModeTimer.Enabled = True

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Yellow}:Start Beer 1 "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If

End Sub

Sub StartBeer2
	if Tilted Then Exit Sub

	DOF 136, DOFPulse

	ClearTwoLines
	PupStartBeer

	StopAllMusic
	SwitchMusic "Mode9"

	nBeerMode2Progress = 0
	Mode(CurrentPlayer,0) = 14
	Mode(CurrentPlayer,14) = 2
	TurnOffModeLights
	LightEffect 24

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Yellow}:Start Beer 2 "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub StartBeer3
	if Tilted Then Exit Sub

	DOF 136, DOFPulse

	ClearTwoLines
	PupStartBeer

	StopAllMusic
	SwitchMusic "Mode9"

	nBeerMode3Progress = 0
	Mode(CurrentPlayer,0) = 15
	Mode(CurrentPlayer,15) = 2
	TurnOffModeLights
	LightEffect 25

	DisplayTimerPopUp
	nTimerCount = 180
	ModeTimer.Enabled = True

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Yellow}:Start Beer 3 "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub StartLadybird
	if Tilted Then Exit Sub

	DOF 136, DOFPulse

	ClearTwoLines
	Mode(CurrentPlayer,0) = 17
	Mode(CurrentPlayer,17) = 2
	TurnOffModeLights
	LightEffect 26

	StopAllMusic
	SwitchMusic "Mode13"

	DisplayTimerPopUp
	nTimerCount = 60
	ModeTimer.Enabled = True

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Yellow}:Start LadyBird "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub ForceSetLawnMower
	if bLockEngaged Then
		anLawnMower(CurrentPlayer,0) = 5
		anLawnMower(CurrentPlayer,1) = 1
		anLawnMower(CurrentPlayer,2) = 1
		anLawnMower(CurrentPlayer,3) = 1
		anLawnMower(CurrentPlayer,4) = 1
		anLawnMower(CurrentPlayer,5) = 1
	End If
End Sub

Sub StartLawnmower
	if Tilted Then Exit Sub
	Mode(CurrentPlayer,0) = 16
	Mode(CurrentPlayer,16) = 2

	DOF 136, DOFPulse

	bReleasingBalls = True
	ClearTwoLines
	bLockEngaged = False
	bMowerNotStarted = False
	MasterMowerLocation = 0

	AdvanceKickers.Enabled = True

	StopAllMusic
	SwitchMusic "Mode12"

	PuPlayer.playevent pDMDVideo,"Mode16","LawnMowerStart.mp4",100,70,0,0,""

	DisplayTimerPopUp
	nTimerCount = 120
	ModeTimer.Enabled = True
	TurnOffModeLights
	LightEffect 39
	EnableBallSaver 20

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Yellow}:Mower Multiball "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

sub StartHank1
	if Tilted Then Exit Sub

	DOF 136, DOFPulse

	ClearTwoLines
	nHankMode1Progress = 0
	Mode(CurrentPlayer,0) = 7
	Mode(CurrentPlayer,7) = 2
	TurnOffModeLights
	LightEffect 28

	StopAllMusic
	SwitchMusic "Mode5"

	DisplayTimerPopUp
	nTimerCount = 90
	ModeTimer.Enabled = True

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Yellow}:Start Hank 1 "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

sub StartHank2
	if Tilted Then Exit Sub

	DOF 136, DOFPulse
	ClearTwoLines
	bHankMode2Ramp = True
	bHankMode2Orbit = False
	nHankMode2Progress = 0
	Mode(CurrentPlayer,0) = 8
	Mode(CurrentPlayer,8) = 2
	TurnOffModeLights
	LightEffect 19  ' start with ramps
	'LightEffect 29

	StopAllMusic
	SwitchMusic "Mode5"

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Yellow}:Start Hank 2 "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

sub StartHank3
	if Tilted Then Exit Sub

	DOF 136, DOFPulse
	ClearTwoLines
	nHankMode3Progress = 0
	Mode(CurrentPlayer,0) = 9
	Mode(CurrentPlayer,9) = 2
	TurnOffModeLights

	'Target Lights
	LightEffect 30

	StopAllMusic
	SwitchMusic "Mode5"

	DisplayTimerPopUp
	nTimerCount = 90
	ModeTimer.Enabled = True

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Yellow}:Start Hank 3 "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub StartPropane
	if Tilted Then Exit Sub

	DOF 137, DOFPulse
End Sub

Sub StartBBQFeast
	if Tilted Then Exit Sub

	DOF 136, DOFPulse
	ClearTwoLines
	Mode(CurrentPlayer,0) = 18
	Mode(CurrentPlayer,18) = 2

	PupStartBBQ
	StopAllMusic
	SwitchMusic "Mode14"

	nBBQModeProgress = 0
	TurnOffModeLights
	LightEffect 19

	DisplayTimerPopUp
	nTimerCount = 180
	ModeTimer.Enabled = True

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Yellow}:Start BBQFeast "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If

End Sub

'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
Dim ChaseLightsArray
Dim C1,C2,C3,C4,C5,C6,C7,C8,C9,C10
ChaseLightsArray = Array(C1,C2,C3,C4,C5,C6,C7,C8,C9,C10)
'C1 - Boomhauer
'C2 - Beer
'C3 - Ladybird
'C4 - LawnMower
'C5 - Hank
'C6 - Propane
'C7 - Bill
'C8 - BBQ
'C9 - Cotton
'C10 - Luanne



Function rndNumNot(min, max, notNum)
    Randomize
    Dim iNum

Do While 1
    iNum = Int((max-min+1)*Rnd+min)
    If iNum <> notNum Then
        Exit Do
    End If 
loop

rndNumNot = iNum ' return values
End Function




'General sub for turning on only random light in an array of lights
Sub ChaseLights
    Dim L, Cnt

	StopAllModeSeq

	Debug.print "PRE CP: " &nCottonModeProgress

	if nCottonModeProgress > 5 Then 
		CottonModeChase.Enabled = False
		BallHandlingQueue.Add "WinCotton","WinCotton",90,50,0,0,0,False
		Debug.print " SHOULD STOP COTTON"
		Exit Sub
	End If

	Debug.print "CP: " &nCottonModeProgress


		FindChaseLight
		Debug.print "Phase 2 X: "&nNewCottonShot
'        Cnt = 0
'        For Cnt = 1 to 10
 '          Cnt = Cnt + 1
			'Debug.print "Value " & Cnt &" - " & nCottonShots(cnt)
 '          If nCottonShots(cnt) = 0 Then 

 '               If Cnt = nNewCottonShot Then 'Turn on this Light Sequence
					nCottonActive = nNewCottonShot
					'Debug.print "Chase Lights:: turning on light Sequence : "&Cnt
					Select Case nCottonActive
						Case 1
							LightSeqBoom.UpdateInterval = LargeSeqInterval
							LightSeqBoom.Play SeqUpOn, LargeSeqUpdate, 1000
						Case 2
							LightSeqBeer2.UpdateInterval = LargeSeqInterval
							LightSeqBeer2.Play SeqUpOn, LargeSeqUpdate, 1000
						Case 3
							LightSeqLadybird.UpdateInterval = LargeSeqInterval
							LightSeqLadybird.Play SeqUpOn, LargeSeqUpdate, 1000
						Case 4
							LightSeqLawnmower.UpdateInterval = LargeSeqInterval
							LightSeqLawnmower.Play SeqUpOn, LargeSeqUpdate, 1000
						Case 5
							LightSeqHank2.UpdateInterval = LargeSeqInterval
							LightSeqHank2.Play SeqUpOn, LargeSeqUpdate, 1000
						Case 6
							LightSeqPropane.UpdateInterval = LargeSeqInterval
							LightSeqPropane.Play SeqUpOn, LargeSeqUpdate, 1000
						Case 7
							LightSeqBill.UpdateInterval = LargeSeqInterval
							LightSeqBill.Play SeqUpOn, LargeSeqUpdate, 1000
						Case 8
							LightSeqBBQ2.UpdateInterval = LargeSeqInterval
							LightSeqBBQ2.Play SeqUpOn, LargeSeqUpdate, 1000
						Case 9
							LightSeqCotton.UpdateInterval = LargeSeqInterval
							LightSeqCotton.Play SeqUpOn, LargeSeqUpdate, 1000
						Case 10
							LightSeqLuanne.UpdateInterval = LargeSeqInterval
							LightSeqLuanne.Play SeqUpOn, LargeSeqUpdate, 1000
					End Select
					
                    Exit Sub
  '              End If
			' RC15 Added else clause 
'			Else
'				ChaseLights
'            End If
'        Next

End Sub
'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

Sub StartCotton
	if Tilted Then Exit Sub
	ClearTwoLines

	ResetCottonProgress
	Mode(CurrentPlayer,0) = 11
	Mode(CurrentPlayer,11) = 2
	TurnOffModeLights

	nCottonModeProgress = 0
	nNewCottonShot = rndNumNot(1,10,nCottonActive)
	ChaseLights
	ChaseTimerReset

	StopAllMusic
	SwitchMusic "Mode7"

	PuPStartMultiball

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Yellow}:Start Cotton "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub StartLuanne
	if Tilted Then Exit Sub
	ClearTwoLines

	if Mode(CurrentPlayer,0) <> 0 Then Exit Sub
	' 2 ball multiball right orbit hit 6 + 1 times

	if bLuanneReady Then

		bLuanneReady = False
		Mode(CurrentPlayer,0) = 4
		Mode(CurrentPlayer,4) = 2
		PuPStartMultiball
		AddMultiball 1
		EnableBallSaver 20
		TurnOffModeLights

		StopAllMusic
		SwitchMusic "Mode2"

		LightEffect 36

		if Scorbit.bSessionActive then
			GameModeStrTmp="BL{Yellow}:Start Luanne "
			Scorbit.SetGameMode(GameModeStrTmp)
		End If
	End If
End Sub

Sub StartBobby
	if Tilted Then Exit Sub

	ClearTwoLines


	if Mode(CurrentPlayer,0) <> 0 Then Exit Sub
	' 2 ball multiball right orbit hit 6 + 1 times


	if bBobbyReady Then

	StopAllMusic
	SwitchMusic "Mode8"

		BobbyTargetCount = 0
		bBobbyReady = False
		Mode(CurrentPlayer,0) = 12
		Mode(CurrentPlayer,12) = 2
		PuPStartMultiball
		AddMultiball 2
		EnableBallSaver 20
		TurnOffModeLights
		LightEffect 39
		PrepareBobbyMovement

		if Scorbit.bSessionActive then
			GameModeStrTmp="BL{Yellow}:Start Bobby "
			Scorbit.SetGameMode(GameModeStrTmp)
		End If
	End If
End Sub

Sub StartDrinkAlamo
	if Tilted Then Exit Sub

	bReadyDrinkAlamo = False

	OpenCooler
'DOF 114,DOFPulse
	EnableBallSaver 20
	BallHandlingQueue.Add "ResetDrinkAlamo(CurrentPlayer)","ResetDrinkAlamo(CurrentPlayer)",1,20000,0,0,0,False


	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Yellow}:Drink Alamo !! "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub ResetCottonProgress
	dim i
	For i = 0 to 10
		nCottonShots(i) = 0
	Next
End Sub

Sub ResetPeggyMode1Progress
	dim i
	For i = 0 to 10
		nPeggyMode1Progress(i) = 0
	Next
End Sub

Sub ResetPeggyMode3Progress
	dim i
	For i = 0 to 4
		nPeggyMode3Progress(i) = 0
	Next
End Sub

Sub StartPeggy1
	if Tilted Then Exit Sub

	ClearTwoLines
	PupStartPeggy1
	ResetPeggyMode1Progress
	Mode(CurrentPlayer,0) = 1
	Mode(CurrentPlayer,1) = 2
	Light101.State = 1	'Red
	Light130.State = 0 'Purple

	TurnOffModeLights
	LightEffect 39
	StopAllMusic
	SwitchMusic "mode1"
	'UpdateModeMessage

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Yellow}:Start Peggy 1 "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub StartPeggy2
	if Tilted Then Exit Sub

	ClearTwoLines
	PupStartPeggy2
	BobbyTargetCount = 0
	nPeggyMode2Progress = 0

	Mode(CurrentPlayer,0) = 2
	Mode(CurrentPlayer,2) = 2
	Light102.State = 1	'White
	Light130.State = 0 'Purple
	StopAllMusic
	SwitchMusic "mode1"

	TurnOffModeLights
	DisplayTimerPopUp
	nTimerCount = 120
	ModeTimer.Enabled = True
	PrepareBobbyMovement

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Yellow}:Start Peggy 2 "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub StartPeggy3
	if Tilted Then Exit Sub

	ClearTwoLines
	PupStartPeggy3
	ResetPeggyMode3Progress
	Mode(CurrentPlayer,0) = 3
	Mode(CurrentPlayer,3) = 2
	Light103.State = 1	'Blue
	Light130.State = 0 'Purple
	StopAllMusic
	SwitchMusic "mode1"

	TurnOffModeLights
	LightEffect 41
	DisplayTimerPopUp
	nTimerCount = 60
	ModeTimer.Enabled = True

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Yellow}:Start Peggy 3 "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub WizardMode
	bWizardMode = True
	Mode(CurrentPlayer,0) = 19
	Mode(CurrentPlayer,19) = 2

	ClearTwoLines
	StopAllMusic
	SwitchMusic "WizardMode"

	PuPStartMultiball

	AddMultiball 4
	EnableBallSaver 20

	TurnOffModeLights
	LightEffect 39

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Purple}:WIZARD MODE "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub
'*****************
'''  STOP MODES ''
'*****************
Sub StopAnyModes
	StopAllMusic
	Select Case Mode(CurrentPlayer,0)
		Case 0

		Case 1
			if Mode(CurrentPlayer,1) = 2 Then StopPeggy1
			if Scorbit.bSessionActive then
				GameModeStrTmp="BL{Red}:STOP Peggy 1 "
				Scorbit.SetGameMode(GameModeStrTmp)
			End If
		Case 2
			if Mode(CurrentPlayer,2) = 2 Then StopPeggy2
			if Scorbit.bSessionActive then
				GameModeStrTmp="BL{Red}:STOP Peggy 2"
				Scorbit.SetGameMode(GameModeStrTmp)
			End If
		Case 3
			if Mode(CurrentPlayer,3) = 2 Then StopPeggy3
			if Scorbit.bSessionActive then
				GameModeStrTmp="BL{Red}:STOP Peggy 3 "
				Scorbit.SetGameMode(GameModeStrTmp)
			End If
		Case 4
			'NA Mball
		Case 5
			'NA Mball
		Case 6
			'NA Mball
		Case 7
			if Mode(CurrentPlayer,7) = 2 Then StopHank1
			if Scorbit.bSessionActive then
				GameModeStrTmp="BL{Red}:STOP Hank 1 "
				Scorbit.SetGameMode(GameModeStrTmp)
			End If
		Case 8
			if Mode(CurrentPlayer,8) = 2 Then StopHank2
			if Scorbit.bSessionActive then
				GameModeStrTmp="BL{Red}:STOP Hank 2 "
				Scorbit.SetGameMode(GameModeStrTmp)
			End If
		Case 9
			if Mode(CurrentPlayer,9) = 2 Then StopHank3
			if Scorbit.bSessionActive then
				GameModeStrTmp="BL{Red}:STOP Hank 3 "
				Scorbit.SetGameMode(GameModeStrTmp)
			End If
		Case 10
			'NA Mball
		Case 11
			if Mode(CurrentPlayer,11) = 2 Then StopCotton
			if Scorbit.bSessionActive then
				GameModeStrTmp="BL{Red}:STOP Cotton "
				Scorbit.SetGameMode(GameModeStrTmp)
			End If
		Case 12
			'NA Mball
		Case 13
			if Mode(CurrentPlayer,13) = 2 Then StopBeer1
			if Scorbit.bSessionActive then
				GameModeStrTmp="BL{Red}:STOP Beer 1 "
				Scorbit.SetGameMode(GameModeStrTmp)
			End If
		Case 14
			if Mode(CurrentPlayer,14) = 2 Then StopBeer2
			if Scorbit.bSessionActive then
				GameModeStrTmp="BL{Red}:STOP Beer 2 "
				Scorbit.SetGameMode(GameModeStrTmp)
			End If
		Case 15
			if Mode(CurrentPlayer,15) = 2 Then StopBeer3
			if Scorbit.bSessionActive then
				GameModeStrTmp="BL{Red}:STOP Beer 3 "
				Scorbit.SetGameMode(GameModeStrTmp)
			End If
		Case 16

		Case 17

		Case 18
			if Mode(CurrentPlayer,18) = 2 Then StopBBQFeast
			if Scorbit.bSessionActive then
				GameModeStrTmp="BL{Red}:STOP BBQFeast "
				Scorbit.SetGameMode(GameModeStrTmp)
			End If
		Case 19

	End Select
End Sub


Sub StopBill
	'NA
End Sub

Sub StopDale
	'NA
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,10) = 0
	LightSeqHank2.StopPlay()
	BallHandlingQueue.Add "ResetDaleLights(CurrentPlayer)","ResetDaleLights(CurrentPlayer)",55,10,0,0,0,False
	UpdateMusicNow
	pDMDSplashTwoLines "DALE MODE", "FAILED", 3, cLightBlue
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True

End Sub

Sub StopBoom
	'NA
End Sub

sub StopBeer1
	LightSeqBeer.StopPlay
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,13) = 0

	LightSeqLadybird.StopPlay()
    LightSeqLuanne.StopPlay()

	ModeTimer.Enabled = 0
	HideTimerPopUp

	BallHandlingQueue.Add "ResetBeerLights(CurrentPlayer)","ResetBeerLights(CurrentPlayer)",55,10,0,0,0,False
	UpdateMusicNow
	
	pDMDSplashTwoLines "BEER MODE 1", "FAILED", 3, cRed
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
	
End Sub

sub StopBeer2
	LightSeqBeer.StopPlay
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,14) = 0

	LightSeqBeer2.StopPlay()
    LightSeqHank2.StopPlay()
	LightSeqLawnMower.StopPlay()
	LightSeqPropane.StopPlay()
	LightSeqBBQ2.StopPlay()

	BallHandlingQueue.Add "ResetBeerLights(CurrentPlayer)","ResetBeerLights(CurrentPlayer)",55,10,0,0,0,False
	UpdateMusicNow

	pDMDSplashTwoLines "BEER MODE 2", "FAILED", 3, cRed
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
End Sub

sub StopBeer3
	LightSeqBeer.StopPlay
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,15) = 0

	LightSeqBoom.StopPlay()
    LightSeqLadybird.StopPlay()
	LightSeqBill.StopPlay()
	LightSeqCotton.StopPlay()
	LightSeqLuanne.StopPlay()

	ModeTimer.Enabled = 0
	HideTimerPopUp
	
	BallHandlingQueue.Add "ResetBeerLights(CurrentPlayer)","ResetBeerLights(CurrentPlayer)",55,10,0,0,0,False
	UpdateMusicNow
	pDMDSplashTwoLines "BEER MODE 3", "FAILED", 3, cRed
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
End Sub

Sub StopLadybird
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,17) = 0
    LightSeqLadybird.StopPlay()
	BallHandlingQueue.Add "ResetLadyBird(CurrentPlayer)","ResetLadyBird(CurrentPlayer)",55,10,0,0,0,False
	ModeTimer.Enabled = 0
	HideTimerPopUp
	UpdateMusicNow
End Sub

Sub StopLawnmower
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,16) = 0

    LightSeqBoom.StopPlay()
    LightSeqBeer2.StopPlay()
    LightSeqLadybird.StopPlay()
    LightSeqLawnmower.StopPlay()
    LightSeqHank2.StopPlay()
    LightSeqPropane.StopPlay()
    LightSeqBill.StopPlay()
    LightSeqBBQ2.StopPlay()
    LightSeqCotton.StopPlay()
    LightSeqLuanne.StopPlay()

	
	BallHandlingQueue.Add "ResetLawnMowerAll","ResetLawnMowerAll",55,10,0,0,0,False
	bMowerNotStarted = True
	bLockEngaged = False
	nLockedBalls = 0
	UpdateMusicNow
	PuPlayer.playevent pDMDVideo,"Mode16","LawnMowerStart.mp4",100,71,5,0,""

	ModeTimer.Enabled = 0
	HideTimerPopUp
End Sub

sub StopHank1

	debug.print "Stop hank"
	LightSeqPeggy.StopPlay
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,7) = 0

	BallHandlingQueue.Add "ResetHankLights(CurrentPlayer)","ResetHankLights(CurrentPlayer)",55,10,0,0,0,False
	DisplayHankProgress
	UpdateMusicNow
	PupFailHank

	ModeTimer.Enabled = 0
	HideTimerPopUp
End Sub

sub StopHank2
	LightSeqHank.StopPlay
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,8) = 0
	Light005.State = 1
	BallHandlingQueue.Add "ResetHankLights(CurrentPlayer)","ResetHankLights(CurrentPlayer)",55,10,0,0,0,False
	DisplayHankProgress
	UpdateMusicNow
	PupFailHank

	DMDQueue.Add "DelayHankStopSeq","DelayHankStopSeq",85,1200,0,0,0,False
	LightSeqBoom.StopPlay()
	LightSeqBeer2.StopPlay()
    LightSeqLadybird.StopPlay()
	LightSeqLawnMower.StopPlay()
    LightSeqHank2.StopPlay()
	LightSeqPropane.StopPlay()
	LightSeqBill.StopPlay()
    LightSeqBBQ2.StopPlay()
	LightSeqCotton.StopPlay()
    LightSeqLuanne.StopPlay()

End Sub

sub StopHank3
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,9) = 0
	Light004.State = 1
	BallHandlingQueue.Add "ResetHankLights(CurrentPlayer)","ResetHankLights(CurrentPlayer)",55,10,0,0,0,False
	DisplayHankProgress
	UpdateMusicNow
	PupFailHank

    LightSeqYEP.StopPlay()	
    LightSeqDrinkAlamo.StopPlay()

	Light109.State = 0 ' Football

	Light095.State = 0  ' CIA
	Light096.State = 0  ' NSA
	Light097.State = 0  ' FBI

	Light098.State = 0  'P
	Light099.State = 0  'E
	Light100.State = 0  'G

	ModeTimer.Enabled = 0
	HideTimerPopUp
End Sub

Sub StopPropane
	
End Sub

Sub StopBBQFeast
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,18) = 0
	StopModeRampSeq
	Light009.State = 0

	LightSeqBeer2.StopPlay()
    LightSeqHank2.StopPlay()
	LightSeqLawnMower.StopPlay()
	LightSeqPropane.StopPlay()
	LightSeqBBQ2.StopPlay()

	BallHandlingQueue.Add "ResetBBQFeast(CurrentPlayer)","ResetBBQFeast(CurrentPlayer)",55,10,0,0,0,False
	UpdateMusicNow

	ModeTimer.Enabled = 0
	HideTimerPopUp

	pDMDSplashTwoLines "BBQ FEAST", "FAILED", 3, cRed
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
End Sub

Sub StopCotton
	
	BallHandlingQueue.Add "ResetCottonLights(CurrentPlayer)","ResetCottonLights(CurrentPlayer)",55,10,0,0,0,False


    LightSeqBoom.StopPlay()
    LightSeqBeer2.StopPlay()
    LightSeqLadybird.StopPlay()
    LightSeqLawnmower.StopPlay()
    LightSeqHank2.StopPlay()
    LightSeqPropane.StopPlay()
    LightSeqBill.StopPlay()
    LightSeqBBQ2.StopPlay()
    LightSeqCotton.StopPlay()
    LightSeqLuanne.StopPlay()

	CottonModeChase.Enabled = False

	DisplayCottonProgress

	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,11) = 0
	UpdateMusicNow
End Sub

Sub StopLuanne
	'NA
End Sub

Sub StopBobby
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,12) = 0
    LightSeqBoom.StopPlay()
    LightSeqBeer2.StopPlay()
    LightSeqLadybird.StopPlay()
    LightSeqLawnmower.StopPlay()
    LightSeqHank2.StopPlay()
    LightSeqPropane.StopPlay()
    LightSeqBill.StopPlay()
    LightSeqBBQ2.StopPlay()
    LightSeqCotton.StopPlay()
    LightSeqLuanne.StopPlay()
	pDMDSplashTwoLines "Bobby Mode", "FAILED", 3, cRed
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
	
	BallHandlingQueue.Add "ResetBobbyLights(CurrentPlayer)","ResetBobbyLights(CurrentPlayer)",55,10,0,0,0,False
	DisplayBobbyProgress
	RemoveCollisionsBobby

	UpdateMusicNow
End Sub

Sub StopDrinkAlamo
	'NA
End Sub

Sub StopPeggy1
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,1) = 0
	Light101.State = 0	'Red

    LightSeqBoom.StopPlay()
    LightSeqBeer2.StopPlay()
    LightSeqLadybird.StopPlay()
    LightSeqLawnmower.StopPlay()
    LightSeqHank2.StopPlay()
    LightSeqPropane.StopPlay()
    LightSeqBill.StopPlay()
    LightSeqBBQ2.StopPlay()
    LightSeqCotton.StopPlay()
    LightSeqLuanne.StopPlay()

	pDMDSplashTwoLines "Peggy Mode 1", "FAILED", 3, cRed
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
	
	BallHandlingQueue.Add "ResetPeggyLights(CurrentPlayer)","ResetPeggyLights(CurrentPlayer)",55,10,0,0,0,False
	DisplayPeggyProgress

	UpdateMusicNow
End Sub

Sub StopPeggy2
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,2) = 0
	Light102.State = 0	'White
	pDMDSplashTwoLines "Peggy Mode 2", "FAILED", 3, cRed
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
	BallHandlingQueue.Add "ResetPeggyLights(CurrentPlayer)","ResetPeggyLights(CurrentPlayer)",55,10,0,0,0,False
	DisplayPeggyProgress
	UpdateMusicNow



	RemoveCollisionsBobby	
	BallHandlingQueue.Add "RemoveCollisionsBobby","RemoveCollisionsBobby",55,500,0,0,0,False


	MVTargetMove.Enabled = 0
	MVTargetDescend.Enabled = 1

	ModeTimer.Enabled = 0
	HideTimerPopUp
End Sub

Sub StopPeggy3
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,3) = 0
	Light103.State = 0	'Blue
	UpdateMusicNow

    LightSeqLadybird.StopPlay()
    LightSeqHank2.StopPlay()
    LightSeqBobby.StopPlay()
    LightSeqLuanne.StopPlay()

	pDMDSplashTwoLines "Peggy Mode 3", "FAILED", 3, cRed
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
	BallHandlingQueue.Add "ResetPeggyLights(CurrentPlayer)","ResetPeggyLights(CurrentPlayer)",55,10,0,0,0,False
	DisplayPeggyProgress

	ModeTimer.Enabled = 0
	HideTimerPopUp
End Sub

'*****************
''''' WIN MODES ''
'*****************
Sub WinBill
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,6) = 1
	Light146.State = 1	'G

	Light109.State = 0
	LightSeqBill.StopPlay()

	DOF 141, DOFPulse

	StopMBVideo
	UpdateMusicNow

	Addbonus SCORE_MODE_COMPLETED

	pDMDSplashTwoLines "Bill Mode", "COMPLETED", 3, cRed
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
	
	BallHandlingQueue.Add "ResetBillLights(CurrentPlayer)","ResetBillLights(CurrentPlayer)",55,10,0,0,0,False
	DisplayBillProgress
	CheckKingExtraBall

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Blue}:WIN Bill "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub WinDale
	Mode(CurrentPlayer,10) = 1
	Mode(CurrentPlayer,0) = 0
	Light144.State = 1	'I
	'LightSeqDale.StopPlay
	LightSeqHank2.StopPlay()

	DOF 141, DOFPulse

	StopMBVideo
	UpdateMusicNow

	Addbonus SCORE_MODE_COMPLETED

	pDMDSplashTwoLines "Dale Mode", "COMPLETED", 3, cLightBlue
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
	

	BallHandlingQueue.Add "ResetDaleLights(CurrentPlayer)","ResetDaleLights(CurrentPlayer)",55,10,0,0,0,False
	BallHandlingQueue.Add "DisplayDaleProgress","DisplayDaleProgress",50,100,0,0,0,False
	
	CheckHillExtraBall

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Blue}:WIN Dale "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub WinBoom
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,5) = 1
	Light145.State = 1	'N
	Light112.State = 0
	LightSeqBoom.StopPlay()
    LightSeqHank2.StopPlay()
	LightSeqBill.StopPlay()

	DOF 141, DOFPulse

	PuPlayer.playevent pDMDVideo,"Mode6","BoomStart.mp4",100,31,5,0,""
	UpdateMusicNow

	Addbonus SCORE_MODE_COMPLETED

	pDMDSplashTwoLines "Boom Mode", "COMPLETED", 3, cOrange
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
	
	BallHandlingQueue.Add "ResetBoomLights(CurrentPlayer)","ResetBoomLights(CurrentPlayer)",55,10,0,0,0,False
	DisplayBoomProgress
	CheckKingExtraBall

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Blue}:WIN Boom "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub WinBeer1
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,13) = 1

	LightSeqLadybird.StopPlay()
    LightSeqLuanne.StopPlay()

	DOF 141, DOFPulse

	ModeTimer.Enabled = 0
	HideTimerPopUp

	Addbonus SCORE_MODE_COMPLETED
	
	BallHandlingQueue.Add "ResetBeerLights(CurrentPlayer)","ResetBeerLights(CurrentPlayer)",55,10,0,0,0,False
	UpdateMusicNow

	pDMDSplashTwoLines "Boom Mode 1", "COMPLETED", 3, cOrange
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Blue}:WIN Beer 1 "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub WinBeer2
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,14) = 1

	LightSeqBeer2.StopPlay()
    LightSeqHank2.StopPlay()
	LightSeqLawnMower.StopPlay()
	LightSeqPropane.StopPlay()
	LightSeqBBQ2.StopPlay()

	DOF 141, DOFPulse

	Addbonus SCORE_MODE_COMPLETED
	BallHandlingQueue.Add "ResetBeerLights(CurrentPlayer)","ResetBeerLights(CurrentPlayer)",55,10,0,0,0,False
	UpdateMusicNow

	pDMDSplashTwoLines "Boom Mode 2", "COMPLETED", 3, cOrange
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Blue}:WIN Beer 2 "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub WinBeer3
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,15) = 1

	LightSeqBoom.StopPlay()
    LightSeqLadybird.StopPlay()
	LightSeqBill.StopPlay()
	LightSeqCotton.StopPlay()
	LightSeqLuanne.StopPlay()

	DOF 141, DOFPulse

	Addbonus SCORE_MODE_COMPLETED
	BallHandlingQueue.Add "ResetBeerLights(CurrentPlayer)","ResetBeerLights(CurrentPlayer)",55,10,0,0,0,False
	UpdateMusicNow

	pDMDSplashTwoLines "Beer Mode 3", "COMPLETED", 3, cOrange
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True

	addmultiball 3

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Blue}:WIN Beer 3 "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub WinLadybird
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,17) = 1
    LightSeqLadybird.StopPlay()

	DOF 141, DOFPulse

	ModeTimer.Enabled = 0
	HideTimerPopUp
	UpdateMusicNow
	BallHandlingQueue.Add "ResetLadybird(CurrentPlayer)","ResetLadybird(CurrentPlayer)",55,10,0,0,0,False

	PuPlayer.playevent pDMDVideo,"Mode10","LadybirdStart.mp4",100,31,5,0,""

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Blue}:WIN Ladybird "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub WinLawnmower
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,16) = 1

    LightSeqBoom.StopPlay()
    LightSeqBeer2.StopPlay()
    LightSeqLadybird.StopPlay()
    LightSeqLawnmower.StopPlay()
    LightSeqHank2.StopPlay()
    LightSeqPropane.StopPlay()
    LightSeqBill.StopPlay()
    LightSeqBBQ2.StopPlay()
    LightSeqCotton.StopPlay()
    LightSeqLuanne.StopPlay()

	DOF 141, DOFPulse

	
	BallHandlingQueue.Add "ResetLawnMowerAll","ResetLawnMowerAll",55,10,0,0,0,False
	bLockEngaged = False
	bMowerNotStarted = True
	nLockedBalls = 0
	UpdateLockKickers
	PuPlayer.playevent pDMDVideo,"Mode16","LawnMowerStart.mp4",100,71,5,0,""
	UpdateMusicNow

	ModeTimer.Enabled = 0
	HideTimerPopUp
	HideLockedBallPopUp
End Sub

Sub WinHank1
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,7) = 1
	LightSeqPeggy.StopPlay()
	Light006.State = 1

	DOF 141, DOFPulse

	Addbonus SCORE_MODE_COMPLETED
	UpdateMusicNow

	pDMDSplashTwoLines "Hank Mode 1", "COMPLETED", 3, cBlue
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
	BallHandlingQueue.Add "ResetHankLights(CurrentPlayer)","ResetHankLights(CurrentPlayer)",55,10,0,0,0,False
	DisplayHankProgress

	ModeTimer.Enabled = 0
	HideTimerPopUp

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Blue}:WIN Hank 1 "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub WinHank2
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,8) = 1

	Light005.State = 1

	DOF 141, DOFPulse

	Addbonus SCORE_MODE_COMPLETED
	UpdateMusicNow

	DMDQueue.Add "DelayHankStopSeq","DelayHankStopSeq",85,1200,0,0,0,False
	LightSeqBoom.StopPlay()
	LightSeqBeer2.StopPlay()
    LightSeqLadybird.StopPlay()
	LightSeqLawnMower.StopPlay()
    LightSeqHank2.StopPlay()
	LightSeqPropane.StopPlay()
	LightSeqBill.StopPlay()
    LightSeqBBQ2.StopPlay()
	LightSeqCotton.StopPlay()
    LightSeqLuanne.StopPlay()

	pDMDSplashTwoLines "Hank Mode 2", "COMPLETED", 3, cBlue
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
	BallHandlingQueue.Add "ResetHankLights(CurrentPlayer)","ResetHankLights(CurrentPlayer)",55,10,0,0,0,False
	DisplayHankProgress

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Blue}:WIN Hank 2 "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub DelayHankStopSeq
	StopModeRampSeq
	StopModeOrbitSeq
End Sub

Sub WinHank3
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,9) = 1
	UpdateMusicNow

	Light147.State = 1	'H

    LightSeqYEP.StopPlay()	
    LightSeqDrinkAlamo.StopPlay()

	DOF 141, DOFPulse

	Light109.State = 0 ' Football

	Light095.State = 0  ' CIA
	Light096.State = 0  ' NSA
	Light097.State = 0  ' FBI

	Light098.State = 0  'P
	Light099.State = 0  'E
	Light100.State = 0  'G

	Addbonus SCORE_MODE_COMPLETED

	pDMDSplashTwoLines "Hank Mode 3", "COMPLETED", 3, cBlue
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
	BallHandlingQueue.Add "ResetHankLights(CurrentPlayer)","ResetHankLights(CurrentPlayer)",55,10,0,0,0,False
	DisplayHankProgress
	CheckHillExtraBall

	ModeTimer.Enabled = 0
	HideTimerPopUp

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Blue}:WIN Hank 3 "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub WinPropane

End Sub

Sub WinBBQFeast
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,18) = 1
	StopModeRampSeq
	Light009.State = 0

	LightSeqBeer2.StopPlay()
    LightSeqHank2.StopPlay()
	LightSeqLawnMower.StopPlay()
	LightSeqPropane.StopPlay()
	LightSeqBBQ2.StopPlay()

	DOF 141, DOFPulse

	Addscore SCORE_BBQFEAST
	pDMDSplashTwoLines "BBQ Mode", "COMPLETED", 3, cGrey
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
	
	BallHandlingQueue.Add "ResetBBQFeast(CurrentPlayer)","ResetBBQFeast(CurrentPlayer)",55,10,0,0,0,False
	UpdateMusicNow

	ModeTimer.Enabled = 0
	HideTimerPopUp

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Blue}:WIN BBQFeast "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub WinCotton
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,11) = 1
	Light149.State = 1	'L

    LightSeqBoom.StopPlay()
    LightSeqBeer2.StopPlay()
    LightSeqLadybird.StopPlay()
    LightSeqLawnmower.StopPlay()
    LightSeqHank2.StopPlay()
    LightSeqPropane.StopPlay()
    LightSeqBill.StopPlay()
    LightSeqBBQ2.StopPlay()
    LightSeqCotton.StopPlay()
    LightSeqLuanne.StopPlay()

	DOF 141, DOFPulse

	UpdateMusicNow

	CottonModeChase.Enabled = False

	Addbonus SCORE_MODE_COMPLETED
	pDMDSplashTwoLines "Cotton Mode", "COMPLETED", 3, cLightGreen
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
	
	BallHandlingQueue.Add "ResetCottonLights(CurrentPlayer)","ResetCottonLights(CurrentPlayer)",55,10,0,0,0,False
	BallHandlingQueue.Add "DisplayCottonProgress","DisplayCottonProgress",55,200,0,0,0,False
	
	CheckHillExtraBall

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Blue}:WIN Cotton "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub WinLuanne
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,4) = 1
	Light148.State = 1	'I
	UpdateMusicNow

	LightSeqHank2.StopPlay()
	LightSeqPeggy.StopPlay()

	DOF 141, DOFPulse

	StopMBVideo

	Addbonus SCORE_MODE_COMPLETED
	pDMDSplashTwoLines "Luanne Mode", "COMPLETED", 3, cPurple
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
	
	BallHandlingQueue.Add "ResetLuanneLights(CurrentPlayer)","ResetLuanneLights(CurrentPlayer)",55,10,0,0,0,False
	DisplayLuanneProgress
	CheckKingExtraBall

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Blue}:WIN Luanne "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub WinBobby
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,12) = 1
	Light150.State = 1	'L

	LightSeqBoom.StopPlay()
	LightSeqBeer2.StopPlay()
	LightSeqDrinkAlamo.StopPlay()
    LightSeqLadybird.StopPlay()
	LightSeqLawnMower.StopPlay()
    LightSeqHank2.StopPlay()
	LightSeqPropane.StopPlay()
	LightSeqBill.StopPlay()
    LightSeqBBQ2.StopPlay()
	LightSeqCotton.StopPlay()
    LightSeqLuanne.StopPlay()
    LightSeqBobby.StopPlay()

	DOF 141, DOFPulse

	UpdateMusicNow

	StopMBVideo

	MVTargetMove.Enabled = 0
	MVTargetDescend.Enabled = 1

	Addbonus SCORE_MODE_COMPLETED
	pDMDSplashTwoLines "Bobby Mode", "COMPLETED", 3, cYellow
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
	
	BallHandlingQueue.Add "ResetBobbyLights(CurrentPlayer)","ResetBobbyLights(CurrentPlayer)",55,10,0,0,0,False
	RemoveCollisionsBobby
	DisplayBobbyProgress
	CheckHillExtraBall

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Blue}:WIN Bobby "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub WinDrinkAlamo
	ResetDrinkAlamo(CurrentPlayer)
	DOF 142, DOFPulse
End Sub

Sub WinPeggy1
	'LightEffect --- random ??
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,1) = 1
	Light101.State = 1	'Red
	UpdateMusicNow

	LightSeqBoom.StopPlay()
	LightSeqBeer2.StopPlay()
	LightSeqDrinkAlamo.StopPlay()
    LightSeqLadybird.StopPlay()
	LightSeqLawnMower.StopPlay()
    LightSeqHank2.StopPlay()
	LightSeqPropane.StopPlay()
	LightSeqBill.StopPlay()
    LightSeqBBQ2.StopPlay()
	LightSeqCotton.StopPlay()
    LightSeqLuanne.StopPlay()
    LightSeqBobby.StopPlay()

	DOF 141, DOFPulse

	Addbonus SCORE_MODE_COMPLETED

	pDMDSplashTwoLines "Peggy Mode 1", "COMPLETED", 3, cGreen
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
	BallHandlingQueue.Add "ResetPeggyLights(CurrentPlayer)","ResetPeggyLights(CurrentPlayer)",55,10,0,0,0,False
	
	DisplayPeggyProgress

	PupWinPeggy1

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Blue}:WIN Peggy 1 "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub WinPeggy2
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,2) = 1
	Light102.State = 1	'White
	UpdateMusicNow

	DOF 141, DOFPulse

	Addbonus SCORE_MODE_COMPLETED

	pDMDSplashTwoLines "Peggy Mode 2", "COMPLETED", 3, cGreen
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
	BallHandlingQueue.Add "ResetPeggyLights(CurrentPlayer)","ResetPeggyLights(CurrentPlayer)",55,10,0,0,0,False
	DisplayPeggyProgress

	RemoveCollisionsBobby	
	BallHandlingQueue.Add "RemoveCollisionsBobby","RemoveCollisionsBobby",55,500,0,0,0,False

	PupWinPeggy2

	ModeTimer.Enabled = 0
	HideTimerPopUp

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Blue}:WIN Peggy 2 "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

Sub WinPeggy3
	Mode(CurrentPlayer,0) = 0
	Mode(CurrentPlayer,3) = 1
	Light098.State = 1	'P
	Light099.State = 1	'E
	Light100.State = 1	'G
	Light130.State = 1  'Purple
	Light103.State = 1	'Blue 
	Light143.State = 1	'K
	UpdateMusicNow

    LightSeqLadybird.StopPlay()
    LightSeqHank2.StopPlay()
    LightSeqBobby.StopPlay()
    LightSeqLuanne.StopPlay()

	DOF 141, DOFPulse

	Addbonus SCORE_MODE_COMPLETED

	pDMDSplashTwoLines "Peggy Mode 3", "COMPLETED", 3, cGreen
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
	BallHandlingQueue.Add "ResetPeggyLights(CurrentPlayer)","ResetPeggyLights(CurrentPlayer)",55,10,0,0,0,False
	DisplayPeggyProgress

	PupWinPeggy3

	ModeTimer.Enabled = 0
	HideTimerPopUp

	if Scorbit.bSessionActive then
		GameModeStrTmp="BL{Blue}:WIN Peggy 3 "
		Scorbit.SetGameMode(GameModeStrTmp)
	End If
End Sub

'=============================

Sub CalcCompletedModes
	CompletedModes(CurrentPlayer) = 0
	CompletedModes(CurrentPlayer) = Mode(CurrentPlayer,3) mod 2 + Mode(CurrentPlayer,4) mod 2 + Mode(CurrentPlayer,5) mod 2 + Mode(CurrentPlayer,6) mod 2 + Mode(CurrentPlayer,9) mod 2 + Mode(CurrentPlayer,10) mod 2 + Mode(CurrentPlayer,11) mod 2 + Mode(CurrentPlayer,12) mod 2       
End Sub

'=============================

'*****************
''''' PROGRESS ''
'*****************

Sub CheckHankMode1Progress
	if nHankMode1Progress > 4 Then WinHank1
End Sub

Sub CheckHankMode2Progress
	if nHankMode2Progress > 5 Then WinHank2
End Sub

Sub CheckHankMode3Progress
	if nHankMode3Progress > 9 Then WinHank3
End Sub

Sub CheckBBQProgress
	if nBBQModeProgress > 14 Then WinBBQFeast
End Sub

Sub CheckBeerMode1
	if nBeerMode1Progress > 99 Then WinBeer1
End Sub

Sub CheckBeerMode2
	if nBeerMode2Progress > 5 Then WinBeer2
End Sub

Sub CheckBeerMode3
	if nBeerMode3Progress > 5 Then WinBeer3
End Sub

Sub CheckMowerProgress
	Dim tmpX, i
	tmpX = 0
	for i = 1 to 10
		if nMowerProgress(i) = 1 Then tmpX = TmpX + 1
	Next
	nMowerProgress(0) = tmpX
	Debug.print "PEGGY PROGRESS: " & tmpX 
	if tmpX = 5 or TmpX = 10 Then 
		Addscore SCORE_SUPER_JACKPOT
	Else
		Addscore SCORE_JACKPOT
	End If
End Sub


Sub CheckPeggyProgress
	Dim tmpX, i
	tmpX = 0
	for i = 1 to 11
		if nPeggyMode1Progress(i) = 1 Then tmpX = TmpX + 1
	Next
	nPeggyMode1Progress(0) = tmpX
	Debug.print "PEGGY PROGRESS: " & nPeggyMode1Progress(0)
	if tmpX > 9 Then 
		WinPeggy1
	Else
		PupProgressPeggy1
	End If
End Sub

Sub CheckPeggy3Progress
	Dim tmpX, i
	tmpX = 0
	for i = 1 to 4
		if nPeggyMode3Progress(i) = 1 Then tmpX = TmpX + 1
	Next
	Debug.print "PEGGY PROGRESS: " & tmpX 
	if tmpX > 3 Then 
		WinPeggy3
	Else
		PupProgressPeggy3
	End If
End Sub


' Hack to return Narnia ball back in play
Sub TimerNarnia_Timer

    Dim b, BOT
	BOT = getBalls
	For b = 0 to UBound(BOT)
		if BOT(b).z < -200 Then
			BOT(b).x = 902 : BOT(b).y = 2000  : BOT(b).z = 0
			BOT(b).angmomx= 0 : BOT(b).angmomy= 0 :	BOT(b).angmomz= 0
			BOT(b).velx = 0 : BOT(b).vely = 0 : BOT(b).velz = 0
			bAutoPlunger = True
			'KickerAutoPlunge.enabled = True
		end if
	next
end sub

Sub StopKOTHSequences
	LightSeqKOTH1.StopPlay()
	LightSeqKOTH2.StopPlay()
	LightSeqKOTH3.StopPlay()
	LightSeqKOTH4.StopPlay()
	LightSeqKOTH5.StopPlay()
	LightSeqKOTH6.StopPlay()
	LightSeqKOTH7.StopPlay()
	LightSeqKOTH8.StopPlay()
End Sub

Sub UpdateKOTHLights

	Select Case CompletedModes(CurrentPlayer)
		Case 0
			LightEffect 18
		Case Else
			StopKOTHSequences
	End Select

End Sub


Sub Gate002_Hit()

End Sub

Sub Target007_Hit()  'Target Near Football
	if Tilted Then Exit Sub
	
	light109.State = 2
	BallHandlingQueue.Add "light109.State = 0","Light109.State = 0",1,1500,0,0,0,False
	'Addscore SCORE_SMALL_TARGET

	if Mode(CurrentPlayer,6) = 2 Then addscore SCORE_JACKPOT ' Bill Mode
	if Mode(CurrentPlayer,7) = 2 Then nHankMode1Progress = nHankMode1Progress + 1
End Sub

'***************************************************************
' ZQUE: VPIN WORKSHOP ADVANCED QUEUING SYSTEM - 1.2.0
'***************************************************************
' WHAT IS IT?
' The VPin Workshop Advanced Queuing System allows table authors
' to put sub routine calls in a queue without creating a bunch
' of timers. There are many use cases for this: queuing sequences
' for light shows and DMD scenes, delaying solenoids until the
' DMD is finished playing all its sequences (such as holding a
' ball in a scoop), managing what actions take priority over
' others (e.g. an extra ball sequence is probably more important
' than a small jackpot), and many more.
'
' This system uses Scripting.Dictionary, a single timer, and the
' GameTime global to keep track of everything in the queue.
' This allows for better stability and a virtually unlimited
' number of items in the queue. It also allows for greater
' versatility, like pre-delays, queue delays, priorities, and
' even modifying items in the queue.
'
' The VPin Workshop Queuing System can replace vpmTimer as a
' proper queue system (each item depends on the previous)
' whereas vpmTimer is a collection of virtual timers that run
' in parallel. It also adds on other advanced functionality.
' However, this queue system does not have ROM support out of
' the box like vpmTimer does.
'
' I recommend reading all the comments before you implement the
' queuing system into your table.
'
' WHAT YOU NEED to use the queuing system:
' 1) Put this VBS file in your scripts folder, or copy / paste
'    the code into your table script (and skip step 2).
' 2) Include this file via Scripting.FileSystemObject, and
'    ExecuteGlobal it.
' 3) Make one or more queues by constructing the vpwQueueManager:
'    Dim queue : Set queue = New vpwQueueManager
' 4) Create (or use) a timer that is always enabled and
'    preferably has an interval of 1 millisecond. Use a
'    higher number for less time precision but less resource
'    use. You only need one timer even if you
'    have multiple queues.
' 5) For each queue you created, call its Tick routine in
'    the timer's *_timer() routine:
'    queue.Tick
' 6) You're done! Refer to the routines in vpwQueueManager to
'    learn how to use the queuing system.
'
' TUTORIAL: https://youtu.be/kpPYgOiUlxQ
'***************************************************************

'===========================================
' vpwQueueManager
' This class manages a queue of
' vpwQueueItems and executes them.
'===========================================
Class vpwQueueManager
	Public qItems ' A dictionary of vpwQueueItems in the queue (do NOT use native Scripting.Dictionary.Add/Remove; use the vpwQueueManager's Add/Remove methods instead!)
	Public preQItems ' A dictionary of vpwQueueItems pending to be added to qItems
	Public debugOn 'Null = no debug. String = activate debug by using this unique label for the queue. REQUIRES baldgeek's error logs.
	
	'----------------------------------------------------------
	' vpwQueueManager.qCurrentItem
	' This contains a string of the key currently active / at
	' the top of the queue. An empty string means no items are
	' active right now.
	' This is an important property; it should be monitored
	' in another timer or routine whenever you Add a queue item
	' with a -1 (indefinite) preDelay or postDelay. Then, for
	' preDelay, ExecuteCurrentItem should be called to run the
	' queue item. And for postDelay, DoNextItem should be
	' called to move to the next item in the queue.
	'
	' For example, let's say you add a queue item with the
	' key "kickTheBall" and an indefinite preDelay. You want
	' to wait until another timer fires before this queue item
	' executes and kicks the ball out of a scoop. In the other
	' timer, you will monitor qCurrentItem. Once it equals
	' "kickTheBall", call ExecuteCurrentItem, which will run
	' the queue item and presumably kick out the ball.
	'
	' WARNING!: If you do not properly execute one of these
	' callback routines on an indefinite delayed item, then
	' the queue will effectively freeze / stop until you do.
	'---------------------------------------------------------
	Public qCurrentItem
	
	Public preDelayTime ' The GameTime the preDelay for the qCurrentItem was started
	Public postDelayTime ' The GameTime the postDelay for the qCurrentItem was started
	
	Private onQueueEmpty ' A string or object to be called every time the queue empties (use the QueueEmpty property to get/set this)
	Private queueWasEmpty ' Boolean to determine if the queue was already empty when firing DoNextItem
	Private preDelayTransfer ' Number of milliseconds of preDelay to transfer over to the next queue item when doNextItem is called
	
	Private Sub Class_Initialize
		Set qItems = CreateObject("Scripting.Dictionary")
		Set preQItems = CreateObject("Scripting.Dictionary")
		qCurrentItem = ""
		onQueueEmpty = ""
		queueWasEmpty = True
		debugOn = Null
		preDelayTransfer = 0
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.Tick
	' This is where all the magic happens! Call this method in
	' your timer's _timer routine to check the queue and
	' execute the necessary methods. We do not iterate over
	' every item in the queue here, which allows for superior
	' performance even if you have hundreds of items in the
	' queue.
	'----------------------------------------------------------
	Public Sub Tick()
		Dim item
		If qItems.Count > 0 Then ' Don't waste precious resources if we have nothing in the queue
			
			' If no items are active, or the currently active item no longer exists, move to the next item in the queue.
			' (This is also a failsafe to ensure the queue continues to work even if an item gets manually deleted from the dictionary).
			If qCurrentItem = "" Or Not qItems.Exists(qCurrentItem) Then
				DoNextItem
			Else ' We are good; do stuff as normal
				Set item = qItems.item(qCurrentItem)
				
				If item.Executed Then
					' If the current item was executed and the post delay passed, go to the next item in the queue
					If item.postDelay >= 0 And GameTime >= (postDelayTime + item.postDelay) Then
						DebugLog qCurrentItem & " - postDelay of " & item.postDelay & " passed."
						DoNextItem
					End If
				Else
					' If the current item expires before it can be executed, go to the next item in the queue
					If item.timeToLive > 0 And GameTime >= (item.queuedOn + item.timeToLive) Then
						DebugLog qCurrentItem & " - expired (Time To live). Moving To the Next queue item."
						DoNextItem
					End If
					
					' If the current item was not executed yet and the pre delay passed, then execute it
					If item.preDelay >= 0 And GameTime >= (preDelayTime + item.preDelay) Then
						DebugLog qCurrentItem & " - preDelay of " & item.preDelay & " passed. Executing callback."
						item.Execute
						preDelayTime = 0
						postDelayTime = GameTime
					End If
				End If
			End If
		End If
		
		' Loop through each item in the pre-queue to find any that is ready to be added
		If preQItems.Count > 0 Then
			Dim k, key
			k = preQItems.Keys
			For Each key In k
				Set item = preQItems.Item(key)
				
				' If a queue item was pre-queued and is ready to be considered as actually in the queue, add it
				If GameTime >= (item.queuedOn + item.preQueueDelay) Then
					DebugLog key & " (preQueue) - preQueueDelay of " & item.preQueueDelay & " passed. Item added To the main queue."
					preQItems.Remove key
					Me.Add key, item.Callback, item.priority, 0, item.preDelay, item.postDelay, item.timeToLive, item.executeNow
				End If
			Next
		End If
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.DoNextItem
	' Goes to the next item in the queue and deletes the
	' currently active one.
	'----------------------------------------------------------
	Public Sub DoNextItem()
		If Not qCurrentItem = "" Then
			If qItems.Exists(qCurrentItem) Then qItems.Remove qCurrentItem ' Remove the current item from the queue if it still exists
			qCurrentItem = ""
		End If
		
		If qItems.Count > 0 Then
			Dim k, key
			Dim nextItem
			Dim nextItemPriority
			Dim item
			nextItemPriority = 0
			nextItem = ""
			
			' Find which item needs to run next based on priority first, queue order second (ignore items with an active preQueueDelay)
			k = qItems.Keys
			For Each key In k
				Set item = qItems.Item(key)
				
				If item.preQueueDelay <= 0 And item.priority > nextItemPriority Then
					nextItem = key
					nextItemPriority = item.priority
				End If
			Next
			
			If qItems.Exists(nextItem) Then
				Set item = qItems.Item(nextItem)
				DebugLog "DoNextItem - checking " & nextItem & " (priority " & item.priority & ")"
				
				' Make sure the item is not expired and not already executed. If it is, remove it and re-call doNextItem
				If (item.timeToLive > 0 And GameTime >= (item.queuedOn + item.timeToLive + preDelayTransfer)) Or item.executed = True Then
					DebugLog "DoNextItem - " & nextItem & " expired (Time To live) Or already executed. Removing And going To the Next item."
					qItems.Remove nextItem
					DoNextItem
					Exit Sub
				End If
				
				'Transfer preDelay time when applicable
				If preDelayTransfer > 0 And item.preDelay > -1 Then
					DebugLog "DoNextItem " & nextItem & " - Transferred remaining postDelay of " & preDelayTransfer & " milliseconds from previously overridden queue item To its preDelay And timeToLive"
					qItems.Item(nextItem).preDelay = item.preDelay + preDelayTransfer
					If item.timeToLive > 0 Then qItems.Item(nextItem).timeToLive = item.timeToLive + preDelayTransfer
					preDelayTransfer = 0
				End If
				
				' Set item as current / active, and execute if it has no pre-delay (otherwise Tick will take care of pre-delay)
				qCurrentItem = nextItem
				If item.preDelay = 0 Then
					DebugLog "DoNextItem - " & nextItem & " Now active. It has no preDelay, so executing callback immediately."
					item.Execute
					preDelayTime = 0
					postDelayTime = GameTime
				Else
					DebugLog "DoNextItem - " & nextItem & " Now active. Waiting For a preDelay of " & item.preDelay & " before executing."
					preDelayTime = GameTime
					postDelayTime = 0
				End If
			End If
		ElseIf queueWasEmpty = False Then
			DebugLog "DoNextItem - Queue Is Now Empty; executing queueEmpty callback."
			CallQueueEmpty() ' Call QueueEmpty if this was the last item in the queue
		End If
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.ExecuteCurrentItem
	' Helper routine that can be used when the current item is
	' on an indefinite preDelay. Call this when you are ready
	' for that item to execute.
	'----------------------------------------------------------
	Public Sub ExecuteCurrentItem()
		If Not qCurrentItem = "" And qItems.Exists(qCurrentItem) Then
			DebugLog "ExecuteCurrentItem - Executing the callback For " & qCurrentItem & "."
			Dim item
			Set item = qItems.Item(qCurrentItem)
			item.Execute
			preDelayTime = 0
			postDelayTime = GameTime
		End If
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.Add
	' REQUIRES Class vpwQueueItem
	'
	' Add an item to the queue.
	'
	' PARAMETERS:
	'
	' key (string) - Unique name for this queue item
	' WARNING: Specifying a key that already exists will
	' overwrite the item in the queue. This is by design. Also
	' note the following behaviors:
	' * Tickers / clocks for tracking delay times will NOT be
	' restarted for this item (but the total duration will be
	' updated. For example, if the old preDelay was 3 seconds
	' and 2 seconds elapsed, but Add was called to update
	' preDelay to 5 seconds, then the queue item will now
	' execute in 3 more seconds (new preDelay - time elapsed)).
	' However, timeToLive WILL be restarted.
	' * Items will maintain their same place in the queue.
	' * If key = qCurrentItem (overwriting the currently active
	' item in the queue) and qCurrentItem already executed
	' the callback (but is waiting for a postDelay), then the
	' current queue item's remaining postDelay will be added to
	' the preDelay of the next item, and this item will be
	' added to the bottom of the queue for re-execution.
	' If you do not want it to re-execute, then add an If
	' guard on your call to the Add method checking
	' "If Not vpwQueueManager.qCurrentItem = key".
	'
	' qCallback (object|string) - An object to be called,
	' or string to be executed globally, when this queue item
	' runs. I highly recommend making sub routines for groups
	' of things that should be executed by the queue so that
	' your qCallback string does not get long, and you can
	' easily organize your callbacks. Also, use double
	' double-quotes when the call itself has quotes in it
	' (VBScript escaping).
	' Example: "playsound ""Plunger"""
	'
	' priority (number) - Items in the queue will be executed
	' in order from highest priority to lowest. Items with the
	' same priority will be executed in order according to
	' when they were added to the queue. Use any number
	' greater than 0. My recommendation is to make a plan for
	' your table on how you will prioritize various types of
	' queue items and what priority number each type should
	' have. Also, you should reserve priority 1 (lowest) to
	' items which should wait until everything else in the
	' queue is done (such as ejecting a ball from a scoop).
	'
	' preQueueDelay (number) - The number of
	' milliseconds before the queue actually considers this
	' item as "in the queue" (pretend you started a timer to
	' add this item into the queue after this delay; this
	' logically works in a similar way; the only difference is
	' timeToLive is still considered even when an item is
	' pre-queued.) Set to 0 to add to the queue immediately.
	' NOTE: this should be less than timeToLive.
	'
	' preDelay (number) - The number of milliseconds before
	' the qCallback executes once this item is active (top)
	' in the queue. Set this to 0 to immediately execute the
	' qCallback when this item becomes active.
	' Set this to -1 to have an indefinite delay until
	' vpwQueueManager.ExecuteCurrentItem is called (see the
	' comment for qCurrentItem for more information).
	' NOTE: this should be less than timeToLive. And, if
	' timeToLive runs out before preDelay runs out, the item
	' will be removed and will not execute.
	'
	' postDelay (number) - After the qCallback executes, the
	' number of milliseconds before moving on to the next item
	' in the queue. Set this to -1 to have an indefinite delay
	' until vpwQueueManager.DoNextItem is called (see the
	' comment for qCurrentItem for more information).
	'
	' timeToLive (number) - After this item is added to the
	' queue, the number of milliseconds before this queue item
	' expires / is removed if the qCallback is not executed by
	' then. Set to 0 to never expire. NOTE: If not 0, this
	' should be greater than preDelay + preQueueDelay or the
	' item will expire before the qCallback is executed.
	' Example use case: Maybe a player scored a jackpot, but
	' it would be awkward / irrelevant to play that jackpot
	' sequence if it hasn't played after a few seconds (e.g.
	' other items in the queue took priority).
	'
	' executeNow (boolean) - Specify true if this item
	' should interrupt the queue and run immediately. This
	' will only happen, however, if the currently active item
	' has a priority less than or equal to the item you are
	' adding. Note this does not bypass preQueueDelay nor
	' preDelay if set.
	' Example: If a player scores an extra ball, you might
	' want that to interrupt everything else going on as it
	' is an important milestone.
	'----------------------------------------------------------
	Public Sub Add(key, qCallback, priority, preQueueDelay, preDelay, postDelay, timeToLive, executeNow)
		DebugLog "Adding queue item " & key
		
		'Construct the item class
		Dim newClass
		Set newClass = New vpwQueueItem
		With newClass
			.Callback = qCallback
			.priority = priority
			.preQueueDelay = preQueueDelay
			.preDelay = preDelay
			.postDelay = postDelay
			.timeToLive = timeToLive
			.executeNow = executeNow
		End With
		
		'If we are attempting to overwrite the current queue item which already executed, take the remaining postDelay and add it to the preDelay of the next item. And set us up to immediately go to the next item while re-adding this item to the queue.
		If preQueueDelay <= 0 And qItems.Exists(key) And qCurrentItem = key Then
			If qItems.Item(key).executed = True Then
				DebugLog key & " (Add) - Attempting To overwrite the current queue item which already executed. Immediately re-queuing this item To the bottom of the queue, transferring the remaining postDelay To the Next item, And going To the Next item."
				If qItems.Item(key).postDelay >= 0 Then
					preDelayTransfer = ((postDelayTime + qItems.Item(key).postDelay) - GameTime)
				End If
				
				'Remove current queue item so we can go to the next item, this can be re-queued to the bottom, and the remaining postDelay transferred to the preDelay of the next item
				qItems.Remove qCurrentItem
				qCurrentItem = ""
			End If
		End If
		
		' Determine execution stuff if this item does not have a pre-queue delay
		If preQueueDelay <= 0 Then
			If executeNow = True Then
				' Make sure this item does not immediately execute if the current item has a higher priority
				If Not qCurrentItem = "" And qItems.Exists(qCurrentItem) Then
					Dim item
					Set item = qItems.Item(qCurrentItem)
					If item.priority <= priority Then
						DebugLog key & " (Add) - Execute Now was Set To True And this item's priority (" & priority & ") Is >= the active item's priority (" & item.priority & " from " & qCurrentItem & "). Making it the current active queue item."
						qCurrentItem = key
						If preDelay = 0 And preDelayTransfer = 0 Then
							DebugLog key & " (Add) - No pre-delay. Executing the callback immediately."
							newClass.Execute
							preDelayTime = 0
							postDelayTime = GameTime
						Else
							DebugLog key & " (Add) - Waiting For a pre-delay of " & (preDelay + preDelayTransfer) & " before executing the callback."
							preDelayTime = GameTime
							postDelayTime = 0
						End If
					Else
						DebugLog key & " (Add) - Execute Now was Set To True, but this item's priority (" & priority & ") Is Not >= the active item's priority (" & item.priority & " from " & qCurrentItem & "). This item will Not be executed Now And will be added To the queue normally."
					End If
				Else
					DebugLog key & " (Add) - Execute Now was Set To True And no item was active In the queue. Making it the current active queue item."
					qCurrentItem = key
					If preDelay = 0 Then
						DebugLog key & " (Add) - No pre-delay. Executing the callback immediately."
						preDelayTransfer = 0 'No preDelay transfer if we are immediately re-executing the same queue item
						newClass.Execute
						preDelayTime = 0
						postDelayTime = GameTime
					Else
						DebugLog key & " (Add) - Waiting For a pre-delay of " & preDelay & " before executing the callback."
						preDelayTime = GameTime
						postDelayTime = 0
					End If
				End If
			End If
			If qItems.Exists(key) Then 'Overwrite existing item in the queue if it exists
				DebugLog key & " (Add) - Already exists In the queue. Updating the item With the new parameters passed In Add."
				Set qItems.Item(key) = newClass
			Else
				DebugLog key & " (Add) - Added To the queue."
				qItems.Add key, newClass
			End If
			queueWasEmpty = False
		Else
			If preQItems.Exists(key) Then 'Overwrite existing item in the preQueue if it exists
				DebugLog key & " (Add) - Already exists In the preQueue. Updating the item With the new parameters passed In Add."
				Set preQItems.Item(key) = newClass
			Else
				DebugLog key & " (Add) - Added To the preQueue."
				preQItems.Add key, newClass
			End If
		End If
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.Remove
	'
	' Removes an item from the queue. It is better to use this
	' than to remove the item from qItems directly as this sub
	' will also call DoNextItem to advance the queue if
	' the item removed was the active item.
	' NOTE: This only removes items from qItems; to remove
	' an item from preQItems, use the standard
	' Scripting.Dictionary Remove method.
	'
	' PARAMETERS:
	'
	' key (string) - Unique name of the queue item to remove.
	'----------------------------------------------------------
	Public Sub Remove(key)
		If qItems.Exists(key) Then
			DebugLog key & " (Remove)"
			qItems.Remove key
			If qCurrentItem = key Or qCurrentItem = "" Then DoNextItem ' Ensure the queue does not get stuck
		End If
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.RemoveAll
	'
	' Removes all items from the queue / clears the queue.
	' It is better to call this sub than to remove all items
	' from qItems directly because this sub cleans up the queue
	' to ensure it continues to work properly.
	'
	' PARAMETERS:
	'
	' preQueue (boolean) - Also clear the pre-queue.
	'----------------------------------------------------------
	Public Sub RemoveAll(preQueue)
		DebugLog "Queue was emptied via RemoveAll."
		
		' Loop through each item in the queue and remove it
		Dim k, key
		k = qItems.Keys
		For Each key In k
			qItems.Remove key
		Next
		qCurrentItem = ""
		
		If queueWasEmpty = False Then CallQueueEmpty() ' Queue is now empty, so call our callback if applicable
		
		If preQueue Then
			k = preQItems.Keys
			For Each key In k
				preQItems.Remove key
			Next
		End If
	End Sub
	
	'----------------------------------------------------------
	' Get vpwQueueManager.QueueEmpty
	' Get the current callback for when the queue is empty.
	'----------------------------------------------------------
	Public Property Get QueueEmpty()
		If IsObject(onQueueEmpty) Then
			Set QueueEmpty = onQueueEmpty
		Else
			QueueEmpty = onQueueEmpty
		End If
	End Property
	
	'----------------------------------------------------------
	' Let vpwQueueManager.QueueEmpty
	' Set the callback to call every time the queue empties.
	' This could be useful for setting a sub routine to be
	' called each time the queue empties for doing things such
	' as ejecting balls from scoops. Unlike using the Add
	' method, this callback is immune from getting removed by
	' higher priority items in the queue and will be called
	' every time the queue is emptied, not just once.
	'
	' PARAMETERS:
	'
	' callback (object|string) - The callback to call every
	' time the queue empties.
	'----------------------------------------------------------
	Public Property Let QueueEmpty(callback)
		If IsObject(callback) Then
			Set onQueueEmpty = callback
		ElseIf VarType(callback) = vbString Then
			onQueueEmpty = callback
		End If
	End Property
	
	'----------------------------------------------------------
	' Get vpwQueueManager.CallQueueEmpty
	' Private method that actually calls the QueueEmpty
	' callback.
	'----------------------------------------------------------
	Private Sub CallQueueEmpty()
		If queueWasEmpty = True Then Exit Sub
		queueWasEmpty = True
		
		If IsObject(onQueueEmpty) Then
			Call onQueueEmpty(0)
		ElseIf VarType(onQueueEmpty) = vbString Then
			If onQueueEmpty > "" Then ExecuteGlobal onQueueEmpty
		End If
	End Sub
	
	'----------------------------------------------------------
	' DebugLog
	' Log something if debugOn is not null.
	' REQUIRES / uses the WriteToLog sub from Baldgeek's
	' error log library.
	'----------------------------------------------------------
	Private Sub DebugLog(message)
		If Not IsNull(debugOn) Then
			WriteToLog "VPW Queue " & debugOn, message
		End If
	End Sub
End Class

'===========================================
' vpwQueueItem
' Represents a single item for the queue
' system. Do NOT use this class directly.
' Instead, use the vpwQueueManager.Add
' routine.

' You can, however, access an individual
' item in the queue via
' vpwQueueManager.qItems and then modify
' its properties while it is still in the
' queue.
'===========================================
Class vpwQueueItem  ' Do not construct this class directly; use vpwQueueManager.Add instead, and vpwQueueManager.qItems.Item(key) to modify an item's properties.
	Public priority ' The item's set priority
	Public timeToLive ' The item's set timeToLive milliseconds requested
	Public preQueueDelay ' The item's pre-queue milliseconds requested
	Public preDelay ' The item's pre delay milliseconds requested
	Public postDelay ' The item's post delay milliseconds requested
	Public executeNow ' Whether the item was set to Execute immediately
	Private qCallback ' The item's callback object or string (use the Callback property on the class to get/set it)
	
	Public executed ' Whether or not this item's qCallback was executed yet
	Public queuedOn ' The game time this item was added to the queue
	Public executedOn ' The game time this item was executed
	
	Private Sub Class_Initialize
		' Defaults
		priority = 0
		timeToLive = 0
		preQueueDelay = 0
		preDelay = 0
		postDelay = 0
		qCallback = ""
		executeNow = False
		
		queuedOn = GameTime
		executedOn = 0
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueItem.Execute
	' Executes the qCallback on this item if it was not yet
	' already executed.
	Public Sub Execute()
		If executed Then Exit Sub ' Do not allow an item's qCallback to ever Execute more than one time
		
		'Mark as execute before actually executing callback; that way, if callback recursively adds the item back into the queue, then we can properly handle it.
		executed = True
		executedOn = GameTime
		
		' Execute qCallback
		If IsObject(qCallback) Then
			Call qCallback(0)
		ElseIf VarType(qCallback) = vbString Then
			If qCallback > "" Then ExecuteGlobal qCallback
		End If
	End Sub
	
	Public Property Get Callback()
		If IsObject(qCallback) Then
			Set Callback = qCallback
		Else
			Callback = qCallback
		End If
	End Property
	
	Public Property Let Callback(cb)
		If IsObject(cb) Then
			Set qCallback = cb
		ElseIf VarType(cb) = vbString Then
			qCallback = cb
		End If
	End Property
End Class


Sub QueueTimer_Timer()
	BallHandlingQueue.Tick
	GeneralPupQueue.Tick
	DMDQueue.Tick
	AudioQueue.Tick
	EOBQueue.Tick	
End Sub

Sub WipeAllQueues
	GeneralPupQueue.RemoveAll(True)
	AudioQueue.RemoveAll(True)
	BallHandlingQueue.RemoveAll(True)
	DMDQueue.RemoveAll(True)
	EOBQueue.RemoveAll(True)
End Sub

'***************************************************************
' END VPIN WORKSHOP ADVANCED QUEUING SYSTEM
'***************************************************************
Sub Modetimer_Timer()
	if nTimerCount < 1 Then ExitModeTimer

	if nTimerCount > 99 Then
		PuPlayer.LabelSet pDMD,"TimerValue",nTimerCount,1,"{'mt':2,'fonth':7,'xalign':1,'yalign':1,'xpos':26.75,'ypos':10.5}"
	Else
		PuPlayer.LabelSet pDMD,"TimerValue",nTimerCount,1,"{'mt':2,'fonth':8.5,'xalign':1,'yalign':1,'xpos':27,'ypos':10.5}"
	End If	

	nTimerCount = nTimerCount - 1
End Sub

Sub ExitModetimer
	ModeTimer.Enabled = False
	HideTimerPopUp

	Select Case Mode(CurrentPlayer,0)
		Case 1

		Case 2
			StopPeggy2
		Case 3
			StopPeggy3
		Case 4

		Case 5

		Case 6

		Case 7
			StopHank1
		Case 8

		Case 9
			StopHank3
		Case 10
	
		Case 11

		Case 12
			StopBobby
		Case 13
			StopBeer1
		Case 14

		Case 15
			StopBeer1
		Case 16
			WinLawnmower
		Case 17
			WinLadybird
		Case 18
			StopBBQFeast 
	End Select

End Sub

Sub ChaseTimerReset
	CottonModeChase.Enabled = False
	CottonModeChase.Enabled = True
End Sub

Sub CottonModeChase_Timer()
	 nNewCottonShot = rndNumNot(1,10,nCottonActive)
	ChaseLights
End Sub

Sub PlunngerTrigger_Hit()
	if bSkillshotReady Then ResetSkillshotTimer.Enabled = True
	LastTriggerHit = "PlungerTrigger"
End Sub

Sub ResetSkillShotTimer_Timer()
	bSkillshotReady = False
	ResetSkillshotTimer.Enabled = False
End Sub

Sub AwardSkillshot
	ResetSkillShotTimer_Timer
	AddScore SCORE_SKILLSHOT
	pDMDSplashTwoLines "SKILLSHOT",FormatScore(SCORE_SKILLSHOT),3, cRed
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,3100,0,0,0,True
End Sub

Sub Gate004_Hit()
	Debug.print "Gate 4"
	if Tilted Then Exit Sub

	RampCount = RampCount + 1

	Select Case Mode(CurrentPlayer,0)
		Case 0
			anHankLights(CurrentPlayer,0) = anHankLights(CurrentPlayer,0) + 1
			if anHankLights(CurrentPlayer,0) > 5 Then anHankLights(CurrentPlayer,0) = 5  : CheckHank ' Start Mode	
			anHankLights(CurrentPlayer,anHankLights(CurrentPlayer,0)) = 1
			DisplayHankProgress
		Case 1
			if Mode(CurrentPlayer,1) = 2 Then
				if nPeggyMode1Progress(5) = 0 Then
					nPeggyMode1Progress(5) = 1
					addscore SCORE_MODE_PROGRESS
					LightSeqHank2.StopPlay()
					CheckPeggyProgress
				End If
			End If
		Case 3
			if Mode(CurrentPlayer,3) = 2 Then
				if nPeggyMode3Progress(2) = 0 Then
					nPeggyMode3Progress(2) = 1
					addscore SCORE_MODE_PROGRESS
					LightSeqHank2.StopPlay()
					CheckPeggy3Progress
				End If
			End If
		Case 4
			if Mode(CurrentPlayer,4) = 2 Then addscore SCORE_JACKPOT  ' Luanne Mode
		Case 5
			if Mode(CurrentPlayer,5) = 2 Then addscore SCORE_JACKPOT ' Boom Mode
		Case 8
			if Mode(CurrentPlayer,8) = 2 And bHankMode2Orbit Then 
				nHankMode2Progress = nHankMode2Progress + 1
				CheckHankMode2Progress
				AddScore SCORE_MODE_PROGRESS
				StopModeRampSeq
				LightEffect 29
			End If
		Case 10
			if Mode(CurrentPlayer,10) = 2 Then addscore SCORE_JACKPOT ' Dale Mode
		Case 11
			if Mode(CurrentPlayer,11) = 2 And nCottonActive = 5 Then 
				nCottonShots(5) = 1
				nCottonModeProgress = nCottonModeProgress + 1
				nNewCottonShot = rndNumNot(1,10,nCottonActive) 
				ChaseLights
				ChaseTimerReset
			End If
		Case 12
			if Mode(CurrentPlayer,12) = 2 Then 
				if BobbyTargetCount = 0 Then
					addscore SCORE_SUPER_JACKPOT ' Bobby Mode
				Elseif BobbyTargetCount < 3 Then
					addscore SCORE_JACKPOT ' Bobby Mode
				End If
			End If
		Case 14
			if mode(currentplayer,14) = 2 Then nBeerMode2Progress = nBeerMode2Progress + 1 :  CheckBeerMode2
		Case 16
			if Mode(CurrentPlayer,16) = 2 Then
				if nMowerProgress(5) = 0 Then
					nMowerProgress(5) = 1
					LightSeqHank2.StopPlay()
					CheckMowerProgress
				End If
			End If
		Case 18
			if Mode(CurrentPlayer,18) = 2 Then 	
				nBBQModeProgress = nBBQModeProgress + 1
				CheckBBQProgress
			End If
		Case 19
			if Mode(CurrentPlayer,19) = 2 Then addscore SCORE_SUPER_JACKPOT
	End Select

	LastSwitchHit = "Gate004"
End Sub

dim bOneBall
Sub TractorKicker1_Hit()
If TractorKicker1.BallCntOver = 0 Then Debug.print "Ball CNT: " 

	if bReleasingBalls Then
		'Do Nothing
		Debug.print "ball b1"
	Elseif Mode(CurrentPlayer,0) <> 0 Then 
		AdvanceLockedBallsOnce
		'UpdateTrough
	Else
		Debug.print "ball 1: " &bReleasingBalls
		bAutoPlunger = True
		addmultiball 1
		nLockedBalls = 1
		PupLockBallDisplay
		UpdateLockKickers
	End If
End Sub

Sub TractorKicker2_Hit()
	if bReleasingBalls Then
		'Do Nothing
		Debug.print "ball b2"
	Elseif Mode(CurrentPlayer,0) <> 0 Then 
		AdvanceLockedBallsOnce
		'UpdateTrough
	Else
		Debug.print "ball 2: " &bReleasingBalls
		bAutoPlunger = True
		addmultiball 1
		nLockedBalls = 2
		PupLockBallDisplay
		UpdateLockKickers
	End If
End Sub

Sub TractorKicker3_Hit()
'	addmultiball 1
	'nLockedBalls = 3
'	UpdateLockKickers
	if bReleasingBalls Then
		'Do Nothing
		Debug.print "ball b3"
	Elseif Mode(CurrentPlayer,0) <> 0 Then 
		AdvanceLockedBallsOnce
		'UpdateTrough
	Else
		Debug.print "ball 3"
		bAutoPlunger = True
		addmultiball 1
		nLockedBalls = 3
		PupLockBallDisplay
		UpdateLockKickers
	End If
End Sub

Sub TractorKicker4_Hit()
	if bReleasingBalls Then
		'Do Nothing
		Debug.print "ball b4"
	Elseif Mode(CurrentPlayer,0) <> 0 Then 
		AdvanceLockedBallsOnce
		'UpdateTrough
	Else
		Debug.print "ball 4"
		bAutoPlunger = True
		addmultiball 1
		nLockedBalls = 4
		PupLockBallDisplay
		UpdateLockKickers
	End If
End Sub

Sub TractorKicker5_Hit()
	'nLockedBalls = 5
	' Start Multiball
	if bReleasingBalls Then
		'Do Nothing
		Debug.print "ball b1"
	Elseif Mode(CurrentPlayer,0) <> 0 Then 
		AdvanceLockedBallsOnce
		'UpdateTrough
	Else
		Debug.print "START MULTIBALL"
		bReleasingBalls = True
		nLockedBalls = 5
		if Mode(CurrentPlayer,0) = 0 Then
			ClearTwoLines
			HideLockedBallPopUp
			StartLawnMower
		Else
			AdvanceLockedBallsOnce
		End If
	End If
End Sub

Const nLockSpeed = 100
AdvanceKickers.Interval = 1000
Sub AdvanceLockedBallsOnce
	bReleasingBalls = True
	If TractorKicker1.BallCntOver = 1 Then TractorKicker1.kick 150, 2
	If TractorKicker2.BallCntOver = 1 Then TractorKicker2.kick 150, 2
	If TractorKicker3.BallCntOver = 1 Then TractorKicker3.kick 150, 2
	If TractorKicker4.BallCntOver = 1 Then TractorKicker4.kick 150, 2
	If TractorKicker5.BallCntOver = 1 Then TractorKicker5.kick 150, 2
End Sub

Sub KickLockedBall(b)
	Select Case b
		Case 1
			TractorKicker1.Kick 150, 1
		Case 2
			TractorKicker2.Kick 150, 1
		Case 3
			TractorKicker3.Kick 150, 1
		Case 4
			TractorKicker4.Kick 150, 1
		Case 5
			TractorKicker5.Kick 150, 1
	End Select
End Sub

Const nLockAllSpeed = 100
AdvanceKickers.Interval = nLockedBalls*500+10
Sub AdvanceKickers_Timer
	AdvanceKickers.Interval = nLockedBalls*500+10

	Select Case nLockedBalls
		Case 0
			' Should never hit this
			Debug.print " *****************"
			Debug.print " ******  WTF *****"
			Debug.print " *****************"
			bReleasingBalls = False
			bLockEngaged = False	
			UpdateLockKickers
			HideLockedBallPopUp
			'ResetBallLocks
		Case 1
			BallsOnPlayfield = BallsOnPlayfield + 1
			KickLockedBall 1
			' Make sure no balls were left locked in kickers
			LetEmGoFast
		Case 2
			BallsOnPlayfield = BallsOnPlayfield + 1
			KickLockedBall 1
			BallHandlingQueue.Add "KickLockedBall 2","KickLockedBall 2",1,nLockAllSpeed,0,0,0,False
			'AdvanceKickers.Interval = nLockedBalls*250
		Case 3
			BallsOnPlayfield = BallsOnPlayfield + 1
			KickLockedBall 1
			BallHandlingQueue.Add "KickLockedBall 2","KickLockedBall 2",1,nLockAllSpeed,0,0,0,False
			BallHandlingQueue.Add "KickLockedBall 3","KickLockedBall 3",1,nLockAllSpeed*2,0,0,0,False
			'AdvanceKickers.Interval = nLockedBalls*250
		Case 4
			BallsOnPlayfield = BallsOnPlayfield + 1
			KickLockedBall 1
			BallHandlingQueue.Add "KickLockedBall 2","KickLockedBall 2",1,nLockAllSpeed,0,0,0,False
			BallHandlingQueue.Add "KickLockedBall 3","KickLockedBall 3",1,nLockAllSpeed*2,0,0,0,False
			BallHandlingQueue.Add "KickLockedBall 4","KickLockedBall 4",1,nLockAllSpeed*3,0,0,0,False
			'AdvanceKickers.Interval = nLockedBalls*250
		Case 5
			BallsOnPlayfield = BallsOnPlayfield + 1
			KickLockedBall 1
			BallHandlingQueue.Add "KickLockedBall 2","KickLockedBall 2",1,nLockAllSpeed,0,0,0,False
			BallHandlingQueue.Add "KickLockedBall 3","KickLockedBall 3",1,nLockAllSpeed*2,0,0,0,False
			BallHandlingQueue.Add "KickLockedBall 4","KickLockedBall 4",1,nLockAllSpeed*3,0,0,0,False
			BallHandlingQueue.Add "KickLockedBall 5","KickLockedBall 5",1,nLockAllSpeed*4,0,0,0,False	
			'AdvanceKickers.Interval = nLockedBalls*250
	End Select
	if nLockedBalls = 0 Then AdvanceKickers.Enabled = False : MoveLawnMowerReverse
	nLockedBalls = nLockedBalls - 1
	'if bReleasingBalls = False Then DisplayLockPopUp
	PupLockBallDisplay
End Sub


Sub LetEmGo
	bReleasingBalls = True
	bLockEngaged = False
	ResetBallLocks

	BallHandlingQueue.Add "KickLockedBall 1","KickLockedBall 1",1,250,0,0,0,False
	BallHandlingQueue.Add "KickLockedBall 2","KickLockedBall 2",1,500,0,0,0,False
	BallHandlingQueue.Add "KickLockedBall 3","KickLockedBall 3",1,750,0,0,0,False
	BallHandlingQueue.Add "KickLockedBall 4","KickLockedBall 4",1,1000,0,0,0,False
	BallHandlingQueue.Add "KickLockedBall 5","KickLockedBall 5",1,1250,0,0,0,False
End Sub

Sub LetEmGoFast
	bReleasingBalls = True
	bLockEngaged = False
	ResetBallLocks

	BallHandlingQueue.Add "KickLockedBall 2","KickLockedBall 2",1,20,0,0,0,False
	BallHandlingQueue.Add "KickLockedBall 3","KickLockedBall 3",1,30,0,0,0,False
	BallHandlingQueue.Add "KickLockedBall 4","KickLockedBall 4",1,40,0,0,0,False
	BallHandlingQueue.Add "KickLockedBall 5","KickLockedBall 5",1,50,0,0,0,False
End Sub

'*******************************************
'	ZDRN: Drain, Trough, and Ball Release
'*******************************************
' It is best practice to never destroy balls. This leads to more stable and accurate pinball game simulations.
' The following code supports a "physical trough" where balls are not destroyed.
' To use this,
'   - The trough geometry needs to be modeled with walls, and a set of kickers needs to be added to
'	 the trough. The number of kickers depends on the number of physical balls on the table.
'   - A timer called "UpdateTroughTimer" needs to be added to the table. It should have an interval of 300 and be initially disabled.
'   - The balls need to be created within the Table1_Init sub. A global ball array (gBOT) can be created and used throughout the script


'TROUGH


'Sub UpdateTrough
'	UpdateTroughTimer.Interval = 500
'	UpdateTroughTimer.Enabled = 1
'End Sub

'Sub UpdateTroughTimer_Timer
'Debug.print " In Trough Timer"	'
'	bReleasingBalls = True
'	If TractorKicker1.BallCntOver = 1 Then TractorKicker1.kick 150, 2
'	If TractorKicker2.BallCntOver = 1 Then TractorKicker2.kick 150, 2
'	If TractorKicker3.BallCntOver = 1 Then TractorKicker3.kick 150, 2
'	If TractorKicker4.BallCntOver = 1 Then TractorKicker4.kick 150, 2
'	If TractorKicker5.BallCntOver = 1 Then TractorKicker5.kick 150, 2
'	'bReleasingBalls = True
'	Me.Enabled = 0
'End Sub

'***************************************************************
'* ZVAR - Round robin variations on videos
'***************************************************************
Dim oRoundRobinVariations
Dim oRoundRobinPlays
Dim oRoundRobinEvents
Dim oRoundRobinLengths

Sub InitRoundRobin
	Set oRoundRobinVariations = CreateObject("Scripting.Dictionary")
	Set oRoundRobinPlays = CreateObject("Scripting.Dictionary")
	Set oRoundRobinEvents = CreateObject("Scripting.Dictionary")
	Set oRoundRobinLengths = CreateObject("Scripting.Dictionary")

	oRoundRobinVariations.add "Drain", 12
	oRoundRobinPlays.add "Drain", 0
	oRoundRobinEvents.add "Drain", Array(310,311,312,313,314,315,316,317,318,319,320,321)
	oRoundRobinLengths.add "Drain", Array(8432,7877,7664,9840,7109,8539,8539,8603,9712,9605,10480,10501)


	oRoundRobinVariations.add "BallLocked", 19
	oRoundRobinPlays.add "BallLocked", 0
	oRoundRobinEvents.add "BallLocked", Array(420,421,422,423,424,425,426,427,428,429,430,431,432,433,434,435,436,437,438)
	oRoundRobinLengths.add "BallLocked", Array(4039,4039,4039,5872,4039,4039,8112,10139,8560,10139,8368,5296,6317,9920,7389,7784,10315,8899,7088)

	oRoundRobinVariations.add "BallSaved", 16
	oRoundRobinPlays.add "BallSaved", 0
	oRoundRobinEvents.add "BallSaved", Array(470,471,472,473,474,475,476,477,478,479,480,481,482,483,484,485)
	oRoundRobinLengths.add "BallSaved", Array(6995,4487,7389,9758,7784,7018,7643,7131,10736,10629,9093,11739,10736,8432,8713,7273)

	oRoundRobinVariations.add "ArtModeFail", 12
	oRoundRobinPlays.add "ArtModeFail", 0
	oRoundRobinEvents.add "ArtModeFail", Array(521,522,523,524,525,526,527,528,529,530,531,532)
	oRoundRobinLengths.add "ArtModeFail", Array(8667,8944,11314,7180,16259,8341,8876,8225,8736,6600,12226,7761)

	oRoundRobinVariations.add "ArtWizardMode", 2
	oRoundRobinPlays.add "ArtWizardMode", 0
	oRoundRobinEvents.add "ArtWizardMode", Array(450, 451)
	oRoundRobinLengths.add "ArtWizardMode", Array(270750, 189852 )

	oRoundRobinVariations.add "HandleTilt", 2
	oRoundRobinPlays.add "HandleTilt", 0
	oRoundRobinEvents.add "HandleTilt", Array(1052, 1053)
	oRoundRobinLengths.add "HandleTilt", Array(8197, 29332)

	oRoundRobinVariations.add "GameOver", 9
	oRoundRobinPlays.add "GameOver", 0
	oRoundRobinEvents.add "GameOver", Array(789, 790, 791, 792, 793, 794, 795, 796, 797)
	oRoundRobinLengths.add "GameOver", Array(10176, 16212, 15331, 12916, 14866, 49278, 13509, 9944, 13636)

	oRoundRobinVariations.add "SuperSkillshot", 4
	oRoundRobinPlays.add "SuperSkillshot", 0
	oRoundRobinEvents.add "SuperSkillshot", Array(645, 646, 647, 648)
	oRoundRobinLengths.add "SuperSkillshot", Array(13659, 6507, 7598, 23225)

	oRoundRobinVariations.add "Skillshot", 10
	oRoundRobinPlays.add "Skillshot", 0
	oRoundRobinEvents.add "Skillshot", Array(461, 462, 463, 464, 465, 466, 467, 468, 469, 460)
	oRoundRobinLengths.add "Skillshot", Array(6896, 8005, 6763, 3605, 9541, 8411, 8945, 10478, 7784, 6984)

	oRoundRobinVariations.add "SkillshotFail", 13
	oRoundRobinPlays.add "SkillshotFail", 0
	oRoundRobinEvents.add "SkillshotFail", Array(631, 632, 633, 634, 635, 636, 637, 638, 639, 640, 641, 642, 643)
	oRoundRobinLengths.add "SkillshotFail", Array(8496, 9413, 7493, 11099, 9944, 7227, 2653, 10989, 12080, 10989, 7854, 5276, 5996)

	oRoundRobinVariations.add "Intro", 6
	oRoundRobinPlays.add "Intro", 0
	oRoundRobinEvents.add "Intro", Array(660, 661, 662, 663, 664, 665)
	oRoundRobinLengths.add "Intro", Array(138907, 85230, 64879, 102747, 116208, 218784)

	oRoundRobinVariations.add "Multiball", 6
	oRoundRobinPlays.add "Multiball", 0
	oRoundRobinEvents.add "Multiball", Array(721, 722, 723, 724, 725, 726)
	oRoundRobinLengths.add "Multiball", Array(69186, 200928, 233134, 123118, 149983, 109952)

	oRoundRobinVariations.add "Attract", 3
	oRoundRobinPlays.add "Attract", 0
	oRoundRobinEvents.add "Attract", Array(1060, 1061, 1062)
	oRoundRobinLengths.add "Attract", Array(70757, 79580, 99410)

End Sub



Function RandomEvent(sEvent)
	Dim nIndex, nPlays, anEvents

	nIndex = IndexTmp
	anEvents = oRoundRobinEvents.Item(sEvent)
	RandomEvent = anEvents(nIndex)
End Function

Function RandomLength(sEvent)
	Dim nIndex, nPlays, anEvents

	nIndex = IndexTmp
	anEvents = oRoundRobinLengths.Item(sEvent)
	RandomLength = anEvents(nIndex)
End Function

Function RoundRobinEvent(sEvent)
	Dim nIndex, nPlays, anEvents

	nPlays = oRoundRobinPlays.Item(sEvent)
	oRoundRobinPlays.Item(sEvent) = nPlays + 1
	nIndex = nPlays Mod oRoundRobinVariations.Item(sEvent)
	anEvents = oRoundRobinEvents.Item(sEvent)
	RoundRobinEvent = anEvents(nIndex)
End Function

Function RoundRobinLength(sEvent)
	Dim nIndex, nPlays, anEvents

	nPlays = oRoundRobinPlays.Item(sEvent) - 1
	nIndex = nPlays Mod oRoundRobinVariations.Item(sEvent)
	anEvents = oRoundRobinLengths.Item(sEvent)
	RoundRobinLength = anEvents(nIndex)
End Function

'*******************************************
'  ZPUP - PUP Helpers
'*******************************************
'=============================================================================================
' Unique routines to make sure pup videos are not repeated until all videos in folder are seen
'=============================================================================================

Function rndZeroArrayCol1d(arrayIn)
    ' send in a 1d array (arrayIn) will return a random col for a column that has a value equal to "zero"
    Dim maxCol: maxCol = UBound(arrayIn) ' get the max number of cols in array 
    Randomize
    Dim col

' check if the array row as all columns with zero values
	Dim i
	dim allOnes: allOnes = True

	for i=0 to maxCol-1
		if arrayIn(i) = 0 Then
			allOnes = False   
		End If
	Next

	If allOnes then ' this array is set to all ones.  return -1
		rndZeroArrayCol1d = 999
		Exit Function 
	End If

	Do While 1
		col =  Int(maxCol*Rnd)
		If arrayIn(col) = 0 or allOnes Then
			Exit Do
		End If
	Loop

	rndZeroArrayCol1d = col ' return values
End Function
'------------------------------------------------
Sub FindChaseLight()
		IndexTmp = rndZeroArrayCol1d(nCottonShots)
	if IndexTmp <> 999 Then	
		nCottonShots(IndexTmp) = 1
		nNewCottonShot = IndexTmp
	Else
		ResetChaseTracker
	End If
End Sub

Sub ResetChaseTracker
Dim i
	For i = 0 to Ubound(nCottonShots)-1
		nCottonShots(i) = 0
	Next

End Sub

'------------------------------------------------
Sub FindDrain()
		IndexTmp = rndZeroArrayCol1d(DrainTracker)
	if IndexTmp <> 999 Then	
		DrainTracker(IndexTmp) = 1
	Else
		ResetDrainTracker
	End If
End Sub

Sub ResetDrainTracker
Dim i
	For i = 0 to Ubound(DrainTracker)-1
		DrainTracker(i) = 0
	Next

End Sub
'------------------------------------------------
Sub FindBallLocked()
		IndexTmp = rndZeroArrayCol1d(BallLockedTracker)
	if IndexTmp <> 999 Then	
		BallLockedTracker(IndexTmp) = 1
	Else
		ResetBallLockedTracker
	End If
End Sub

Sub ResetBallLockedTracker
Dim i
	For i = 0 to Ubound(BallLockedTracker)-1
		BallLockedTracker(i) = 0
	Next

End Sub
'------------------------------------------------
Sub FindBallSaved()
		IndexTmp = rndZeroArrayCol1d(BallSavedTracker)
	if IndexTmp <> 999 Then	
		BallSavedTracker(IndexTmp) = 1
	Else
		ResetBallSavedTracker
	End If
End Sub

Sub ResetBallSavedTracker
Dim i
	For i = 0 to Ubound(BallSavedTracker)-1
		BallSavedTracker(i) = 0
	Next

End Sub
'------------------------------------------------
Sub FindGameOver()
		IndexTmp = rndZeroArrayCol1d(GameOverTracker)
	if IndexTmp <> 999 Then	
		GameOverTracker(IndexTmp) = 1
	Else
		ResetGameOverTracker
	End If
End Sub

Sub ResetGameOverTracker
Dim i
	For i = 0 to Ubound(GameOverTracker)-1
		GameOverTracker(i) = 0
	Next

End Sub
'------------------------------------------------

'playevent  SNUM, playlist, filename, volume, priority, playtype, seconds, special""
'   PuPlayer.playevent pDMDFull,"RandomScoring","Fire Missile 1.mp4",100,1,0,0,""
'
'playevent allows you in table script to do almost everything you can like pupevent
'playtype is whats similar... maybe confusing for some new people, but pup-pack gurus will understand the following playtypes
'//playtype for triggers
'ptNormal=0;
'ptLoop=1;
'ptSplashReset=2;
'ptSplashResume=3;
'ptStopScreen=4;
'ptStopFile=5;
'ptSetBG=6;
'ptPlaySSF=7;
'ptSkipSameP=8;
'ptCustomFunc=9;
'ptForcePlay=10;
'ptQueueSameP=11;
'ptQueueAlways=12;

Sub PuPGameLaunch
Dim i

	if Puptype = 1 Then
		i = RndNbr(8)
	Else
		i = RndNbr(4)
	End If
	PuPlayer.playevent pDMDVideo,"GameLaunch","Launch"&i &".mp4",100,15,0,0,""
End Sub

Dim MB_Video
Sub PuPStartMultiball
Dim i

'	if Puptype = 1 Then
'		i = RndNbr(44)
'	Else
'		i = RndNbr(6)
'	End If
	Select Case Mode(CurrentPlayer,0)
		Case 4
			MB_Video = "MULTIBALL4.mp4"
		Case 6
			MB_Video = "MULTIBALL36.mp4"
		Case 10
			MB_Video = "MULTIBALL33.mp4"
		Case 11
			MB_Video = "MULTIBALL7.mp4"
		Case 12
			MB_Video = "MULTIBALL17.mp4"
		Case Else
			i = RndNbr(44)
			MB_Video = "MULTIBALL" & i & ".mp4"
	End Select
	

	PuPlayer.playevent pDMDVideo,"Multiball",MB_Video,100,40,0,0,""

End Sub

Sub StopMBVideo
	PuPlayer.playevent pDMDVideo,"Multiball",MB_Video,100,41,5,0,""
End Sub

Sub PupGameOver
	bGameReady = False
	PuPlayer.playevent pDMDVideo,"GameOver","GAMEOVER1.mp4",100,99,0,0,""
	DMDQueue.Add "bGameReady = True","bGameReady = True",95,6000,0,0,0,True
End Sub

Sub GameOverMesssages
	pDMDSplashTwoLines "GAME", "OVER", 4, cPurple
	DMDQueue.Add "ClearTwoLines","ClearTwoLines",95,4100,0,0,0,True
End Sub

Sub PuPBallLock
	Select Case nLockedBalls
		Case 1
			PuPlayer.playevent pDMDVideo,"BallLock","BALLLOCK1.mp4",100,45,0,0,""
		Case 2
			PuPlayer.playevent pDMDVideo,"BallLock","BALLLOCK2.mp4",100,45,0,0,""
		Case 3
			PuPlayer.playevent pDMDVideo,"BallLock","BALLLOCK4.mp4",100,45,0,0,""
		Case 4
			PuPlayer.playevent pDMDVideo,"BallLock","BALLLOCK3.mp4",100,45,0,0,""
	End Select
End Sub

Sub PuPBallSave
	PuPlayer.playevent pDMDVideo,"BallSave","BallSave"&nBallSaveCounter &".mp4",100,48,0,0,""
	nBallSaveCounter = nBallSaveCounter + 1
	if nBallSaveCounter > 4 Then nBallSaveCounter = 1
End Sub

sub PupStartBBQ
	Dim i
	i = RndNbr(4)
	PuPlayer.playevent pDMDVideo,"StartBBQ","BBQStart"&i &".mp4",100,30,0,0,""
End Sub

sub PupStartBeer
	Dim i
	i = RndNbr(2)
	PuPlayer.playevent pDMDVideo,"StartBeer","Beer"&i &".mp4",100,30,0,0,""
End Sub

sub PupStartBobby
exit sub
	Dim i
	if Puptype = 1 Then
		i = RndNbr(7)
	Else
		i = RndNbr(4)
	End If
	PuPlayer.playevent pDMDVideo,"StartBobby","BobbyStart"&i &".mp4",100,30,0,0,""
End Sub

sub PupStartCotton
Exit Sub
	Dim i
	i = RndNbr(0)
	PuPlayer.playevent pDMDVideo,"StartCotton","CottonStart"&i &".mp4",100,30,0,0,""
End Sub

sub PupStartBoom
	PuPlayer.playevent pDMDVideo,"Mode6","BoomStart.mp4",100,30,0,0,""
End Sub

sub PupStartDale
	Dim i
	i = RndNbr(4)
	PuPlayer.playevent pDMDVideo,"StartDale","DaleStart"&i &".mp4",100,30,0,0,""
End Sub

sub PupStartLadybird
	PuPlayer.playevent pDMDVideo,"Multiball","Multiball31.mp4",100,30,0,0,""
End Sub

'*********************
' ** PEGGY Modes *****
sub PupStartPeggy1
	PuPlayer.playevent pDMDVideo,"Mode1","Mode1Start.mp4",100,30,0,0,""
End Sub

sub PupProgressPeggy1
	Select case nPeggyMode1Progress(0)
		Case 2
			PuPlayer.playevent pDMDVideo,"Mode1","Mode1Progress1.mp4",100,35,0,0,""
		Case 4
			PuPlayer.playevent pDMDVideo,"Mode1","Mode1Progress2.mp4",100,35,0,0,""
		Case 6
			PuPlayer.playevent pDMDVideo,"Mode1","Mode1Progress3.mp4",100,35,0,0,""
		Case 8
			PuPlayer.playevent pDMDVideo,"Mode1","Mode1Progress4.mp4",100,35,0,0,""
	End Select
End Sub

sub PupFailPeggy1
	Dim i
	i = RndNbr(2)
	PuPlayer.playevent pDMDVideo,"Mode1","Mode1Fail"&i &".mp4",100,60,0,0,""
End Sub

sub PupWinPeggy1
	Dim i
	i = RndNbr(2)
	PuPlayer.playevent pDMDVideo,"Mode1","Mode1Win"&i &".mp4",100,60,0,0,""
End Sub

'##############################
sub PupStartPeggy2
	PuPlayer.playevent pDMDVideo,"Mode2","Mode2Start.mp4",100,30,0,0,""
End Sub

sub PupProgressPeggy2
	PuPlayer.playevent pDMDVideo,"Mode2","Mode2Progress.mp4",100,35,0,0,""
End Sub

sub PupFailPeggy2
	PuPlayer.playevent pDMDVideo,"Mode2","Mode2Fail.mp4",100,60,0,0,""
End Sub

sub PupWinPeggy2
	PuPlayer.playevent pDMDVideo,"Mode2","Mode2Win.mp4",100,60,0,0,""
End Sub

'##############################
sub PupStartPeggy3
	PuPlayer.playevent pDMDVideo,"Mode3","Mode3Start.mp4",100,30,0,0,""
End Sub

sub PupProgressPeggy3
	Dim i
	i = RndNbr(5)
	PuPlayer.playevent pDMDVideo,"Mode3","Mode3Progress"&i &".mp4",100,35,0,0,""
End Sub

sub PupFailPeggy3
	Dim i
	i = RndNbr(3)
	PuPlayer.playevent pDMDVideo,"Mode3","Mode3Fail"&i &".mp4",100,60,0,0,""
End Sub

sub PupWinPeggy3
	PuPlayer.playevent pDMDVideo,"Mode3","Mode3Win.mp4",100,60,0,0,""
End Sub

'##############################


Sub PuPDrain
	Dim i

	Select Case Mode(CurrentPlayer,0)
		Case 0
			if Puptype = 1 Then
				
				FindDrain
				if IndexTmp = 999 Then FindDrain
				i = IndexTmp
			Else
				i = RndNbr(10)
			End If
			PuPlayer.playevent pDMDVideo,"Drain","BL"&i &".mp4",100,85,0,0,""
		Case 1
			PupFailPeggy1
		Case 2
			PupFailPeggy1
		Case 3
			PupFailPeggy3
		Case Else
			if Puptype = 1 Then
				
				FindDrain
				if IndexTmp = 999 Then FindDrain
				i = IndexTmp
			Else
				i = RndNbr(10)
			End If
			PuPlayer.playevent pDMDVideo,"Drain","BL"&i &".mp4",100,85,0,0,""
	End Select
End Sub

sub PupFailBobby
	Dim i
	if Puptype = 1 Then
		i = RndNbr(9)
	Else
		i = RndNbr(4)
	End If
	PuPlayer.playevent pDMDVideo,"FailBobby","BobbyFail"&i &".mp4",100,60,0,0,""
End Sub

sub PupFailBill
	Dim i
	i = RndNbr(4)
	PuPlayer.playevent pDMDVideo,"FailBill","BillFail"&i &".mp4",100,60,0,0,""
End Sub

sub PupFailBoom
	Dim i
	i = RndNbr(2)
	PuPlayer.playevent pDMDVideo,"FailBoom","BoomFail"&i &".mp4",100,60,0,0,""
End Sub

sub PupFailPeggy
	Dim i
	i = RndNbr(4)
	PuPlayer.playevent pDMDVideo,"FailPeggy","PeggyFail"&i &".mp4",100,60,0,0,""
End Sub

sub PupFailLuanne
	Dim i
	i = RndNbr(2)
	PuPlayer.playevent pDMDVideo,"FailLuanne","LuanneFail"&i &".mp4",100,60,0,0,""
End Sub

sub PupFailLadybird
	Dim i
	i = RndNbr(1)
	PuPlayer.playevent pDMDVideo,"FailLadybird","Ladybirdfail"&i &".mp4",100,60,0,0,""
End Sub

sub PupFailHank
	Dim i
	i = RndNbr(6)
	PuPlayer.playevent pDMDVideo,"FailHank","HankFail"&i &".mp4",100,60,0,0,""
End Sub




'*******************************************
'  ZQUE - Event queue
'*******************************************

Class EventItem
	Public priority
	Public maxQueueLength
	Public text
	Public duration
	Public soundName
	Public soundVolume
	Public pupTrigger
	Public lightshow
	Public holdBallAt
	Public interruptOngoing
	Public special
	Public Enabled
	Public withMode

	Private Sub Class_Initialize
		priority = 0
		maxQueueLength = Empty
		text = ""
		duration = 0
		soundName = ""
		soundVolume = 0
		pupTrigger = 0
		lightshow = 0
		holdBallAt = 0
		interruptOngoing = False
		special = ""
		Enabled = True
		withMode = 0
	End Sub
End Class

' priority queue for events
Class EventQueue
	private maQueue()
	private mnSize
	private mnCapacity

	private sub Class_Initialize
		mnCapacity = 10
		mnSize = 0
		redim maQueue(mnCapacity)
	end Sub

	public property get Size
		Size = mnSize
	end property

	public Sub add(element)
		Dim i, nInsertPoint
		Dim nValidItems

'		If "modeselect" = element.special Then
'			element.duration = 999999999
'			element.priority = ePrioModeSelect
'		End If
		' Check if queue length exceeds max for the event
		If (Not IsEmpty(element.maxQueueLength)) Then
			If mnSize > element.maxQueueLength Then Exit Sub
		End If
		' Grow capacity if needed
		If mnSize >= mnCapacity Then
			mnCapacity = mnCapacity * 2
			Redim preserve maQueue(mnCapacity)
		End If
		' Handle event immediately if the event should interrupt ongoing events
		If element.interruptOngoing Then
			If oCurrentEvent is Nothing Then
				' TODO: interrupt audio / video / lightshows
				' HandleQueueEvent element
				' Exit Sub
			Elseif element.priority <= oCurrentEvent.priority Then
				' TODO: interrupt audio / video
				'If bEnablePuP Then
				'	PuPlayer.playstop pDMD
				'End If
				Set oCurrentEvent = Nothing
				nTextDuration = 0
				nTextPriority = ePrioIdle

			' RTP commented 19A
'				if bEnablePuP Then
'					For Each i in Array("Splash1", "Splash2A", "Splash2B", _
'										"Splash3A", "Splash3B", "Splash3C")
'						pDMDLabelSet i, ""
'						'debug5 "displaying splash " &GameTime
'					Next
'				End If
				' interrupt light shows
				'''LightShowEnd
				'HandleQueueEvent element
				'Exit Sub
			End If
		End If
		' Special case if the queue is empty
		if 0 = mnSize Then
'			If oCurrentEvent is Nothing Then
				' Handle event immediately if no event is currently ongoing
'				HandleQueueEvent element
'			Else
				Set maQueue(0) = element
				mnSize = mnSize + 1
'			End If
		Else
			' Find the first item less prioritized than the item to add
			nInsertPoint = mnSize
			For i = 0 to mnSize - 1
				If maQueue(i).priority > element.priority Then
					nInsertPoint = i
					Exit For
				End If
			Next
			'copy everything past the insert point to the next index
			If nInsertPoint < mnSize Then
				For i = mnSize - 1 to nInsertPoint step -1
					Set maQueue(i + 1) = maQueue(i)
				Next
			End If

			'overwrite the item at the insert point
			Set maQueue(nInsertPoint) = element
			mnSize = mnSize + 1

			' delete items where the new queue size exceeds the item's max
			nValidItems = 0
			For i = 0 to mnSize - 1
				If IsEmpty(maQueue(i).maxQueueLength) Then
					Set maQueue(nValidItems) = maQueue(i)
					nValidItems = nValidItems + 1
				Elseif maQueue(i).maxQueueLength <= mnSize Then
					Set maQueue(nValidItems) = maQueue(i)
					nValidItems = nValidItems + 1
				End If
			Next

			If 0 = nValidItems Then
				mnCapacity = 10
				mnSize = 0
				redim maQueue(mnCapacity)
			Else
				mnSize = nValidItems
			End If
		End If
	end Sub

	public Function deQueue()
		Dim i

		If 0 = mnSize Then
			set deQueue = null
		Else
			set deQueue = maQueue(0)
			'copy everything past the first item to the previous index
			If mnSize > 1 Then
				For i = 1 to mnSize - 1
					Set maQueue(i - 1) = maQueue(i)
				Next
			End If
			mnSize = mnSize - 1
			' shrink the queue capacity if it has grown and we removed the last item
			if mnSize < 1 and mnCapacity > 10 Then
				mnCapacity = 10
				mnSize = 0
				redim maQueue(mnCapacity)
			End If
		End If
	End Function

	' Return the first item without removing it
	public function Peek()
		If 0 = mnSize Then
			set Peek = null
		Else
			set Peek = maQueue(0)
		End If
	end Function

	public sub Clear()
		mnCapacity = 10
		mnSize = 0
		redim maQueue(mnCapacity)
	end Sub
End Class

Sub HandleQueueEvent
	Dim i, bHandleMore, bSkipEvent

	If GameTime - nTimeLastPuP < 200 Then
		Set oCurrentEvent = Nothing
		Exit Sub
	End If
	Set oCurrentEvent = oEventQueue.deQueue
	bHandleMore = True

' "Entering Event While " &pDMDCurPage
'Debug3 " In Handle Event Queue: " &oCurrentEvent.PupTrigger

	While bHandleMore
			bSkipEvent = False

' Commented RTP 19A
'			If oCurrentEvent.withMode <> eModeNone Then
'				If Not avModesRunning(nPlayer).Contains(oCurrentEvent.withMode) Then
'					'RTP
'					debug3 "**** SKIPPING EVENT ***"
'					bSkipEvent = True
'				End If
'			End If
			If oCurrentEvent.duration > 0 And (not bSkipEvent) Then
'				If IsArray(oCurrentEvent.text) Then
'					If ePrioModeProgress = oCurrentEvent.priority then
'						nTimeModeTextDisplay = GameTime
'						DMDTimedText asModeMessage, oCurrentEvent.duration, oCurrentEvent.priority, oCurrentEvent.pupTrigger	'RTP27
'						Debug5 "EpriorModeProgress = Curr Priority"
'					Else
'						DMDTimedText oCurrentEvent.text, oCurrentEvent.duration, oCurrentEvent.priority, oCurrentEvent.pupTrigger
'						Debug5 "NOT   -- EpriorModeProgress = Curr Priority"
'					End If
'				ElseIf oCurrentEvent.text <> "" Then
'					DMDTimedText oCurrentEvent.text, oCurrentEvent.duration, oCurrentEvent.priority, oCurrentEvent.pupTrigger
'						Debug5 "NULL TEXT ===   EpriorModeProgress"
'				Else
'						Debug5 "Text Time == 0"
'					nDMDTextDisplayTime = 0
'				End If
				bHandleMore = False
				' lightshows
'				If oCurrentEvent.lightshow <> eLightShowNone Then
'					LightShowStart oCurrentEvent
'				End If
				' video 
				If (oCurrentEvent.pupTrigger <> 0) And bEnablePuP Then
					' Do not play any video other than Bender multiball
					' if Bender multiball is starting or running
					nTimePupStartPlay = GameTime
					PuPEvent oCurrentEvent.pupTrigger
					'Debug3 "Calling pTrigger: " &oCurrentEvent.pupTrigger
				End If
				' audio
'				DuckMusic
			Else
			' If an event has duration 0, handle the next event immediately after
				If oEventQueue.Size > 0 Then
					Set oCurrentEvent = oEventQueue.deQueue
					bHandleMore = True
				Else
					Set oCurrentEvent = Nothing
					bHandleMore = False
'					sDMDScene = "Scoreboard" ' RTP27
				End If
			End If
	Wend
'Debug2 "Leaving Event While: "&pDMDCurPage

End Sub

Sub ClearEvents
'	debug5 "Calling Clear Events"
	' interrupt videos
	If bEnablePuP and (Not oCurrentEvent is Nothing) Then
		PuPlayer.playstop pDMD
	End If
	Set oCurrentEvent = Nothing
	nTextDuration = 0
	nTextPriority = ePrioIdle
	oEventQueue.Clear
	nTimeLastPuP = GameTime
	' interrupt light shows
'	LightShowEnd
End Sub

Dim VRRoom
DIM VRThings
Sub LoadVRRoom

	for each VRThings in VR_Cab:VRThings.visible = 0:Next
	for each VRThings in VR_Min:VRThings.visible = 0:Next
	for each VRThings in VR_Mega:VRThings.visible = 0:Next

	TimerPlunger.Enabled = True

	If RenderingMode = 2 or VRTest Then
		VRRoom = VRRoomChoice
		ramp15.Visible = 0
		ramp16.Visible = 0
		ramp17.Visible = 0
	Else
		VRRoom = 0
		TimerPlunger.Enabled = False
	End If

	If VRRoom = 1 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		for each VRThings in VR_Mega:VRThings.visible = 0:Next

	End If
	If VRRoom = 2 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 1:Next
		for each VRThings in VR_Mega:VRThings.visible = 0:Next
	End If
	If VRRoom = 3 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		for each VRThings in VR_Mega:VRThings.visible = 1:Next
	End If


End Sub


Sub Overhead_GIon
	Overhead_high.state = 1
	overhead_Low.state = 1
	trough_L.state = 1
End Sub

Sub Overhead_GIoff
	Overhead_high.state = 0
	overhead_Low.state = 0
	trough_L.state = 0
End Sub



'**************************
'   SCORBIT
'**************************
'==================================================================================================================
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  SCORBIT Interface
' To Use:
' 1) Define a timer tmrScorbit
' 2) Call DoInit at the end of PupInit or in Table Init if you are nto using pup with the appropriate parameters
'     Replace 389 with your TableID from Scorbit 
'     Replace GRWvz-MP37P from your table on OPDB - eg: https://opdb.org/machines/2103
'		if Scorbit.DoInit(389, "PupOverlays", "1.0.0", "GRWvz-MP37P") then 
'			tmrScorbit.Interval=2000
'			tmrScorbit.UserValue = 0
'			tmrScorbit.Enabled=True 
'		End if 
' 3) Customize helper functions below for different events if you want or make your own 
' 4) Call 
'		DoInit - After Pup/Screen is setup (PuPInit)
'		StartSession - When a game starts (ResetForNewGame)
'		StopSession - When the game is over (Table1_Exit, EndOfGame)
'		SendUpdate - called when Score Changes (AddScore)
'			SendUpdate(P1Score, P2Score, P3Score, P4Score, CurrentBall, CurrentPlayer, NumberPlayers)
'			Example:  Scorbit.SendUpdate Score(0), Score(1), Score(2), Score(3), Balls, CurrentPlayer+1, PlayersPlayingGame
'		SetGameMode - When different game events happen like starting a mode, MB etc.  (ScorbitBuildGameModes helper function shows you how)
' 5) Drop the binaries sQRCode.exe and sToken.exe in your Pup Root so we can create session tokens and QRCodes.
'	- Drop QRCode Images (QRCodeS.png, QRcodeB.png) in yur pup PuPOverlays if you want to use those 
' 6) Callbacks 
'		Scorbit_Paired   	- Called when machine is successfully paired.  Hide QRCode and play a sound 
'		Scorbit_PlayerClaimed	- Called when player is claimed.  Hide QRCode, play a sound and display name 
'		ScorbitClaimQR		- Call before/after plunge (swPlungerRest_Hit, swPlungerRest_UnHit)
' 7) Other 
'		Set Pair QR Code	- During Attract
'			if (Scorbit.bNeedsPairing) then 
'				PuPlayer.LabelSet pDMDFull, "ScorbitQR_a", "PuPOverlays\\QRcode.png",1,"{'mt':2,'width':32, 'height':64,'xalign':0,'yalign':0,'ypos':5,'xpos':5}"
'				PuPlayer.LabelSet pDMDFull, "ScorbitQRIcon_a", "PuPOverlays\\QRcodeS.png",1,"{'mt':2,'width':36, 'height':85,'xalign':0,'yalign':0,'ypos':3,'xpos':3,'zback':1}"
'			End if 
'		Set Player Names 	- Wherever it makes sense but I do it here: (pPupdateScores)
'		   if ScorbitActive then 
'			if Scorbit.bSessionActive then
'				PlayerName=Scorbit.GetName(CurrentPlayer+1)
'				if PlayerName="" then PlayerName= "Player " & CurrentPlayer+1 
'			End if 
'		   End if 
'
'
'>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
' TABLE CUSTOMIZATION START HERE 

Sub Scorbit_Paired()								' Scorbit callback when new machine is paired 
dbg2 "Scorbit PAIRED"
	PlaySound "scorbit_login"


	ResetBackglass
	GeneralPupQueue.Add "ResetBackglass","ResetBackglass",25,100,0,0,0,False

	
	pbackglasslabelhide "ScorbitQR1"
End Sub 


Sub Scorbit_PlayerClaimed(PlayerNum, PlayerName)	' Scorbit callback when QR Is Claimed 
dbg2 "Scorbit LOGIN"
	PlaySound "scorbit_login"
	ScorbitClaimQR(False)
End Sub 


Sub ScorbitClaimQR(bShow)	
dbg2 "In ScorbitClaimQR: " &bShow					'  Show QRCode on first ball for users to claim this position
dbg2 "Session Active: " &Scorbit.bSessionActive
dbg2 "bNeedsPairing:" &Scorbit.bNeedsPairing
	if Scorbit.bSessionActive=False then Exit Sub 
	if ScorbitShowClaimQR=False then Exit Sub
	if Scorbit.bNeedsPairing then exit sub 


'dbg2 "BShow: " & bShow
'dbg2 "nBall: " &balls
'dbg2 "bGameInPlay: " & bGameStarted
'dbg2 "GetName: " &Scorbit.GetName(nPlayer)

	if bShow and bOnTheFirstBallScorbit and Scorbit.GetName(CurrentPlayer)="" then 
		if DMDType = 0 Then pdmdsetpage 77
		PuPlayer.playlistplayex pBackglass,"PuPOverlays","Scorbit_Claim.png",0,1
		PuPlayer.LabelSet pBackglass, "ScorbitQR2", "PuPOverlays\\QRclaim.png",1,"{'mt':2,'width':19.61, 'height':36,'xalign':0,'yalign':0,'ypos':32,'xpos':74.6}"
	Else 
		dbg2 "Hiding QR claim & Overlay"
		HideScorbit
	End if 
End Sub 

Sub StopScorbit
	Scorbit.StopSession Score(1), Score(2), Score(3), Score(4), PlayersPlayingGame   ' Stop updateing scores
End Sub


Sub ScorbitBuildGameModes()		' Custom function to build the game modes for better stats 
	dim GameModeStr
	if Scorbit.bSessionActive=False then Exit Sub 
	Dbg2 " Shoulsub sofd be adding Game String:" &GameModeStr
	Scorbit.SetGameMode(GameModeStr)
End Sub 






' END ----------

Sub Scorbit_LOGUpload(state)	' Callback during the log creation process.  0=Creating Log, 1=Uploading Log, 2=Done 
	Select Case state 
		case 0:
			dbg "CREATING LOG"
		case 1:
			dbg "Uploading LOG"
		case 2:
			dbg "LOG Complete"
	End Select 
End Sub 
'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
' TABLE CUSTOMIZATION END HERE - NO NEED TO EDIT BELOW THIS LINE



dim Scorbit : Set Scorbit = New ScorbitIF
' Workaround - Call get a reference to Member Function
Sub tmrScorbit_Timer()								' Timer to send heartbeat 
	Scorbit.DoTimer(tmrScorbit.UserValue)
	tmrScorbit.UserValue=tmrScorbit.UserValue+1
	if tmrScorbit.UserValue>5 then tmrScorbit.UserValue=0
End Sub 
Function ScorbitIF_Callback()
	Scorbit.Callback()
End Function 
Class ScorbitIF

	Public bSessionActive
	Public bNeedsPairing
	Private bUploadLog
	Private bActive
	Private LOGFILE(10000000)
	Private LogIdx

	Private bProduction

	Private TypeLib
	Private MyMac
	Private Serial
	Private MyUUID
	Private TableVersion

	Private SessionUUID
	Private SessionSeq
	Private SessionTimeStart
	Private bRunAsynch
	Private bWaitResp
	Private GameMode
	Private GameModeOrig		' Non escaped version for log
	Private VenueMachineID
	Private CachedPlayerNames(4)
	Private SaveCurrentPlayer

	Public bEnabled
	Private sToken
	Private machineID
	Private dirQRCode
	Private opdbID
	Private wsh

	Private objXmlHttpMain
	Private objXmlHttpMainAsync
	Private fso
	Private Domain

	Public Sub Class_Initialize()
		bActive="false"
		bSessionActive=False
		bEnabled=False 
	End Sub 

	Property Let UploadLog(bValue)
		bUploadLog = bValue
	End Property

	Sub DoTimer(bInterval)	' 2 second interval
		dim holdScores(4)
		dim i
		if bInterval=0 then 
			SendHeartbeat()
		elseif bRunAsynch And bSessionActive = True then ' Game in play (Updated for TNA to resolve stutter in CoopMode)
			Scorbit.SendUpdate Score(1), Score(2), Score(3), Score(4), balls, CurrentPlayer, PlayersPlayingGame
		End if 
	End Sub 

	Function GetName(PlayerNum)	' Return Parsed Players name  
		if PlayerNum<1 or PlayerNum>4 then 
			GetName=""
		else 
			GetName=CachedPlayerNames(PlayerNum-1)
		End if 
	End Function 

	Function DoInit(MyMachineID, Directory_PupQRCode, Version, opdb)
		dim Nad
		Dim EndPoint
		Dim resultStr 
		Dim UUIDParts 
		Dim UUIDFile

		bProduction=1
'		bProduction=0
		SaveCurrentPlayer=0
		VenueMachineID=""
		bWaitResp=False 
		bRunAsynch=False 
		DoInit=False 
		opdbID=opdb
		dirQrCode=Directory_PupQRCode
		MachineID=MyMachineID
		TableVersion=version
		bNeedsPairing=False
		if bProduction then 
			domain = "api.scorbit.io"
		else 
			domain = "staging.scorbit.io"
			domain = "scorbit-api-staging.herokuapp.com"
		End if 
		Set fso = CreateObject("Scripting.FileSystemObject")
		dim objLocator:Set objLocator = CreateObject("WbemScripting.SWbemLocator")
		Dim objService:Set objService = objLocator.ConnectServer(".", "root\cimv2")
		Set objXmlHttpMain = CreateObject("Msxml2.ServerXMLHTTP")
		Set objXmlHttpMainAsync = CreateObject("Microsoft.XMLHTTP")
		objXmlHttpMain.onreadystatechange = GetRef("ScorbitIF_Callback")
		Set wsh = CreateObject("WScript.Shell")

		' Get Mac for Serial Number 
		dim Nads: set Nads = objService.ExecQuery("Select * from Win32_NetworkAdapter where physicaladapter=true")
		for each Nad in Nads
			if not isnull(Nad.MACAddress) then
				if left(Nad.MACAddress, 6)<>"00090F" then ' Skip over forticlient MAC
dbg2 "Using MAC Addresses:" & Nad.MACAddress & " From Adapter:" & Nad.description   
					MyMac=replace(Nad.MACAddress, ":", "")
					Exit For 
				End if 
			End if 
		Next
		Serial=eval("&H" & mid(MyMac, 5))
		if Serial<0 then Serial=eval("&H" & mid(MyMac, 6))		' Mac Address Overflow Special Case 
		if MyMachineID<>2108 then 			' GOTG did it wrong but MachineID should be added to serial number also
			Serial=Serial+MyMachineID
		End if 
'		Serial=123456
		dbg2 "Serial:" & Serial

		' Get System UUID
		set Nads = objService.ExecQuery("SELECT * FROM Win32_ComputerSystemProduct")
		for each Nad in Nads
			dbg2 "Using UUID:" & Nad.UUID   
			MyUUID=Nad.UUID
			Exit For 
		Next

		if MyUUID="" then 
			MsgBox "SCORBIT - Can get UUID, Disabling."
			Exit Function
		elseif MyUUID="03000200-0400-0500-0006-000700080009" or ScorbitAlternateUUID then
			If fso.FolderExists(UserDirectory) then 
				If fso.FileExists(UserDirectory & "ScorbitUUID.dat") then
					Set UUIDFile = fso.OpenTextFile(UserDirectory & "ScorbitUUID.dat",1)
					MyUUID = UUIDFile.ReadLine()
					UUIDFile.Close
					Set UUIDFile = Nothing
				Else 
					MyUUID=GUID()
					Set UUIDFile=fso.CreateTextFile(UserDirectory & "ScorbitUUID.dat",True)
					UUIDFile.WriteLine MyUUID
					UUIDFile.Close
					Set UUIDFile=Nothing
				End if
			End if 
		End if

		' Clean UUID
		UUIDParts=split(MyUUID, "-")
		MyUUID=LCASE(Hex(eval("&h" & UUIDParts(0))+MyMachineID) & UUIDParts(1) &  UUIDParts(2) &  UUIDParts(3) & UUIDParts(4))		 ' Add MachineID to UUID
		MyUUID=LPad(MyUUID, 32, "0")
'		MyUUID=Replace(MyUUID, "-",  "")
		dbg2 "MyUUID:" & MyUUID 


		' Authenticate and get our token 
		if getStoken() then 
			bEnabled=True 
'			SendHeartbeat
			DoInit=True
		End if 
	End Function 

	Sub Callback()
		Dim ResponseStr
		Dim i 
		Dim Parts
		Dim Parts2
		Dim Parts3
		if bEnabled=False then Exit Sub 

		if bWaitResp and objXmlHttpMain.readystate=4 then 
'			dbg2 "CALLBACK: " & objXmlHttpMain.Status & " " & objXmlHttpMain.readystate
			if objXmlHttpMain.Status=200 and objXmlHttpMain.readystate = 4 then 
				ResponseStr=objXmlHttpMain.responseText
				'debug3 "RESPONSE: " & ResponseStr

				' Parse Name 
				If bSessionActive = True Then
					if CachedPlayerNames(SaveCurrentPlayer-1)="" then  ' Player doesnt have a name
						if instr(1, ResponseStr, "cached_display_name") <> 0 Then	' There are names in the result
							Parts=Split(ResponseStr,",{")							' split it 
							if ubound(Parts)>=SaveCurrentPlayer-1 then 				' Make sure they are enough avail
								if instr(1, Parts(SaveCurrentPlayer-1), "cached_display_name")<>0 then 	' See if mine has a name 
									CachedPlayerNames(SaveCurrentPlayer-1)=GetJSONValue(Parts(SaveCurrentPlayer-1), "cached_display_name")		' Get my name
									CachedPlayerNames(SaveCurrentPlayer-1)=Replace(CachedPlayerNames(SaveCurrentPlayer-1), """", "")
									Scorbit_PlayerClaimed SaveCurrentPlayer, CachedPlayerNames(SaveCurrentPlayer-1)
	'								dbg2 "Player Claim:" & SaveCurrentPlayer & " " & CachedPlayerNames(SaveCurrentPlayer-1)
								End if 
							End if
						End if 
					else												    ' Check for unclaim 
						if instr(1, ResponseStr, """player"":null")<>0 Then	' Someone doesnt have a name
							Parts=Split(ResponseStr,"[")						' split it 
	'dbg2 "Parts:" & Parts(1)
							Parts2=Split(Parts(1),"}")							' split it 
							for i = 0 to Ubound(Parts2)
	'dbg2 "Parts2:" & Parts2(i)
								if instr(1, Parts2(i), """player"":null")<>0 Then
									CachedPlayerNames(i)=""
								End if 
							Next 
						End if 
					End if
				End If

				'Check heartbeat
				HandleHeartbeatResp ResponseStr
			End if 
			bWaitResp=False
		End if 
	End Sub

	Public Sub StartSession()
		if bEnabled=False then Exit Sub 
		dbg2 "Scorbit Start Session" 
		CachedPlayerNames(0)=""
		CachedPlayerNames(1)=""
		CachedPlayerNames(2)=""
		CachedPlayerNames(3)=""
		bRunAsynch=True 
		bActive="true"
		bSessionActive=True
		SessionSeq=0
		SessionUUID=GUID()
		SessionTimeStart=GameTime
		LogIdx=0
		SendUpdate 0, 0, 0, 0, 1, 1, 1
	End Sub

	' Custom method for TNA to work around coop mode stuttering
	Public Sub ForceAsynch(enabled)
		if bEnabled=False then Exit Sub
		if bSessionActive=True then Exit Sub 'Sessions should always control asynch when active
		bRunAsynch=enabled
	End Sub

	Public Sub StopSession(P1Score, P2Score, P3Score, P4Score, NumberPlayers)
		StopSession2 P1Score, P2Score, P3Score, P4Score, NumberPlayers, False
	End Sub 

	Public Sub StopSession2(P1Score, P2Score, P3Score, P4Score, NumberPlayers, bCancel)
		Dim i
		dim objFile
		if bEnabled=False then Exit Sub 
		bRunAsynch=False 'Asynch might have been forced on in TNA to prevent coop mode stutter
		if bSessionActive=False then Exit Sub 
dbg2 "Scorbit Stop Session" 

		bActive="false" 
		SendUpdate P1Score, P2Score, P3Score, P4Score, -1, -1, NumberPlayers
		bSessionActive=False
'		SendHeartbeat

		if bUploadLog and LogIdx<>0 and bCancel=False then 
			dbg2 "Creating Scorbit Log: Size" & LogIdx
			Scorbit_LOGUpload(0)
			Set objFile = fso.CreateTextFile(puplayer.getroot&"\" & cGameName & "\sGameLog.csv")
			For i = 0 to LogIdx-1 
				objFile.Writeline LOGFILE(i)
			Next 
			objFile.Close
			LogIdx=0
			Scorbit_LOGUpload(1)
			pvPostFile "https://" & domain & "/api/session_log/", puplayer.getroot&"\" & cGameName & "\sGameLog.csv", False
			Scorbit_LOGUpload(2)
			on error resume next
			fso.DeleteFile(puplayer.getroot&"\" & cGameName & "\sGameLog.csv")
			on error goto 0
		End if 

	End Sub 

	Public Sub SetGameMode(GameModeStr)
		GameModeOrig=GameModeStr
		GameMode=GameModeStr
		GameMode=Replace(GameMode, ":", "%3a")
		GameMode=Replace(GameMode, ";", "%3b")
		GameMode=Replace(GameMode, " ", "%20")
		GameMode=Replace(GameMode, "{", "%7B")
		GameMode=Replace(GameMode, "}", "%7D")
	End sub 

	Public Sub SendUpdate(P1Score, P2Score, P3Score, P4Score, CurrentBall, nPlayer, NumberPlayers)
		SendUpdateAsynch P1Score, P2Score, P3Score, P4Score, CurrentBall, nPlayer, NumberPlayers, bRunAsynch
	End Sub 

	Public Sub SendUpdateAsynch(P1Score, P2Score, P3Score, P4Score, CurrentBall, nPlayer, NumberPlayers, bAsynch)
		dim i
		Dim PostData
		Dim resultStr
		dim LogScores(4)

		if bUploadLog then 
			if NumberPlayers>=1 then LogScores(0)=P1Score
			if NumberPlayers>=2 then LogScores(1)=P2Score
			if NumberPlayers>=3 then LogScores(2)=P3Score
			if NumberPlayers>=4 then LogScores(3)=P4Score
			LOGFILE(LogIdx)=DateDiff("S", "1/1/1970", Now()) & "," & LogScores(0) & "," & LogScores(1) & "," & LogScores(2) & "," & LogScores(3) & ",,," &  nPlayer & "," & CurrentBall & ",""" & GameModeOrig & """"
			LogIdx=LogIdx+1
		End if


		if bSessionActive=False then Exit Sub 

		if bEnabled=False then Exit Sub 

		if bWaitResp then exit sub ' Drop message until we get our next response 

		SaveCurrentPlayer=nPlayer
		PostData = "session_uuid=" & SessionUUID & "&session_time=" & GameTime-SessionTimeStart+1 & _
					"&session_sequence=" & SessionSeq & "&active=" & bActive

		SessionSeq=SessionSeq+1
		if NumberPlayers > 0 then 
			for i = 0 to NumberPlayers-1
				PostData = PostData & "&current_p" & i+1 & "_score="
				if i <= NumberPlayers-1 then 
					if i = 0 then PostData = PostData & P1Score
					if i = 1 then PostData = PostData & P2Score
					if i = 2 then PostData = PostData & P3Score
					if i = 3 then PostData = PostData & P4Score
				else 
					PostData = PostData & "-1"
				End if 
			Next 
'Dbg2 "Score:" &P1Score &" XXX"
			PostData = PostData & "&current_ball=" & CurrentBall & "&current_player=" & nPlayer
			if GameMode<>"" then PostData=PostData & "&game_modes=" & GameMode

		End if 
		resultStr = PostMsg("https://" & domain, "/api/entry/", PostData, bAsynch)
		'if resultStr<>"" then debug3 "SendUpdate Resp:" & resultStr    			'rtp12
	End Sub 

' PRIVATE BELOW 
	Private Function LPad(StringToPad, Length, CharacterToPad)
	  Dim x : x = 0
	  If Length > Len(StringToPad) Then x = Length - len(StringToPad)
	  LPad = String(x, CharacterToPad) & StringToPad
	End Function

	Private Function GUID()		
		Dim TypeLib
		Set TypeLib = CreateObject("Scriptlet.TypeLib")
		GUID = Mid(TypeLib.Guid, 2, 36)
	End Function

	Private Function GetJSONValue(JSONStr, key)
		dim i 
		Dim tmpStrs,tmpStrs2
		if Instr(1, JSONStr, key)<>0 then 
			tmpStrs=split(JSONStr,",")
			for i = 0 to ubound(tmpStrs)
				if instr(1, tmpStrs(i), key)<>0 then 
					tmpStrs2=split(tmpStrs(i),":")
					GetJSONValue=tmpStrs2(1)
					exit for
				End if 
			Next 
		End if 
	End Function

	Private Sub SendHeartbeat()
		Dim resultStr
		if bEnabled=False then Exit Sub 
		resultStr = GetMsgHdr("https://" & domain, "/api/heartbeat/", "Authorization", "SToken " & sToken)
		
		'Customized for TNA
		If bRunAsynch = False Then 
			dbg2 "Heartbeat Resp:" & resultStr
			HandleHeartbeatResp ResultStr
		End If
	End Sub 

	'TNA custom method
	Private Sub HandleHeartbeatResp(resultStr)
		dim TmpStr
		Dim Command
		Dim rc
		'Dim QRFile:QRFile=puplayer.getroot&"\" & cGameName & "\" & dirQrCode
		Dim QRFile:QRFile=puplayer.getroot & cGameName & "\" & dirQrCode
'dbg2 "QRFile: " &QRFile
		If VenueMachineID="" then
			If resultStr<>"" And Not InStr(resultStr, """machine_id"":" & machineID)=0 Then 'We Paired
				bNeedsPairing=False
				dbg2 "Scorbit: Paired"
				Scorbit_Paired()
			ElseIf resultStr<>"" And Not InStr(resultStr, """unpaired"":true")=0 Then 'We Did not Pair
				dbg2 "Scorbit: NOT Paired"
				bNeedsPairing=True
			Else
				' Error (or not a heartbeat); do nothing
			End If

			TmpStr=GetJSONValue(resultStr, "venuemachine_id")
			if TmpStr<>"" then 
				VenueMachineID=TmpStr
'dbg2 "VenueMachineID=" & VenueMachineID			
				'Command = """" & puplayer.getroot&"\" & cGameName & "\sQRCode.exe"" " & VenueMachineID & " " & opdbID & " """ & QRFile & """"
				Command = """" & puplayer.getroot & cGameName & "\sQRCode.exe"" " & VenueMachineID & " " & opdbID & " """ & QRFile & """"
				rc = wsh.Run(Command, 0, False)
			End if 
		End if
	End Sub

	Private Function getStoken()
		Dim result
		Dim results
'		dim wsh
		Dim tmpUUID:tmpUUID="adc12b19a3504453a7414e722f58736b"
		Dim tmpVendor:tmpVendor="vscorbitron"
		Dim tmpSerial:tmpSerial="999990104"
		'Dim QRFile:QRFile=puplayer.getroot&"\" & cGameName & "\" & dirQrCode
		Dim QRFile:QRFile=puplayer.getroot & cGameName & "\" & dirQrCode
		'Dim sTokenFile:sTokenFile=puplayer.getroot&"\" & cGameName & "\sToken.dat"
		Dim sTokenFile:sTokenFile=puplayer.getroot & cGameName & "\sToken.dat"

		' Set everything up
		tmpUUID=MyUUID
		tmpVendor="vpin"
		tmpSerial=Serial
		
		on error resume next
		fso.DeleteFile(sTokenFile)
		On error goto 0 

		' get sToken and generate QRCode
'		Set wsh = CreateObject("WScript.Shell")
		Dim waitOnReturn: waitOnReturn = True
		Dim windowStyle: windowStyle = 0
		Dim Command 
		Dim rc
		Dim objFileToRead

		'Command = """" & puplayer.getroot&"\" & cGameName & "\sToken.exe"" " & tmpUUID & " " & tmpVendor & " " &  tmpSerial & " " & MachineID & " """ & QRFile & """ """ & sTokenFile & """ " & domain
		Command = """" & puplayer.getroot & cGameName & "\sToken.exe"" " & tmpUUID & " " & tmpVendor & " " &  tmpSerial & " " & MachineID & " """ & QRFile & """ """ & sTokenFile & """ " & domain
dbg2 "RUNNING Command:" & Command
		rc = wsh.Run(Command, windowStyle, waitOnReturn)
dbg2 "Return:" & rc
		if FileExists(puplayer.getroot&"\" & cGameName & "\sToken.dat") and rc=0 then
			Set objFileToRead = fso.OpenTextFile(puplayer.getroot&"\" & cGameName & "\sToken.dat",1)
			result = objFileToRead.ReadLine()
			objFileToRead.Close
			Set objFileToRead = Nothing

			if Instr(1, result, "Invalid timestamp")<> 0 then 
				MsgBox "Scorbit Timestamp Error: Please make sure the time on your system is exact"
				getStoken=False
			elseif Instr(1, result, ":")<>0 then 
				results=split(result, ":")
				sToken=results(1)
				sToken=mid(sToken, 3, len(sToken)-4)
dbg2 "Got TOKEN:" & sToken
				getStoken=True
			Else 
dbg2 "ERROR:" & result
				getStoken=False
			End if 
		else 
dbg2 "ERROR No File:" & rc
		End if 

	End Function 

	private Function FileExists(FilePath)
		If fso.FileExists(FilePath) Then
			FileExists=CBool(1)
		Else
			FileExists=CBool(0)
		End If
	End Function

	Private Function GetMsg(URLBase, endpoint)
		GetMsg = GetMsgHdr(URLBase, endpoint, "", "")
	End Function

	Private Function GetMsgHdr(URLBase, endpoint, Hdr1, Hdr1Val)
		Dim Url
		Url = URLBase + endpoint & "?session_active=" & bActive
'dbg2 "Url:" & Url  & "  Async=" & bRunAsynch
		objXmlHttpMain.open "GET", Url, bRunAsynch
'		objXmlHttpMain.setRequestHeader "Content-Type", "text/xml"
		objXmlHttpMain.setRequestHeader "Cache-Control", "no-cache"
		if Hdr1<> "" then objXmlHttpMain.setRequestHeader Hdr1, Hdr1Val

'		on error resume next
			err.clear
			objXmlHttpMain.send ""
			if err.number=-2147012867 then 
				MsgBox "Multiplayer Server is down (" & err.number & ") " & Err.Description
				bEnabled=False
			elseif err.number <> 0 then 
				debug3 "Server error: (" & err.number & ") " & Err.Description
			End if 
			if bRunAsynch=False then 
dbg2 "Status: " & objXmlHttpMain.status
				If objXmlHttpMain.status = 200 Then
					GetMsgHdr = objXmlHttpMain.responseText
				Else 
					GetMsgHdr=""
				End if 
			Else 
				bWaitResp=True
				GetMsgHdr=""
			End if 
'		On error goto 0

	End Function

	Private Function PostMsg(URLBase, endpoint, PostData, bAsynch)
		Dim Url

		Url = URLBase + endpoint
'dbg2 "PostMSg:" & Url & " " & PostData			'rtp12

		objXmlHttpMain.open "POST",Url, bAsynch
		objXmlHttpMain.setRequestHeader "Content-Type", "application/x-www-form-urlencoded"
		objXmlHttpMain.setRequestHeader "Content-Length", Len(PostData)
		objXmlHttpMain.setRequestHeader "Cache-Control", "no-cache"
		objXmlHttpMain.setRequestHeader "Authorization", "SToken " & sToken
		if bAsynch then bWaitResp=True 

		on error resume next
			objXmlHttpMain.send PostData
			if err.number=-2147012867 then 
				MsgBox "Multiplayer Server is down (" & err.number & ") " & Err.Description
				bEnabled=False
			elseif err.number <> 0 then 
				'debug3 "Multiplayer Server error (" & err.number & ") " & Err.Description
			End if 
			If objXmlHttpMain.status = 200 Then
				PostMsg = objXmlHttpMain.responseText
			else 
				PostMsg="ERROR: " & objXmlHttpMain.status & " >" & objXmlHttpMain.responseText & "<"
			End if 
		On error goto 0
	End Function

	Private Function pvPostFile(sUrl, sFileName, bAsync)
'dbg2 "Posting File " & sUrl & " " & sFileName & " " & bAsync & " File:" & Mid(sFileName, InStrRev(sFileName, "\") + 1)
		Dim STR_BOUNDARY:STR_BOUNDARY  = GUID()
		Dim nFile  
		Dim baBuffer()
		Dim sPostData
		Dim Response

		'--- read file
		Set nFile = fso.GetFile(sFileName)
		With nFile.OpenAsTextStream()
			sPostData = .Read(nFile.Size)
			.Close
		End With


		'--- prepare body
		sPostData = "--" & STR_BOUNDARY & vbCrLf & _
			"Content-Disposition: form-data; name=""uuid""" & vbCrLf & vbCrLf & _
			SessionUUID & vbcrlf & _
			"--" & STR_BOUNDARY & vbCrLf & _
			"Content-Disposition: form-data; name=""log_file""; filename=""" & SessionUUID & ".csv""" & vbCrLf & _
			"Content-Type: application/octet-stream" & vbCrLf & vbCrLf & _
			sPostData & vbCrLf & _
			"--" & STR_BOUNDARY & "--"


		'--- post
		With objXmlHttpMain
			.Open "POST", sUrl, bAsync
			.SetRequestHeader "Content-Type", "multipart/form-data; boundary=" & STR_BOUNDARY
			.SetRequestHeader "Authorization", "SToken " & sToken
			.Send sPostData ' pvToByteArray(sPostData)
			If Not bAsync Then
				Response= .ResponseText
				pvPostFile = Response
dbg2 "Upload Response: " & Response
			End If
		End With

	End Function

	Private Function pvToByteArray(sText)
		pvToByteArray = StrConv(sText, 128)		' vbFromUnicode
	End Function

End Class 

'  END SCORBIT 
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
Sub DelayQRClaim_Timer()
	if bOnTheFirstBall AND bBallInPlungerLane then 
		ScorbitClaimQR(True)
		Dbg2 " Should be calling Show Claim"
	End If
'	 ScorbitClaimQR(True)
'	DelayQRClaim.Enabled=False
End Sub

sub CheckPairing
dbg2 "Inside SCORBIT check pairing"
	if (Scorbit.bNeedsPairing) then 
		dbg2 "Should be displaying pairing info"
		if DMDType = 0 Then pdmdsetpage 77

		PuPlayer.playlistplayex pBackglass,"PuPOverlays","Scorbit_Pair.png",0,1
		PuPlayer.LabelSet pBackglass, "ScorbitQR1", "PuPOverlays\\QRcode.png",1,"{'mt':2,'width':19.61, 'height':36,'xalign':0,'yalign':0,'ypos':32,'xpos':74.6}"

		DelayQRClaim.Interval=6000
		DelayQRClaim.Enabled=True
	Else
dbg2 "Already Paired"
		DelayQRClaim.Interval=6000
		DelayQRClaim.Enabled=True
	end if
End sub

Sub HideScorbit
	pDMDsetpage pScores
	DelayQRClaim.Enabled = False
	if DMDType = 0 Then
		PuPlayer.playlistplayex pBackglass,"PuPOverlays","defaultDMD.png",0,1
	Else
		PuPlayer.playlistplayex pBackglass,"PuPOverlays","Card1.png",0,1
		if renderingmode = 2 then VR_CabBackglass.image = "card1"
	End If
	pBackglasslabelhide "ScorbitQR1"
	pBackglasslabelhide "ScorbitQRIcon1"
	pBackglasslabelhide "ScorbitQR2"
	pBackglasslabelhide "ScorbitQRIcon2"
End Sub


Sub Dbg2( myDebugText )
' Uncomment the next line to turn off debugging
Exit Sub

If Not IsObject( objIEDebugWindow ) Then
Set objIEDebugWindow = CreateObject( "InternetExplorer.Application" )
objIEDebugWindow.Navigate "about:blank"
objIEDebugWindow.Visible = True
objIEDebugWindow.ToolBar = False
objIEDebugWindow.Width = 600	
objIEDebugWindow.Height = 900
objIEDebugWindow.Left = 2100
objIEDebugWindow.Top = 100
Do While objIEDebugWindow.Busy
Loop
objIEDebugWindow.Document.Title = "My Debug Window"
objIEDebugWindow.Document.Body.InnerHTML = "<b>KOTH Debug Window -TimeStamp: " & GameTime& "</b></br>"
End If

objIEDebugWindow.Document.Body.InnerHTML = objIEDebugWindow.Document.Body.InnerHTML & myDebugText & " --TimeStamp:<b> " & GameTime & "</b><br>" & vbCrLf
End Sub

Sub UpdateModeMessages_Init()
	
End Sub

Sub ClearPupAttractMessages
	PuPlayer.LabelSet pDMD, "Attract2a", "",  0, ""
	PuPlayer.LabelSet pDMD, "Attract2b", "",  0, ""
End Sub


Sub AttractTimer_Timer()
	Select Case AttractTimerCount
		Case 0
			AttractTimer.Interval = 2000
			pDMDLabelhide "AttractCredits"
			PuPlayer.playevent pDMDVideo,"Attract","grimmy_0.mp4",100,30,5,0,""
			if bFreeplay Then
					PuPlayer.LabelSet pDMD,"Attract2a"," FREE ",1,"{'mt':2,'color': " & cWhite &" }"
					PuPlayer.LabelSet pDMD,"Attract2b"," PLAY ",1,"{'mt':2,'color': " & cWhite &" }"  
			Else
				If Credits> 0 Then
					PuPlayer.LabelSet pDMD,"Attract2a"," CREDITS ",1,"{'mt':2,'color': " & cWhite &" }"
					PuPlayer.LabelSet pDMD,"Attract2b"," "&credits&" ",1,"{'mt':2,'color': " & cWhite &" }"  
				Else
					PuPlayer.LabelSet pDMD,"Attract2a"," CREDITS 0 ",1,"{'mt':2,'color': " & cWhite &" }"
					PuPlayer.LabelSet pDMD,"Attract2b"," INSERT COIN ",1,"{'mt':2,'color': " & cWhite &" }"  
				End If
			End If

		Case 1
				PuPlayer.LabelSet pDMD,"Attract2a"," "&HighScoreName(0)&" ",1,"{'mt':2,'color': " & cBlue &" }"
				PuPlayer.LabelSet pDMD,"Attract2b",FormatScore(HighScore(0)),1,"{'mt':2,'color': " & cRed &" }" 
		Case 2
				PuPlayer.LabelSet pDMD,"Attract2a"," "&HighScoreName(1)&" ",1,""
				PuPlayer.LabelSet pDMD,"Attract2b",FormatScore(HighScore(1)),1,""   
		Case 3
				PuPlayer.LabelSet pDMD,"Attract2a"," "&HighScoreName(2)&" ",1,""
				PuPlayer.LabelSet pDMD,"Attract2b",FormatScore(HighScore(2)),1,""   
		Case 4
				PuPlayer.LabelSet pDMD,"Attract2a"," "&HighScoreName(3)&" ",1,""
				PuPlayer.LabelSet pDMD,"Attract2b",FormatScore(HighScore(3)),1,""   
		Case 5

				PuPlayer.LabelSet pDMD,"Attract2a"," GAME OVER ",1,""
				PuPlayer.LabelSet pDMD,"Attract2b","",1,""   	
		Case 6
			AttractTimer.Interval = 4000
			ClearPupAttractMessages
			PuPlayer.playevent pDMDVideo,"Attract","merlinrtp_0.mp4",100,30,0,0,""
		Case 7
			PuPlayer.playevent pDMDVideo,"Attract","picasso_0.mp4",100,30,0,0,""
		Case 8
			PuPlayer.playevent pDMDVideo,"Attract","gman_0.mp4",100,30,0,0,""
		Case 9
			PuPlayer.playevent pDMDVideo,"Attract","zandy_0.mp4",100,30,0,0,""

		Case 10
			PuPlayer.playevent pDMDVideo,"Attract","grimmy_0.mp4",100,30,0,0,""
	End Select

	AttractTimerCount = AttractTimerCount + 1

	if AttractTimerCount > 10 Then AttractTimerCount = 0
End Sub

Dim BeerMinZ, BeerMaxZ, BeerCanZ, BeerWobbleCount, BeerLeft, BeerRight, WobbleSpeed

StopBeerWobble

Sub WobbleBeerCan_Timer()
	BeerWobbleCount = BeerWobbleCount + 1
	if BeerWobbleCount >  160 Then StopBeerWobble


	if BeerLeft Then
	'if BeerWobbleCount > 80 And BeerWobbleCount Mod 4 = 0 Then BeerMaxZ = BeerMaxZ - .1 : Debug.print "Test Left"
		BeerCanZ = BeerCanZ - WobbleSpeed
		'Debug.print  "Z Left:" &BeerCanZ
		primitive014.rotZ = primitive014.rotZ + BeerCanZ
		primitive130.rotZ = primitive130.rotZ + BeerCanZ
		if primitive014.rotZ < -BeerMaxZ Then BeerLeft = 0:BeerRight = 1: BeerCanZ = 0 : BeerMaxZ = BeerMaxZ -.1
	Elseif BeerRight Then
	'if BeerWobbleCount > 80 And BeerWobbleCount Mod 4 = 0 Then BeerMaxZ = BeerMaxZ - .1: Debug.print "Test Right"
		BeerCanZ = BeerCanZ + WobbleSpeed
		'Debug.print  "Z Right:" &BeerCanZ
		primitive014.rotZ = primitive014.rotZ + BeerCanZ
		primitive130.rotZ = primitive130.rotZ + BeerCanZ
		if primitive014.rotZ > BeerMaxZ Then BeerLeft = 1:BeerRight = 0: BeerCanZ = 0 : BeerMaxZ = BeerMaxZ -.1
	End If
	
End Sub

Sub StopBeerWobble
	BeerWobbleCount = 0
	BeerCanZ = 0
	BeerLeft = 1
	BeerRight = 0
	BeerMaxZ = 8
	WobbleSpeed = .25
	Primitive014.RotZ = 0
	Primitive130.RotZ = 0
	WobbleBeerCan.Enabled = 0
End Sub
