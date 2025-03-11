

' ****************    Masters of the Universe    **********************
'
'				Future Pinball original table by: Rom

'			  	  Mastered Edition mod by: TerryRed

' *********************************************************************

' Visual Pinball version: 1.0b PuP PostDMD Mod


' Visual Pinball table conversion by: randr

' Video clip adjustments, fixes, DETAILS, DOF and Pinup Player support by: TerryRed

' SSF by: Thalamus

' Pinup Player Scoreboard DMD and 'PostDMD' LCD support by: LynnInDenver
' Images for High Score in Pinup Player Scoreboard by Antonio Deluca

' *********************************************************************

' NOTE: Much of the code for DOF was translated from DOFLinx code of
' the Future Pinball version. Therefor you will see Timers, variables,
' and sub-routines labelled with DOFLinx that are used with DOF related
' code and commands.

' Since this table was converted from the Future Pinball version, you will
' also see commands or code referencing Future Pinball.

' *********************************************************************

' You will see DOF commands and some subs that are specific for
' Pinup Player. 

' NOTE: You DO NOT require Pinup Player to play this table! It is only
' needed if you want to use the PuP-Pack (Backglass Videos) with the table.
' You will need TerryRed's PuP-Pack to make use of this feature.



Option Explicit				' Force explicit variable declaration

Const ScoreType = 1 		' 1=UltraDMD     2=PuPlayer DMD     3=PuPlayer PostDMD
							' PuPlayer PostDMD is based on the size of the Music screen - set the Music screen to what your full-size LCD is, and it will fall in appropriately.
Const ArcadeLine1="Powered by"
Const ArcadeLine2="Visual"
Const ArcadeLine3="Pinball"
Const DisablePlayfieldMovie=False	'Disable the video billboard on the playfield..

'********* UltraDMD **************
Dim UltraDMD:UltraDMD=0
Const UltraDMD_VideoMode_Stretch = 0
Const UltraDMD_VideoMode_Top = 1
Const UltraDMD_VideoMode_Middle = 2
Const UltraDMD_VideoMode_Bottom = 3
Const UltraDMD_Animation_FadeIn = 0
Const UltraDMD_Animation_FadeOut = 1
Const UltraDMD_Animation_ZoomIn = 2
Const UltraDMD_Animation_ZoomOut = 3
Const UltraDMD_Animation_ScrollOffLeft = 4
Const UltraDMD_Animation_ScrollOffRight = 5
Const UltraDMD_Animation_ScrollOnLeft = 6
Const UltraDMD_Animation_ScrollOnRight = 7
Const UltraDMD_Animation_ScrollOffUp = 8
Const UltraDMD_Animation_ScrollOffDown = 9
Const UltraDMD_Animation_ScrollOnUp = 10
Const UltraDMD_Animation_ScrollOnDown = 11
Const UltraDMD_Animation_None = 14
Const UltraDMD_deOn = 1500

'********* End UltraDMD **************


'********* PuPlayer DMD **************
Dim PuPlayer
Dim FirstRun
FirstRun=1
Const cGameName = "motu" 
Const pDMD=1		'Standard DMD window. Use for "legacy" type DMD screen.
Const pLargeDMD=11 	'Custom Screen for full LCD users.
Const LabelRed=255
Const LabelOrange=33023
Const LabelYellow=65535
Const LabelGreen=65280
Const LabelBlue=16711680
Const LabelPurple=16711808
Const LabelBlack=0
Const LabelGray=8421504
Const LabelWhite=16777215


Sub LoadPuPlayer
		If ScoreType=1 Then Exit Sub: End If
PuPlayer.Init pLargeDMD,cGameName
PuPlayer.Init pDMD,cGameName
If ScoreType = 3 Then
PuPlayer.SendMSG "{ 'mt':301, 'SN': " & pLargeDMD & ", 'FN':15, 'CP':'4,0,0,100,100' }" 'custompos, based on Audio/Music Screen
PuPlayer.playlistadd pLargeDMD,"DMD", 1 , 0
End If
If ScoreType = 2 Then
PuPlayer.SetScreenex pDMD,0,0,0,0,0  'Set PuPlayer Screen <screen number> , xpos, ypos, width, height, poptype
PuPlayer.playlistadd pDMD,"DMD",1,0
End If

'Set Background video on DMD

PuPlayer.playlistplayex pLargeDMD,"DMD","pattern.mp4",0,1
PuPlayer.SetBackground pLargeDMD,1
PuPlayer.playlistplayex pDMD,"DMD","pattern.mp4",0,1
PuPlayer.SetBackground pDMD,1

	'syntax - PuPlayer.LabelNew <screen# or pDMD>,<Labelname>,<fontName>,<size%>,<colour>,<rotation>,<xAlign>,<yAlign>,<xpos>,<ypos>,<PageNum>,<visible>

If ScoreType = 3 Then
	PuPlayer.LabelInit pLargeDMD
End If

	'Page 1 (default score display)
	PuPlayer.LabelNew pLargeDMD,"Play1score","Serif Gothic",		18,LabelYellow	,0,0,1,5,15,1,0
	PuPlayer.LabelNew pLargeDMD,"Play2score","Serif Gothic",		18,LabelYellow	,0,2,1,95,15,1,0
	PuPlayer.LabelNew pLargeDMD,"curscore","Serif Gothic Black",		30,LabelWhite	,0,1,1,50,45,1,1
	PuPlayer.LabelNew pLargeDMD,"Ball","Serif Gothic",			20,LabelWhite	,0,1,1,20,87,1,1
	PuPlayer.LabelNew pLargeDMD,"curplayer","Serif Gothic",		20,LabelWhite		 	,0,1,1,70,87,1,1

	'Page 2 (single line splash)
	PuPlayer.LabelNew pLargeDMD,"Splash1a","Serif Gothic Black",35,LabelWhite,0,1,1,0,50,2,1

	'Page 3 (double line splash)
	PuPlayer.LabelNew pLargeDMD,"Splash2a","Serif Gothic Black",20,LabelYellow,0,1,1,0,40,3,1
	PuPlayer.LabelNew pLargeDMD,"Splash2b","Serif Gothic Black",20,LabelYellow,0,1,1,0,60,3,1

	'Page 4 (triple line splash)
	PuPlayer.LabelNew pLargeDMD,"Splash3a","Serif Gothic Black",20,LabelYellow,0,1,1,0,25,4,1
	PuPlayer.LabelNew pLargeDMD,"Splash3b","Serif Gothic Black",20,LabelYellow,0,1,1,0,50,4,1
	PuPlayer.LabelNew pLargeDMD,"Splash3c","Serif Gothic Black",20,LabelYellow,0,1,1,0,75,4,1

	'Page 5 (High Score Display)
	PuPlayer.LabelNew pLargeDMD,"HiScoreImage","Arial"           ,100,LabelYellow   ,0,1,1, 0,0,5,1  'new image type
	PuPlayer.LabelNew pLargeDMD,"HiScoreName","Serif Gothic Black",		40,LabelYellow	,0,1,1,71,60,5,1
	PuPlayer.LabelNew pLargeDMD,"HiScoreRank","Serif Gothic",		30,LabelWhite	,0,1,1,71,20,5,1
	PuPlayer.LabelNew pLargeDMD,"HiScoreValue","Serif Gothic Black",		15,LabelWhite	,0,1,1,71,80,5,1
	puPlayer.LabelSet pLargeDMD,"HiScoreImage","DMD\\CLEAR.png",1,"{'mt':2,'color':111111, 'width': 100, 'height': 100, 'yalign': 0}"

	'Page 6 (double line large splash)
	PuPlayer.LabelNew pLargeDMD,"Splash6a","Serif Gothic Black",35,LabelYellow,0,1,1,0,35,6,1
	PuPlayer.LabelNew pLargeDMD,"Splash6b","Serif Gothic Black",35,LabelYellow,0,1,1,0,65,6,1

	'Page 7 (High Score Leaderboard Entry)
	PuPlayer.LabelNew pLargeDMD,"EntryName","Serif Gothic",40,LabelYellow,0,1,1,50,20,7,1
	PuPlayer.LabelNew pLargeDMD,"EntryLeftLetter","Serif Gothic",50,LabelYellow,0,1,1,25,75,7,1
	PuPlayer.LabelNew pLargeDMD,"EntryMiddleLetter","Serif Gothic",50,LabelYellow,0,1,1,25,75,7,1
	PuPlayer.LabelNew pLargeDMD,"EntryRightLetter","Serif Gothic",50,LabelYellow,0,1,1,25,75,7,1

	'Page 8 (Winners Don't Use Drugs)
	PuPlayer.LabelNew pLargeDMD,"DrugImage","Arial",100,LabelYellow,0,1,1,0,0,8,1
	PuPlayer.LabelNew pLargeDMD,"DrugLabel","Serif Gothic",18,LabelYellow,0,1,1,50,85,8,1
	puPlayer.LabelSet pLargeDMD,"DrugImage","DMD\\CLEAR.png",1,"{'mt':2,'color':111111, 'width': 100, 'height': 100, 'yalign': 0}"
	PuPlayer.LabelSet pLargeDMD,"DrugLabel",CHR(147)&"Winners Don"&CHR(146)&"t Use Drugs"&CHR(148),1,""

	'Page 9 (quadruple line splash)
	PuPlayer.LabelNew pLargeDMD,"Splash4a","Serif Gothic Black",20,LabelYellow,0,1,1,0,15,9,1
	PuPlayer.LabelNew pLargeDMD,"Splash4b","Serif Gothic Black",20,LabelYellow,0,1,1,0,35,9,1
	PuPlayer.LabelNew pLargeDMD,"Splash4c","Serif Gothic Black",20,LabelYellow,0,1,1,0,65,9,1
	PuPlayer.LabelNew pLargeDMD,"Splash4d","Serif Gothic Black",20,LabelYellow,0,1,1,0,85,9,1

If ScoreType = 2 Then
	PuPlayer.LabelInit pDMD
End If

	'syntax - PuPlayer.LabelNew <screen# or pDMD>,<Labelname>,<fontName>,<size%>,<colour>,<rotation>,<xAlign>,<yAlign>,<xpos>,<ypos>,<PageNum>,<visible>

	'Page 1 (default score display)
	PuPlayer.LabelNew pDMD,"Play1scoreDMD","Serif Gothic",		27,LabelYellow	,0,0,1,5,15,1,0
	PuPlayer.LabelNew pDMD,"Play2scoreDMD","Serif Gothic",		27,LabelYellow	,0,2,1,95,15,1,0
	PuPlayer.LabelNew pDMD,"curscoreDMD","Serif Gothic Black",		45,LabelWhite	,0,1,1,50,45,1,1
	PuPlayer.LabelNew pDMD,"BallDMD","Serif Gothic",			30,LabelWhite	,0,1,1,20,87,1,1
	PuPlayer.LabelNew pDMD,"curplayerDMD","Serif Gothic",		30,LabelWhite		 	,0,1,1,70,87,1,1

	'Page 2 (single line splash)
	PuPlayer.LabelNew pDMD,"Splash1aDMD","Serif Gothic Black",35,LabelWhite,0,1,1,0,50,2,1

	'Page 3 (double line splash)
	PuPlayer.LabelNew pDMD,"Splash2aDMD","Serif Gothic Black",30,LabelYellow,0,1,1,0,30,3,1
	PuPlayer.LabelNew pDMD,"Splash2bDMD","Serif Gothic Black",30,LabelYellow,0,1,1,0,70,3,1

	'Page 4 (triple line splash)
	PuPlayer.LabelNew pDMD,"Splash3aDMD","Serif Gothic Black",20,LabelYellow,0,1,1,0,25,4,1
	PuPlayer.LabelNew pDMD,"Splash3bDMD","Serif Gothic Black",20,LabelYellow,0,1,1,0,50,4,1
	PuPlayer.LabelNew pDMD,"Splash3cDMD","Serif Gothic Black",20,LabelYellow,0,1,1,0,75,4,1

	'Page 5 (High Score Display)
	PuPlayer.LabelNew pDMD,"HiScoreImageDMD","Arial"           ,100,LabelYellow   ,0,1,1, 0,0,5,1  'new image type
	PuPlayer.LabelNew pDMD,"HiScoreNameDMD","Serif Gothic Black",		60,LabelYellow	,0,1,1,60,45,5,1
	PuPlayer.LabelNew pDMD,"HiScoreRankDMD","Serif Gothic",		30,LabelWhite	,0,1,1,60,15,5,1
	PuPlayer.LabelNew pDMD,"HiScoreValueDMD","Serif Gothic Black",		30,LabelWhite	,0,1,1,60,80,5,1
	puPlayer.LabelSet pDMD,"HiScoreImage","DMD\\CLEAR.png",1,"{'mt':2,'color':111111, 'width': 50, 'height': 100, 'yalign': 0, 'xalign':0}"

	'Page 6 (double line large splash)
	PuPlayer.LabelNew pDMD,"Splash6aDMD","Serif Gothic Black",35,LabelYellow,0,1,1,0,35,6,1
	PuPlayer.LabelNew pDMD,"Splash6bDMD","Serif Gothic Black",35,LabelYellow,0,1,1,0,65,6,1

	'Page 7 (High Score Leaderboard Entry)
	PuPlayer.LabelNew pDMD,"EntryNameDMD","Serif Gothic",40,LabelYellow,0,1,1,50,20,7,1
	PuPlayer.LabelNew pDMD,"EntryLeftLetterDMD","Serif Gothic",50,LabelYellow,0,1,1,25,75,7,1
	PuPlayer.LabelNew pDMD,"EntryMiddleLetterDMD","Serif Gothic",50,LabelYellow,0,1,1,25,75,7,1
	PuPlayer.LabelNew pDMD,"EntryRightLetterDMD","Serif Gothic",50,LabelYellow,0,1,1,25,75,7,1

	'Page 8 (Winners Don't Use Drugs)
	PuPlayer.LabelNew pDMD,"DrugImageDMD","Arial",100,LabelYellow,0,0,1,5,5,8,1
	PuPlayer.LabelNew pDMD,"DrugLabel1DMD","Serif Gothic Black",35,LabelYellow,0,1,1,60,30,8,1
	PuPlayer.LabelNew pDMD,"DrugLabel2DMD","Serif Gothic Black",35,LabelYellow,0,1,1,60,70,8,1
	puPlayer.LabelSet pDMD,"DrugImage","DMD\\CLEAR.png",1,"{'mt':2,'color':111111, 'width': 60, 'height': 120, 'yalign': 0}"
	PuPlayer.LabelSet pDMD,"DrugLabel1DMD",CHR(147)&"Winners Don"&CHR(146)&"t",1,""
	PuPlayer.LabelSet pDMD,"DrugLabel2DMD","Use Drugs"&CHR(148),1,""

	'Page 9 (quadruple line splash)
	PuPlayer.LabelNew pDMD,"Splash4aDMD","Serif Gothic Black",20,LabelYellow,0,1,1,0,15,9,1
	PuPlayer.LabelNew pDMD,"Splash4bDMD","Serif Gothic Black",20,LabelYellow,0,1,1,0,35,9,1
	PuPlayer.LabelNew pDMD,"Splash4cDMD","Serif Gothic Black",20,LabelYellow,0,1,1,0,65,9,1
	PuPlayer.LabelNew pDMD,"Splash4dDMD","Serif Gothic Black",20,LabelYellow,0,1,1,0,85,9,1


End Sub

	Sub pUpdateScores
		If ScoreType=1 Then Exit Sub: End If
		PuPlayer.LabelShowPage pLargeDMD,1,0,""
		PuPlayer.LabelSet pLargeDMD,"curscore",FormatNumber(nvScore(CurrentPlayer),0),1,""
		PuPlayer.LabelSet pLargeDMD,"curplayer","Player " & CurrentPlayer,1,""
		PuPlayer.LabelShowPage pDMD,1,0,""
		PuPlayer.LabelSet pDMD,"curscoreDMD",FormatNumber(nvScore(CurrentPlayer),0),1,""
		PuPlayer.LabelSet pDMD,"curplayerDMD","Player " & CurrentPlayer,1,""
		If CurrentPlayer = 1 Then
			PuPlayer.LabelSet pLargeDMD,"Play1score","" & FormatNumber(nvScore(1),0),1,"{'mt':2,'color':" & LabelWhite & "}"
			PuPlayer.LabelSet pDMD,"Play1scoreDMD","" & FormatNumber(nvScore(1),0),1,"{'mt':2,'color':" & LabelWhite & "}"
			'make other scores inactive
			If PlayersPlayingGame = 2 Then
				PuPlayer.LabelSet pLargeDMD,"Play2score","" & FormatNumber(nvScore(2),0),1,"{'mt':2,'color':" & LabelYellow & "}"
				PuPlayer.LabelSet pDMD,"Play2scoreDMD","" & FormatNumber(nvScore(2),0),1,"{'mt':2,'color':" & LabelYellow & "}"
			End If
		End If
		If CurrentPlayer = 2 Then
			PuPlayer.LabelSet pLargeDMD,"Play2score","" & FormatNumber(nvScore(2),0),1,"{'mt':2,'color':" & LabelWhite & "}"
			PuPlayer.LabelSet pLargeDMD,"Play1score","" & FormatNumber(nvScore(1),0),1,"{'mt':2,'color':" & LabelYellow & "}"
			PuPlayer.LabelSet pDMD,"Play2scoreDMD","" & FormatNumber(nvScore(2),0),1,"{'mt':2,'color':" & LabelWhite & "}"
			PuPlayer.LabelSet pDMD,"Play1scoreDMD","" & FormatNumber(nvScore(1),0),1,"{'mt':2,'color':" & LabelYellow & "}"
		End If
	PuPlayer.LabelSet pLargeDMD,"Ball","Balls: " & BallsRemaining(CurrentPlayer),1,""
	PuPlayer.LabelSet pDMD,"BallDMD","Balls: " & BallsRemaining(CurrentPlayer),1,""
	end Sub

Sub pDMDLabelHide(labName)
		If ScoreType=1 Then Exit Sub: End If
	PuPlayer.LabelSet pLargeDMD,labName,"",0,""   
	PuPlayer.LabelSet pDMD,labName,"",0,""   
end sub

Sub pDMDDrugsLarge()  
		If ScoreType=1 Then Exit Sub: End If
	PuPlayer.LabelShowPage pLargeDMD,8,4000,""
	puPlayer.LabelSet pLargeDMD,"DrugImage","DMD\\Winners.png",1,"{'mt':2,'color':111111, 'width': 100, 'height': 100, 'yalign': 0}"
	PuPlayer.LabelSet pLargeDMD,"DrugLabel",CHR(147)&"Winners Don"&CHR(146)&"t Use Drugs"&CHR(148),1,""
end Sub

Sub pDMDDrugsDMD()  
		If ScoreType=1 Then Exit Sub: End If
	PuPlayer.LabelShowPage pDMD,8,4000,""
	puPlayer.LabelSet pDMD,"DrugImageDMD","DMD\\WINNERSDMD.png",1,"{'mt':2,'color':111111, 'width': 22.5, 'height': 90, 'yalign': 0}"
'	puPlayer.LabelSet pDMD,"Splash2aDMD",CHR(147)&"Winners Don"&CHR(146)&"t",1,""
'	PuPlayer.LabelSet pDMD,"Splash2bDMD","Use Drugs"&CHR(148),1,""
'	PuPlayer.LabelShowPage pDMD,3,4000,""
end Sub

Sub pDMDScoreEntry(msgText,msgText2,msgText3,msgText4,mColor)
		If ScoreType=1 Then Exit Sub: End If
	PuPlayer.LabelSet pLargeDMD,"EntryName",msgText,1,""
	PuPlayer.LabelSet pLargeDMD,"EntryLeftLetter",msgText2,1,""
	PuPlayer.LabelSet pLargeDMD,"EntryMiddleLetter",msgText3,1,""
	PuPlayer.LabelSet pLargeDMD,"EntryRightLetter",msgText4,1,""
	PuPlayer.LabelSet pDMD,"EntryNameDMD",msgText,1,""
	PuPlayer.LabelSet pDMD,"EntryLeftLetterDMD",msgText2,1,""
	PuPlayer.LabelSet pDMD,"EntryMiddleLetterDMD",msgText3,1,""
	PuPlayer.LabelSet pDMD,"EntryRightLetterDMD",msgText4,1,""
	PuPlayer.LabelShowPage pLargeDMD,7,0,""
	PuPlayer.LabelShowPage pDMD,7,0,""
end sub

Sub pDMDSplashBig(msgText,timeSec, mColor)
		If ScoreType=1 Then Exit Sub: End If
	PuPlayer.LabelSet pLargeDMD,"Splash1a",msgText,1,"{'mt':1,'at':1,'fc':" & mColor & "}"  
	PuPlayer.LabelSet pDMD,"Splash1aDMD",msgText,1,"{'mt':1,'at':1,'fc':" & mColor & "}"  
	PuPlayer.LabelShowPage pLargeDMD,2,timeSec,""
	PuPlayer.LabelShowPage pDMD,2,timeSec,""
end sub

Sub pDMDScrollBig(msgText,timeSec,mColor)
		If ScoreType=1 Then Exit Sub: End If
	PuPlayer.LabelSet pLargeDMD,"Splash1a",msgText,1,"{'mt':1,'at':2,'xps':1,'xpe':-1:" & (timeSec*1000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
	PuPlayer.LabelSet pDMD,"Splash1aDMD",msgText,1,"{'mt':1,'at':2,'xps':1,'xpe':-1:" & (timeSec*1000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
	PuPlayer.LabelShowPage pLargeDMD,2,timeSec,""
	PuPlayer.LabelShowPage pDMD,2,timeSec,""
end sub

Sub pDMDScrollBigV(msgText,timeSec,mColor)
		If ScoreType=1 Then Exit Sub: End If
	PuPlayer.LabelSet pLargeDMD,"Splash1a",msgText,1,"{'mt':1,'at':2,'yps':1,'ype':-1:" & (timeSec*1000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
	PuPlayer.LabelSet pDMD,"Splash1aDMD",msgText,1,"{'mt':1,'at':2,'yps':1,'ype':-1:" & (timeSec*1000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
	PuPlayer.LabelShowPage pLargeDMD,2,timeSec,""
	PuPlayer.LabelShowPage pDMD,2,timeSec,""
end sub

Sub pDMDSplashLines(msgText,msgText2,timeSec,mColor)
		If ScoreType=1 Then Exit Sub: End If
	PuPlayer.LabelSet pLargeDMD,"Splash2a",msgText,1,"{'mt':1,'at':1,'color':" & mColor & "}"
	PuPlayer.LabelSet pLargeDMD,"Splash2b",msgText2,1,"{'mt':1,'at':1,'color':" & mColor & "}"   
	PuPlayer.LabelSet pDMD,"Splash2aDMD",msgText,1,"{'mt':1,'at':1,'color':" & mColor & "}"
	PuPlayer.LabelSet pDMD,"Splash2bDMD",msgText2,1,"{'mt':1,'at':1,'color':" & mColor & "}"   
	PuPlayer.LabelShowPage pLargeDMD,3,timeSec,""
	PuPlayer.LabelShowPage pDMD,3,timeSec,""
end Sub

Sub pDMDSplashLinesLarge(msgText,msgText2,timeSec,mColor)
		If ScoreType=1 Then Exit Sub: End If
	PuPlayer.LabelSet pLargeDMD,"Splash2a",msgText,1,"{'mt':1,'at':1,'fc':" & mColor & "}"
	PuPlayer.LabelSet pLargeDMD,"Splash2b",msgText2,1,"{'mt':1,'at':1,'fc':" & mColor & "}"   
	PuPlayer.LabelShowPage pLargeDMD,3,timeSec,""
end Sub

Sub pDMDSplashLinesDMD(msgText,msgText2,timeSec,mColor)
		If ScoreType=1 Then Exit Sub: End If
	PuPlayer.LabelSet pDMD,"Splash2aDMD",msgText,1,"{'mt':1,'at':1,'fc':" & mColor & "}"
	PuPlayer.LabelSet pDMD,"Splash2bDMD",msgText2,1,"{'mt':1,'at':1,'fc':" & mColor & "}"   
	PuPlayer.LabelShowPage pDMD,3,timeSec,""
end Sub

Sub pDMDSplashBigLines(msgText,msgText2,timeSec,mColor)
		If ScoreType=1 Then Exit Sub: End If
	PuPlayer.LabelSet pLargeDMD,"Splash6a",msgText,1,"{'mt':1,'at':1,'fc':" & mColor & "}"
	PuPlayer.LabelSet pLargeDMD,"Splash6b",msgText2,1,"{'mt':1,'at':1,'fc':" & mColor & "}"   
	PuPlayer.LabelSet pDMD,"Splash6aDMD",msgText,1,"{'mt':1,'at':1,'fc':" & mColor & "}"
	PuPlayer.LabelSet pDMD,"Splash6bDMD",msgText2,1,"{'mt':1,'at':1,'fc':" & mColor & "}"   
	PuPlayer.LabelShowPage pLargeDMD,6,timeSec,""
	PuPlayer.LabelShowPage pDMD,6,timeSec,""
end Sub

Sub pDMDSplashScore(msgText,timeSec,mColor)
		If ScoreType=1 Then Exit Sub: End If
	PuPlayer.LabelSet pLargeDMD,"MsgScore",msgText,1,"{'mt':1,'at':1,'fq':250:"& (timeSec*1000) &",'fc':" & mColor & "}"
	PuPlayer.LabelSet pDMD,"MsgScoreDMD",msgText,1,"{'mt':1,'at':1,'fq':250:"& (timeSec*1000) &",'fc':" & mColor & "}"
end Sub

Sub pDMDSplashScoreScroll(msgText,timeSec,mColor)
		If ScoreType=1 Then Exit Sub: End If
	PuPlayer.LabelSet pLargeDMD,"MsgScore",msgText,1,"{'mt':1,'at':2,'xps':1,'xpe':-1:"& (timeSec*1000) &", 'mlen':"& (timeSec*1000) &",'tt':0, 'fc':" & mColor & "}"
	PuPlayer.LabelSet pDMD,"MsgScoreDMD",msgText,1,"{'mt':1,'at':2,'xps':1,'xpe':-1:"& (timeSec*1000) &", 'mlen':"& (timeSec*1000) &",'tt':0, 'fc':" & mColor & "}"
end Sub

Sub pDMDSplashTriple(topText,middleText,bottomText, timeSec, mColor)  
		If ScoreType=1 Then Exit Sub: End If
	PuPlayer.LabelSet pLargeDMD,"Splash3a"  ,topText  ,1,"{'mt':1,'fc':" & mColor & "}"
	PuPlayer.LabelSet pLargeDMD,"Splash3b",middleText,1,"{'mt':1,'fc':" & mColor & "}"
	PuPlayer.LabelSet pLargeDMD,"Splash3c" ,bottomText ,1,"{'mt':1,'fc':" & mColor & "}"   
	PuPlayer.LabelSet pDMD,"Splash3aDMD"  ,topText  ,1,"{'mt':1,'fc':" & mColor & "}"
	PuPlayer.LabelSet pDMD,"Splash3bDMD",middleText,1,"{'mt':1,'fc':" & mColor & "}"
	PuPlayer.LabelSet pDMD,"Splash3cDMD" ,bottomText ,1,"{'mt':1,'fc':" & mColor & "}"   
	PuPlayer.LabelShowPage pLargeDMD,4,timeSec,""  'show page 4
	PuPlayer.LabelShowPage pDMD,4,timeSec,""  'show page 4
end Sub

Sub pDMDSplashQuad(firstText,secondText,thirdText,fourthText, timeSec, mColor)  
		If ScoreType=1 Then Exit Sub: End If
	PuPlayer.LabelSet pLargeDMD,"Splash4a"  ,firstText  ,1,"{'mt':1,'fc':" & mColor & "}"
	PuPlayer.LabelSet pLargeDMD,"Splash4b",secondText,1,"{'mt':1,'fc':" & mColor & "}"
	PuPlayer.LabelSet pLargeDMD,"Splash4c" ,thirdText ,1,"{'mt':1,'fc':" & mColor & "}"   
	PuPlayer.LabelSet pLargeDMD,"Splash4d" ,fourthText ,1,"{'mt':1,'fc':" & mColor & "}"   
	PuPlayer.LabelSet pDMD,"Splash2aDMD"  ,firstText & secondText  ,1,"{'mt':1,'fc':" & mColor & "}"
	PuPlayer.LabelSet pDMD,"Splash2bDMD",thirdText & fourthText,1,"{'mt':1,'fc':" & mColor & "}"
	PuPlayer.LabelShowPage pLargeDMD,9,timeSec,""  'show page 4
	PuPlayer.LabelShowPage pDMD,3,timeSec,""  'show page 4
end Sub

Sub pDMDTargetLetters(backText,middleText,flashText, timeSec, mColor)  
		If ScoreType=1 Then Exit Sub: End If
	PuPlayer.LabelSet pLargeDMD,"Splash3a"  ,backText  ,1,""
	PuPlayer.LabelSet pLargeDMD,"Splash3b",middleText,1,""
	PuPlayer.LabelSet pLargeDMD,"Splash3c" ,flashText ,1,"{'mt':1,'at':1,'fq':250:" & (timeSec*1000) & ",'fc':" & mColor & "}"   
	PuPlayer.LabelSet pDMD,"Splash3aDMD"  ,backText  ,1,""
	PuPlayer.LabelSet pDMD,"Splash3bDMD",middleText,1,""
	PuPlayer.LabelSet pDMD,"Splash3cDMD" ,flashText ,1,"{'mt':1,'at':1,'fq':250:" & (timeSec*1000) & ",'fc':" & mColor & "}"   
	PuPlayer.LabelShowPage pLargeDMD,4,timeSec,""  'show page 4
	PuPlayer.LabelShowPage pDMD,4,timeSec,""  'show page 4
end Sub

Sub pDMDHighScore(nameText,rankText,valueText,imageText, timeSec, mColor)  
		If ScoreType=1 Then Exit Sub: End If
If FirstRun=1 Then
	PuPlayer.LabelShowPage pLargeDMD,5,timeSec,""  'show page 5
	PuPlayer.LabelShowPage pDMD,5,timeSec,""  'show page 5
	FirstRun=0
End If
	puPlayer.LabelSet pLargeDMD,"HiScoreImage",imageText,1,"{'mt':2,'color':111111, 'width': 100, 'height': 100, 'yalign': 0}"
	PuPlayer.LabelSet pLargeDMD,"HiScoreName"  ,nameText  ,1,""
	PuPlayer.LabelSet pLargeDMD,"HiScoreRank",rankText,1,""
	PuPlayer.LabelSet pLargeDMD,"HiScoreValue" ,"" & FormatNumber(valueText,0) ,1,""   
	puPlayer.LabelSet pDMD,"HiScoreImageDMD",imageText,1,"{'mt':2,'color':111111, 'width': 50, 'height': 100, 'yalign': 0, 'xalign': 0}"
	PuPlayer.LabelSet pDMD,"HiScoreNameDMD"  ,nameText  ,1,""
	PuPlayer.LabelSet pDMD,"HiScoreRankDMD",rankText,1,""
	PuPlayer.LabelSet pDMD,"HiScoreValueDMD" ,"" & FormatNumber(valueText,0) ,1,""   
	PuPlayer.LabelShowPage pLargeDMD,5,timeSec,""  'show page 5
	PuPlayer.LabelShowPage pDMD,5,timeSec,""  'show page 5
end Sub

Sub pDMDZoomBig(msgText,timeSec,mColor)  'new Zoom
		If ScoreType=1 Then Exit Sub: End If
	PuPlayer.LabelSet pLargeDMD,"Splash1a",msgText,1,"{'mt':1,'at':3,'hstart':5,'hend':80:" & (timeSec*1000) & ",'mlen':" & (timeSec*500) & ",'tt':5,'fc':" & mColor & "}"
	PuPlayer.LabelSet pDMD,"Splash1aDMD",msgText,1,"{'mt':1,'at':3,'hstart':5,'hend':80:" & (timeSec*1000) & ",'mlen':" & (timeSec*500) & ",'tt':5,'fc':" & mColor & "}"
	PuPlayer.LabelShowPage pLargeDMD,2,timeSec,""
	PuPlayer.LabelShowPage pDMD,2,timeSec,""
end sub

Sub pDMDTargetLettersInfo(msgText,msgInfo, timeSec, mColor)  'msgInfo = '0211'  0= layer 1, 1=layer 2, 2=top layer3.
		If ScoreType=1 Then Exit Sub: End If
	Dim backText
	Dim middleText
	Dim flashText
	Dim curChar
	Dim i

	For i=1 To Len(msgInfo)
		curChar="" & Mid(msgInfo,i,1)
		if curChar="0" Then
			backText=backText & Mid(msgText,i,1)
				middleText=middleText & " "
				flashText=flashText & " "          
		End If
		if curChar="1" Then
				backText=backText & " "
				middleText=middleText & Mid(msgText,i,1)
				flashText=flashText & " "          
		End If
		if curChar="2" Then
				backText=backText & " "
				middleText=middleText & " "
				flashText=flashText & Mid(msgText,i,1)
		End If   
	Next 

	PuPlayer.LabelSet pLargeDMD,"Splash3a"  ,backText  ,1,""
	PuPlayer.LabelSet pLargeDMD,"Splash3b",middleText,1,""
	PuPlayer.LabelSet pLargeDMD,"Splash3c" ,flashText ,0,"{'mt':1,'at':1,'fq':250,'len':" & (timeSec*1000) & ",'fc':" & mColor & "}"   
	PuPlayer.LabelSet pDMD,"Splash3aDMD"  ,backText  ,1,""
	PuPlayer.LabelSet pDMD,"Splash3bDMD",middleText,1,""
	PuPlayer.LabelSet pDMD,"Splash3cDMD" ,flashText ,0,"{'mt':1,'at':1,'fq':250,'len':" & (timeSec*1000) & ",'fc':" & mColor & "}"   
	PuPlayer.LabelShowPage pLargeDMD,4,timeSec,""  'show page 5
	PuPlayer.LabelShowPage pDMD,4,timeSec,""  'show page 5
end Sub

'******* End PuPlayer DMD ************


' Thalamus 2018-10-17 : Improved directional sounds
'  .5 = lower volume
' 1.5 = higher volume

Const VolBump   = 2    ' Bumpers volume.
Const VolGates  = 1    ' Gates volume.
Const VolMetal  = 1    ' Metals volume.
Const VolRH     = 1    ' Rubber hits volume.
Const VolPo     = 1    ' Rubber posts volume.
Const VolPi     = 1    ' Rubber pins volume.
Const VolTarg   = 1    ' Targets volume.
Const VolKick   = 1    ' Kicker volume.
Const VolSpin   = .25  ' Spinners volume.
Const VolFlip   = 1    ' Flipper volume.
Const VolSling  = 2    ' SlingShot volume.
Const VolHeman  = 1    ' Heman volume.
Const VolRamp   = 1    ' Ramphit volume.


' Define any Constants
Const constMaxPlayers 		= 2 		' Maximum number of players per game (between 1 and 4)
Const constBallSaverTime	= 10000	' Time in which a free ball is given if it lost very quickly
												' Set this to 0 if you don't want this feature
Const constMaxMultiplier	= 6		' Defines the maximum bonus multiplier level

Dim movieSpeed

movieSpeed= 90 'msecs

' Define Global Variables
'
Dim PlayersPlayingGame		' number of players playing the current game
Dim CurrentPlayer				' current player (1-4) playing the game
Dim BonusPoints(4)			' Bonus Points for the current player
Dim BonusMultiplier(4)		' Bonus Multiplier for the current player
Dim BallsRemaining(4)		' Balls remaining to play (inclusive) for each player
Dim ExtraBallsAwards(4)		' number of EB's out-standing (for each player)

' Define Game Control Variables
Dim LastSwitchHit				' Id of last switch hit
Dim BallsOnPlayfield			' number of balls on playfield (multiball exclusive)
Dim BallsInLock				' number of balls in multi-ball lock

' Define Game Flags
Dim bFreePlay					' Either in Free Play or Handling Credits
Dim bOnTheFirstBall			' First Ball (player one). Used for Adding New Players
Dim bBallInPlungerLane		' is there a ball in the plunger lane
Dim bBallSaverActive			' is the ball saver active
Dim bMultiBallMode			' multiball mode active ?
Dim bEnteringAHighScore		' player is entering their name into the high score table
dim scoreupdate
dim lastpoints
Dim nvBallsPerGame:nvBallsPerGame = 3
Dim nvCredits:nvCredits = 0
Dim VpTilted:VpTilted = False
Dim VpGameInPlay:VpGameInPlay = False
Dim nvTotalGamesPlayed:nvTotalGamesPlayed = 0
Dim nvScore(4)
Dim Score:Score=0
Dim AttractVideo
Dim CurrentMusicTunePlaying:CurrentMusicTunePlaying = 0
Dim discspeed: discspeed=150		'set the disc physical max speed, higher for more ball effect
Dim discrotspeed: discrotspeed=15	'set the visual disc rotation speed, units in degrees per timer cycle, higher is faster
Dim nvjackpot:nvjackpot = 0
AttractVideo = Array("CA ", 31)
Dim CastleMB_Mode_Active:CastleMB_Mode_Active=False
Dim SkeletorMB_Mode_Active:SkeletorMB_Mode_Active=False
Dim RoadRipperMB_Mode_Active:RoadRipperMB_Mode_Active=False
Dim HeManMB_Mode_Active:HeManMB_Mode_Active=False
Dim TableFirstLoad:TableFirstLoad=true

'Clips that will play
Dim CAClip
Dim LogoClip
Dim hemanpogClip
Dim CastleOpenClip
Dim CastlecloseClip
Dim HeyyayClip
Dim evillynclip
Dim GreySkullLitclip
Dim hemanattackclip
Dim hemanCellclip
Dim RoadRipperclip
Dim RipperMBclip
Dim Backoffclip
Dim Skeletorextraballclip
Dim SkeletorFrightenedclip
Dim SkeletorHollowclip
Dim SkeletorLaughingclip
Dim Skeletormbclip
Dim Skeletorgetoutclip
Dim Skeletorpalaceclip
Dim Skeletorpatienceclip
Dim Stophimclip
Dim Skeletorstunrayclip
Dim Skeletorsnakemountainclip
Dim sorceressflyclip
Dim sorceresshoverclip
Dim sorceressswoopclip
Dim spinninglogoclip
Dim spinningswordclip
Dim wellwellclip
Dim frightened
Dim boob

If DisablePlayfieldMovie=False Then
CAClip = Array("CA ", 31)
Logoclip = Array("Logo ", 16)
hemanpogClip = Array("hemanpog ", 191)
CastleOpenClip = Array("CastleOpen ", 42)
CastlecloseClip = Array("Castleclose ", 42)
HeyyayClip = Array("Heyyay ", 118)
evillynclip = Array("evillyn ", 29)
GreySkullLitclip = Array("GreySkullLit ", 37)
hemanattackclip = Array("hemanattack ", 25)
hemanCellclip = Array("HemanCell ", 36)
RoadRipperclip = Array("RoadRipper ", 37)
Backoffclip = Array("Backoff ", 19)
SkeletorHollowclip = Array("SkeletorHollow ", 34)
SkeletorLaughingclip = Array("SkeletorLaughing ", 22)
Skeletormbclip = Array("Skeletormb ", 103)
Skeletorgetoutclip = Array("Skeletorgetout ", 55)
Skeletorpalaceclip = Array("Skeletorpalace ", 29)
Skeletorpatienceclip = Array("Skeletorpatience ", 32)
Stophimclip = Array("Stophim ", 23)
Skeletorstunrayclip = Array("Skeletorstunray ", 38)
Skeletorsnakemountainclip = Array("Skeletorsnakemountain ", 26)
sorceressflyclip = Array("sorceressfly ", 23)
sorceresshoverclip = Array("sorceresshover ", 24)
sorceressswoopclip = Array("sorceressswoop ", 22)
spinninglogoclip = Array("spinninglogo ", 21)
spinningswordclip = Array("spinningsword ", 32)
wellwellclip = Array("wellwell ", 43)
frightened = Array("frightened ", 29)
boob = Array("boob ", 51)
End If
dim atractcount



'####  Mastered Edition Videos Section: TerryRed  ####


Sub Videos_MOTU_Intro()   ' PuP - MOTU Intro - Attract
	DOF 600, DOFPulse 
End Sub

Sub Videos_Ball_Saved()   'PuP - Ball Saved
	DOF 601, DOFPulse  
End Sub

Sub Videos_Castle_Jackpot()   'PuP - Castle_Jackpot
	DOF 602, DOFPulse  
End Sub

Sub Videos_HeMan_Jackpot()   'PuP - HeMan_Jackpot
	DOF 614, DOFPulse  
End Sub

Sub Videos_Skeletor_Jackpot()   'PuP - Skeletor_Jackpot
	DOF 628, DOFPulse   
End Sub

Sub Videos_Ripper_Jackpot()   'PuP - Ripper_Jackpot
	DOF 622, DOFPulse   
End Sub

Sub Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
	DOF 638, DOFPulse   
End Sub

Sub Videos_LeftFlipper()  'PuP - Left Flipper Video
If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	DOF 640, DOFPulse   
End Sub

Sub Videos_RightFlipper()  'PuP - Right Flipper Video
If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	DOF 641, DOFPulse   
End Sub

Sub Videos_CoinInserted()   'PuP - Coin Inserted
	DOF 642, DOFPulse 
End Sub

Sub Videos_Castle_Open_50000()  'Play Castle_Open 50,000 Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=100
	signon.imageb = ""
	Animation "CastleOpen_", 42, 1
	signon.visible = true
	VideoIntro.interval = 3780
	VideoIntro.enabled = true
	End If
	DOF 603, DOFPulse  'PuP - Castle Grayskull
End Sub
'
Sub Videos_Castle_Close_100000()  'Play Castle_Close Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=100
	signon.imageb = ""
	Animation "Castleclose_ ", 42, 1
	signon.visible = true
	VideoIntro.interval = 3780
	VideoIntro.enabled = true
	End If
	DOF 603, DOFPulse  'PuP - Castle Grayskull
End Sub
'
Sub Videos_Castle_Open_250000()  'Play Castle_Open 250,000 Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=100
	signon.imageb = ""
	Animation "CastleOpen_", 42, 1
	signon.visible = true
	VideoIntro.interval = 3780
	VideoIntro.enabled = true
	End If
	DOF 603, DOFPulse  'PuP - Castle Grayskull
End Sub
'
Sub Videos_EvilLynn_Laugh()  'Play EvilLynn_Laugh Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=90
	signon.imageb = ""
	Animation "EVilLyn_Laugh_", 29, 1
	signon.visible = true
	VideoIntro.interval = 2610
	VideoIntro.enabled = true
	End If
	DOF 608, DOFPulse  'PuP - Ball 1 Locked
End Sub
'
Sub Videos_Grayskull_Lit()  'Play Grayskull_Lit Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=50
	signon.imageb = ""
	Animation "GraySkull_lit_", 37, 1
	signon.visible = true
	VideoIntro.interval = 2000
	VideoIntro.enabled = true
	End If
	DOF 604, DOFPulse  'PuP - Grayskull Lit
End Sub
'
Sub Videos_HeMan_AttackBall()  'Play HeMan_AttackBall Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=90
	signon.imageb = ""
	Animation "He-Man_AttackBall_", 25, 1
	signon.visible = true
	VideoIntro.interval = 2250
	VideoIntro.enabled = true
	End If
	DOF 615, DOFPulse  'PuP - He-Man Kicker
End Sub
'
Sub Videos_Skeletor_BackOff()  'Play Skeletor_BackOff Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=80
	signon.imageb = ""
	Animation "Skeletor_BackOff_", 19, 1
	signon.visible = true
	VideoIntro.interval = 1510
	VideoIntro.enabled = true
	End If
	DOF 625, DOFPulse  'PuP - Skeletor Hit 2
End Sub
'
Sub Videos_Skeletor_ExtraBall()  'Play Skeletor_ExtraBall Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=60
	signon.imageb = ""
	Animation "Skeletor_Extraball_", 29, 1
	signon.visible = true
	VideoIntro.interval = 1800
	VideoIntro.enabled = true
	End If
	DOF 606, DOFPulse  'PuP - Extra Ball
End Sub
'
Sub Videos_Skeletor_Frightened()  'Play Skeletor_Frightened Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=90
	signon.imageb = ""
	Animation "Skeletor_Frightened_", 29, 1
	signon.visible = true
	VideoIntro.interval = 2610
	VideoIntro.enabled = true
	End If
	DOF 624, DOFPulse  'PuP - Skeletor Hit 1
End Sub
'
Sub Videos_Skeletor_Hollow()  'Play Skeletor_Hollow Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=90
	signon.imageb = ""
	Animation "Skeletor_Hollow_", 34, 1
	signon.visible = true
	VideoIntro.interval = 3060
	VideoIntro.enabled = true
	End If
	DOF 611, DOFPulse  'PuP - Ball 1 Locked - 2
End Sub
'
Sub Videos_Skeletor_Laughing()  'Play Skeletor_Laughing Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=100
	signon.imageb = ""
	Animation "Skeletor_Laughing_", 22, 1
	signon.visible = true
	VideoIntro.interval = 1980
	VideoIntro.enabled = true
	End If
	DOF 627, DOFPulse  'PuP - Skeletor Hit 4
End Sub
'
Sub Videos_Skeletor_Patience()  'Play Skeletor_Patience Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=80
	signon.imageb = ""
	Animation "skeletor_patience_", 32, 1
	signon.visible = true
	VideoIntro.interval = 2400
	VideoIntro.enabled = true
	End If
	DOF 626, DOFPulse  'PuP - Skeletor Hit 3
End Sub
'
Sub Videos_Skeletor_Boob()  'Play Skeletor_Boob Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=80
	signon.imageb = ""
	Animation "skeletor_patience_", 32, 1
	signon.visible = true
	Videoboob.interval = 2000
	Videoboob.enabled = true
	End If
	DOF 620, DOFPulse  'PuP - No Pay No Play
End Sub
'
Sub Videos_Skeletor_StopHim()  'Play Skeletor_StopHim Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=100
	signon.imageb = ""
	Animation "Skeletor_Stophimyoufools_", 23, 1
	signon.visible = true
	VideoIntro.interval = 2070
	VideoIntro.enabled = true
	End If
	DOF 616, DOFPulse  'PuP - HeMAN Lit
End Sub
'
Sub Videos_Skeletor_Stunray()  'Play Skeletor_Stunray Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=90
	signon.imageb = ""
	Animation "Skeletor_Stunray_", 38, 1
	signon.visible = true
	VideoIntro.interval = 3420
	VideoIntro.enabled = true
	End If
	DOF 623, DOFPulse  'PuP - Skeletor Ball Locked
End Sub
'
Sub Videos_Snake_Mountain()  'Play Snake_Mountain Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=130
	signon.imageb = ""
	Animation "SnakeMountain_", 26, 1
	signon.visible = true
	VideoIntro.interval = 2440
	VideoIntro.enabled = true
	End If
	DOF 609, DOFPulse  'PuP - Ball 2 Locked
End Sub
'
Sub Videos_Sorceress_Fly()  'Play Sorceress_Fly Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If C1.state = LightStateOn then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=80
	signon.imageb = ""
	Animation "sorceress_fly_", 23, 1
	signon.visible = true
	VideoIntro.interval = 2070
	VideoIntro.enabled = true
	End If
	DOF 631, DOFPulse  'PuP - Sorceress Fly
End Sub
'
Sub Videos_Sorceress_Hover()  'Play Sorceress_Hover Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If C1.state = LightStateOn then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=80
	signon.imageb = ""
	Animation "sorceress_hover_", 24, 1
	signon.visible = true
	VideoIntro.interval = 2160
	VideoIntro.enabled = true
	End If
	DOF 632, DOFPulse  'PuP - Sorceress Hover
End Sub
'
Sub Videos_Sorceress_Swoop()  'Play Sorceress_Swoop Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If C1.state = LightStateOn then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=90
	signon.imageb = ""
	Animation "sorceress_swoop_", 22, 1
	signon.visible = true
	VideoIntro.interval = 1980
	VideoIntro.enabled = true
	End If
	DOF 633, DOFPulse  'PuP - Sorceress Swoop
End Sub
'
Sub Videos_Well_Well()  'Play Well_Well Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=70
	signon.imageb = ""
	Animation "wellwell_", 43, 1
	signon.visible = true
	VideoIntro.interval = 4400
	VideoIntro.enabled = true
	End If
	DOF 613, DOFPulse  'PuP - Ball 3 Locked - 2
End Sub
'
Sub Videos_HeMan_Cell()  'Play HeMan_Cell Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=80
	signon.imageb = ""
	Animation "HeMan_Cell_", 36, 1
	signon.visible = true
	VideoIntro.interval = 4500
	VideoIntro.enabled = true
	End If
	DOF 610, DOFPulse  'PuP - Ball 3 Locked
End Sub
'
Sub Videos_Road_Ripper_50000()  'Play Road_Ripper 50000 Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=80
	signon.imageb = ""
	Animation "Road_Ripper_", 37, 1
	signon.visible = true
	VideoIntro.interval = 3000
	VideoIntro.enabled = true
	End If
	DOF 621, DOFPulse  'PuP - Road Ripper
End Sub
'
Sub Videos_Road_Ripper_100000()  'Play Road_Ripper 100000 Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=80
	signon.imageb = ""
	Animation "Road_Ripper_", 37, 1
	signon.visible = true
	VideoIntro.interval = 3000
	VideoIntro.enabled = true
	End If
	DOF 621, DOFPulse  'PuP - Road Ripper
End Sub
'
Sub Videos_Road_Ripper_250000()  'Play Road_Ripper 250000 Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=80
	signon.imageb = ""
	Animation "Road_Ripper_", 37, 1
	signon.visible = true
	VideoIntro.interval = 3000
	VideoIntro.enabled = true
	End If
	DOF 621, DOFPulse  'PuP - Road Ripper
End Sub
'
Sub Videos_Skeletor_Palace()  'Play Skeletor_Palace Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=100
	signon.imageb = ""
	Animation "Skeletor_Palace_", 29, 1
	signon.visible = true
	VideoIntro.interval = 2610
	VideoIntro.enabled = true
	End If
	DOF 612, DOFPulse  'PuP - Ball 2 Locked - 2
End Sub
'
Sub Videos_Spinning_Sword()  'Play Spinning_Sword Video
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=50
	signon.imageb = ""
	Animation "spinningsword_", 32, 1
	signon.visible = true
	VideoIntro.interval = 1700
	VideoIntro.enabled = true
	End If
	DOF 618, DOFPulse  'PuP - MB End
End Sub
'
Sub Videos_Skeletor_GetOut()  'Play Skeletor_GetOut Video
	If DisablePlayfieldMovie=False Then
	movieSpeed=83
	signon.imageb = ""
	Animation "Skeletor_getOut_", 55, 1
	signon.visible = true
	VideoIntro.interval = 4800
	VideoIntro.enabled = true
	End If
	DOF 607, DOFPulse  'PuP - Game Over
End Sub
'
Sub Videos_Spinning_LOGO()  'Play Spinning_LOGO
	If (CastleMB_Mode_Active=True) or (SkeletorMB_Mode_Active=True) or (RoadRipperMB_Mode_Active=True) or (HeManMB_Mode_Active=True) then: Exit Sub
	If bBallSaverActive = True Then: Exit Sub
	If DisablePlayfieldMovie=False Then
	movieSpeed=50
	signon.imageb = ""
	Animation "Spinning_Logo_", 21, 1
	signon.visible = true
	VideoIntro.interval = 1200
	VideoIntro.enabled = true
	playsound "SpinningLOGO"
	End If
	DOF 507, DOFOn  'RGB Undercab - Main Mode
	DOF 619, DOFPulse  'PuP - New Ball
End Sub
'
Sub Videos_TILT()  'Play TILT
	CastleMB_Mode_Active=False
	SkeletorMB_Mode_Active=False
	RoadRipperMB_Mode_Active=False
	HeManMB_Mode_Active=False
	CastleMB_Loop.enabled=False
	SkeletorMB_Loop.enabled=False
	RoadRipperMB_Loop.enabled=False
	HeManMB_Loop.enabled=False
	If DisablePlayfieldMovie=False Then
	movieSpeed=70
	signon.imageb = ""
	Animation "Skeletor_tMB_", 51, 1
	signon.visible = true
	VideoIntro.interval = 3800
	VideoIntro.enabled = true
	End If
	DOF 634, DOFPulse  'PuP - TILT
End Sub

''---- MultiBall Videos ----
'
Sub Videos_Hey_Yay_Loop()  'Play the Hey_Yay (Castle MB) Video Looped
	If DisablePlayfieldMovie=False Then
	movieSpeed=50
	signon.imageb = ""
	Animation "Hey_Yay_", 118, 2
	signon.visible = true
	CastleMB_Loop.interval = 5800
	CastleMB_Loop.enabled=True
	End If
	DOF 605, DOFPulse  'PuP - Castle MB
End Sub

Sub Videos_Hey_Yay_FadeOut()  'Hey_Yay (Castle MB) Video FadeOut
	CastleMB_Loop.enabled=False
End Sub
'
Sub Videos_Skeletor_MB()  'Play the Skeletor_MB Video
	If DisablePlayfieldMovie=False Then
	movieSpeed=60
	signon.imageb = ""
	Animation "Skeletor_mb_", 103, 1
	signon.visible = true
	SkeletorMB_Loop_Delay.interval = 3000
	SkeletorMB_Loop_Delay.enabled=True
	End If
	DOF 630, DOFPulse  'PuP - Skeletor MB - Start
End Sub

Sub SkeletorMB_Loop_Delay_timer()
	SkeletorMB_Loop_Delay.enabled=False
	SkeletorMB_Loop.interval = 3000
	SkeletorMB_Loop.enabled=True
End Sub

Sub Videos_Skeletor_MB_FadeOut()  'Skeletor_MB Video FadeOut
	SkeletorMB_Loop.enabled=False
End Sub
'
Sub Videos_Ripper_MB()  'Play the Ripper_MB Video
	If DisablePlayfieldMovie=False Then
	movieSpeed=60
	signon.imageb = ""
	Animation "Ripper_mb_", 34, 1
	signon.visible = true
	RoadRipperMB_Loop_Delay.interval=2000
	RoadRipperMB_Loop_Delay.enabled=True
	End If
	DOF 636, DOFPulse  'PuP - RoadRipperMB - Start
End Sub

Sub RoadRipperMB_Loop_Delay_timer()
	RoadRipperMB_Loop_Delay.enabled=False
	'movieSpeed=1
	If DisablePlayfieldMovie=False Then
	signon.imageb = "RoadRipper_MB"
	End If
	DOF 635, DOFPulse  'PuP - RoadRipperMB
End Sub

Sub Videos_Ripper_MB_FadeOut()  'Ripper_MB Video FadeOut
	RoadRipperMB_Loop.enabled=False
End Sub
'
Sub Videos_HeMan_POG()  'Play He-Man "By the Power of Grayskull" Video
	If DisablePlayfieldMovie=False Then
	movieSpeed=50
	signon.imageb = ""
	Animation "HeMan_Pog_", 191, 1
	signon.visible = true
	End If
	DOF 617, DOFPulse  'PuP - POG
End Sub
'
Sub Videos_HeMan_POG_Stop()  'Stop HeMan_POG Video, Start He-Man MB
	If DisablePlayfieldMovie=False Then
	movieSpeed=40
	signon.imageb = ""
	Animation "HeMan_MB_", 11, 1
	signon.visible = true
	End If
	HeManMB_Loop.interval=300
	HeManMB_Loop.enabled=True
	DOF 637, DOFPulse  'PuP - HeMan MB
End Sub
'
Sub Videos_HeMan_MB_Stop()  'Fade Out HeMan_MB Video
	HeManMB_Loop.enabled=false
End Sub

'*** VPX Video Timers ****

Dim Hname, Hsteps, Hloops, Hpos, posinc

Sub holotimer_timer()
	Dim imagename
	HPos=(HPos+posinc)' mod "imagename"(1)
	if HPos = 0 then HPos = 1
	if HPos < 10 then
		imagename = Hname & "00" & Hpos
	elseif HPos < 100 then
		imagename = Hname & "0" & Hpos
	else
		imagename = Hname & Hpos
	end if
	signon.imagea = imagename
end Sub

Sub VideoIntro_timer()
	If DisablePlayfieldMovie=True Then Exit Sub: End If
	holotimer.enabled = False
	signon.visible = true
	me.enabled = False
	signon.imageb = "logo 006"
	signon.imagea = ""
End Sub

Sub Videoboob_timer()
	If DisablePlayfieldMovie=True Then Exit Sub: End If
	holotimer.enabled = False
	signon.visible = true
	me.enabled = False
End Sub

Sub CastleMB_Loop_timer()
	If DisablePlayfieldMovie=True Then Exit Sub: End If
	movieSpeed=50
	Animation "Hey_Yay_", 118, 2
End Sub

Sub SkeletorMB_Loop_timer()
	If DisablePlayfieldMovie=True Then Exit Sub: End If
	movieSpeed=70
	Animation "Skeletor_tMB_", 51, 1
End Sub

Sub RoadRipperMB_Loop_timer()
	
End Sub

Sub HeManMB_Loop_timer()
	If DisablePlayfieldMovie=True Then Exit Sub: End If
	movieSpeed=40
	Animation "HeMan_MB_", 11, 174
End Sub

Sub Animation(name, numframes, loops)
	If DisablePlayfieldMovie=True Then Exit Sub: End If
	HSteps = numframes
	HPos=0
	Hname = name
	Hloops = loops
	posinc = 1
    holotimer.interval = movieSpeed
	holotimer.enabled=1
End Sub

Sub attractdelay_Timer
	If DisablePlayfieldMovie=True Then Exit Sub: End If
  Animation AttractVideo(0), AttractVideo(1), 0
  
End Sub


'####### Videos Section Ends ##########

On Error Resume Next
ExecuteGlobal GetTextFile("core.vbs")
If Err Then MsgBox "Can't open core.vbs"
On Error Goto 0

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0


' *********************************************************************
' **                                                                 **
' **               Future Pinball Defined Script Events              **
' **                                                                 **
' *********************************************************************


' The Method Is Called Immediately the Game Engine is Ready to
' Start Processing the Script.2
'
Sub MOTU_init()
	signon.imageb = ""
	attractdelay.Interval = 2790
	attractdelay.Enabled = 1

	If DisablePlayfieldMovie=True Then
		Primitive5.visible=False
		signon.visible=False
	End If

LoadUltraDMD
If ScoreType>1 Then
	Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay") 
	LoadPuPlayer
End If
LoadEm
Loadhs	
CastleDown.Interval = 20
CastleDown.Enabled = True
RotonBlattTimer.Enabled = False
stopSound "ROTONloop"
BankUP.interval = 60
BankUP.enabled = True
kickback.Pullback
attractseq.Play SeqArcBottomLeftUpOn ,20,2:attractseq.Play SeqArcBottomrightUpOn ,20,2
attractseq.Play SeqArcBottomLeftUpOn ,20,20:attractseq.Play SeqArcBottomrightUpOn ,20,20
'hologram.frame 125, 155, 125
Plungerbulb.state = 0
Plungerbulbfv.visible = false
plungerbulbbulb.blenddisablelighting = 0
bulb12.state = 0
bulb12fv.visible = false
bulb8.state = 0
bulb8fv.visible = false
bulb11.state = 0
bulb11fv.visible = false
bulb14.state = 0
bulb14fv.visible = false
	DMD_CancelRendering
	DMD_DisplayScene "MASTERS OF","THE UNIVERSE", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
''DispDmd1.QueueText "[f3][xc][y5]MASTERS OF[y18]THE UNIVERSE", deFlip, 10000, True
''DispDmd2.QueueText "[f3][xc][y5]MASTERS OF[y18]THE UNIVERSE", deFlip, 10000, True
	' seed the randomiser (rnd(1) function)
	Randomize

	' initalise the player display reel to the last known player scores

	' We want the player to put in credits (Change this
	bFreePlay = FALSE

	' kill the last switch hit (this this variable is very usefull for game control)
	set LastSwitchHit = DummyTrigger

	' initialse any other flags
	bOnTheFirstBall = FALSE
	bEnteringAHighScore	= FALSE
	BallsOnPlayfield = 0
	BallsInLock	= 0

	EndOfGame()
End Sub


' This Method is Called when the user has exited the Game Player. It should
' save any RAM variables that need to be persistant.
'
Sub FuturePinball_EndPlay()

End Sub
'******************************* DMD *****************************************************


'dmd tHIngS
Dim firstframe
Dim lastframe
Dim Font
Dim Dmdinuse
dim speed
dim hold
dim textstring
dim dmdstring

'Every time this timer expired add 1 to the font, and continue runing untill it reach the last frame
sub TimerDMD_Timer()
	'AddDebugText ""&firstframe
	TimerDMD.Enabled = false
	firstframe = firstframe + 1
	dmdstring = "[f"+cstr(Font)+"][xc][yc]" &chr(firstframe) &"[f1] "&textstring
	'DispDmd1.Text=dmdstring
	'DispDmd2.Text=dmdstring

	if (firstframe => lastframe) then
		TimerDMD.Enabled = false
		Timeraddzero.Interval = hold
		Timeraddzero.Enabled = true
		exit sub
	end if
	TimerDMD.Interval = speed
	TimerDMD.Enabled = True
end sub

'Release the DMD, show the score
sub Timeraddzero_Timer
		textstring="[f1] "
		Timeraddzero.Enabled = False
		Dmdinuse=false
		addscore(0)
end sub

' Some extra functions for formating the score etc
function RandomNumber(ByVal max)
	RandomNumber = Int(max * Rnd + 1)
end function


Function FormatScore(num)
    Dim n, f, s
    n = CStr(num)
    f = ""

    do while len(n)>3
        if len(f)>0 then
            f = Right(n, 3) & "," & f
        else
            f = Right(n, 3)
        end if
        n = Left(n, Len(n)-3)
    loop
    if len(n)>0 then
        if len(f) > 0 then
            f = n & "," & f
        else
            f = n
        end if
    end if
    FormatScore = f
End Function

'===========================================================================================================

sub flushdmdtimer_Timer()
scoreupdate = true
addscore(0)
flushdmdtimer.Enabled = false
end sub

'**************************************************************************

Dim nvhighscorename
sub DmdAttractTimer_Timer
		DmdAttractTimer.enabled=false
		TimerDMD.Enabled = false
		Timeraddzero.Enabled = False
		atractcount=atractcount+1
      scoreupdate = false
If (vpGameInPlay = false) Then
	select case (atractcount)
		case 1
         'playmusic "motu_intro.mp3"
			myPlayMusicForMode(4)
			Videos_MOTU_Intro()   ' PuP - MOTU Intro - Attract
			DmdAttractTimer.Interval =7600
			DmdAttractTimer.Enabled =7600
			
		case 2
			DmdAttractTimer.Interval =3800
			DmdAttractTimer.Enabled =3800
			
		case 3
			DmdAttractTimer.Interval =3800
			DmdAttractTimer.Enabled =3800
			
		case 4
			DmdAttractTimer.Interval =3800
			DmdAttractTimer.Enabled =3800
			
		case 5
			DmdAttractTimer.Interval =3800
			DmdAttractTimer.Enabled =3800
			
		case 6
			DmdAttractTimer.Interval =3800
			DmdAttractTimer.Enabled =3800
			
		case 7
			DmdAttractTimer.Interval =3800
			DmdAttractTimer.Enabled =3800
			
		case 8
			DmdAttractTimer.Interval =3800
			DmdAttractTimer.Enabled =3800
			
		case 9
			DmdAttractTimer.Interval =3800
			DmdAttractTimer.Enabled =3800
			
		case 10
			DmdAttractTimer.Interval =3800
			DmdAttractTimer.Enabled =3800
			
		case 11
			DmdAttractTimer.Interval =3800
			DmdAttractTimer.Enabled =3800
			
		case 12
			DmdAttractTimer.Interval =3800
			DmdAttractTimer.Enabled =3800
			
		case 13
			DmdAttractTimer.Interval =3800
			DmdAttractTimer.Enabled =3800
			
		case 14
			DmdAttractTimer.Interval =3800
			DmdAttractTimer.Enabled =3800
			
		case 15
			DmdAttractTimer.Interval =3800
			DmdAttractTimer.Enabled =3800
			
		case 16
			DmdAttractTimer.Interval =3800
			DmdAttractTimer.Enabled =3800
			
		case 17
			DmdAttractTimer.Interval =3800
			DmdAttractTimer.Enabled =3800
			
		case 18
			'playmusic "ATTRACTMUSIC.mp3"
			myPlayMusicForMode(1)
			DmdAttractTimer.Interval =2500
			DmdAttractTimer.Enabled =2500
				DMD_CancelRendering
				DMD_DisplayScene "HEMAN  "& HighScorename(0),""& HighScore(0), UltraDMD_Animation_None, 3000, UltraDMD_Animation_None
				'DispDmd1.Text ="[f3][y4]HEMAN[f3][y20]" & nvhighscorename(1) & " - " & formatscore(nvhighscore(1))
				'DispDmd2.Text ="[f3][y4]HEMAN[f3][y20]" & nvhighscorename(1) & " - " & formatscore(nvhighscore(1))
		case 19
			DmdAttractTimer.Interval =2500
			DmdAttractTimer.Enabled =2500
				DMD_CancelRendering
				DMD_DisplayScene "Skeletor  "& HighScorename(1),""& HighScore(1), UltraDMD_Animation_None, 2500, UltraDMD_Animation_None
				'DispDmd1.Text ="[f3][y4]SKELETOR[f3][y20]" & nvhighscorename(2) & " - " & formatscore(nvhighscore(2))
				'DispDmd2.Text ="[f3][y4]SKELETOR[f3][y20]" & nvhighscorename(2) & " - " & formatscore(nvhighscore(2))
		case 20
			DmdAttractTimer.Interval =2500
			DmdAttractTimer.Enabled =2500
				DMD_CancelRendering
				DMD_DisplayScene "evil lyn  "& HighScorename(2),""& HighScore(2), UltraDMD_Animation_None, 2000, UltraDMD_Animation_None
				'DispDmd1.Text ="[f3][y4]EVIL-LYNN[f3][y20]" & nvhighscorename(3) & " - " & formatscore(nvhighscore(3))
				'DispDmd2.Text ="[f3][y4]EVIL-LYNN[f3][y20]" & nvhighscorename(3) & " - " & formatscore(nvhighscore(3))
      case 21
			DmdAttractTimer.Interval =2500
			DmdAttractTimer.Enabled =2500
				DMD_CancelRendering
				DMD_DisplayScene "orko  "& HighScorename(3),""& HighScore(3), UltraDMD_Animation_None, 2000, UltraDMD_Animation_None
				'DispDmd1.Text ="[f3][y4]ORKO[f3][y20]" & nvhighscorename(4) & " - " & formatscore(nvhighscore(4))
				'DispDmd2.Text ="[f3][y4]ORKO[f3][y20]" & nvhighscorename(4) & " - " & formatscore(nvhighscore(4))
      case 22
			DmdAttractTimer.Interval =2500
			DmdAttractTimer.Enabled =2500
				DMD_CancelRendering
				DMD_DisplayScene "LAST GAME  ",""& (nvScore(CurrentPlayer)), UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
				'DispDmd1.Text ="[f3][y4]LAST PLAYER" & "[f3][y20]" & FormatNumber(nvScore(CurrentPlayer), 0, -1, 0, -1)
				'DispDmd2.Text ="[f3][y4]LAST PLAYER" & "[f3][y20]" & FormatNumber(nvScore(CurrentPlayer), 0, -1, 0, -1)
		case 23
			DmdAttractTimer.Interval =2500
			DmdAttractTimer.Enabled =2500
				DMD_CancelRendering
				DMD_DisplayScene "FP original by:","ROM", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
				'DispDmd1.Text ="[f3][y4]Created By:" & "[f3][y20]ROM"
				'DispDmd2.Text ="[f3][y4]Created By:" & "[f3][y20]ROM"
      case 24
			DmdAttractTimer.Interval =2500
			DmdAttractTimer.Enabled =2500
				DMD_CancelRendering
				DMD_DisplayScene "","", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
				'DispDmd1.Text ="[f3][y4]DMD mod:" & "[f3][y20] HYPERION"
				'DispDmd2.Text ="[f3][y4]DMD mod:" & "[f3][y20] HYPERION"
      case 25
			DmdAttractTimer.Interval =2500
			DmdAttractTimer.Enabled =2500

				'DispDmd1.Text ="[f3][y4]Mastered Edition:" & "[f3][y20]TERRYRED"
				'DispDmd2.Text ="[f3][y4]Mastered Edition:" & "[f3][y20]TERRYRED"
      case 26
			DmdAttractTimer.Interval =2500
			DmdAttractTimer.Enabled =2500
				'DispDmd1.Text ="[f3][y4]' ' DOFLinx MX mod:" & "[f3][y20]TERRYRED"
				'DispDmd2.Text ="[f3][y4]' ' DOFLinx MX mod:" & "[f3][y20]TERRYRED"
		case 27
			TimerDMD.Enabled = false
			Timeraddzero.Enabled = false
			DmdAttractTimer.Interval =3000
			DmdAttractTimer.Enabled =3000
         atractcount=0
			IF nvCredits=0 THEN
				DMD_DisplayScene "CREDITS","INSERT COINS" &nvCredits, UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
				'DispDmd1.Text ="[edge4][b][f3][Xc][y3]CREDITS "&(nvCredits)&"[f3][xc][y20]INSERT COINS[/b]"
				'DispDmd2.Text="[edge4][b][f3][Xc][y3]CREDITS "&(nvCredits)&"[f3][xc][y20]INSERT COINS[/b]"
			END IF
			IF nvCredits>0 THEN
				DMD_DisplayScene "","CREDITS" &nvCredits, UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
				'DispDmd1.Text ="[b][f5][Xc][Yc]CREDITS "&(nvCredits)
				'DispDmd2.Text="[b][f5][Xc][Yc]CREDITS "&(nvCredits)
			END IF
	end select
end if
end sub

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    PlaySoundAtVol SoundFX("fx_sling",DOFContactors), sling1, VolSling
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	' add some points
	AddScore(500)
	' flash the lights around the slingshot
	FlashForMs RightSlingshotBulb1, 100, 50, LightStateon
	FlashForMs RightSlingshotBulb2, 100, 50, LightStateOn
	FlashForMs Flasher5f, 100, 50, LightStateOff
	DOFLinx_Right_Slingshot_Hit() '' ' DOFLinx - Right Slingshot Hit
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0':gi1.State = 1:Gi2.State = 1
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
    PlaySoundAtVol SoundFX("fx_sling",DOFContactors), sling2, VolSling
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	' add some points
	AddScore(500)
	' flash the lights around the slingshot
	FlashForMs LeftSlingshotBulb1, 100, 50, LightStateOn
	FlashForMs LeftSlingshotBulb2, 100, 50, LightStateOn
	FlashForMs Flasher1f, 100, 50, LightStateOff
	DOFLinx_Left_Slingshot_Hit() '' ' DOFLinx - Left Slingshot Hit
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0':gi3.State = 1:Gi4.State = 1
    End Select
    LStep = LStep + 1
End Sub

' The User Has Pressed A Key on the Keyboard..
'
Sub MOTU_KeyDown(ByVal KeyCode)
	'AddDebugText "KeyCode = " & KeyCode
	'AddDebugText "Titled = " & fpTilted
	If (keycode = 48) Then ' in case of EMERGENCY press B if a ball is ABSOLUTELY lost and the PLUNGER CREATES NO NEW BALL
 		PlungerKicker.kick 90, 10
	End If


	If (keycode = 37) Then ' in case of EMERGENCY press K if a ball is ABSOLUTELY lost and the PLUNGER CREATES NO NEW BALL
 		SkeletorLock.solenoidpulse
		DOFLinx_SkeletorLock() '' ' DOFLinx - SkeletorLock
		HeManKicker.solenoidpulse
		DOFLinx_HeManKicker() '' ' DOFLinx - HeManKicker
		PlungerKicker.kick 90, 10
		DOFLinx_PlungerKicker() '' ' DOFLinx - PlungerKicker
		Kickback.solenoidpulse
		DOFLinx_Kickback() '' ' DOFLinx - Kickback
		Rubber20.solenoidpulse
		Rubber21.solenoidpulse
	End If

	' Process any keys which are valid at all times

	' The Player has Inserted a Coin
	If (KeyCode = AddCreditKey) or (KeyCode = AddCreditKey2) Then
		PlaySoundAtVol "CoinIn", Drain, 1
		PlaySound "Myah!"
		DOFLinx_Coin_Inserted()        '' ' DOFLinx: Coin Inserted
		Videos_CoinInserted()   'PuP - Coin Inserted
		nvCredits = nvCredits + 1
		'AddDebugText "Insert Coin, Credits = " & nvCredits
		' If not Tilted, then give some feed back to the player
		If (vpTilted = FALSE) Then
				DmdAttractTimer.Enabled = false
				TimerDMD.Enabled = false
				Timeraddzero.Enabled = False
				atractcount=0
				DmdAttractTimer.Interval = 10000
				DmdAttractTimer.Enabled =true
				DMD_CancelRendering
				DMD_DisplayScene "CREDITS","PRESS START" &nvCredits, UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
				pDMDSplashLines "CREDITS" & nvCredits,"PRESS START", 5, LabelYellow
				'DispDmd1.Text="[f3][Xc][Y3]CREDITS "&(nvCredits) & "[b][f4][xc][y17]PRESS START[/b]"
				'DispDmd2.Text="[f3][Xc][Y3]CREDITS "&(nvCredits) & "[b][f4][xc][y17]PRESS START[/b]"
			' Flash the Slingshot Bulbs. This is rather Common on Pinball machines
			' though you can do anything at this point
			FlashForMs LeftSlingshotBulb1, 200, 100, LightStateOff
			FlashForMs LeftSlingshotBulb2, 200, 100, LightStateOff
			FlashForMs RightSlingshotBulb1, 200, 100, LightStateOff
			FlashForMs RightSlingshotBulb2, 200, 100, LightStateOff
			' Maybe play a sound at this point or put something on the display
			DOFLinx_CreditsIn_StartFlash() '' ' DOFLinx: Credits In - Coin Button On, Start Button Flashing
		End If
	End If

	If (KeyCode = PlungerKey) Then

		If (vpGameInPlay = TRUE) Then
			' and not Tilted ?
				If (vpTilted = FALSE) Then
						'Plunger.solenoidpulse()
						PlaySoundAtVol "fx_PlungerRelease", Plunger, 1
						Plunger.kick 0,35
						'PlungerState(1)
						DOFLinx_AutoPlunger() '' ' DOFLinx - AutoPlunger
				end if
		end if

	End If

	' Process The Next set of keys depeding of wether there is a game in progress or not
	' Is A Game in Progress?
	If (vpGameInPlay = TRUE) Then
		' and not Tilted ?
		If (vpTilted = FALSE) Then

			' If the Left Flipper Key Has Been Press, Activate The Left Flipper(s)
			If (KeyCode = LeftFlipperKey) Then
				LeftFlipper.RotateToend
				DOFLinx_Left_Flipper_On() '' ' DOFLinx - Left Flipper On
				Videos_LeftFlipper()  'PuP - Left Flipper Video
            Flipper3.RotateToend
            PlaySoundAtVol "fx_flipup", Flipper3, VolFlip
			PlaySoundAtVol "fx_flipup", LeftFlipper, VolFlip
			End If

			' If the Right Flipper Key Has Been Press, Activate The Right Flipper(s)
			If (KeyCode = RightFlipperKey) Then
				RightFlipper.RotateToend
				DOFLinx_Right_Flipper_On() '' ' DOFLinx - Right Flipper On
				Videos_RightFlipper()  'PuP - Right Flipper Video
				PlaySoundAtVol "fx_flipup", RightFlipper, VolFlip
			End If

			' Another player starting?
			If (KeyCode = StartGameKey) or (KeyCode = AddCreditKey2) Then
				' must be less than max players and must be on the the very first ball of the game
				If ((PlayersPlayingGame < constMaxPlayers) And (bOnTheFirstBall = TRUE)) Then
					' free play or credits
					If (bFreePlay = TRUE) Then
						DOFLinx_CoinOn_StartOn()       '' ' DOFLinx: Game in Play - Coins & Start Button On
						' add the extra player
						PlayersPlayingGame = PlayersPlayingGame + 1
						'AddDebugText "Extra Player (Free Play)" & PlayersPlayingGame
						' Update the stats
						nvTotalGamesPlayed = nvTotalGamesPlayed + 1
					Else
						If (nvCredits > 0) then
							PlayersPlayingGame = PlayersPlayingGame + 1
							'AddDebugText "Extra Player (Credits)" & PlayersPlayingGame
							' update the stats
							nvTotalGamesPlayed = nvTotalGamesPlayed + 1
							nvCredits = nvCredits - 1
							DOFLinx_CoinOn_StartOn()       '' ' DOFLinx: Game in Play - Coins & Start Button On
							DMD_DisplayScene "Player","2", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
						Else
							' Not Enough Credits to start a game.
							' Maybe Play a Sound or put "Insert Coin" on a display
							DOFLinx_NoPay_NoPlay()         '' ' DOFLinx: NoPay NoPlay
							Videos_Skeletor_Boob()  'Play Skeletor_Patience Video
							'attractdelay.Enabled = false
							PlaySound "YouRoyalBoob"
							'attractdelay.Enabled = true
						End If
					End If
				End If
			End If
		End If ' If (fpTilted)

	Else	' If (fpGameInPlay)

		' there isn't a game in play (we are in the attract mode)
      ' wait for the start key to be pressed and start a new game
      If (KeyCode = StartGameKey) Then
			' Free Play Mode ?
			If (bFreePlay = TRUE) Then
				' Yep.  Only allow a game to Start if there are no balls on the playfield.
				' This can happen if the player starts a game too quick after finishing the
				' last game and the machine is ejecting any locked balls.
				If (BallsOnPlayfield = 0) Then
					' A Game is now in progress, reset the table for a new Game
					ResetForNewGame()
					DOFLinx_CoinOn_StartOn()       '' ' DOFLinx: Game in Play - Coins & Start Button On
				End If
			Else
				' paying by credits, do we have any ?
				If (nvCredits > 0) Then
					' Yep.  Only allow a game to start if there are no balls on the playfield
					If (BallsOnPlayfield = 0) Then
						' remove a credit
						nvCredits = nvCredits - 1
						' much the same as above
						ResetForNewGame()
						DOFLinx_CoinOn_StartOn()       '' ' DOFLinx: Game in Play - Coins & Start Button On
					End If
				Else
					' Not Enough Credits to start a game.
					' Maybe Play a Sound or put "Insert Coin" on a display
					DOFLinx_NoPay_NoPlay()         '' ' DOFLinx: NoPay NoPlay
					Videos_Skeletor_Boob()  'Play Skeletor_Patience Video
					'attractdelay.Enabled = false
					PlaySound "YouRoyalBoob"
					'attractdelay.Enabled = true
				End If
			End If
		End If

	End If ' If (fpGameInPlay)
	If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If
If keycode = LeftTiltKey Then
		Nudge 90, 2
		checktilt
	End If
    
	If keycode = RightTiltKey Then
		Nudge 270, 2
		checktilt
	End If
    
	If keycode = CenterTiltKey Then
		Nudge 0, 2
		checktilt
	End If
End Sub


' The User Has Released A Key on the Keyboard..
'
Sub MOTU_KeyUp(ByVal KeyCode)

	' Process any keys which are valid at all times

	' The Player has released the Plunger, Let it go..
	If (KeyCode = (PlungerKey)) and (VpGameInPlay = TRUE) Then
		'Plunger.solenoidpulse
		'PlungerState(0)
	End If

	' Process The Next set of keys depeding of wether there is a game in progress or not

	' Is A Game in Progress?
	If (vpGameInPlay = TRUE) Then
		' and not tilted
		If (vpTilted = FALSE) Then

			' The Left Flipper Key has been released, Turn Off the Left Flipper(s)
			If (KeyCode = LeftFlipperKey) Then
				LeftFlipper.RotateToStart
				DOFLinx_Left_Flipper_Off() '' ' DOFLinx - Left Flipper Off
            Flipper3.RotateToStart
			PlaySoundAtVol "fx_FlipperDown", Flipper3, VolFlip
			PlaySoundAtVol "fx_FlipperDown", LeftFlipper, VolFlip
			End If

			' The Right Flipper Key has been released, Turn Off the Right Flipper(s)
			If (KeyCode = RightFlipperKey) Then
				RightFlipper.RotateToStart
				DOFLinx_Right_Flipper_Off() '' ' DOFLinx - Right Flipper Off
				PlaySoundAtVol "fx_FlipperDown", RightFlipper, VolFlip
			End If
		End If
	End If
End Sub

Sub PlungerState(state)
	Dim Blinking, Solid
	If state=1 or state=2 or state=3 then
		Blinking = LightStateBlinking
		Solid = LightStateOn
		if state = LightStateOn Then
			if bBallInPlungerLane = TRUE Then PlaySoundAtVol "fx_PlungerPull", Plunger, 1
		else
			PlaySound "fx_PlungerLetGo"
			autoTimer.Enabled=True
			CreateSaveBallTimer.Enabled = True
		end if
	else
		Solid = LightStateOff
		Blinking = lightstateblinking
		Plunger.kick 0, 35
		PlaySoundAtVol "fx_PlungerRelease", Plunger, 1

		Plunger.timerenabled=0
	End If

End Sub


' The User Has Paused The Game..
'
Sub MOTU_Paused()
End Sub


' The User Has UnPaused (Resumed) The Game..
'
Sub MOTU_UnPaused()
End Sub

Sub AllPlasOff()
	LightSeqGI.StopPlay
	LightSeqGI.play SeqAllOff
End Sub


Sub AllPlasOn()
	LightSeqGI.StopPlay
	LightSeqGI.play SeqAllOn
End Sub

' The Played has Nudged the Table a little too hard/much and a Warning
' must be given to the player
'
Sub MOTU_TiltWarning(ByVal Warnings)
	'AddDebugText "Tilt Warning" & Warnings
PlaySound "fx_Nudge"
playsound "blunderingfool"
DOFLinx_TILT_Warning() ' DOFLinx - Tilt Warning
DMD_CancelRendering
DMD_DisplayScene "","WARNING", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashBig "WARNING!", 2.5,LabelWhite
'DispDmd1.QueueText "[bf][box1,1,1,128,32][/bf][f4][xc][yc]WARNING", denone, 1600, True
'DispDmd2.QueueText "[bf][box1,1,1,128,32][/bf][f4][xc][yc]WARNING", denone, 1600, True
flushdmdtimer.Interval = 1600
flushdmdtimer.Enabled = True
	' play a sound at this point and put something on a display
End Sub


' The Player has tilted the machine (Too Many Warnings)
'
Dim TiltSens

Sub CheckTilt
If Tilttimer.Enabled = True and vpGameInPlay = TRUE and VPTilted = False Then 
		TiltSens = TiltSens + 1
		if TiltSens = 3 Then
			vpTilted = True
DOFLinx_TILTED()  ' DOFLinx - Tilted
	Videos_TILT()  'Play TILT
	'AddDebugText "**Tilted**"
	' play a sound
	PlaySound "Tilt"
	playsound "tilt_triumph"
	AllPlasOff()
	DMD_CancelRendering
	DMD_DisplayScene "","tilt", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
	pDMDSplashBig "TILT!", 2.5,LabelWhite
	'DispDmd1.AddFont 8, "tilt"
	'DispDmd2.AddFont 8, "tilt"
	LeftFlipper.RotateToStart
	DOFLinx_Left_Flipper_Off() '' ' DOFLinx - Left Flipper Off
	RightFlipper.RotateToStart
	DOFLinx_Right_Flipper_Off() '' ' DOFLinx - Right Flipper Off
	Flipper3.RotateToStart
	shootagainlight.state = LightStateOff
	TiltRecoveryTimer.Interval = 7000
	TiltRecoveryTimer.Enabled	= TRUE
	flushdmdtimer.Interval = 7000
	flushdmdtimer.Enabled = True

		Else	
			PlaySound "fx_Nudge"
			playsound "blunderingfool"
			DOFLinx_TILT_Warning() ' DOFLinx - Tilt Warning
			DMD_CancelRendering
			if TiltSens = 1 Then
				DMD_DisplayScene "","You Boob...", UltraDMD_Animation_ScrollOnUp, UltraDMD_deOn, UltraDMD_Animation_None
				pDMDSplashBig "You Boob...", 4,LabelYellow
			ElseIf TiltSens = 2 Then
				DMD_DisplayScene "You Blundering","Fool", UltraDMD_Animation_ScrollOnUp, UltraDMD_deOn, UltraDMD_Animation_None
				pDMDSplashLines "You Blundering","Fool", 4,LabelYellow
			Else
				DMD_DisplayScene "Easy","Muscle Head ...", UltraDMD_Animation_ScrollOnUp, UltraDMD_deOn, UltraDMD_Animation_None
				pDMDSplashLines "Easy","Muscle Head", 4,LabelYellow
			End If
			Tilttimer.enabled = False	
			Tilttimer.enabled = True
		End If
	Else
		TiltSens = 0
		Tilttimer.Enabled = True
	End If
		
	' ensure that the flippers are down (as the keys won't work from now on)
	' you may wish to turn off any lights at this point. (The Light Sequencer
	' will make this very easy)
	' start the tilt recovery timer which waits until all balls have drained
	' before doing the end of ball sequence (or end of game)

End Sub

Sub Tilttimer_Timer()
	Plunger.kick 0, 35
	Tilttimer.Enabled = False
End Sub

'' A Music Channel has finished Playing.
''
'' Channel is set to the channel number that has finished.
''
Sub FuturePinball_MusicFinished(ByVal Channel)
End Sub


' High Score entry has been completed by the player.
'
' Position is set to the position in the high score table (1 - 4)
' if it is set to 0 then there was no new High Score
'
' Special is set to 1 if the Special High Score was beaten
'
Sub FuturePinball_NameEntryComplete(ByVal Position, ByVal Special)
	' has the player beaten a high score
	'If (Position <> 0) Then
		' maybe award something (like an extra credit) based on the high score position
	'End If

	' has the player beaten the special score (if applicable)
	'If (Special <> 0) Then
	'End If

	'bEnteringAHighScore = FALSE

	' check to see if there are any more players playing to the current game
	'EndOfBallComplete()
End Sub



' *********************************************************************
' **                                                                 **
' **                     User Defined Script Events                  **
' **                                                                 **
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
If ScoreType>1 Then
	PuPlayer.LabelShowPage pDMD,1,0,""
End If
attractdelay.Enabled = 0
DmdIntro.enabled = False
PUPIntro.enabled = False

Plungerbulb.state = 0
Plungerbulbfv.visible = false
plungerbulbbulb.blenddisablelighting = 0
bulb12.state = 0
bulb12fv.visible = false
bulb8.state = 0
bulb8fv.visible = false
bulb11.state = 0
bulb11fv.visible = false
bulb13.state = 0
bulb13fv.visible = false
BulbLock4.blenddisablelighting = 0
BulbLock5.blenddisablelighting = 0
bulb14.state = 0
bulb14fv.visible = false
ModulokHitBulb.state = LightStateOn
ModulokHitBulb2.state = LightStateOn

	'playmusic "GAMEPLAY music.mp3"', true,0.6
	myPlayMusicForMode(2)
  DmdAttractTimer.enabled=false

	'Skeletor Roton Reset*********
skeletorhitbulb.state = LightStateOn
Bulb8.state = LightStateOff
flashbulb5t.Enabled = False
S1.state = LightStateBlinking
S2.state = LightStateBlinking
S3.state = LightStateBlinking
S4.state = LightStateOff
BankUP.Interval =  60
BankUP.Enabled =  true

	'Castle Reset*********
CastleControl.state = LightStateOff
CASTLEcollidewall.collidable = false
CASTLEcollidewall2.collidable = false
C1.state = LightStateBlinking
C2.state = LightStateOff
C3.state = LightStateOff
C4.state = LightStateOff
C5.state = LightStateOff
bulb11.state = LightStateOff
flashbulb4t.Enabled = False
CastleDown.Interval = 20
CastleDown.Enabled = True
bulb13.state = LightStateOff
flashbulb3t.enabled = False
BulbLock4.blenddisablelighting = 0
BulbLock5.blenddisablelighting = 0
bulb14.state = LightStateOff
flashbulb3t.enabled = False
diffquoteC.state = LightStateOff


	'Extras Reset*********
EB1.state = LightStateOff
EB2.state = LightStateOff
ExtraBall.state = LightStateOff
SpecialLeft.state = LightStateOn
SpecialRight.state = LightStateOff
SpecialLeft2.state = LightStateOff
SpecialRight2.state = LightStateOff
KickBackDiverter.IsDropped = true



	'RoadRipper Reset*********
WheelieOn.state = LightStateOn
R1.state = LightStateBlinking
R2.state = LightStateOff
R3.state = LightStateOff
R4.state = LightStateOff

' reset HEMAN
H1.state = LightStateBlinking
H2.state = LightStateOff
H3.state = LightStateOff
H4.state = LightStateOff
H5.state = LightStateOff
bulb12.state = LightStateOff
flashbulb2t.enabled = false
diffquoteHM.state = LightStateOff

attractseq.stopplay

	Dim	i

	'AddDebugText "ResetForNewGame"

	' get Future Pinball to zero out the nvScore (and nvSpecialScore) Variables
	' aswell and ensure the camera is looking in the right direction.
	' this also Sets the fpGameInPlay flag
	BeginGame()

scoreupdate = true

	' increment the total number of games played
   nvTotalGamesPlayed = nvTotalGamesPlayed + 1

	' Start with player 1
	CurrentPlayer = 1

	' Single player (for now, more can be added in later)
	PlayersPlayingGame = 1

	' We are on the First Ball (for Player One)
	bOnTheFirstBall = TRUE

	' initialise all the variables which are used for the duration of the game
	' (do all players incase any new ones start a game)
	For i = 1 To constMaxPlayers
		' Bonus Points for the current player
		BonusPoints(i)	= 0
		' Initial Bonus Multiplier
		BonusMultiplier(i) = 1
		' Balls Per Game
		BallsRemaining(i) = nvBallsPerGame
		' Number of EB's out-standing
		ExtraBallsAwards(i) = 0
	Next

	' initialise any other flags
	bMultiBallMode = FALSE

	' you may wish to start some music, play a sound, do whatever at this point

	' set up the start delay to handle any Start of Game Attract Sequence
	FirstBallDelayTimer.Interval = 500
	FirstBallDelayTimer.Enabled = TRUE
End Sub

Sub BeginGame()
	AllPlasOn()
	vpTilted=FALSE
	VpGameInPlay = TRUE
	nvScore(0)=0
	nvScore(1)=0
	nvScore(2)=0
	nvScore(3)=0

End Sub

' This Timer is used to delay the start of a game to allow any attract sequence to
' complete.  When it expires it creates a ball for the player to start playing with
'
Sub FirstBallDelayTimer_Timer()
	' stop the timer
	FirstBallDelayTimer.Enabled = FALSE

	' reset the table for a new ball
	ResetForNewPlayerBall()

	' create a new ball in the shooters lane
	CreateNewBall()
End Sub


' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))
'
Sub ResetForNewPlayerBall()

scoreupdate = true

SpecialLeft.state = LightStateOn
Kickbackdiverter.Isdropped = true
kickback.Pullback
flashbulb1t.enabled = True
KickBackDiverter.IsDropped = true
'plungerbulb.state = LightStateBlinking

	' make sure the correct display is upto date
	AddScore(0)

	' set the current players bonus multiplier back down to 1X
	SetBonusMultiplier(1)

	' reset any drop targets, lights, game modes etc..
	ShootAgainLight.State = LightStateOff
End Sub


' Create a new ball on the Playfield
'
Sub CreateNewBall()
		
				DMD_CancelRendering
				DMD_DisplayScene "PLAYER",""& currentplayer, UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
				pDMDZoomBig "Player "& CurrentPlayer, 2,LabelWhite
	'			'DispDmd1.Text ="[edge4][f4][xc][yc][b]PLAYER "& currentplayer
	'			'DispDmd2.Text ="[edge4][f4][xc][yc][b]PLAYER "& currentplayer

			flushdmdtimer.interval = 2000
			flushdmdtimer.Enabled = true
	 'end if

	' create a ball in the plunger lane kicker.
	PlungerKicker.CreateBall
	PlaySoundAtVol "fx_BallLaunch", Plunger, 1
	PlungerKicker.kick 90, 10
	' There is a (or another) ball on the playfield
	BallsOnPlayfield = BallsOnPlayfield + 1

	' kick it out..
	Plunger.kick 0, 35
	PlaySoundAtVol "fx_PlungerRelease", Plunger, 1
	DOFLinx_PlungerKicker() '' ' DOFLinx - PlungerKicker
	Videos_Spinning_LOGO()  'Play Spinning_LOGO
	AllPlasOn()
End Sub


' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded
'
Sub EndOfBall()
	Dim BonusDelayTime

	'AddDebugText "EndOfBall"
	AllPlasOff()
	' the first ball has been lost. From this point on no new players can join in
	bOnTheFirstBall = FALSE

	' only process any of this if the table is not tilted.  (the tilt recovery
	' mechanism will handle any extra balls or end of game)
	If (vpTilted = FALSE) Then
		Dim AwardPoints

		' add in any bonus points (multipled by the bunus multiplier)
		AwardPoints = BonusPoints(CurrentPlayer) * BonusMultiplier(CurrentPlayer)
		AddScore(AwardPoints)
		'AddDebugText "Bonus Points = " & AwardPoints

		' you may wish to do some sort of display effects which the bonus is
		' being added to the players score

		' add a bit of a delay to allow for the bonus points to be added up
		BonusDelayTime = 1000
	Else
		' no bonus, so move to the next state quickly
		BonusDelayTime = 20
	End If

	' start the end of ball timer which allows you to add a delay at this point
	EndOfBallTimer.Interval = BonusDelayTime
	EndOfBallTimer.Enabled = TRUE
End Sub


' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the currentplayer)
'
Sub EndOfBallTimer_Timer()
	' disable the timer
	EndOfBallTimer.Enabled = FALSE

'	Videos_Drained() 'Turn Off active Videos and Text when drained
	if VPgameinplay = true then

	BankUP.interval = 60    'added for VPX version
	BankUP.enabled = True  'added for vpx version

	End If

	' if were tilted, reset the internal tilted flag (this will also
	' set fpTiltWarnings back to zero) which is useful if we are changing player LOL
	vpTilted = FALSE

	' has the player won an extra-ball ? (might be multiple outstanding)
	If (ExtraBallsAwards(CurrentPlayer) <> 0) Then

		'AddDebugText "Extra Ball"

		' yep got to give it to them
		ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1

		' if no more EB's then turn off any shoot again light
		If (ExtraBallsAwards(CurrentPlayer) = 0) Then
			ShootAgainLight.State = LightStateOff
		End If

		' You may wish to do a bit of a song and dance at this point

		' Create a new ball in the shooters lane
		CreateNewBall()

	Else	' no extra balls

		BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1

		' was that the last ball ?
		If (BallsRemaining(CurrentPlayer) <= 0) Then
		CheckHighScore()
				else'dmdinuse=true
'	DMD_CancelRendering
'DMD_DisplayScene "GAME OVER",""& currentplayer, UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
''DispDmd1.QueueText "[edge4][f4][xc][yc]GAME OVER",denone,1500, True
''DispDmd2.QueueText "[edge4][f4][xc][yc]GAME OVER",denone,1500, True
				'DispDmd1.Text ="[edge4][f4][xc][yc]GAME OVER"
				'DispDmd2.Text ="[edge4][f4][xc][yc]GAME OVER"
			
'EndOfGame
'DmdIntro.enabled = True
		
'		Else

			' not the last ball (for that player)
			' if multiple players are playing then move onto the next one
			EndOfBallComplete()

		End If
	End If
End Sub


' This function is called when the end of bonus display
' (or high score entry finished) and it either end the game or
' move onto the next player (or the next ball of the same player)
'
Sub EndOfBallComplete()
  Dim NextPlayer

	'AddDebugText "EndOfBall - Complete"

	' are there multiple players playing this game ?
	If (PlayersPlayingGame > 1) Then
		' then move to the next player
		NextPlayer = CurrentPlayer + 1
		' are we going from the last player back to the first
		' (ie say from player 4 back to player 1)
		If (NextPlayer > PlayersPlayingGame) Then
			NextPlayer = 1
		End If
	Else
		NextPlayer = CurrentPlayer
	End If

	'AddDebugText "Next Player = " & NextPlayer

   ' is it the end of the game ? (all balls been lost for all players)
	If ((BallsRemaining(CurrentPlayer) <= 0) And (BallsRemaining(NextPlayer) <= 0)) Then
		' you may wish to do some sort of Point Match free game award here
		' generally only done when not in free play mode
		Vpgameinplay = false
		' set the machine into game over mode
'StopMusic 1
EndMusic
playsound "skeletor_getout"
Videos_Skeletor_GetOut()  'Play Skeletor_GetOut Video
	
      EndOfGame
'EndOfGame
'DmdIntro.enabled = True
		' you may wish to put a Game Over message on the

	Else
		' set the next player
		CurrentPlayer = NextPlayer

		' make sure the correct display is upto date
		AddScore(0)

		' reset the playfield for the new player (or new ball)
		ResetForNewPlayerBall()

		' and create a new ball
		CreateNewBall()

	End If
End Sub


' This frunction is called at the End of the Game, it should reset all
' Drop targets, and eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
VPgameinplay = false
AllPlasOff()
EndMusic

if TableFirstLoad=false then
	playsound "skeletor_getout"
	Videos_Skeletor_GetOut()  'Play Skeletor_GetOut Video
End If

TableFirstLoad=False

'hologram.frame 125, 155, 125
DmdIntro.enabled = True
PUPIntro.enabled = True
attractseq.Play SeqArcBottomLeftUpOn ,20,2:attractseq.Play SeqArcBottomrightUpOn ,20,2
attractseq.Play SeqArcBottomLeftUpOn ,20,20:attractseq.Play SeqArcBottomrightUpOn ,20,20
	'AddDebugText "End Of Game"

	' let Future Pinball know that the game has finished.
	' This also clear the fpGameInPlay flag.
'	EndGame()

	' ensure that the flippers are down
	LeftFlipper.Rotatetostart
	DOFLinx_Left_Flipper_Off() '' ' DOFLinx - Left Flipper Off
	RightFlipper.Rotatetostart
	DOFLinx_Right_Flipper_Off() '' ' DOFLinx - Right Flipper Off
	Flipper3.Rotatetostart

	' turn off the reel bulbs
				DMD_CancelRendering
				DMD_DisplayScene "GAME OVER",""& currentplayer, UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
				pDMDSplashBigLines "GAME","OVER", 5,LabelYellow
				'DispDmd1.Text ="[edge4][f4][xc][yc]GAME OVER"
				'DispDmd2.Text ="[edge4][f4][xc][yc]GAME OVER"

   DmdAttractTimer.Interval = 5000
   DmdAttractTimer.Enabled = true



	' set any lights for the attract mode
	SetAllLightsForAttractMode()
BankUP.interval = 60
BankUP.enabled = True
	' you may wish to light any Game Over Light you may have
'playmusic 1, "ATTRACTMUSIC", false, 0.6


End Sub


' The tilt recovery timer waits for all the balls to drain before continuing on
' as per normal
'
Sub TiltRecoveryTimer_Timer()
	' disable the timer
	TiltRecoveryTimer.Enabled	= FALSE
	' if all the balls have been drained then..
	If (BallsOnPlayfield = 0) Then
		' do the normal end of ball thing (this dosn't give a bonus if the table is tilted)
		EndOfBall()
	Else
		' else retry (checks again in another second)
		TiltRecoveryTimer.Interval = 1000
		TiltRecoveryTimer.Enabled = TRUE
	End If

End Sub


' Set any lights for the Attract Mode.
'
Sub SetAllLightsForAttractMode()
	ShootAgainLight.State = LightStateBlinking

End Sub



' *********************************************************************
' **                                                                 **
' **                   Drain / Plunger Functions                     **
' **                                                                 **
' *********************************************************************

' lost a ball ;-( check to see how many balls are on the playfield.
' if only one then decrement the remaining count and test for End of game
' if more than 1 ball (multi-ball) then kill of the ball but don't create
' a new one
'
Sub Drain_Hit()
Drain.DestroyBall
	BallsOnPlayfield = BallsOnPlayfield - 1

DOFLinx_Drain_Hit() '' ' DOFLinx - Drain Hit

flashforms flasher1f, 1000, 500, LightStateOff
flashforms flasher5f, 1000, 500, LightStateOff

' ALL needed to reset SKELETOR Multiball is the following single line

' reset HEMAN Multiball
if H5.state = LightStateOn and ballsonplayfield = 1 and ShootAgainLight.state = LightStateOff then
H1.state = LightStateBlinking
H2.state = LightStateOff
H3.state = LightStateOff
H4.state = LightStateOff
H5.state = LightStateOff
Jackpot1.state = LightStateOff
Jackpot2.state = LightStateOff
Jackpot3.state = LightStateOff
Jackpot4.state = LightStateOff
Bulb12.state = LightStateOff
flashbulb2t.enabled = false
'playmusic "GAMEPLAY music.mp3"
myPlayMusicForMode(2)
duringMULTIBALL.stopplay
DOFLinx_HeMan_MultiBall_End()  'DOFLinx_HeMan_MultiBall_End
HeManMB_Mode_Active=False
Videos_HeMan_MB_Stop()  'Fade Out HeMan_MB Video
Videos_Spinning_Sword()  'Play Spinning_Sword Video
playsound "SpinningSword"
end if

if H5.state = LightStateOn and ballsonplayfield = 2 and ShootAgainLight.state = LightStateOn then
H1.state = LightStateBlinking
H2.state = LightStateOff
H3.state = LightStateOff
H4.state = LightStateOff
H5.state = LightStateOff
Jackpot1.state = LightStateOff
Jackpot2.state = LightStateOff
Jackpot3.state = LightStateOff
Jackpot4.state = LightStateOff
Bulb12.state = LightStateOff
flashbulb2t.enabled = false
'playmusic "GAMEPLAY music.mp3"
myPlayMusicForMode(2)
duringMULTIBALL.stopplay
DOFLinx_HeMan_MultiBall_End()  'DOFLinx_HeMan_MultiBall_End
HeManMB_Mode_Active=False
Videos_HeMan_MB_Stop()  'Fade Out HeMan_MB Video
Videos_Spinning_Sword()  'Play Spinning_Sword Video
playsound "SpinningSword"
end if

if ballsonplayfield = 1 and S4.state = LightStateBlinking and ShootAgainLight.state = LightStateOff then
BankUP.Interval = 60
BankUP.Enabled = true
playsound "skeletorlaughs3"
playsound "skeletorlaughs3"
Jackpot1.state = LightStateOff
Jackpot2.state = LightStateOff
Jackpot3.state = LightStateOff
Jackpot4.state = LightStateOff
duringMULTIBALL.stopplay
'hologram.frame 78, 86
DOFLinx_Skeletor_MultiBall_End()  'DOFLinx_Skeletor_MultiBall_End
SkeletorMB_Mode_Active=False
Videos_Skeletor_MB_FadeOut()  'Skeletor_MB Video FadeOut
Videos_Spinning_Sword()  'Play Spinning_Sword Video
playsound "SpinningSword"
end if

if ballsonplayfield = 1 and S4.state = LightStateBlinking and ShootAgainLight.state = LightStateOn then
BankUP.Interval = 60
BankUP.Enabled = true
playsound "skeletorlaughs3"
playsound "skeletorlaughs3"
Jackpot1.state = LightStateOff
Jackpot2.state = LightStateOff
Jackpot3.state = LightStateOff
Jackpot4.state = LightStateOff
duringMULTIBALL.stopplay
DOFLinx_Skeletor_MultiBall_End()  'DOFLinx_Skeletor_MultiBall_End
SkeletorMB_Mode_Active=False
Videos_Skeletor_MB_FadeOut()  'Skeletor_MB Video FadeOut
Videos_Spinning_Sword()  'Play Spinning_Sword Video
playsound "SpinningSword"
end if

if ballsonplayfield = 1  and ShootAgainLight.state = LightStateOff then
Jackpot1.state = LightStateOff
Jackpot2.state = LightStateOff
Jackpot3.state = LightStateOff
Jackpot4.state = LightStateOff
'playmusic "GAMEPLAY music.mp3"
myPlayMusicForMode(2)
DOFLinx_HeMan_MultiBall_End()  'DOFLinx_HeMan_MultiBall_End
SkeletorMB_Mode_Active=False
Videos_Skeletor_MB_FadeOut()  'Skeletor_MB Video FadeOut
Videos_Spinning_Sword()  'Play Spinning_Sword Video
playsound "SpinningSword"
end if
'
if ballsonplayfield = 1  and ShootAgainLight.state = LightStateOn  then
Jackpot1.state = LightStateOff
Jackpot2.state = LightStateOff
Jackpot3.state = LightStateOff
Jackpot4.state = LightStateOff
'playmusic "GAMEPLAY music.mp3"
myPlayMusicForMode(2)
DOFLinx_HeMan_MultiBall_End()  'DOFLinx_HeMan_MultiBall_End
HeManMB_Mode_Active=False
Videos_HeMan_MB_Stop()  'Fade Out HeMan_MB Video
Videos_Spinning_Sword()  'Play Spinning_Sword Video
playsound "SpinningSword"
end if

' reset Ripper Multiball
if R4.state = LightStateOn and ballsonplayfield = 1 and ShootAgainLight.state = LightStateOff then
R1.state = LightStateBlinking
R2.state = LightStateOff
R3.state = LightStateOff
R4.state = LightStateOff
Jackpot1.state = LightStateOff
Jackpot2.state = LightStateOff
Jackpot3.state = LightStateOff
Jackpot4.state = LightStateOff
'playmusic "GAMEPLAY music.mp3"', true
myPlayMusicForMode(2)
'effectmusic 3, fadeoutandstop, 0, 2000
'effectmusic 1, playandfadein, 1.0, 2000
duringMULTIBALL.stopplay
'hologram.frame 78, 86
DOFLinx_RipperMB_End()  'DOFLinx_RipperMB_End
RoadRipperMB_Mode_Active=False
Videos_Ripper_MB_FadeOut()  'Ripper_MB Video FadeOut
Videos_Spinning_Sword()  'Play Spinning_Sword Video
playsound "SpinningSword"
end if

' reset CASTLE Multiball
if C5.state = LightStateOn and ballsonplayfield = 1 and ShootAgainLight.state = LightStateOff then
C5.state = LightStateOff
C4.state = LightStateOff
C3.state = LightStateOff
C2.state = LightStateOff
C1.state = LightStateBlinking
Jackpot1.state = LightStateOff
Jackpot2.state = LightStateOff
Jackpot3.state = LightStateOff
Jackpot4.state = LightStateOff
bulb11.state = LightStateOff
flashbulb4t.Enabled = False
'playmusic "GAMEPLAY music.mp3"
myPlayMusicForMode(2)
bulb13.state = LightStateOff
flashbulb3t.enabled = False
BulbLock4.blenddisablelighting = 0
BulbLock5.blenddisablelighting = 0
bulb14.state = LightStateOff
flashbulb3t.enabled = False
duringMULTIBALL.stopplay
flasher11.state = LightStateOff
flasher11f.Visible = False
flasher11t.enabled = False
disco1f.Visible = False
disco2.state = LightStateOff
disco2f.Visible = False
DOFLinx_CastleMB_End()  'DOFLinx_CastleMB_End
CastleMB_Mode_Active=False
Videos_Hey_Yay_FadeOut()  'Hey_Yay (Castle MB) Video FadeOut
Videos_Spinning_Sword()  'Play Spinning_Sword Video
playsound "SpinningSword"
end if

		If (bBallSaverActive = false) And (vpTilted = FALSE) and (BallsRemaining(CurrentPlayer) >1)Then
	         select case RandomNumber(3)
	               case 1
                  PlaySound "dobetterthanthat"
                  case 2
                  PlaySound "getouyofmysight"
                  case 3
                  PlaySound "longformoment"
            end select
	   end if
	LightSeqGI.StopPlay
	' Destroy the ball
'	Drain.DestroyBall
'	BallsOnPlayfield = BallsOnPlayfield - 1
	' pretend to knock the ball into the ball storage mechRandomNumber
	PlaySoundAtVol "rDrain" ,Drain, 1
	'PlaySound "dobetterthanthat"
	' if there is a game in progress and
	If (vpGameInPlay = TRUE) And (vpTilted = FALSE) Then

		' is the ball saver active,
		If (bBallSaverActive = TRUE) Then
			' yep, create a new ball in the shooters lane
			CreateNewBall()
			' you may wish to put something on a display or play a sound at this point
             plungertimer.Interval = 1000
			plungertimer.Enabled = True
			DMD_CancelRendering
			DMD_DisplayScene "BALL","SAVED", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
			pDMDSplashBigLines "BALL","SAVED", 3,LabelYellow
             'DispDmd1.QueueText "[f4][xc][y3][bf]BALL[y18]SAVED[/bf]", deFlip, 5000, True
				 'DispDmd2.QueueText "[f4][xc][y3][bf]BALL[y18]SAVED[/bf]", deFlip, 5000, True
             playsound "ManAtArms 01"
			Videos_Ball_Saved()   'PuP - Ball Saved
		Else

			' cancel any multiball if on last ball (ie. lost all other balls)
			'
			If (BallsOnPlayfield = 1) Then
				' and in a multi-ball??
				If (bMultiBallMode = True) then
					' not in multiball mode any more
					bMultiBallMode = False
					' you may wish to change any music over at this point and
					' turn off any multiball specific lights
				End If
			End If

			' was that the last ball on the playfield
			If (BallsOnPlayfield = 0) Then
				' handle the end of ball (change player, high score entry etc..)
				EndOfBall()
				DOFLinx_Drained() '' ' DOFLinx - Drained
			End If

		End If
	End If
End Sub


' A ball is pressing down the trigger in the shooters lane
'
Sub PlungerLaneTrigger_Hit()
	bBallInPlungerLane = TRUE
	DOFLinx_Launch_Button_Flashing() ' DOFLinx - Launch Button Flashing

	' remember last trigger hit by the ball
	set LastSwitchHit = PlungerLaneTrigger
End Sub


Sub PlungerLaneTrigger_UnHit()
	DOFLinx_Launch_Button_Off() ' DOFLinx - Launch Button Off
End Sub


' A Ball may of rolled into the Plunger Lane Kicker, if so then kick it
' back out again
'
Sub PlungerKicker_Hit()
	PlungerKicker.kick 90, 10
PlaySoundAtVol "fx_BallRelease", Plunger, 1
	DOFLinx_PlungerKicker() '' ' DOFLinx - PlungerKicker
End Sub


' The Ball has rolled out of the Plunger Lane.  Check to see if a ball saver machanisim
' is needed and if so fire it up.
'
Sub PlungerLaneGate_Hit()
	' if there is a need for a ball saver, then start off a timer
	' only start if it is currently not running, else it will reset the time period
	If (constBallSaverTime <> 0) And (bBallSaverActive <> TRUE) Then
		' and only if the last trigger hit was the plunger wire.
		' (ball in the shooters lane)
		If (LastSwitchHit.Name = "PlungerLaneTrigger") Then
			' set our game flag
			bBallSaverActive = TRUE
			' start the timer
			BallSaverTimer.Enabled = FALSE
			BallSaverTimer.Interval = constBallSaverTime
			BallSaverTimer.Enabled = TRUE
			' if you have a ball saver light you might want to turn it on at this
			' point (or make it flash)
			ShootAgainLight.State = LightStateBlinking
		End If
	End If
End Sub


' The ball saver timer has expired.  Turn it off and reset the game flag
'
Sub BallSaverTimer_Timer()
	' stop the timer from repeating
	BallSaverTimer.Enabled = FALSE
	' clear the flag
	bBallSaverActive = FALSE
	' if you have a ball saver light then turn it off at this point
shootagainlight.state = LightStateOff

End Sub



' *********************************************************************
' **                                                                 **
' **                   Supporting Score Functions                    **
' **                                                                 **
' *********************************************************************

' Add points to the score and update the score board
'
Sub AddScore(points)
dim displayscore
	If (vpTilted = FALSE) Then
		' add the points to the current players score variable
		nvScore(CurrentPlayer) = nvScore(CurrentPlayer) + points
       lastpoints=points


		'Player1Reel.State = LightStateOff
		'Player2Reel.State = LightStateOff
		'Player3Reel.State = LightStateOff
		'Player4Reel.State = LightStateOff
       displayscore = (nvScore(CurrentPlayer))
if displayscore = "0" then
displayscore = "00"

end if
if scoreupdate = true then
		' add the points to the correct display and light the current players display
		Select Case (CurrentPlayer)
			Case 1:	'DispDmd1.Text = "[f5][xc][y3]" & displayscore & "[f1] [x0] [y27] Balls " & (BallsRemaining(CurrentPlayer)) & " [x72] [y27] Credits "& nvCredits & "[x50][y27]P:" & currentplayer
						'DispDmd2.Text = "[f5][xc][y3]" & displayscore & "[f1] [x0] [y27] Balls " & (BallsRemaining(CurrentPlayer)) & " [x72] [y27] Credits "& nvCredits & "[x50][y27]P:" & currentplayer
					If ScoreType=1 Then
					UltraDMD.DisplayScoreboard 2, 1, nvScore(1), nvScore(2), 0, 0, "Balls:" & BallsRemaining(CurrentPlayer), "Player:"& currentplayer
					End If
					pUpdateScores

			Case 2:		If ScoreType=1 Then
						UltraDMD.DisplayScoreboard 2, 2, nvScore(1), nvScore(2), 0, 0, "Balls:" & BallsRemaining(CurrentPlayer), "Player:"& currentplayer
						End If
						pUpdateScores
						'DispDmd1.Text = "[f5][xc][y3]" & displayscore & "[f1] [x0] [y27] Balls " & (BallsRemaining(CurrentPlayer)) & " [x72] [y27] Credits "& nvCredits & "[x50][y27]P:" & currentplayer
						'DispDmd2.Text = "[f5][xc][y3]" & displayscore & "[f1] [x0] [y27] Balls " & (BallsRemaining(CurrentPlayer)) & " [x72] [y27] Credits "& nvCredits & "[x50][y27]P:" & currentplayer

			Case 3:	'DispDmd1.Text = "[f5][xc][y3]" & displayscore & "[f1] [x0] [y27] Balls " & (BallsRemaining(CurrentPlayer)) & " [x72] [y27] Credits "& nvCredits & "[x50][y27]P:" & currentplayer
						'DispDmd2.Text = "[f5][xc][y3]" & displayscore & "[f1] [x0] [y27] Balls " & (BallsRemaining(CurrentPlayer)) & " [x72] [y27] Credits "& nvCredits & "[x50][y27]P:" & currentplayer


			Case 4:	'DispDmd1.Text = "[f5][xc][y3]" & displayscore & "[f1] [x0] [y27] Balls " & (BallsRemaining(CurrentPlayer)) & " [x72] [y27] Credits "& nvCredits & "[x50][y27]P:" & currentplayer
						'DispDmd2.Text = "[f5][xc][y3]" & displayscore & "[f1] [x0] [y27] Balls " & (BallsRemaining(CurrentPlayer)) & " [x72] [y27] Credits "& nvCredits & "[x50][y27]P:" & currentplayer

		End Select
	End if
end if

	' you may wish to check to see if the player has gotten a replay
End Sub


'
Sub AddJackpot(points)
	' Jackpots only generally increment in multiball mode and not tilted
	' but this dosn't have to tbe the case
	If (vpTilted = False) Then

		If (bMultiBallMode = TRUE) Then
			nvJackpot = nvJackpot + points
			' you may wish to limit the jackpot to a upper limit, ie..
			'	If (nvJackpot >= 6000) Then
			'		nvJackpot = 6000
			' 	End if
		End if
	End if
End Sub


' Will increment the Bonus Multiplier to the next level
'
Sub IncrementBonusMultiplier()
	Dim NewBonusLevel

	' if not at the maximum bonus level
	if (BonusMultiplier(CurrentPlayer) < constMaxMultiplier) then
		' then set it the next next one and set the lights
		NewBonusLevel = BonusMultiplier(CurrentPlayer) + 1
		SetBonusMultiplier(NewBonusLevel)
   End if
End Sub


' Set the Bonus Multiplier to the specified level and set any lights accordingly
'
Sub SetBonusMultiplier(Level)
	' Set the multiplier to the specified level
	BonusMultiplier(CurrentPlayer) = Level

	' If the multiplier is 1 then turn off all the bonus lights
	If (BonusMultiplier(CurrentPlayer) = 1) Then
		' insert your own code here
	Else
		' there is a bonus, turn on all the lights upto the current level
		If (BonusMultiplier(CurrentPlayer) >= 2) Then
			' insert your own code here
		End If
		' etc..
	End If
End Sub



' *********************************************************************
' **                                                                 **
' **                     Table Object Script Events                  **
' **                                                                 **
' *********************************************************************

' The Left Slingshot has been Hit, Add Some Points and Flash the Slingshot Lights
'
'Sub LeftSlingshotRubber_Hit()
'	' add some points
'	AddScore(500)
'	' flash the lights around the slingshot
'	FlashForMs LeftSlingshotBulb1, 100, 50, LightStateOff
'	FlashForMs LeftSlingshotBulb2, 100, 50, LightStateOff
'	FlashForMs Flasher1, 100, 50, LightStateOff
	DOFLinx_Left_Slingshot_Hit() '' ' DOFLinx - Left Slingshot Hit
'End Sub


' The Right Slingshot has been Hit, Add Some Points and Flash the Slingshot Lights
'
'Sub RightSlingshotRubber_Hit()
'	' add some points
'	AddScore(500)
'	' flash the lights around the slingshot
'	FlashForMs RightSlingshotBulb1, 100, 50, LightStateOff
'	FlashForMs RightSlingshotBulb2, 100, 50, LightStateOff
'	FlashForMs Flasher5, 100, 50, LightStateOff
	DOFLinx_Right_Slingshot_Hit() '' ' DOFLinx - Right Slingshot Hit
'End Sub


' The Left InLane trigger has been Hit
'
Sub LeftInLaneTrigger_Hit()
	' add some points
	AddScore(1000)
	' remember last trigger hit by the ball
	set LastSwitchHit = LeftInLaneTrigger
	DOFLinx_LeftInLaneTrigger_Hit() 'DOFLinx_LeftInLaneTrigger_Hit
End Sub


' The Right InLane trigger has been Hit
'
Sub RightInLaneTrigger_Hit()
	' add some points
	AddScore(1000)
	' remember last trigger hit by the ball
	set LastSwitchHit = RightInLaneTrigger
	DOFLinx_RightInLaneTrigger_Hit() 'DOFLinx_RightInLaneTrigger_Hit
End Sub


' The Left OutLane trigger has been Hit
'
Sub LeftOutLaneTrigger_Hit()
FlashForMs flasher1f, 600, 50, LightStateOff
	' add some points
	AddScore(10000)
	' remember last trigger hit by the ball
	set LastSwitchHit = LeftOutLaneTrigger
End Sub


' The Right OutLane trigger has been Hit
'
Sub RightOutLaneTrigger_Hit()
flashforms flasher5f, 600, 50, LightStateOff
	' add some points
	AddScore(10000)
	' remember last trigger hit by the ball
	set LastSwitchHit = RightOutLaneTrigger
End Sub




'*MUSIK bei EFFEKTEN aus und einblenden:
'**  effectmusic 1, fadeoutandpause, 0, 5
'**  musicin.set true, 1

'sub musicin_Timer()
'musicin.Enabled = false
''effectmusic 1, playandfadein, 1.0, 500
'end sub


'----------------------------------------------------
'(plungertimer for ball saver)
sub plungertimer_Timer()
plungertimer.Enabled = false
PlungerKicker.kick 90, 10
PlaysoundAtVol "fx_BallRelease", Plunger, 1
DOFLinx_AutoPlunger() ' DOFLinx - AutoPlunger
Plunger.kick 0, 35
PlaysoundAtVol "fx_BallLaunch", Plunger, 1
end sub


sub plungercreateball_Timer()
plungercreateball.Enabled = false
plungerkicker.createball
PlungerKicker.kick 90, 10
signon.imageb = "logo 005"
PlaysoundAtVol "fx_BallRelease", Plunger, 1
end sub

'-----------------------------------------------------
'**************************************************************************
'timed circular 4 flashers:
'integrate this in script somewhere (1000 or 2000 must be VARIATED!!!)
'fltimer1.set true, 1
'fltimer2.set true, 2000


sub fltimer1_Timer()
fltimer1.Enabled = false
flasher8.blinkpattern = "1000"
flasher9.blinkpattern = "0100"
flasher6.blinkpattern = "0010"
flasher13.blinkpattern = "0001"

flasher8.BlinkInterval = 50
flasher9.BlinkInterval = 50
flasher6.BlinkInterval = 50
flasher13.BlinkInterval = 50

flasher8.state = LightStateBlinking
flasher9.state = LightStateBlinking
flasher6.state = LightStateBlinking
flasher13.state = LightStateBlinking
end sub

sub fltimer2_Timer()
fltimer2.Enabled = false
flasher8.state = LightStateOff
flasher8f.visible = False
flasher9.state = LightStateOff
flasher9f.visible = False
flasher6.state = LightStateOff
flasher6f.visible = False
flasher13.state = LightStateOff
flasher13f.visible = False
end sub



'timed circular 8 jackpot flashers:
'integrate this in script somewhere (1000 or 2000 must be VARIATED!!!)
'fltimerA.set true, 1
'fltimerB.set true, 2000


sub fltimerA_Timer()
fltimerA.Enabled = false
flasher2.blinkpattern = "10001000"
flasher5.blinkpattern = "01000000"
flasher8.blinkpattern = "00100100"
flasher9.blinkpattern = "00010000"
flasher10.blinkpattern = "10000100"
flasher6.blinkpattern = "01000000"
flasher12.blinkpattern = "00101000"
flasher13.blinkpattern = "00010001"

flasher2.BlinkInterval = 50
flasher5.BlinkInterval = 50
flasher8.BlinkInterval = 50
flasher9.BlinkInterval = 50
flasher10.BlinkInterval = 50
flasher6.BlinkInterval = 50
flasher12.BlinkInterval = 50
flasher13.BlinkInterval = 50

flasher2.state = LightStateBlinking
flasher5.state = LightStateBlinking
flasher8.state = LightStateBlinking
flasher9.state = LightStateBlinking
flasher10.state = LightStateBlinking
flasher6.state = LightStateBlinking
flasher12.state = LightStateBlinking
flasher13.state = LightStateBlinking
end sub

sub fltimerB_Timer()
fltimerB.Enabled = false
flasher2.state = LightStateOff
flasher2f.visible = False
flasher5.state = LightStateOff
flasher5f.visible = False
flasher8.state = LightStateOff
flasher8f.visible = False
flasher9.state = LightStateOff
flasher9f.visible = False
flasher10.state = LightStateOff
flasher10f.visible = False
flasher6.state = LightStateOff
flasher6f.visible = False
flasher12.state = LightStateOff
flasher12f.visible = False
flasher13.state = LightStateOff
flasher13f.visible = False
end sub


'Kick it back:
sub kickbacktrigger_hit()
if SpecialLeft.state = LightStateOn then
SpecialLeft.state = LightStateOff
SpecialLeft2.state = LightStateOff
SpecialRight.state = LightStateOff
SpecialRight2.state = LightStateOff
kickback.Fire
kickback.Pullback
DOFLinx_Kickback() '' ' DOFLinx - Kickback
playsound "sword01"
playsound "sword01"
'playsound "solenoid"
playsoundAtVol "solenoid", kickbacktrigger, VolKick
	
flushdmdtimer.Interval = 1550
flushdmdtimer.Enabled = True
kickbackclose.Interval = 500
kickbackclose.Enabled = True
end if
end sub

sub kickbackclose_Timer()
kickbackclose.Enabled = false
KickBackDiverter.IsDropped = True
end sub

'ONLY SCORING Triggers:*****
sub OuterOrbitTrigger_hit()
addscore (25000)
playsound "ThunderLong"'added for DEBUG
flashforms flasher12f, 600, 50, LightStateOff

end sub

sub target4_hit()
playsound "sword01"
playsound "sword01"
DOFLinx_Target4_Hit() '' ' DOFLinx - Target4 Hit

flushdmdtimer.Interval = 1100
flushdmdtimer.Enabled = True
addscore (5000)
end sub


sub target5_hit()
playsound "sword02"
playsound "sword02"
DOFLinx_Target5_Hit() '' ' DOFLinx - Target5 Hit
		
flushdmdtimer.Interval = 1500
flushdmdtimer.Enabled = True
addscore (5000)
end sub



'******************EXTRA BALL****************************(initiated by the castle ramp somewhere)
sub target9_hit()
playsound "laser05"
playsound "laser05"
DOFLinx_Target9_Hit() '' ' DOFLinx - Target9 Hit
if EB1.state = LightStateBlinking and EB2.state = LightStateOn then
EB1.state = LightStateOn
EXTRABALL.state = LightStateBlinking
addscore (20000)
flashforms Flasher1f, 1000, 100, LightStateOff
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
If ScoreType=1 Then
DMD_CancelRendering
DMD_DisplayScene "GET THE","EXTRA BALL", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
End If
pDMDSplashBigLines "Get the","Extra Ball", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]GET THE[y18][bf]EXTRA BALL[/bf]", denone, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]GET THE[y18][bf]EXTRA BALL[/bf]", denone, 2000, True
end if
if EB1.state = LightStateBlinking and EB2.state = LightStateBlinking then
EB1.state = LightStateOn
addscore (15000)
end if
if EB1.state = LightStateOn then
addscore (15000)
end if
if EB1.state = LightStateOff then
addscore (5000)
end if
end sub

sub target10_hit()
playsound "laser05"
playsound "laser05"
DOFLinx_Target10_Hit() '' ' DOFLinx - Target10 Hit
if EB2.state = LightStateBlinking and EB1.state = LightStateOn then
EB2.state = LightStateOn
EXTRABALL.state = LightStateBlinking
addscore (20000)
flashforms Flasher1f, 1000, 100, LightStateOff
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
If ScoreType=1 Then
DMD_CancelRendering
DMD_DisplayScene "GET THE","EXTRA BALL", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
End If
pDMDSplashBigLines "Get the","Extra Ball", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]GET THE[y18]EXTRA BALL", deFlip, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]GET THE[y18]EXTRA BALL", deFlip, 2000, True
end if
if EB1.state = LightStateBlinking and EB2.state = LightStateBlinking then
EB2.state = LightStateOn
addscore (15000)
end if
if EB2.state = LightStateOn then
addscore (15000)
end if
if EB2.state = LightStateOff then
addscore (5000)
end if
end sub

sub EBtarget_hit()
DOFLinx_EBtarget_Hit() '' ' DOFLinx - EBtarget Hit
addscore (15000)
if ExtraBall.state = LightStateOff then
playsound "Laser04"
playsound "Laser04"
end if
if ExtraBall.state = LightStateBlinking then
playsound "surprise"

DOFLinx_Extra_Ball()  '' ' DOFLinx: Extra Ball
Videos_Skeletor_ExtraBall()  'Play Skeletor_ExtraBall Video
			
flushdmdtimer.Interval = 3800
flushdmdtimer.Enabled = True
ExtraBall.state = LightStateOff
EB1.state = LightStateOff
EB2.state = LightStateOff
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1500
fltimerB.Enabled = True
ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1 ' awards extra ball to current player
ShootAgainLight.state = LightStateOn
end if
end sub

'SPECIAL Triggers:**************************************************************
sub SpecialLeftTrigger_hit()
if SpecialLeft.state = LightStateOn then
addscore (500000)
flashforms Flasher1f, 1000, 100, LightStateOff
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
If ScoreType=1 Then
DMD_CancelRendering
DMD_DisplayScene "KICKBACK","", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
End If
pDMDSplashBig "KICKBACK", 2.5,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]KICKBACK &[y18]500.000", deFlip, 3000, True
'DispDmd2.QueueText "[f3][xc][y5]KICKBACK &[y18]500.000", deFlip, 3000, True
end if
end sub

sub SpecialLEFT2Trigger_hit()
DOFLinx_LeftInLaneTrigger_Hit() 'DOFLinx_LeftInLaneTrigger_Hit
if SpecialLeft2.state = LightStateOn then
addscore (1000000)
SpecialLeft2.state = LightStateOff
SpecialRight.state = LightStateOff
SpecialRight2.state = LightStateOff
flashforms Flasher1f, 1000, 100, LightStateOff
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SPECIAL","1 Million -", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SPECIAL","1 Million", 4,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SPECIAL[y18]1 Million -", deFlip, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SPECIAL[y18]1 Million -", deFlip, 2000, True
end if
end sub

sub SpecialRightTrigger_hit()
if SpecialRight.state = LightStateOn then
SpecialLeft2.state = LightStateOff
SpecialRight.state = LightStateOff
SpecialRight2.state = LightStateOff
fltimer1.Interval = 1
fltimer1.Enabled = True
fltimer2.Interval = 2000
fltimer2.Enabled = True
ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1 ' awards extra ball to current player
flashforms shootagainlight, 1500, 150, LightStateOn
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "E X T R A","B A L L", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "Extra","Ball", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]E X T R A[y18]B A L L", deFlip, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]E X T R A[y18]B A L L", deFlip, 2000, True
end if
end sub

sub SpecialRight2Trigger_hit()
DOFLinx_RightInLaneTrigger_Hit() 'DOFLinx_LeftInLaneTrigger_Hit
if SpecialRight2.state = LightStateOn then
addscore (1000000)
SpecialLeft2.state = LightStateOff
SpecialRight.state = LightStateOff
SpecialRight2.state = LightStateOff
flashforms Flasher5f, 1000, 100, LightStateOff
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SPECIAL","1 Million", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SPECIAL","1 Million", 2,LabelYellow

'DispDmd1.QueueText "[f3][xc][y5]SPECIAL[y18]1 Million", deFlip, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SPECIAL[y18]1 Million", deFlip, 2000, True
end if
end sub

''******************************************
''			MODULOK Wiggle
''******************************************

Sub Modulokwiggle_Timer
If MODULOK.ObjRoty =< 0 Then
		MODULOK.ObjRoty = MODULOK.ObjRoty +10
		Modulokwiggle.enabled=False
Else
If MODULOK.ObjRoty >< 0 Then
		MODULOK.ObjRoty = MODULOK.ObjRoty -20
Modulokwiggle.enabled=True
	End If
End If
End Sub



'******BUMPERS SCRIPT*************************************************************
'--------------------


sub bumper1_hit()

Modulokwiggle.enabled = true

PlaysoundAtVol "fx_bumperacreat1", Bumper1, VolBump
if ballsonplayfield = 4 then
addscore (100000)
flashforms flasher7, 100, 50, LightStateOff
flashforms BumperLight, 100, 50, LightStateOff
end if
if ballsonplayfield = 3 then
addscore (50000)
flashforms flasher7, 100, 50, LightStateOff
flashforms BumperLight, 100, 50, LightStateOff
end if
if ballsonplayfield = 2 then
addscore (10000)
flashforms flasher7, 100, 50, LightStateOff
flashforms BumperLight, 100, 50, LightStateOff
end if
if ballsonplayfield = 1 then
addscore (500)
flashforms flasher7, 100, 50, LightStateOff
flashforms BumperLight, 100, 50, LightStateOff
end if
		
flushdmdtimer.Interval = 1400
flushdmdtimer.Enabled = true
DOFLinx_Bumper1_Hit() '' ' DOFLinx - Bumper 1 Hit
end sub

sub bumper2_hit()

Modulokwiggle.enabled = true

PlaysoundAtVol "fx_bumperacreat2", Bumper2, VolBump
if ballsonplayfield = 4 then
addscore (100000)
flashforms flasher4, 100, 50, LightStateOff
flashforms BumperLight, 100, 50, LightStateOff
end if
if ballsonplayfield = 3 then
addscore (50000)
flashforms flasher4, 100, 50, LightStateOff
flashforms BumperLight, 100, 50, LightStateOff
end if
if ballsonplayfield = 2 then
addscore (10000)
flashforms flasher4, 100, 50, LightStateOff
flashforms BumperLight, 100, 50, LightStateOff
end if
if ballsonplayfield = 1 then
addscore (1000)
flashforms flasher4, 100, 50, LightStateOff
flashforms BumperLight, 100, 50, LightStateOff
end if

	
flushdmdtimer.Interval = 1330
flushdmdtimer.Enabled = true
DOFLinx_Bumper2_Hit() '' ' DOFLinx - Bumper 2 Hit
end sub

sub bumper3_hit()

Modulokwiggle.enabled = true

PlaysoundAtVol "fx_bumperacreat3", Bumper3, VolBump
if ballsonplayfield = 4 then
addscore (100000)
flashforms flasher3, 100, 50, LightStateOff
flashforms BumperLight, 100, 50, LightStateOff
end if
if ballsonplayfield = 3 then
addscore (50000)
flashforms flasher3, 100, 50, LightStateOff
flashforms BumperLight, 100, 50, LightStateOff
end if
if ballsonplayfield = 2 then
addscore (10000)
flashforms flasher3, 100, 50, LightStateOff
flashforms BumperLight, 100, 50, LightStateOff
end if
if ballsonplayfield = 1 then
addscore (1500)
flashforms flasher3, 100, 50, LightStateOff
flashforms BumperLight, 100, 50, LightStateOff
end if

flushdmdtimer.Interval = 1400
flushdmdtimer.Enabled = true
DOFLinx_Bumper3_Hit() '' ' DOFLinx - Bumper 3 Hit
end sub






'***********HeMan SCRIPT***************************************************************

sub InitiateHeMan_hit()
addscore (25000)
SpecialLeft.state = LightStateOn
Kickbackdiverter.IsDropped = true
if H1.state = LightStateOn then
playsound "laser08b"

flushdmdtimer.Interval = 1200
flushdmdtimer.Enabled =true
end if

if H1.state = LightStateBlinking then
H1.state = LightStateOn
H2.state = LightStateBlinking
fltimer1.Interval = 1
fltimer1.Enabled = true
fltimer2.Interval = 1000
fltimer2.Enabled = true
scoreupdate = false
playsound "stophim"

flushdmdtimer.Interval = 2500
flushdmdtimer.Enabled = True
DOFLinx_HeMan_Lit()  'DOFLinx_HeMan_Lit
Videos_Skeletor_StopHim()  'Play Skeletor_StopHim Video
end if
end sub

'He Man Ball timer

sub ballout1_Timer()
ballout1.Enabled = false
'playmusic "MULTIBALL HeMan.mp3"
myPlayMusicForMode(6)
DOFLinx_HeMan_MultiBall_Start()  'DOFLinx_HeMan_MultiBall_Start
HeManMB_Mode_Active=True
Videos_HeMan_POG_Stop()  'Fade Out HeMan_POG Video

plungerkicker.createball
PlungerKicker.kick 90, 10

plungertimer.Interval = 1000
plungertimer.Enabled = 1000
flashforms flasher5f, 2000, 1000, LightStateOff
flashforms flasher1f, 2000, 1000, LightStateOff
flashforms flasher2f, 2000, 1000, LightStateOff
flashforms flasher6f, 2000, 1000, LightStateOff
flashforms flasher9f, 2000, 1000, LightStateOff
flashforms flasher12f, 2000, 1000, LightStateOff
flashforms flasher10f, 2000, 1000, LightStateOff
flashforms flasher8f, 2000, 1000, LightStateOff
end sub

sub ballout2_Timer()
ballout2.Enabled =  false
plungerkicker.createball
PlungerKicker.kick 90, 10
plungertimer.Interval = 1000
plungertimer.Enabled = 1000
flashforms flasher5f, 2000, 1000, LightStateOff
flashforms flasher1f, 2000, 1000, LightStateOff
flashforms flasher2f, 2000, 1000, LightStateOff
flashforms flasher6f, 2000, 1000, LightStateOff
flashforms flasher9f, 2000, 1000, LightStateOff
flashforms flasher12f, 2000, 1000, LightStateOff
flashforms flasher10f, 2000, 1000, LightStateOff
flashforms flasher8f, 2000, 1000, LightStateOff
end sub

sub ballout3_Timer()
ballout3.Enabled = false
plungerkicker.createball
PlungerKicker.kick 90, 10
plungertimer.Interval = 1000
plungertimer.Enabled = 1000
flashforms flasher5f, 2000, 1000, LightStateOff
flashforms flasher1f, 2000, 1000, LightStateOff
flashforms flasher2f, 2000, 1000, LightStateOff
flashforms flasher6f, 2000, 1000, LightStateOff
flashforms flasher9f, 2000, 1000, LightStateOff
flashforms flasher12f, 2000, 1000, LightStateOff
flashforms flasher10f, 2000, 1000, LightStateOff
flashforms flasher8f, 2000, 1000, LightStateOff
end sub

sub ballout4_Timer()
ballout4.Enabled = false
plungerkicker.createball
PlungerKicker.kick 90, 10
plungertimer.Interval = 1000
plungertimer.Enabled = 1000
flashforms flasher5f, 2000, 1000, LightStateOff
flashforms flasher1f, 2000, 1000, LightStateOff
flashforms flasher2f, 2000, 1000, LightStateOff
flashforms flasher6f, 2000, 1000, LightStateOff
flashforms flasher9f, 2000, 1000, LightStateOff
flashforms flasher12f, 2000, 1000, LightStateOff
flashforms flasher10f, 2000, 1000, LightStateOff
flashforms flasher8f, 2000, 1000, LightStateOff
end sub


sub HeManKicker_hit()

DOFLinx_HeManKicker_hit() ' DOFLinx - HeManKicker_hit
playsound "scoop-agujero"
playsound "rballfall"

if ballsonplayfield = 9 then
addscore (10000000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","10 Million", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","10 Million", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollIn, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollIn, 2000, True
Hemantimer1.Interval = 1000
Hemantimer1.Enabled = True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()  '' ' DOFLinx: Combo - Jackpot - Effects
end if
if ballsonplayfield = 8 then
addscore (10000000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","10 Million", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","10 Million", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollIn, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollIn, 2000, True
Hemantimer1.Interval = 1000
Hemantimer1.Enabled = True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()  '' ' DOFLinx: Combo - Jackpot - Effects
end if
if ballsonplayfield = 7 then
addscore (10000000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","10 Million", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","10 Million", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollIn, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollIn, 2000, True
Hemantimer1.Interval = 1000
Hemantimer1.Enabled = True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()  '' ' DOFLinx: Combo - Jackpot - Effects
end if
if ballsonplayfield = 6 then
addscore (10000000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","10 Million", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","10 Million", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollIn, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollIn, 2000, True
Hemantimer1.Interval = 1000
Hemantimer1.Enabled = True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()  '' ' DOFLinx: Combo - Jackpot - Effects
end if
if ballsonplayfield = 5 then
addscore (10000000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","10 Million", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","10 Million", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollIn, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollIn, 2000, True
Hemantimer1.Interval = 1000
Hemantimer1.Enabled = True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()  '' ' DOFLinx: Combo - Jackpot - Effects
end if
if ballsonplayfield = 4 then
addscore (10000000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","10 Million", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","10 Million", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollIn, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollIn, 2000, True
Hemantimer1.Interval = 1000
Hemantimer1.Enabled = True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()  '' ' DOFLinx: Combo - Jackpot - Effects
end if
if ballsonplayfield = 3 then
addscore (1000000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","1 Million", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","1 Million", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]1 Million", deScrollIn, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]1 Million", deScrollIn, 2000, True
Hemantimer1.Interval = 1000
Hemantimer1.Enabled = True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()  '' ' DOFLinx: Combo - Jackpot - Effects
end if
if ballsonplayfield = 2 then
addscore (500000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","500,000", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","500,000", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]500,000", deScrollIn, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]500,000", deScrollIn, 2000, True
Hemantimer1.Interval = 1000
Hemantimer1.Enabled = True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()  '' ' DOFLinx: Combo - Jackpot - Effects
end if

if H1.state = LightStateOn and H2.state = LightStateOn and H3.state = LightStateOn and H4.state = LightStateOn and H5.state = LightStateBlinking and ballsonplayfield = 1 then
addscore (500000)
HeManMultiball=0
H5.state = LightStateOn
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 12000
fltimerB.Enabled = True
HeManKicker.destroyball
ballout1.Interval = 9200
ballout1.Enabled = True
ballout2.Interval = 14000
ballout2.Enabled = True
ballout3.Interval = 16000
ballout3.Enabled = True
ballout4.Interval = 18000
ballout4.Enabled = True
scoreupdate = false
ballsonplayfield = 4
Jackpot1.state = LightStateBlinking
Jackpot2.state = LightStateBlinking
Jackpot3.state = LightStateBlinking
Jackpot4.state = LightStateBlinking
SpecialLeft.state = LightStateOn
SpecialLeft2.state = LightStateOn
SpecialRight.state = LightStateOn
SpecialRight2.state = LightStateOn
Kickbackdiverter.IsDropped = True
playsound "HeMan Multiball Go"
DOFLinx_HeMan_MB_Go() 'DOFLinx_HeMan_MB_Go
Videos_HeMan_POG()  'Play He-Man "By the Power of Grayskull" Video
EndMusic

diffquoteHM.state = LightStateOn
duringMULTIBALL.play seqalloff

HeManMultiballTimer.Interval = 50
HeManMultiballTimer.Enabled = true
end if

' ************* ball lock 6

if H1.state = LightStateOn and H2.state = LightStateOn and H3.state = LightStateOn and H4.state = LightStateBlinking and H5.state = LightStateOff and ballsonplayfield = 1 and diffquoteHM.state = LightStateOn then
addscore (250000)
H4.state = LightStateOn
H5.state = LightStateBlinking
'bulb12.state = LightStateBlinking
flashbulb2t.enabled = True
fltimer1.interval = 1
fltimer1.Enabled = True
fltimer2.Interval = 1500
fltimer2.Enabled = True
HeManKicker.destroyball
plungercreateball.Interval = 3000
plungercreateball.Enabled = true
plungertimer.Interval = 4000
plungertimer.Enabled = True
scoreupdate = false
'flushdmdtimer.set true , 2000

playsound "Welllwell"
playsound "Welllwell"
Videos_Well_Well()  'Play Well_Well Video
	
flushdmdtimer.Interval = 3800
flushdmdtimer.Enabled = True
DOFLinx_Ball3_Locked() 'DOFLinx_Ball3_Locked
end if

' ************* ball lock 3

if H1.state = LightStateOn and H2.state = LightStateOn and H3.state = LightStateOn and H4.state = LightStateBlinking and H5.state = LightStateOff and ballsonplayfield = 1 and diffquoteHM.state = LightStateOff then
addscore (250000)
H4.state = LightStateOn
H5.state = LightStateBlinking
flashbulb2t.enabled = True
fltimer1.interval = 1
fltimer1.Enabled = True
fltimer2.Interval = 1500
fltimer2.Enabled = True
HeManKicker.destroyball
plungercreateball.Interval = 3000
plungercreateball.Enabled = true
plungertimer.Interval = 4000
plungertimer.Enabled = True
scoreupdate = false

playsound "HeMan01"
playsound "HeMan01"
		
flushdmdtimer.Interval = 3600
flushdmdtimer.Enabled = True
DOFLinx_Ball3_Locked() 'DOFLinx_Ball3_Locked
Videos_HeMan_Cell()  'Play HeMan_Cell Video
end if

' ************* ball lock 5

if H1.state = LightStateOn and H2.state = LightStateOn and H3.state = LightStateBlinking and H4.state = LightStateOff and H5.state = LightStateOff and ballsonplayfield = 1 and diffquoteHM.state = LightStateOn then
addscore (100000)
H3.state = LightStateOn
H4.state = LightStateBlinking
fltimer1.interval = 1
fltimer1.Enabled = True
fltimer2.Interval = 1500
fltimer2.Enabled = True
HeManKicker.destroyball
plungercreateball.Interval = 3000
plungercreateball.Enabled = true
plungertimer.Interval = 4000
plungertimer.Enabled = True
scoreupdate = false
playsound "skeletor06"
playsound "skeletor06"
flushdmdtimer.Interval = 3600
flushdmdtimer.Enabled = True
DOFLinx_Ball2_Locked() 'DOFLinx_Ball2_Locked
Videos_Skeletor_Palace()  'Play Skeletor_Palace Video
end if

' ************* ball lock 2

if H1.state = LightStateOn and H2.state = LightStateOn and H3.state = LightStateBlinking and H4.state = LightStateOff and H5.state = LightStateOff and ballsonplayfield = 1 and diffquoteHM.state = LightStateOff then
addscore (100000)
H3.state = LightStateOn
H4.state = LightStateBlinking
fltimer1.Interval = 1
fltimer1.Enabled = True
fltimer2.Interval = 1500
fltimer2.Enabled = True
HeManKicker.destroyball
plungercreateball.Interval = 3000
plungercreateball.Enabled = True
plungertimer.Interval = 4000
plungertimer.Enabled = True
scoreupdate = false
playsound "ManAtArms 02"
playsound "ManAtArms 02"
Videos_Snake_Mountain()  'Play Snake_Mountain Video
flushdmdtimer.Interval = 4000
flushdmdtimer.Enabled = True
DOFLinx_Ball2_Locked() 'DOFLinx_Ball2_Locked
end if


' ************* ball lock 4

if H1.state = LightStateOn and H2.state = LightStateBlinking and H3.state = LightStateOff and H4.state = LightStateOff and H5.state = LightStateOff and ballsonplayfield = 1 and diffquoteHM.state = LightStateOn then
addscore (50000)
H2.state = LightStateOn
H3.state = LightStateBlinking
fltimer1.Interval = 1
fltimer1.Enabled = True
fltimer2.Interval = 1500
fltimer2.Enabled = True
HeManKicker.destroyball
plungercreateball.Interval = 3000
plungercreateball.Enabled = True
plungertimer.Interval = 4000
plungertimer.Enabled = True
scoreupdate = false
playsound "hollow"
playsound "hollow"

Videos_Skeletor_Hollow()  'Play Skeletor_Hollow Video
flushdmdtimer.Interval = 3800
flushdmdtimer.Enabled = True
DOFLinx_Ball1_Locked() 'DOFLinx_Ball1_Locked
end if

' ************* ball lock 1

if H1.state = LightStateOn and H2.state = LightStateBlinking and H3.state = LightStateOff and H4.state = LightStateOff and H5.state = LightStateOff and ballsonplayfield = 1 and diffquoteHM.state = LightStateOff then
addscore (50000)
H2.state = LightStateOn
H3.state = LightStateBlinking
fltimer1.Interval = 1
fltimer1.Enabled = True
fltimer2.Interval = 1500
fltimer2.Enabled = True
HeManKicker.destroyball
plungercreateball.Interval = 3000
plungercreateball.Enabled = True
plungertimer.Interval = 4000
plungertimer.Enabled = True
scoreupdate = false
playsound "evillynn01"
playsound "evillynn01"
Videos_EvilLynn_Laugh()  'Play EvilLynn_Laugh Video
flushdmdtimer.Interval = 3000
flushdmdtimer.Enabled = True
DOFLinx_Ball1_Locked() 'DOFLinx_Ball1_Locked
end if

if H1.state = LightStateBlinking and ballsonplayfield = 1 then
Videos_HeMan_AttackBall()  'Play HeMan_AttackBall Video
playsound "LASER09"
playsound "LASER09"
addscore (25000)
Hemantimer1.Interval =1000
Hemantimer1.Enabled = True
flushdmdtimer.Interval = 1900
flushdmdtimer.Enabled = True
end if
end sub

dim HeManMultiball

sub HeManMultiballTimer_Timer
		HeManMultiball=HeManMultiball+1
		HeManMultiballTimer.enabled=false
		TimerDMD.Enabled = false
		Timeraddzero.Enabled = False

	select case (HeManMultiball)
		case 1
			  HeManMultiballTimer.Interval = 6000
				HeManMultiballTimer.Enabled = True
			DMD_CancelRendering
			DMD_DisplayScene "By the POWER of","GRAYSKULL!", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
			pDMDSplashLines "By the POWER of","GRAYSKULL!", 6,LabelYellow
           'DispDmd1.Text ="[f3][xc][y5]By the POWER of[y18]GRAYSKULL!"
           'DispDmd2.Text ="[f3][xc][y5]By the POWER of[y18]GRAYSKULL!"
		case 2
			  HeManMultiballTimer.Interval = 3200
				HeManMultiballTimer.Enabled = True
			pDMDSplashLines "You Have the","POWER!!!", 3.2,LabelYellow
			DMD_CancelRendering
			DMD_DisplayScene "You Have the","POWER!!!", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
           'DispDmd1.Text ="[f3][xc][y5]You Have the[y18]POWER!!!"
           'DispDmd2.Text ="[f3][xc][y5]You Have the[y18]POWER!!!"
		case 3
			  HeManMultiballTimer.Interval = 2500
				HeManMultiballTimer.Enabled = True
			DMD_CancelRendering
			DMD_DisplayScene "HE-MAN MULTIBALL","500,000", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
			pDMDSplashLines "HE-MAN MULTIBALL","500,000", 2.5,LabelYellow
           'DispDmd1.Text ="[f3][xc][y5]HE-MAN MULTIBALL[y18]500,000"
           'DispDmd2.Text ="[f3][xc][y5]HE-MAN MULTIBALL[y18]500,000"
		case 4
			TimerDMD.Enabled = false
			Timeraddzero.Enabled = false
	end select
end sub

Sub hemanleft_timer
If HeMan.roty < 170 Then
		HeMan.roty = HeMan.roty +1		
	Else
		hemanleft.enabled=False
	ENd If
End Sub

Sub hemanright_timer
If HeMan.roty > 65 Then
		HeMan.roty = HeMan.roty -1		
			
	Else
		
		hemanright.enabled=False
		
	ENd If
		
End Sub

Sub hemanCenter_timer
If HeMan.roty < 122 Then
		HeMan.roty = HeMan.roty +1		
			
	Else
		
		hemanCenter.enabled=False
		
	ENd If

End Sub

sub Hemantimer1_Timer()
Hemantimer1.Enabled =  FALSE
hemanleft.interval = 3
hemanleft.enabled = true
Hemantimer2.Interval = 550
Hemantimer2.Enabled = True
playsoundAtVol "motormoving", HeMan, VolHeman
DOFLinx_HeMan_Rotate() 'DOFLinx_HeMan_Rotate
End Sub

sub Hemantimer2_Timer()
Hemantimer2.Enabled = FALSE
'HeMan.roty = 170', 250
Hemantimer3.Interval = 200
Hemantimer3.Enabled = True
HeManKicker.Kick 70, 60
PlaysoundAtVol "fx_BallRelease", HeManKicker, VolKick ' TODO - Added - Thal
DOFLinx_HeManKicker() '' ' DOFLinx - HeManKicker
flashforms flasher13f, 400, 50, LightStateOff
flashforms bulb26, 400, 50, LightStateOff
End Sub

sub Hemantimer3_Timer()
Hemantimer3.Enabled =  FALSE
'HeMan.roty = 75', 45
Hemantimer4.Interval = 160
Hemantimer4.Enabled = 160
'playsound "rtargethit01" ' TODO - expected to be HeMan, VolHeman
playsoundatvol "rtargethit01", HeMan, VolHeman
playsound "HeManHUAH"
playsound "HeManHUAH"
Playsound "sword03"
Playsound "sword03"
hemanright.interval = 1
hemanright.enabled = true
End Sub

sub Hemantimer4_Timer()
Hemantimer4.Enabled = FALSE
hemanCenter.interval=5
hemancenter.enabled=true
End Sub
'************Skeletor SCRIPT**************************************************************

sub SkeletorLock_hit()
DOFLinx_SkeletorLock_hit() ' DOFLinx - SkeletorLock_hit
if skeletorhitbulb.state = LightStateOn and ballsonplayfield = 1 then
skeletorhitbulb.state = LightStateOff
Rotontimer1.Interval = 1
Rotontimer1.Enabled = True
Rotontimer2.Interval = 150
Rotontimer2.Enabled = True
Rotontimer3.Interval = 300
Rotontimer3.Enabled = True
Hitagain.Interval = 500
Hitagain.Enabled = True
flashforms bulb21, 1000, 50, LightStateOff
flashforms bulb22, 1000, 50, LightStateOff
flashforms bulb23, 1000, 50, LightStateOff
flashforms bulb24, 1000, 50, LightStateOff
flashforms flasher2f, 1000, 500, LightStateOff
flashforms flasher6f, 1000, 500, LightStateOff

SkeletorLock.destroyball
plungerkicker.createball
PlungerKicker.kick 90, 10
PlaysoundAtVol "fx_BallRelease", SkeletorLock, VolKick
addscore (1000000)
playsound "rballfall"
BankUp.Interval = 60
BankUp.Enabled = True

plungertimer.Interval = 1000
plungertimer.Enabled = True
fltimer1.Interval = 1
fltimer1.Enabled = True
fltimer2.Interval = 2000
fltimer2.Enabled = True
scoreupdate = false
playsound "skeletor stunray"
playsound "skeletor stunray"
Videos_Skeletor_Stunray()  'Play Skeletor_Stunray Video
flushdmdtimer.Interval = 3800
flushdmdtimer.Enabled = True
'Bulb8.state = LightStateBlinking
flashbulb5t.Enabled = True
end if

if skeletorhitbulb.state = LightStateOn and ballsonplayfield = 9 then
addscore (2500000)
SkeletorLock.Kick 180, 20
DOFLinx_SkeletorLock() '' ' DOFLinx - SkeletorLock

skeletorhitbulb.state = LightStateOff
Rotontimer1.Interval = 1
Rotontimer1.Enabled = True
Rotontimer2.Interval = 150
Rotontimer2.Enabled = True
Rotontimer3.Interval = 300
Rotontimer3.Enabled = True
Hitagain.Interval = 500
Hitagain.Enabled = True
flashforms bulb21, 1000, 50, LightStateOff
flashforms bulb22, 1000, 50, LightStateOff
flashforms bulb23, 1000, 50, LightStateOff
flashforms bulb24, 1000, 50, LightStateOff
flashforms flasher2f, 1000, 500, LightStateOff
flashforms flasher6f, 1000, 500, LightStateOff
end if

if skeletorhitbulb.state = LightStateOn and ballsonplayfield = 8 then
addscore (2500000)
SkeletorLock.Kick 180, 20
DOFLinx_SkeletorLock() '' ' DOFLinx - SkeletorLock

skeletorhitbulb.state = LightStateOff
Rotontimer1.Interval = 1
Rotontimer1.Enabled = True
Rotontimer2.Interval = 150
Rotontimer2.Enabled = True
Rotontimer3.Interval = 300
Rotontimer3.Enabled = True
Hitagain.Interval = 500
Hitagain.Enabled = True
flashforms bulb21, 1000, 50, LightStateOff
flashforms bulb22, 1000, 50, LightStateOff
flashforms bulb23, 1000, 50, LightStateOff
flashforms bulb24, 1000, 50, LightStateOff
flashforms flasher2f, 1000, 500, LightStateOff
flashforms flasher6f, 1000, 500, LightStateOff
end if

if skeletorhitbulb.state = LightStateOn and ballsonplayfield = 7 then
addscore (2500000)
SkeletorLock.Kick 180, 20
DOFLinx_SkeletorLock() '' ' DOFLinx - SkeletorLock

skeletorhitbulb.state = LightStateOff
Rotontimer1.Interval = 1
Rotontimer1.Enabled = True
Rotontimer2.Interval = 150
Rotontimer2.Enabled = True
Rotontimer3.Interval = 300
Rotontimer3.Enabled = True
Hitagain.Interval = 500
Hitagain.Enabled = True
flashforms bulb21, 1000, 50, LightStateOff
flashforms bulb22, 1000, 50, LightStateOff
flashforms bulb23, 1000, 50, LightStateOff
flashforms bulb24, 1000, 50, LightStateOff
flashforms flasher2f, 1000, 500, LightStateOff
flashforms flasher6f, 1000, 500, LightStateOff
end if

if skeletorhitbulb.state = LightStateOn and ballsonplayfield = 6 then
addscore (2500000)
SkeletorLock.Kick 180, 20
DOFLinx_SkeletorLock() '' ' DOFLinx - SkeletorLock

skeletorhitbulb.state = LightStateOff
Rotontimer1.Interval = 1
Rotontimer1.Enabled = True
Rotontimer2.Interval = 150
Rotontimer2.Enabled = True
Rotontimer3.Interval = 300
Rotontimer3.Enabled = True
Hitagain.Interval = 500
Hitagain.Enabled = True
flashforms bulb21, 1000, 50, LightStateOff
flashforms bulb22, 1000, 50, LightStateOff
flashforms bulb23, 1000, 50, LightStateOff
flashforms bulb24, 1000, 50, LightStateOff
flashforms flasher2f, 1000, 500, LightStateOff
flashforms flasher6f, 1000, 500, LightStateOff
end if

if skeletorhitbulb.state = LightStateOn and ballsonplayfield = 5 then
addscore (2500000)
SkeletorLock.Kick 180, 20
DOFLinx_SkeletorLock() '' ' DOFLinx - SkeletorLock

skeletorhitbulb.state = LightStateOff
Rotontimer1.Interval = 1
Rotontimer1.Enabled = True
Rotontimer2.Interval = 150
Rotontimer2.Enabled = True
Rotontimer3.Interval = 300
Rotontimer3.Enabled = True
Hitagain.Interval = 500
Hitagain.Enabled = True
flashforms bulb21, 1000, 50, LightStateOff
flashforms bulb22, 1000, 50, LightStateOff
flashforms bulb23, 1000, 50, LightStateOff
flashforms bulb24, 1000, 50, LightStateOff
flashforms flasher2f, 1000, 500, LightStateOff
flashforms flasher6f, 1000, 500, LightStateOff
end if

if skeletorhitbulb.state = LightStateOn and ballsonplayfield = 4 then
addscore (2500000)
SkeletorLock.Kick 180, 20
DOFLinx_SkeletorLock() '' ' DOFLinx - SkeletorLock

skeletorhitbulb.state = LightStateOff
Rotontimer1.Interval = 1
Rotontimer1.Enabled = True
Rotontimer2.Interval = 150
Rotontimer2.Enabled = True
Rotontimer3.Interval = 300
Rotontimer3.Enabled = True
Hitagain.Interval = 500
Hitagain.Enabled = True
flashforms bulb21, 1000, 50, LightStateOff
flashforms bulb22, 1000, 50, LightStateOff
flashforms bulb23, 1000, 50, LightStateOff
flashforms bulb24, 1000, 50, LightStateOff
flashforms flasher2f, 1000, 500, LightStateOff
flashforms flasher6f, 1000, 500, LightStateOff
end if

if skeletorhitbulb.state = LightStateOn and ballsonplayfield = 3 then
addscore (2000000)
SkeletorLock.Kick 180, 20
DOFLinx_SkeletorLock() '' ' DOFLinx - SkeletorLock

skeletorhitbulb.state = LightStateOff
Rotontimer1.Interval = 1
Rotontimer1.Enabled = True
Rotontimer2.Interval = 150
Rotontimer2.Enabled = True
Rotontimer3.Interval = 300
Rotontimer3.Enabled = True
Hitagain.Interval = 500
Hitagain.Enabled = True
flashforms bulb21, 1000, 50, LightStateOff
flashforms bulb22, 1000, 50, LightStateOff
flashforms bulb23, 1000, 50, LightStateOff
flashforms bulb24, 1000, 50, LightStateOff
flashforms flasher2f, 1000, 500, LightStateOff
flashforms flasher6f, 1000, 500, LightStateOff
end if

if skeletorhitbulb.state = LightStateOn and ballsonplayfield = 2 then
addscore (1500000)
SkeletorLock.Kick 180, 20
DOFLinx_SkeletorLock() '' ' DOFLinx - SkeletorLock

skeletorhitbulb.state = LightStateOff
Rotontimer1.Interval = 1
Rotontimer1.Enabled = True
Rotontimer2.Interval = 150
Rotontimer2.Enabled = True
Rotontimer3.Interval = 300
Rotontimer3.Enabled = True
Hitagain.Interval = 500
Hitagain.Enabled = True
flashforms bulb21, 1000, 50, LightStateOff
flashforms bulb22, 1000, 50, LightStateOff
flashforms bulb23, 1000, 50, LightStateOff
flashforms bulb24, 1000, 50, LightStateOff
flashforms flasher2f, 1000, 500, LightStateOff
flashforms flasher6f, 1000, 500, LightStateOff
end if

if skeletorhitbulb.state = LightStateOff and ballsonplayfield = 9 then
SkeletorLock.Kick 180, 20
DOFLinx_SkeletorLock() '' ' DOFLinx - SkeletorLock
end if

if skeletorhitbulb.state = LightStateOff and ballsonplayfield = 8 then
SkeletorLock.Kick 180, 20
DOFLinx_SkeletorLock() '' ' DOFLinx - SkeletorLock
end if

if skeletorhitbulb.state = LightStateOff and ballsonplayfield = 7 then
SkeletorLock.Kick 180, 20
end if

if skeletorhitbulb.state = LightStateOff and ballsonplayfield = 6 then
SkeletorLock.Kick 180, 20
DOFLinx_SkeletorLock() '' ' DOFLinx - SkeletorLock
end if

if skeletorhitbulb.state = LightStateOff and ballsonplayfield = 5 then
SkeletorLock.Kick 180, 20
DOFLinx_SkeletorLock() '' ' DOFLinx - SkeletorLock
end if

if skeletorhitbulb.state = LightStateOff and ballsonplayfield = 4 then
SkeletorLock.Kick 180, 20
DOFLinx_SkeletorLock() '' ' DOFLinx - SkeletorLock
end if

if skeletorhitbulb.state = LightStateOff and ballsonplayfield = 3 then
SkeletorLock.Kick 180, 20
DOFLinx_SkeletorLock() '' ' DOFLinx - SkeletorLock
end if

if skeletorhitbulb.state = LightStateOff and ballsonplayfield = 2 then
SkeletorLock.Kick 180, 20
DOFLinx_SkeletorLock() '' ' DOFLinx - SkeletorLock
end if

if skeletorhitbulb.state = LightStateOff and ballsonplayfield = 1 then
SkeletorLock.Kick 180, 20
DOFLinx_SkeletorLock() '' ' DOFLinx - SkeletorLock
end if

end sub


sub rubber20_hit()
DOFLinx_Skeletor_Hit_Left()  'DOFLinx_Skeletor_Hit_Left
if skeletorhitbulb.state = LightStateOn then
addscore (500000)
skeletorhitbulb.state = LightStateOff
Rotontimer1.Interval = 1
Rotontimer1.Enabled = True
Rotontimer2.Interval = 150
Rotontimer2.Enabled = True
Rotontimer3.Interval = 300
Rotontimer3.Enabled = True
Hitagain.Interval = 500
Hitagain.Enabled = True
flashforms bulb21, 1000, 50, LightStateOff
flashforms bulb22, 1000, 50, LightStateOff
flashforms bulb23, 1000, 50, LightStateOff
flashforms bulb24, 1000, 50, LightStateOff
flashforms flasher2f, 1000, 500, LightStateOff
flashforms flasher6f, 1000, 500, LightStateOff
end if
end sub

sub rubber21_hit()
DOFLinx_Skeletor_Hit_Right()   'DOFLinx_Skeletor_Hit_Right
if skeletorhitbulb.state = LightStateOn then
addscore (500000)
skeletorhitbulb.state = LightStateOff
Rotontimer1.Interval = 1
Rotontimer1.Enabled = True
Rotontimer2.Interval = 150
Rotontimer2.Enabled = True
Rotontimer3.Interval = 300
Rotontimer3.Enabled = True
Hitagain.Interval = 500
Hitagain.Enabled = True
flashforms bulb21, 1000, 50, LightStateOff
flashforms bulb22, 1000, 50, LightStateOff
flashforms bulb23, 1000, 50, LightStateOff
flashforms bulb24, 1000, 50, LightStateOff
flashforms flasher2f, 1000, 500, LightStateOff
flashforms flasher6f, 1000, 500, LightStateOff
end if
end sub

sub Hitagain_Timer()
Hitagain.Enabled = false
skeletorhitbulb.state = LightStateOn
end sub


sub guide27_hit()
playsoundatvol "rampclonk02", Recognizer3Bank1, 1
end sub

sub S1Target_hit()

if S1.state = LightStateBlinking and S2.state = LightStateOn and S3.state = LightStateOn and flashbulb5t.Enabled = True and ballsonplayfield = 1 then
SkeletorMB_Mode_Active=True
End If

if S1.state = LightStateBlinking and S2.state = LightStateBlinking and S3.state = LightStateBlinking and ballsonplayfield = 1 then
PlaySound "amifrightened"

Videos_Skeletor_Frightened()  'Play Skeletor_Frightened Video
flushdmdtimer.Interval = 2420
flushdmdtimer.Enabled = True
end if

if S1.state = LightStateBlinking and S2.state = LightStateOn and S3.state = LightStateBlinking and ballsonplayfield = 1 then
PlaySound "amifrightened"

Videos_Skeletor_Frightened()  'Play Skeletor_Frightened Video
flushdmdtimer.Interval = 2420
flushdmdtimer.Enabled = True
end if

if S1.state = LightStateBlinking and S2.state = LightStateBlinking and S3.state = LightStateOn and ballsonplayfield = 1 then
PlaySound "amifrightened"

Videos_Skeletor_Frightened()  'Play Skeletor_Frightened Video
flushdmdtimer.Interval = 2420
flushdmdtimer.Enabled = True
end if

if S1.state = LightStateOn and S2.state = LightStateBlinking and S3.state = LightStateBlinking and ballsonplayfield = 1 then
PlaySound "amifrightened"

Videos_Skeletor_Frightened()  'Play Skeletor_Frightened Video
flushdmdtimer.Interval = 2420
flushdmdtimer.Enabled = True
end if

if S1.state = LightStateOn and S2.state = LightStateBlinking and S3.state = LightStateOn and ballsonplayfield = 1 then
PlaySound "amifrightened"

Videos_Skeletor_Frightened()  'Play Skeletor_Frightened Video
flushdmdtimer.Interval = 2420
flushdmdtimer.Enabled = True
end if

if S1.state = LightStateOn and S2.state = LightStateOn and S3.state = LightStateBlinking and ballsonplayfield = 1 then
PlaySound "amifrightened"

Videos_Skeletor_Frightened()  'Play Skeletor_Frightened Video
flushdmdtimer.Interval = 2420
flushdmdtimer.Enabled = True
end if

if S1.state = LightStateBlinking and S2.state = LightStateOn and S3.state = LightStateOn and flashbulb5t.Enabled = True and ballsonplayfield = 1 then
S1.state = LightStateOn
Bulb8.state = LightStateOff
flashbulb5t.Enabled = False
addscore (50000)
Bankdown.Interval = 25
Bankdown.Enabled = True
playsound "HeMan05"
playsound "HeMan05"
S4.state = LightStateBlinking
SkeletorLock.createball
kicker1.createball:kicker1.Kick 180, 1
kicker2.createball:kicker2.Kick 180, 1
kicker3.createball:kicker3.Kick 180, 1
kicker4.createball:kicker4.Kick 180, 1
kicker5.createball:kicker5.Kick 180, 1
kicker6.createball:kicker6.Kick 180, 1
kicker7.createball:kicker7.Kick 180, 1
SkeletorLock.Kick 180, 20
DOFLinx_SkeletorLock() '' ' DOFLinx - SkeletorLock
DOFLinx_Skeletor_MultiBall()  'DOFLinx_Skeletor_MultiBall
Videos_Skeletor_MB()  'Play the Skeletor_MB Video
ballsonplayfield = 9
fltimer1.Interval = 1
fltimer1.Enabled = True
fltimer2.Interval = 2000
fltimer2.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SKELETOR","MULTIBALL", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SKELETOR","MULTIBALL", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5][b]SKELETOR[y18]MULTIBALL[/b]", deScrollDown, 2000, True
'DispDmd2.QueueText "[f3][xc][y5][b]SKELETOR[y18]MULTIBALL[/b]", deScrollDown, 2000, True
Jackpot1.state = LightStateBlinking
Jackpot2.state = LightStateBlinking
Jackpot3.state = LightStateBlinking
Jackpot4.state = LightStateBlinking
SpecialLeft.state = LightStateOn
SpecialLeft2.state = LightStateOn
SpecialRight.state = LightStateOn
SpecialRight2.state = LightStateOn
Kickbackdiverter.IsDropped = True
'playmusic "MULTIBALL Skeletor.mp3"
myPlayMusicForMode(8)
duringMULTIBALL.play seqalloff
'hologram.frame 94, 124, 94
end if


if S1.state = LightStateBlinking and S2.state = LightStateOn and S3.state = LightStateOn and ballsonplayfield = 1 then
S1.state = LightStateOn
addscore (50000)
Bankdown.Interval = 25
Bankdown.Enabled = True
playsound "skeletorlaughs2"

Videos_Skeletor_Laughing()  'Play Skeletor_Laughing Video
flushdmdtimer.Interval = 3500
flushdmdtimer.Enabled = True
S4.state = LightStateBlinking
end if
if S1.state = LightStateOn then
addscore (10000)
playsound "laser04"
end if
if S1.state = LightStateBlinking and ballsonplayfield = 1 then
playsound "laser04"
S1.state = LightStateOn
addscore (10000)
end if
DOFLinx_Skeletor_Targets_Hit() 'DOFLinx_Skeletor_Targets_Hit
end sub



sub S2Target_hit()

if S1.state = LightStateOn and S2.state = LightStateBlinking and S3.state = LightStateOn and flashbulb5t.Enabled = True and ballsonplayfield = 1 then
SkeletorMB_Mode_Active=True
End If

if S1.state = LightStateBlinking and S2.state = LightStateBlinking and S3.state = LightStateBlinking and ballsonplayfield = 1 then
PlaySound "trymypatiance"
Videos_Skeletor_Patience()  'Play Skeletor_Patience Video
flushdmdtimer.Interval = 2500
flushdmdtimer.Enabled = True
end if

if S1.state = LightStateOn and S2.state = LightStateBlinking and S3.state = LightStateBlinking and ballsonplayfield = 1 then
PlaySound "trymypatiance"

Videos_Skeletor_Patience()  'Play Skeletor_Patience Video
flushdmdtimer.Interval = 2500
flushdmdtimer.Enabled = True
end if

if S1.state = LightStateBlinking and S2.state = LightStateBlinking and S3.state = LightStateOn and ballsonplayfield = 1 then
PlaySound "trymypatiance"
Videos_Skeletor_Patience()  'Play Skeletor_Patience Video
flushdmdtimer.Interval = 2500
flushdmdtimer.Enabled = True
end if

if S1.state = LightStateBlinking and S2.state = LightStateOn and S3.state = LightStateBlinking and ballsonplayfield = 1 then
PlaySound "trymypatiance"
Videos_Skeletor_Patience()  'Play Skeletor_Patience Video
flushdmdtimer.Interval = 2500
flushdmdtimer.Enabled = True
end if

if S1.state = LightStateBlinking and S2.state = LightStateOn and S3.state = LightStateOn and ballsonplayfield = 1 then
PlaySound "trymypatiance"
Videos_Skeletor_Patience()  'Play Skeletor_Patience Video
flushdmdtimer.Interval = 2500
flushdmdtimer.Enabled = True
end if

if S1.state = LightStateOn and S2.state = LightStateOn and S3.state = LightStateBlinking and ballsonplayfield = 1 then
PlaySound "trymypatiance"
Videos_Skeletor_Patience()  'Play Skeletor_Patience Video
flushdmdtimer.Interval = 2500
flushdmdtimer.Enabled = True
end if

if S1.state = LightStateOn and S2.state = LightStateBlinking and S3.state = LightStateOn and flashbulb5t.Enabled = True and ballsonplayfield = 1 then
S2.state = LightStateOn
Bulb8.state = LightStateOff
flashbulb5t.Enabled = False
addscore (50000)
Bankdown.Interval = 25
Bankdown.Enabled = True
playsound "HeMan05"
playsound "HeMan05"
S4.state = LightStateBlinking
SkeletorLock.createball
kicker1.createball:kicker1.Kick 180, 1
kicker2.createball:kicker2.Kick 180, 1
kicker3.createball:kicker3.Kick 180, 1
kicker4.createball:kicker4.Kick 180, 1
kicker5.createball:kicker5.Kick 180, 1
kicker6.createball:kicker6.Kick 180, 1
kicker7.createball:kicker7.Kick 180, 1
SkeletorLock.Kick 180, 20
DOFLinx_SkeletorLock() '' ' DOFLinx - SkeletorLock
DOFLinx_Skeletor_MultiBall()  'DOFLinx_Skeletor_MultiBall
Videos_Skeletor_MB()  'Play the Skeletor_MB Video
ballsonplayfield = 9
fltimer1.Interval = 2000
fltimer1.Enabled = True
fltimer2.Interval = 2000
fltimer2.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SKELETOR","MULTIBALL", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SKELETOR","MULTIBALL", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5][b]SKELETOR[y18]MULTIBALL[/b]", deScrollDown, 2000, True
'DispDmd2.QueueText "[f3][xc][y5][b]SKELETOR[y18]MULTIBALL[/b]", deScrollDown, 2000, True
Jackpot1.state = LightStateBlinking
Jackpot2.state = LightStateBlinking
Jackpot3.state = LightStateBlinking
Jackpot4.state = LightStateBlinking
SpecialLeft.state = LightStateOn
SpecialLeft2.state = LightStateOn
SpecialRight.state = LightStateOn
SpecialRight2.state = LightStateOn
Kickbackdiverter.IsDropped = True
'playmusic "MULTIBALL Skeletor.mp3"
myPlayMusicForMode(8)
duringMULTIBALL.play seqalloff
'hologram.frame 94, 124, 94
end if

if S1.state = LightStateOn and S2.state = LightStateBlinking and S3.state = LightStateOn and ballsonplayfield = 1 then
S2.state = LightStateOn
addscore (50000)
Bankdown.Interval = 25
Bankdown.Enabled = True
playsound "skeletorlaughs2"

Videos_Skeletor_Laughing()  'Play Skeletor_Laughing Video
flushdmdtimer.Interval = 3500
flushdmdtimer.Enabled = True
S4.state = LightStateBlinking
end if

if S2.state = LightStateOn then
addscore (10000)
playsound "laser04"
end if
if S2.state = LightStateBlinking and ballsonplayfield = 1 then
S2.state = LightStateOn
addscore (10000)
playsound "laser04"
end if
DOFLinx_Skeletor_Targets_Hit() 'DOFLinx_Skeletor_Targets_Hit
end sub

sub S3Target_hit()

if S1.state = LightStateOn and S2.state = LightStateOn and S3.state = LightStateBlinking and flashbulb5t.Enabled = True and ballsonplayfield = 1 then
SkeletorMB_Mode_Active=True
End If

if S1.state = LightStateBlinking and S2.state = LightStateBlinking and S3.state = LightStateBlinking and ballsonplayfield = 1 then
PlaySound "backup"

Videos_Skeletor_BackOff()  'Play Skeletor_BackOff Video
flushdmdtimer.Interval = 1700
flushdmdtimer.Enabled = True
end if

if S1.state = LightStateBlinking and S2.state = LightStateOn and S3.state = LightStateBlinking and ballsonplayfield = 1 then
PlaySound "backup"

Videos_Skeletor_BackOff()  'Play Skeletor_BackOff Video
flushdmdtimer.Interval = 1700
flushdmdtimer.Enabled = True
end if

if S1.state = LightStateOn and S2.state = LightStateBlinking and S3.state = LightStateBlinking and ballsonplayfield = 1 then
PlaySound "backup"

Videos_Skeletor_BackOff()  'Play Skeletor_BackOff Video
flushdmdtimer.Interval = 1700
flushdmdtimer.Enabled = True
end if

if S1.state = LightStateBlinking and S2.state = LightStateBlinking and S3.state = LightStateOn and ballsonplayfield = 1 then
PlaySound "backup"

Videos_Skeletor_BackOff()  'Play Skeletor_BackOff Video
flushdmdtimer.Interval = 1700
flushdmdtimer.Enabled = True
end if

if S1.state = LightStateBlinking and S2.state = LightStateOn and S3.state = LightStateOn and ballsonplayfield = 1 then
PlaySound "backup"

Videos_Skeletor_BackOff()  'Play Skeletor_BackOff Video
flushdmdtimer.Interval = 1700
flushdmdtimer.Enabled = True
end if

if S1.state = LightStateOn and S2.state = LightStateBlinking and S3.state = LightStateOn and ballsonplayfield = 1 then
PlaySound "backup"

Videos_Skeletor_BackOff()  'Play Skeletor_BackOff Video
flushdmdtimer.Interval = 1700
flushdmdtimer.Enabled = True
end if

if S1.state = LightStateOn and S2.state = LightStateOn and S3.state = LightStateBlinking and flashbulb5t.Enabled = True and ballsonplayfield = 1 then
S3.state = LightStateOn
Bulb8.state = LightStateOff
flashbulb5t.Enabled = False
addscore (50000)
Bankdown.Interval = 25
Bankdown.Enabled = True
playsound "HeMan05"
playsound "HeMan05"
S4.state = LightStateBlinking
SkeletorLock.createball
kicker1.createball:kicker1.Kick 180, 1
kicker2.createball:kicker2.Kick 180, 1
kicker3.createball:kicker3.Kick 180, 1
kicker4.createball:kicker4.Kick 180, 1
kicker5.createball:kicker5.Kick 180, 1
kicker6.createball:kicker6.Kick 180, 1
kicker7.createball:kicker7.Kick 180, 1
SkeletorLock.Kick 180, 20
DOFLinx_SkeletorLock() '' ' DOFLinx - SkeletorLock
DOFLinx_Skeletor_MultiBall()  'DOFLinx_Skeletor_MultiBall
Videos_Skeletor_MB()  'Play the Skeletor_MB Video
ballsonplayfield = 9
fltimer1.Interval = 1
fltimer1.Enabled = True
fltimer2.Interval = 2000
fltimer2.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SKELETOR","MULTIBALL", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SKELETOR","MULTIBALL", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5][b]SKELETOR[y18]MULTIBALL[/b]", deScrollDown, 2000, True
'DispDmd2.QueueText "[f3][xc][y5][b]SKELETOR[y18]MULTIBALL[/b]", deScrollDown, 2000, True
Jackpot1.state = LightStateBlinking
Jackpot2.state = LightStateBlinking
Jackpot3.state = LightStateBlinking
Jackpot4.state = LightStateBlinking
SpecialLeft.state = LightStateOn
SpecialLeft2.state = LightStateOn
SpecialRight.state = LightStateOn
SpecialRight2.state = LightStateOn
Kickbackdiverter.IsDropped = True
'playmusic "MULTIBALL Skeletor.mp3"
myPlayMusicForMode(8)
duringMULTIBALL.play seqalloff
'hologram.frame 94, 124, 94
end if

if S1.state = LightStateOn and S2.state = LightStateOn and S3.state = LightStateBlinking and ballsonplayfield = 1 then
S3.state = LightStateOn
addscore (50000)
Bankdown.Interval = 25
Bankdown.Enabled = True
S4.state = LightStateBlinking
playsound "skeletorlaughs2"

Videos_Skeletor_Laughing()  'Play Skeletor_Laughing Video
flushdmdtimer.Interval = 3500
flushdmdtimer.Enabled = True
end if

if S3.state = LightStateOn then
addscore (10000)
playsound "laser04"
end if

if S3.state = LightStateBlinking and ballsonplayfield = 1 then
S3.state = LightStateOn
addscore (10000)
playsound "laser04"
end if
DOFLinx_Skeletor_Targets_Hit() 'DOFLinx_Skeletor_Targets_Hit
end sub

Dim omgitspins:omgitspins = True
Sub RotonBlattTimer_Timer()
	RotonBlatt.ObjRotz = RotonBlatt.ObjRotz - 1 'constant spin
End Sub

Sub RoadRipperTyresTimer_Timer()
	RoadRipperTyres.Rotz = RoadRipperTyres.Rotz + 2 'constant spin
End Sub

Sub SpinningDisk1Timer_Timer()
		SpinningDisk1.ObjRotz = SpinningDisk1.ObjRotz - 1 'constant spin
End Sub

'******* Spinning Disc *********



Dim mDISC
    Set mDISC=New cvpmTurnTable
		mDISC.InitTurnTable SpinningDiskTrigger,discspeed
		mDISC.SpinUp=1000
		mDISC.SpinDown=10
		mDISC.CreateEvents"mDISC"

Sub RotationTimer_Timer
	dim temptimer
	if mDisc.speed > 0 Then
		temptimer = int(((1/mDisc.speed)*100)+0.5)
		if temptimer<1 Then
				RotationTimer.interval = 1
			elseif temptimer>100 then
				RotationTimer.interval = 100
			else rotationtimer.interval=temptimer
		end if
		SpinningDisk1.ObjRotz = (SpinningDisk1.ObjRotz + discrotspeed) MOD 360
	  Else
		RotationTimer.interval = 1
	end If
End Sub

'****** Stop SpinDisc ***************


Sub StopDisc
	mDISC.MotorOn = 0
	stopsound "disc_noise"
	stopsound "disc_noise1"
	stopsound "Disc_Noise2"
End Sub

sub BankUP1_Timer()
	If Recognizer3Bank.z < 52 Then
		Recognizer3Bank.z = Recognizer3Bank.z +1
	Else
		BankUP1.enabled=False
	End If
End Sub

sub BankUP_Timer()
BankUP.Enabled = false
BankUP1.enabled = true
playsoundAtVol "motormoving", Recognizer3Bank, 1
BankCOLLIDABLE.Interval = 1500
BankCOLLIDABLE.Enabled =True
mDISC.MotorOn = 0
RotonBlattTimer.Enabled = False
stopSound "ROTONloop"
DOFLinx_Skeletor_BattleMode_End()   'DOFLinx_Skeletor_BattleMode_End
end sub

sub BankDown1_Timer()
	If Recognizer3Bank.z > -10 Then
		Recognizer3Bank.z = Recognizer3Bank.z -1
	Else
		BankDown1.enabled=False
	End If
End Sub

sub BankDOWN_Timer()
BankDOWN.Enabled = false
BankDOWN1.Enabled = True
playsoundAtVol "motormoving", Recognizer3Bank, 1
S1Target.collidable = false
S2Target.collidable = false
S3Target.collidable = false
BankUNCOLLIDABLE.Interval = 1500
BankUNCOLLIDABLE.Enabled =True
SpinningDisk1Timer.Enabled = True
mDISC.MotorOn = 1
RotonBlattTimer.Enabled = True
playSound "ROTONloop", -1
'myPlayMusicForMode(9)
soundtimer.Interval = 30000
soundtimer.Enabled =True
DOFLinx_Skeletor_BattleMode_Start()   'DOFLinx_Skeletor_BattleMode_Start
'end if
end sub

sub soundtimer_Timer()
soundtimer.Enabled = false
end sub

sub BankUNCOLLIDABLE_Timer()
BankUNCOLLIDABLE.Enabled = false
guide27.Isdropped = True
end sub

sub BankCOLLIDABLE_Timer()
BankCOLLIDABLE.Enabled = false
guide27.IsDropped = False
S1Target.collidable = true
S2Target.collidable = true
S3Target.collidable = true
S1.state = LightStateBlinking
S2.state = LightStateBlinking
S3.state = LightStateBlinking
S4.state = LightStateOff
end sub

Sub roton1_timer
If roton.Objroty < 10 Then
		roton.Objroty = roton.Objroty +1		
	Else
		roton1.enabled=False
	ENd If
End Sub

Sub roton2_timer
If roton.Objroty > -10 Then
		roton.Objroty = roton.Objroty -1		
	Else
		roton2.enabled=False
	ENd If
		
End Sub

Sub roton3_timer
If roton.Objroty < 0 Then
		roton.Objroty = roton.Objroty +1		
		Else
		roton3.enabled=False
	ENd If

End Sub


' first = speed, second = angle
sub Rotontimer1_Timer()'right
Rotontimer1.Enabled = false
roton1.interval = 1
roton1.enabled = true
playsound "laser10"
playsound "laser10"

end sub

sub Rotontimer2_Timer()'Left
Rotontimer2.Enabled = false
roton2.interval = 1
roton2.enabled = true
end sub

sub Rotontimer3_Timer()'Center
Rotontimer3.Enabled = false
roton3.interval = 1
roton3.enabled = true
end sub


' *****************RoadRipper SCRIPT******************************************
sub RipperRampTrigger_hit()

flashforms RipperNeonf, 600, 50, LightStateOn'Ripperneonp.visible=ripperneon.state

if R1.state = LightStateOn and R2.state = LightStateOn and R3.state = LightStateOn and R4.state = LightStateBlinking and ballsonplayfield = 1 then
RoadRipperMB_Mode_Active=True
End If

if ballsonplayfield = 9 then
addscore (10000000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","10 Million", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","10 Million", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deWipeIn, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deWipeIn, 2000, True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()  '' ' DOFLinx: Combo - Jackpot - Effects
end if
if ballsonplayfield = 8 then
addscore (10000000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","10 Million", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","10 Million", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deWipeIn, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deWipeIn, 2000, True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()  '' ' DOFLinx: Combo - Jackpot - Effects
end if
if ballsonplayfield = 7 then
addscore (10000000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","10 Million", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","10 Million", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deWipeIn, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deWipeIn, 2000, True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()  '' ' DOFLinx: Combo - Jackpot - Effects
end if
if ballsonplayfield = 6 then
addscore (10000000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","10 Million", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","10 Million", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deWipeIn, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deWipeIn, 2000, True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()  '' ' DOFLinx: Combo - Jackpot - Effects
end if
if ballsonplayfield = 5 then
addscore (10000000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","10 Million", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","10 Million", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deWipeIn, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deWipeIn, 2000, True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()   ' DOFLinx: Combo - Jackpot - Effects
end if
if ballsonplayfield = 4 then
addscore (10000000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","10 Million", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","10 Million", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deWipeIn, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deWipeIn, 2000, True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()  '' ' DOFLinx: Combo - Jackpot - Effects
end if
if ballsonplayfield = 3 then
addscore (1000000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","1 Million", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","1 Million", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]1 Million", deWipeIn, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]1 Million", deWipeIn, 2000, True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()  '' ' DOFLinx: Combo - Jackpot - Effects
end if
if ballsonplayfield = 2 then
addscore (500000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","500,000", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","500,000", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]500,000", deWipeIn, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]500,000", deWipeIn, 2000, True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()  '' ' DOFLinx: Combo - Jackpot - Effects
end if

if R1.state = LightStateOn and R2.state = LightStateOn and R3.state = LightStateOn and R4.state = LightStateBlinking and ballsonplayfield = 1 then
R4.state = LightStateOn
addscore (1000000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 2000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "ROAD RIPPER","MULTIBALL", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "ROAD RIPPER","MULTIBALL", 2,LabelYellow
'DispDmd1.QueueText "[box1,1,1,128,32][edge4][f3][xc][y5][bf]ROAD RIPPER[y18]MULTIBALL[/bf]", denone, 2000, True
'DispDmd2.QueueText "[box1,1,1,128,32][edge4][f3][xc][y5][bf]ROAD RIPPER[y18]MULTIBALL[/bf]", denone, 2000, True
plungerkicker.createball
PlungerKicker.kick 90, 10
plungertimer.Interval = 1000
plungertimer.Enabled = True
ballsonplayfield = ballsonplayfield +1
Jackpot1.state = LightStateBlinking
Jackpot2.state = LightStateBlinking
Jackpot3.state = LightStateBlinking
Jackpot4.state = LightStateBlinking
SpecialLeft.state = LightStateOn
SpecialLeft2.state = LightStateOn
SpecialRight.state = LightStateOn
SpecialRight2.state = LightStateOn
Kickbackdiverter.IsDropped = True
'playmusic "MULTIBALL Ripper.mp3"
myPlayMusicForMode(7)
duringMULTIBALL.play seqalloff
DOFLinx_RipperMB_Start()  'DOFLinx_RipperMB_Start
Videos_Ripper_MB()  'Play the Ripper_MB Video
end if
if R1.state = LightStateOn and R2.state = LightStateOn and R3.state = LightStateBlinking and R4.state = LightStateOff and ballsonplayfield = 1 then
R3.state = LightStateOn
R4.state = LightStateBlinking
addscore (250000)
fltimer1.Interval = 1
fltimer1.Enabled = True
fltimer2.Interval = 1000
fltimer2.Enabled = True

scoreupdate = false
flushdmdtimer.Interval = 2800
flushdmdtimer.Enabled = True
Videos_Road_Ripper_250000()  'Play Road_Ripper 250000 Video
end if
if R1.state = LightStateOn and R2.state = LightStateBlinking and R3.state = LightStateOff and R4.state = LightStateOff and ballsonplayfield = 1 then
R2.state = LightStateOn
R3.state = LightStateBlinking
addscore (100000)
fltimer1.Interval = 1
fltimer1.Enabled = True
fltimer2.Interval = 1000
fltimer2.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2800
flushdmdtimer.Enabled = True
Videos_Road_Ripper_100000()  'Play Road_Ripper 100000 Video
end if
if R1.state = LightStateBlinking and R2.state = LightStateOff and R3.state = LightStateOff and R4.state = LightStateOff and ballsonplayfield = 1 then
R1.state = LightStateOn
R2.state = LightStateBlinking
addscore (50000)
fltimer1.Interval = 1
fltimer1.Enabled = True
fltimer2.Interval = 1000
fltimer2.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2800
flushdmdtimer.Enabled = True
Videos_Road_Ripper_50000()  'Play Road_Ripper 50000 Video
end if
end sub



' *****************RoadRipper Wheelie***(WORKS ALREADY separately)!!!

sub RoadRippertrigger1_hit()
addscore (25000)
if WheelieOn.state = LightStateOn then
WheelieOn.state = LightStateOff
RipperUp.Interval = 1
RipperUp.Enabled = True
RipperDown.Interval = 2500
RipperDown.Enabled = True
WheelieOnTimer.Interval = 2600
WheelieOnTimer.Enabled = 2600
DOFLinx_RoadRipperRamp() 'DOFLinx_RoadRipperRamp
end if
end sub

Sub RoadRipperUp_timer()
If RoadRipper.Rotz < 30 Then
		RoadRipper.Rotz = RoadRipper.Rotz +1		
	Else
		RoadRipperUp.enabled=False
	ENd If
end Sub

sub RipperUP_Timer()
RipperUP.Enabled = False
DOFLinx_Ripper_UP() 'DOFLinx_Ripper_UP
playsound "Wheelie"
RipperMotor.PullBack' (2450)
RipperMotor.Fire
RoadRipperUp.interval = 10
RoadRipperUp.enabled = true
end sub

Sub RoadRipperdown_timer()
If RoadRipper.Rotz > 0 Then
		RoadRipper.Rotz = RoadRipper.Rotz -1		
	Else
		RoadRipperdown.enabled=False
	ENd If
end Sub

sub RipperDown_Timer()
RipperDown.Enabled = false
'RoadRipper.rotz  = 0', 0
playsoundAtVol "rtargethit01", RoadRipper, VolTarg
playsoundAtVol "rtargethit01", RoadRipper, VolTarg
playsoundAtVol "rampclonk01", RoadRipper, VolRamp
playsoundAtVol "rampclonk01", RoadRipper, VolRamp
RoadRipperdown.interval = 5
RoadRipperdown.enabled = true
end sub


sub WheelieOnTimer_Timer()
WheelieOnTimer.Enabled = false
WheelieOn.state = LightStateOn
end sub

sub TyresGo_hit()
TyresStop.Interval = 5000
TyresStop.Enabled = True
RoadRipperTyresTimer.enabled = True
end sub

sub TyresStop_Timer()
TyresStop.Enabled =false
RoadRipperTyresTimer.enabled = False
end sub






' *****************Castle SCRIPT**************************************
sub InitiateCastle_hit()
playsound "laser07"
playsound "laser07"
addscore (25000)
if C1.state = LightStateBlinking then
C1.state = LightStateOn
C2.state = LightStateBlinking
fltimer1.Interval = 1
fltimer1.Enabled = True
fltimer2.Interval =1000
fltimer2.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = true
DMD_CancelRendering
DMD_DisplayScene "CASTLE GRAYSKULL","IS LIT", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "CASTLE GRAYSKULL","IS LIT", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5][b]CASTLE GRAYSKULL[y18]IS LIT[/b]", denone, 2000, True
'DispDmd2.QueueText "[f3][xc][y5][b]CASTLE GRAYSKULL[y18]IS LIT[/b]", denone, 2000, True
DOFLinx_Grayskull_Lit()  'DOFLinx_Grayskull_Lit
Videos_Grayskull_Lit()  'Play Grayskull_Lit Video
flashbulb3t.enabled = True
end if
end sub


sub CastleRampTrigger_hit()

if C2.state = LightStateOn and C3.state = LightStateOn and C4.state = LightStateOn and C5.state = LightStateBlinking and ballsonplayfield = 1 then
CastleMB_Mode_Active=True
End If

	Select Case RandomNumber(3)
		Case 1: flushdmdtimer.Interval =3800
				flushdmdtimer.Enabled =True
				Videos_Sorceress_Swoop()  'Play Sorceress_Swoop Video

		Case 2:  flushdmdtimer.Interval =2300
				flushdmdtimer.Enabled =True
				Videos_Sorceress_Hover()  'Play Sorceress_Hover Video

		Case 3:  flushdmdtimer.Interval = 2500
				flushdmdtimer.Enabled = True
				Videos_Sorceress_Fly()  'Play Sorceress_Fly Video
	End Select

DOFLinx_GraySkullRamp() 'DOFLinx_GraySkullRamp

flashforms CastleNeonf, 600, 50, LightStateOn


if ballsonplayfield = 9 then
addscore (10000000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","10 Million", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","10 Million", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollInVert, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollInVert, 2000, True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()  '' ' DOFLinx: Combo - Jackpot - Effects
playsound "ZoarkScream"
playsound "ZoarkScream"
end if
if ballsonplayfield = 8 then
addscore (10000000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","10 Million", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","10 Million", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollInVert, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollInVert, 2000, True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()  '' ' DOFLinx: Combo - Jackpot - Effects
playsound "ZoarkScream"
playsound "ZoarkScream"
end if
if ballsonplayfield = 7 then
addscore (10000000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","10 Million", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","10 Million", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollInVert, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollInVert, 2000, True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()  '' ' DOFLinx: Combo - Jackpot - Effects
playsound "ZoarkScream"
playsound "ZoarkScream"
end if
if ballsonplayfield = 6 then
addscore (10000000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","10 Million", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","10 Million", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollInVert, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollInVert, 2000, True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()  '' ' DOFLinx: Combo - Jackpot - Effects
playsound "ZoarkScream"
playsound "ZoarkScream"
end if
if ballsonplayfield = 5 then
addscore (10000000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","10 Million", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","10 Million", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollInVert, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollInVert, 2000, True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()  '' ' DOFLinx: Combo - Jackpot - Effects
playsound "ZoarkScream"
playsound "ZoarkScream"
end if
if ballsonplayfield = 4 then
addscore (10000000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","10 Million", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","10 Million", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollInVert, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]10 Million", deScrollInVert, 2000, True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()  '' ' DOFLinx: Combo - Jackpot - Effects
playsound "ZoarkScream"
playsound "ZoarkScream"
end if
if ballsonplayfield = 3 then
addscore (1000000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","1 Million", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","1 Million", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]1 Million", deScrollInVert, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]1 Million", deScrollInVert, 2000, True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()  '' ' DOFLinx: Combo - Jackpot - Effects
playsound "ZoarkScream"
playsound "ZoarkScream"
end if
if ballsonplayfield = 2 then
addscore (500000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 1000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "SUPER JACKPOT","500.000", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "SUPER JACKPOT","500,000", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]500.000", deScrollInVert, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]SUPER JACKPOT[y18]500,000", deScrollInVert, 2000, True
playsound "Jackpot01"
playsound "Jackpot01"
Videos_Jackpot_All_Modes()   'PuP - Jackpot All Modes
DOFLinx_Combo_Jackpot()  '' ' DOFLinx: Combo - Jackpot - Effects
playsound "ZoarkScream"
playsound "ZoarkScream"
end if

if C2.state = LightStateOn and C3.state = LightStateOn and C4.state = LightStateOn and C5.state = LightStateBlinking and ballsonplayfield = 1 then
C5.state = LightStateOn
EB1.state = LightStateBlinking
EB2.state = LightStateBlinking
CastleDown.Interval = 20
CastleDown.Enabled = True
addscore (500000)
fltimerA.Interval = 1
fltimerA.Enabled = True
fltimerB.Interval = 2000
fltimerB.Enabled = True
scoreupdate = false
flushdmdtimer.Interval = 2000
flushdmdtimer.Enabled = True
DMD_CancelRendering
DMD_DisplayScene "CASTLE","MULTIBALL", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
pDMDSplashLines "CASTLE","MULTIBALL", 2,LabelYellow
'DispDmd1.QueueText "[f3][xc][y5]CASTLE[y18]MULTIBALL", deScrollUp, 2000, True
'DispDmd2.QueueText "[f3][xc][y5]CASTLE[y18]MULTIBALL", deScrollUp, 2000, True
plungerkicker.createball
PlungerKicker.kick 90, 10
plungertimer.Interval = 1000
plungertimer.Enabled = True
ballsonplayfield = ballsonplayfield +1
Jackpot1.state = LightStateBlinking
Jackpot2.state = LightStateBlinking
Jackpot3.state = LightStateBlinking
Jackpot4.state = LightStateBlinking
SpecialLeft.state = LightStateOn
SpecialLeft2.state = LightStateOn
SpecialRight.state = LightStateOn
SpecialRight2.state = LightStateOn
Kickbackdiverter.IsDropped = True
'playmusic "HeManAltern.mp3"
myPlayMusicForMode(3)
flasher11t.enabled = True
diffquoteC.state = LightStateOn
duringMULTIBALL.play seqalloff
'hologram.frame 233, 322, 233
DOFLinx_CastleMB_Start()  'DOFLinx_CastleMB_Start
Videos_Hey_Yay_Loop()  'Play the Hey_Yay (Castle MB) Video Looped
end if

if C2.state = LightStateOn and C3.state = LightStateOn and C4.state = LightStateBlinking and C5.state = LightStateOff and ballsonplayfield = 1 and diffquoteC.state = LightStateOn then
C4.state = LightStateOn
C5.state = LightStateBlinking
CastleUp.Interval = 20
CastleUp.Enabled = True
addscore (250000)
fltimer1.Interval = 1
fltimer1.Enabled = True
fltimer2.Interval = 1000
fltimer2.Enabled = True
scoreupdate = false
'bulb11.state = LightStateBlinking
flashbulb4t.Enabled = True
playsound "skeletor09"
playsound "skeletor09"
Videos_Castle_Open_250000()  'Play Castle_Open 250,000 Video
flushdmdtimer.Interval = 3800
flushdmdtimer.Enabled = True
end if

if C2.state = LightStateOn and C3.state = LightStateOn and C4.state = LightStateBlinking and C5.state = LightStateOff and ballsonplayfield = 1 and diffquoteC.state = LightStateOff then
C4.state = LightStateOn
C5.state = LightStateBlinking
CastleUp.Interval = 20
CastleUp.Enabled = True
addscore (250000)
fltimer1.Interval = 1
fltimer1.Enabled = True
fltimer2.Interval = 1000
fltimer2.Enabled = True
scoreupdate = false
'bulb11.state = LightStateBlinking'
flashbulb4t.Enabled = True
playsound "TeRah"
playsound "TeRah"
Videos_Castle_Open_250000()  'Play Castle_Open 250,000 Video
flushdmdtimer.Interval = 3800
flushdmdtimer.Enabled = True
end if

if C2.state = LightStateOn and C3.state = LightStateBlinking and C4.state = LightStateOff and C5.state = LightStateOff and ballsonplayfield = 1 and diffquoteC.state = LightStateOn then
C3.state = LightStateOn
C4.state = LightStateBlinking
CastleDown.Interval = 20
CastleDown.Enabled = True
addscore (100000)
fltimer1.Interval = 1
fltimer1.Enabled = True
fltimer2.Interval = 1000
fltimer2.Enabled = True
scoreupdate = false
playsound "skeletor07"
playsound "skeletor07"
Videos_Castle_Close_100000()  'Play Castle_Close Video
end if

if C2.state = LightStateOn and C3.state = LightStateBlinking and C4.state = LightStateOff and C5.state = LightStateOff and ballsonplayfield = 1 and diffquoteC.state = LightStateOff then
C3.state = LightStateOn
C4.state = LightStateBlinking
CastleDown.Interval = 20
CastleDown.Enabled = True
addscore (100000)
fltimer1.Interval = 1
fltimer1.Enabled = True
fltimer2.Interval = 1000
fltimer2.Enabled = True
scoreupdate = false
playsound "HeMan06"
playsound "HeMan06"
Videos_Castle_Close_100000()  'Play Castle_Close Video
flushdmdtimer.Interval = 3800
flushdmdtimer.Enabled = True
end if

if C2.state = LightStateBlinking and C3.state = LightStateOff and C4.state = LightStateOff and C5.state = LightStateOff and ballsonplayfield = 1 and diffquoteC.state = LightStateOn then
C2.state = LightStateOn
C3.state = LightStateBlinking
Castleup.Interval = 20
Castleup.Enabled = True
addscore (50000)
fltimer1.Interval = 1
fltimer1.Enabled = True
fltimer2.Interval = 1000
fltimer2.Enabled = True
scoreupdate = false
playsound "skeletor02"
playsound "skeletor02"
Videos_Castle_Open_50000()  'Play Castle_Open 50,000 Video
flushdmdtimer.Interval = 3800
flushdmdtimer.Enabled = True
end if

if C2.state = LightStateBlinking and C3.state = LightStateOff and C4.state = LightStateOff and C5.state = LightStateOff and ballsonplayfield = 1 and diffquoteC.state = LightStateOff then
C2.state = LightStateOn
C3.state = LightStateBlinking
CastleUp.Interval = 20
CastleUp.Enabled = True
addscore (50000)
fltimer1.Interval = 1
fltimer1.Enabled = True
fltimer2.Interval = 1000
fltimer2.Enabled = True
scoreupdate = false
playsound "HeMan02"
playsound "HeMan02"
Videos_Castle_Open_50000()  'Play Castle_Open 50,000 Video
flushdmdtimer.Interval = 3800
flushdmdtimer.Enabled = True
end if

if C2.state = LightStateOff and ballsonplayfield = 1 then
playsound "ZoarkScream"
playsound "ZoarkScream"
playsound "thunderlong"
playsound "thunderlong"
addscore (15000)
flashforms  flasher10f, 1200, 150, LightStateOff
end if
end sub


sub ControlON_Timer()
ControlOn.Enabled = false
CastleControl.state = LightStateOn
end sub

sub ControlBlink_Timer()
ControlBlink.Enabled = false
CastleControl.state = LightStateBlinking
end sub

Sub CastleUp1_Timer
If CASTLE.TransY < -227 Then
		CASTLE.TransY = CASTLE.TransY +1
	Else
		CASTLEup1.enabled=False
End If
End Sub

sub CASTLEup_Timer()
CASTLEup.Enabled = false
CASTLEup1.Enabled = True
playsoundAtVol "motormoving", Castle, 1
DOFLinx_Grayskull_RaiseUp()  'DOFLinx_Grayskull_RaiseUp
playsound "stonefall"
playsound "thunderlong"
CASTLEcollidewall.collidable = true
CASTLEcollidewall2.collidable = true
end sub

Sub CastleDown1_Timer
If CASTLE.TransY > -427 Then
		CASTLE.TransY = CASTLE.TransY -1
	Else
		CASTLEDown1.enabled=False
End If
End Sub

sub CASTLEdown_Timer()
CASTLEdown.Enabled = false
CASTLEdown1.Enabled = True
playsoundAtVol "motormoving", Castle, 1


CASTLEdown.Enabled = false
DOFLinx_Grayskull_LowerDown()  'DOFLinx_Grayskull_LowerDown
CASTLEnotcollide.Interval =  2000
CASTLEnotcollide.Enabled = true
'End If
end sub

sub CASTLEnotcollide_Timer()
CASTLEnotcollide.Enabled = false
CASTLEcollidewall.collidable = false
CASTLEcollidewall2.collidable = false

end sub

sub CASTLEcollidewall_hit()
playsoundatvol "rampclonk01", BulbLock4, VolRamp
end sub

sub CASTLEcollidewall2_hit()
playsoundatvol "rampclonk02", BulbLock5, VolRamp
end sub




sub plungerbulboff_hit()
if flashbulb1t.enabled = True then
flashbulb1t.enabled = False
plungerbulbfv.visible = false
Plungerbulb.state = 0
plungerbulbbulb.blenddisablelighting = 0

end if
end sub


sub trigger24_hit()
if C2.state = LightStateOff then
playsound "CreekScream"
playsound "CreekScream"
flushdmdtimer.Interval = 2300
flushdmdtimer.Enabled = True
end if
if Jackpot2.state = LightStateBlinking then
playsound "CreekScream"
playsound "CreekScream"
end if
end sub

sub trigger1_hit()
flashforms RipperNeonf, 600, 50, LightStateOff
RipperNeonf.visible= false
PlaySoundAtVol "fx_BallDropl", Trigger1, 1
end sub

sub trigger3_hit()
flashforms CastleNeonf, 600, 50, LightStateOff
CastleNeonf.visible= false
PlaySoundAtVol "fx_BallDropr", Trigger3, 1
end sub


sub Gate1_hit()
playsoundAtVol "fx_GateHit", Gate1, VolGates
end sub

Sub DispDmd1_Empty()
			if (vpGameInPlay = TRUE) and (Dmdinuse = FALSE) and (vpTiltWarnings=FALSE)then
                  'DispDmd1.Text = "[f5][xc][y3]" & displayscore & "[f1] [x0] [y27] Balls " & (BallsRemaining(CurrentPlayer)) & " [x72] [y27] Credits "& nvCredits & "[x50][y27]P:" & currentplayer
						'DispDmd2.Text = "[f5][xc][y3]" & displayscore & "[f1] [x0] [y27] Balls " & (BallsRemaining(CurrentPlayer)) & " [x72] [y27] Credits "& nvCredits & "[x50][y27]P:" & currentplayer
         end if
End Sub



'-----------------------------------------------------------------------------

'									HIGH SCORE COMMON DYSPLAY RUTTINE
'-----------------------------------------------------------------------------

dim miletraes'Selected initial
dim letra1'Name Initial 1
dim letra2'Name Initial 2
dim letra3'Name Initial 3
dim letra4'END letter

DIM ADER1
DIM ADER2
DIM ADER3
DIM ADER4
DIM ADER5
DIM ADER6
DIM ADER7
DIM AIZQ1
DIM AIZQ2
DIM AIZQ3
DIM AIZQ4
DIM AIZQ5
DIM AIZQ6
DIM AIZQ7

dim benterhighscoremode'Mode flag
dim seleccionandoletra'We are chosing the 1,2,3 letter, mive up with enter
dim highscoremoveright'Move right true right flipper. false left flipper
dim tempnvname
dim giveacredit
dim temphsscore
dim tempposition


miletraes=64


' 62 IS THE backspace
' 63 IS THE END
' 64 IS THE Space

Sub moveletterhs

'	if highscoremoveright=true then
'		miletraes=miletraes+1
'			IF miletraes>90 THEN
'				miletraes=62
'			END IF
'	end if
'	if highscoremoveright=false then
'		miletraes=miletraes-1
'		IF miletraes<62 THEN
'			miletraes=90
'		END IF
'	end if
'
'
'		ADER1=miletraes+1
'		IF ADER1>90 THEN
'			ADER1=62
'		END IF
'	ADER2=ADER1+1
'		IF ADER2>90 THEN
'			ADER2=62
'		END IF
'	ADER3=ADER2+1
'		IF ADER3>90 THEN
'			ADER3=62
'		END IF
'	ADER4=ADER3+1
'		IF ADER4>90 THEN
'			ADER4=62
'		END IF
'	ADER5=ADER4+1
'		IF ADER5>90 THEN
'			ADER5=62
'		END IF
'	ADER6=ADER5+1
'		IF ADER6>90 THEN
'			ADER6=62
'		END IF
'	ADER7=ADER6+1
'		IF ADER7>90 THEN
'			ADER7=62
'		END IF
'
'	AIZQ1=miletraes-1
'		IF AIZQ1<62 THEN
'			AIZQ1=90
'		END IF
'	AIZQ2=AIZQ1-1
'		IF AIZQ2<62 THEN
'			AIZQ2=90
'		END IF
'	AIZQ3=AIZQ2-1
'		IF AIZQ3<62 THEN
'			AIZQ3=90
'		END IF
'	AIZQ4=AIZQ3-1
'		IF AIZQ4<62 THEN
'			AIZQ4=90
'		END IF
'	AIZQ5=AIZQ4-1
'		IF AIZQ5<62 THEN
'			AIZQ5=90
'		END IF
'	AIZQ6=AIZQ5-1
'		IF AIZQ6<62 THEN
'			AIZQ6=90
'		END IF
'	AIZQ7=AIZQ6-1
'		IF AIZQ7<62 THEN
'			AIZQ7=90
'		END IF
'
'	'DispDmd1.AddFont 30, "motu_font_hiscore"
'	'DispDmd2.AddFont 30, "motu_font_hiscore"
'
'
'select case seleccionandoletra
'	case 1
'		letra1=miletraes
'		'DispDmd1.Text=   "[f2][XC][Y1]ENTER INITIALS"&"[f30][x0][y10]uvwyz"&"[f30][x83][y10]oprst"&"[B][f30][x45][y10]"&CHR(letra1)&"[f30][x55][y10]"&CHR(letra2)&"[f30][x65][y10]"&CHR(letra3)&"[/b][BF][box2,63,20,70,30][/BF][f30][XC][Y21]"&CHR(AIZQ7)&CHR(AIZQ6)&CHR(AIZQ5)&CHR(AIZQ4)&CHR(AIZQ3)&CHR(AIZQ2)&CHR(AIZQ1)&"          [f30][X62][Y21]"&CHR(miletraes)&"[f30][X72][Y21]"&CHR(ADER1)&CHR(ADER2)&CHR(ADER3)&CHR(ADER4)&CHR(ADER5)&CHR(ADER6)&CHR(ADER7)
'		'DispDmd2.Text=  "[f2][XC][Y1]ENTER INITIALS"&"[f30][x0][y10]uvwyz"&"[f30][x83][y10]oprst"&"[B][f30][x45][y10]"&CHR(letra1)&"[f30][x55][y10]"&CHR(letra2)&"[f30][x65][y10]"&CHR(letra3)&"[/b][BF][box2,63,20,70,30][/BF][f30][XC][Y21]"&CHR(AIZQ7)&CHR(AIZQ6)&CHR(AIZQ5)&CHR(AIZQ4)&CHR(AIZQ3)&CHR(AIZQ2)&CHR(AIZQ1)&"          [f30][X62][Y21]"&CHR(miletraes)&"[f30][X72][Y21]"&CHR(ADER1)&CHR(ADER2)&CHR(ADER3)&CHR(ADER4)&CHR(ADER5)&CHR(ADER6)&CHR(ADER7)
'	case 2
'		letra2=miletraes
'		'DispDmd1.Text=   "[f2][XC][Y1]ENTER INITIALS"&"[f30][x0][y10]uvwyz"&"[f30][x83][y10]oprst"&"[f30][x45][y10]"&CHR(letra1)&"[B][f30][x55][y10]"&CHR(letra2)&"[f30][x65][y10]"&CHR(letra3)&"[/b][BF][box2,63,20,70,30][/BF][f30][XC][Y21]"&CHR(AIZQ7)&CHR(AIZQ6)&CHR(AIZQ5)&CHR(AIZQ4)&CHR(AIZQ3)&CHR(AIZQ2)&CHR(AIZQ1)&"          [f30][X62][Y21]"&CHR(miletraes)&"[f30][X72][Y21]"&CHR(ADER1)&CHR(ADER2)&CHR(ADER3)&CHR(ADER4)&CHR(ADER5)&CHR(ADER6)&CHR(ADER7)
'		'DispDmd2.Text=  "[f2][XC][Y1]ENTER INITIALS"&"[f30][x0][y10]uvwyz"&"[f30][x83][y10]oprst"&"[f30][x45][y10]"&CHR(letra1)&"[B][f30][x55][y10]"&CHR(letra2)&"[f30][x65][y10]"&CHR(letra3)&"[/b][BF][box2,63,20,70,30][/BF][f30][XC][Y21]"&CHR(AIZQ7)&CHR(AIZQ6)&CHR(AIZQ5)&CHR(AIZQ4)&CHR(AIZQ3)&CHR(AIZQ2)&CHR(AIZQ1)&"          [f30][X62][Y21]"&CHR(miletraes)&"[f30][X72][Y21]"&CHR(ADER1)&CHR(ADER2)&CHR(ADER3)&CHR(ADER4)&CHR(ADER5)&CHR(ADER6)&CHR(ADER7)
'	case 3
'		letra3=miletraes
'		'DispDmd1.Text=   "[f2][XC][Y1]ENTER INITIALS"&"[f30][x0][y10]uvwyz"&"[f30][x83][y10]oprst"&"[f30][x45][y10]"&CHR(letra1)&"[f30][x55][y10]"&CHR(letra2)&"[B][f30][x65][y10]"&CHR(letra3)&"[/b][BF][box2,63,20,70,30][/BF][f30][XC][Y21]"&CHR(AIZQ7)&CHR(AIZQ6)&CHR(AIZQ5)&CHR(AIZQ4)&CHR(AIZQ3)&CHR(AIZQ2)&CHR(AIZQ1)&"          [f30][X62][Y21]"&CHR(miletraes)&"[f30][X72][Y21]"&CHR(ADER1)&CHR(ADER2)&CHR(ADER3)&CHR(ADER4)&CHR(ADER5)&CHR(ADER6)&CHR(ADER7)
'		'DispDmd2.Text=  "[f2][XC][Y1]ENTER INITIALS"&"[f30][x0][y10]uvwyz"&"[f30][x83][y10]oprst"&"[f30][x45][y10]"&CHR(letra1)&"[f30][x55][y10]"&CHR(letra2)&"[B][f30][x65][y10]"&CHR(letra3)&"[/b][BF][box2,63,20,70,30][/BF][f30][XC][Y21]"&CHR(AIZQ7)&CHR(AIZQ6)&CHR(AIZQ5)&CHR(AIZQ4)&CHR(AIZQ3)&CHR(AIZQ2)&CHR(AIZQ1)&"          [f30][X62][Y21]"&CHR(miletraes)&"[f30][X72][Y21]"&CHR(ADER1)&CHR(ADER2)&CHR(ADER3)&CHR(ADER4)&CHR(ADER5)&CHR(ADER6)&CHR(ADER7)
'	case 4'END
'		letra4=miletraes
'		'DispDmd1.Text=   "[f2][XC][Y1]ENTER INITIALS"&"[f30][x0][y10]uvwyz"&"[f30][x83][y10]oprst"&"[f30][x45][y10]"&CHR(letra1)&"[f30][x55][y10]"&CHR(letra2)&"[f30][x65][y10]"&CHR(letra3)&"[f30][x75][y10]"&CHR(letra4)&"[/b][BF][box2,63,20,70,30][/BF][f30][XC][Y21]"&CHR(AIZQ7)&CHR(AIZQ6)&CHR(AIZQ5)&CHR(AIZQ4)&CHR(AIZQ3)&CHR(AIZQ2)&CHR(AIZQ1)&"          [f30][X62][Y21]"&CHR(miletraes)&"[f30][X72][Y21]"&CHR(ADER1)&CHR(ADER2)&CHR(ADER3)&CHR(ADER4)&CHR(ADER5)&CHR(ADER6)&CHR(ADER7)
'		'DispDmd2.Text=  "[f2][XC][Y1]ENTER INITIALS"&"[f30][x0][y10]uvwyz"&"[f30][x83][y10]oprst"&"[f30][x45][y10]"&CHR(letra1)&"[f30][x55][y10]"&CHR(letra2)&"[f30][x65][y10]"&CHR(letra3)&"[f30][x75][y10]"&CHR(letra4)&"[/b][BF][box2,63,20,70,30][/BF][f30][XC][Y21]"&CHR(AIZQ7)&CHR(AIZQ6)&CHR(AIZQ5)&CHR(AIZQ4)&CHR(AIZQ3)&CHR(AIZQ2)&CHR(AIZQ1)&"          [f30][X62][Y21]"&CHR(miletraes)&"[f30][X72][Y21]"&CHR(ADER1)&CHR(ADER2)&CHR(ADER3)&CHR(ADER4)&CHR(ADER5)&CHR(ADER6)&CHR(ADER7)
'
'end select
'

end sub


sub endselection
'		benterhighscoremode=false
'		if letra1=64 then
'			letra1=32
'		end if
'		if letra2=64 then
'			letra2=32
'		end if
'		if letra3=64 then
'			letra3=32
'		end if
'
'		tempnvname=""&CHR(letra1)&CHR(letra2)&CHR(letra3)&""
'		dim act
'	if  nvScore(CurrentPlayer) => nvHighScore(1) then
'		for act=2 to 10
'			execute "nvHighScore("&-act+12&")=nvHighScore("&-act + 11&")"
'			execute "nvHighScoreName("&-act+12&")=nvHighScoreName("&-act + 11&")"
'		next
'				nvHighScore(1)=nvScore(CurrentPlayer)
'				nvHighScoreName(1)=tempnvname
'				giveacredit=true
'				temphsscore=nvScore(CurrentPlayer)
'				tempposition=1
'				thehseseste
'		exit sub
'	end if
'	if  nvScore(CurrentPlayer) => nvHighScore(2) then
'		for act=3 to 10
'			execute "nvHighScore("&-act+13&")=nvHighScore("&-act + 12&")"
'			execute "nvHighScoreName("&-act+13&")=nvHighScoreName("&-act + 12&")"
'		next
'		nvHighScore(2)=nvScore(CurrentPlayer)
'		nvHighScoreName(2)=tempnvname
'
'				giveacredit=true
'				temphsscore=nvScore(CurrentPlayer)
'				tempposition=2
'				thehseseste
'		exit sub
'	end if
'	if  nvScore(CurrentPlayer) => nvHighScore(3) then
'		for act=4 to 10
'			execute "nvHighScore("&-act + 14&")=nvHighScore("&-act + 13&")"
'			execute "nvHighScoreName("&-act + 14&")=nvHighScoreName("&-act + 13&")"
'		next
'		nvHighScore(3)=nvScore(CurrentPlayer)
'		nvHighScoreName(3)=tempnvname
'
'				giveacredit=true
'				temphsscore=nvScore(CurrentPlayer)
'				tempposition=3
'				thehseseste
'		exit sub
'	end if
'	if  nvScore(CurrentPlayer) => nvHighScore(4) then
'		for act=5 to 10
'			execute "nvHighScore("&-act + 15&")=nvHighScore("&-act + 14&")"
'			execute "nvHighScoreName("&-act + 15&")=nvHighScoreName("&-act + 14&")"
'		next
'		nvHighScore(4)=nvScore(CurrentPlayer)
'		nvHighScoreName(4)=tempnvname
'
'				giveacredit=false
'				temphsscore=nvScore(CurrentPlayer)
'				tempposition=4
'				thehseseste
'		exit sub
'	end if
'	if  nvScore(CurrentPlayer) => nvHighScore(5) then
'		for act=6 to 10
'			execute "nvHighScore("&-act + 16&")=nvHighScore("&-act + 15&")"
'			execute "nvHighScoreName("&-act + 16&")=nvHighScoreName("&-act + 15&")"
'		next
'		nvHighScore(5)=nvScore(CurrentPlayer)
'		nvHighScoreName(5)=tempnvname
'
'				giveacredit=false
'				temphsscore=nvScore(CurrentPlayer)
'				tempposition=5
'				thehseseste
'		exit sub
'	end if
'	if  nvScore(CurrentPlayer) => nvHighScore(6) then
'		for act=7 to 10
'			execute "nvHighScore("&-act + 17&")=nvHighScore("&-act + 16&")"
'			execute "nvHighScoreName("&-act + 17&")=nvHighScoreName("&-act + 16&")"
'		next
'		nvHighScore(6)=nvScore(CurrentPlayer)
'		nvHighScoreName(6)=tempnvname
'
'				giveacredit=false
'				temphsscore=nvScore(CurrentPlayer)
'				tempposition=6
'				thehseseste
'		exit sub
'	end if
'	if  nvScore(CurrentPlayer) => nvHighScore(7) then
'		for act=8 to 10
'			execute "nvHighScore("&-act + 18&")=nvHighScore("&-act + 17&")"
'			execute "nvHighScoreName("&-act + 18&")=nvHighScoreName("&-act + 17&")"
'		next
'		nvHighScore(7)=nvScore(CurrentPlayer)
'		nvHighScoreName(7)=tempnvname
'
'				giveacredit=false
'				temphsscore=nvScore(CurrentPlayer)
'				tempposition=7
'				thehseseste
'		exit sub
'	end if
'	if  nvScore(CurrentPlayer) => nvHighScore(8) then
'		for act=9 to 10
'			execute "nvHighScore("&-act + 19&")=nvHighScore("&-act + 18&")"
'			execute "nvHighScoreName("&-act + 19&")=nvHighScoreName("&-act + 18&")"
'		next
'		nvHighScore(8)=nvScore(CurrentPlayer)
'		nvHighScoreName(8)=tempnvname
'
'				giveacredit=false
'				temphsscore=nvScore(CurrentPlayer)
'				tempposition=8
'				thehseseste
'		exit sub
'	end if
'	if  nvScore(CurrentPlayer) => nvHighScore(9) then
'			nvHighScore(10)=nvHighScore(9)
'			nvHighScoreName(10)=nvHighScoreName(9)
'		nvHighScore(9)=nvScore(CurrentPlayer)
'		nvHighScoreName(9)=tempnvname
'
'				giveacredit=false
'				temphsscore=nvScore(CurrentPlayer)
'				tempposition=9
'				thehseseste
'		exit sub
'	end if
'	if  nvScore(CurrentPlayer) => nvHighScore(10) then
'		nvHighScore(10)=nvScore(CurrentPlayer)
'		nvHighScoreName(10)=tempnvname
'
'				giveacredit=false
'				temphsscore=nvScore(CurrentPlayer)
'				tempposition=10
'				thehseseste
'		exit sub
'	end if

end sub



sub thehseseste
'	afterhseob.Interval = 3000
'	afterhseob.Enabled = True
'	if giveacredit=true then
'		giveacredit=false
'
'					nvCredits = nvCredits + 1
'					FlashForMs  Flasher2f,  800, 100, LightStateOff
'					FlashForMs  Flasher6f,  800, 100, LightStateOff
'	end if

		'DispDmd1.Text= "[f2][XC][Y1]PLAYER "&(CurrentPlayer)&"[f2][XC][Y11]TOTAL "&formatscore(temphsscore)&"[f30][xc][y20]"&tempnvname&" RANK-" &tempposition
		'DispDmd2.Text= "[f2][XC][Y1]PLAYER "&(CurrentPlayer)&"[f2][XC][Y11]TOTAL "&formatscore(temphsscore)&"[f30][xc][y20]"&tempnvname&" RANK-" &tempposition

end sub


sub afterhseob_Timer
'	afterhseob.Interval = 1500
'	afterhseob.Enabled = False
				EndOfBallComplete
end sub

'******************************************************
' 					ULTRADMD
'******************************************************

Sub LoadUltraDMD
	If not ScoreType = 1 Then Exit Sub: End If
    Set UltraDMD = CreateObject("UltraDMD.DMDObject")
    UltraDMD.Init

    Dim fso, curDir
    Set fso = CreateObject("Scripting.FileSystemObject")
    curDir = fso.GetAbsolutePathName(".")
    Set fso = nothing

    ' A Major version change indicates the version is no longer backward compatible
    If Not UltraDMD.GetMajorVersion = 1 Then
        MsgBox "Incompatible Version of UltraDMD found."
        Exit Sub
    End If


    'A Minor version change indicates new features that are all backward compatible
    If UltraDMD.GetMinorVersion < 1 Then
        MsgBox "Incompatible Version of UltraDMD found.  Please update to version 1.1 or newer."
        Exit Sub
    End If


    UltraDMD.SetProjectFolder curDir & "\Jaws.UltraDMD"
    UltraDMD.SetVideoStretchMode UltraDMD_VideoMode_Middle

End Sub

Sub DMD_DisplayScene(toptext,bottomtext,animateIn,pauseTime,animateOut)
	If not ScoreType = 1 Then Exit Sub: End If
	UltraDMD.DisplayScene00 "", toptext, 13, bottomtext, 15, animateIn, pauseTime, animateOut
	ScoreTimer.enabled = true
End Sub

Sub DMD_DisplaySceneExWithId(id,toptext,bottomtext,animateIn,pauseTime,animateOut)
	If not ScoreType = 1 Then Exit Sub: End If
	UltraDMD.DisplayScene00ExWithId id, 0, "", toptext, 13, 9, bottomtext, 15, 9, animateIn, pauseTime, animateOut
	ScoreTimer.enabled = true
End Sub

Sub DMD_ScrollingCredits(text,animateIn,pauseTime,animateOut)
	If not ScoreType = 1 Then Exit Sub: End If
	UltraDMD.ScrollingCredits "", text, 15, animateIn, pauseTime, animateOut
	ScoreTimer.enabled = true
End Sub

Sub DMD_ModifyScene(id,toptext,bottomtext)
	If not ScoreType = 1 Then Exit Sub: End If
	UltraDMD.ModifyScene00 id, toptext, bottomtext
End Sub

Sub DMD_ModifySceneEx(id,toptext,bottomtext,pauseTime)
	If not ScoreType = 1 Then Exit Sub: End If
	UltraDMD.ModifyScene00Ex id, toptext, bottomtext,pauseTime
End Sub

Sub DMDId(id, toptext, bottomtext, duration) 'used in the highscore entry routine
	If not ScoreType = 1 Then Exit Sub: End If
    UltraDMD.DisplayScene00ExwithID id, False, "", toptext, 15, 0, bottomtext, 15, 0, 14, duration, 14
End Sub

Sub DMDMod(id, toptext, bottomtext, duration) 'used in the highscore entry routine
	If not ScoreType = 1 Then Exit Sub: End If
    UltraDMD.ModifyScene00Ex id, toptext, bottomtext, duration
End Sub

Sub DMD_CancelScene(id)
	If not ScoreType = 1 Then Exit Sub: End If
	UltraDMD.CancelRenderingWithId id
End Sub

Sub DMD_DisplayhighScene(toptext,bottomtext,animateIn,pauseTime,animateOut)
	If not ScoreType = 1 Then Exit Sub: End If
	UltraDMD.DisplayScene00 "", toptext, 13, bottomtext, 15, animateIn, pauseTime, animateOut
	'ScoreTimer.enabled = true
End Sub

Sub DMD_CancelRendering()
	scoreupdate = true
	addscore(0)
	If not ScoreType = 1 Then Exit Sub: End If
	UltraDMD.CancelRendering
End Sub

Dim DMDIntroState:DMDIntroState=0
Dim PUPIntroState:PUPIntroState=0

Sub DMDIntro_Timer()
	If not ScoreType = 1 Then Exit Sub: End If
	DMDIntroState = DMDIntroState + 1
	Select Case (DMDIntroState)
		Case 1: DMD_DisplayScene "MASTERS OF","THE UNIVERSE", UltraDMD_Animation_None, 4000, UltraDMD_Animation_None
		'Case 2: DMD_DisplayScene "Heman","Pinball", UltraDMD_Animation_None, 4000, UltraDMD_Animation_None
		Case 2: DMD_DisplayScene "Dont Use","Drugs!", UltraDMD_Animation_None, 4000, UltraDMD_Animation_None
		Case 3: DMD_DisplayScene "FP Table","Created By Rom", UltraDMD_Animation_None, 4000, UltraDMD_Animation_None
		Case 4: DMD_DisplayScene "VP Conversion","by RandR", UltraDMD_Animation_None, 4000, UltraDMD_Animation_None
		Case 5: DMD_DisplayScene "DOF, DETAILS, PuP","by TerryRed", UltraDMD_Animation_None, 4000, UltraDMD_Animation_None
		Case 6: DMD_DisplayScene "Thanks Thalamus","for SSF!", UltraDMD_Animation_None, 4000, UltraDMD_Animation_None
		Case 7: DMD_DisplayScene "HEMAN  "& HighScorename(0),""& HighScore(0), UltraDMD_Animation_None, 3000, UltraDMD_Animation_None
		Case 8: DMD_DisplayScene "Skeletor  "& HighScorename(1),""& HighScore(1), UltraDMD_Animation_None, 2500, UltraDMD_Animation_None
		Case 9: DMD_DisplayScene "evil lyn  "& HighScorename(2),""& HighScore(2), UltraDMD_Animation_None, 2000, UltraDMD_Animation_None
		Case 10: DMD_DisplayScene "orko  "& HighScorename(3),""& HighScore(3), UltraDMD_Animation_None, 2000, UltraDMD_Animation_None
		Case 11: DMD_DisplayScene "LAST GAME  ",""& (nvScore(CurrentPlayer)), UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
		Case 12: if bFreePlay = True Then
					DMD_DisplayScene "Press Start","Free Play", UltraDMD_Animation_None, 4000, UltraDMD_Animation_None
				elseif (nvcredits = 0) then
					DMD_DisplayScene "Insert Coin","Credits " & nvCredits, UltraDMD_Animation_None, 4000, UltraDMD_Animation_None
				elseif (nvcredits > 0) then
					DMD_DisplayScene "Press Start","Credits " & nvCredits, UltraDMD_Animation_None, 4000, UltraDMD_Animation_None
				end if
		Case 13: DMD_DisplayScene "GAME","OVER", UltraDMD_Animation_None, UltraDMD_deOn, UltraDMD_Animation_None
		Case 14: DisplayScore
		Case 15: DMDIntroState = 0
	End Select
End Sub

Sub PUPIntro_Timer()
		If ScoreType=1 Then Exit Sub: End If
	PUPIntroState = PUPIntroState + 1
	Select Case (PUPIntroState)
		Case 1:
			pDMDSplashLinesLarge " "," ", 6,LabelYellow
			pDMDSplashLinesDMD "He-Man And The","Masters Of The Universe", 6,LabelYellow
			PuPlayer.playlistplayex pLargeDMD,"DMD","Hemantitlecard.mp4",40,1
		Case 2:
			pDMDDrugsLarge
			pDMDDrugsDMD
		Case 3: 
			pDMDSplashLines "Future Pinball Table","Created By Rom",6,LabelWhite
		Case 4: 
			pDMDSplashLines "VPX Conversion","by RandR",6,LabelBlue
		Case 5: 
			pDMDSplashQuad "DOF, Details, PuP","by TerryRed","PostDMD PuP by","LynnInDenver",6,LabelYellow
		Case 6: 
			pDMDSplashLines "PinUp Player by","Nailbuster",6,16777215
		Case 7: 
			pDMDSplashLines "Thanks Thalamus","for SSF!",6,LabelYellow
		Case 8: 
			pDMDHighScore HighScorename(0),"HE-MAN", HighScore(0),"DMD//he_man.png", 6,LabelYellow
		Case 9: 
			pDMDHighScore HighScorename(1),"Skeletor", HighScore(1),"DMD//skeletor.png", 6,LabelYellow
		Case 10: 
			pDMDHighScore HighScorename(2),"Evil-Lyn", HighScore(2),"DMD//evil_lyn.png", 6,LabelYellow
		Case 11: 
			pDMDHighScore HighScorename(3),"Orko", HighScore(3),"DMD//orko.png", 6,LabelYellow
		Case 12: 
			pDMDSplashBigLines "LAST GAME",""& (nvScore(CurrentPlayer)), 6,LabelYellow
		Case 13: if bFreePlay = True Then
					pDMDSplashBigLines "Press Start","FREE PLAY", 6,LabelWhite
				elseif (nvcredits = 0) then
					pDMDSplashBigLines "Insert Coin","Credits " & nvCredits, 6,LabelYellow
				elseif (nvcredits > 0) then
					pDMDSplashBigLines "Press Start","Credits " & nvCredits, 6,LabelYellow
				end if
		Case 14: 
			pDMDSplashBigLines "GAME","OVER", 6,LabelYellow
		Case 15:
			pDMDSplashLinesLarge " "," ", 6,LabelYellow
			pDMDSplashLinesDMD "Download More Tables at","VPinball.com", 6,LabelYellow
			PuPlayer.playlistplayex pLargeDMD,"DMD","Vpinball.mp4",40,1
		Case 16:
			pDMDSplashLinesDMD "Download More Tables at","VPinball.com", 6,LabelYellow
		Case 17:
			pDMDSplashTriple ArcadeLine1,ArcadeLine2,ArcadeLine3, 6, LabelWhite
			PUPIntroState = 0
	End Select
End Sub

Sub ScoreTimer_Timer()
	If Countdown.enabled = false Then
		if NOT UltraDMD.isrendering then
			DisplayScore
			ScoreTimer.enabled = false
		Else
			DisplayModeTime
		end if

	End If

End Sub

Function NumberFormat(num)
	NumberFormat = Replace(FormatNumber(num,0,False,False,True), ",", ".")
End Function

Sub DisplayModeTime()
	Dim localExp

End Sub

Sub DisplayScore()

	scoreupdate = true
	addscore(0)

End Sub

Sub AddJackpot(score)
	If (vpTilted = False) and (vpGameInPlay=TRUE) Then
		nvJackpot = nvJackpot + nvscore

		DisplayJackpot
	End if
End Sub

Sub DisplayJackpot()
	If not ScoreType = 1 Then Exit Sub: End If
	If Not UltraDMD.IsRendering Then DMD_DisplaySceneExWithId "jackpot","Jackpot",nvjackpot,UltraDMD_Animation_ScrollOnUp,UltraDMD_deOn,UltraDMD_Animation_None
	DMD_ModifyScene "jackpot","Jackpot",nvjackpot
End Sub

Sub DMD_CancelRendering()
	If not ScoreType = 1 Then Exit Sub: End If
	UltraDMD.CancelRendering
End Sub




''******************************************************
' 						MUSIC
'******************************************************

Dim song

Sub myPlayMusicForMode(Mode)
	If (CurrentMusicTunePlaying <> Mode) Then
		Select Case (Mode)
			Case 0: EndMusic
			Case 1:	song = "ATTRACTMUSIC.mp3":endmusic:playmusic song
			Case 2: song = "GAMEPLAY music.mp3":endmusic:playmusic song
			Case 3: song = "HeManAltern.mp3":endmusic:playmusic song
			Case 4: song = "Motu_intro.mp3":endmusic:playmusic song
			Case 5:	song = "MULTIBALL castle.mp3":endmusic:playmusic song
			Case 6: song = "MULTIBALL HeMan.mp3":endmusic:playmusic song
			Case 7:	song = "MULTIBALL Ripper.mp3":endmusic:playmusic song
			Case 8:	song = "MULTIBALL Skeletor.mp3":endmusic:playmusic song
			'Case 9:	song = "ROTONloop.mp3":endmusic:playmusic song
		End Select
		CurrentMusicTunePlaying = Mode
	End If
End Sub
'
Sub Motu_MusicDone()
	PlayMusic Song
End Sub

'********************************************************************************************
' Only for VPX 10.2 and higher.
' FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light or flasher will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Return to previous State
'********************************************************************************************

Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState) 'thanks gtxjoe for the first myVersion

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
' Flasher Effects

Dim FlashLevel1, FlashLevel2, FlashLevel3, FlashLevel4
flasher1.IntensityScale = 0
flasher5.IntensityScale = 0
flasher8.IntensityScale = 0
flasher9.IntensityScale = 0
flasher6.IntensityScale = 0
flasher2.IntensityScale = 0
flasher11.IntensityScale = 0
flasher13.IntensityScale = 0
flasher10.IntensityScale = 0
flasher12.IntensityScale = 0
RipperNeon.IntensityScale = 0
CastleNeon.IntensityScale = 0
Disco1.IntensityScale = 0
Disco2.IntensityScale = 0
flasher11.IntensityScale = 0

Dim FlashLevelSS, FlashLevelBS, FlashLevelLS, FlashLevelRS, FlashLevelRR, FlashLevelRW, FlashLevelRSS, FlashLevelRCY, FlashLevelRCR, FlashLevelRCL, FlashLevelRCM, FlashLevelRMM, FlashLevelRTR, FlashLevelRL, FlashLevelRT, FlashLevelRRT, FlashLevelRRT2, FlashLevelTTT2

'*** Right Sling Flasher ***
Sub Flashertimer_Timer()
	dim flashx3, matdim

	' Right Sling Flasher ************************************************************
	If Flasher1F.visible Then FlashLevelSS = 1

	If FlashLevelSS <= 0 Then
		FlashLevelSS = 0
		Flasher1Fv.visible = 0
		Flasherbase1.BlendDisableLighting = 0
	Else
		flashx3 = FlashLevelSS^3
		Flasher1Fv.visible = 1
		Flasher1Fv.opacity = 1000 * flashx3^0.8

		Flasher1.IntensityScale = flashx3

		matdim = Round(10 * FlashLevelSS)
		if matdim = 10 then matdim = 9
		Flasherbase1.BlendDisableLighting = flashx3
		Flasherbase1lit.BlendDisableLighting = 10 * flashx3
		Flasherbase1lit.material = "domelit" & matdim

		FlashLevelSS = FlashLevelSS * 0.8 - 0.01
		If FlashLevelSS < 0.15 Then
			Flasherbase1lit.visible = 0
		Else
			Flasherbase1lit.visible = 1
		end If
end If

'*** Left Sling Flasher ***
	' Right Sling Flasher ************************************************************
	If Flasher5F.visible Then FlashLevelBS = 1

	If FlashLevelBS <= 0 Then
		FlashLevelBS = 0
		Flasher5Fv.visible = 0
		Flasherbase5.BlendDisableLighting = 0
	Else
		flashx3 = FlashLevelBS^3
		Flasher5Fv.visible = 1
		Flasher5fv.opacity = 1000 * flashx3^0.8

		Flasher5.IntensityScale = flashx3

		matdim = Round(10 * FlashLevelBS)
		if matdim = 10 then matdim = 9
		Flasherbase5.BlendDisableLighting = flashx3
		Flasherbase5lit.BlendDisableLighting = 10 * flashx3
		Flasherbase5lit.material = "domelit" & matdim

		FlashLevelBS = FlashLevelBS * 0.8 - 0.01
		If FlashLevelBS < 0.15 Then
			Flasherbase5lit.visible = 0
		Else
			Flasherbase5lit.visible = 1
		end If
end If

'*** Left Lower Ramp flasher ***
	' Left Lower Ramp flasher ************************************************************
	If flasher8F.visible Then FlashLevelLS = 1

	If FlashLevelLS <= 0 Then
		FlashLevelLS = 0
		flasher8Fv.visible = 0
		Flasherbase8.BlendDisableLighting = 0
	Else
		flashx3 = FlashLevelLS^3
		flasher8Fv.visible = 1
		flasher8Fv.opacity = 1000 * flashx3^0.8

		flasher8.IntensityScale = flashx3

		matdim = Round(10 * FlashLevelLS)
		if matdim = 10 then matdim = 9
		Flasherbase8.BlendDisableLighting = flashx3
		Flasherbase8lit.BlendDisableLighting = 10 * flashx3
		Flasherbase8lit.material = "domelit" & matdim

		FlashLevelLS = FlashLevelLS * 0.8 - 0.01
		If FlashLevelLS < 0.15 Then
			Flasherbase8lit.visible = 0
		Else
			Flasherbase8lit.visible = 1
		end If
end If
'*** Upper Left Corner flasher ***
	' Upper Left Corner flasher ************************************************************
	If flasher9F.visible Then FlashLevelRS = 1

	If FlashLevelRS <= 0 Then
		FlashLevelRS = 0
		flasher9Fv.visible = 0
		Flasherbase9.BlendDisableLighting = 0
	Else
		flashx3 = FlashLevelRS^3
		flasher9Fv.visible = 1
		flasher9Fv.opacity = 1000 * flashx3^0.8

		flasher9.IntensityScale = flashx3

		matdim = Round(10 * FlashLevelRS)
		if matdim = 10 then matdim = 9
		Flasherbase9.BlendDisableLighting = flashx3
		Flasherbase9lit.BlendDisableLighting = 10 * flashx3
		Flasherbase9lit.material = "domelit" & matdim

		FlashLevelRS = FlashLevelRS * 0.8 - 0.01
		If FlashLevelRS < 0.15 Then
			Flasherbase9lit.visible = 0
		Else
			Flasherbase9lit.visible = 1
		end If
end If
'*** Upper Left Purple flasher ***
	' Upper Left Purple flasher  ************************************************************
	If flasher2F.visible Then FlashLevelRSS = 1

	If FlashLevelRSS <= 0 Then
		FlashLevelRSS = 0
		flasher2Fv.visible = 0
		Flasherbase2.BlendDisableLighting = 0
	Else
		flashx3 = FlashLevelRSS^3
		flasher2Fv.visible = 1
		flasher2Fv.opacity = 1000 * flashx3^0.8

		flasher2.IntensityScale = flashx3

		matdim = Round(10 * FlashLevelRSS)
		if matdim = 10 then matdim = 9
		Flasherbase2.BlendDisableLighting = flashx3
		Flasherbase2lit.BlendDisableLighting = 10 * flashx3
		Flasherbase2lit.material = "domelit" & matdim

		FlashLevelRSS = FlashLevelRSS * 0.8 - 0.01
		If FlashLevelRSS < 0.15 Then
			Flasherbase2lit.visible = 0
		Else
			Flasherbase2lit.visible = 1
		end If
end If
'*** Upper right Green flasher ***
	' Upper right Green flasher ************************************************************
	If flasher6F.visible Then FlashLevelRR = 1

	If FlashLevelRR <= 0 Then
		FlashLevelRR = 0
		flasher6Fv.visible = 0
		Flasherbase6.BlendDisableLighting = 0
	Else
		flashx3 = FlashLevelRR^3
		flasher6Fv.visible = 1
		flasher6Fv.opacity = 1000 * flashx3^0.8

		Flasher6.IntensityScale = flashx3

		matdim = Round(10 * FlashLevelRR)
		if matdim = 10 then matdim = 9
		Flasherbase6.BlendDisableLighting = flashx3
		Flasherbase6lit.BlendDisableLighting = 10 * flashx3
		Flasherbase6lit.material = "domelit" & matdim

		FlashLevelRR = FlashLevelRR * 0.8 - 0.01
		If FlashLevelRR < 0.15 Then
			Flasherbase6lit.visible = 0
		Else
			Flasherbase6lit.visible = 1
		end If
end If
'*** Upper Right Yellow flasher ***
	' Upper Right Yellow flasher ************************************************************
	If flasher11F.visible Then FlashLevelRW = 1

	If FlashLevelRW <= 0 Then
		FlashLevelRW = 0
		flasher11Fv.visible = 0
		Flasherbase11.BlendDisableLighting = 0
	Else
		flashx3 = FlashLevelRW^3
		flasher11Fv.visible = 1
		flasher11Fv.opacity = 1000 * flashx3^0.8

		Flasher11.IntensityScale = flashx3

		matdim = Round(10 * FlashLevelRW)
		if matdim = 10 then matdim = 9
		Flasherbase11.BlendDisableLighting = flashx3
		Flasherbase11lit.BlendDisableLighting = 10 * flashx3
		Flasherbase11lit.material = "domelit" & matdim

		FlashLevelRW = FlashLevelRW * 0.8 - 0.01
		If FlashLevelRW < 0.15 Then
			Flasherbase11lit.visible = 0
		Else
			Flasherbase11lit.visible = 1
		end If
end If
'*** Right Side Green flasher ***
	' Right Side Green flasher  ************************************************************
	If flasher13F.visible Then FlashLevelRSS = 1

	If FlashLevelRSS <= 0 Then
		FlashLevelRSS = 0
		flasher13fv.visible = 0
		Flasherbase13.BlendDisableLighting = 0
	Else
		flashx3 = FlashLevelRSS^3
		flasher13fv.visible = 1
		flasher13fv.opacity = 1000 * flashx3^0.8

		flasher13.IntensityScale = flashx3

		matdim = Round(10 * FlashLevelRSS)
		if matdim = 10 then matdim = 9
		Flasherbase13.BlendDisableLighting = flashx3
		Flasherbase13lit.BlendDisableLighting = 10 * flashx3
		Flasherbase13lit.material = "domelit" & matdim

		FlashLevelRSS = FlashLevelRSS * 0.8 - 0.01
		If FlashLevelRSS < 0.15 Then
			Flasherbase13lit.visible = 0
		Else
			Flasherbase13lit.visible = 1
		end If
end If

'*** White flasher ***
	' White Flasher ************************************************************
	If Flasher10f.visible Then FlashLevelRCY = 1

	If FlashLevelRCY <= 0 Then
		FlashLevelRCY = 0
		Flasher10fv.visible = 0
		Flasherbase10.BlendDisableLighting = 0
	Else
		flashx3 = FlashLevelRCY^3
		Flasher10fv.visible = 1
		Flasher10fv.opacity = 1000 * flashx3^0.8

		Flasher10.IntensityScale = flashx3

		matdim = Round(10 * FlashLevelRCY)
		if matdim = 10 then matdim = 9
		Flasherbase10.BlendDisableLighting = flashx3
		Flasherbase10lit.BlendDisableLighting = 10 * flashx3
		Flasherbase10lit.material = "domelit" & matdim

		FlashLevelRCY = FlashLevelRCY * 0.8 - 0.01
		If FlashLevelRCY < 0.15 Then
			Flasherbase10lit.visible = 0
		Else
			Flasherbase10lit.visible = 1
		end If
end If

'*** Red flasher ***
	' Red Flasher ************************************************************
	If Flasher12f.visible Then FlashLevelRCR = 1

	If FlashLevelRCR <= 0 Then
		FlashLevelRCR = 0
		Flasher12fv.visible = 0
		Flasherbase12.BlendDisableLighting = 0
	Else
		flashx3 = FlashLevelRCR^3
		Flasher12fv.visible = 1
		Flasher12fv.opacity = 1000 * flashx3^0.8

		Flasher12.IntensityScale = flashx3

		matdim = Round(10 * FlashLevelRCR)
		if matdim = 10 then matdim = 9
		Flasherbase12.BlendDisableLighting = flashx3
		Flasherbase12lit.BlendDisableLighting = 10 * flashx3
		Flasherbase12lit.material = "domelit" & matdim

		FlashLevelRCR = FlashLevelRCR * 0.8 - 0.01
		If FlashLevelRCR < 0.15 Then
			Flasherbase12lit.visible = 0
		Else
			Flasherbase12lit.visible = 1
		end If
end If
'*** Ripper Neon ***
	' Ripper Neon ************************************************************
	If RipperNeonf.visible Then FlashLevelRCL = 1

	If FlashLevelRCL <= 0 Then
		FlashLevelRCL = 0
		RipperNeonfv.visible = 0
		RipperNeonp.BlendDisableLighting = 0
	Else
		flashx3 = FlashLevelRCL^3
		RipperNeonfv.visible = 1
		RipperNeonfv.opacity = 1000 * flashx3^0.8

		RipperNeon.IntensityScale = flashx3

		matdim = Round(10 * FlashLevelRCL)
		if matdim = 10 then matdim = 9
		RipperNeonp.BlendDisableLighting = flashx3
		RipperNeonplit.BlendDisableLighting = 10 * flashx3
		RipperNeonplit.material = "domelit" & matdim

		FlashLevelRCl = FlashLevelRCL * 0.8 - 0.01
		If FlashLevelRCL < 0.15 Then
			RipperNeonplit.visible = 0
		Else
			RipperNeonplit.visible = 1
		end If
end If

'*** Castle Neon ***
	' Castle Neon ************************************************************
	If CastleNeonf.visible Then FlashLevelRCM = 1

	If FlashLevelRCM <= 0 Then
		FlashLevelRCM = 0
		CastleNeonfv.visible = 0
		CastleNeonp.BlendDisableLighting = 0
	Else
		flashx3 = FlashLevelRCM^3
		CastleNeonfv.visible = 1
		CastleNeonfv.opacity = 1000 * flashx3^0.8

		CastleNeon.IntensityScale = flashx3

		matdim = Round(10 * FlashLevelRCM)
		if matdim = 10 then matdim = 9
		CastleNeonp.BlendDisableLighting = flashx3
		CastleNeonplit.BlendDisableLighting = 10 * flashx3
		CastleNeonplit.material = "domelit" & matdim

		FlashLevelRCM = FlashLevelRCM * 0.8 - 0.01
		If FlashLevelRCM < 0.15 Then
			CastleNeonplit.visible = 0
		Else
			CastleNeonplit.visible = 1
		end If
end If

'*** Right disco1 Flasher ***
	' Castle Neon ************************************************************
	If disco1f.visible Then FlashLevelRMM = 1

	If FlashLevelRMM <= 0 Then
		FlashLevelRMM = 0
		disco1fv.visible = 0
		FlasherbaseDisco1.BlendDisableLighting = 0
	Else
		flashx3 = FlashLevelRMM^3
		disco1fv.visible = 1
		disco1fv.opacity = 1000 * flashx3^0.8

		disco1.IntensityScale = flashx3

		matdim = Round(10 * FlashLevelRMM)
		if matdim = 10 then matdim = 9
		FlasherbaseDisco1.BlendDisableLighting = flashx3
		FlasherbaseDisco1lit.BlendDisableLighting = 10 * flashx3
		FlasherbaseDisco1lit.material = "domelit" & matdim

		FlashLevelRMM = FlashLevelRMM * 0.8 - 0.01
		If FlashLevelRMM < 0.15 Then
			FlasherbaseDisco1lit.visible = 0
		Else
			FlasherbaseDisco1lit.visible = 1
		end If
end If

'*** Left disco2 Flasher ***
	' Castle Neon ************************************************************
	If disco2f.visible Then FlashLevelRTR = 1

	If FlashLevelRTR <= 0 Then
		FlashLevelRTR = 0
		disco2fv.visible = 0
		FlasherbaseDisco2.BlendDisableLighting = 0
	Else
		flashx3 = FlashLevelRTR^3
		disco2fv.visible = 1
		disco2fv.opacity = 1000 * flashx3^0.8

		disco2.IntensityScale = flashx3

		matdim = Round(10 * FlashLevelRTR)
		if matdim = 10 then matdim = 9
		FlasherbaseDisco2.BlendDisableLighting = flashx3
		FlasherbaseDisco2lit.BlendDisableLighting = 10 * flashx3
		FlasherbaseDisco2lit.material = "domelit" & matdim

		FlashLevelRTR = FlashLevelRTR * 0.8 - 0.01
		If FlashLevelRTR < 0.15 Then
			FlasherbaseDisco2lit.visible = 0
		Else
			FlasherbaseDisco2lit.visible = 1
		end If
end If
end sub


''*** green Plunger bulb flasher ***
sub flashbulb1t_timer()
	if plungerbulb.state = 1 Then
		plungerbulb.state = 0
		plungerbulbFv.visible = false
		plungerbulbbulb.blenddisablelighting = 0
	else

	 if plungerbulb.state = 0 Then
		plungerbulb.state = 1
		plungerbulbFv.visible = true
		plungerbulbbulb.blenddisablelighting = 1
		 end if
	end if
end sub

''*** Red Heman bulb flasher ***
sub flashbulb2t_timer()
	if bulb12.state = 1 Then
		bulb12.state = 0
		bulb12Fv.visible = false
		
	else

	 if bulb12.state = 0 Then
		bulb12.state = 1
		bulb12Fv.visible = true
		
	 end if
	end if
end sub

''*** Castle 2 Green bulbs flasher ***
sub flashbulb3t_timer()
	if bulb14.state = 1 and Bulb13.state = 1 Then
		bulb14.state = 0:Bulb13.state = 0
		bulb14Fv.visible = false:bulb13Fv.visible = false
		BulbLock4.blenddisablelighting = 0:BulbLock5.blenddisablelighting = 0
	else

	 if bulb14.state = 0 and bulb13.state = 0 Then
		bulb14.state = 1:bulb13.state = 1
		bulb14Fv.visible = true:bulb13Fv.visible = true
		BulbLock4.blenddisablelighting = 1:BulbLock5.blenddisablelighting = 1
	 end if
	end if
end sub

''*** Castle Yellow bulb flasher ***
sub flashbulb4t_timer()
	if bulb11.state = 1 Then
		bulb11.state = 0
		bulb11Fv.visible = false
		
	else

	 if bulb11.state = 0 Then
		bulb11.state = 1
		bulb11Fv.visible = true
		
	 end if
	end if
end sub

''*** Center Green bulb flasher ***
sub flashbulb5t_timer()
	if bulb8.state = 1 Then
		bulb8.state = 0
		bulb8Fv.visible = false
		
	else

	 if bulb8.state = 0 Then
		bulb8.state = 1
		bulb8Fv.visible = true
		
	 end if
	end if
end sub

''*** Disco1 bulb flasher ***
sub Disco1t_timer()

end sub

''*** Disco2 bulb flasher ***
sub Disco2t_timer()

end sub

''*** Flasher11 bulb flasher ***
sub Flasher11t_timer()
	if Flasher11.state = 1 Then
		Flasher11.state = 0
		Flasher11Fv.visible = false
		
	else

	 if Flasher11.state = 0 Then
		Flasher11.state = 1
		Flasher11Fv.visible = true
		
	 end if
	end if
end sub
'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle

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

Sub PlaySoundAtVol(sound, tableobj, Volume)
  PlaySound sound, 1, Volume, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function RndNum(min, max)
    RndNum = Int(Rnd() * (max-min + 1) ) + min ' Sets a random number between min and max
End Function

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / MOTU.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / MOTU.width-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 200)
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

Const tnob = 10 ' total number of balls
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
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
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
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 20, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
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
	PlaySound "pinhit_low", 0, Vol(ActiveBall)*VolPi, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall)*VolTarg, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall)*VolMetal, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall)*VolMetal, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall)*VolMetal, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall)*VolGates, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner", 0, 1*VolSpin, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*VolPo, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Const TableName = "MOTUHighScore"

Sub InstantInfo
	
	If ScoreType = 1 Then
  DMD "HighScores.gif", "", "", 2500
	End If
 Dim X
'    Jackpot = 250000 (Score(CurrentPlayer) / 10, 0)
	If ScoreType=1 Then
    DMD "black.jpg", "", "INSTANT INFO", 500
	End If
        x = LoadValue(TableName, "HighScore1")
    If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 100 End If
End Sub

Dim TotalgamesPlayed
Dim Credits
Dim HighScore(4)
Dim HighScorename(4)
'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
    Dim x
    x = LoadValue(TableName, "HighScore1")
    If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 9900000 End If

    x = LoadValue(TableName, "HighScore1Name")
    If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "RNR" End If

    x = LoadValue(TableName, "HighScore2")
    If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 900000 End If

    x = LoadValue(TableName, "HighScore2Name")
    If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "HEM" End If

    x = LoadValue(TableName, "HighScore3")
    If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 90000 End If

    x = LoadValue(TableName, "HighScore3Name")
    If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "SKE" End If

    x = LoadValue(TableName, "HighScore4")
    If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 9000 End If

    x = LoadValue(TableName, "HighScore4Name")
    If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "ORK" End If

    x = LoadValue(TableName, "Credits")
    If(x <> "") then Credits = CInt(x) Else Credits = 0 End If

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


Sub AwardSpecial()
	DMD_DisplayhighScene "EXTRA GAME","WON", UltraDMD_Animation_None, 3000, UltraDMD_Animation_None
	pDMDSplashBigLines "EXTRA GAME","WON",0,0
    Credits = Credits + 1
end Sub

Sub CheckHighscore()
    Dim tmp
    tmp = nvScore(1)

    If nvScore(2)> tmp Then tmp = nvScore(2)
    If nvScore(3)> tmp Then tmp = nvScore(3)
    If nvScore(4)> tmp Then tmp = nvScore(4)

    If tmp> HighScore(1) Then 'add 1 credit for beating the highscore
        AwardSpecial
    End If

    If tmp> HighScore(3) Then
        vpmtimer.addtimer 2000, "PlaySound ""Orgo01"" '"
        HighScore(3) = tmp
        'enter player's name
        HighScoreEntryInit()
    Else
        EndOfBallComplete()
    End If
End Sub

Sub HighScoreEntryInit()
    hsbModeActive = True
    PlaySound "fx_knocker"
	Playsound "Orgo01"
	 hsLetterFlash = 0
	
    hsEnteredDigits(0) = "A"
    hsEnteredDigits(1) = "A"
    hsEnteredDigits(2) = "A"
    hsCurrentDigit = 0

    hsValidLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ<+-0123456789" ' < is used to delete the last letter
    hsCurrentLetter = 1
	DMD_Clearforhighscore
    DMDId "hsc", "Enter", "Your Name", 999999
    HighScoreDisplayName()
End Sub

Sub EnterHighScoreKey(keycode)
    If keycode = LeftFlipperKey Then
        Playsound "fx_Previous"
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0) then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayName()
    End If

    If keycode = RightFlipperKey Then
        Playsound "fx_Next"
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter> len(hsValidLetters) ) then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayName()
    End If

    If keycode = StartGameKey OR keycode = PlungerKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<") then
            playsound "fx_Enter"
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3) then
                HighScoreCommitName()
            else
                HighScoreDisplayName()
            end if
        else
            playsound "fx_Esc"
            hsEnteredDigits(hsCurrentDigit) = " "
            if(hsCurrentDigit> 0) then
                hsCurrentDigit = hsCurrentDigit - 1
            end if
            HighScoreDisplayName()
        end if
    end if
End Sub

Sub HighScoreDisplayName()
    Dim i, TempStr

    TempStr = " >"
    if(hsCurrentDigit> 0) then TempStr = TempStr & hsEnteredDigits(0)
    if(hsCurrentDigit> 1) then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit> 2) then TempStr = TempStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3) then
        if(hsLetterFlash <> 0) then
            TempStr = TempStr & "_"
        else
            TempStr = TempStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit <1) then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit <2) then TempStr = TempStr & hsEnteredDigits(2)

    TempStr = TempStr & "< "
	DMDMod "hsc", "YOUR NAME:", Mid(TempStr, 2, 5), 999999
	pDMDTargetLetters "Enter", "Your Name", "" & Mid(TempStr,2,5),0,16777215
    
End Sub

Sub HighScoreCommitName()
    hsbModeActive = False
    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ") then
        hsEnteredName = "YOU"
    end if

    HighScoreName(3) = hsEnteredName
    SortHighscore
    DMD_Clearforhighscore
	EndOfBallComplete()
End Sub

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 0 to 3
        For j = 0 to 2
            If HighScore(j) <HighScore(j + 1) Then
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

Sub DMD_Clearforhighscore()
	If not ScoreType = 1 Then Exit Sub: End If
	UltraDMDTimer.Enabled = 0
    UltraDMDScoreTimer.Enabled = 0
    UltraDMD.CancelRendering
    UltraDMD.Clear
End Sub

Sub UltraDMDScoreTimer_Timer()
	If not ScoreType = 1 Then Exit Sub: End If
    If NOT UltraDMD.IsRendering Then
        DMDScoreNow
    End If
End Sub

Sub DMDScoreNow
 	If not ScoreType = 1 Then Exit Sub: End If
   DMD_Clearforhighscore
    'DMDScore
End Sub

'Sub UltraDMDTimer_Timer() 'used for repeating the attrack mode and the instant info.
'    If bInstantInfo Then
'        InstantInfo
'    ElseIf bAttractMode Then
'        ShowTableInfo
'    End If
'End Sub



Sub MOTU_Exit()
	'Controller.Pause=False
	If B2SOn Then Controller.stop
	Savehs
	'if VideoIntro.enabled = false Then
		'If UltraDMD.IsRendering Then
		'UltraDMD.CancelRendering
		'End If
		'UltraDMD = NULL
	'End If
End Sub


'**************************************
'*********     DOF Section    *********
'**************************************

Dim DOFLinx_HeManMB_Stage
Dim DOFLinx_CastleMB_Stage
Dim DOFLinx_RipperMB_Stage

'--- Flippers ---

Sub DOFLinx_Left_Flipper_On()    '' ' DOFLinx - Left Flipper On
	DOF 101, DOFOn 
End Sub
'
Sub DOFLinx_Right_Flipper_On()   '' ' DOFLinx - Right Flipper On
	DOF 102, DOFOn 
End Sub
'
Sub DOFLinx_Left_Flipper_Off()   '' ' DOFLinx - Left Flipper Off
	DOF 101, DOFOff
End Sub
'
Sub DOFLinx_Right_Flipper_Off()  '' ' DOFLinx - Right Flipper Off
	DOF 102, DOFOff
End Sub
'
''--- Slingshots ---
'
Sub DOFLinx_Left_Slingshot_Hit()  '' ' DOFLinx - Left Slingshot Hit
	DOF 110, DOFPulse  ' DOFLinx DOF MX - Left Sling Shot
End Sub
'
Sub DOFLinx_Right_Slingshot_Hit() '' ' DOFLinx - Right Slingshot Hit
	DOF 111, DOFPulse  ' DOFLinx DOF MX - Left Sling Shot
End Sub

''--- Bumpers ---

Sub DOFLinx_Bumper1_Hit() '' ' DOFLinx - Bumper 1 Hit
	DOF 120, DOFPulse  ' DOFLinx DOF MX - Bumper 1
End Sub
'
Sub DOFLinx_Bumper2_Hit() '' ' DOFLinx - Bumper 2 Hit
	DOF 121, DOFPulse  ' DOFLinx DOF MX - Bumper 2
End Sub
'
Sub DOFLinx_Bumper3_Hit() '' ' DOFLinx - Bumper 3 Hit
	DOF 122, DOFPulse  ' DOFLinx DOF MX - Bumper 3
End Sub
'
''--- Plunger, Drain ---
'
Sub DOFLinx_AutoPlunger()   '' ' DOFLinx - AutoPlunger
	DOF 132, DOFPulse  ' DOFLinx DOF MX - Ball in Launched
End Sub
'
Sub DOFLinx_Kickback()      '' ' DOFLinx - Kickback
	DOF 133, DOFPulse  ' DOFLinx DOF MX - Kickback
End Sub

Sub DOFLinx_PlungerKicker() '' ' DOFLinx - PlungerKicker
	DOF 103, DOFPulse 
End Sub
'
Sub DOFLinx_Drain_Hit()     '' ' DOFLinx - Drain Hit

End Sub
'
''--- Kickers ---
'
Sub DOFLinx_HeManKicker_hit()  '' ' DOFLinx - HeManKicker_hit

End Sub
'
Sub DOFLinx_HeManKicker()      '' ' DOFLinx - HeManKicker
	DOF 104, DOFPulse   
	DOF 506, DOFPulse  ' DOF Strobe
	DOF 406, DOFPulse  ' DOFLinx DOF MX - Strobe
	DOF 235, DOFPulse  ' DOFLinx DOF MX - HeMan Kicker
End Sub
'
Sub DOFLinx_SkeletorLock_hit() ' DOFLinx - SkeletorLock_hit

End Sub
'
Sub DOFLinx_SkeletorLock()     '' ' DOFLinx - SkeletorLock
	DOF 105, DOFPulse  'DOF - Solenoid
	DOF 506, DOFPulse  ' DOF Strobe
	DOF 406, DOFPulse  ' DOFLinx DOF MX - Strobes
	DOF 407, DOFPulse  ' DOFLinx DOF MX - Beacon
End Sub
'
''*******************************************
''*********     BUTTONS SECTION     *********
''*******************************************
'
'' This is where your Start, Coin, Launch and other Buttons are
'' turned ON / OFF or Flashing for normal table events. There may also be other
'' effects triggered such as RGB under cabinet lighting, or feedback.
'
''------ COIN and START Buttons ------
'
Sub DOFLinx_NoCredits_StartOff()   '' ' DOFLinx: No Credits - Coin Button Flashing, Start Button Off
	DOF 513, DOFOff  ' DOF - Start Button Flashing
	DOF 514, DOFOff  ' DOF - Coin Button On
	DOF 512, DOFOff  ' DOF - Start Button Off
	DOF 515, DOFOn  ' DOF - Coin Button Flashing

End Sub
'
Sub DOFLinx_CreditsIn_StartFlash() '' ' DOFLinx: Credits In - Coin Button On, Start Button Flashing
	DOF 512, DOFOff  ' DOF - Start Button Off
	DOF 513, DOFOn  ' DOF - Start Button Flashing
	DOF 515, DOFOff  ' DOF - Coin Button Flashing
	DOF 514, DOFOn  ' DOF - Coin Button On
End Sub
'
Sub DOFLinx_Coin_Inserted()        '' ' DOFLinx: Coin Inserted
	DOF 506, DOFPulse  ' DOF Strobe
	DOF 406, DOFPulse  ' DOFLinx DOF MX - Strobe
	DOF 407, DOFPulse  ' DOFLinx DOF MX - Beacon
End Sub
'
Sub DOFLinx_NoPay_NoPlay()         '' ' DOFLinx: NoPay NoPlay
	DOF 420, DOFPulse ' DOFLinx DOF MX - No Pay No Play
	DOF 407, DOFPulse ' DOFLinx DOF MX - Beacon
End Sub
'
Sub DOFLinx_CoinOn_StartOn()       '' ' DOFLinx: Game in Play - Coins & Start Button On
	DOF 422, DOFOff  ' DOFLinx DOF MX - Game Started
	DOF 513, DOFOff  ' DOF - Start Button Flashing
	DOF 515, DOFOff  ' DOF - Coin Button Flashing
	DOF 512, DOFOn  ' DOF - Start Button Off
	DOF 514, DOFOn  ' DOF - Coin Button On
End Sub
'
''------ LAUNCH Button ------
'
Sub DOFLinx_Launch_Button_Flashing() '' ' DOFLinx - Launch Button Flashing
	DOF 310, DOFOn  ' DOFLinx DOF MX - Ball is Ready To Launch
End Sub
'
Sub DOFLinx_Launch_Button_Off()  ' DOFLinx - Launch Button Off
	DOF 310, DOFOff  ' DOFLinx DOF MX - Ball is Ready To Launch OFF
End Sub
'
'
'
''***********************************
''********     ROLLOVERS     ********
''***********************************
'
'' Light a RGB Flasher for Rollovers. These can be located at the back of
'' the playfield, at the Inner and Outer Lanes, etc.
'
Sub DOFLinx_LeftInLaneTrigger_Hit() 'DOFLinx_LeftInLaneTrigger_Hit
	DOF 301, DOFPulse  ' DOFLinx DOF MX - Left Inlane Rollover
End Sub
'
Sub DOFLinx_RightInLaneTrigger_Hit() 'DOFLinx_RightInLaneTrigger_Hit
	DOF 308, DOFPulse  ' DOFLinx DOF MX - Right Inlane Rollover
End Sub
'
Sub DOFLinx_Right_Drain_Trigger_Hit() 'DOFLinx_Right_Drain_Trigger (Create a new Trigger)
	DOFLinx_Right_Drained()
	DOF 309, DOFPulse ' DOFLinx DOF MX - Right OuterLane Rollover / Drain
End Sub
'
Sub DOFLinx_Left_Drain_Trigger_Hit() 'DOFLinx_Left_Drain_Trigger (Create a new Trigger)
	DOFLinx_Left_Drained()
	DOF 300, DOFPulse  ' DOFLinx DOF - Left OuterLane Rollover / Drain
End Sub
'

''*********************************
''********     DRAIN     **********
''*********************************
'
'' When your ball drains out, create an animated RGB Flasher
'' effect on either the Outer Lanes or the center Drain. Turn off
'' the RGB under cabinet lighting, or any other devices that may
'' be running.
'
Sub DOFLinx_Drained() ' DOFLinx - Drained
	DOF 504, DOFOff  ' DOF Beacon Off
	DOF_ALL_RGB_Undercab_OFF() 'Turn off all RGB Undercab Triggers
	DOF 130, DOFPulse  ' DOFLinx DOF MX - Drain
End Sub
'
Sub DOFLinx_Left_Drained() '' ' DOFLinx - Left Drained

End Sub
'
Sub DOFLinx_Right_Drained() '' ' DOFLinx - Right Drained

End Sub
'
'
'
''****************************************
''**********    TILT Section    **********
''****************************************
'
''------ TILT WARNING ------
'
'' When there is a TILT Warning, change RGB under cabinet
'' lighting to red momentarily.
'
Sub DOFLinx_TILT_Warning()'' ' DOFLinx - Tilt Warning
	DOF 401, DOFPulse  ' DOFLinx DOF MX - TILT Warning
	DOF 516, DOFPulse  ' DOF RGB Undercab - Red Pulse
End Sub
'
''--------- TILTED ----------
'
'' When TILTED, all RGB Flashers flash red. Turn off devices still running
'' (except maybe the beacon momentarily), and turn off the RGB under
'' cabinet lighting.
'
Sub DOFLinx_TILTED()  '' ' DOFLinx - Tilted
	DOF_ALL_RGB_Undercab_OFF() 'Turn off all RGB Undercab Triggers
	DOF 505, DOFPulse  ' DOF Beacon Pulse
	DOF 400, DOFPulse  ' DOFLinx DOF MX - TILT
	DOF 401, DOFPulse  ' DOFLinx DOF MX - TILT Warning
End Sub
'

''*********************************
''********     TARGETS     ********
''*********************************
'
'' Create an effect when hitting Targets throughout the playfield.
'' This can be a RGB flasher or even a solenoid for impact feedback, etc.
'
Sub DOFLinx_Skeletor_Targets_Hit() 'DOFLinx_Skeletor_Targets_Hit
	DOF 230, DOFPulse  ' DOFLinx DOF MX - Skeletor Targets Hit
End Sub
'
''---- Castle Grayskull Targets ----
'
Sub DOFLinx_Target9_Hit() '' ' DOFLinx - Target9 Hit
	DOF 231, DOFPulse  ' DOFLinx DOF MX - Castle GraySkull Targets Hit
End Sub
'
Sub DOFLinx_Target10_Hit() '' ' DOFLinx - Target10 Hit
	DOF 231, DOFPulse  ' DOFLinx DOF MX - Castle GraySkull Targets Hit
End Sub
'
''---- Extra Ball Target ----
'
Sub DOFLinx_EBtarget_Hit() '' ' DOFLinx - EBtarget Hit
	DOF 232, DOFPulse  ' DOFLinx DOF MX - Extra Ball Target Hit
End Sub
'
''---- He-Man Targets ----
'
Sub DOFLinx_Target4_Hit() '' ' DOFLinx - Target4 Hit
	DOF 233, DOFPulse  ' DOFLinx DOF MX - He-Man Targets Hit
End Sub
'
Sub DOFLinx_Target5_Hit() '' ' DOFLinx - Target5 Hit
	DOF 233, DOFPulse  ' DOFLinx DOF MX - He-Man Targets Hit
End Sub
'
''---- Skeletor Hit ----
'
Sub DOFLinx_Skeletor_Hit_Left()  'DOFLinx_Skeletor_Hit_Left
	DOF 234, DOFPulse  ' DOFLinx DOF MX - Skeletor Roton Hit
End Sub
'
Sub DOFLinx_Skeletor_Hit_Right()  'DOFLinx_Skeletor_Hit_Right
	DOF 234, DOFPulse  ' DOFLinx DOF MX - Skeletor Roton Hit
End Sub
'
'
'
''**********************************************
''************     RAMP Effects     ************
''**********************************************
'
'' Create some kind of animated RGB Flashers effect
'' when the ball is shot into a ramp. Maybe also have
'' some feedback effects such as Blower Fan.
'
Sub DOFLinx_RoadRipperRamp() 'DOFLinx_RoadRipperRamp
	DOF 441, DOFPulse  ' DOFLinx DOF MX - Right Ramp
End Sub
'
Sub DOFLinx_GraySkullRamp() 'DOFLinx_GraySkullRamp
	DOF 440, DOFPulse  ' DOFLinx DOF MX - Left Ramp
End Sub
'
'
''*********************************************
''**********     ORBIT Flashers    ************
''*********************************************
'
'' Create an RGB animated effect when the ball goes through the Orbit.
'' Sometimes it's easier to create new ' ' DOF labelled triggers
'' for each RGB flasher, as opposed to creating an entire sub-routine
'' with Timers, etc.
'
Sub DOF_Orbit_OL_Trigger_Hit() 'DOF_Orbit_OL_Trigger_Hit
	DOF 430, DOFPulse  ' DOFLinx DOF MX - Orbit OL Trigger
End Sub
'
Sub DOF_Orbit_IL_Trigger_Hit() 'DOF_Orbit_IL_Trigger_Hit
	DOF 431, DOFPulse  ' DOFLinx DOF MX - Orbit IL Trigger
End Sub
'
Sub DOF_Orbit_CN_Trigger_Hit() 'DOF_Orbit_CN_Trigger_Hit
	DOF 432, DOFPulse  ' DOFLinx DOF MX - Orbit CN Trigger
End Sub
'
Sub DOF_Orbit_IR_Trigger_Hit() 'DOF_Orbit_IR_Trigger_Hit
	DOF 433, DOFPulse  ' DOFLinx DOF MX - Orbit IR Trigger
End Sub
'
Sub DOF_Orbit_OR_Trigger_Hit() 'DOF_Orbit_OR_Trigger_Hit
	DOF 434, DOFPulse  ' DOFLinx DOF MX - Orbit OR Trigger
End Sub



''***********************************************
''*******    COMBO, or JACKPOT effects    *******
''***********************************************
'
'' Create a RGB Flashers effect when you get something
'' like a COMBO or JACKPOT bonus.
'
Sub DOFLinx_Combo_Jackpot()  ' DOFLinx: Combo - Jackpot - Effects
		DOF 506, DOFPulse  ' DOF Strobe
		DOF 450, DOFPulse  ' DOFLinx DOF MX - Jackpot
End Sub
'
Sub DOFLinx_Extra_Ball()  '' ' DOFLinx: Extra Ball
		DOF 451, DOFPulse ' DOFLinx DOF MX - Extra Ball
End Sub
'
Sub DOFLinx_Ball1_Locked() 'DOFLinx_Ball1_Locked
		DOF 452, DOFPulse  ' DOFLinx DOF MX - Ball 1 Locked
End Sub
'
Sub DOFLinx_Ball2_Locked() 'DOFLinx_Ball2_Locked
		DOF 453, DOFPulse  ' DOFLinx DOF MX - Ball 2 Locked
End Sub
'
Sub DOFLinx_Ball3_Locked() 'DOFLinx_Ball3_Locked
		DOF 454, DOFPulse  ' DOFLinx DOF MX - Ball 3 Locked
End Sub
'
Sub DOFLinx_HeMan_Jackpot()  'DOFLinx_HeMan_Jackpot

End Sub
'
'
''************************************
''***********    MODES    ************
''************************************
'
'' Create a RGB Flashers effect when you enabled a specific
'' mode or have started a mulit-ball, etc.
'
Sub DOFLinx_HeMan_Lit()     'DOFLinx_HeMan_Lit
		DOF 460, DOFPulse  ' DOFLinx DOF MX - HEMAN Lit
End Sub
'
Sub DOFLinx_GraySkull_Lit() 'DOFLinx_GraySkull_Lit
		DOF 461, DOFPulse  ' DOFLinx DOF MX - CASTLE LIT
End Sub
'
Sub DOFLinx_Skeletor_MultiBall()  'DOFLinx_Skeletor_MultiBall
		DOF 504, DOFOn  ' DOF Beacon
		DOF_ALL_RGB_Undercab_OFF() 'Turn off all RGB Undercab Triggers
		DOF 511, DOFOn  'RGB Undercab - Skeletor Mode
		DOF 482, DOFOn  ' DOFLinx DOF MX - MultiBall ON
End Sub
'
Sub DOFLinx_Skeletor_MultiBall_End()  'DOFLinx_Skeletor_MultiBall_End
	DOF 504, DOFOff  ' DOF Beacon OFF
	DOF 511, DOFOff  'RGB Undercab - Skeletor Mode Off
	DOF 507, DOFOn  'RGB Undercab - Main Mode
	DOF 482, DOFOff  ' DOFLinx DOF MX - MultiBall OFF
End Sub
'
Sub DOFLinx_Skeletor_BattleMode_Start()   'DOFLinx_Skeletor_BattleMode_Start
		DOF 462, DOFPulse  ' DOFLinx DOF MX - Skeletor Battle Mode Start
End Sub
'
Sub DOFLinx_Skeletor_BattleMode_End()   'DOFLinx_Skeletor_BattleMode_End
	DOF 511, DOFOff  'RGB Undercab - Skeletor Mode Off
	DOF 507, DOFOn  'RGB Undercab - Main Mode
	DOF 504, DOFOff  ' DOF Beacon OFF
End Sub
'
Sub DOFLinx_HeMan_MB_Go() 'DOFLinx_HeMan_MB_Go
	DOF_ALL_RGB_Undercab_OFF() 'Turn off all RGB Undercab Triggers
	DOFLinx_HeMan_POG.Enabled = True
	DOF 470, DOFOn  ' DOFLinx DOF MX - HeMan MultiBall Go
End Sub
'
Sub DOFLinx_HeMan_POG_timer() 'DOFLinx_HeMan_POG_Expired
	DOFLinx_HeMan_POG.Enabled = False
	DOFLinx_HeMan_POG_Start()
	DOF 470, DOFOff  ' DOFLinx DOF MX - HeMan MultiBall Go OFF
End Sub
'
Sub DOFLinx_HeMan_POG_Start()  'DOFLinx_HeMan_POG_Start
	DOF 506, DOFPulse  ' DOF Strobe
	DOF 471, DOFOn  ' DOFLinx DOF MX - HeMan POG
End Sub
'
Sub DOFLinx_HeMan_MultiBall_Start()  'DOFLinx_HeMan_MultiBall_Start
	DOFLinx_HeManMB_Stage=1
	DOFLinx_HeManMB.Enabled = True
	DOF 504, DOFOn  ' DOF Beacon
	DOF 508, DOFOn  'RGB Undercab - HeMan Mode
	DOF 471, DOFOff  ' DOFLinx DOF MX - HeMan POG OFF
	DOF 472, DOFOn   ' DOFLinx DOF MX - HeMan MultiBall Start
End Sub
'
Sub DOFLinx_HeMan_MultiBall_End()  'DOFLinx_HeMan_MultiBall_End
	DOF 504, DOFOff  ' DOF Beacon OFF
	DOFLinx_HeManMB.Enabled = False
	DOFLinx_HeManMB_Stage=1
	DOF 508, DOFOff  'RGB Undercab - HeMan Mode Off
	DOF 507, DOFOn  'RGB Undercab - Main Mode
	DOF 472, DOFOff  ' DOFLinx DOF MX - HeMan MultiBall Stop
End Sub
'
Sub DOFLinx_CastleMB_Start()  'DOFLinx_CastleMB_Start
	DOF 504, DOFOn  ' DOF Beacon
	DOFLinx_CastleMB_Stage=1
	DOFLinx_CastleMB.Enabled=True
	DOF_ALL_RGB_Undercab_OFF() 'Turn off all RGB Undercab Triggers
	DOF 509, DOFOn  'RGB Undercab - Castle Mode
	DOF 480, DOFOn  ' DOFLinx DOF MX - CastleMB Start
End Sub
'
Sub DOFLinx_CastleMB_End()  'DOFLinx_CastleMB_End
	DOF 504, DOFOff  ' DOF Beacon OFF
	DOFLinx_CastleMB_Stage=1
	DOFLinx_CastleMB.Enabled=False
	DOF 509, DOFOff  'RGB Undercab - Castle Mode Off
	DOF 507, DOFOn  'RGB Undercab - Main Mode
	DOF 480, DOFOff  ' DOFLinx DOF MX - CastleMB End
End Sub
'
Sub DOFLinx_RipperMB_Start()  'DOFLinx_RipperMB_Start
	DOF 504, DOFOn  ' DOF Beacon
	DOFLinx_RipperMB_Stage=1
	DOFLinx_RipperMB.Enabled=True
	DOF_ALL_RGB_Undercab_OFF() 'Turn off all RGB Undercab Triggers
	DOF 510, DOFOn  'RGB Undercab - Ripper Mode
	DOF 481, DOFOn  ' DOFLinx DOF MX - RipperMB Start
End Sub
'
Sub DOFLinx_RipperMB_End()  'DOFLinx_RipperMB_End
	DOF 504, DOFOff  ' DOF Beacon OFF
	DOFLinx_RipperMB_Stage=1
	DOFLinx_RipperMB.Enabled=False
	DOF 510, DOFOff  'RGB Undercab - Ripper Mode Off
	DOF 507, DOFOn  'RGB Undercab - Main Mode
	DOF 481, DOFOff  ' DOFLinx DOF MX - RipperMB End
End Sub
'

Sub DOF_ALL_RGB_Undercab_OFF() 'Turn off all RGB Undercab Triggers
	DOF 507, DOFOff  'RGB Undercab - Main Mode
	DOF 508, DOFOff  'RGB Undercab - Heman - Red
	DOF 509, DOFOff  'RGB Undercab - Castle - Green
	DOF 510, DOFOff  'RGB Undercab - Ripper - Yellow
	DOF 511, DOFOff  'RGB Undercab - Skeletor - Blue
End Sub


'
''****************************************
''********    MISC SECTION    ************
''****************************************
'
'
''******  Gear Motor: He-Man, Castle Grayskull, Road Ripper  ******
'
Sub DOFLinx_Grayskull_RaiseUp()    'DOFLinx_Grayskull_RaiseUp
	DOF 500, DOFPulse   'DOF - Gear Motor
End Sub
'
Sub DOFLinx_Grayskull_LowerDown()  'DOFLinx_Grayskull_LowerDown
	DOF 500, DOFPulse   'DOF - Gear motor
End Sub
'
Sub DOFLinx_HeMan_Rotate()         'DOFLinx_HeMan_Rotate
	DOF 501, DOFPulse   'DOF - Gear Motor
End Sub
'
Sub DOFLinx_Ripper_UP()            'DOFLinx_Ripper_UP
	DOF 502, DOFPulse   'DOF - Gear Motor
End Sub
'
'
'
''*******************************************************************
''************    RGB FLASHERS: Animation Section    ****************
''*******************************************************************
'
'
''   These Sub-Routines Need to have their "Stage" set to 1
''   FIRST and then their "Timer.enabled = True". This must be done
''	 in another sub-routine to create the RGB Flashers animation
''   in your cabinet for any trable action you want.
'
''	 The "Stage" variables must be declared somewhere in the script,
''	 (usually in a variables section) and the "Timers" need to be set
''   to a time typically from 80 to 100 ms (in the script, or the editor)
''   depending on what you need.
'
'
' DOFLinx - Flashers animation - HeMan MultiBall
Sub DOFLinx_HeManMB_timer()
  Select Case DOFLinx_HeManMB_Stage
    Case 1 : DOFLinx_HeManMB_Stage=2

    Case 2 : DOFLinx_HeManMB_Stage=3

    Case 3 : DOFLinx_HeManMB_Stage=4

    Case 4 : DOFLinx_HeManMB_Stage=5

    Case 5 : DOFLinx_HeManMB_Stage=6

    Case 6 : DOFLinx_HeManMB_Stage=1

  end Select
End Sub

' DOFLinx - Flashers animation - Castle MultiBall
Sub DOFLinx_CastleMB_timer()
  Select Case DOFLinx_CastleMB_Stage
    Case 1 : DOFLinx_CastleMB_Stage=2
					Disco1f.visible = false
					'Disco1.state = 0
					'Disco1Fv.visible = false
					Disco2f.visible = true
					'Disco2.state = 1
					'Disco2Fv.visible = true
					DOF 483, DOFPulse  ' DOFLinx DOF MX - Side Flasher Effect
    Case 2 : DOFLinx_CastleMB_Stage=3
					Disco1f.visible = true
					'Disco1.state = 1
					'Disco1Fv.visible = True
					Disco2f.visible = false
					'Disco2.state = 0
					'Disco2Fv.visible = false
					DOF 484, DOFPulse  ' DOFLinx DOF MX - Side Flasher Effect
    Case 3 : DOFLinx_CastleMB_Stage=4
					Disco1f.visible = false
					'Disco1.state = 0
					'Disco1Fv.visible = false
					Disco2f.visible = true
					'Disco2.state = 1
					'Disco2Fv.visible = true
					DOF 485, DOFPulse  ' DOFLinx DOF MX - Side Flasher Effect
    Case 4 : DOFLinx_CastleMB_Stage=5
					Disco1f.visible = true
					'Disco1.state = 1
					'Disco1Fv.visible = True
					Disco2f.visible = false
					'Disco2.state = 0
					'Disco2Fv.visible = false
					DOF 486, DOFPulse  ' DOFLinx DOF MX - Side Flasher Effect
    Case 5 : DOFLinx_CastleMB_Stage=1
					Disco1f.visible = false
					'Disco1.state = 0
					'Disco1Fv.visible = false
					Disco2f.visible = true
					'Disco2.state = 1
					'Disco2Fv.visible = true
					DOF 487, DOFPulse  ' DOFLinx DOF MX - Side Flasher Effect
  end Select
End Sub

' DOFLinx - Flashers animation - Ripper Multiball
Sub DOFLinx_RipperMB_timer()
  Select Case DOFLinx_RipperMB_Stage
    Case 1 : DOFLinx_RipperMB_Stage=2
               'Leave Blank
    Case 2 : DOFLinx_RipperMB_Stage=3
					DOF 494, DOFPulse  ' DOFLinx DOF MX - Side Flasher Effect
    Case 3 : DOFLinx_RipperMB_Stage=4
					DOF 490, DOFPulse  ' DOFLinx DOF MX - Side Flasher Effect
    Case 4 : DOFLinx_RipperMB_Stage=5
					DOF 492, DOFPulse  ' DOFLinx DOF MX - Side Flasher Effect
    Case 5 : DOFLinx_RipperMB_Stage=6
					DOF 495, DOFPulse  ' DOFLinx DOF MX - Side Flasher Effect
    Case 6 : DOFLinx_RipperMB_Stage=7
               'Leave Blank
    Case 7 : DOFLinx_RipperMB_Stage=8
					DOF 493, DOFPulse  ' DOFLinx DOF MX - Side Flasher Effect
    Case 8 : DOFLinx_RipperMB_Stage=9
					DOF 490, DOFPulse  ' DOFLinx DOF MX - Side Flasher Effect
    Case 9 : DOFLinx_RipperMB_Stage=10
					DOF 494, DOFPulse  ' DOFLinx DOF MX - Side Flasher Effect
    Case 10 : DOFLinx_RipperMB_Stage=1
					DOF 491, DOFPulse  ' DOFLinx DOF MX - Side Flasher Effect
  end Select
End Sub
'
''---------- RGB Flashers Animation Section Ends -----------

''#################################################
''##########    DOF MAIN SECTION ENDS    ##########
''#################################################
'