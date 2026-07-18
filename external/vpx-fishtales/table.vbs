

Option Explicit
Randomize
SetLocale 1033

Const TableVersion = "1.1"

'*******************************************
'	ZCON: Constants and Global Variables
'*******************************************

'ROM
Const cGameName = "ft_l5"

'Ball Size and Mass
Const BallSize = 50
Const BallMass = 1

Dim tablewidth: tablewidth = FishTales.width
Dim tableheight: tableheight = FishTales.height

' Pre-computed constants for BSUpdate (eliminates per-frame division)
Dim BS_halfTable: BS_halfTable = tablewidth / 2
Dim BS_ambientDiv: BS_ambientDiv = BallSize / AmbientMovement
  
Dim DesktopMode: DesktopMode = FishTales.ShowDT
Dim UseVPMDMD
If Desktopmode or RenderingMode = 2 Then UseVPMDMD = 1 Else UseVPMDMD = 0

Const UseVPMModSol = 2

Const tnob = 4 ' total number of balls
Const lob = 1 ' total number of locked balls

Dim FTBall1, FTBall2, FTBall3, FTCapBall
Dim gBOT
' OPT: last-known positions for ST/DT groups — skip LM primitive writes when unchanged
Dim lastST(5)  : Dim i_st  : For i_st  = 0 To 5 : lastST(i_st)   = -99999 : Next
Dim lastDTz(0) : Dim lastDTrx(0) : Dim lastDTry(0)
lastDTz(0) = -99999 : lastDTrx(0) = -99999 : lastDTry(0) = -99999

' Pre-built string arrays for hot-path sound calls (eliminates per-tick string concatenation)
Dim BallRollStr(5)
BallRollStr(0) = "BallRoll_0" : BallRollStr(1) = "BallRoll_1" : BallRollStr(2) = "BallRoll_2"
BallRollStr(3) = "BallRoll_3" : BallRollStr(4) = "BallRoll_4" : BallRollStr(5) = "BallRoll_5"

Dim RampLoopStr(6), WireLoopStr(6)
RampLoopStr(1) = "RampLoop1" : RampLoopStr(2) = "RampLoop2" : RampLoopStr(3) = "RampLoop3"
RampLoopStr(4) = "RampLoop4" : RampLoopStr(5) = "RampLoop5" : RampLoopStr(6) = "RampLoop6"
WireLoopStr(1) = "wireloop1" : WireLoopStr(2) = "wireloop2" : WireLoopStr(3) = "wireloop3"
WireLoopStr(4) = "wireloop4" : WireLoopStr(5) = "wireloop5" : WireLoopStr(6) = "wireloop6"

' Pre-built image name arrays for VR animation timers
Dim SmokeStr(60), FireStr(62), ShootingStarStr(31)
Dim ftsi
For ftsi = 1 To 60 : SmokeStr(ftsi) = "Smoke_" & ftsi : Next
For ftsi = 1 To 62 : FireStr(ftsi) = "Fire_" & ftsi : Next
For ftsi = 1 To 31 : ShootingStarStr(ftsi) = "ShootingStar_" & ftsi : Next

' Cached state for StartButtonLighting guard
Dim lastL88State: lastL88State = -1

'*******************************************
'	ZVLM: VLM Arrays
'*******************************************

' VLM  Arrays - Start
' Arrays per baked part
Dim BP_BoatPlastic: BP_BoatPlastic=Array(BM_BoatPlastic, LM_GITop_BoatPlastic, LM_Flashers_f19_BoatPlastic, LM_Flashers_f20_BoatPlastic, LM_Flashers_f21_BoatPlastic, LM_Flashers_f22_BoatPlastic, LM_Flashers_f23_BoatPlastic, LM_Flashers_f25_BoatPlastic, LM_Flashers_f26_BoatPlastic, LM_Inserts_l11_BoatPlastic, LM_Inserts_l12_BoatPlastic, LM_Inserts_l13_BoatPlastic, LM_Inserts_l14_BoatPlastic, LM_Inserts_l35_BoatPlastic, LM_Inserts_l36_BoatPlastic, LM_Inserts_l37_BoatPlastic, LM_Inserts_l38_BoatPlastic)
Dim BP_Bumper1Ring: BP_Bumper1Ring=Array(BM_Bumper1Ring, LM_GITop_Bumper1Ring, LM_Flashers_f26_Bumper1Ring, LM_Inserts_l16_Bumper1Ring)
Dim BP_Bumper2Ring: BP_Bumper2Ring=Array(BM_Bumper2Ring, LM_GITop_Bumper2Ring, LM_Flashers_f26_Bumper2Ring, LM_Flashers_f27_Bumper2Ring, LM_GISplit_gitop020_Bumper2Ring)
Dim BP_Bumper3Ring: BP_Bumper3Ring=Array(BM_Bumper3Ring, LM_GITop_Bumper3Ring, LM_Flashers_f26_Bumper3Ring, LM_Flashers_f27_Bumper3Ring, LM_GISplit_gitop020_Bumper3Ring)
Dim BP_BumperEdges: BP_BumperEdges=Array(BM_BumperEdges, LM_GITop_BumperEdges, LM_Flashers_f26_BumperEdges, LM_Flashers_f27_BumperEdges, LM_GISplit_gitop020_BumperEdges, LM_Inserts_l18_BumperEdges)
Dim BP_Bumper_Socket: BP_Bumper_Socket=Array(BM_Bumper_Socket, LM_GITop_Bumper_Socket, LM_Flashers_f26_Bumper_Socket, LM_GISplit_gitop020_Bumper_Sock, LM_Inserts_l16_Bumper_Socket)
Dim BP_Bumper_Socket_001: BP_Bumper_Socket_001=Array(BM_Bumper_Socket_001, LM_GITop_Bumper_Socket_001, LM_Flashers_f26_Bumper_Socket_0, LM_Flashers_f27_Bumper_Socket_0, LM_GISplit_gitop020_Bumper_Sock)
Dim BP_Bumper_Socket_002: BP_Bumper_Socket_002=Array(BM_Bumper_Socket_002, LM_GITop_Bumper_Socket_002, LM_Flashers_f26_Bumper_Socket_0, LM_Flashers_f27_Bumper_Socket_0, LM_GISplit_gitop020_Bumper_Sock)
Dim BP_Bumpers: BP_Bumpers=Array(BM_Bumpers, LM_GITop_Bumpers, LM_Flashers_f26_Bumpers)
Dim BP_Catapult: BP_Catapult=Array(BM_Catapult, LM_GITop_Catapult, LM_Flashers_f25_Catapult, LM_GISplit_gi009_Catapult)
Dim BP_ClearPlastics: BP_ClearPlastics=Array(BM_ClearPlastics, LM_GITop_ClearPlastics, LM_Flashers_f17_ClearPlastics, LM_Flashers_f25_ClearPlastics, LM_Flashers_f26_ClearPlastics, LM_GISplit_gitop020_ClearPlasti)
Dim BP_FishingRod: BP_FishingRod=Array(BM_FishingRod, LM_GITop_FishingRod, LM_Flashers_f17_FishingRod, LM_Flashers_f25_FishingRod, LM_Flashers_f26_FishingRod, LM_GISplit_gi009_FishingRod, LM_GISplit_gitop020_FishingRod, LM_Inserts_l26_FishingRod, LM_Inserts_l27_FishingRod, LM_Inserts_l28_FishingRod, LM_Inserts_l81_FishingRod, LM_Inserts_l82_FishingRod, LM_Inserts_l83_FishingRod, LM_Inserts_l84_FishingRod, LM_Inserts_l85_FishingRod)
Dim BP_FlipperLDown: BP_FlipperLDown=Array(BM_FlipperLDown, LM_GIBottom_FlipperLDown, LM_GISplit_gi002_FlipperLDown, LM_GISplit_gi006_FlipperLDown, LM_Inserts_l43_FlipperLDown, LM_Inserts_l51_FlipperLDown, LM_Inserts_l52_FlipperLDown)
Dim BP_FlipperLUp: BP_FlipperLUp=Array(BM_FlipperLUp, LM_GIBottom_FlipperLUp, LM_GISplit_gi002_FlipperLUp, LM_GISplit_gi006_FlipperLUp, LM_Inserts_l43_FlipperLUp, LM_Inserts_l51_FlipperLUp, LM_Inserts_l52_FlipperLUp)
Dim BP_FlipperRDown: BP_FlipperRDown=Array(BM_FlipperRDown, LM_GIBottom_FlipperRDown, LM_GISplit_gi002_FlipperRDown, LM_GISplit_gi006_FlipperRDown, LM_Inserts_l43_FlipperRDown, LM_Inserts_l52_FlipperRDown, LM_Inserts_l54_FlipperRDown)
Dim BP_FlipperRUp: BP_FlipperRUp=Array(BM_FlipperRUp, LM_GIBottom_FlipperRUp, LM_GISplit_gi002_FlipperRUp, LM_GISplit_gi006_FlipperRUp, LM_Inserts_l43_FlipperRUp, LM_Inserts_l52_FlipperRUp, LM_Inserts_l54_FlipperRUp)
Dim BP_Gate: BP_Gate=Array(BM_Gate, LM_GITop_Gate, LM_Flashers_f26_Gate)
Dim BP_LeftPost: BP_LeftPost=Array(BM_LeftPost, LM_GIBottom_LeftPost, LM_GISplit_gi002_LeftPost, LM_GISplit_gi009_LeftPost, LM_Inserts_l48_LeftPost, LM_Inserts_l58_LeftPost)
Dim BP_LeftSling1: BP_LeftSling1=Array(BM_LeftSling1, LM_GIBottom_LeftSling1, LM_GISplit_gi002_LeftSling1, LM_GISplit_gi006_LeftSling1, LM_Inserts_l21_LeftSling1, LM_Inserts_l58_LeftSling1)
Dim BP_LeftSling2: BP_LeftSling2=Array(BM_LeftSling2, LM_GIBottom_LeftSling2, LM_GISplit_gi002_LeftSling2, LM_GISplit_gi006_LeftSling2, LM_Inserts_l21_LeftSling2, LM_Inserts_l58_LeftSling2)
Dim BP_LeftSling3: BP_LeftSling3=Array(BM_LeftSling3, LM_GIBottom_LeftSling3, LM_GISplit_gi002_LeftSling3, LM_GISplit_gi006_LeftSling3, LM_Inserts_l21_LeftSling3, LM_Inserts_l58_LeftSling3)
Dim BP_LeftSling4: BP_LeftSling4=Array(BM_LeftSling4, LM_GIBottom_LeftSling4, LM_GISplit_gi002_LeftSling4, LM_GISplit_gi006_LeftSling4, LM_Inserts_l21_LeftSling4, LM_Inserts_l58_LeftSling4)
Dim BP_LeftSlingArm: BP_LeftSlingArm=Array(BM_LeftSlingArm, LM_GIBottom_LeftSlingArm, LM_GISplit_gi002_LeftSlingArm)
Dim BP_MetalRamps: BP_MetalRamps=Array(BM_MetalRamps, LM_GITop_MetalRamps, LM_Flashers_f17_MetalRamps, LM_Flashers_f18_MetalRamps, LM_Flashers_f19_MetalRamps, LM_Flashers_f20_MetalRamps, LM_Flashers_f21_MetalRamps, LM_Flashers_f22_MetalRamps, LM_Flashers_f23_MetalRamps, LM_Flashers_f25_MetalRamps, LM_Flashers_f26_MetalRamps, LM_Flashers_f27_MetalRamps, LM_GIBottom_MetalRamps, LM_GISplit_gi002_MetalRamps, LM_GISplit_gi006_MetalRamps, LM_GISplit_gi011_MetalRamps, LM_GISplit_gi009_MetalRamps, LM_GISplit_gitop020_MetalRamps, LM_Inserts_l11_MetalRamps, LM_Inserts_l12_MetalRamps, LM_Inserts_l13_MetalRamps, LM_Inserts_l14_MetalRamps, LM_Inserts_l15_MetalRamps, LM_Inserts_l16_MetalRamps, LM_Inserts_l17_MetalRamps, LM_Inserts_l18_MetalRamps, LM_Inserts_l21_MetalRamps, LM_Inserts_l22_MetalRamps, LM_Inserts_l23_MetalRamps, LM_Inserts_l24_MetalRamps, LM_Inserts_l25_MetalRamps, LM_Inserts_l26_MetalRamps, LM_Inserts_l27_MetalRamps, LM_Inserts_l28_MetalRamps, LM_Inserts_l31_MetalRamps, LM_Inserts_l34_MetalRamps, _
	LM_Inserts_l35_MetalRamps, LM_Inserts_l36_MetalRamps, LM_Inserts_l37_MetalRamps, LM_Inserts_l38_MetalRamps, LM_Inserts_l45_MetalRamps, LM_Inserts_l46_MetalRamps, LM_Inserts_l47_MetalRamps, LM_Inserts_l48_MetalRamps, LM_Inserts_l48a_MetalRamps, LM_Inserts_l55_MetalRamps, LM_Inserts_l56_MetalRamps, LM_Inserts_l57_MetalRamps, LM_Inserts_l58_MetalRamps, LM_Inserts_l61_MetalRamps, LM_Inserts_l62_MetalRamps, LM_Inserts_l63_MetalRamps, LM_Inserts_l64_MetalRamps, LM_Inserts_l65_MetalRamps, LM_Inserts_l66_MetalRamps, LM_Inserts_l67_MetalRamps, LM_Inserts_l68_MetalRamps, LM_Inserts_l71_MetalRamps, LM_Inserts_l72_MetalRamps, LM_Inserts_l73_MetalRamps, LM_Inserts_l74_MetalRamps, LM_Inserts_l75_MetalRamps, LM_Inserts_l76_MetalRamps, LM_Inserts_l77_MetalRamps, LM_Inserts_l78_MetalRamps, LM_Inserts_l81_MetalRamps, LM_Inserts_l82_MetalRamps, LM_Inserts_l83_MetalRamps, LM_Inserts_l84_MetalRamps, LM_Inserts_l85_MetalRamps, LM_Inserts_l86_MetalRamps)
Dim BP_MiniPF: BP_MiniPF=Array(BM_MiniPF, LM_GITop_MiniPF, LM_Flashers_f19_MiniPF, LM_Flashers_f20_MiniPF, LM_Flashers_f21_MiniPF, LM_Flashers_f22_MiniPF, LM_Flashers_f23_MiniPF, LM_Flashers_f25_MiniPF, LM_Flashers_f26_MiniPF, LM_Inserts_l11_MiniPF, LM_Inserts_l12_MiniPF, LM_Inserts_l13_MiniPF, LM_Inserts_l14_MiniPF, LM_Inserts_l15_MiniPF, LM_Inserts_l35_MiniPF, LM_Inserts_l36_MiniPF, LM_Inserts_l37_MiniPF, LM_Inserts_l38_MiniPF)
Dim BP_Parts: BP_Parts=Array(BM_Parts, LM_GITop_Parts, LM_Flashers_f17_Parts, LM_Flashers_f18_Parts, LM_Flashers_f19_Parts, LM_Flashers_f20_Parts, LM_Flashers_f21_Parts, LM_Flashers_f22_Parts, LM_Flashers_f23_Parts, LM_Flashers_f25_Parts, LM_Flashers_f26_Parts, LM_Flashers_f27_Parts, LM_GIBottom_Parts, LM_GISplit_gi002_Parts, LM_GISplit_gi006_Parts, LM_GISplit_gi011_Parts, LM_GISplit_gi009_Parts, LM_GISplit_gitop020_Parts, LM_Inserts_l11_Parts, LM_Inserts_l12_Parts, LM_Inserts_l13_Parts, LM_Inserts_l14_Parts, LM_Inserts_l15_Parts, LM_Inserts_l16_Parts, LM_Inserts_l17_Parts, LM_Inserts_l18_Parts, LM_Inserts_l21_Parts, LM_Inserts_l24_Parts, LM_Inserts_l25_Parts, LM_Inserts_l26_Parts, LM_Inserts_l27_Parts, LM_Inserts_l28_Parts, LM_Inserts_l34_Parts, LM_Inserts_l35_Parts, LM_Inserts_l36_Parts, LM_Inserts_l37_Parts, LM_Inserts_l38_Parts, LM_Inserts_l43_Parts, LM_Inserts_l46_Parts, LM_Inserts_l47_Parts, LM_Inserts_l48_Parts, LM_Inserts_l48a_Parts, LM_Inserts_l51_Parts, LM_Inserts_l52_Parts, LM_Inserts_l54_Parts, _
	LM_Inserts_l58_Parts, LM_Inserts_l61_Parts, LM_Inserts_l62_Parts, LM_Inserts_l63_Parts, LM_Inserts_l67_Parts, LM_Inserts_l68_Parts, LM_Inserts_l71_Parts, LM_Inserts_l72_Parts, LM_Inserts_l73_Parts, LM_Inserts_l74_Parts, LM_Inserts_l75_Parts, LM_Inserts_l76_Parts, LM_Inserts_l77_Parts, LM_Inserts_l78_Parts, LM_Inserts_l81_Parts, LM_Inserts_l82_Parts, LM_Inserts_l83_Parts, LM_Inserts_l84_Parts, LM_Inserts_l85_Parts, LM_Inserts_l86_Parts)
Dim BP_Playfield: BP_Playfield=Array(BM_Playfield, LM_GITop_Playfield, LM_Flashers_f17_Playfield, LM_Flashers_f18_Playfield, LM_Flashers_f19_Playfield, LM_Flashers_f25_Playfield, LM_Flashers_f26_Playfield, LM_Flashers_f27_Playfield, LM_GIBottom_Playfield, LM_GISplit_gi002_Playfield, LM_GISplit_gi006_Playfield, LM_GISplit_gi011_Playfield, LM_GISplit_gi009_Playfield, LM_GISplit_gitop020_Playfield, LM_Inserts_l16_Playfield, LM_Inserts_l17_Playfield, LM_Inserts_l21_Playfield, LM_Inserts_l22_Playfield, LM_Inserts_l23_Playfield, LM_Inserts_l24_Playfield, LM_Inserts_l25_Playfield, LM_Inserts_l26_Playfield, LM_Inserts_l27_Playfield, LM_Inserts_l28_Playfield, LM_Inserts_l31_Playfield, LM_Inserts_l32_Playfield, LM_Inserts_l33_Playfield, LM_Inserts_l34_Playfield, LM_Inserts_l41_Playfield, LM_Inserts_l42_Playfield, LM_Inserts_l43_Playfield, LM_Inserts_l44_Playfield, LM_Inserts_l45_Playfield, LM_Inserts_l46_Playfield, LM_Inserts_l47_Playfield, LM_Inserts_l48_Playfield, LM_Inserts_l48a_Playfield, LM_Inserts_l51_Playfield, _
	LM_Inserts_l52_Playfield, LM_Inserts_l53_Playfield, LM_Inserts_l54_Playfield, LM_Inserts_l55_Playfield, LM_Inserts_l56_Playfield, LM_Inserts_l57_Playfield, LM_Inserts_l58_Playfield, LM_Inserts_l61_Playfield, LM_Inserts_l62_Playfield, LM_Inserts_l63_Playfield, LM_Inserts_l64_Playfield, LM_Inserts_l65_Playfield, LM_Inserts_l66_Playfield, LM_Inserts_l67_Playfield, LM_Inserts_l68_Playfield, LM_Inserts_l71_Playfield, LM_Inserts_l72_Playfield, LM_Inserts_l73_Playfield, LM_Inserts_l74_Playfield, LM_Inserts_l75_Playfield, LM_Inserts_l76_Playfield, LM_Inserts_l77_Playfield, LM_Inserts_l78_Playfield, LM_Inserts_l81_Playfield, LM_Inserts_l82_Playfield, LM_Inserts_l83_Playfield, LM_Inserts_l84_Playfield, LM_Inserts_l85_Playfield)
Dim BP_RailsLockdown: BP_RailsLockdown=Array(BM_RailsLockdown, LM_GITop_RailsLockdown, LM_Flashers_f26_RailsLockdown, LM_GIBottom_RailsLockdown, LM_GISplit_gi002_RailsLockdown, LM_GISplit_gi006_RailsLockdown, LM_GISplit_gi009_RailsLockdown, LM_Inserts_l81_RailsLockdown, LM_Inserts_l82_RailsLockdown)
Dim BP_Ramp: BP_Ramp=Array(BM_Ramp, LM_GITop_Ramp, LM_Flashers_f26_Ramp, LM_GISplit_gitop020_Ramp, LM_Inserts_l16_Ramp, LM_Inserts_l17_Ramp)
Dim BP_RampCovers: BP_RampCovers=Array(BM_RampCovers, LM_GITop_RampCovers, LM_Flashers_f25_RampCovers, LM_Flashers_f26_RampCovers, LM_Flashers_f27_RampCovers, LM_GISplit_gitop020_RampCovers, LM_Inserts_l16_RampCovers, LM_Inserts_l17_RampCovers, LM_Inserts_l81_RampCovers, LM_Inserts_l82_RampCovers, LM_Inserts_l83_RampCovers, LM_Inserts_l84_RampCovers, LM_Inserts_l85_RampCovers)
Dim BP_RampCoversAlt: BP_RampCoversAlt=Array(BM_RampCoversAlt, LM_GITop_RampCoversAlt, LM_Flashers_f17_RampCoversAlt, LM_Flashers_f26_RampCoversAlt, LM_Flashers_f27_RampCoversAlt, LM_GISplit_gitop020_RampCoversA, LM_Inserts_l16_RampCoversAlt, LM_Inserts_l17_RampCoversAlt, LM_Inserts_l81_RampCoversAlt, LM_Inserts_l82_RampCoversAlt, LM_Inserts_l83_RampCoversAlt, LM_Inserts_l84_RampCoversAlt, LM_Inserts_l85_RampCoversAlt)
Dim BP_RampEdges: BP_RampEdges=Array(BM_RampEdges, LM_GITop_RampEdges, LM_Flashers_f17_RampEdges, LM_Flashers_f25_RampEdges, LM_Flashers_f26_RampEdges, LM_Flashers_f27_RampEdges, LM_Inserts_l16_RampEdges, LM_Inserts_l17_RampEdges, LM_Inserts_l18_RampEdges, LM_Inserts_l28_RampEdges, LM_Inserts_l81_RampEdges, LM_Inserts_l82_RampEdges, LM_Inserts_l83_RampEdges, LM_Inserts_l85_RampEdges)
Dim BP_Reel: BP_Reel=Array(BM_Reel, LM_GITop_Reel, LM_Flashers_f18_Reel, LM_Flashers_f25_Reel, LM_Flashers_f26_Reel)
Dim BP_ReelStickerLM: BP_ReelStickerLM=Array(BM_ReelStickerLM, LM_GITop_ReelStickerLM, LM_Flashers_f17_ReelStickerLM, LM_Flashers_f25_ReelStickerLM, LM_Flashers_f26_ReelStickerLM, LM_GIBottom_ReelStickerLM, LM_GISplit_gi009_ReelStickerLM, LM_Inserts_l81_ReelStickerLM, LM_Inserts_l82_ReelStickerLM, LM_Inserts_l83_ReelStickerLM, LM_Inserts_l84_ReelStickerLM, LM_Inserts_l85_ReelStickerLM)
Dim BP_RightPost: BP_RightPost=Array(BM_RightPost, LM_GIBottom_RightPost, LM_GISplit_gi006_RightPost, LM_GISplit_gi011_RightPost, LM_Inserts_l48a_RightPost, LM_Inserts_l68_RightPost)
Dim BP_RightSling1: BP_RightSling1=Array(BM_RightSling1, LM_GIBottom_RightSling1, LM_GISplit_gi002_RightSling1, LM_GISplit_gi006_RightSling1, LM_GISplit_gi011_RightSling1, LM_Inserts_l24_RightSling1, LM_Inserts_l68_RightSling1)
Dim BP_RightSling2: BP_RightSling2=Array(BM_RightSling2, LM_GIBottom_RightSling2, LM_GISplit_gi002_RightSling2, LM_GISplit_gi006_RightSling2, LM_GISplit_gi011_RightSling2, LM_Inserts_l24_RightSling2, LM_Inserts_l68_RightSling2)
Dim BP_RightSling3: BP_RightSling3=Array(BM_RightSling3, LM_GIBottom_RightSling3, LM_GISplit_gi002_RightSling3, LM_GISplit_gi006_RightSling3, LM_GISplit_gi011_RightSling3, LM_Inserts_l24_RightSling3, LM_Inserts_l68_RightSling3)
Dim BP_RightSling4: BP_RightSling4=Array(BM_RightSling4, LM_GIBottom_RightSling4, LM_GISplit_gi002_RightSling4, LM_GISplit_gi006_RightSling4, LM_GISplit_gi011_RightSling4, LM_Inserts_l24_RightSling4, LM_Inserts_l68_RightSling4)
Dim BP_RightSlingArm: BP_RightSlingArm=Array(BM_RightSlingArm, LM_GIBottom_RightSlingArm, LM_GISplit_gi006_RightSlingArm)
Dim BP_Sideblades: BP_Sideblades=Array(BM_Sideblades, LM_GITop_Sideblades, LM_Flashers_f18_Sideblades, LM_Flashers_f25_Sideblades, LM_Flashers_f26_Sideblades, LM_Flashers_f27_Sideblades, LM_GIBottom_Sideblades, LM_GISplit_gi009_Sideblades, LM_GISplit_gitop020_Sideblades, LM_Inserts_l48_Sideblades, LM_Inserts_l77_Sideblades, LM_Inserts_l84_Sideblades, LM_Inserts_l85_Sideblades)
Dim BP_StretchTruth: BP_StretchTruth=Array(BM_StretchTruth, LM_GITop_StretchTruth, LM_Flashers_f17_StretchTruth, LM_Flashers_f19_StretchTruth, LM_Flashers_f25_StretchTruth, LM_Flashers_f26_StretchTruth, LM_Inserts_l81_StretchTruth, LM_Inserts_l82_StretchTruth, LM_Inserts_l83_StretchTruth, LM_Inserts_l84_StretchTruth, LM_Inserts_l85_StretchTruth)
Dim BP_UnderPF: BP_UnderPF=Array(BM_UnderPF, LM_GITop_UnderPF, LM_Flashers_f17_UnderPF, LM_Flashers_f18_UnderPF, LM_Flashers_f19_UnderPF, LM_Flashers_f20_UnderPF, LM_Flashers_f21_UnderPF, LM_Flashers_f22_UnderPF, LM_Flashers_f23_UnderPF, LM_Flashers_f25_UnderPF, LM_Flashers_f26_UnderPF, LM_Flashers_f27_UnderPF, LM_GIBottom_UnderPF, LM_GISplit_gi002_UnderPF, LM_GISplit_gi006_UnderPF, LM_GISplit_gi011_UnderPF, LM_GISplit_gi009_UnderPF, LM_GISplit_gitop020_UnderPF, LM_Inserts_l11_UnderPF, LM_Inserts_l12_UnderPF, LM_Inserts_l13_UnderPF, LM_Inserts_l14_UnderPF, LM_Inserts_l15_UnderPF, LM_Inserts_l16_UnderPF, LM_Inserts_l17_UnderPF, LM_Inserts_l18_UnderPF, LM_Inserts_l21_UnderPF, LM_Inserts_l22_UnderPF, LM_Inserts_l23_UnderPF, LM_Inserts_l24_UnderPF, LM_Inserts_l25_UnderPF, LM_Inserts_l26_UnderPF, LM_Inserts_l27_UnderPF, LM_Inserts_l28_UnderPF, LM_Inserts_l31_UnderPF, LM_Inserts_l32_UnderPF, LM_Inserts_l33_UnderPF, LM_Inserts_l34_UnderPF, LM_Inserts_l35_UnderPF, LM_Inserts_l36_UnderPF, LM_Inserts_l37_UnderPF, _
	LM_Inserts_l38_UnderPF, LM_Inserts_l41_UnderPF, LM_Inserts_l42_UnderPF, LM_Inserts_l43_UnderPF, LM_Inserts_l44_UnderPF, LM_Inserts_l45_UnderPF, LM_Inserts_l46_UnderPF, LM_Inserts_l47_UnderPF, LM_Inserts_l48_UnderPF, LM_Inserts_l48a_UnderPF, LM_Inserts_l51_UnderPF, LM_Inserts_l52_UnderPF, LM_Inserts_l53_UnderPF, LM_Inserts_l54_UnderPF, LM_Inserts_l55_UnderPF, LM_Inserts_l56_UnderPF, LM_Inserts_l57_UnderPF, LM_Inserts_l58_UnderPF, LM_Inserts_l61_UnderPF, LM_Inserts_l62_UnderPF, LM_Inserts_l63_UnderPF, LM_Inserts_l64_UnderPF, LM_Inserts_l65_UnderPF, LM_Inserts_l66_UnderPF, LM_Inserts_l67_UnderPF, LM_Inserts_l68_UnderPF, LM_Inserts_l72_UnderPF, LM_Inserts_l73_UnderPF, LM_Inserts_l74_UnderPF, LM_Inserts_l75_UnderPF, LM_Inserts_l76_UnderPF, LM_Inserts_l77_UnderPF, LM_Inserts_l78_UnderPF, LM_Inserts_l81_UnderPF, LM_Inserts_l82_UnderPF, LM_Inserts_l83_UnderPF, LM_Inserts_l84_UnderPF, LM_Inserts_l85_UnderPF)
Dim BP_sw25: BP_sw25=Array(BM_sw25, LM_GIBottom_sw25, LM_GISplit_gi002_sw25)
Dim BP_sw26: BP_sw26=Array(BM_sw26, LM_GIBottom_sw26, LM_GISplit_gi002_sw26)
Dim BP_sw27: BP_sw27=Array(BM_sw27, LM_GITop_sw27, LM_Flashers_f18_sw27, LM_Flashers_f25_sw27, LM_GIBottom_sw27, LM_GISplit_gi002_sw27, LM_GISplit_gi009_sw27, LM_Inserts_l46_sw27, LM_Inserts_l47_sw27)
Dim BP_sw28: BP_sw28=Array(BM_sw28, LM_GITop_sw28, LM_Flashers_f18_sw28, LM_Flashers_f25_sw28, LM_GIBottom_sw28, LM_GISplit_gi009_sw28, LM_Inserts_l45_sw28, LM_Inserts_l46_sw28)
Dim BP_sw32: BP_sw32=Array(BM_sw32, LM_GITop_sw32, LM_Flashers_f26_sw32)
Dim BP_sw33: BP_sw33=Array(BM_sw33, LM_Flashers_f26_sw33)
Dim BP_sw34: BP_sw34=Array(BM_sw34, LM_GITop_sw34, LM_Flashers_f17_sw34, LM_Flashers_f25_sw34, LM_Flashers_f26_sw34, LM_Inserts_l28_sw34, LM_Inserts_l81_sw34, LM_Inserts_l82_sw34, LM_Inserts_l83_sw34, LM_Inserts_l84_sw34, LM_Inserts_l85_sw34)
Dim BP_sw41: BP_sw41=Array(BM_sw41, LM_GITop_sw41, LM_Flashers_f19_sw41, LM_Flashers_f20_sw41, LM_Flashers_f21_sw41, LM_Flashers_f22_sw41, LM_Flashers_f25_sw41, LM_Flashers_f26_sw41, LM_Inserts_l14_sw41, LM_Inserts_l15_sw41)
Dim BP_sw42a: BP_sw42a=Array(BM_sw42a, LM_GITop_sw42a, LM_Flashers_f26_sw42a)
Dim BP_sw43a: BP_sw43a=Array(BM_sw43a, LM_GITop_sw43a, LM_Flashers_f26_sw43a)
Dim BP_sw44: BP_sw44=Array(BM_sw44, LM_GITop_sw44, LM_Flashers_f26_sw44, LM_GISplit_gitop020_sw44, LM_Inserts_l18_sw44, LM_Inserts_l75_sw44)
Dim BP_sw45: BP_sw45=Array(BM_sw45, LM_GITop_sw45, LM_Flashers_f26_sw45, LM_GISplit_gitop020_sw45, LM_Inserts_l17_sw45)
Dim BP_sw46: BP_sw46=Array(BM_sw46, LM_GITop_sw46, LM_Flashers_f26_sw46, LM_Inserts_l16_sw46)
Dim BP_sw48: BP_sw48=Array(BM_sw48, LM_GITop_sw48, LM_Flashers_f27_sw48, LM_Inserts_l71_sw48, LM_Inserts_l75_sw48)
Dim BP_sw54: BP_sw54=Array(BM_sw54, LM_Flashers_f18_sw54, LM_GIBottom_sw54, LM_GISplit_gi011_sw54)
Dim BP_sw55: BP_sw55=Array(BM_sw55, LM_Flashers_f18_sw55, LM_GIBottom_sw55, LM_GISplit_gi011_sw55, LM_Inserts_l55_sw55)
Dim BP_sw56: BP_sw56=Array(BM_sw56)
Dim BP_sw61: BP_sw61=Array(BM_sw61, LM_GITop_sw61, LM_Flashers_f27_sw61, LM_Inserts_l71_sw61, LM_Inserts_l75_sw61, LM_Inserts_l78_sw61)
Dim BP_sw62: BP_sw62=Array(BM_sw62, LM_GITop_sw62, LM_Flashers_f26_sw62, LM_Flashers_f27_sw62, LM_GISplit_gitop020_sw62)
Dim BP_sw64: BP_sw64=Array(BM_sw64, LM_GITop_sw64, LM_Flashers_f26_sw64)
Dim BP_sw65: BP_sw65=Array(BM_sw65, LM_GIBottom_sw65, LM_GISplit_gi006_sw65)
Dim BP_sw66: BP_sw66=Array(BM_sw66, LM_GIBottom_sw66, LM_GISplit_gi006_sw66)
' Arrays per lighting scenario
Dim BL_Flashers_f17: BL_Flashers_f17=Array(LM_Flashers_f17_ClearPlastics, LM_Flashers_f17_FishingRod, LM_Flashers_f17_MetalRamps, LM_Flashers_f17_Parts, LM_Flashers_f17_Playfield, LM_Flashers_f17_RampCoversAlt, LM_Flashers_f17_RampEdges, LM_Flashers_f17_ReelStickerLM, LM_Flashers_f17_StretchTruth, LM_Flashers_f17_UnderPF, LM_Flashers_f17_sw34)
Dim BL_Flashers_f18: BL_Flashers_f18=Array(LM_Flashers_f18_MetalRamps, LM_Flashers_f18_Parts, LM_Flashers_f18_Playfield, LM_Flashers_f18_Reel, LM_Flashers_f18_Sideblades, LM_Flashers_f18_UnderPF, LM_Flashers_f18_sw27, LM_Flashers_f18_sw28, LM_Flashers_f18_sw54, LM_Flashers_f18_sw55)
Dim BL_Flashers_f19: BL_Flashers_f19=Array(LM_Flashers_f19_BoatPlastic, LM_Flashers_f19_MetalRamps, LM_Flashers_f19_MiniPF, LM_Flashers_f19_Parts, LM_Flashers_f19_Playfield, LM_Flashers_f19_StretchTruth, LM_Flashers_f19_UnderPF, LM_Flashers_f19_sw41)
Dim BL_Flashers_f20: BL_Flashers_f20=Array(LM_Flashers_f20_BoatPlastic, LM_Flashers_f20_MetalRamps, LM_Flashers_f20_MiniPF, LM_Flashers_f20_Parts, LM_Flashers_f20_UnderPF, LM_Flashers_f20_sw41)
Dim BL_Flashers_f21: BL_Flashers_f21=Array(LM_Flashers_f21_BoatPlastic, LM_Flashers_f21_MetalRamps, LM_Flashers_f21_MiniPF, LM_Flashers_f21_Parts, LM_Flashers_f21_UnderPF, LM_Flashers_f21_sw41)
Dim BL_Flashers_f22: BL_Flashers_f22=Array(LM_Flashers_f22_BoatPlastic, LM_Flashers_f22_MetalRamps, LM_Flashers_f22_MiniPF, LM_Flashers_f22_Parts, LM_Flashers_f22_UnderPF, LM_Flashers_f22_sw41)
Dim BL_Flashers_f23: BL_Flashers_f23=Array(LM_Flashers_f23_BoatPlastic, LM_Flashers_f23_MetalRamps, LM_Flashers_f23_MiniPF, LM_Flashers_f23_Parts, LM_Flashers_f23_UnderPF)
Dim BL_Flashers_f25: BL_Flashers_f25=Array(LM_Flashers_f25_BoatPlastic, LM_Flashers_f25_Catapult, LM_Flashers_f25_ClearPlastics, LM_Flashers_f25_FishingRod, LM_Flashers_f25_MetalRamps, LM_Flashers_f25_MiniPF, LM_Flashers_f25_Parts, LM_Flashers_f25_Playfield, LM_Flashers_f25_RampCovers, LM_Flashers_f25_RampEdges, LM_Flashers_f25_Reel, LM_Flashers_f25_ReelStickerLM, LM_Flashers_f25_Sideblades, LM_Flashers_f25_StretchTruth, LM_Flashers_f25_UnderPF, LM_Flashers_f25_sw27, LM_Flashers_f25_sw28, LM_Flashers_f25_sw34, LM_Flashers_f25_sw41)
Dim BL_Flashers_f26: BL_Flashers_f26=Array(LM_Flashers_f26_BoatPlastic, LM_Flashers_f26_Bumper_Socket, LM_Flashers_f26_Bumper_Socket_0, LM_Flashers_f26_Bumper_Socket_0, LM_Flashers_f26_Bumper1Ring, LM_Flashers_f26_Bumper2Ring, LM_Flashers_f26_Bumper3Ring, LM_Flashers_f26_BumperEdges, LM_Flashers_f26_Bumpers, LM_Flashers_f26_ClearPlastics, LM_Flashers_f26_FishingRod, LM_Flashers_f26_Gate, LM_Flashers_f26_MetalRamps, LM_Flashers_f26_MiniPF, LM_Flashers_f26_Parts, LM_Flashers_f26_Playfield, LM_Flashers_f26_RailsLockdown, LM_Flashers_f26_Ramp, LM_Flashers_f26_RampCovers, LM_Flashers_f26_RampCoversAlt, LM_Flashers_f26_RampEdges, LM_Flashers_f26_Reel, LM_Flashers_f26_ReelStickerLM, LM_Flashers_f26_Sideblades, LM_Flashers_f26_StretchTruth, LM_Flashers_f26_UnderPF, LM_Flashers_f26_sw32, LM_Flashers_f26_sw33, LM_Flashers_f26_sw34, LM_Flashers_f26_sw41, LM_Flashers_f26_sw42a, LM_Flashers_f26_sw43a, LM_Flashers_f26_sw44, LM_Flashers_f26_sw45, LM_Flashers_f26_sw46, LM_Flashers_f26_sw62, LM_Flashers_f26_sw64)
Dim BL_Flashers_f27: BL_Flashers_f27=Array(LM_Flashers_f27_Bumper_Socket_0, LM_Flashers_f27_Bumper_Socket_0, LM_Flashers_f27_Bumper2Ring, LM_Flashers_f27_Bumper3Ring, LM_Flashers_f27_BumperEdges, LM_Flashers_f27_MetalRamps, LM_Flashers_f27_Parts, LM_Flashers_f27_Playfield, LM_Flashers_f27_RampCovers, LM_Flashers_f27_RampCoversAlt, LM_Flashers_f27_RampEdges, LM_Flashers_f27_Sideblades, LM_Flashers_f27_UnderPF, LM_Flashers_f27_sw48, LM_Flashers_f27_sw61, LM_Flashers_f27_sw62)
Dim BL_GIBottom: BL_GIBottom=Array(LM_GIBottom_FlipperLDown, LM_GIBottom_FlipperLUp, LM_GIBottom_FlipperRDown, LM_GIBottom_FlipperRUp, LM_GIBottom_LeftPost, LM_GIBottom_LeftSling1, LM_GIBottom_LeftSling2, LM_GIBottom_LeftSling3, LM_GIBottom_LeftSling4, LM_GIBottom_LeftSlingArm, LM_GIBottom_MetalRamps, LM_GIBottom_Parts, LM_GIBottom_Playfield, LM_GIBottom_RailsLockdown, LM_GIBottom_ReelStickerLM, LM_GIBottom_RightPost, LM_GIBottom_RightSling1, LM_GIBottom_RightSling2, LM_GIBottom_RightSling3, LM_GIBottom_RightSling4, LM_GIBottom_RightSlingArm, LM_GIBottom_Sideblades, LM_GIBottom_UnderPF, LM_GIBottom_sw25, LM_GIBottom_sw26, LM_GIBottom_sw27, LM_GIBottom_sw28, LM_GIBottom_sw54, LM_GIBottom_sw55, LM_GIBottom_sw65, LM_GIBottom_sw66)
Dim BL_GISplit_gi002: BL_GISplit_gi002=Array(LM_GISplit_gi002_FlipperLDown, LM_GISplit_gi002_FlipperLUp, LM_GISplit_gi002_FlipperRDown, LM_GISplit_gi002_FlipperRUp, LM_GISplit_gi002_LeftPost, LM_GISplit_gi002_LeftSling1, LM_GISplit_gi002_LeftSling2, LM_GISplit_gi002_LeftSling3, LM_GISplit_gi002_LeftSling4, LM_GISplit_gi002_LeftSlingArm, LM_GISplit_gi002_MetalRamps, LM_GISplit_gi002_Parts, LM_GISplit_gi002_Playfield, LM_GISplit_gi002_RailsLockdown, LM_GISplit_gi002_RightSling1, LM_GISplit_gi002_RightSling2, LM_GISplit_gi002_RightSling3, LM_GISplit_gi002_RightSling4, LM_GISplit_gi002_UnderPF, LM_GISplit_gi002_sw25, LM_GISplit_gi002_sw26, LM_GISplit_gi002_sw27)
Dim BL_GISplit_gi006: BL_GISplit_gi006=Array(LM_GISplit_gi006_FlipperLDown, LM_GISplit_gi006_FlipperLUp, LM_GISplit_gi006_FlipperRDown, LM_GISplit_gi006_FlipperRUp, LM_GISplit_gi006_LeftSling1, LM_GISplit_gi006_LeftSling2, LM_GISplit_gi006_LeftSling3, LM_GISplit_gi006_LeftSling4, LM_GISplit_gi006_MetalRamps, LM_GISplit_gi006_Parts, LM_GISplit_gi006_Playfield, LM_GISplit_gi006_RailsLockdown, LM_GISplit_gi006_RightPost, LM_GISplit_gi006_RightSling1, LM_GISplit_gi006_RightSling2, LM_GISplit_gi006_RightSling3, LM_GISplit_gi006_RightSling4, LM_GISplit_gi006_RightSlingArm, LM_GISplit_gi006_UnderPF, LM_GISplit_gi006_sw65, LM_GISplit_gi006_sw66)
Dim BL_GISplit_gi009: BL_GISplit_gi009=Array(LM_GISplit_gi009_Catapult, LM_GISplit_gi009_FishingRod, LM_GISplit_gi009_LeftPost, LM_GISplit_gi009_MetalRamps, LM_GISplit_gi009_Parts, LM_GISplit_gi009_Playfield, LM_GISplit_gi009_RailsLockdown, LM_GISplit_gi009_ReelStickerLM, LM_GISplit_gi009_Sideblades, LM_GISplit_gi009_UnderPF, LM_GISplit_gi009_sw27, LM_GISplit_gi009_sw28)
Dim BL_GISplit_gi011: BL_GISplit_gi011=Array(LM_GISplit_gi011_MetalRamps, LM_GISplit_gi011_Parts, LM_GISplit_gi011_Playfield, LM_GISplit_gi011_RightPost, LM_GISplit_gi011_RightSling1, LM_GISplit_gi011_RightSling2, LM_GISplit_gi011_RightSling3, LM_GISplit_gi011_RightSling4, LM_GISplit_gi011_UnderPF, LM_GISplit_gi011_sw54, LM_GISplit_gi011_sw55)
Dim BL_GISplit_gitop020: BL_GISplit_gitop020=Array(LM_GISplit_gitop020_Bumper_Sock, LM_GISplit_gitop020_Bumper_Sock, LM_GISplit_gitop020_Bumper_Sock, LM_GISplit_gitop020_Bumper2Ring, LM_GISplit_gitop020_Bumper3Ring, LM_GISplit_gitop020_BumperEdges, LM_GISplit_gitop020_ClearPlasti, LM_GISplit_gitop020_FishingRod, LM_GISplit_gitop020_MetalRamps, LM_GISplit_gitop020_Parts, LM_GISplit_gitop020_Playfield, LM_GISplit_gitop020_Ramp, LM_GISplit_gitop020_RampCovers, LM_GISplit_gitop020_RampCoversA, LM_GISplit_gitop020_Sideblades, LM_GISplit_gitop020_UnderPF, LM_GISplit_gitop020_sw44, LM_GISplit_gitop020_sw45, LM_GISplit_gitop020_sw62)
Dim BL_GITop: BL_GITop=Array(LM_GITop_BoatPlastic, LM_GITop_Bumper_Socket, LM_GITop_Bumper_Socket_001, LM_GITop_Bumper_Socket_002, LM_GITop_Bumper1Ring, LM_GITop_Bumper2Ring, LM_GITop_Bumper3Ring, LM_GITop_BumperEdges, LM_GITop_Bumpers, LM_GITop_Catapult, LM_GITop_ClearPlastics, LM_GITop_FishingRod, LM_GITop_Gate, LM_GITop_MetalRamps, LM_GITop_MiniPF, LM_GITop_Parts, LM_GITop_Playfield, LM_GITop_RailsLockdown, LM_GITop_Ramp, LM_GITop_RampCovers, LM_GITop_RampCoversAlt, LM_GITop_RampEdges, LM_GITop_Reel, LM_GITop_ReelStickerLM, LM_GITop_Sideblades, LM_GITop_StretchTruth, LM_GITop_UnderPF, LM_GITop_sw27, LM_GITop_sw28, LM_GITop_sw32, LM_GITop_sw34, LM_GITop_sw41, LM_GITop_sw42a, LM_GITop_sw43a, LM_GITop_sw44, LM_GITop_sw45, LM_GITop_sw46, LM_GITop_sw48, LM_GITop_sw61, LM_GITop_sw62, LM_GITop_sw64)
Dim BL_Inserts_l11: BL_Inserts_l11=Array(LM_Inserts_l11_BoatPlastic, LM_Inserts_l11_MetalRamps, LM_Inserts_l11_MiniPF, LM_Inserts_l11_Parts, LM_Inserts_l11_UnderPF)
Dim BL_Inserts_l12: BL_Inserts_l12=Array(LM_Inserts_l12_BoatPlastic, LM_Inserts_l12_MetalRamps, LM_Inserts_l12_MiniPF, LM_Inserts_l12_Parts, LM_Inserts_l12_UnderPF)
Dim BL_Inserts_l13: BL_Inserts_l13=Array(LM_Inserts_l13_BoatPlastic, LM_Inserts_l13_MetalRamps, LM_Inserts_l13_MiniPF, LM_Inserts_l13_Parts, LM_Inserts_l13_UnderPF)
Dim BL_Inserts_l14: BL_Inserts_l14=Array(LM_Inserts_l14_BoatPlastic, LM_Inserts_l14_MetalRamps, LM_Inserts_l14_MiniPF, LM_Inserts_l14_Parts, LM_Inserts_l14_UnderPF, LM_Inserts_l14_sw41)
Dim BL_Inserts_l15: BL_Inserts_l15=Array(LM_Inserts_l15_MetalRamps, LM_Inserts_l15_MiniPF, LM_Inserts_l15_Parts, LM_Inserts_l15_UnderPF, LM_Inserts_l15_sw41)
Dim BL_Inserts_l16: BL_Inserts_l16=Array(LM_Inserts_l16_Bumper_Socket, LM_Inserts_l16_Bumper1Ring, LM_Inserts_l16_MetalRamps, LM_Inserts_l16_Parts, LM_Inserts_l16_Playfield, LM_Inserts_l16_Ramp, LM_Inserts_l16_RampCovers, LM_Inserts_l16_RampCoversAlt, LM_Inserts_l16_RampEdges, LM_Inserts_l16_UnderPF, LM_Inserts_l16_sw46)
Dim BL_Inserts_l17: BL_Inserts_l17=Array(LM_Inserts_l17_MetalRamps, LM_Inserts_l17_Parts, LM_Inserts_l17_Playfield, LM_Inserts_l17_Ramp, LM_Inserts_l17_RampCovers, LM_Inserts_l17_RampCoversAlt, LM_Inserts_l17_RampEdges, LM_Inserts_l17_UnderPF, LM_Inserts_l17_sw45)
Dim BL_Inserts_l18: BL_Inserts_l18=Array(LM_Inserts_l18_BumperEdges, LM_Inserts_l18_MetalRamps, LM_Inserts_l18_Parts, LM_Inserts_l18_RampEdges, LM_Inserts_l18_UnderPF, LM_Inserts_l18_sw44)
Dim BL_Inserts_l21: BL_Inserts_l21=Array(LM_Inserts_l21_LeftSling1, LM_Inserts_l21_LeftSling2, LM_Inserts_l21_LeftSling3, LM_Inserts_l21_LeftSling4, LM_Inserts_l21_MetalRamps, LM_Inserts_l21_Parts, LM_Inserts_l21_Playfield, LM_Inserts_l21_UnderPF)
Dim BL_Inserts_l22: BL_Inserts_l22=Array(LM_Inserts_l22_MetalRamps, LM_Inserts_l22_Playfield, LM_Inserts_l22_UnderPF)
Dim BL_Inserts_l23: BL_Inserts_l23=Array(LM_Inserts_l23_MetalRamps, LM_Inserts_l23_Playfield, LM_Inserts_l23_UnderPF)
Dim BL_Inserts_l24: BL_Inserts_l24=Array(LM_Inserts_l24_MetalRamps, LM_Inserts_l24_Parts, LM_Inserts_l24_Playfield, LM_Inserts_l24_RightSling1, LM_Inserts_l24_RightSling2, LM_Inserts_l24_RightSling3, LM_Inserts_l24_RightSling4, LM_Inserts_l24_UnderPF)
Dim BL_Inserts_l25: BL_Inserts_l25=Array(LM_Inserts_l25_MetalRamps, LM_Inserts_l25_Parts, LM_Inserts_l25_Playfield, LM_Inserts_l25_UnderPF)
Dim BL_Inserts_l26: BL_Inserts_l26=Array(LM_Inserts_l26_FishingRod, LM_Inserts_l26_MetalRamps, LM_Inserts_l26_Parts, LM_Inserts_l26_Playfield, LM_Inserts_l26_UnderPF)
Dim BL_Inserts_l27: BL_Inserts_l27=Array(LM_Inserts_l27_FishingRod, LM_Inserts_l27_MetalRamps, LM_Inserts_l27_Parts, LM_Inserts_l27_Playfield, LM_Inserts_l27_UnderPF)
Dim BL_Inserts_l28: BL_Inserts_l28=Array(LM_Inserts_l28_FishingRod, LM_Inserts_l28_MetalRamps, LM_Inserts_l28_Parts, LM_Inserts_l28_Playfield, LM_Inserts_l28_RampEdges, LM_Inserts_l28_UnderPF, LM_Inserts_l28_sw34)
Dim BL_Inserts_l31: BL_Inserts_l31=Array(LM_Inserts_l31_MetalRamps, LM_Inserts_l31_Playfield, LM_Inserts_l31_UnderPF)
Dim BL_Inserts_l32: BL_Inserts_l32=Array(LM_Inserts_l32_Playfield, LM_Inserts_l32_UnderPF)
Dim BL_Inserts_l33: BL_Inserts_l33=Array(LM_Inserts_l33_Playfield, LM_Inserts_l33_UnderPF)
Dim BL_Inserts_l34: BL_Inserts_l34=Array(LM_Inserts_l34_MetalRamps, LM_Inserts_l34_Parts, LM_Inserts_l34_Playfield, LM_Inserts_l34_UnderPF)
Dim BL_Inserts_l35: BL_Inserts_l35=Array(LM_Inserts_l35_BoatPlastic, LM_Inserts_l35_MetalRamps, LM_Inserts_l35_MiniPF, LM_Inserts_l35_Parts, LM_Inserts_l35_UnderPF)
Dim BL_Inserts_l36: BL_Inserts_l36=Array(LM_Inserts_l36_BoatPlastic, LM_Inserts_l36_MetalRamps, LM_Inserts_l36_MiniPF, LM_Inserts_l36_Parts, LM_Inserts_l36_UnderPF)
Dim BL_Inserts_l37: BL_Inserts_l37=Array(LM_Inserts_l37_BoatPlastic, LM_Inserts_l37_MetalRamps, LM_Inserts_l37_MiniPF, LM_Inserts_l37_Parts, LM_Inserts_l37_UnderPF)
Dim BL_Inserts_l38: BL_Inserts_l38=Array(LM_Inserts_l38_BoatPlastic, LM_Inserts_l38_MetalRamps, LM_Inserts_l38_MiniPF, LM_Inserts_l38_Parts, LM_Inserts_l38_UnderPF)
Dim BL_Inserts_l41: BL_Inserts_l41=Array(LM_Inserts_l41_Playfield, LM_Inserts_l41_UnderPF)
Dim BL_Inserts_l42: BL_Inserts_l42=Array(LM_Inserts_l42_Playfield, LM_Inserts_l42_UnderPF)
Dim BL_Inserts_l43: BL_Inserts_l43=Array(LM_Inserts_l43_FlipperLDown, LM_Inserts_l43_FlipperLUp, LM_Inserts_l43_FlipperRDown, LM_Inserts_l43_FlipperRUp, LM_Inserts_l43_Parts, LM_Inserts_l43_Playfield, LM_Inserts_l43_UnderPF)
Dim BL_Inserts_l44: BL_Inserts_l44=Array(LM_Inserts_l44_Playfield, LM_Inserts_l44_UnderPF)
Dim BL_Inserts_l45: BL_Inserts_l45=Array(LM_Inserts_l45_MetalRamps, LM_Inserts_l45_Playfield, LM_Inserts_l45_UnderPF, LM_Inserts_l45_sw28)
Dim BL_Inserts_l46: BL_Inserts_l46=Array(LM_Inserts_l46_MetalRamps, LM_Inserts_l46_Parts, LM_Inserts_l46_Playfield, LM_Inserts_l46_UnderPF, LM_Inserts_l46_sw27, LM_Inserts_l46_sw28)
Dim BL_Inserts_l47: BL_Inserts_l47=Array(LM_Inserts_l47_MetalRamps, LM_Inserts_l47_Parts, LM_Inserts_l47_Playfield, LM_Inserts_l47_UnderPF, LM_Inserts_l47_sw27)
Dim BL_Inserts_l48: BL_Inserts_l48=Array(LM_Inserts_l48_LeftPost, LM_Inserts_l48_MetalRamps, LM_Inserts_l48_Parts, LM_Inserts_l48_Playfield, LM_Inserts_l48_Sideblades, LM_Inserts_l48_UnderPF)
Dim BL_Inserts_l48a: BL_Inserts_l48a=Array(LM_Inserts_l48a_MetalRamps, LM_Inserts_l48a_Parts, LM_Inserts_l48a_Playfield, LM_Inserts_l48a_RightPost, LM_Inserts_l48a_UnderPF)
Dim BL_Inserts_l51: BL_Inserts_l51=Array(LM_Inserts_l51_FlipperLDown, LM_Inserts_l51_FlipperLUp, LM_Inserts_l51_Parts, LM_Inserts_l51_Playfield, LM_Inserts_l51_UnderPF)
Dim BL_Inserts_l52: BL_Inserts_l52=Array(LM_Inserts_l52_FlipperLDown, LM_Inserts_l52_FlipperLUp, LM_Inserts_l52_FlipperRDown, LM_Inserts_l52_FlipperRUp, LM_Inserts_l52_Parts, LM_Inserts_l52_Playfield, LM_Inserts_l52_UnderPF)
Dim BL_Inserts_l53: BL_Inserts_l53=Array(LM_Inserts_l53_Playfield, LM_Inserts_l53_UnderPF)
Dim BL_Inserts_l54: BL_Inserts_l54=Array(LM_Inserts_l54_FlipperRDown, LM_Inserts_l54_FlipperRUp, LM_Inserts_l54_Parts, LM_Inserts_l54_Playfield, LM_Inserts_l54_UnderPF)
Dim BL_Inserts_l55: BL_Inserts_l55=Array(LM_Inserts_l55_MetalRamps, LM_Inserts_l55_Playfield, LM_Inserts_l55_UnderPF, LM_Inserts_l55_sw55)
Dim BL_Inserts_l56: BL_Inserts_l56=Array(LM_Inserts_l56_MetalRamps, LM_Inserts_l56_Playfield, LM_Inserts_l56_UnderPF)
Dim BL_Inserts_l57: BL_Inserts_l57=Array(LM_Inserts_l57_MetalRamps, LM_Inserts_l57_Playfield, LM_Inserts_l57_UnderPF)
Dim BL_Inserts_l58: BL_Inserts_l58=Array(LM_Inserts_l58_LeftPost, LM_Inserts_l58_LeftSling1, LM_Inserts_l58_LeftSling2, LM_Inserts_l58_LeftSling3, LM_Inserts_l58_LeftSling4, LM_Inserts_l58_MetalRamps, LM_Inserts_l58_Parts, LM_Inserts_l58_Playfield, LM_Inserts_l58_UnderPF)
Dim BL_Inserts_l61: BL_Inserts_l61=Array(LM_Inserts_l61_MetalRamps, LM_Inserts_l61_Parts, LM_Inserts_l61_Playfield, LM_Inserts_l61_UnderPF)
Dim BL_Inserts_l62: BL_Inserts_l62=Array(LM_Inserts_l62_MetalRamps, LM_Inserts_l62_Parts, LM_Inserts_l62_Playfield, LM_Inserts_l62_UnderPF)
Dim BL_Inserts_l63: BL_Inserts_l63=Array(LM_Inserts_l63_MetalRamps, LM_Inserts_l63_Parts, LM_Inserts_l63_Playfield, LM_Inserts_l63_UnderPF)
Dim BL_Inserts_l64: BL_Inserts_l64=Array(LM_Inserts_l64_MetalRamps, LM_Inserts_l64_Playfield, LM_Inserts_l64_UnderPF)
Dim BL_Inserts_l65: BL_Inserts_l65=Array(LM_Inserts_l65_MetalRamps, LM_Inserts_l65_Playfield, LM_Inserts_l65_UnderPF)
Dim BL_Inserts_l66: BL_Inserts_l66=Array(LM_Inserts_l66_MetalRamps, LM_Inserts_l66_Playfield, LM_Inserts_l66_UnderPF)
Dim BL_Inserts_l67: BL_Inserts_l67=Array(LM_Inserts_l67_MetalRamps, LM_Inserts_l67_Parts, LM_Inserts_l67_Playfield, LM_Inserts_l67_UnderPF)
Dim BL_Inserts_l68: BL_Inserts_l68=Array(LM_Inserts_l68_MetalRamps, LM_Inserts_l68_Parts, LM_Inserts_l68_Playfield, LM_Inserts_l68_RightPost, LM_Inserts_l68_RightSling1, LM_Inserts_l68_RightSling2, LM_Inserts_l68_RightSling3, LM_Inserts_l68_RightSling4, LM_Inserts_l68_UnderPF)
Dim BL_Inserts_l71: BL_Inserts_l71=Array(LM_Inserts_l71_MetalRamps, LM_Inserts_l71_Parts, LM_Inserts_l71_Playfield, LM_Inserts_l71_sw48, LM_Inserts_l71_sw61)
Dim BL_Inserts_l72: BL_Inserts_l72=Array(LM_Inserts_l72_MetalRamps, LM_Inserts_l72_Parts, LM_Inserts_l72_Playfield, LM_Inserts_l72_UnderPF)
Dim BL_Inserts_l73: BL_Inserts_l73=Array(LM_Inserts_l73_MetalRamps, LM_Inserts_l73_Parts, LM_Inserts_l73_Playfield, LM_Inserts_l73_UnderPF)
Dim BL_Inserts_l74: BL_Inserts_l74=Array(LM_Inserts_l74_MetalRamps, LM_Inserts_l74_Parts, LM_Inserts_l74_Playfield, LM_Inserts_l74_UnderPF)
Dim BL_Inserts_l75: BL_Inserts_l75=Array(LM_Inserts_l75_MetalRamps, LM_Inserts_l75_Parts, LM_Inserts_l75_Playfield, LM_Inserts_l75_UnderPF, LM_Inserts_l75_sw44, LM_Inserts_l75_sw48, LM_Inserts_l75_sw61)
Dim BL_Inserts_l76: BL_Inserts_l76=Array(LM_Inserts_l76_MetalRamps, LM_Inserts_l76_Parts, LM_Inserts_l76_Playfield, LM_Inserts_l76_UnderPF)
Dim BL_Inserts_l77: BL_Inserts_l77=Array(LM_Inserts_l77_MetalRamps, LM_Inserts_l77_Parts, LM_Inserts_l77_Playfield, LM_Inserts_l77_Sideblades, LM_Inserts_l77_UnderPF)
Dim BL_Inserts_l78: BL_Inserts_l78=Array(LM_Inserts_l78_MetalRamps, LM_Inserts_l78_Parts, LM_Inserts_l78_Playfield, LM_Inserts_l78_UnderPF, LM_Inserts_l78_sw61)
Dim BL_Inserts_l81: BL_Inserts_l81=Array(LM_Inserts_l81_FishingRod, LM_Inserts_l81_MetalRamps, LM_Inserts_l81_Parts, LM_Inserts_l81_Playfield, LM_Inserts_l81_RailsLockdown, LM_Inserts_l81_RampCovers, LM_Inserts_l81_RampCoversAlt, LM_Inserts_l81_RampEdges, LM_Inserts_l81_ReelStickerLM, LM_Inserts_l81_StretchTruth, LM_Inserts_l81_UnderPF, LM_Inserts_l81_sw34)
Dim BL_Inserts_l82: BL_Inserts_l82=Array(LM_Inserts_l82_FishingRod, LM_Inserts_l82_MetalRamps, LM_Inserts_l82_Parts, LM_Inserts_l82_Playfield, LM_Inserts_l82_RailsLockdown, LM_Inserts_l82_RampCovers, LM_Inserts_l82_RampCoversAlt, LM_Inserts_l82_RampEdges, LM_Inserts_l82_ReelStickerLM, LM_Inserts_l82_StretchTruth, LM_Inserts_l82_UnderPF, LM_Inserts_l82_sw34)
Dim BL_Inserts_l83: BL_Inserts_l83=Array(LM_Inserts_l83_FishingRod, LM_Inserts_l83_MetalRamps, LM_Inserts_l83_Parts, LM_Inserts_l83_Playfield, LM_Inserts_l83_RampCovers, LM_Inserts_l83_RampCoversAlt, LM_Inserts_l83_RampEdges, LM_Inserts_l83_ReelStickerLM, LM_Inserts_l83_StretchTruth, LM_Inserts_l83_UnderPF, LM_Inserts_l83_sw34)
Dim BL_Inserts_l84: BL_Inserts_l84=Array(LM_Inserts_l84_FishingRod, LM_Inserts_l84_MetalRamps, LM_Inserts_l84_Parts, LM_Inserts_l84_Playfield, LM_Inserts_l84_RampCovers, LM_Inserts_l84_RampCoversAlt, LM_Inserts_l84_ReelStickerLM, LM_Inserts_l84_Sideblades, LM_Inserts_l84_StretchTruth, LM_Inserts_l84_UnderPF, LM_Inserts_l84_sw34)
Dim BL_Inserts_l85: BL_Inserts_l85=Array(LM_Inserts_l85_FishingRod, LM_Inserts_l85_MetalRamps, LM_Inserts_l85_Parts, LM_Inserts_l85_Playfield, LM_Inserts_l85_RampCovers, LM_Inserts_l85_RampCoversAlt, LM_Inserts_l85_RampEdges, LM_Inserts_l85_ReelStickerLM, LM_Inserts_l85_Sideblades, LM_Inserts_l85_StretchTruth, LM_Inserts_l85_UnderPF, LM_Inserts_l85_sw34)
Dim BL_Inserts_l86: BL_Inserts_l86=Array(LM_Inserts_l86_MetalRamps, LM_Inserts_l86_Parts)
Dim BL_Room: BL_Room=Array(BM_BoatPlastic, BM_Bumper_Socket, BM_Bumper_Socket_001, BM_Bumper_Socket_002, BM_Bumper1Ring, BM_Bumper2Ring, BM_Bumper3Ring, BM_BumperEdges, BM_Bumpers, BM_Catapult, BM_ClearPlastics, BM_FishingRod, BM_FlipperLDown, BM_FlipperLUp, BM_FlipperRDown, BM_FlipperRUp, BM_Gate, BM_LeftPost, BM_LeftSling1, BM_LeftSling2, BM_LeftSling3, BM_LeftSling4, BM_LeftSlingArm, BM_MetalRamps, BM_MiniPF, BM_Parts, BM_Playfield, BM_RailsLockdown, BM_Ramp, BM_RampCovers, BM_RampCoversAlt, BM_RampEdges, BM_Reel, BM_ReelStickerLM, BM_RightPost, BM_RightSling1, BM_RightSling2, BM_RightSling3, BM_RightSling4, BM_RightSlingArm, BM_Sideblades, BM_StretchTruth, BM_UnderPF, BM_sw25, BM_sw26, BM_sw27, BM_sw28, BM_sw32, BM_sw33, BM_sw34, BM_sw41, BM_sw42a, BM_sw43a, BM_sw44, BM_sw45, BM_sw46, BM_sw48, BM_sw54, BM_sw55, BM_sw56, BM_sw61, BM_sw62, BM_sw64, BM_sw65, BM_sw66)
' Global arrays
Dim BG_Bakemap: BG_Bakemap=Array(BM_BoatPlastic, BM_Bumper_Socket, BM_Bumper_Socket_001, BM_Bumper_Socket_002, BM_Bumper1Ring, BM_Bumper2Ring, BM_Bumper3Ring, BM_BumperEdges, BM_Bumpers, BM_Catapult, BM_ClearPlastics, BM_FishingRod, BM_FlipperLDown, BM_FlipperLUp, BM_FlipperRDown, BM_FlipperRUp, BM_Gate, BM_LeftPost, BM_LeftSling1, BM_LeftSling2, BM_LeftSling3, BM_LeftSling4, BM_LeftSlingArm, BM_MetalRamps, BM_MiniPF, BM_Parts, BM_Playfield, BM_RailsLockdown, BM_Ramp, BM_RampCovers, BM_RampCoversAlt, BM_RampEdges, BM_Reel, BM_ReelStickerLM, BM_RightPost, BM_RightSling1, BM_RightSling2, BM_RightSling3, BM_RightSling4, BM_RightSlingArm, BM_Sideblades, BM_StretchTruth, BM_UnderPF, BM_sw25, BM_sw26, BM_sw27, BM_sw28, BM_sw32, BM_sw33, BM_sw34, BM_sw41, BM_sw42a, BM_sw43a, BM_sw44, BM_sw45, BM_sw46, BM_sw48, BM_sw54, BM_sw55, BM_sw56, BM_sw61, BM_sw62, BM_sw64, BM_sw65, BM_sw66)
Dim BG_Lightmap: BG_Lightmap=Array(LM_Flashers_f17_ClearPlastics, LM_Flashers_f17_FishingRod, LM_Flashers_f17_MetalRamps, LM_Flashers_f17_Parts, LM_Flashers_f17_Playfield, LM_Flashers_f17_RampCoversAlt, LM_Flashers_f17_RampEdges, LM_Flashers_f17_ReelStickerLM, LM_Flashers_f17_StretchTruth, LM_Flashers_f17_UnderPF, LM_Flashers_f17_sw34, LM_Flashers_f18_MetalRamps, LM_Flashers_f18_Parts, LM_Flashers_f18_Playfield, LM_Flashers_f18_Reel, LM_Flashers_f18_Sideblades, LM_Flashers_f18_UnderPF, LM_Flashers_f18_sw27, LM_Flashers_f18_sw28, LM_Flashers_f18_sw54, LM_Flashers_f18_sw55, LM_Flashers_f19_BoatPlastic, LM_Flashers_f19_MetalRamps, LM_Flashers_f19_MiniPF, LM_Flashers_f19_Parts, LM_Flashers_f19_Playfield, LM_Flashers_f19_StretchTruth, LM_Flashers_f19_UnderPF, LM_Flashers_f19_sw41, LM_Flashers_f20_BoatPlastic, LM_Flashers_f20_MetalRamps, LM_Flashers_f20_MiniPF, LM_Flashers_f20_Parts, LM_Flashers_f20_UnderPF, LM_Flashers_f20_sw41, LM_Flashers_f21_BoatPlastic, LM_Flashers_f21_MetalRamps, LM_Flashers_f21_MiniPF, _
	LM_Flashers_f21_Parts, LM_Flashers_f21_UnderPF, LM_Flashers_f21_sw41, LM_Flashers_f22_BoatPlastic, LM_Flashers_f22_MetalRamps, LM_Flashers_f22_MiniPF, LM_Flashers_f22_Parts, LM_Flashers_f22_UnderPF, LM_Flashers_f22_sw41, LM_Flashers_f23_BoatPlastic, LM_Flashers_f23_MetalRamps, LM_Flashers_f23_MiniPF, LM_Flashers_f23_Parts, LM_Flashers_f23_UnderPF, LM_Flashers_f25_BoatPlastic, LM_Flashers_f25_Catapult, LM_Flashers_f25_ClearPlastics, LM_Flashers_f25_FishingRod, LM_Flashers_f25_MetalRamps, LM_Flashers_f25_MiniPF, LM_Flashers_f25_Parts, LM_Flashers_f25_Playfield, LM_Flashers_f25_RampCovers, LM_Flashers_f25_RampEdges, LM_Flashers_f25_Reel, LM_Flashers_f25_ReelStickerLM, LM_Flashers_f25_Sideblades, LM_Flashers_f25_StretchTruth, LM_Flashers_f25_UnderPF, LM_Flashers_f25_sw27, LM_Flashers_f25_sw28, LM_Flashers_f25_sw34, LM_Flashers_f25_sw41, LM_Flashers_f26_BoatPlastic, LM_Flashers_f26_Bumper_Socket, LM_Flashers_f26_Bumper_Socket_0, LM_Flashers_f26_Bumper_Socket_0, LM_Flashers_f26_Bumper1Ring, _
	LM_Flashers_f26_Bumper2Ring, LM_Flashers_f26_Bumper3Ring, LM_Flashers_f26_BumperEdges, LM_Flashers_f26_Bumpers, LM_Flashers_f26_ClearPlastics, LM_Flashers_f26_FishingRod, LM_Flashers_f26_Gate, LM_Flashers_f26_MetalRamps, LM_Flashers_f26_MiniPF, LM_Flashers_f26_Parts, LM_Flashers_f26_Playfield, LM_Flashers_f26_RailsLockdown, LM_Flashers_f26_Ramp, LM_Flashers_f26_RampCovers, LM_Flashers_f26_RampCoversAlt, LM_Flashers_f26_RampEdges, LM_Flashers_f26_Reel, LM_Flashers_f26_ReelStickerLM, LM_Flashers_f26_Sideblades, LM_Flashers_f26_StretchTruth, LM_Flashers_f26_UnderPF, LM_Flashers_f26_sw32, LM_Flashers_f26_sw33, LM_Flashers_f26_sw34, LM_Flashers_f26_sw41, LM_Flashers_f26_sw42a, LM_Flashers_f26_sw43a, LM_Flashers_f26_sw44, LM_Flashers_f26_sw45, LM_Flashers_f26_sw46, LM_Flashers_f26_sw62, LM_Flashers_f26_sw64, LM_Flashers_f27_Bumper_Socket_0, LM_Flashers_f27_Bumper_Socket_0, LM_Flashers_f27_Bumper2Ring, LM_Flashers_f27_Bumper3Ring, LM_Flashers_f27_BumperEdges, LM_Flashers_f27_MetalRamps, LM_Flashers_f27_Parts, _
	LM_Flashers_f27_Playfield, LM_Flashers_f27_RampCovers, LM_Flashers_f27_RampCoversAlt, LM_Flashers_f27_RampEdges, LM_Flashers_f27_Sideblades, LM_Flashers_f27_UnderPF, LM_Flashers_f27_sw48, LM_Flashers_f27_sw61, LM_Flashers_f27_sw62, LM_GIBottom_FlipperLDown, LM_GIBottom_FlipperLUp, LM_GIBottom_FlipperRDown, LM_GIBottom_FlipperRUp, LM_GIBottom_LeftPost, LM_GIBottom_LeftSling1, LM_GIBottom_LeftSling2, LM_GIBottom_LeftSling3, LM_GIBottom_LeftSling4, LM_GIBottom_LeftSlingArm, LM_GIBottom_MetalRamps, LM_GIBottom_Parts, LM_GIBottom_Playfield, LM_GIBottom_RailsLockdown, LM_GIBottom_ReelStickerLM, LM_GIBottom_RightPost, LM_GIBottom_RightSling1, LM_GIBottom_RightSling2, LM_GIBottom_RightSling3, LM_GIBottom_RightSling4, LM_GIBottom_RightSlingArm, LM_GIBottom_Sideblades, LM_GIBottom_UnderPF, LM_GIBottom_sw25, LM_GIBottom_sw26, LM_GIBottom_sw27, LM_GIBottom_sw28, LM_GIBottom_sw54, LM_GIBottom_sw55, LM_GIBottom_sw65, LM_GIBottom_sw66, LM_GISplit_gi002_FlipperLDown, LM_GISplit_gi002_FlipperLUp, _
	LM_GISplit_gi002_FlipperRDown, LM_GISplit_gi002_FlipperRUp, LM_GISplit_gi002_LeftPost, LM_GISplit_gi002_LeftSling1, LM_GISplit_gi002_LeftSling2, LM_GISplit_gi002_LeftSling3, LM_GISplit_gi002_LeftSling4, LM_GISplit_gi002_LeftSlingArm, LM_GISplit_gi002_MetalRamps, LM_GISplit_gi002_Parts, LM_GISplit_gi002_Playfield, LM_GISplit_gi002_RailsLockdown, LM_GISplit_gi002_RightSling1, LM_GISplit_gi002_RightSling2, LM_GISplit_gi002_RightSling3, LM_GISplit_gi002_RightSling4, LM_GISplit_gi002_UnderPF, LM_GISplit_gi002_sw25, LM_GISplit_gi002_sw26, LM_GISplit_gi002_sw27, LM_GISplit_gi006_FlipperLDown, LM_GISplit_gi006_FlipperLUp, LM_GISplit_gi006_FlipperRDown, LM_GISplit_gi006_FlipperRUp, LM_GISplit_gi006_LeftSling1, LM_GISplit_gi006_LeftSling2, LM_GISplit_gi006_LeftSling3, LM_GISplit_gi006_LeftSling4, LM_GISplit_gi006_MetalRamps, LM_GISplit_gi006_Parts, LM_GISplit_gi006_Playfield, LM_GISplit_gi006_RailsLockdown, LM_GISplit_gi006_RightPost, LM_GISplit_gi006_RightSling1, LM_GISplit_gi006_RightSling2, _
	LM_GISplit_gi006_RightSling3, LM_GISplit_gi006_RightSling4, LM_GISplit_gi006_RightSlingArm, LM_GISplit_gi006_UnderPF, LM_GISplit_gi006_sw65, LM_GISplit_gi006_sw66, LM_GISplit_gi009_Catapult, LM_GISplit_gi009_FishingRod, LM_GISplit_gi009_LeftPost, LM_GISplit_gi009_MetalRamps, LM_GISplit_gi009_Parts, LM_GISplit_gi009_Playfield, LM_GISplit_gi009_RailsLockdown, LM_GISplit_gi009_ReelStickerLM, LM_GISplit_gi009_Sideblades, LM_GISplit_gi009_UnderPF, LM_GISplit_gi009_sw27, LM_GISplit_gi009_sw28, LM_GISplit_gi011_MetalRamps, LM_GISplit_gi011_Parts, LM_GISplit_gi011_Playfield, LM_GISplit_gi011_RightPost, LM_GISplit_gi011_RightSling1, LM_GISplit_gi011_RightSling2, LM_GISplit_gi011_RightSling3, LM_GISplit_gi011_RightSling4, LM_GISplit_gi011_UnderPF, LM_GISplit_gi011_sw54, LM_GISplit_gi011_sw55, LM_GISplit_gitop020_Bumper_Sock, LM_GISplit_gitop020_Bumper_Sock, LM_GISplit_gitop020_Bumper_Sock, LM_GISplit_gitop020_Bumper2Ring, LM_GISplit_gitop020_Bumper3Ring, LM_GISplit_gitop020_BumperEdges, LM_GISplit_gitop020_ClearPlasti, _
	LM_GISplit_gitop020_FishingRod, LM_GISplit_gitop020_MetalRamps, LM_GISplit_gitop020_Parts, LM_GISplit_gitop020_Playfield, LM_GISplit_gitop020_Ramp, LM_GISplit_gitop020_RampCovers, LM_GISplit_gitop020_RampCoversA, LM_GISplit_gitop020_Sideblades, LM_GISplit_gitop020_UnderPF, LM_GISplit_gitop020_sw44, LM_GISplit_gitop020_sw45, LM_GISplit_gitop020_sw62, LM_GITop_BoatPlastic, LM_GITop_Bumper_Socket, LM_GITop_Bumper_Socket_001, LM_GITop_Bumper_Socket_002, LM_GITop_Bumper1Ring, LM_GITop_Bumper2Ring, LM_GITop_Bumper3Ring, LM_GITop_BumperEdges, LM_GITop_Bumpers, LM_GITop_Catapult, LM_GITop_ClearPlastics, LM_GITop_FishingRod, LM_GITop_Gate, LM_GITop_MetalRamps, LM_GITop_MiniPF, LM_GITop_Parts, LM_GITop_Playfield, LM_GITop_RailsLockdown, LM_GITop_Ramp, LM_GITop_RampCovers, LM_GITop_RampCoversAlt, LM_GITop_RampEdges, LM_GITop_Reel, LM_GITop_ReelStickerLM, LM_GITop_Sideblades, LM_GITop_StretchTruth, LM_GITop_UnderPF, LM_GITop_sw27, LM_GITop_sw28, LM_GITop_sw32, LM_GITop_sw34, LM_GITop_sw41, LM_GITop_sw42a, LM_GITop_sw43a, _
	LM_GITop_sw44, LM_GITop_sw45, LM_GITop_sw46, LM_GITop_sw48, LM_GITop_sw61, LM_GITop_sw62, LM_GITop_sw64, LM_Inserts_l11_BoatPlastic, LM_Inserts_l11_MetalRamps, LM_Inserts_l11_MiniPF, LM_Inserts_l11_Parts, LM_Inserts_l11_UnderPF, LM_Inserts_l12_BoatPlastic, LM_Inserts_l12_MetalRamps, LM_Inserts_l12_MiniPF, LM_Inserts_l12_Parts, LM_Inserts_l12_UnderPF, LM_Inserts_l13_BoatPlastic, LM_Inserts_l13_MetalRamps, LM_Inserts_l13_MiniPF, LM_Inserts_l13_Parts, LM_Inserts_l13_UnderPF, LM_Inserts_l14_BoatPlastic, LM_Inserts_l14_MetalRamps, LM_Inserts_l14_MiniPF, LM_Inserts_l14_Parts, LM_Inserts_l14_UnderPF, LM_Inserts_l14_sw41, LM_Inserts_l15_MetalRamps, LM_Inserts_l15_MiniPF, LM_Inserts_l15_Parts, LM_Inserts_l15_UnderPF, LM_Inserts_l15_sw41, LM_Inserts_l16_Bumper_Socket, LM_Inserts_l16_Bumper1Ring, LM_Inserts_l16_MetalRamps, LM_Inserts_l16_Parts, LM_Inserts_l16_Playfield, LM_Inserts_l16_Ramp, LM_Inserts_l16_RampCovers, LM_Inserts_l16_RampCoversAlt, LM_Inserts_l16_RampEdges, LM_Inserts_l16_UnderPF, LM_Inserts_l16_sw46, _
	LM_Inserts_l17_MetalRamps, LM_Inserts_l17_Parts, LM_Inserts_l17_Playfield, LM_Inserts_l17_Ramp, LM_Inserts_l17_RampCovers, LM_Inserts_l17_RampCoversAlt, LM_Inserts_l17_RampEdges, LM_Inserts_l17_UnderPF, LM_Inserts_l17_sw45, LM_Inserts_l18_BumperEdges, LM_Inserts_l18_MetalRamps, LM_Inserts_l18_Parts, LM_Inserts_l18_RampEdges, LM_Inserts_l18_UnderPF, LM_Inserts_l18_sw44, LM_Inserts_l21_LeftSling1, LM_Inserts_l21_LeftSling2, LM_Inserts_l21_LeftSling3, LM_Inserts_l21_LeftSling4, LM_Inserts_l21_MetalRamps, LM_Inserts_l21_Parts, LM_Inserts_l21_Playfield, LM_Inserts_l21_UnderPF, LM_Inserts_l22_MetalRamps, LM_Inserts_l22_Playfield, LM_Inserts_l22_UnderPF, LM_Inserts_l23_MetalRamps, LM_Inserts_l23_Playfield, LM_Inserts_l23_UnderPF, LM_Inserts_l24_MetalRamps, LM_Inserts_l24_Parts, LM_Inserts_l24_Playfield, LM_Inserts_l24_RightSling1, LM_Inserts_l24_RightSling2, LM_Inserts_l24_RightSling3, LM_Inserts_l24_RightSling4, LM_Inserts_l24_UnderPF, LM_Inserts_l25_MetalRamps, LM_Inserts_l25_Parts, LM_Inserts_l25_Playfield, _
	LM_Inserts_l25_UnderPF, LM_Inserts_l26_FishingRod, LM_Inserts_l26_MetalRamps, LM_Inserts_l26_Parts, LM_Inserts_l26_Playfield, LM_Inserts_l26_UnderPF, LM_Inserts_l27_FishingRod, LM_Inserts_l27_MetalRamps, LM_Inserts_l27_Parts, LM_Inserts_l27_Playfield, LM_Inserts_l27_UnderPF, LM_Inserts_l28_FishingRod, LM_Inserts_l28_MetalRamps, LM_Inserts_l28_Parts, LM_Inserts_l28_Playfield, LM_Inserts_l28_RampEdges, LM_Inserts_l28_UnderPF, LM_Inserts_l28_sw34, LM_Inserts_l31_MetalRamps, LM_Inserts_l31_Playfield, LM_Inserts_l31_UnderPF, LM_Inserts_l32_Playfield, LM_Inserts_l32_UnderPF, LM_Inserts_l33_Playfield, LM_Inserts_l33_UnderPF, LM_Inserts_l34_MetalRamps, LM_Inserts_l34_Parts, LM_Inserts_l34_Playfield, LM_Inserts_l34_UnderPF, LM_Inserts_l35_BoatPlastic, LM_Inserts_l35_MetalRamps, LM_Inserts_l35_MiniPF, LM_Inserts_l35_Parts, LM_Inserts_l35_UnderPF, LM_Inserts_l36_BoatPlastic, LM_Inserts_l36_MetalRamps, LM_Inserts_l36_MiniPF, LM_Inserts_l36_Parts, LM_Inserts_l36_UnderPF, LM_Inserts_l37_BoatPlastic, _
	LM_Inserts_l37_MetalRamps, LM_Inserts_l37_MiniPF, LM_Inserts_l37_Parts, LM_Inserts_l37_UnderPF, LM_Inserts_l38_BoatPlastic, LM_Inserts_l38_MetalRamps, LM_Inserts_l38_MiniPF, LM_Inserts_l38_Parts, LM_Inserts_l38_UnderPF, LM_Inserts_l41_Playfield, LM_Inserts_l41_UnderPF, LM_Inserts_l42_Playfield, LM_Inserts_l42_UnderPF, LM_Inserts_l43_FlipperLDown, LM_Inserts_l43_FlipperLUp, LM_Inserts_l43_FlipperRDown, LM_Inserts_l43_FlipperRUp, LM_Inserts_l43_Parts, LM_Inserts_l43_Playfield, LM_Inserts_l43_UnderPF, LM_Inserts_l44_Playfield, LM_Inserts_l44_UnderPF, LM_Inserts_l45_MetalRamps, LM_Inserts_l45_Playfield, LM_Inserts_l45_UnderPF, LM_Inserts_l45_sw28, LM_Inserts_l46_MetalRamps, LM_Inserts_l46_Parts, LM_Inserts_l46_Playfield, LM_Inserts_l46_UnderPF, LM_Inserts_l46_sw27, LM_Inserts_l46_sw28, LM_Inserts_l47_MetalRamps, LM_Inserts_l47_Parts, LM_Inserts_l47_Playfield, LM_Inserts_l47_UnderPF, LM_Inserts_l47_sw27, LM_Inserts_l48_LeftPost, LM_Inserts_l48_MetalRamps, LM_Inserts_l48_Parts, LM_Inserts_l48_Playfield, _
	LM_Inserts_l48_Sideblades, LM_Inserts_l48_UnderPF, LM_Inserts_l48a_MetalRamps, LM_Inserts_l48a_Parts, LM_Inserts_l48a_Playfield, LM_Inserts_l48a_RightPost, LM_Inserts_l48a_UnderPF, LM_Inserts_l51_FlipperLDown, LM_Inserts_l51_FlipperLUp, LM_Inserts_l51_Parts, LM_Inserts_l51_Playfield, LM_Inserts_l51_UnderPF, LM_Inserts_l52_FlipperLDown, LM_Inserts_l52_FlipperLUp, LM_Inserts_l52_FlipperRDown, LM_Inserts_l52_FlipperRUp, LM_Inserts_l52_Parts, LM_Inserts_l52_Playfield, LM_Inserts_l52_UnderPF, LM_Inserts_l53_Playfield, LM_Inserts_l53_UnderPF, LM_Inserts_l54_FlipperRDown, LM_Inserts_l54_FlipperRUp, LM_Inserts_l54_Parts, LM_Inserts_l54_Playfield, LM_Inserts_l54_UnderPF, LM_Inserts_l55_MetalRamps, LM_Inserts_l55_Playfield, LM_Inserts_l55_UnderPF, LM_Inserts_l55_sw55, LM_Inserts_l56_MetalRamps, LM_Inserts_l56_Playfield, LM_Inserts_l56_UnderPF, LM_Inserts_l57_MetalRamps, LM_Inserts_l57_Playfield, LM_Inserts_l57_UnderPF, LM_Inserts_l58_LeftPost, LM_Inserts_l58_LeftSling1, LM_Inserts_l58_LeftSling2, _
	LM_Inserts_l58_LeftSling3, LM_Inserts_l58_LeftSling4, LM_Inserts_l58_MetalRamps, LM_Inserts_l58_Parts, LM_Inserts_l58_Playfield, LM_Inserts_l58_UnderPF, LM_Inserts_l61_MetalRamps, LM_Inserts_l61_Parts, LM_Inserts_l61_Playfield, LM_Inserts_l61_UnderPF, LM_Inserts_l62_MetalRamps, LM_Inserts_l62_Parts, LM_Inserts_l62_Playfield, LM_Inserts_l62_UnderPF, LM_Inserts_l63_MetalRamps, LM_Inserts_l63_Parts, LM_Inserts_l63_Playfield, LM_Inserts_l63_UnderPF, LM_Inserts_l64_MetalRamps, LM_Inserts_l64_Playfield, LM_Inserts_l64_UnderPF, LM_Inserts_l65_MetalRamps, LM_Inserts_l65_Playfield, LM_Inserts_l65_UnderPF, LM_Inserts_l66_MetalRamps, LM_Inserts_l66_Playfield, LM_Inserts_l66_UnderPF, LM_Inserts_l67_MetalRamps, LM_Inserts_l67_Parts, LM_Inserts_l67_Playfield, LM_Inserts_l67_UnderPF, LM_Inserts_l68_MetalRamps, LM_Inserts_l68_Parts, LM_Inserts_l68_Playfield, LM_Inserts_l68_RightPost, LM_Inserts_l68_RightSling1, LM_Inserts_l68_RightSling2, LM_Inserts_l68_RightSling3, LM_Inserts_l68_RightSling4, LM_Inserts_l68_UnderPF, _
	LM_Inserts_l71_MetalRamps, LM_Inserts_l71_Parts, LM_Inserts_l71_Playfield, LM_Inserts_l71_sw48, LM_Inserts_l71_sw61, LM_Inserts_l72_MetalRamps, LM_Inserts_l72_Parts, LM_Inserts_l72_Playfield, LM_Inserts_l72_UnderPF, LM_Inserts_l73_MetalRamps, LM_Inserts_l73_Parts, LM_Inserts_l73_Playfield, LM_Inserts_l73_UnderPF, LM_Inserts_l74_MetalRamps, LM_Inserts_l74_Parts, LM_Inserts_l74_Playfield, LM_Inserts_l74_UnderPF, LM_Inserts_l75_MetalRamps, LM_Inserts_l75_Parts, LM_Inserts_l75_Playfield, LM_Inserts_l75_UnderPF, LM_Inserts_l75_sw44, LM_Inserts_l75_sw48, LM_Inserts_l75_sw61, LM_Inserts_l76_MetalRamps, LM_Inserts_l76_Parts, LM_Inserts_l76_Playfield, LM_Inserts_l76_UnderPF, LM_Inserts_l77_MetalRamps, LM_Inserts_l77_Parts, LM_Inserts_l77_Playfield, LM_Inserts_l77_Sideblades, LM_Inserts_l77_UnderPF, LM_Inserts_l78_MetalRamps, LM_Inserts_l78_Parts, LM_Inserts_l78_Playfield, LM_Inserts_l78_UnderPF, LM_Inserts_l78_sw61, LM_Inserts_l81_FishingRod, LM_Inserts_l81_MetalRamps, LM_Inserts_l81_Parts, LM_Inserts_l81_Playfield, _
	LM_Inserts_l81_RailsLockdown, LM_Inserts_l81_RampCovers, LM_Inserts_l81_RampCoversAlt, LM_Inserts_l81_RampEdges, LM_Inserts_l81_ReelStickerLM, LM_Inserts_l81_StretchTruth, LM_Inserts_l81_UnderPF, LM_Inserts_l81_sw34, LM_Inserts_l82_FishingRod, LM_Inserts_l82_MetalRamps, LM_Inserts_l82_Parts, LM_Inserts_l82_Playfield, LM_Inserts_l82_RailsLockdown, LM_Inserts_l82_RampCovers, LM_Inserts_l82_RampCoversAlt, LM_Inserts_l82_RampEdges, LM_Inserts_l82_ReelStickerLM, LM_Inserts_l82_StretchTruth, LM_Inserts_l82_UnderPF, LM_Inserts_l82_sw34, LM_Inserts_l83_FishingRod, LM_Inserts_l83_MetalRamps, LM_Inserts_l83_Parts, LM_Inserts_l83_Playfield, LM_Inserts_l83_RampCovers, LM_Inserts_l83_RampCoversAlt, LM_Inserts_l83_RampEdges, LM_Inserts_l83_ReelStickerLM, LM_Inserts_l83_StretchTruth, LM_Inserts_l83_UnderPF, LM_Inserts_l83_sw34, LM_Inserts_l84_FishingRod, LM_Inserts_l84_MetalRamps, LM_Inserts_l84_Parts, LM_Inserts_l84_Playfield, LM_Inserts_l84_RampCovers, LM_Inserts_l84_RampCoversAlt, LM_Inserts_l84_ReelStickerLM, _
	LM_Inserts_l84_Sideblades, LM_Inserts_l84_StretchTruth, LM_Inserts_l84_UnderPF, LM_Inserts_l84_sw34, LM_Inserts_l85_FishingRod, LM_Inserts_l85_MetalRamps, LM_Inserts_l85_Parts, LM_Inserts_l85_Playfield, LM_Inserts_l85_RampCovers, LM_Inserts_l85_RampCoversAlt, LM_Inserts_l85_RampEdges, LM_Inserts_l85_ReelStickerLM, LM_Inserts_l85_Sideblades, LM_Inserts_l85_StretchTruth, LM_Inserts_l85_UnderPF, LM_Inserts_l85_sw34, LM_Inserts_l86_MetalRamps, LM_Inserts_l86_Parts)
Dim BG_All: BG_All=Array(BM_BoatPlastic, BM_Bumper_Socket, BM_Bumper_Socket_001, BM_Bumper_Socket_002, BM_Bumper1Ring, BM_Bumper2Ring, BM_Bumper3Ring, BM_BumperEdges, BM_Bumpers, BM_Catapult, BM_ClearPlastics, BM_FishingRod, BM_FlipperLDown, BM_FlipperLUp, BM_FlipperRDown, BM_FlipperRUp, BM_Gate, BM_LeftPost, BM_LeftSling1, BM_LeftSling2, BM_LeftSling3, BM_LeftSling4, BM_LeftSlingArm, BM_MetalRamps, BM_MiniPF, BM_Parts, BM_Playfield, BM_RailsLockdown, BM_Ramp, BM_RampCovers, BM_RampCoversAlt, BM_RampEdges, BM_Reel, BM_ReelStickerLM, BM_RightPost, BM_RightSling1, BM_RightSling2, BM_RightSling3, BM_RightSling4, BM_RightSlingArm, BM_Sideblades, BM_StretchTruth, BM_UnderPF, BM_sw25, BM_sw26, BM_sw27, BM_sw28, BM_sw32, BM_sw33, BM_sw34, BM_sw41, BM_sw42a, BM_sw43a, BM_sw44, BM_sw45, BM_sw46, BM_sw48, BM_sw54, BM_sw55, BM_sw56, BM_sw61, BM_sw62, BM_sw64, BM_sw65, BM_sw66, LM_Flashers_f17_ClearPlastics, LM_Flashers_f17_FishingRod, LM_Flashers_f17_MetalRamps, LM_Flashers_f17_Parts, LM_Flashers_f17_Playfield, _
	LM_Flashers_f17_RampCoversAlt, LM_Flashers_f17_RampEdges, LM_Flashers_f17_ReelStickerLM, LM_Flashers_f17_StretchTruth, LM_Flashers_f17_UnderPF, LM_Flashers_f17_sw34, LM_Flashers_f18_MetalRamps, LM_Flashers_f18_Parts, LM_Flashers_f18_Playfield, LM_Flashers_f18_Reel, LM_Flashers_f18_Sideblades, LM_Flashers_f18_UnderPF, LM_Flashers_f18_sw27, LM_Flashers_f18_sw28, LM_Flashers_f18_sw54, LM_Flashers_f18_sw55, LM_Flashers_f19_BoatPlastic, LM_Flashers_f19_MetalRamps, LM_Flashers_f19_MiniPF, LM_Flashers_f19_Parts, LM_Flashers_f19_Playfield, LM_Flashers_f19_StretchTruth, LM_Flashers_f19_UnderPF, LM_Flashers_f19_sw41, LM_Flashers_f20_BoatPlastic, LM_Flashers_f20_MetalRamps, LM_Flashers_f20_MiniPF, LM_Flashers_f20_Parts, LM_Flashers_f20_UnderPF, LM_Flashers_f20_sw41, LM_Flashers_f21_BoatPlastic, LM_Flashers_f21_MetalRamps, LM_Flashers_f21_MiniPF, LM_Flashers_f21_Parts, LM_Flashers_f21_UnderPF, LM_Flashers_f21_sw41, LM_Flashers_f22_BoatPlastic, LM_Flashers_f22_MetalRamps, LM_Flashers_f22_MiniPF, LM_Flashers_f22_Parts, _
	LM_Flashers_f22_UnderPF, LM_Flashers_f22_sw41, LM_Flashers_f23_BoatPlastic, LM_Flashers_f23_MetalRamps, LM_Flashers_f23_MiniPF, LM_Flashers_f23_Parts, LM_Flashers_f23_UnderPF, LM_Flashers_f25_BoatPlastic, LM_Flashers_f25_Catapult, LM_Flashers_f25_ClearPlastics, LM_Flashers_f25_FishingRod, LM_Flashers_f25_MetalRamps, LM_Flashers_f25_MiniPF, LM_Flashers_f25_Parts, LM_Flashers_f25_Playfield, LM_Flashers_f25_RampCovers, LM_Flashers_f25_RampEdges, LM_Flashers_f25_Reel, LM_Flashers_f25_ReelStickerLM, LM_Flashers_f25_Sideblades, LM_Flashers_f25_StretchTruth, LM_Flashers_f25_UnderPF, LM_Flashers_f25_sw27, LM_Flashers_f25_sw28, LM_Flashers_f25_sw34, LM_Flashers_f25_sw41, LM_Flashers_f26_BoatPlastic, LM_Flashers_f26_Bumper_Socket, LM_Flashers_f26_Bumper_Socket_0, LM_Flashers_f26_Bumper_Socket_0, LM_Flashers_f26_Bumper1Ring, LM_Flashers_f26_Bumper2Ring, LM_Flashers_f26_Bumper3Ring, LM_Flashers_f26_BumperEdges, LM_Flashers_f26_Bumpers, LM_Flashers_f26_ClearPlastics, LM_Flashers_f26_FishingRod, LM_Flashers_f26_Gate, _
	LM_Flashers_f26_MetalRamps, LM_Flashers_f26_MiniPF, LM_Flashers_f26_Parts, LM_Flashers_f26_Playfield, LM_Flashers_f26_RailsLockdown, LM_Flashers_f26_Ramp, LM_Flashers_f26_RampCovers, LM_Flashers_f26_RampCoversAlt, LM_Flashers_f26_RampEdges, LM_Flashers_f26_Reel, LM_Flashers_f26_ReelStickerLM, LM_Flashers_f26_Sideblades, LM_Flashers_f26_StretchTruth, LM_Flashers_f26_UnderPF, LM_Flashers_f26_sw32, LM_Flashers_f26_sw33, LM_Flashers_f26_sw34, LM_Flashers_f26_sw41, LM_Flashers_f26_sw42a, LM_Flashers_f26_sw43a, LM_Flashers_f26_sw44, LM_Flashers_f26_sw45, LM_Flashers_f26_sw46, LM_Flashers_f26_sw62, LM_Flashers_f26_sw64, LM_Flashers_f27_Bumper_Socket_0, LM_Flashers_f27_Bumper_Socket_0, LM_Flashers_f27_Bumper2Ring, LM_Flashers_f27_Bumper3Ring, LM_Flashers_f27_BumperEdges, LM_Flashers_f27_MetalRamps, LM_Flashers_f27_Parts, LM_Flashers_f27_Playfield, LM_Flashers_f27_RampCovers, LM_Flashers_f27_RampCoversAlt, LM_Flashers_f27_RampEdges, LM_Flashers_f27_Sideblades, LM_Flashers_f27_UnderPF, LM_Flashers_f27_sw48, _
	LM_Flashers_f27_sw61, LM_Flashers_f27_sw62, LM_GIBottom_FlipperLDown, LM_GIBottom_FlipperLUp, LM_GIBottom_FlipperRDown, LM_GIBottom_FlipperRUp, LM_GIBottom_LeftPost, LM_GIBottom_LeftSling1, LM_GIBottom_LeftSling2, LM_GIBottom_LeftSling3, LM_GIBottom_LeftSling4, LM_GIBottom_LeftSlingArm, LM_GIBottom_MetalRamps, LM_GIBottom_Parts, LM_GIBottom_Playfield, LM_GIBottom_RailsLockdown, LM_GIBottom_ReelStickerLM, LM_GIBottom_RightPost, LM_GIBottom_RightSling1, LM_GIBottom_RightSling2, LM_GIBottom_RightSling3, LM_GIBottom_RightSling4, LM_GIBottom_RightSlingArm, LM_GIBottom_Sideblades, LM_GIBottom_UnderPF, LM_GIBottom_sw25, LM_GIBottom_sw26, LM_GIBottom_sw27, LM_GIBottom_sw28, LM_GIBottom_sw54, LM_GIBottom_sw55, LM_GIBottom_sw65, LM_GIBottom_sw66, LM_GISplit_gi002_FlipperLDown, LM_GISplit_gi002_FlipperLUp, LM_GISplit_gi002_FlipperRDown, LM_GISplit_gi002_FlipperRUp, LM_GISplit_gi002_LeftPost, LM_GISplit_gi002_LeftSling1, LM_GISplit_gi002_LeftSling2, LM_GISplit_gi002_LeftSling3, LM_GISplit_gi002_LeftSling4, _
	LM_GISplit_gi002_LeftSlingArm, LM_GISplit_gi002_MetalRamps, LM_GISplit_gi002_Parts, LM_GISplit_gi002_Playfield, LM_GISplit_gi002_RailsLockdown, LM_GISplit_gi002_RightSling1, LM_GISplit_gi002_RightSling2, LM_GISplit_gi002_RightSling3, LM_GISplit_gi002_RightSling4, LM_GISplit_gi002_UnderPF, LM_GISplit_gi002_sw25, LM_GISplit_gi002_sw26, LM_GISplit_gi002_sw27, LM_GISplit_gi006_FlipperLDown, LM_GISplit_gi006_FlipperLUp, LM_GISplit_gi006_FlipperRDown, LM_GISplit_gi006_FlipperRUp, LM_GISplit_gi006_LeftSling1, LM_GISplit_gi006_LeftSling2, LM_GISplit_gi006_LeftSling3, LM_GISplit_gi006_LeftSling4, LM_GISplit_gi006_MetalRamps, LM_GISplit_gi006_Parts, LM_GISplit_gi006_Playfield, LM_GISplit_gi006_RailsLockdown, LM_GISplit_gi006_RightPost, LM_GISplit_gi006_RightSling1, LM_GISplit_gi006_RightSling2, LM_GISplit_gi006_RightSling3, LM_GISplit_gi006_RightSling4, LM_GISplit_gi006_RightSlingArm, LM_GISplit_gi006_UnderPF, LM_GISplit_gi006_sw65, LM_GISplit_gi006_sw66, LM_GISplit_gi009_Catapult, LM_GISplit_gi009_FishingRod, _
	LM_GISplit_gi009_LeftPost, LM_GISplit_gi009_MetalRamps, LM_GISplit_gi009_Parts, LM_GISplit_gi009_Playfield, LM_GISplit_gi009_RailsLockdown, LM_GISplit_gi009_ReelStickerLM, LM_GISplit_gi009_Sideblades, LM_GISplit_gi009_UnderPF, LM_GISplit_gi009_sw27, LM_GISplit_gi009_sw28, LM_GISplit_gi011_MetalRamps, LM_GISplit_gi011_Parts, LM_GISplit_gi011_Playfield, LM_GISplit_gi011_RightPost, LM_GISplit_gi011_RightSling1, LM_GISplit_gi011_RightSling2, LM_GISplit_gi011_RightSling3, LM_GISplit_gi011_RightSling4, LM_GISplit_gi011_UnderPF, LM_GISplit_gi011_sw54, LM_GISplit_gi011_sw55, LM_GISplit_gitop020_Bumper_Sock, LM_GISplit_gitop020_Bumper_Sock, LM_GISplit_gitop020_Bumper_Sock, LM_GISplit_gitop020_Bumper2Ring, LM_GISplit_gitop020_Bumper3Ring, LM_GISplit_gitop020_BumperEdges, LM_GISplit_gitop020_ClearPlasti, LM_GISplit_gitop020_FishingRod, LM_GISplit_gitop020_MetalRamps, LM_GISplit_gitop020_Parts, LM_GISplit_gitop020_Playfield, LM_GISplit_gitop020_Ramp, LM_GISplit_gitop020_RampCovers, LM_GISplit_gitop020_RampCoversA, _
	LM_GISplit_gitop020_Sideblades, LM_GISplit_gitop020_UnderPF, LM_GISplit_gitop020_sw44, LM_GISplit_gitop020_sw45, LM_GISplit_gitop020_sw62, LM_GITop_BoatPlastic, LM_GITop_Bumper_Socket, LM_GITop_Bumper_Socket_001, LM_GITop_Bumper_Socket_002, LM_GITop_Bumper1Ring, LM_GITop_Bumper2Ring, LM_GITop_Bumper3Ring, LM_GITop_BumperEdges, LM_GITop_Bumpers, LM_GITop_Catapult, LM_GITop_ClearPlastics, LM_GITop_FishingRod, LM_GITop_Gate, LM_GITop_MetalRamps, LM_GITop_MiniPF, LM_GITop_Parts, LM_GITop_Playfield, LM_GITop_RailsLockdown, LM_GITop_Ramp, LM_GITop_RampCovers, LM_GITop_RampCoversAlt, LM_GITop_RampEdges, LM_GITop_Reel, LM_GITop_ReelStickerLM, LM_GITop_Sideblades, LM_GITop_StretchTruth, LM_GITop_UnderPF, LM_GITop_sw27, LM_GITop_sw28, LM_GITop_sw32, LM_GITop_sw34, LM_GITop_sw41, LM_GITop_sw42a, LM_GITop_sw43a, LM_GITop_sw44, LM_GITop_sw45, LM_GITop_sw46, LM_GITop_sw48, LM_GITop_sw61, LM_GITop_sw62, LM_GITop_sw64, LM_Inserts_l11_BoatPlastic, LM_Inserts_l11_MetalRamps, LM_Inserts_l11_MiniPF, LM_Inserts_l11_Parts, _
	LM_Inserts_l11_UnderPF, LM_Inserts_l12_BoatPlastic, LM_Inserts_l12_MetalRamps, LM_Inserts_l12_MiniPF, LM_Inserts_l12_Parts, LM_Inserts_l12_UnderPF, LM_Inserts_l13_BoatPlastic, LM_Inserts_l13_MetalRamps, LM_Inserts_l13_MiniPF, LM_Inserts_l13_Parts, LM_Inserts_l13_UnderPF, LM_Inserts_l14_BoatPlastic, LM_Inserts_l14_MetalRamps, LM_Inserts_l14_MiniPF, LM_Inserts_l14_Parts, LM_Inserts_l14_UnderPF, LM_Inserts_l14_sw41, LM_Inserts_l15_MetalRamps, LM_Inserts_l15_MiniPF, LM_Inserts_l15_Parts, LM_Inserts_l15_UnderPF, LM_Inserts_l15_sw41, LM_Inserts_l16_Bumper_Socket, LM_Inserts_l16_Bumper1Ring, LM_Inserts_l16_MetalRamps, LM_Inserts_l16_Parts, LM_Inserts_l16_Playfield, LM_Inserts_l16_Ramp, LM_Inserts_l16_RampCovers, LM_Inserts_l16_RampCoversAlt, LM_Inserts_l16_RampEdges, LM_Inserts_l16_UnderPF, LM_Inserts_l16_sw46, LM_Inserts_l17_MetalRamps, LM_Inserts_l17_Parts, LM_Inserts_l17_Playfield, LM_Inserts_l17_Ramp, LM_Inserts_l17_RampCovers, LM_Inserts_l17_RampCoversAlt, LM_Inserts_l17_RampEdges, LM_Inserts_l17_UnderPF, _
	LM_Inserts_l17_sw45, LM_Inserts_l18_BumperEdges, LM_Inserts_l18_MetalRamps, LM_Inserts_l18_Parts, LM_Inserts_l18_RampEdges, LM_Inserts_l18_UnderPF, LM_Inserts_l18_sw44, LM_Inserts_l21_LeftSling1, LM_Inserts_l21_LeftSling2, LM_Inserts_l21_LeftSling3, LM_Inserts_l21_LeftSling4, LM_Inserts_l21_MetalRamps, LM_Inserts_l21_Parts, LM_Inserts_l21_Playfield, LM_Inserts_l21_UnderPF, LM_Inserts_l22_MetalRamps, LM_Inserts_l22_Playfield, LM_Inserts_l22_UnderPF, LM_Inserts_l23_MetalRamps, LM_Inserts_l23_Playfield, LM_Inserts_l23_UnderPF, LM_Inserts_l24_MetalRamps, LM_Inserts_l24_Parts, LM_Inserts_l24_Playfield, LM_Inserts_l24_RightSling1, LM_Inserts_l24_RightSling2, LM_Inserts_l24_RightSling3, LM_Inserts_l24_RightSling4, LM_Inserts_l24_UnderPF, LM_Inserts_l25_MetalRamps, LM_Inserts_l25_Parts, LM_Inserts_l25_Playfield, LM_Inserts_l25_UnderPF, LM_Inserts_l26_FishingRod, LM_Inserts_l26_MetalRamps, LM_Inserts_l26_Parts, LM_Inserts_l26_Playfield, LM_Inserts_l26_UnderPF, LM_Inserts_l27_FishingRod, LM_Inserts_l27_MetalRamps, _
	LM_Inserts_l27_Parts, LM_Inserts_l27_Playfield, LM_Inserts_l27_UnderPF, LM_Inserts_l28_FishingRod, LM_Inserts_l28_MetalRamps, LM_Inserts_l28_Parts, LM_Inserts_l28_Playfield, LM_Inserts_l28_RampEdges, LM_Inserts_l28_UnderPF, LM_Inserts_l28_sw34, LM_Inserts_l31_MetalRamps, LM_Inserts_l31_Playfield, LM_Inserts_l31_UnderPF, LM_Inserts_l32_Playfield, LM_Inserts_l32_UnderPF, LM_Inserts_l33_Playfield, LM_Inserts_l33_UnderPF, LM_Inserts_l34_MetalRamps, LM_Inserts_l34_Parts, LM_Inserts_l34_Playfield, LM_Inserts_l34_UnderPF, LM_Inserts_l35_BoatPlastic, LM_Inserts_l35_MetalRamps, LM_Inserts_l35_MiniPF, LM_Inserts_l35_Parts, LM_Inserts_l35_UnderPF, LM_Inserts_l36_BoatPlastic, LM_Inserts_l36_MetalRamps, LM_Inserts_l36_MiniPF, LM_Inserts_l36_Parts, LM_Inserts_l36_UnderPF, LM_Inserts_l37_BoatPlastic, LM_Inserts_l37_MetalRamps, LM_Inserts_l37_MiniPF, LM_Inserts_l37_Parts, LM_Inserts_l37_UnderPF, LM_Inserts_l38_BoatPlastic, LM_Inserts_l38_MetalRamps, LM_Inserts_l38_MiniPF, LM_Inserts_l38_Parts, LM_Inserts_l38_UnderPF, _
	LM_Inserts_l41_Playfield, LM_Inserts_l41_UnderPF, LM_Inserts_l42_Playfield, LM_Inserts_l42_UnderPF, LM_Inserts_l43_FlipperLDown, LM_Inserts_l43_FlipperLUp, LM_Inserts_l43_FlipperRDown, LM_Inserts_l43_FlipperRUp, LM_Inserts_l43_Parts, LM_Inserts_l43_Playfield, LM_Inserts_l43_UnderPF, LM_Inserts_l44_Playfield, LM_Inserts_l44_UnderPF, LM_Inserts_l45_MetalRamps, LM_Inserts_l45_Playfield, LM_Inserts_l45_UnderPF, LM_Inserts_l45_sw28, LM_Inserts_l46_MetalRamps, LM_Inserts_l46_Parts, LM_Inserts_l46_Playfield, LM_Inserts_l46_UnderPF, LM_Inserts_l46_sw27, LM_Inserts_l46_sw28, LM_Inserts_l47_MetalRamps, LM_Inserts_l47_Parts, LM_Inserts_l47_Playfield, LM_Inserts_l47_UnderPF, LM_Inserts_l47_sw27, LM_Inserts_l48_LeftPost, LM_Inserts_l48_MetalRamps, LM_Inserts_l48_Parts, LM_Inserts_l48_Playfield, LM_Inserts_l48_Sideblades, LM_Inserts_l48_UnderPF, LM_Inserts_l48a_MetalRamps, LM_Inserts_l48a_Parts, LM_Inserts_l48a_Playfield, LM_Inserts_l48a_RightPost, LM_Inserts_l48a_UnderPF, LM_Inserts_l51_FlipperLDown, _
	LM_Inserts_l51_FlipperLUp, LM_Inserts_l51_Parts, LM_Inserts_l51_Playfield, LM_Inserts_l51_UnderPF, LM_Inserts_l52_FlipperLDown, LM_Inserts_l52_FlipperLUp, LM_Inserts_l52_FlipperRDown, LM_Inserts_l52_FlipperRUp, LM_Inserts_l52_Parts, LM_Inserts_l52_Playfield, LM_Inserts_l52_UnderPF, LM_Inserts_l53_Playfield, LM_Inserts_l53_UnderPF, LM_Inserts_l54_FlipperRDown, LM_Inserts_l54_FlipperRUp, LM_Inserts_l54_Parts, LM_Inserts_l54_Playfield, LM_Inserts_l54_UnderPF, LM_Inserts_l55_MetalRamps, LM_Inserts_l55_Playfield, LM_Inserts_l55_UnderPF, LM_Inserts_l55_sw55, LM_Inserts_l56_MetalRamps, LM_Inserts_l56_Playfield, LM_Inserts_l56_UnderPF, LM_Inserts_l57_MetalRamps, LM_Inserts_l57_Playfield, LM_Inserts_l57_UnderPF, LM_Inserts_l58_LeftPost, LM_Inserts_l58_LeftSling1, LM_Inserts_l58_LeftSling2, LM_Inserts_l58_LeftSling3, LM_Inserts_l58_LeftSling4, LM_Inserts_l58_MetalRamps, LM_Inserts_l58_Parts, LM_Inserts_l58_Playfield, LM_Inserts_l58_UnderPF, LM_Inserts_l61_MetalRamps, LM_Inserts_l61_Parts, LM_Inserts_l61_Playfield, _
	LM_Inserts_l61_UnderPF, LM_Inserts_l62_MetalRamps, LM_Inserts_l62_Parts, LM_Inserts_l62_Playfield, LM_Inserts_l62_UnderPF, LM_Inserts_l63_MetalRamps, LM_Inserts_l63_Parts, LM_Inserts_l63_Playfield, LM_Inserts_l63_UnderPF, LM_Inserts_l64_MetalRamps, LM_Inserts_l64_Playfield, LM_Inserts_l64_UnderPF, LM_Inserts_l65_MetalRamps, LM_Inserts_l65_Playfield, LM_Inserts_l65_UnderPF, LM_Inserts_l66_MetalRamps, LM_Inserts_l66_Playfield, LM_Inserts_l66_UnderPF, LM_Inserts_l67_MetalRamps, LM_Inserts_l67_Parts, LM_Inserts_l67_Playfield, LM_Inserts_l67_UnderPF, LM_Inserts_l68_MetalRamps, LM_Inserts_l68_Parts, LM_Inserts_l68_Playfield, LM_Inserts_l68_RightPost, LM_Inserts_l68_RightSling1, LM_Inserts_l68_RightSling2, LM_Inserts_l68_RightSling3, LM_Inserts_l68_RightSling4, LM_Inserts_l68_UnderPF, LM_Inserts_l71_MetalRamps, LM_Inserts_l71_Parts, LM_Inserts_l71_Playfield, LM_Inserts_l71_sw48, LM_Inserts_l71_sw61, LM_Inserts_l72_MetalRamps, LM_Inserts_l72_Parts, LM_Inserts_l72_Playfield, LM_Inserts_l72_UnderPF, _
	LM_Inserts_l73_MetalRamps, LM_Inserts_l73_Parts, LM_Inserts_l73_Playfield, LM_Inserts_l73_UnderPF, LM_Inserts_l74_MetalRamps, LM_Inserts_l74_Parts, LM_Inserts_l74_Playfield, LM_Inserts_l74_UnderPF, LM_Inserts_l75_MetalRamps, LM_Inserts_l75_Parts, LM_Inserts_l75_Playfield, LM_Inserts_l75_UnderPF, LM_Inserts_l75_sw44, LM_Inserts_l75_sw48, LM_Inserts_l75_sw61, LM_Inserts_l76_MetalRamps, LM_Inserts_l76_Parts, LM_Inserts_l76_Playfield, LM_Inserts_l76_UnderPF, LM_Inserts_l77_MetalRamps, LM_Inserts_l77_Parts, LM_Inserts_l77_Playfield, LM_Inserts_l77_Sideblades, LM_Inserts_l77_UnderPF, LM_Inserts_l78_MetalRamps, LM_Inserts_l78_Parts, LM_Inserts_l78_Playfield, LM_Inserts_l78_UnderPF, LM_Inserts_l78_sw61, LM_Inserts_l81_FishingRod, LM_Inserts_l81_MetalRamps, LM_Inserts_l81_Parts, LM_Inserts_l81_Playfield, LM_Inserts_l81_RailsLockdown, LM_Inserts_l81_RampCovers, LM_Inserts_l81_RampCoversAlt, LM_Inserts_l81_RampEdges, LM_Inserts_l81_ReelStickerLM, LM_Inserts_l81_StretchTruth, LM_Inserts_l81_UnderPF, LM_Inserts_l81_sw34, _
	LM_Inserts_l82_FishingRod, LM_Inserts_l82_MetalRamps, LM_Inserts_l82_Parts, LM_Inserts_l82_Playfield, LM_Inserts_l82_RailsLockdown, LM_Inserts_l82_RampCovers, LM_Inserts_l82_RampCoversAlt, LM_Inserts_l82_RampEdges, LM_Inserts_l82_ReelStickerLM, LM_Inserts_l82_StretchTruth, LM_Inserts_l82_UnderPF, LM_Inserts_l82_sw34, LM_Inserts_l83_FishingRod, LM_Inserts_l83_MetalRamps, LM_Inserts_l83_Parts, LM_Inserts_l83_Playfield, LM_Inserts_l83_RampCovers, LM_Inserts_l83_RampCoversAlt, LM_Inserts_l83_RampEdges, LM_Inserts_l83_ReelStickerLM, LM_Inserts_l83_StretchTruth, LM_Inserts_l83_UnderPF, LM_Inserts_l83_sw34, LM_Inserts_l84_FishingRod, LM_Inserts_l84_MetalRamps, LM_Inserts_l84_Parts, LM_Inserts_l84_Playfield, LM_Inserts_l84_RampCovers, LM_Inserts_l84_RampCoversAlt, LM_Inserts_l84_ReelStickerLM, LM_Inserts_l84_Sideblades, LM_Inserts_l84_StretchTruth, LM_Inserts_l84_UnderPF, LM_Inserts_l84_sw34, LM_Inserts_l85_FishingRod, LM_Inserts_l85_MetalRamps, LM_Inserts_l85_Parts, LM_Inserts_l85_Playfield, _
	LM_Inserts_l85_RampCovers, LM_Inserts_l85_RampCoversAlt, LM_Inserts_l85_RampEdges, LM_Inserts_l85_ReelStickerLM, LM_Inserts_l85_Sideblades, LM_Inserts_l85_StretchTruth, LM_Inserts_l85_UnderPF, LM_Inserts_l85_sw34, LM_Inserts_l86_MetalRamps, LM_Inserts_l86_Parts)
' VLM  Arrays - End


'*******************************************
'	ZLOA: Load Stuff
'*******************************************

'*************************************************

'*********** Standard definitions ****************

Const UseSolenoids 	= 2
Const UseLamps 		= 1
Const UseSync 		= 0
Const UseGI 		= 1
Const HandleMech	= 0

'Standard Sounds
Const SSolenoidOn 		= "SOL_On"
Const SSolenoidOff 		= "SOL_Off"
Const SCoin = ""

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table (installed with the VPX package in the scripts folder)"
On Error Goto 0

LoadVPM "02700000", "WPC.VBS", 3.52

NoUpperLeftFlipper
NoUpperRightFlipper


'*******************************************
'	ZTIM: Main Timers
'*******************************************

Dim FrameTime, InitFrame: InitFrame = 0
Sub FrameTimer_Timer
	FrameTime = gametime - InitFrame : InitFrame = GameTime
	BSUpdate
	RollingUpdate
	DoSTAnim		'handle stand up target animations
	UpdateStandupTargets
	DoDTAnim		'handle drop up target animations
	UpdateDropTargets
	AnimateBumperSkirts
	NudgeAnim
	If ShowVR Then
		StartButtonLighting
		If VRRoom = 1 Then
			UpdateAnimations
		End If
	End If
End Sub

' FIXME for the time being, the cor timer interval must be 10 ms (so below 60FPS framerate)
CorTimer.Interval = 10
Sub CorTimer_Timer(): Cor.Update: End Sub


'*******************************************
'	ZINI: Table Initialization
'*******************************************

Sub FishTales_Init
	vpmInit me
	With Controller
		.GameName = cGameName
        .SplashInfoLine = "Fish Tales - Williams 1992"
		 If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
		.HandleKeyboard = 0
        .HandleMechanics = 0
        .Hidden = 0
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
		.Switch(22) = 1 'close coin door
		.Switch(24) = 1 'and keep it closed
	End With

	' Main Timer init
	PinMAMETimer.Interval = 2  ' OPT Rule8: UseVPMModSol=2 sets PinMAMEInterval=-2 (sub-frame); force 2ms positive
	PinMAMETimer.Enabled = True

	vpmMapLights AllLamps

	'Nudging
	vpmNudge.TiltSwitch = 14
	vpmNudge.Sensitivity = 3
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingShot, RightSlingShot)


	'Ball initializations need for physical trough
	Set FTBall1 = sw16.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set FTBall2 = sw17.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set FTBall3 = sw18.CreateSizedballWithMass(Ballsize/2,Ballmass)

	Controller.Switch(16) = 1
	Controller.Switch(17) = 1
	Controller.Switch(18) = 1

	'*** Use gBOT in the script wherever BOT is normally used. Then there is no need for GetBalls calls ***
	gBOT = Array(FTBall1, FTBall2, FTBall3)

	'Boat Captive Ball
	Set FTCapBall = CapKicker.createsizedballwithmass(ballsize/2, ballmass)
	CapKicker.enabled=0
	CapKicker.kick 0,1

	RotateReel 'Set Reel Visuals
	InitSwitchPrims 'Set Switch Visuals

	'Initilize Visuals
	SolFF 0
	LeftSlingShot.timerenabled = True
	RightSlingShot.timerenabled = True
	UpdateGI2 2, 0
	UpdateGI2 4, 0
End Sub


'*******************************************
'  ZOPT: User Options
'*******************************************

Dim LightLevel : LightLevel = 0.25				' Level of room lighting (0 to 1), where 0 is dark and 100 is brightest
Dim BallLightLevel : BallLightLevel = 0			' Level of ball brightness overide (0 to 1), where 0 is no over-ride and and 100 is brightest
Dim LUTSel : LutSel = 0							' 0 - none (default), 1 - HF's, 2 - VPW's
Dim VolumeDial : VolumeDial = 0.8           	'Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume : BallRollVolume = 0.5   	'Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.5 		'Level of ramp rolling volume. Value between 0 and 1
Dim OutlaneDifficulty : OutlaneDifficulty = 1	'Easy - 0, Medium - 1 (default), Hard - 2 
Dim FlipperTricksOpt : FlipperTricksOpt = 1		'Easy (Keyboard & Gamepad) - 0, Standard (Optimal for leaf switches) - 1 (default)
Dim RenderProbeOpt : RenderProbeOpt = 2			'0 - No Refraction (best performance), 1 - Roughness 0 (improved performance), 2 - Ramp Only Full (balanced), 3 - Full Refraction (best visuals)
Dim RampDecals : RampDecals = 1					'No Ramp Decals, 1 - Ramp Decals (default)
Dim ReflOpt : ReflOpt = 1						'0 = no reflections, 1 - static reflections (bake maps only), 2 - dynamic reflections (bake maps and light maps) 
Dim SBZScale : SBZScale = 1						'Z scale of the side blades between 1 and 2 applied in cabinet mode only
Dim VPMDMDVisible: VPMDMDVisible = 1 			' 0 - Not Visible, 1 - Visible,

'VR Room
Dim VRRoom : VRRoom = 1				   			'0 - Ultra Minimal Room, 1 - Fishing Scene (default)
Dim Topper : Topper = 1							'0 - no VR Topper,  1 - VR Topper is on (default)
Dim AmbientSound : AmbientSound = 2				'0 - no sound, 1 - wave sounds, 2 - wave and generator sounds (default)
Dim VRDesktop: VRDesktop = 0					'0 - disable VR Room in desktop mode (default) 1 - enable VR Room in desktop mode

' Called when options are tweaked by the player. 
' - 0: game has started, good time to load options and adjust accordingly
' - 1: an option has changed
' - 2: options have been reseted
' - 3: player closed the tweak UI, good time to update staticly prerendered parts
' Table1.Option arguments are: 
' - option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional arry of literal strings
Dim dspTriggered : dspTriggered = False
Sub FishTales_OptionEvent(ByVal eventId)
	If eventId = 1 And Not dspTriggered Then dspTriggered = True : DisableStaticPreRendering = True : End If


	RenderProbeOpt = FishTales.Option("Refraction Probe Setting", 0, 3, 1, 2, 0, Array("No Refraction (best performance)", "Roughness 0 (improved performance)", "Ramp Only Full (balanced)", "Full Refraction (best visuals)"))
	SetRenderProbes RenderProbeOpt
	RampDecals = FishTales.Option("Ramp Cover Decals", 0, 1, 1, 1, 0, Array("No Decals", "Show Decals"))
	SetRampDecals RampDecals
	ReflOpt = FishTales.Option("Playfield Reflections (if enabled)", 0, 2, 1, 1, 0, Array("No Reflections", "Static Reflections", "Dynamic Reflections (includes light maps)"))
	SetRefl ReflOpt
	SBZScale = FishTales.Option("Side blade z-scale (cabinet mode only)", 1, 2, 0.05, 1, 1)
	SetSBZScale SBZScale

	'Desktop DMD
	VPMDMDVisible = FishTales.Option("Desktop DMD", 0, 1, 1, 1, 0, Array("Not Visible", "Visible"))
	If VPMDMDVisible = 1 Then
		Scoretext.Visible = 1
	Else
		Scoretext.Visible = 0
	End If

	'VR Stuff
	VRRoom = FishTales.Option("VR Room", 0, 1, 1, 1, 0, Array("Ultra Minimal", "Fishing Scene"))
	Topper = FishTales.Option("VR Topper", 0, 1, 1, 1, 0, Array("Not Visible", "Visible"))
	AmbientSound = FishTales.Option("VR Ambient Sounds", 0, 2, 1, 2, 0, Array("No Sound", "Wave Sounds", "Waves and Generator Sounds"))
	VRDesktop = FishTales.Option("VR Room in Desktop", 0, 1, 1, 0, 0, Array("Disabled", "Enabled"))

	SetVRRoom

	'Difficulty
	OutlaneDifficulty = FishTales.Option("Outlane Difficulty", 0, 2, 1, 1, 0, Array("Easy", "Medium", "Hard"))
	SetOLDifficulty OutlaneDifficulty
	FlipperTricksOpt = FishTales.Option("Flipper Tricks Difficulty", 0, 1, 1, 1, 0, Array("Easy (Keyboard & Gamepad)", "Standard (Optimal for leaf switches)"))
	SetFlipperTricks FlipperTricksOpt
	
    ' Sound volumes
    VolumeDial = FishTales.Option("Mech Volume", 0, 1, 0.01, 0.8, 1)
    BallRollVolume = FishTales.Option("Ball Roll Volume", 0, 1, 0.01, 0.5, 1)
	RampRollVolume = FishTales.Option("Ramp Roll Volume", 0, 1, 0.01, 0.5, 1)

	' Room brightness
	LightLevel = FishTales.Option("Table Brightness (Ambient Light Level)", 0, 1, 0.01, .25, 1)
	BallLightLevel = FishTales.Option("Ball Brightness Overide ('0' is no over-ride)", 0, 1, 0.01, 1, 1)
	SetRoomBrightness LightLevel, BallLightLevel
    
	'LUT Selector
	LUTSel = FishTales.Option("LUT Selection (n/a for Fishing Scene)", 0, 2, 1, 0, 0, Array("None (Default)", "Haunt Freak's", "VPW 1-to-1"))
	SetLUT LUTSel

	If eventId = 3 And dspTriggered Then dspTriggered = False : DisableStaticPreRendering = False : End If
End Sub

Sub SetRenderProbes(Opt)
	BM_Ramp.Material = "VLM.Bake.Ramp"
	BM_RampEdges.Material = "VLM.Bake.RampEdges"
	BM_Bumpers.Material = "VLM.Bake.BumpOut"
	BM_BumperEdges.Material = "VLM.Bake.BumpOut"		

	On Error Resume Next
		Select Case Opt
			Case 0:
				BM_Ramp.RefractionProbe = ""
				BM_RampEdges.RefractionProbe = ""
				BM_Bumpers.RefractionProbe = ""
				BM_BumperEdges.RefractionProbe = ""

				BM_Ramp.Material = "VLM.Bake.Ramp_HO"
				BM_RampEdges.Material = "VLM.Bake.RampEdges_HO"
				BM_Bumpers.Material = "VLM.Bake.BumpOut_HO"
				BM_BumperEdges.Material = "VLM.Bake.BumpOut_HO"
			Case 1:
				BM_Ramp.RefractionProbe = "Plastic0"
				BM_RampEdges.RefractionProbe = "PlasticEdges0"
				BM_Bumpers.RefractionProbe = "Bumpers0"
				BM_BumperEdges.RefractionProbe = "Bumpers0"
			Case 2:
				BM_Ramp.RefractionProbe = "Plastic"
				BM_RampEdges.RefractionProbe = "PlasticEdges0"
				BM_Bumpers.RefractionProbe = ""
				BM_BumperEdges.RefractionProbe = ""

				BM_Bumpers.Material = "VLM.Bake.BumpOut_HO"
				BM_BumperEdges.Material = "VLM.Bake.BumpOut_HO"
			Case 3:
				BM_Ramp.RefractionProbe = "Plastic"
				BM_RampEdges.RefractionProbe = "PlasticEdges"
				BM_Bumpers.RefractionProbe = "Bumpers"
				BM_BumperEdges.RefractionProbe = "Bumpers"
		End Select
	On Error Goto 0
End Sub

Sub SetRampDecals(Opt) 'Ramp Decals and other LM initialization
	Dim BP
	For each BP in BP_RampCoversAlt: BP.visible = Opt : Next
	'For each BP in BP_RampDecalTop: BP.visible = Opt : Next

	For each BP in BP_RampCovers: BP.visible = not Opt : Next

	LM_Inserts_l16_RampCovers.visible = false
	LM_Inserts_l16_RampCoversAlt.visible = False
	LM_Inserts_l17_RampCovers.visible = False
	LM_Inserts_l17_RampCoversAlt.visible = False

	LM_GITop_Reel.visible = False	
End Sub

Sub SetRefl(Opt)
	Dim BP

	For each BP in BG_All: BP.reflectionenabled = False : Next

	BM_Playfield.visible = True
	BM_Playfield_Dyn.visible = False
	
	Select Case Opt
		Case 1:
			For each BP in BG_Bakemap
				If not (Right(BP.Name, 9) = "Playfield" or Right(BP.Name,7) = "UnderPF" or Right(BP.Name, 13) = "RailsLockdown") Then
					BP.reflectionenabled = True
				End If
			Next
		Case 2:
			For each BP in BG_All
				If not (Right(BP.Name, 9) = "Playfield" or Right(BP.Name,7) = "UnderPF" or Right(BP.Name, 13) = "RailsLockdown") Then
					BP.reflectionenabled = True
				End If
			Next
			BM_Playfield.visible=False
			BM_Playfield_Dyn.visible=True
	End Select
End Sub

Sub SetSBZScale(Opt)
	Dim BP

	If DesktopMode = False and RenderingMode <> 2 Then
		For each BP in BP_Sideblades
			BP.size_z = Opt
		Next
	End If
End Sub


Sub SetOLDifficulty(Opt)
	Dim BP, lvl

	zCol_Rubber_Post_L_Easy.collidable = 0
	zCol_Rubber_Post_R_Easy.collidable = 0

	zCol_Rubber_Post_L_Medium.collidable = 0
	zCol_Rubber_Post_R_Medium.collidable = 0

	zCol_Rubber_Post_L_Hard.collidable = 0
	zCol_Rubber_Post_R_Hard.collidable = 0


	Select Case Opt
		Case 0: 
			lvl = 0
			zCol_Rubber_Post_L_Easy.collidable = 1
			zCol_Rubber_Post_R_Easy.collidable = 1
		Case 1:
			lvl = -10
			zCol_Rubber_Post_L_Medium.collidable = 1
			zCol_Rubber_Post_R_Medium.collidable = 1
		Case 2:
			lvl = -20
			zCol_Rubber_Post_L_Hard.collidable = 1
			zCol_Rubber_Post_R_Hard.collidable = 1
	End Select

	For Each BP in BP_LeftPost 
		BP.transy = lvl
	Next
	For Each BP in BP_RightPost
		BP.transy = lvl
	Next
End Sub

Sub SetFlipperTricks(Opt)
	Select Case Opt
		Case 0: EOST = 0.275
		Case 1: EOST = 0.375
	End Select
End Sub

Sub SetLUT(Opt)
	dim xOpt
	xOpt = Opt
	If (RenderingMode = 2 or (DesktopMode and VRDesktop = 1)) and VRRoom = 1 Then xOpt = 3
	Select Case xOpt
		Case 0: FishTales.ColorGradeImage = ""
		Case 1: FishTales.ColorGradeImage = "ColorGradeHF"
		Case 2: FishTales.ColorGradeImage = "ColorGradeVPW_1to1"
		Case 3: FishTales.ColorGradeImage = "ColorGrade_8"
	End Select
End Sub

'****************************
' 	ZBRI: Room Brightness
'****************************

' Update these arrays if you want to change more materials with room light level
Dim RoomBrightnessMtlArray: RoomBrightnessMtlArray = Array("VLM.Bake.Active","VLM.Bake.Solid","VLM.Bake.RampCovers","VLM.Bake.RampEdges","VLM.Bake.BumpIn","VLM.Bake.BumpOut","VLM.Bake.Ramp","VLM.Bake.FishingRod","Plastic with an image", "Level_Shadow", "Level_Bubble", "Level_Liquid", "Level_Ends", "VLM.Bake.RampEdges_HO","VLM.Bake.BumpOut_HO","VLM.Bake.Ramp_HO")
Dim BallColor

Sub SetRoomBrightness(input, binput)
	dim lvl : lvl = input
	If lvl > 1 Then lvl = 1
	If lvl < 0 Then lvl = 0
	lvl = lvl*lvl  ' OPT Rule1

	' Lighting level
	Dim v: v=(lvl * 250 + 5)/255

	Dim i: For i = 0 to UBound(RoomBrightnessMtlArray)
		If RoomBrightnessMtlArray(i) <> "Level_Liquid" Then
			ModulateMaterialBaseColor RoomBrightnessMtlArray(i), i, v
		Else
			If v <= 0.05 Then 
				ModulateMaterialBaseColor RoomBrightnessMtlArray(i), i, 0.05
			Elseif v > 0.6 Then
				ModulateMaterialBaseColor RoomBrightnessMtlArray(i), i, 0.6
			Else
				ModulateMaterialBaseColor RoomBrightnessMtlArray(i), i, v
			End If
		End If
	Next

	If v <= 0.085 Then 
		Level_Bubble.blenddisablelighting = (2.5 - 11.765 * v)
	Elseif v > 0.6 Then
		Level_Bubble.blenddisablelighting = (1.25 - v)
	Else
		Level_Bubble.blenddisablelighting = (1.64 - 1.65 * v)
	End If

	If binput <> 0 Then
		lvl = binput*binput  ' OPT Rule1
		v = (lvl * 175 + 80)
	Else
		v = (lvl * 100 + 155)
	End If

	BallColor = v

	SetBallColor rgb(BallColor, BallColor, BallColor)

	If StringSticker.blenddisablelighting = 0.2 Then
		UpdateGI2 4, 0
	End If
End Sub

Dim SavedMtlColorArray
SaveMtlColors
Sub SaveMtlColors
	ReDim SavedMtlColorArray(UBound(RoomBrightnessMtlArray))
	Dim i: For i = 0 to UBound(RoomBrightnessMtlArray)
		SaveMaterialBaseColor RoomBrightnessMtlArray(i), i
	Next
End Sub

Sub SaveMaterialBaseColor(name, idx)
	Dim wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
	GetMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
	SavedMtlColorArray(idx) = round(base,0)
End Sub


Sub ModulateMaterialBaseColor(name, idx, val)
	Dim wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
	Dim red, green, blue, saved_base, new_base
 
	'First get the existing material properties
	GetMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle

	'Get saved color
	saved_base = SavedMtlColorArray(idx)
    
	'Next extract the r,g,b values from the base color
	red = saved_base And &HFF
	green = (saved_base \ &H100) And &HFF
	blue = (saved_base \ &H10000) And &HFF
	'msgbox red & " " & green & " " & blue

	'Create new color scaled down by 'val', and update the material
	new_base = RGB(red*val, green*val, blue*val)
	UpdateMaterial name, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, new_base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
End Sub

Sub SetBallColor(Opt)
	Dim i
	For each i in Array(FTBall1, FTBall2, FTBall3, FTCapBall)
		i.color = Opt
	Next
End Sub


'*******************************************
'  ZKEY: Key Press Handling
'*******************************************

Sub FishTales_KeyDown(ByVal keycode)
	If keycode = PlungerKey Then 
		Controller.Switch(31) = 1
		If VRroom = 1 Then CastButton.y = Castbutton.y -1: CastButton.z = Castbutton.z -2	 ' VRroom					
	end if

	If keycode = LeftFlipperKey and VRroom = 1 Then FlipperButtonLeft.X = FlipperButtonLeft.X + 8 ' VRroom
	If keycode = RightFlipperKey and VRroom = 1 Then FlipperButtonRight.X = FlipperButtonRight.X - 8 ' VRroom

	If keycode = LeftTiltKey Then Nudge 90, 1: SoundNudgeLeft: LevelNudgeY 0.7, -1 : End If
	If keycode = RightTiltKey Then Nudge 270, 1: SoundNudgeRight: LevelNudgeY 0.7, -1 : End If
	If keycode = CenterTiltKey Then Nudge 0, 1: SoundNudgeCenter: LevelNudgeY 1, -1 : End If

	If keycode = StartGameKey Then 
		SoundStartButton
		If VRroom = 1 Then 
		Primary_StartButton.y = Primary_StartButton.y - 5	 ' VRroom
		StartButton2.y = StartButton2.y - 5	 ' VRroom
		End If
	End If

	If keycode = AddCreditKey Or keycode = AddCreditKey2 Then SoundCoinIn

	If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub FishTales_KeyUp(ByVal keycode)
	If keycode = PlungerKey Then 
		Controller.Switch(31) = 0
		If VRroom = 1 Then CastButton.y = Castbutton.y +1: CastButton.z = Castbutton.z +2	 ' VRroom					
	end if

	If keycode = LeftFlipperKey and VRroom = 1 Then FlipperButtonLeft.X = FlipperButtonLeft.X - 8 ' VRroom
	If keycode = RightFlipperKey and VRroom = 1 Then FlipperButtonRight.X = FlipperButtonRight.X + 8 ' VRroom

	If keycode = StartGameKey Then 
		If VRroom = 1 Then 
		Primary_StartButton.y = Primary_StartButton.y + 5	 ' VRroom
		StartButton2.y = StartButton2.y + 5	 ' VRroom
		End If
	End If

	If vpmKeyUp(keycode) Then Exit Sub
End Sub



'*******************************************
'  ZSOL: Solenoids
'*******************************************

SolCallback(1)  =	"AutoPlunger" 	
SolCallback(2)	=	"SolCatapult" 	
SolCallback(3)  =	"SolVUK"		
SolCallback(6)  =   "SolGate"		
SolCallback(7)  =	"SolKnocker"	
SolCallback(8)  =	"TopperFish"  ' For VRTopper
SolCallback(9)  = 	"SolDrain"		
SolCallback(10) = 	"SolRelease"	
SolCallback(11) = 	"SolFF"			
SolCallback(12) =	"SolDTUp"									
SolCallback(13) =	"SolDTDown"					
SolCallback(28) =   "ReelMotor"

'Flipper Solenoids
SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(sLRFlipper) = "SolRFlipper"

'Flasher Solenoids
SolModCallBack(17) =	"Flash17"  'Jackpot Flasher
SolModCallBack(18) =	"Flash18"  'Super Jackpot Flasher
SolModCallBack(19) =	"Flash19"  'Instant Multiball Flasher
SolModCallBack(20) =	"Flash20"  'Light Extraball Flasher
SolModCallBack(21) =	"Flash21"  'Rock the Boat Flasher
SolModCallBack(22) =	"Flash22"  'Video Mode Flasher
SolModCallBack(23) =	"Flash23"  'Hold Bonus Flasher
SolModCallBack(25) =	"Flash25"  'Reel Flasher
SolModCallBack(26) =	"Flash26"  'Top Left Flasher
SolModCallBack(27) =	"Flash27"  'Caster Club Flasher

  



'*******************************************
'  ZAUT: AutoPlunger
'*******************************************

Sub AutoPlunger(Enabled)
	If Enabled Then
		Plunger.Fire
		SoundSaucerKick 0,Plunger
		'SoundPlungerReleaseBall()
	End If
End Sub

'*******************************************
'  ZCAT: Catapult
'*******************************************

Dim catdir, catball, catlen

catlen = Catapult.y - catprim.y

Sub Catapult_hit
	Controller.Switch(36) = 1
	SoundSaucerLock
	set catball = activeball
End Sub

Sub SolCatapult(enabled)
	If enabled Then
		SoundCatKick Catapult
		catdir=1
		CatapultTimer.enabled = 1
	End If
End Sub

Sub CatapultTimer_timer()
	Dim BP
	CatPrim.RotX = CatPrim.RotX + catdir*10

	If not isempty(catball) Then
		catball.x = Catapult.x
		catball.y = catapult.y - dSin(catprim.rotx) * catlen
		catball.z = 25 + catlen * dSin(catprim.rotx)

		If CatPrim.Rotx > 10 Then
			Controller.Switch(36) = 0
		End If
	End If

	If CatPrim.RotX >= 90 And catdir = 1 Then 
		catdir = -0.5
		If not isempty(catball) Then
			catapult.kickz 0, 40, 0 , -25
			controller.Switch(36) = 0 
			catball = empty
		End If
	End If
	If CatPrim.RotX <=1 Then 
		CatapultTimer.enabled = 0
	End If

	For each BP in BP_Catapult : BP.Rotx = CatPrim.Rotx : Next
End Sub

'*******************************************
'  ZVUK: VUK Caster Club
'*******************************************

dim vukball

Sub sw47_Hit
	Controller.Switch(47) = 1
	SoundSaucerLock
	set vukball = activeball
End Sub

Sub SolVUK(enabled)
	If enabled Then
		sw47.kick 0, 45, 1.56
		KPlungerPrim.TransY = 20
		If not isEmpty(vukball) Then
			Controller.Switch(47) = 0
			vukball = Empty
			SoundVukKick sw47			
		Else
			SoundCatKick sw47
		End If
	Else
		KPlungerPrim.TransY = 0
	End If
End Sub

'*******************************************
'  ZGAT: Gate Solenoid and Gates
'*******************************************

Sub SolGate(Enabled)
	If enabled Then
		Gate.Open = True
		Gate.Collidable = False
		SoundDiverterSolenoid 1, Gate
	Else
		Gate.Open = False
		Gate.collidable = True
		SoundDiverterSolenoid 0, Gate
	End If
End Sub

Sub Gate_Animate
	Dim a : a = Gate.CurrentAngle
	GatePrim.ObjRotY = a* -60/90 - 15 
	Dim BP : For Each BP in BP_Gate : BP.rotx = abs(GatePrim.ObjRotY): Next
End Sub

'*******************************************
'	ZKNO: KNOCKER
'*******************************************

Sub SolKnocker(Enabled)
	If enabled Then
		KnockerSolenoid 'Add knocker position object
	End If
End Sub

'*******************************************
'	ZDRN: Drain, Trough, and Ball Release
'*******************************************

Sub sw16_Hit:Controller.Switch(16) = 1:UpdateTrough:End Sub
Sub sw16_UnHit:Controller.Switch(16) = 0:UpdateTrough:End Sub
Sub sw17_Hit:Controller.Switch(17) = 1:UpdateTrough:End Sub
Sub sw17_UnHit:Controller.Switch(17) = 0:UpdateTrough:End Sub
Sub sw18_Hit:Controller.Switch(18) = 1:UpdateTrough:End Sub
Sub sw18_UnHit:Controller.Switch(18) = 0:UpdateTrough:End Sub

Sub UpdateTrough
	UpdateTroughTimer.Interval = 50
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer
	If sw16.BallCntOver = 0 Then sw17.kick 62, 10
	If sw17.BallCntOver = 0 Then sw18.kick 62, 10
	Me.Enabled = 0
End Sub


'	DRAIN & RELEASE
Sub Drain_Hit 
	controller.Switch(15) = 1
	RandomSoundDrain Drain 
End Sub

Sub Drain_unHit 
	controller.Switch(15) = 0
End Sub

Sub SolDrain(Enabled)
	If Enabled Then
		UpdateTrough
		Drain.kick 62, 20
		RandomSoundOutholeKicker Drain		
	End If
End Sub

Sub SolRelease(enabled)
	If enabled Then 
		sw16.kick 62, 10		
		RandomSoundBallRelease sw16
	End If
End Sub

'*******************************************
'	ZFIS: Fish Finder Saucer
'*******************************************

dim ffball

Sub sw63_Hit
	Controller.Switch(63) = 1
	SoundSaucerLock
	set ffball = activeball
End Sub

Sub SolFF(enabled)
	If enabled Then
		FishFinderCupWall001.collidable = 1
		dim rndkick
		rndkick = rnd * 2 + 9 '14.5
		sw63.kick 270, rndkick
		If not isEmpty(ffball) Then
			Controller.Switch(63) = 0
			ffball = Empty
			SoundSaucerKick 1, sw63
		Else
			SoundSaucerKick 0, sw63
		End If
	Else
		FishFinderCupWall001.collidable = 0
	End If
End Sub

'*******************************************
'	ZDTA: Drop Targets
'*******************************************

Sub SolDTUp(Enabled)
	If Enabled Then
		RandomSoundDropTargetReset sw48p
		DTRaise 48
	End If
End Sub

Sub SolDTDown(Enabled)
	If Enabled Then
		SoundDropTargetDrop sw48p
		DTDrop 48
	End If
End Sub

Sub sw48_Hit
	DTHit 48
End Sub

Sub UpdateDropTargets
	' OPT: only write when DT position/rotation changed — each write forces LM primitive recompose
	Dim BP, tz, rx, ry
	tz = sw48p.transz : rx = sw48p.rotx : ry = sw48p.roty
	If tz <> lastDTz(0) Or rx <> lastDTrx(0) Or ry <> lastDTry(0) Then
		lastDTz(0)=tz : lastDTrx(0)=rx : lastDTry(0)=ry
		For each BP in BP_sw48 : BP.transz=tz : BP.rotx=rx : BP.roty=ry : Next
	End If
End Sub

'******************************************************
'  DROP TARGETS INITIALIZATION
'******************************************************

Class DropTarget
  Private m_primary, m_secondary, m_prim, m_sw, m_animate, m_isDropped

  Public Property Get Primary(): Set Primary = m_primary: End Property
  Public Property Let Primary(input): Set m_primary = input: End Property

  Public Property Get Secondary(): Set Secondary = m_secondary: End Property
  Public Property Let Secondary(input): Set m_secondary = input: End Property

  Public Property Get Prim(): Set Prim = m_prim: End Property
  Public Property Let Prim(input): Set m_prim = input: End Property

  Public Property Get Sw(): Sw = m_sw: End Property
  Public Property Let Sw(input): m_sw = input: End Property

  Public Property Get Animate(): Animate = m_animate: End Property
  Public Property Let Animate(input): m_animate = input: End Property

  Public Property Get IsDropped(): IsDropped = m_isDropped: End Property
  Public Property Let IsDropped(input): m_isDropped = input: End Property

  Public default Function init(primary, secondary, prim, sw, animate, isDropped)
    Set m_primary = primary
    Set m_secondary = secondary
    Set m_prim = prim
    m_sw = sw
    m_animate = animate
    m_isDropped = isDropped

    Set Init = Me
  End Function
End Class

'Define a variable for each drop target
Dim DT48

Set DT48 = (new DropTarget)(sw48, sw48a, sw48p, 48, 0, False)

Dim DTArray
DTArray = Array(DT48)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 52 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 10 'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 'max degrees primitive rotates when hit
Const DTDropDelay = 20 'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40 'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 30 'velocity at which the target will brick, set to '0' to disable brick
Const DTEnableBrick = 0 'Set to 0 to disable bricking, 1 to enable bricking
Const DTMass = 0.1 'Mass of the Drop Target (between 0 and 1), higher values provide more resistance

'******************************************************
'  DROP TARGETS FUNCTIONS
'******************************************************

Sub DTHit(switch)
	Dim i
	i = DTArrayID(switch)
	
	PlayTargetSound
	DTArray(i).animate = DTCheckBrick(ActiveBall,DTArray(i).prim)
	If DTArray(i).animate = 1 Or DTArray(i).animate = 3 Or DTArray(i).animate = 4 Then
		DTBallPhysics ActiveBall, DTArray(i).prim.rotz, DTMass
	End If
	DoDTAnim
End Sub

Sub DTRaise(switch)
	Dim i
	i = DTArrayID(switch)
	
	DTArray(i).animate =  - 1
	DoDTAnim
End Sub

Sub DTDrop(switch)
	Dim i
	i = DTArrayID(switch)
	
	DTArray(i).animate = 1
	DoDTAnim
End Sub

Function DTArrayID(switch)
	Dim i
	For i = 0 To UBound(DTArray)
		If DTArray(i).sw = switch Then
			DTArrayID = i
			Exit Function
		End If
	Next
End Function

Sub DTBallPhysics(aBall, angle, mass)
	Dim rangle,bangle,calc1, calc2, calc3
	rangle = (angle - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	
	calc1 = cor.BallVel(aball.id) * Cos(bangle - rangle) * (aball.mass - mass) / (aball.mass + mass)
	calc2 = cor.BallVel(aball.id) * Sin(bangle - rangle) * Cos(rangle + 4 * Atn(1) / 2)
	calc3 = cor.BallVel(aball.id) * Sin(bangle - rangle) * Sin(rangle + 4 * Atn(1) / 2)
	
	aBall.velx = calc1 * Cos(rangle) + calc2
	aBall.vely = calc1 * Sin(rangle) + calc3
End Sub

'Check if target is hit on it's face or sides and whether a 'brick' occurred
Function DTCheckBrick(aBall, dtprim)
	Dim bangle, bangleafter, rangle, rangle2, Xintersect, Yintersect, cdist, perpvel, perpvelafter, paravel, paravelafter
	rangle = (dtprim.rotz - 90) * 3.1416 / 180
	rangle2 = dtprim.rotz * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)
	
	Xintersect = (aBall.y - dtprim.y - Tan(bangle) * aball.x + Tan(rangle2) * dtprim.x) / (Tan(rangle2) - Tan(bangle))
	Yintersect = Tan(rangle2) * Xintersect + (dtprim.y - Tan(rangle2) * dtprim.x)
	
	cdist = Distance(dtprim.x, dtprim.y, Xintersect, Yintersect)
	
	perpvel = cor.BallVel(aball.id) * Cos(bangle - rangle)
	paravel = cor.BallVel(aball.id) * Sin(bangle - rangle)
	
	perpvelafter = BallSpeed(aBall) * Cos(bangleafter - rangle)
	paravelafter = BallSpeed(aBall) * Sin(bangleafter - rangle)
	
	If perpvel > 0 And  perpvelafter <= 0 Then
		If DTEnableBrick = 1 And  perpvel > DTBrickVel And DTBrickVel <> 0 And cdist < 8 Then
			DTCheckBrick = 3
		Else
			DTCheckBrick = 1
		End If
	ElseIf perpvel > 0 And ((paravel > 0 And paravelafter > 0) Or (paravel < 0 And paravelafter < 0)) Then
		DTCheckBrick = 4
	Else
		DTCheckBrick = 0
	End If
End Function

Sub DoDTAnim()
	Dim i, dt
	For i = 0 To UBound(DTArray)
		Set dt = DTArray(i)
		dt.animate = DTAnimate(dt.primary, dt.secondary, dt.prim, dt.sw, dt.animate)
	Next
End Sub

Function DTAnimate(primary, secondary, prim, switch, animate)
	Dim transz, switchid
	Dim animtime, rangle
	
	switchid = switch
	
	Dim ind
	ind = DTArrayID(switchid)
	
	rangle = prim.rotz * PI / 180
	
	DTAnimate = animate
	
	If animate = 0 Then
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	ElseIf primary.uservalue = 0 Then
		primary.uservalue = GameTime
	End If
	
	animtime = GameTime - primary.uservalue
	
	If (animate = 1 Or animate = 4) And animtime < DTDropDelay Then
		primary.collidable = 0
		If animate = 1 Then secondary.collidable = 1 Else secondary.collidable = 0
		prim.rotx = DTMaxBend * Cos(rangle)
		prim.roty = DTMaxBend * Sin(rangle)
		DTAnimate = animate
		Exit Function
	ElseIf (animate = 1 Or animate = 4) And animtime > DTDropDelay Then
		primary.collidable = 0
		If animate = 1 Then secondary.collidable = 1 Else secondary.collidable = 1 'If animate = 1 Then secondary.collidable = 1 Else secondary.collidable = 0 'updated by rothbauerw to account for edge case
		prim.rotx = DTMaxBend * Cos(rangle)
		prim.roty = DTMaxBend * Sin(rangle)
		animate = 2
		SoundDropTargetDrop prim
	End If
	
	If animate = 2 Then
		transz = (animtime - DTDropDelay) / DTDropSpeed * DTDropUnits *  - 1
		If prim.transz >  - DTDropUnits  Then
			prim.transz = transz
		End If
		
		prim.rotx = DTMaxBend * Cos(rangle) / 2
		prim.roty = DTMaxBend * Sin(rangle) / 2
		
		If prim.transz <= - DTDropUnits Then
			prim.transz =  - DTDropUnits
			secondary.collidable = 0
			DTArray(ind).isDropped = True 'Mark target as dropped
			controller.Switch(Switchid) = 1
			primary.uservalue = 0
			DTAnimate = 0
			Exit Function
		Else
			DTAnimate = 2
			Exit Function
		End If
	End If
	
	If animate = 3 And animtime < DTDropDelay Then
		primary.collidable = 0
		secondary.collidable = 1
		prim.rotx = DTMaxBend * Cos(rangle)
		prim.roty = DTMaxBend * Sin(rangle)
	ElseIf animate = 3 And animtime > DTDropDelay Then
		primary.collidable = 1
		secondary.collidable = 0
		prim.rotx = 0
		prim.roty = 0
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	End If
	
	If animate =  - 1 Then
		transz = (1 - (animtime) / DTDropUpSpeed) * DTDropUnits *  - 1
		
		If prim.transz =  - DTDropUnits Then
			Dim b
			Dim gBOT
			gBOT = GetBalls
			
			For b = 0 To UBound(gBOT)
				If InRotRect(gBOT(b).x,gBOT(b).y,prim.x, prim.y, prim.rotz, - 25, - 10,25, - 10,25,25, - 25,25) And gBOT(b).z < prim.z + DTDropUnits + 25 Then
					gBOT(b).velz = 20
				End If
			Next
		End If
		
		If prim.transz < 0 Then
			prim.transz = transz
		ElseIf transz > 0 Then
			prim.transz = transz
		End If
		
		If prim.transz > DTDropUpUnits Then
			DTAnimate =  - 2
			prim.transz = DTDropUpUnits
			prim.rotx = 0
			prim.roty = 0
			primary.uservalue = GameTime
		End If
		primary.collidable = 0
		secondary.collidable = 1
		DTArray(ind).isDropped = False 'Mark target as not dropped
		controller.Switch(Switchid) = 0
	End If
	
	If animate =  - 2 And animtime > DTRaiseDelay Then
		prim.transz = (animtime - DTRaiseDelay) / DTDropSpeed * DTDropUnits *  - 1 + DTDropUpUnits
		If prim.transz < 0 Then
			prim.transz = 0
			primary.uservalue = 0
			DTAnimate = 0
			
			primary.collidable = 1
			secondary.collidable = 0
		End If
	End If
End Function

Function DTDropped(switchid)
	Dim ind
	ind = DTArrayID(switchid)
	
	DTDropped = DTArray(ind).isDropped
End Function

'******************************************************
'	ZSTA: STAND-UP TARGETS by Rothbauerw
'******************************************************

Sub sw27_Hit():STHit 27:End Sub 'Left Bottom Standup Target
Sub sw28_Hit():STHit 28:End Sub 'Left Top Standup Target

Sub sw41_Hit():STHit 41:End Sub 'Target Boat Captive Ball

Sub sw54_Hit():STHit 54:End Sub 'Right Top Standup Target
Sub sw55_Hit():STHit 55:End Sub 'Right Bottom Standup Target

Sub sw61_Hit():STHit 61:End Sub 'Caster Club Target

Sub UpdateStandupTargets
	' OPT: only write transx when BM position changed — each write forces LM primitive recompose
	Dim BP, tx
	tx = BM_sw27.transx : If tx <> lastST(0) Then lastST(0)=tx : For each BP in BP_sw27 : BP.transx=tx : Next : End If
	tx = BM_sw28.transx : If tx <> lastST(1) Then lastST(1)=tx : For each BP in BP_sw28 : BP.transx=tx : Next : End If
	tx = BM_sw41.transx : If tx <> lastST(2) Then lastST(2)=tx : For each BP in BP_sw41 : BP.transx=tx : Next : End If
	tx = BM_sw54.transx : If tx <> lastST(3) Then lastST(3)=tx : For each BP in BP_sw54 : BP.transx=tx : Next : End If
	tx = BM_sw55.transx : If tx <> lastST(4) Then lastST(4)=tx : For each BP in BP_sw55 : BP.transx=tx : Next : End If
	tx = BM_sw61.transx : If tx <> lastST(5) Then lastST(5)=tx : For each BP in BP_sw61 : BP.transx=tx : Next : End If
End Sub

'******************************************************

Class StandupTarget
  Private m_primary, m_prim, m_sw, m_animate

  Public Property Get Primary(): Set Primary = m_primary: End Property
  Public Property Let Primary(input): Set m_primary = input: End Property

  Public Property Get Prim(): Set Prim = m_prim: End Property
  Public Property Let Prim(input): Set m_prim = input: End Property

  Public Property Get Sw(): Sw = m_sw: End Property
  Public Property Let Sw(input): m_sw = input: End Property

  Public Property Get Animate(): Animate = m_animate: End Property
  Public Property Let Animate(input): m_animate = input: End Property

  Public default Function init(primary, prim, sw, animate)
    Set m_primary = primary
    Set m_prim = prim
    m_sw = sw
    m_animate = animate

    Set Init = Me
  End Function
End Class

'Define a variable for each stand-up target
Dim ST27, ST28, ST41, ST54, ST55, ST61

'Set array with stand-up target objects
'
'StandupTargetvar = Array(primary, prim, swtich)
'   primary:	vp target to determine target hit
'   prim:	   primitive target used for visuals and animation
'				   IMPORTANT!!!
'				   transy must be used to offset the target animation
'   switch:	 ROM switch number
'   animate:	Arrary slot for handling the animation instrucitons, set to 0
'
'You will also need to add a secondary hit object for each stand up (name sw11o, sw12o, and sw13o on the example Table1)
'these are inclined primitives to simulate hitting a bent target and should provide so z velocity on high speed impacts

Set ST27 = (new StandupTarget)(sw27, BM_sw27,27, 0)
Set ST28 = (new StandupTarget)(sw28, BM_sw28,28, 0)
Set ST41 = (new StandupTarget)(sw41, BM_sw41,41, 0)
Set ST54 = (new StandupTarget)(sw54, BM_sw54,54, 0)
Set ST55 = (new StandupTarget)(sw55, BM_sw55,55, 0)
Set ST61 = (new StandupTarget)(sw61, BM_sw61,61, 0)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
'   STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST27, ST28, ST41, ST54, ST55, ST61)

'Configure the behavior of Stand-up Targets
Const STAnimStep = 1.5  'vpunits per animation step (control return to Start)
Const STMaxOffset = 9   'max vp units target moves when hit

Const STMass = 0.2	  'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

'******************************************************
'				STAND-UP TARGETS FUNCTIONS
'******************************************************

Sub STHit(switch)
	Dim i
	i = STArrayID(switch)
	
	PlayTargetSound
	STArray(i).animate = STCheckHit(ActiveBall,STArray(i).primary)
	
	If STArray(i).animate <> 0 Then
		DTBallPhysics ActiveBall, STArray(i).primary.orientation, STMass
	End If
	DoSTAnim
End Sub

Function STArrayID(switch)
	Dim i
	For i = 0 To UBound(STArray)
		If STArray(i).sw = switch Then
			STArrayID = i
			Exit Function
		End If
	Next
End Function

Function STCheckHit(aBall, target) 'Check if target is hit on it's face
	Dim bangle, bangleafter, rangle, rangle2, perpvel, perpvelafter, paravel, paravelafter
	rangle = (target.orientation - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)
	
	perpvel = cor.BallVel(aball.id) * Cos(bangle - rangle)
	paravel = cor.BallVel(aball.id) * Sin(bangle - rangle)
	
	perpvelafter = BallSpeed(aBall) * Cos(bangleafter - rangle)
	paravelafter = BallSpeed(aBall) * Sin(bangleafter - rangle)
	
	If perpvel > 0 And  perpvelafter <= 0 Then
		STCheckHit = 1
	ElseIf perpvel > 0 And ((paravel > 0 And paravelafter > 0) Or (paravel < 0 And paravelafter < 0)) Then
		STCheckHit = 1
	Else
		STCheckHit = 0
	End If
End Function

Sub DoSTAnim()
	Dim i, st
	For i = 0 To UBound(STArray)
		Set st = STArray(i)
		st.animate = STAnimate(st.primary, st.prim, st.sw, st.animate)
	Next
End Sub

Function STAnimate(primary, prim, switch,  animate)
	Dim animtime
	
	STAnimate = animate
	
	If animate = 0  Then
		primary.uservalue = 0
		STAnimate = 0
		Exit Function
	ElseIf primary.uservalue = 0 Then
		primary.uservalue = GameTime
	End If
	
	animtime = GameTime - primary.uservalue
	
	If animate = 1 Then
		primary.collidable = 0
		prim.transy =  - STMaxOffset
		vpmTimer.PulseSw switch
		STAnimate = 2
		Exit Function
	ElseIf animate = 2 Then
		prim.transy = prim.transy + STAnimStep
		If prim.transy >= 0 Then
			prim.transy = 0
			primary.collidable = 1
			STAnimate = 0
			Exit Function
		Else
			STAnimate = 2
		End If
	End If
End Function



'*******************************************
'	ZREE: Fishing Reel
'*******************************************

Dim ReelPosition, ReelStop
ReelPosition = 330

Sub ReelMotor(enabled)
 	If enabled Then 
		lastReelTime = 0
		ReelStop = 0
 		ReelTimer.enabled=1
		SoundMotor 1, reel
		ReelEnter1.enabled = False
		ReelEnter2.enabled = False
		ReelEnter3.enabled = False
		ReelBlock.collidable = True
 	Else
		SoundMotor 0, reel
		ReelStop = 1
		ReelTimer.enabled=0
 	End If
End Sub
	
Dim PosToRot
Dim RBall1, RBall2, RBall3
Dim ReelBallRadius: ReelBallRadius = 37
Dim degAngle, degAngle2, degAngle3, ReelAngle
ReelAngle = 20

Dim resolution: resolution = 1

ReelTimer.Interval = -1
Dim lastReelTime: lastReelTime = 0

Sub RotateReel()
	dim timestep, sw37, sw38

	If lastReelTime <> 0 Then
		timestep = gametime - lastReelTime
		ReelPosition = ReelPosition + resolution/5*timestep
		degAngle = degAngle + resolution/5*timestep
		degAngle2 = degAngle2 + resolution/5*timestep
		degAngle3 = degAngle3 + resolution/5*timestep
	End If

	lastReelTime = gametime 

	If ReelPosition >= 360 then ReelPosition = ReelPosition mod 360

	' ReelPositions
	' 10 - 30		Lock 1
	' 70 - 90 		Ball 3 Out
	' 130 - 150		Lock 3
	' 190 - 210 	Ball 1 Out
	' 250 - 270		Lock 2
	' 310 - 330		Ball 2 Out


	Select Case True
		Case  (ReelPosition >= 0 and ReelPosition < 20) : sw38 = 1 : sw37 = 1
		Case  (ReelPosition >= 20 and ReelPosition < 30) : sw38 = 0 : sw37 = 1
		Case  (ReelPosition >= 30 and ReelPosition < 50) : sw38 = 1 : sw37 = 1
		Case  (ReelPosition >= 50 and ReelPosition < 60) : sw38 = 0 : sw37 = 0
		Case  (ReelPosition >= 60 and ReelPosition < 80) : sw38 = 0 : sw37 = 1
		Case  (ReelPosition >= 80 and ReelPosition < 90) : sw38 = 0 : sw37 = 0
		Case  (ReelPosition >= 90 and ReelPosition < 110) : sw38 = 1 : sw37 = 0
		Case  (ReelPosition >= 110 and ReelPosition < 120) : sw38 = 0 : sw37 = 0
		Case  (ReelPosition >= 120 and ReelPosition < 140) : sw38 = 0 : sw37 = 1
		Case  (ReelPosition >= 140 and ReelPosition < 150) : sw38 = 0 : sw37 = 0
		Case  (ReelPosition >= 150 and ReelPosition < 170) : sw38 = 1 : sw37 = 0
		Case  (ReelPosition >= 170 and ReelPosition < 180) : sw38 = 0 : sw37 = 0
		Case  (ReelPosition >= 180 and ReelPosition < 200) : sw38 = 0 : sw37 = 1
		Case  (ReelPosition >= 200 and ReelPosition < 210) : sw38 = 0 : sw37 = 0
		Case  (ReelPosition >= 210 and ReelPosition < 230) : sw38 = 1 : sw37 = 0
		Case  (ReelPosition >= 230 and ReelPosition < 240) : sw38 = 0 : sw37 = 0
		Case  (ReelPosition >= 240 and ReelPosition < 260) : sw38 = 0 : sw37 = 1
		Case  (ReelPosition >= 260 and ReelPosition < 270) : sw38 = 0 : sw37 = 0
		Case  (ReelPosition >= 270 and ReelPosition < 290) : sw38 = 1 : sw37 = 0
		Case  (ReelPosition >= 290 and ReelPosition < 300) : sw38 = 0 : sw37 = 0
		Case  (ReelPosition >= 300 and ReelPosition < 320) : sw38 = 0 : sw37 = 1
		Case  (ReelPosition >= 320 and ReelPosition < 330) : sw38 = 0 : sw37 = 0
		Case  (ReelPosition >= 330 and ReelPosition < 360) : sw38 = 1 : sw37 = 0
	End Select

	Controller.Switch(37) = sw37
	Controller.Switch(38) = sw38

	'debug.print ReelPosition & " " & sw37 & " " & sw38

	Reel.Rotx = ReelPosition + 46

	If not isEmpty(RBall1) Then
		RBall1.x = ReelEnter1.x
		RBall1.y = Reel.y + (ReelBallRadius * dCos(degAngle))
		RBall1.z = Reel.z + (ReelBallRadius * dSin(degAngle)) + 25
	End If

	If not isEmpty(RBall2) Then
		RBall2.x = ReelEnter2.x
		RBall2.y = Reel.y + (ReelBallRadius * dCos(degAngle2))
		RBall2.z = Reel.z + (ReelBallRadius * dSin(degAngle2)) + 25
	End If

	If not isEmpty(RBall3) Then
		RBall3.x = ReelEnter3.x
		RBall3.y = Reel.y + (ReelBallRadius * dCos(degAngle3))
		RBall3.z = Reel.z + (ReelBallRadius * dSin(degAngle3)) + 25
	End If

	dim BP
	For each BP in BP_Reel
		BP.rotz = reel.rotx + 55
		BP.x = 117.0175
		BP.y = 991.3585
		BP.z = 117.594
	Next
	For each BP in BP_ReelStickerLM
		BP.x = 115.9624
		BP.y = 991.3585
		BP.z = 117.594
	Next
	StringSticker.rotx = reel.rotx

	If ReelStop = 1 Then
		BallOut()
		'debug.print "Stop"
	End If

End Sub

Sub ReelTimer_Timer()
	RotateReel()
End Sub


Sub BallOut()
	If ReelPosition > ReelAngle - 10 and ReelPosition < ReelAngle + 10 and isEmpty(RBall1) Then ReelEnter1.enabled = True: degAngle = 90: ReelBlock.collidable = false
	If ReelPosition > ReelAngle - 10 + 120 and ReelPosition < ReelAngle + 10 + 120 and isEmpty(RBall2) Then ReelEnter2.enabled = True: degAngle2 = 90: ReelBlock.collidable = false
	If ReelPosition > ReelAngle - 10 + 240 and ReelPosition < ReelAngle + 10 + 240 and isEmpty(RBall3) Then ReelEnter3.enabled = True: degAngle3 = 90: ReelBlock.collidable = false

	If ReelPosition >= ReelAngle - 10 + 180 And ReelPosition <= ReelAngle + 10 + 180  And Not isEmpty(RBall1) Then RBall1.z = RBall1.z - 25: ReelEnter1.kick 250, 2, 0: RBall1 = Empty
	If ReelPosition >= ReelAngle - 10 + 300  And ReelPosition <= ReelAngle + 10 + 300 And Not isEmpty(RBall2) Then RBall2.z = RBall2.z - 25: ReelEnter2.kick 250, 2, 0: RBall2 = Empty
	If ReelPosition >= ReelAngle - 10 + 60  And ReelPosition <= ReelAngle + 10 + 60 And Not isEmpty(RBall3) Then RBall3.z = RBall3.z - 25: ReelEnter3.kick 250, 2, 0: RBall3 = Empty
End Sub

Sub ReelEnter1_Hit() 	
	RandomSoundBallBouncePlayfieldSoft Activeball
	set RBall1 = Activeball
End Sub

Sub ReelEnter2_Hit() 	
	RandomSoundBallBouncePlayfieldSoft Activeball
	set RBall2 = Activeball
End Sub

Sub ReelEnter3_Hit() 	
	RandomSoundBallBouncePlayfieldSoft Activeball
	set RBall3 = Activeball
End Sub


'*******************************************
'	ZFLP: Flippers
'*******************************************

Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
	If Enabled Then
		FlipperActivate LeftFlipper, LFPress
		LF.Fire  'leftflipper.rotatetoend
	
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If		
	Else
		FlipperDeActivate LeftFlipper, LFPress
		LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
    End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		FlipperActivate RightFlipper, RFPress
		RF.Fire 'rightflipper.rotatetoend

		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		FlipperDeActivate RightFlipper, RFPress
		RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LF.ReProcessBalls ActiveBall
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RF.ReProcessBalls ActiveBall
	RightFlipperCollide parm
End Sub

Sub LeftFlipper_Animate
	Dim a : a = LeftFlipper.CurrentAngle  + 270
	Dim b : b = LeftFlipper.x
	Dim c : c = LeftFlipper.y

	FlipperLSh.RotZ = LeftFlipper.CurrentAngle

	Dim v, BP
	v = 255.0 * (122.0 - LeftFlipper.CurrentAngle) / (122.0 -  74.5)

	For each BP in BP_FlipperLDown
		BP.Rotz = a
		BP.x = b
		BP.y = c
		BP.Size_x = 0.98 'remove when visuals are updated
		BP.Size_y = 0.98 'remove when visuals are updated
		BP.visible = v < 128.0
	Next
	For each BP in BP_FlipperLUp
		BP.Rotz = a
		BP.x = b
		BP.y = c
		BP.Size_x = 0.98 'remove when visuals are updated
		BP.Size_y = 0.98 'remove when visuals are updated
		BP.visible = v >= 128.0
	Next
End Sub

Sub RightFlipper_Animate
	Dim a : a = RightFlipper.CurrentAngle - 90
	Dim b : b = RightFlipper.x
	Dim c : c = RightFlipper.y

	FlipperRSh.RotZ = RightFlipper.CurrentAngle

	Dim v, BP
	v = 255.0 * (-122.0 - RightFlipper.CurrentAngle) / (-122.0 + 74.5)

	For each BP in BP_FlipperRDown
		BP.Rotz = a
		BP.x = b
		BP.y = c
		BP.Size_x = 0.98 'remove when visuals are updated
		BP.Size_y = 0.98 'remove when visuals are updated
		BP.visible = v < 128.0
	Next
	For each BP in BP_FlipperRup
		BP.Rotz = a
		BP.x = b
		BP.y = c
		BP.Size_x = 0.98 'remove when visuals are updated
		BP.Size_y = 0.98 'remove when visuals are updated
		BP.visible = v >= 128.0
	Next
End Sub


'******************************************************
'	ZFTR:  Flipper Tricks
'******************************************************

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
	
	If Flipper1.currentangle = Endangle1 And EOSNudge1 <> 1 Then
		EOSNudge1 = 1

		If Flipper2.currentangle = EndAngle2 Then
			For b = 0 To UBound(gBOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper1) Then
					Exit Sub
				End If
			Next
			For b = 0 To UBound(gBOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper2) Then
					gBOT(b).velx = gBOT(b).velx / 1.3
					gBOT(b).vely = gBOT(b).vely - 0.5
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


'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Dim ddx,ddy : ddx=ax-bx : ddy=ay-by : Distance = Sqr(ddx*ddx + ddy*ddy)  ' OPT Rule1
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

Function DistanceFromFlipperAngle(ballx, bally, Flipper, Angle)
	DistanceFromFlipperAngle = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Angle + 90)) + Flipper.x, Sin(Radians(angle + 90)) + Flipper.y)
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

'*************************************************
' End - Check ball distance from Flipper for Rem
'*************************************************

Dim LFPress, RFPress, LFCount, RFCount
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
Const EOSTnew = 1.2 '90's and later
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
Const EOSReturn = 0.035  'mid 80's to early 90's
'   Const EOSReturn = 0.025  'mid 90's and later

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
		Dim b
		For b = 0 To UBound(gBOT)
			If Distance(gBOT(b).x, gBOT(b).y, Flipper.x, Flipper.y) < 57 Then 'check for cradle
				If gBOT(b).vely >= - 0.4 Then gBOT(b).vely =  - 0.4
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
'	ZNFF:  FLIPPER CORRECTIONS by nFozzy
'******************************************************

Dim LF : Set LF = New FlipperPolarity
Dim RF : Set RF = New FlipperPolarity

InitPolarity

''*******************************************
''  Late 80's early 90's

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 60
		x.DebugOn=False ' prints some info in debugger

'		x.AddPt "Polarity", 0, 0, 0
'		x.AddPt "Polarity", 1, 0.05, - 5
'		x.AddPt "Polarity", 2, 0.4, - 5
'		x.AddPt "Polarity", 3, 0.6, - 4.5
'		x.AddPt "Polarity", 4, 0.65, - 4.0
'		x.AddPt "Polarity", 5, 0.7, - 3.5
'		x.AddPt "Polarity", 6, 0.75, - 3.0
'		x.AddPt "Polarity", 7, 0.8, - 2.5
'		x.AddPt "Polarity", 8, 0.85, - 2.0
'		x.AddPt "Polarity", 9, 0.9, - 1.5
'		x.AddPt "Polarity", 10, 0.95, - 1.0
'		x.AddPt "Polarity", 11, 1, - 0.5
'		x.AddPt "Polarity", 12, 1.1, 0
'		x.AddPt "Polarity", 13, 1.3, 0
'
'		x.AddPt "Polarity", 0, 0, 0
'		x.AddPt "Polarity", 1, 0.05, - 5
'		x.AddPt "Polarity", 2, 0.16, - 5
'		x.AddPt "Polarity", 3, 0.22, - 0
'		x.AddPt "Polarity", 4, 0.25, - 0
'		x.AddPt "Polarity", 5, 0.3, - 2
'		x.AddPt "Polarity", 6, 0.5, - 3.5
'		x.AddPt "Polarity", 7, 0.65, - 4.0
'		x.AddPt "Polarity", 8, 0.7, - 3.5
'		x.AddPt "Polarity", 9, 0.75, - 3.0
'		x.AddPt "Polarity", 10, 0.8, - 2.5
'		x.AddPt "Polarity", 11, 0.85, - 2.0
'		x.AddPt "Polarity", 12, 0.9, - 1.5
'		x.AddPt "Polarity", 13, 0.95, - 1.0
'		x.AddPt "Polarity", 14, 1, - 0.5
'		x.AddPt "Polarity", 15, 1.1, 0
'		x.AddPt "Polarity", 16, 1.3, 0

		x.AddPt "Polarity", 0, 0, 0
		x.AddPt "Polarity", 1, 0.05, - 5
		x.AddPt "Polarity", 2, 0.16, - 5
		x.AddPt "Polarity", 3, 0.22, - 0
		x.AddPt "Polarity", 4, 0.25, - 0
		x.AddPt "Polarity", 5, 0.3, - 2
		x.AddPt "Polarity", 6, 0.4, - 3
		x.AddPt "Polarity", 7, 0.5, - 4.0
		x.AddPt "Polarity", 8, 0.7, - 3.5
		x.AddPt "Polarity", 9, 0.75, - 3.0
		x.AddPt "Polarity", 10, 0.8, - 2.5
		x.AddPt "Polarity", 11, 0.85, - 2.0
		x.AddPt "Polarity", 12, 0.9, - 1.5
		x.AddPt "Polarity", 13, 0.95, - 1.0
		x.AddPt "Polarity", 14, 1, - 0.5
		x.AddPt "Polarity", 15, 1.1, 0
		x.AddPt "Polarity", 16, 1.3, 0

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
		
		If TypeName(aName) <> "String" Then MsgBox "FlipperPolarity: .SetObjects error: first argument must be a String (And name of Object). Found:" & TypeName(aName) : End If
		If TypeName(aFlipper) <> "Flipper" Then MsgBox "FlipperPolarity: .SetObjects error: Second argument must be a flipper. Found:" & TypeName(aFlipper) : End If
		If TypeName(aTrigger) <> "Trigger" Then MsgBox "FlipperPolarity: .SetObjects error: third argument must be a trigger. Found:" & TypeName(aTrigger) : End If
		If aFlipper.EndAngle > aFlipper.StartAngle Then LR = -1 Else LR = 1 : End If
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
			'debug.print "PolarityCorrect" & " " & GameTime & " " & Round(BallPos*100) & "%" & " AddX:" & Round(AddX,2) & " Vel%:" & Round(VelCoef*100) & " " & NoCorrection & " " & PartialFlipcoef
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
	Dim bsvx,bsvy,bsvz : bsvx=ball.VelX : bsvy=ball.VelY : bsvz=ball.VelZ
	BallSpeed = Sqr(bsvx*bsvx + bsvy*bsvy + bsvz*bsvz)  ' OPT Rule1
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)		'Set up line via two points, no clamping. Input X, output Y
	If X2 = X1 Then PSlope = Y1 : Exit Function  ' OPT Rule7: div-zero guard
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
'	ZBMP: Bumpers
'******************************************************
   
Sub Bumper1_Hit
	VPMTimer.PulseSw 51 
	RandomSoundBumperTop Bumper1
End Sub

Sub Bumper2_Hit
	VPMTimer.PulseSw 52
	RandomSoundBumperMiddle Bumper2
End Sub

Sub Bumper3_Hit
	VPMTimer.PulseSw 53
	RandomSoundBumperBottom Bumper3
End Sub

Sub Bumper1_Animate
	Dim z, BP
	z = Bumper1.CurrentRingOffset
	For Each BP in BP_Bumper1Ring : BP.transz = -z: Next
End Sub

Sub Bumper2_Animate
	Dim z, BP
	z = Bumper2.CurrentRingOffset
	For Each BP in BP_Bumper2Ring : BP.transz = -z: Next
End Sub

Sub Bumper3_Animate
	Dim z, BP
	z = Bumper3.CurrentRingOffset
	For Each BP in BP_Bumper3Ring : BP.transz = -z: Next
End Sub

Dim Bumpers : Bumpers = Array(Bumper1, Bumper2, Bumper3)

Sub AnimateBumperSkirts
	Dim r, g, s, x, y, b, tz, ubGBOT, brx, bry, bsx, bsy
	ubGBOT = UBound(gBOT)
	For r = 0 To 2
		g = 10000.
		brx = Bumpers(r).x
		bry = Bumpers(r).y
		For s = 0 To ubGBOT
			bsx = gBOT(s).x
			bsy = gBOT(s).y
			x = brx - bsx
			y = bry - bsy
			b = x * x + y * y
			If b < g Then g = b
		Next
		tz = 4
		If g < 6400 Then
			tz = 0
		End If
		If r = 0 Then For Each x in BP_Bumper_Socket: x.transZ = tz: Next
		If r = 1 Then For Each x in BP_Bumper_Socket_001: x.transZ = tz: Next
		If r = 2 Then For Each x in BP_Bumper_Socket_002: x.transZ = tz: Next
	Next
End Sub

'******************************************************
'	ZSLG: Slingshots
'******************************************************

Dim Lstep,RStep

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
	VPMTimer.PulseSw 57
    RandomSoundSlingshotLeft sling2

	Dim BP
	For Each BP in BP_LeftSling4 : BP.visible = 1: Next
	For Each BP in BP_LeftSling1 : BP.visible = 0: Next
	For Each BP in BP_LeftSlingArm : BP.transy = 24: Next

    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
	Dim BP

    Select Case LStep
        Case 3:	
			For Each BP in BP_LeftSling4 : BP.visible = 0: Next
			For Each BP in BP_LeftSling3 : BP.visible = 1: Next
			For Each BP in BP_LeftSlingArm : BP.transy = 16: Next
        Case 4:
			For Each BP in BP_LeftSling3 : BP.visible = 0: Next
			For Each BP in BP_LeftSling2 : BP.visible = 1: Next
			For Each BP in BP_LeftSlingArm : BP.transy = 8: Next
        Case 5:
			For Each BP in BP_LeftSling2 : BP.visible = 0: Next
			For Each BP in BP_LeftSling1 : BP.visible = 1: Next
			For Each BP in BP_LeftSlingArm : BP.transy = 0: Next
			LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
	VPMTimer.PulseSw 58
    RandomSoundSlingshotRight sling1

	Dim BP
	For Each BP in BP_RightSling4 : BP.visible = 1: Next
	For Each BP in BP_RightSling1 : BP.visible = 0: Next
	For Each BP in BP_RightSlingArm : BP.transy = 24: Next

    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
	Dim BP

    Select Case RStep
        Case 3:	
			For Each BP in BP_RightSling4 : BP.visible = 0: Next
			For Each BP in BP_RightSling3 : BP.visible = 1: Next
			For Each BP in BP_RightSlingArm : BP.transy = 16: Next
        Case 4:
			For Each BP in BP_RightSling3 : BP.visible = 0: Next
			For Each BP in BP_RightSling2 : BP.visible = 1: Next
			For Each BP in BP_RightSlingArm : BP.transy = 8: Next
        Case 5:
			For Each BP in BP_RightSling2 : BP.visible = 0: Next
			For Each BP in BP_RightSling1 : BP.visible = 1: Next
			For Each BP in BP_RightSlingArm : BP.transy = 0: Next
			RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub


'******************************************************
'	ZSSC: SLINGSHOT CORRECTION FUNCTIONS by apophis
'******************************************************

Dim LS
Set LS = New SlingshotCorrection
Dim RS
Set RS = New SlingshotCorrection

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
	AddSlingsPt 0, 0.00, - 4
	AddSlingsPt 1, 0.45, - 7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4
End Sub

Sub AddSlingsPt(idx, aX, aY)		'debugger wrapper for adjusting flipper script In-game
	Dim a
	a = Array(LS, RS)
	Dim x
	For Each x In a
		x.addpoint idx, aX, aY
	Next
End Sub

Class SlingshotCorrection
	Public DebugOn, Enabled
	Private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2
	
	Public ModIn, ModOut
	
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
		Enabled = True
	End Sub
	
	Public Property Let Object(aInput)
		Set Slingshot = aInput
	End Property
	
	Public Property Let EndPoint1(aInput)
		SlingX1 = aInput.x
		SlingY1 = aInput.y
	End Property
	
	Public Property Let EndPoint2(aInput)
		SlingX2 = aInput.x
		SlingY2 = aInput.y
	End Property
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If GameTime > 100 Then Report
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
	
	
	Public Sub VelocityCorrect(aBall)
		Dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then
			XL = SlingX1
			YL = SlingY1
			XR = SlingX2
			YR = SlingY2
		Else
			XL = SlingX2
			YL = SlingY2
			XR = SlingX1
			YR = SlingY1
		End If
		
		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then
			If Abs(XR - XL) > Abs(YR - YL) Then
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If
		
		'Velocity angle correction
		If Not IsEmpty(ModIn(0) ) Then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'   debug.print " BallPos=" & BallPos &" Angle=" & Angle
			'   debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled Then aBall.Velx = RotVxVy(0)
			If Enabled Then aBall.Vely = RotVxVy(1)
			'   debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely
			'   debug.print " "
		End If
	End Sub
End Class


'******************************************************
'	ZSWI: Switches
'******************************************************

Sub sw25_Hit():VPMTimer.PulseSw 25:End Sub 'Left OutLane
Sub sw26_Hit():VPMTimer.PulseSw 26:End Sub 'Left InLane

Sub sw32_Hit():VPMTimer.PulseSw 32:End Sub 'Ramp Left Sensor
Sub sw33_Hit():VPMTimer.PulseSw 33:End Sub 'Ramp Right Sensor

Sub sw35_Hit():VPMTimer.PulseSw 35:End Sub 'Reel Entry Trigger

Sub sw42a_Hit():VPMTimer.PulseSw 42:End Sub 'Boat Ramp Right Trigger
Sub sw43a_Hit():VPMTimer.PulseSw 43:End Sub 'Boat Ramp Left Trigger

Sub sw44_Hit():VPMTimer.PulseSw 44:End Sub 'E Trigger
Sub sw45_Hit():VPMTimer.PulseSw 45:End Sub 'I Trigger
Sub sw46_Hit():VPMTimer.PulseSw 46:End Sub 'L Trigger

Sub sw56_Hit():Controller.Switch (56)=1:End Sub 'Shooter Lane Trigger
Sub sw56_UnHit():Controller.Switch (56)=0:End Sub

Sub sw62_Hit():VPMTimer.PulseSw 62:End Sub 'Right Green Lane Trigger
Sub sw64_Hit():VPMTimer.PulseSw 64:End Sub 'Left Green Lane Trigger

Sub sw65_Hit():VPMTimer.PulseSw 65:End Sub 'Right InLane
Sub sw66_Hit():VPMTimer.PulseSw 66:End Sub 'Right OutLane



' Animations

Sub InitSwitchPrims
	sw25_Animate:sw26_Animate
	sw42a_Animate:sw43a_Animate:sw44_Animate:sw45_Animate:sw46_Animate
	sw56_Animate
	sw62_Animate:sw64_Animate:sw65_Animate:sw66_Animate
End Sub

Sub sw25_Animate
	Dim x, BP
	x = sw25.CurrentAnimOffset
	For Each BP in BP_sw25:BP.transx = -x: BP.objrotx = -90:BP.objroty = 90:BP.objrotz = 90:Next
End Sub

Sub sw26_Animate
	Dim x, BP
	x = sw26.CurrentAnimOffset
	For Each BP in BP_sw26:BP.transx = -x: BP.objrotx = -90:BP.objroty = 90:BP.objrotz = 90:Next
End Sub

Sub sw32_Animate
	Dim x, BP
	x = sw32.CurrentAnimOffset
	For Each BP in BP_sw32:BP.rotx = x*2/3:Next 'Should be rot instead of trans?
End Sub

Sub sw33_Animate
	Dim x, BP
	x = sw33.CurrentAnimOffset
	For Each BP in BP_sw33:BP.rotx = x*2/3:Next
End Sub

Sub sw42a_Animate
	Dim x, BP
	x = sw42a.CurrentAnimOffset
	For Each BP in BP_sw42a:BP.transx = -x: BP.objrotx = -90:BP.objroty = 312:BP.objrotz = 90:Next
End Sub

Sub sw43a_Animate
	Dim x, BP
	x = sw43a.CurrentAnimOffset
	For Each BP in BP_sw43a:BP.transx = -x: BP.objrotx = -90:BP.objroty = 227:BP.objrotz = 90:Next
End Sub


Sub sw44_Animate
	Dim x, BP
	x = sw44.CurrentAnimOffset
	For Each BP in BP_sw44:BP.transx = -x: BP.objrotx = -90:BP.objroty = 74:BP.objrotz = 90:Next
End Sub

Sub sw45_Animate
	Dim x, BP
	x = sw45.CurrentAnimOffset
	For Each BP in BP_sw45:BP.transx = -x: BP.objrotx = -90:BP.objroty = 90:BP.objrotz = 90:Next
End Sub

Sub sw46_Animate
	Dim x, BP
	x = sw46.CurrentAnimOffset
	For Each BP in BP_sw46:BP.transx = -x: BP.objrotx = -90:BP.objroty = 104:BP.objrotz = 90:Next
End Sub

Sub sw56_Animate
	Dim x, BP
	x = sw56.CurrentAnimOffset
	For Each BP in BP_sw56:BP.transx = -x: BP.objrotx = -90:BP.objroty = 90:BP.objrotz = 90:Next
End Sub

Sub sw62_Animate
	Dim x, BP
	x = sw62.CurrentAnimOffset
	For Each BP in BP_sw62:BP.transx = -x: BP.objrotx = -90:BP.objroty = 345.5:BP.objrotz = 90:Next
End Sub

Sub sw64_Animate
	Dim x, BP
	x = sw64.CurrentAnimOffset
	For Each BP in BP_sw64:BP.transx = -x: BP.objrotx = -90:BP.objroty = 73:BP.objrotz = 90:Next
End Sub

Sub sw65_Animate
	Dim x, BP
	x = sw65.CurrentAnimOffset
	For Each BP in BP_sw65:BP.transx = -x: BP.objrotx = -90:BP.objroty = 90:BP.objrotz = 90:Next
End Sub

Sub sw66_Animate
	Dim x, BP
	x = sw66.CurrentAnimOffset
	For Each BP in BP_sw66:BP.transx = -x: BP.objrotx = -90:BP.objroty = 90:BP.objrotz = 90:Next
End Sub



'******************************************************
'	ZSPI: Spinner
'******************************************************

'Spinner
Sub sw34_Spin():VPMTimer.PulseSw 34:SoundSpinner sw34:End Sub

Sub sw34_animate
	Dim a : a = sw34.CurrentAngle
	Dim BP : For Each BP in BP_sw34 : BP.rotx = -a: BP.Rotz = 0: BP.ObjRotz = -8 :Next
End Sub

'******************************************************
'	ZGII: General Illumination (GI)
'******************************************************

 Set GiCallback2 = GetRef("UpdateGI2")

Dim gistep, xx


Sub UpdateGI2(no, level)
    gistep=level
	'debug.print "UpdateGI2("&no&", "&step&")"

	If gistep = 1 Then
		DOF 101, DOFOn
	Else
		If gistep = 0 Then DOF 101, DOFOff
	End If

    Select Case no
		Case 0 	'Backglass gi001
			For each xx in BackglassGI: xx.state=gistep: next
		Case 1  'Topper
			For each xx in TopperGI: xx.state=gistep: next
        Case 2	'Top GI
            For each xx in TopGI: xx.state=gistep: next
			BM_Reel.blenddisablelighting = (4 - 0.5)*gistep + 0.5
        Case 4	'Bottom GI		
            For each xx in BottomGI: xx.state=gistep: next
			If gistep > 0 then 
				StringStickerShadow.image = "ReelShadow_On"
			Else
				StringStickerShadow.image = "ReelShadow"
			End If
			StringSticker.blenddisablelighting = (0.5 - 0.2)*gistep + 0.2

			Dim nbcolor, c, gs2 : gs2 = gistep*gistep  ' OPT Rule1: cache gistep^2
			If LightLevel >= 0.5 Then
				nbcolor = (0.4 * BallColor * gs2) + (0.6 * Ballcolor)
			ElseIf LightLevel >= 0.2 Then
				nbcolor = (0.6 * BallColor * gs2) + (0.4 * Ballcolor)
			ElseIf LightLevel >= 0.02 Then
				nbcolor = (0.8 * BallColor * gs2) + (0.2 * Ballcolor)
			Else
				nbcolor = (0.9 * BallColor * gs2) + (0.10 * Ballcolor)
			End If

			c = Int(nbcolor)
			If c > 255 Then c = 255

			SetBallColor rgb(c, c, c)
    End Select
End Sub

'******************************************************
'	ZPWM:  PWM Flasher Stuff
'******************************************************

'********************************************
const DebugFlashers = False

Sub Flash17(pwm)
	If DebugFlashers then debug.print "Flash17 "&pwm
	f17.state = pwm
	f17b.state = pwm
End Sub


Sub Flash18(pwm)
	If DebugFlashers then debug.print "Flash18 "&pwm
	f18.state = pwm
	f18b.state = pwm
End Sub

Sub Flash19(pwm)
	If DebugFlashers then debug.print "Flash19 "&pwm
	f19.state = pwm
End Sub

Sub Flash20(pwm)
	If DebugFlashers then debug.print "Flash20 "&pwm
	f20.state = pwm
End Sub

Sub Flash21(pwm)
	If DebugFlashers then debug.print "Flash21 "&pwm
	f21.state = pwm
End Sub

Sub Flash22(pwm)
	If DebugFlashers then debug.print "Flash22 "&pwm
	f22.state = pwm
End Sub

Sub Flash23(pwm)
	If DebugFlashers then debug.print "Flash23 "&pwm
	f23.state = pwm
End Sub

Sub Flash25(pwm)
	If DebugFlashers then debug.print "Flash25 "&pwm
	f25.state = pwm
End Sub

Sub Flash26(pwm)
	If DebugFlashers then debug.print "Flash26 "&pwm
	f26.state = pwm
End Sub

Sub Flash27(pwm)
	If DebugFlashers then debug.print "Flash27 "&pwm
	f27.state = pwm
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
		Dim tbvs:tbvs=vel*vel : Dim tbvz:tbvz=aBall.velz*aBall.velz : Dim tbvr:tbvr=vratio*vratio
		aBall.velx = Sgn(aBall.velx) * Sqr(Abs((tbvs - tbvz) / (1 + tbvr)))  ' OPT Rule1
		aBall.vely = aBall.velx * vratio
		'   debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		'   debug.print "conservation check: " & BallSpeed(aBall)/vel
	End If
End Sub

'Add targets or posts to the TargetBounce collection if you want to activate the targetbouncer code from them
Sub TargetBounce_Hit(idx)
	TargetBouncer ActiveBall, 1
End Sub

'**********************************
' ZMAT: General Math Functions
'**********************************

Dim PI
PI = 4 * Atn(1)

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

Function ArcCos(x)
	If x = 1 Then
		ArcCos = 0/180*PI
	ElseIf x = -1 Then
		ArcCos = 180/180*PI
	Else
		ArcCos = Atn(-x/Sqr(-x * x + 1)) + 2 * Atn(1)
	End If
End Function

Function max(a,b)
	If a > b Then
		max = a
	Else
		max = b
	End If
End Function

Function min(a,b)
	If a > b Then
		min = b
	Else
		min = a
	End If
End Function

' Used for drop targets
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

	Private Sub Class_Initialize  ' OPT Rule5: fixed-size init
		ReDim ballvel(tnob)
		ReDim ballvelx(tnob)
		ReDim ballvely(tnob)
	End Sub

	Public Sub Update()	'tracks in-ball-velocity
		' OPT Rule5: single-pass, ReDim Preserve only when needed
		Dim b, bid
		For Each b In GetBalls
			bid = b.id
			If bid > UBound(ballvel) Then
				ReDim Preserve ballvel(bid)
				ReDim Preserve ballvelx(bid)
				ReDim Preserve ballvely(bid)
			End If
			ballvel(bid)  = BallSpeed(b)
			ballvelx(bid) = b.velx
			ballvely(bid) = b.vely
		Next
	End Sub
End Class

'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************

'******************************************************
'	ZBRL:  BALL ROLLING AND DROP SOUNDS
'******************************************************

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
	Dim b, ball, bz, bvz, ubGBOT
	ubGBOT = UBound(gBOT)

	For b = 0 To ubGBOT
		Set ball = gBOT(b)
		bz = ball.z
		bvz = ball.VelZ

		If BallVel(ball) > 1 And bz < 30 Then
			rolling(b) = True
			PlaySound BallRollStr(b), -1, VolPlayfieldRoll(ball) * BallRollVolume * VolumeDial, AudioPan(ball), 0, PitchPlayfieldRoll(ball), 1, 0, AudioFade(ball)
		Else
			If rolling(b) = True Then
				StopSound BallRollStr(b)
				rolling(b) = False
			End If
		End If

		If bvz < -1 And bz < 55 And bz > 27 Then
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If bvz > -7 Then
					RandomSoundBallBouncePlayfieldSoft ball
				Else
					RandomSoundBallBouncePlayfieldHard ball
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

'***************************************************************
'  ZABS: Ambient ball shadows
'***************************************************************

'Ambient (Room light source)
Const AmbientBSFactor = 0.9    '0 To 1, higher is darker
Const AmbientMovement = 1	   '1+ higher means more movement as the ball moves left and right
Const offsetX = 0			   'Offset x position under ball (These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY = 0			   'Offset y position under ball (^^for example 5,5 if the light is in the back left corner)

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!  (will throw errors if there aren't enough objects)
Dim objBallShadow(5)

'Initialization
BSInit

Sub BSInit()
	Dim iii
	'Prepare the shadow objects before play begins
	For iii = 0 To tnob - 1
		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = 1 + iii / 1000 + 1.04
		objBallShadow(iii).visible = 0
	Next
End Sub


Sub BSUpdate
	Dim s, bx, by, bz, ubGBOT
	ubGBOT = UBound(gBOT)

	For s = 0 To ubGBOT
		bx = gBOT(s).X
		by = gBOT(s).Y
		bz = gBOT(s).Z

		If bz > 20 And bz < 30 Then
			objBallShadow(s).visible = 1  ' OPT Rule6: direct write; read-guard costs same as write in VPX COM
			objBallShadow(s).X = bx + (bx - BS_halfTable) / BS_ambientDiv + offsetX
			objBallShadow(s).Y = by + offsetY
			objBallShadow(s).Z = bz + s / 1000 + 1.04 - 25
		Else
			objBallShadow(s).visible = 0  ' OPT Rule6: direct write
		End If
	Next
End Sub


'******************************************************
' 	ZRRL: RAMP ROLLING SFX
'******************************************************

'	  * WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'	  * To stop tracking ball call WireRampOff

'*******************************************
'  Ramp Triggers
'*******************************************
Sub RampTrig1_hit 'Plunger Ramp Start
	WireRampOn True
End Sub

Sub RampTrig1_unhit
	If activeball.vely > 0 Then
		WireRampOff
	End If
End Sub

Sub RampTrig2_unhit  'Plunger Ramp End
	If activeball.vely < 0 Then
		WireRampOff 
	End If
End Sub

Sub RampTrig4_hit 'Captive Ball
	WireRampOff 
End Sub

Sub RampTrig4_unhit
	WireRampOn True 
End Sub

Sub RampEntranceTrigger_hit 'Boat Ramp Entrance
	If activeball.vely < 0 Then
		RandomSoundRampFlapUp()
		WireRampOn True
	Else
		RandomSoundRampFlapDown()
	End If
End Sub

Sub RampEntranceTrigger_unhit
	If activeball.vely > 0 Then
		WireRampOff
	End If
End Sub

Sub RampExitL_unhit
	WireRampOff
	RandomSoundRampStop RampExitL
End Sub

Sub RampExitR_unhit
	WireRampOff
	RandomSoundRampStop RampExitR
End Sub

Sub RampEntranceL_hit 'Catapult Ramp
	WireRampOn False
End Sub

Sub RampExitTop_unhit
	WireRampOff
End Sub

Sub RampEntranceCC_hit 'Caster Club Ramp
	WireRampOn False
End Sub

Sub RampExitCC_unhit
	WireRampOff
End Sub

Sub sw32_unhit
	PlaySoundAtLevelStatic ("TOM_C_Ramp_6_Improved2"), RightRampSoundLevel, sw32
	RandomSoundMetal
	WireRampOff
	WireRampOn False
End Sub

Sub sw33_unhit
	PlaySoundAtLevelStatic ("TOM_C_Ramp_5_Improved2"), RightRampSoundLevel, sw33
	RandomSoundMetal
	WireRampOff
	WireRampOn False
End Sub

Sub sw42a_unhit
	PlaySoundAtLevelStatic ("TOM_C_Ramp_6_Improved2"), RightRampSoundLevel, sw42a
	RandomSoundMetal
End Sub

Sub sw43a_unhit
	PlaySoundAtLevelStatic ("TOM_C_Ramp_5_Improved2"), RightRampSoundLevel, sw43a
	RandomSoundMetal
End Sub


'/////////////////////////////  RAMP COLLISIONS  ////////////////////////////

dim RRHit1_volume, RRHit2_volume, RRHit3_volume, RRHit4_volume, RRHit5_volume, RRHit6_volume

Dim RightRampSoundLevel:Dim FlapSoundLevel

'///////////////////////-----Ramps-----///////////////////////
'///////////////////////-----Plastic Ramps-----///////////////////////
RightRampSoundLevel = 0.1												'volume level; range [0, 1]

'///////////////////////-----Ramp Flaps-----///////////////////////
FlapSoundLevel = 0.8													'volume level; range [0, 1]

'/////////////////////////////  PLASTIC RIGHT RAMP SOUNDS  ////////////////////////////
sub RRHit1_Hit()
	RRHit1_volume = RightRampSoundLevel
	RRHit1.TimerInterval = 5
	RRHit1.TimerEnabled = 1
	PlaySoundAtLevelStatic ("TOM_C_Ramp_1_Improved2"), RRHit1_volume, RRHit1
end Sub

Sub RRHit2_Hit()
	RRHit2_volume = RightRampSoundLevel
	RRHit2.TimerInterval = 5
	RRHit2.TimerEnabled = 1
	PlaySoundAtLevelStatic ("TOM_C_Ramp_2_Improved2"), RRHit2_volume, RRHit2
End Sub

Sub RRHit3_Hit()
	RRHit3_volume = RightRampSoundLevel
	RRHit3.TimerInterval = 5
	RRHit3.TimerEnabled = 1
	PlaySoundAtLevelStatic ("TOM_C_Ramp_3_Improved2"), RRHit3_volume, RRHit3
End Sub

Sub RRHit4_Hit()
	RRHit4_volume = RightRampSoundLevel
	RRHit4.TimerInterval = 5
	RRHit4.TimerEnabled = 1
	PlaySoundAtLevelStatic ("TOM_C_Ramp_4_Improved2"), RRHit4_volume, RRHit4
End Sub


'/////////////////////////////  PLASTIC RIGHT RAMP SOUND TIMERS  ////////////////////////////
Sub RRHit1_Timer()
'	debug.print "1: " & RRHit1_volume
	If RRHit1_volume > 0 Then
		RRHit1_volume = RRHit1_volume - 0.05
		PlaySoundAtLevelExistingStatic ("TOM_C_Ramp_1_Improved2"), RRHit1_volume, RRHit1
	Else
		Me.TimerEnabled = 0
	End If
End Sub

Sub RRHit2_Timer()
'	debug.print "2: " & RRHit2_volume
	If RRHit2_volume > 0 Then
		RRHit2_volume = RRHit2_volume - 0.05
		PlaySoundAtLevelExistingStatic ("TOM_C_Ramp_2_Improved2"), RRHit2_volume, RRHit2
	Else
		Me.TimerEnabled = 0
	End If
End Sub

Sub RRHit3_Timer()
'	debug.print "3: " & RRHit3_volume
	If RRHit3_volume > 0 Then
		RRHit3_volume = RRHit3_volume - 0.05
		PlaySoundAtLevelExistingStatic ("TOM_C_Ramp_3_Improved2"), RRHit3_volume, RRHit3
	Else
		Me.TimerEnabled = 0
	End If
End Sub

Sub RRHit4_Timer()
'	debug.print "4: " & RRHit4_volume
	If RRHit4_volume > 0 Then
		RRHit4_volume = RRHit4_volume - 0.05
		PlaySoundAtLevelExistingStatic ("TOM_C_Ramp_4_Improved2"), RRHit4_volume, RRHit4
	Else
		Me.TimerEnabled = 0
	End If
End Sub

'/////////////////////////////  PLASTIC RAMPS FLAPS - SOUNDS  ////////////////////////////
Sub RandomSoundRampFlapUp()
'	debug.print "flap up"
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelActiveBall ("TOM_Ramp_Flap_Up_1"), FlapSoundLevel
		Case 2 : PlaySoundAtLevelActiveBall ("TOM_Ramp_Flap_Up_2"), FlapSoundLevel
		Case 3 : PlaySoundAtLevelActiveBall ("TOM_Ramp_Flap_Up_3"), FlapSoundLevel
	End Select
End Sub

Sub RandomSoundRampFlapDown()
'	debug.print "flap down"	
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelActiveBall ("TOM_Ramp_Flap_Down_1"), FlapSoundLevel
		Case 2 : PlaySoundAtLevelActiveBall ("TOM_Ramp_Flap_Down_2"), FlapSoundLevel
		Case 3 : PlaySoundAtLevelActiveBall ("TOM_Ramp_Flap_Down_3"), FlapSoundLevel
	End Select
End Sub

'*******************************************
'  End Ramp Triggers
'*******************************************

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
	Dim ballcount
	ballcount = 0
	Dim x
	For x = 1 To UBound(RampBalls)
		If ID = RampBalls(x, 1) Then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound RampLoopStr(x)
			StopSound WireLoopStr(x)
		End If
		If Not IsEmpty(Rampballs(x,1)) Then ballcount = ballcount + 1
	Next
	If BallCount = 0 Then RampBalls(0,0) = False
End Sub

Sub RampRoll_Timer()
	RampRollUpdate
End Sub

Sub RampRollUpdate()	'Timer update
	Dim x, rBall, rlStr, wlStr
	For x = 1 To UBound(RampBalls)
		rlStr = RampLoopStr(x)
		wlStr = WireLoopStr(x)
		If Not IsEmpty(RampBalls(x,1)) Then
			Set rBall = RampBalls(x,0)
			If BallVel(rBall) > 1 Then
				If RampType(x) Then
					PlaySound rlStr, -1, VolPlayfieldRoll(rBall) * RampRollVolume * VolumeDial, AudioPan(rBall), 0, BallPitchV(rBall), 1, 0, AudioFade(rBall)
					StopSound wlStr
				Else
					StopSound rlStr
					PlaySound wlStr, -1, VolPlayfieldRoll(rBall) * RampRollVolume * VolumeDial, AudioPan(rBall), 0, BallPitch(rBall), 1, 0, AudioFade(rBall)
				End If
				RampBalls(x, 2) = RampBalls(x, 2) + 1
			Else
				StopSound rlStr
				StopSound wlStr
			End If
			If rBall.Z < 30 And RampBalls(x, 2) > RampMinLoops Then
				StopSound rlStr
				StopSound wlStr
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound rlStr
			StopSound wlStr
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


'******************************************************
' 	ZSFX:  MECHANICAL SOUNDS EFFECTS (FLEEP)
'******************************************************

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
SpinnerSoundLevel = 0.5			 'volume level; range [0, 1]

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
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, - 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
	PlaySound playsoundparams, - 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlaySoundAt(soundname, tableobj)
	PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
	PlaySound soundname, 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
	PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
	PlaySound soundname, 1,min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	PlaySound soundname, 1,min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
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
	
	If tmp > 0 Then  ' OPT Rule1: ^10→squaring chain
		Dim at,t2,t4,t8 : at=tmp  : t2=at*at : t4=t2*t2 : t8=t4*t4 : AudioFade = CSng(t8*t2)
	Else
		at=-tmp : t2=at*at : t4=t2*t2 : t8=t4*t4 : AudioFade = CSng(-(t8*t2))
	End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
	Dim tmp, at, t2, t4, t8
	tmp = tableobj.x * 2 / tablewidth - 1
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp < -7000 Then
		tmp = -7000
	End If
	If tmp > 0 Then  ' OPT Rule1: ^10→squaring chain
		at=tmp  : t2=at*at : t4=t2*t2 : t8=t4*t4 : AudioPan = CSng(t8*t2)
	Else
		at=-tmp : t2=at*at : t4=t2*t2 : t8=t4*t4 : AudioPan = CSng(-(t8*t2))
	End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Dim vbv : vbv = BallVel(ball) : Vol = CSng(vbv*vbv)  ' OPT Rule1
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Dim vvz : vvz = ball.velz : Volz = CSng(vvz*vvz)  ' OPT Rule1
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
	Dim bvvx,bvvy : bvvx=ball.VelX : bvvy=ball.VelY : BallVel = Int(Sqr(bvvx*bvvx + bvvy*bvvy))  ' OPT Rule1
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	Dim vpbv : vpbv = BallVel(ball) : VolPlayfieldRoll = RollingSoundFactor * 0.0005 * CSng(vpbv*vpbv*vpbv)  ' OPT Rule1
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	Dim ppbv : ppbv = BallVel(ball) : PitchPlayfieldRoll = ppbv*ppbv * 15  ' OPT Rule1
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

'/////////////////////////////  COIN IN  ////////////////////////////

Sub SoundCoinIn
	Select Case Int(Rnd * 3)
		Case 0
			PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
		Case 1
			PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
		Case 2
			PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
	End Select
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

'///////////////////////////////  OUTHOLE SOUNDS  ///////////////////////////////
Dim Solenoid_OutholeKicker_SoundLevel:Solenoid_OutholeKicker_SoundLevel = 1

Sub RandomSoundOutholeKicker(obj)
	PlaySoundAtLevelStatic SoundFX("Outhole_Kicker_" & Int(Rnd*4)+1,DOFContactors), Solenoid_OutholeKicker_SoundLevel, obj
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
End Sub

Sub GatesWire_hit(idx)
	SoundPlayfieldGate
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

Sub SoundCatKick(saucer)
	PlaySoundAtLevelStatic SoundFX("Catapult_Fire", DOFContactors), SaucerKickSoundLevel, saucer
End Sub

Sub SoundVUKKick(saucer)
	PlaySoundAtLevelStatic SoundFX(("VUK_Exit_" & Int(Rnd * 2) + 1), DOFContactors), SaucerKickSoundLevel, saucer
End Sub


'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////

Sub OnBallBallCollision(ball1, ball2, velocity)

	FlipperCradleCollision ball1, ball2, velocity

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
	
	Dim cvel : cvel = CSng(velocity) : PlaySound (snd), 0, cvel*cvel / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)  ' OPT Rule1
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

'/////////////////////////////  DIVERTER SOLENOID SOUNDS  ////////////////////////////
Dim DiverterSolenoidSoundLevel
DiverterSolenoidSoundLevel = 3

Sub SoundDiverterSolenoid(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic SoundFX("Diverter_UP_2",DOFContactors), DiverterSolenoidSoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic SoundFX("Diverter_DOWN_2",DOFContactors), DiverterSolenoidSoundLevel, obj
	End Select
End Sub

'/////////////////////////////  MOTOR  ////////////////////////////
Dim MotorSoundLevel:MotorSoundLevel = 0.3

Sub SoundMotor(toggle,obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStaticLoop SoundFX("Motor_Long",DOFGear), MotorSoundLevel, obj
		Case 0
			StopSound "Motor_Long"
	End Select
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
' 	ZVRR:  VR Room & Animations (Rawd & DaRdog81)
'******************************************************
'VR Room code and animations - Rawd
'Animation work: DaRdog81 

Dim ShowVR
Dim Thing
Dim TurtleheightSpeed:  TurtleheightSpeed = 0.5
Dim FireAnim: FireAnim = 1
Dim FishmanMove: FishmanMove = false
Dim BatFlying: BatFlying = false
Dim SmokeAnim: SmokeAnim = 1
Dim ShootingStar: ShootingStar = 1
Dim CrabMove: CrabMove = 1
Dim FireSizeSpeed : FireSizeSpeed = 0.02
Randomize 

'Initialize VR Room ****************************************************************************************************
Sub SetVRRoom
	Dim showFishingScene

	If RenderingMode = 2 or (DesktopMode and VRDesktop = 1) Then
		ShowVR = True
	Else
		ShowVR = False
	End If

	showFishingScene = (VRRoom = 1 and ShowVR)

	'Topper
	For each thing in VRTopper: Thing.Visible = Topper and showvr: Next

	'Cabinet
	For each thing in VRCabinet: Thing.Visible = ShowVR: Next

	If DesktopMode = True and Not ShowVR Then 'Show Desktop components
		For Each Thing in BP_RailsLockdown : Thing.Visible = 1: Next
	Else
		For Each Thing in BP_RailsLockdown : Thing.Visible = 0: Next
	End if

	'Backglass
	SetBackglass
	For each thing in VRbackglass: Thing.Visible = ShowVR: Next		
	For each thing in VRbackglassLow: Thing.Visible = ShowVR: Next	

	'VR Room
	If showFishingScene Then 
		InitAnimations
		aWater1.PlayLoop
		aWater2.PlayLoop
		aWater3.PlayLoop
		aTrout.PlayLoop
		aTrout2.PlayLoop
		aFish.PlayLoop
		aFishFins.PlayLoop
		aTurtle.PlayLoop
		aDog.PlayLoop
	Else
		InitAnimations
		aWater1.StopAnim
		aWater2.StopAnim
		aWater3.StopAnim
		aTrout.StopAnim
		aTrout2.StopAnim
		aFish.StopAnim
		aFishFins.StopAnim
		aTurtle.StopAnim
		aDog.StopAnim		
	End If

	For each thing in VRDock: Thing.Visible = VRRoom and ShowVR: Next
	' The fishing scene was only gated by the VRDock collection, which is brittle if
	' table objects are moved out of that collection. Force the animated room actors
	' to follow the same visibility state so they cannot keep rendering while hidden.
	For each thing in Array(NewWater, NewWater2, FarWaterNew, VR_Trout, VR_Trout2, NewFish, NewFishFins, Fish2, VR_TurtleNew, VR_Dog, VRBat, VR_Fisherman, VR_Crab, CrabShadow, VRStar000, VRGenLightRed, VRGenLightGreen, VRPorchlight, Moon)
		thing.Visible = showFishingScene
	Next
	ShootingStarTimer.enabled = VRRoom and ShowVR
	SmokeTimer.enabled = VRRoom and ShowVR
	TroutTimer.enabled = VRRoom and ShowVR
	FishermanTimer.enabled = VRRoom and ShowVR
	BatTimer.enabled = VRRoom and ShowVR
	CrabTimer.enabled = VRRoom and ShowVR
	GenTimer.enabled = VRRoom and ShowVR

	VRPorchlight.blenddisablelighting = 300
	Moon.blenddisablelighting = 10
	VR_coininsertsOpen.blenddisablelighting = 5
	Fishtales.Bloomstrength = 0.05
	VRStar000.blenddisablelighting = 50000
	VRGenLightRed.blenddisablelighting = 0
	VRGenLightGreen.blenddisablelighting = 80

	'Ambient Sounds
	If VRRoom = 1 and AmbientSound > 0 and ShowVR Then 
		playsound "Water", -1, 0.1
		If AmbientSound = 2 Then
			playsound "generatorloop", -1, 0.35
		Else
			Stopsound "generatorloop"
		End If
	Else
		Stopsound "Water"
		Stopsound "generatorloop"
	End If
End Sub

'Set Up VR Backglass
Dim BGSet : BGSet = 0

Sub SetBackglass()
	Dim obj

	If BGSet = 0 Then
		For Each obj In VRBackglass
			obj.x = obj.x
			obj.height = - obj.y + 1210
			obj.y = 15
			obj.rotX = -86.5
		Next
		For Each obj In VRBackglassLow
			obj.x = obj.x
			obj.height = - obj.y + 1210
			obj.y = 25
			obj.rotX = -86.5
		Next
		BGSet = 1
	End If
End Sub

'*************************************************************************************************************************

Sub StartButtonLighting
	Dim curState
	curState = L88.state
	If curState <> lastL88State Then
		lastL88State = curState
		If curState = 1 Then
			Primary_StartButton.DisableLighting = 1
			StartButton2.DisableLighting = 1
		Else
			Primary_StartButton.DisableLighting = 0
			StartButton2.DisableLighting = 0
		End If
	End If
End Sub

Sub TroutTimer_Timer ' Runs all the fish and turtle in circles
	VR_Trout.roty = VR_Trout.roty + 0.1
	VR_Trout2.roty = VR_Trout2.roty - 0.15
	NewFish.rotz = NewFish.rotz + 0.06
	NewFishFins.rotz = NewFishFins.rotz + 0.06
	Fish2.roty = Fish2.roty - 0.05
    VR_TurtleNew.roty = VR_TurtleNew.roty + 0.15
	VR_TurtleNew.z = VR_TurtleNew.z + TurtleheightSpeed
	if VR_TurtleNew.z => -6550 then TurtleheightSpeed = -0.5
	if VR_TurtleNew.z =< -7900 then TurtleheightSpeed = +0.5
End Sub

' Shooting Star
Sub ShootingStarTimer_timer()
    If ShootingStar = 0 then VRStar000.visible = True: ShootingStarTimer.interval = 21  ' sets star Visible and speed of animation
	ShootingStar = ShootingStar + 1  ' Adds 1 to our variable to animate our images in order below
	If ShootingStar => 32 Then  ' If we reach the end of the animation then do the following.... **********************************************************************************************
		ShootingStar = 0  ' Reset our variable back to 0
		VRStar000.visible = False  ' Make the star invisible
		ShootingStarTimer.interval= 10000 + rnd(1)*20000  ' Wait time between stars random number (10 to 30 seconds)		
		' Choose randomly between 3 colours....
		Select Case Int(rnd*3) 
			Case 0: VRStar000.color = rgb(255,255,255) 'White
			Case 1: VRStar000.color = rgb(30,225,35) ' Green  
			Case 2: VRStar000.color = rgb(250,0,250) ' Purple  
		End Select

		'Choose randomly between 3 positions
		Select Case Int(rnd*3) 
			Case 0: VRStar000.x = 132480: VRStar000.y = -147086: VRStar000.z = 5674: VRStar000.rotx = -26: VRStar000.roty = 18: VRStar000.rotz = 56     'star Position 1
			Case 1: VRStar000.x = -51815: VRStar000.y = -223460: VRStar000.z = -11675: VRStar000.rotx = -26: VRStar000.roty = 18: VRStar000.rotz = 156  'star Position 2
			Case 2: VRStar000.x = 70355: VRStar000.y = -215730: VRStar000.z = -11675: VRStar000.rotx = -26: VRStar000.roty = 18: VRStar000.rotz = 156   'star Position 3
		End Select
		Exit Sub ' We want to exit the sub, because We hit the end of our animation and we set everything for the next animation, and we don't want the image swap called for now.
	end If  'End of our If statement. *********************************************************************************************************************************************************
	VRStar000.image = ShootingStarStr(ShootingStar)
End Sub


' Smoke and Fire
Sub SmokeTimer_timer()
	SmokeAnim = SmokeAnim + 1
	If SmokeAnim >= 61 Then SmokeAnim = 1
	Dim sStr
	sStr = SmokeStr(SmokeAnim)
	VRSmoke000.image = sStr
	VRSmoke001.image = sStr

	FireAnim = FireAnim + 1
	If FireAnim >= 63 Then FireAnim = 1
	VRFire000.image = FireStr(FireAnim)

	' Fire Sizing.... 
	VRFire000.size_z = VRFire000.size_z + FireSizeSpeed
	if VRFire000.size_z > 5 then FireSizeSpeed = - 0.02
	if VRFire000.size_z < 2.6 then FireSizeSpeed =  0.02

	'Fire random brightness....
	Select Case Int(rnd*4) 
			Case 0: VRFire000.blenddisablelighting = 12
			Case 1: VRFire000.blenddisablelighting = 14
			Case 2: VRFire000.blenddisablelighting = 17
			Case 3: VRFire000.blenddisablelighting = 20
		End Select
End Sub


Sub FishermanTimer_Timer()
	If FishmanMove = false then 
		aMan.PlayLoop
		FishermanTimer.interval = 2000 + rnd(1)*7000  ' random number between 2000 and 9000 ( 2 to 9 seconds)
		FishmanMove = True
		Exit Sub
	end If

	if FishmanMove = true then
		aMan.StopAnim()
		FishermanTimer.interval = 5000 + rnd(1)*10000  ' random number between 5000 and 15000 ( 5 to 15 seconds)
		FishmanMove = False
		Exit sub
	end If
End Sub


Sub BatTimer_timer()
	BatTimer.interval = 11

	if VRBat.ObjRotZ < 360 then 
		VRBat.ObjRotZ=VRBat.ObjRotZ+.5
		if BatFlying = false then 
			aBat.PlayLoop
			BatFlying = true
		End if
	end if

	if VRBat.ObjRotZ=> 360 Then
		Batflying = False
		aBat.StopAnim()
		VRBat.ObjRotZ = 1
		BatTimer.interval = 45000 + rnd(1)*40000  ' random number between  ( 45 to 85 seconds)
		Exit sub
	end If
end Sub



'Topper Lights and fish animation Code ********************************************
Sub topperfish(Enabled)
	If Enabled then 
		If topper = 1 and ShowVR Then
			TopperFishModel.PlayAnimEndless(0.3)
			TopperFishFins.PlayAnimEndless(0.3)
			PlaySoundAtLevelStatic "hop2", 1, TopperFishModel
		End If
	Else 
		TopperFishModel.StopAnim()
		TopperFishFins.StopAnim()
	End If
End Sub
'End Topper Code ************************************************


' VR Generator Light...
Sub GenTimer_Timer()
	if VRGenLightRed.blenddisablelighting = 0 then 
		VRGenLightRed.blenddisablelighting = 80
		VRGenLightGreen.blenddisablelighting = 0
	else 
		VRGenLightRed.blenddisablelighting = 0
		VRGenLightGreen.blenddisablelighting = 80
	end if
End Sub


' VR Crab animation... 
Sub CrabTimer_Timer()

	CrabTimer.Interval = 11

	If CrabMove = 1 and VR_Crab.y > -1500 then  ' move crab forward and sideways towards front of dock
		aCrab.PlayLoop() 
		VR_Crab.color = rgb(19,1,2)
		CrabShadow.visible = True
		VR_Crab.visible = True
		VR_Crab.y = VR_Crab.y -6 
		VR_Crab.z = VR_Crab.z -0.6 
		VR_Crab.x = VR_Crab.x +6
		CrabShadow.y = CrabShadow.y -6	
		CrabShadow.height = CrabShadow.height -0.6
		CrabShadow.x = CrabShadow.x +6
		if VR_Crab.y =< -1500 then CrabMove = 2  ' Stop crab at edge of dock
	end If

	If CrabMove = 2 and VR_Crab.x < -800 then  ' start turning crab as hes moving perpendicular with the dock edge
		CrabSpeed = 600 ' slow carb legs animation
		InitAnimations  ' needed after a speed change to re-initialize Animations
		VR_Crab.x = VR_Crab.x +6
		VR_Crab.roty = VR_Crab.roty +0.22	
		CrabShadow.x = CrabShadow.x +6
		if VR_Crab.x => -800 then CrabMove = 3: CrabTimer.Interval = 8000: CrabAn = 2: aCrab.Pause(): InitAnimations : aCrab.ResumeAnim():  exit sub  ' we reach 90 degrees and where we wnt to end. stop, and change timer to 5 seconds before next move
	end if

	If CrabMove = 3 and VR_Crab.y < 6000 then ' Run crab past the pinball machine until it hits the planter
		CrabAn = 1
		CrabSpeed = 300  ' move legs faster
		InitAnimations  ' needed after a speed change to re-initialize Animations
		aCrab.PlayLoop() 
		VR_Crab.y = VR_Crab.y +12
		VR_Crab.z = VR_Crab.z +1.2 
		CrabShadow.y = CrabShadow.y +12
		CrabShadow.height = CrabShadow.height +1.22 	
		if VR_Crab.y => 4300 then VR_Crab.color = rgb(8,1,2)  'darken him a bit in the shadow of the planter
		if VR_Crab.y => 6000 then CrabMove = 4: CrabTimer.Interval = 8000: aCrab.Pause():  exit sub  'stop him ant planter and wait 8 seconds
	end If

	if Crabmove = 4 then ' slowly turn him to face the side of dock
		CrabSpeed = 1200  'slow legs
		InitAnimations  ' needed after a speed change to re-initialize Animations
		aCrab.PlayLoop() 
		VR_Crab.roty = VR_Crab.roty +0.22
		if VR_Crab.roty > 630 then CrabMove = 5: CrabTimer.Interval = 5000: aCrab.Pause():  exit sub
	end if

	if Crabmove = 5 then ' run towards side dock edge
		CrabSpeed = 300
		InitAnimations  ' needed after a speed change to re-initialize Animations
		aCrab.PlayLoop() 
		VR_Crab.x = VR_Crab.x +12
		CrabShadow.x = CrabShadow.x +12
		if VR_Crab.x => 2350 then Crabmove = 6
	End If

	If CrabMove = 6 then 'jump
		CrabShadow.visible = false
		VR_Crab.x = VR_Crab.x +22
		VR_Crab.z = VR_Crab.z +12
		If VR_Crab.x => 2500 then CrabMove = 7
	end If

	If CrabMove = 7 then 'fall
		VR_Crab.x = VR_Crab.x +6
		VR_Crab.z = VR_Crab.z -22
		If VR_Crab.z =< -4100 then VR_Crab.color = rgb(35,50,11)  ' Brighten him up and add some green as he hits the water
		If VR_Crab.z =< -5500 then CrabMove = 8
	end If

	If CrabMove = 8 then 'turn and run under dock underwater
		VR_Crab.roty = VR_Crab.roty -0.5
		if VR_Crab.roty =< 490 then VR_Crab.x = VR_Crab.x -8
		if VR_Crab.x < 1000 then Crabmove = 9
	End if 

	If Crabmove = 9 then ' reset crab and timer for next round
		aCrab.Pause()
		VR_Crab.color = rgb(19,1,2)
		VR_Crab.x = -10684
		VR_Crab.y = 4296
		VR_Crab.z = -1365
		VR_Crab.roty = 205
		CrabShadow.x = -10663
		CrabShadow.y =  4315
		CrabShadow.Height = -1345	
		VR_Crab.visible = false
		CrabShadow.visible = false
		CrabTimer.Interval = 50000   ' 50 seconds
		Crabmove = 10
		exit sub
	End If

	If Crabmove = 10 and VR_Crab.y > 500 then  ' move crab forward and sideways towards front of dock
		aCrab.PlayLoop() 
		VR_Crab.color = rgb(19,1,2)
		CrabShadow.visible = True
		VR_Crab.visible = True
		VR_Crab.y = VR_Crab.y -6 
		VR_Crab.z = VR_Crab.z -0.6 
		VR_Crab.x = VR_Crab.x +6
		CrabShadow.y = CrabShadow.y -6	
		CrabShadow.height = CrabShadow.height -0.6
		CrabShadow.x = CrabShadow.x +6
		if VR_Crab.y =< 500 then CrabMove = 11  ' Stop crab in middle of porch
	End If

	If Crabmove = 11 then 
		VR_Crab.x = VR_Crab.x +6
		CrabShadow.x = CrabShadow.x +6
		VR_Crab.roty = VR_Crab.roty +0.22	
		if VR_Crab.x => 0 then  Crabmove = 12:	CrabSpeed = 1200 :InitAnimations  ' needed after a speed change to re-initialize Animations	
	End If

	If Crabmove = 12 then 
		VR_Crab.x = VR_Crab.x +0.01
		CrabShadow.x = CrabShadow.x +0.01
		if VR_Crab.x => 12 then  Crabmove = 13: CrabSpeed = 300 :InitAnimations
	End If

	If Crabmove = 13 then 
		VR_Crab.x = VR_Crab.x -9
		CrabShadow.x = CrabShadow.x -9
		VR_Crab.y = VR_Crab.y -1.1
		CrabShadow.y = CrabShadow.y -1.1
		VR_Crab.Z = VR_Crab.Z -.1
		CrabShadow.height = CrabShadow.height -.1
		if VR_Crab.x =< -10300 then 	
			VR_Crab.Z = VR_Crab.Z -14
			Crabshadow.visible = false
		End if
		if VR_Crab.x =< -11500 then Crabmove = 14		
	end If

	If Crabmove = 14 then ' reset crab and timer for next round
		aCrab.Pause()
		VR_Crab.color = rgb(19,1,2)
		VR_Crab.x = -10684
		VR_Crab.y = 4296
		VR_Crab.z = -1365
		VR_Crab.roty = 205
		CrabShadow.x = -10663
		CrabShadow.y =  4315
		CrabShadow.Height = -1345	
		VR_Crab.visible = false
		CrabTimer.Interval = 50000   ' 50 seconds
		Crabmove = 1
	End If
End Sub

'End Crab Timer **************************************************************************************************************************

' Keyframe Animation ver 2 by NFOZZY   ver0.00002
' Interpolation math adapted from "Smooth interpolation of irregularly spaced keyframes" by Jon Langridge http://archive.gamedev.net/archive/reference/articles/article1497.html 

' Prerequisites: 
'   FRAMETIME global variable. Calculated like this: FrameTime = gametime - InitFrame : InitFrame = GameTime

Sub UpdateAnimations 'Called from Frametimer
	aTrout.Update
	aTrout2.Update
	aWater1.Update
	aWater2.Update
	aWater3.Update
	aFish.Update
	aFishFins.Update
	aTurtle.Update
	aDog.Update
	aBat.Update
	aMan.Update
	aCrab.Update
End Sub

' SETUP:
'   .Update()                  									- Updating on a timer is Required. For best results use the same timer that does the frametime calculation
'   .AddPoint keyframe number, Total Time in MS, Value          - defines a keyframe. Keyframe numbers must start at 0 and must be defined in order!
'   .AddPoint2 keyframe number, Time this frame lingers, Value  - alternative version of the above where keyframe time reflects how long the current frame is (experimental, keep position 0 at x=0)
'   .CallBack(string)           								- Defines the subroutine to be called. Called with one value, this will have to be plugged into whatever you're animating. Must be in quotes (ex: .CallBack("animSlingshot") ) 
'   .InterpolationType = [number between 0-2]      				- define interpolation type. 0 = linear, 1 = Zero-Gradient Cubic (default), 2 = Catmull-Rom Spline Interpolation

dim aWater1 : Set aWater1 = New cAnimation2
dim aWater2 : Set aWater2 = New cAnimation2
dim aWater3 : Set aWater3 = New cAnimation2
dim aTrout : Set aTrout = New cAnimation2
dim aTrout2 : Set aTrout2 = New cAnimation2
dim aFish : Set aFish = New cAnimation2
dim aFishFins : Set aFishFins = New cAnimation2
dim aTurtle : Set aTurtle = New cAnimation2
dim aDog : Set aDog = New cAnimation2
dim aBat : Set aBat = New cAnimation2
dim aMan : Set aMan = New cAnimation2
dim aCrab : Set aCrab = New cAnimation2
Dim CrabSpeed: CrabSpeed = 300
dim CrabAn: CrabAn = 1

Sub InitAnimations() ' animations plot can be printed in debugger with obj.TEST() method

	With aWater1 ' Blue main water
		.InterpolationType=0 	' Catmull-Rom Spline Interpolation
		.AddPoint 0, 0, 0 		' addpoint2 specifies x=duration of current frame, rather than the raw MS of the animation
		.AddPoint 1, 25500, 91  '11500
		.Callback= "animWater"
	End With

	With aWater2 ' Green main water (lower)
		.InterpolationType=0 	' Catmull-Rom Spline Interpolation
		.AddPoint 0, 0, 0 		' addpoint2 specifies x=duration of current frame, rather than the raw MS of the animation
		.AddPoint 1, 25500, 91  '11500
		.Callback= "animWater2"
	End With

	With aWater3 ' Far water
		.InterpolationType=0 	' Catmull-Rom Spline Interpolation
		.AddPoint 0, 0, 0 		' addpoint2 specifies x=duration of current frame, rather than the raw MS of the animation
		.AddPoint 1, 59000, 91  '11500
		.Callback= "animWater3"
	End With

	With aTrout
		.InterpolationType=2 	' Catmull-Rom Spline Interpolation
		.AddPoint 0, 0, 0 		' addpoint2 specifies x=duration of current frame, rather than the raw MS of the animation
		.AddPoint 1, 1800, 18
		.AddPoint 2, 3600, 0
		.Callback= "animTrout"
	End With

	With aTrout2
		.InterpolationType=2 	' Catmull-Rom Spline Interpolation
		.AddPoint 0, 0, 0 		' addpoint2 specifies x=duration of current frame, rather than the raw MS of the animation
		.AddPoint 1, 2000, 18
		.AddPoint 2, 4000, 0
		.Callback= "animTrout2"
	End With

	With aFish
		.InterpolationType=2 	' Catmull-Rom Spline Interpolation
		.AddPoint 0, 0, 0 		' addpoint2 specifies x=duration of current frame, rather than the raw MS of the animation
		.AddPoint 1, 3000, 9 
		.Callback= "animFish"
	End With

	With aFishFins
		.InterpolationType=2 	' Catmull-Rom Spline Interpolation
		.AddPoint 0, 0, 0 		' addpoint2 specifies x=duration of current frame, rather than the raw MS of the animation
		.AddPoint 1, 3000, 9 
		.Callback= "animFishFins"
	End With

	With aTurtle ' Turtle
		.InterpolationType=0 	' Catmull-Rom Spline Interpolation
		.AddPoint 0, 0, 0 		' addpoint2 specifies x=duration of current frame, rather than the raw MS of the animation
		.AddPoint 1, 2800, 83  '11500
		.Callback= "animTurtle"
	End With

	With aDog ' Dog
		.InterpolationType=1	' Catmull-Rom Spline Interpolation
		.AddPoint 0, 0, 0 		' addpoint2 specifies x=duration of current frame, rather than the raw MS of the animation
		.AddPoint 1, 2300, 49  '11500
		.Callback= "animDog"
	End With

	With aBat ' Bat
		.InterpolationType=2 	' Catmull-Rom Spline Interpolation
		.AddPoint 0, 0, 0 		' addpoint2 specifies x=duration of current frame, rather than the raw MS of the animation
		.AddPoint 1, 200, 3
		.AddPoint 2, 400, 0
		.Callback= "animBat"
	End With

	With aMan ' Fisherman
		.InterpolationType=2 	' Catmull-Rom Spline Interpolation
		.AddPoint 0, 0, 0 		' addpoint2 specifies x=duration of current frame, rather than the raw MS of the animation
		.AddPoint 1, 2000, 11
		.Callback= "animMan"
	End With

    With aCrab

		If CrabAn = 1 then 
		.InterpolationType=0 	' Catmull-Rom Spline Interpolation
		.AddPoint 0, 0, 0 		' addpoint2 specifies x=duration of current frame, rather than the raw MS of the animation
		.AddPoint 1, CrabSpeed, 09
		.Callback= "animCrab"
		End if

		If CrabAn = 2 then 
		.InterpolationType=2 	' Catmull-Rom Spline Interpolation
		.AddPoint 0, 0, 0 		' addpoint2 specifies x=duration of current frame, rather than the raw MS of the animation
		.AddPoint 1, 2000, 02
		.AddPoint 2, 4000, 0
		.Callback= "animCrab"
		end if

	End With
End Sub

'Animation Callbacks
Sub animWater(Value) : NewWater.ShowFrame Value :  End Sub
Sub animWater2(Value) : NewWater2.ShowFrame Value :  End Sub
Sub animWater3(Value) : FarWaterNew.ShowFrame Value :  End Sub
Sub animTrout(Value) : VR_Trout.ShowFrame Value :  End Sub
Sub animTrout2(Value) : VR_Trout2.ShowFrame Value :  End Sub
Sub animFish(Value) : NewFish.ShowFrame Value :  End Sub
Sub animFishFins(Value) : NewFishFins.ShowFrame Value :  End Sub
Sub animTurtle(Value) : VR_TurtleNew.ShowFrame Value :  End Sub
Sub animDog(Value) : VR_Dog.ShowFrame Value :  End Sub
Sub animBat(Value) : VRBat.ShowFrame Value :  End Sub
Sub animMan(Value) : VR_Fisherman.ShowFrame Value :  End Sub
Sub animCrab(Value) : VR_Crab.ShowFrame Value :  End Sub


' ****  We do not need to touch anything below here for animations **************************************************************

' USAGE: 
'   .Play()                 - Play Once
'   .PlayImmediate()        - Play Once, resetting animation if necessary 
'   .PlayLoop()             - Play Forever. End with .Play() .PlayImmediate() or .Stop()
'   .Pause()                - Pause animation in place
'   .ResumeAnim()           - Resumes a paused animation. Alternatively use .Play() or .PlayLoop()
'   .StopAnim()             - Stop animation and reset to first keyframe
' ... multiple animations ...
'   .PlayFrom [ms integer]  - Play Once, starting at this millisecond
'   .PlayTo [ms integer]    - Play Once, ending at this millisecond
'   .PlayFromTo [start], [end] - Play Once, starting at [start millisecond] and ending at [end millisecond]. If end is 0 it'll work the same as PlayFrom
' ... info (read only) ...
'   .State                  - (property) returns true or false. True means animation is currently running
'   .Pos                    - (property) returns timecode in milliseconds. If not running, it will be 0


' DEBUG: 
'   .timescale = [float number] - multiplies time (default 1). Ex: setting to 0.5 will play at half speed
'   .TEST()                     - Debug window command, will print X and Y data in debugger 

' NOTE:
' Catmull-Rom Spline Interpolation (.InterpolationType=2) is very "wiggly". Make sure the target has room to overtravel!
'  - checking output with .TEST() is hightly recommended when using Spline interpolation!! Copy the values into a plot such as https://www.desmos.com/calculator

class cAnimation2

    private KeyX, KeyY
    private lock, ms, endMS, UpdateSub, UpdateSubRef
    private LoopAnim
    public timescale
    public InterpolationType ' 0 = linear, 1 = Zero-Gradient Cubic (default), 2 = Catmull-Rom Spline

	Private Sub Class_Initialize 
        redim KeyX(0) : redim KeyY(0) : Lock = True : ms = 0 : endMS = 0 : LoopAnim=False : timescale = 1 : InterpolationType = 1
    End Sub

	Public Property Get State : State = not Lock : End Property
	Public Property Get Pos : Pos = ms : End Property
	Public Property Let CallBack(String) : UpdateSub = String : Set UpdateSubRef = GetRef(UpdateSub): End Property

	public Sub AddPoint(aKey, aX, aY)
        redim preserve KeyX(aKey+2) : redim preserve KeyY(aKey+2) ' +2 because the Resamplers are going to need a duplicate index at start and end
        redim tmparrayX(aKey+2)  : redim tmparrayY(aKey+2)

        if aKey = 0 then tmparrayX(0) = aX : tmparrayY(0) = aY ' buffer start for resampler
        aKey = aKey + 1 ' all points are to be offset by +1 interally

        tmparrayX(aKey) = aX : tmparrayY(aKey) = aY
        tmparrayX(aKey+1) = aX : tmparrayY(aKey+1) = aY

        dim i : for i = 0 to uBound(KeyX)
            if not IsEmpty(tmparrayX(i)) then 
                KeyX(i) = tmparrayX(i)
                KeyY(i) = tmparrayY(i)
            end if
        next
        'TestArrays ' debug
	End Sub

    public sub AddPoint2(aKey, aX, aY) ' aX is duration of current frame. Makes adjustments easier
        if aKey = 0 then AddPoint aKey, aX, aY : exit sub : end if
        dim aX2
        aX2 = KeyX(aKey-0) + aX 
        if aX2 < 0 then msgbox "AddPoint2 Error at keyframe " & aKey & ": ms is less than 0! input ms: " & aX & " previous ms:" & KeyX(aKey) : exit sub : end if
        AddPoint aKey, aX2, aY
    end sub

    Public Sub Play() : endMS=0: Lock=False : LoopAnim=False : end sub
    Public Sub PlayFrom(aMS) : endMS=0 : ms=aMS : Lock=False : LoopAnim=False : end sub
    Public Sub PlayTo(aEndMS) : endMS=aMS : Lock=False : LoopAnim=False : end sub
    Public Sub PlayFromTo(aMS, aEndMS) : ms=aMS : endMS=aEndMS : Lock=False : LoopAnim=False : end sub
    Public Sub PlayImmediate() : endMS=0 : Lock=False : ms=0 : LoopAnim=False : end sub ' Play + Reset animation if it's currently playing
    Public Sub PlayLoop() : endMS=0 : Lock=False : LoopAnim=True : end sub
    Public Sub Pause() : Lock=True : end sub
    Public Sub ResumeAnim() : Lock=False : end sub
    Public Sub StopAnim() ' immediate stop and reset back to frame 1
        ms = 0 : endMS=0 : Lock=True
        UpdateSubRef KeyY(0)
    End Sub

    private function LinearInterp(ByRef t, KeyX, KeyY) ' OUTS TIME
        dim u
        dim deltaX ' a subtraction
        dim i ' find current keyframe position, should this be kept in memory?
		if t >= KeyX(uBound(KeyX)) then LinearInterp = KeyY(uBound(KeyY) ) : t = 0 : Lock = Not LoopAnim : exit function end if
        do while t > keyX(i + 1)
            i = i + 1 
        loop
        if i = 0 then i = 1 ' skip the redundant position 1

        deltaX = KeyX(i+1) - KeyX(i)
        if deltaX <> 0 then 
            u = (t - KeyX(i)) / deltaX
        else 
            u = 0
        end if

        if i >= uBound(KeyX)-1 then t = 0 : Lock = Not LoopAnim ' animation is done, reset time to 0

        LinearInterp = KeyY(i) + u * (KeyY(i+1) - KeyY(i))

    end function

    private function CubicInterp(ByRef t, KeyX, KeyY) ' OUTS TIME
        dim u ' keyframe interp, 0-1
        dim deltaX ' a subtraction
		if t >= KeyX(uBound(KeyX)) then CubicInterp = KeyY(uBound(KeyY) ) : t = 0 : Lock = Not LoopAnim : exit function end if
        dim i ' find current keyframe position, should this be kept in memory?
        do while t > keyX(i + 1)
            i = i + 1 
        loop
        if i = 0 then i = 1 ' skip the redundant position 1

        deltaX = KeyX(i+1) - KeyX(i)

        if deltaX <> 0 then 
            u = (t - KeyX(i)) / deltaX
        else 
            u = 0
        end if

        if i >= uBound(KeyX)-1 then t = 0 : Lock = Not LoopAnim ' animation is done, reset time to 0

        CubicInterp = KeyY(i) * (2*u^3 - 3*u^2+1) + KeyY(i+1) * (3*u^2 - 2*u^3) ' cubic
    end function

    private function SplineInterp(ByRef t, KeyX, KeyY) ' Catmull-Rom spline interpolation. OUTS TIME
        dim keygrad1, keygrad2 ' spline gradients
        dim z1, z2 ' variables checked to prevent divide-by-zero errors
        dim deltaX ' a subtraction that is done three times
        dim u ' keyframe interp, 0-1
		if t >= KeyX(uBound(KeyX)) then SplineInterp = KeyY(uBound(KeyY) ) : t = 0 : Lock = Not LoopAnim : exit function end if

        dim i ' find current keyframe position, should this be kept in memory?
        do while t > keyX(i + 1)
            i = i + 1 
        loop
        if i = 0 then i = 1 ' skip the redundant position 1

        deltaX = KeyX(i+1) - KeyX(i)

        if deltaX <> 0 then 
            u = (t - KeyX(i)) / deltaX
        else 
			u = 0 
		end if

        z1 = (KeyX(i) - KeyX(i-1)) : z2 = (KeyX(i) - KeyX(i-1) )
        if z1 <> 0 and z2 <> 0 then ' check div0
            Keygrad1 = 0.5 * (KeyY(i) - KeyY(i-1)) / z1 + 0.5 * (KeyY(i+1) - KeyY(i) ) / z2
        else 
			keygrad1 = 0 
		end if

        z1 = (KeyX(i+1) - KeyX(i)) : z2 = (KeyX(i+1) - KeyX(i) )
        if z1 <> 0 and z2 <> 0 then 
            Keygrad2 = 0.5 * (KeyY(i+1) - KeyY(i)) / z1 + 0.5 * (KeyY(i+2) - KeyY(i+1) ) / z2
        else 
			keygrad2 = 0 
		end if

        if i >= uBound(KeyX)-1 then t = 0 : Lock = Not LoopAnim ' animation is done, reset time to 0

        SplineInterp = KeyY(i) * (2*u^3 - 3*u^2 + 1) +_
                KeyY(i+1) * (3*u^2 - 2 * u^3) +_
                keygrad1 * deltaX * (u^3 - 2*u^2+u) +_
                keygrad2 * deltaX * (u^3 - u^2)
    end function

	Public Sub Update()
		if not lock then
            ms = ms + FrameTime * timescale
            dim lvl
            Select Case InterpolationType
                case 1 : lvl = CubicInterp(ms, KeyX, KeyY) ' these out MS
                case 2 : lvl = SplineInterp(ms, KeyX, KeyY)
                case else : lvl = LinearInterp(ms, KeyX, KeyY)
            end Select
            if endMS > 0 then : if MS >= endMS then lock=true end if : end if
            UpdateSubRef lvl
		end if		
	End Sub

    public sub TEST()
        dim str
        Select Case InterpolationType
            case 1 : str = "Cubic"
            case 2 : str = "Spline"
            case else : str="Linear"
        end Select
        debug.print str
        dim lvl, i, t : for i = 0 to KeyX(uBound(KeyX))
			t = i ' note these out seconds so 'i' here will infinite loop
            Select Case InterpolationType
                case 1 : lvl = CubicInterp(t, KeyX, KeyY) ' these out t
                case 2 : lvl = SplineInterp(t, KeyX, KeyY)
                case else : lvl = LinearInterp(t, KeyX, KeyY)
            end Select
            debug.print i & ", " & Round(lvl,5) ' print X and Y axis
            ' debug.print Round(lvl,5) ' print just Y axis
        next
    end sub
end class

' End Keyframe Animation ver 2 by NFOZZY  *************************************
' End VRRoom Code *************************************************************


'******************************************************
' 	ZLVL:  Animated Level (rothbauerw)
'******************************************************

Dim LevelYTime:LevelYtime=0
Dim LevelVely, LevelVely2:LevelVely = 0: LevelVely2 = 0
Dim Levely:Levely = 0
Const MaxMovement = 10

Sub UpdateLevel()
	CalcDisplacement LevelVely, LevelYtime, Levely, LevelVely2
	Level_Bubble.transy = - Levely - 10  'Moves bubble in level
End Sub

Sub LevelNudgeY(namp, ndir)
	CalcVelTime namp - rnd * namp * 0.1 , ndir, LevelVely, LevelYTime, LevelVely2
End Sub 

Const lDecay = 0.7 '.95
Const lAcc = 500 '3000

Sub CalcDisplacement(svel, stime, sangle, nvel)
	dim velM, accM , stimef, stimec

	stimec = Gametime - stime	
	stimef = TimeF(MaxDisplacement(svel))

	If stimec >  stimef Then 
		stimec = stimec - stimef
		stime = Gametime - stimec
		
		If nvel <> 0 Then
			if abs(nvel) > abs(svel) then 
				svel = -sgn(svel)*abs(nvel)
			Else
				svel = -svel * lDecay
			End If

			nvel = 0
		Else
			svel = -svel * lDecay
		End If

		stimef = TimeF(MaxDisplacement(svel))

		If stimec > stimef Then
			svel = 0
			sangle  = 0			
		End If
	End If

	velM = veltime(svel, stimec/1000)
	accM = acctime(lAcc, stimec/1000)

	If svel < 0 Then
		sangle = velM + accM
	ElseIf svel > 0 Then
		sangle = velM - accM
	End If
End Sub

Function MaxDisplacement(velocity)
	MaxDisplacement = (Velocity*Velocity)/(2*lAcc)  ' OPT Rule1: ^2→multiply
End Function

Function TimeF(displacement)
	TimeF = 1000*2*SQR(Abs(displacement)*lAcc*2)/lAcc
End Function

Function veltime(vel, time)
	veltime = vel*time
End Function

Function acctime(acc, time)
	acctime = (acc * time*time)/2  ' OPT Rule1
End Function

Function sameSign(num1, num2)
	sameSign = (num1 >= 0 and num2 >= 0) or (num1 < 0 and num2 < 0) 
End Function

Function Vel0(displacement)
	Vel0 = SQR(lAcc* ABS(displacement) * 2)
End Function

Function Sgn(num)
	If num > 0 Then 
		Sgn = 1
	Else	
		Sgn = -1
	End If
End Function

Sub CalcVelTime(simpulse, sidir, svel, stime, nvel)
	if simpulse > 1 then simpulse = 1

'	If ShipKick.enabled = False and ShipKickDir <> 1 Then
		if svel = 0 Then
			svel = simpulse * sidir * Vel0(MaxMovement)
			stime = GameTime
			nvel = 0
		Else
			nvel = simpulse * sidir * Vel0(MaxMovement)
		End If
'	End If
End Sub

Sub NudgeAnim() 'Call from GameTimer
    Dim X, Y
    NudgeSensorStatus X, Y

    If ABS(Y) > 0.05 Then
	LevelNudgeY Abs(y)/0.125, y/Abs(y)
    End If
    UpdateLevel
End Sub



