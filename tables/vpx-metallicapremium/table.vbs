'            /\                                                /\
'           / /\                                              /| \
'          / /  \                                            / |  \
'         / /  ,.----..-------.  ,-..--.   .--.   .--.,-------.| \`\
'        / /  /|  ___||__   __/\/ ,||  |  /|  |  /|  ||   ____||  \
'       /  \ /|| |___ \\ | |  \/ /|||  | / |  | / |  ||  |   / |   \
'      /    ` ||  ___|\\ | |  /  - ||  |/  |  |/  |  ||  |  /  |    \ 
'      /    ` ||  ___|\\ | |  /  - ||  |/  |  |/  |  ||  |  /  |    \ 
'     /,~/    || |___ \\\| | /  /| ||  '--.|  '--.|  ||  '----.|  |\ \
'     ` /     ||_____|\\\|_|/__/ |_||_____||_____||__||._____.||  '-' \ 
'      /     /\\     \ \\\  \  \ | ||     ||     ||  ||       //\  ___ \ 
'     / /\/|/  \\_____\/  \__\__\\_||_____||_____||__|.______//  \|   \ \ 
'    / /    \  /                                              \  /\    \ \
'   / / ,/\/ \/                                                \/  \/\. \ \ 
'  / /./   \                                                       /   \.\ \
' / //   ./                                                         \.   \\ \
' \    ./         Metallica Premium Monsters (Stern 2013)             \.    /
'  \ ./                                                                 \. /
'   `           originally by Fren and many many mods since               '
'*****************************************************************************

'       VPIN WORKSHOP
'
'       VPW Table Revisions (full history at the end)
'
'
'*********************************************************************************************************************************
' === TABLE OF CONTENTS  ===
'
' You can quickly jump to a section by searching the four letter tag (ZXXX)
'
'	ZVAR: Constants and Global Variable
'	ZVLM: VLM Arrays
'	ZTIM: Timers
'	ZINI: Table Initialization and Exiting
'   ZOPT: User Options
' 	ZMAT: General Math Functions
'	ZANI: Misc Animations
' 	ZBBR: Ball Brightness
' 	ZRBR: Room Brightness
'	ZKEY: Key Press Handling
'	ZSOL: Solenoids & Flashers
'	ZDRN: Drain, Trough, and Ball Release
'	ZFLP: Flippers
'	ZSHA: Shaker Motor
'	ZSPK: Sparky 
'	ZCAP: Captive Ball Cube
'	ZCOF: Coffin Magnet
'	ZGRV: Grave Magnet
'	ZSNK: Snake
'	ZHMR: Hammer
'	ZCRS: Cross
'	ZSWI: Switches 
'	ZSLG: Slingshot Animations
'	ZSSC: Slingshot Corrections
'	ZBMP: Bumpers 
' 	ZLAN: Light Animation Calls
' 	ZRGB: RGB Inserts
' 	ZGIU: GI updates
'	ZNFF: Flipper Corrections
' 	ZDMP: Rubber Dampeners
' 	ZBOU: VPW TargetBouncer for targets and posts
' 	ZFLE: Fleep Mechanical Sounds
' 	ZBRL: Ball Rolling and Drop Sounds
' 	ZRRL: Ramp Rolling Sound Effects
'	ZSHA: Ambient Ball Shadows
' 	ZRDT: Drop Targets
'	ZRST: Stand-Up Targets
' 	ZVRR: VR Room / VR Cabinet
'   ZCHA: Change Log
'
'*********************************************************************************************************************************

Option Explicit
Randomize
SetLocale 1033

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0

'******************************************************
'  ZVAR: Constants and Global Variables
'******************************************************
Const TestVRonDT = False

'Const cGameName = "mtl_180hc"	' "LE"	Color patched ROM
Const cGameName = "mtl_180h"	' "LE"	Normall ROM

Const BallSize = 50				 'Ball diameter in VPX units; must be 50
Const BallMass = 1				 'Ball mass must be 1
Const UseSolenoids = 1
Const UseLamps = 1
Const UseSync = 1
Const HandleMech = 0
Const SSolenoidOn = ""
Const SSolenoidOff = ""
Const SCoin = ""
Const UseVPMModSol = 2

Const tnob = 5 ' total number of balls
Const lob = 0 ' total number of locked balls 
Dim tablewidth: tablewidth = Table.width
Dim tableheight: tableheight = Table.height

Dim PlungerIM,LMag,RMag, HMag
Dim MBall1, MBall2, MBall3, MBall4, GNRBall5, GNRBall6, GNRCaptiveBall, gBOT
Dim cBall, CapBall
Dim VRRoom

Dim DesktopMode:DesktopMode = Table.ShowDT
Dim UseVPMDMD
If RenderingMode=2 Then 
	UseVPMDMD = True
Else 
	UseVPMDMD = DesktopMode
End If

LoadVPM "03060000", "sam.VBS", 3.10


'******************************************************
'	ZVLM: VLM Arrays
'******************************************************


' VLM  Arrays - Start
' Arrays per baked part
Dim BP_BR1: BP_BR1=Array(BM_BR1, LM_FL_F25_BR1, LM_FL_F27_BR1, LM_FL_F28_BR1, LM_FL_F29_BR1, LM_FL_F30_BR1, LM_GIR_GIR7_BR1, LM_GIW_GIW11_BR1, LM_GIW_GIW7_BR1, LM_L_l66_BR1, LM_L_l67_BR1, LM_B_BR1)
Dim BP_BR2: BP_BR2=Array(BM_BR2, LM_FL_F22_BR2, LM_FL_F25_BR2, LM_FL_F27_BR2, LM_FL_F28_BR2, LM_FL_F30_BR2, LM_GIR_GIR7_BR2, LM_GIW_GIW7_BR2, LM_L_l22_BR2, LM_L_l66_BR2, LM_L_l67_BR2, LM_B_BR2)
Dim BP_BR3: BP_BR3=Array(BM_BR3, LM_FL_F25_BR3, LM_FL_F27_BR3, LM_FL_F28_BR3, LM_FL_F29_BR3, LM_FL_F30_BR3, LM_FL_F32_BR3, LM_GIWU_GISpotR3_BR3, LM_GIW_GIW11_BR3, LM_GIW_GIW7_BR3, LM_L_L73_BR3, LM_L_l22_BR3, LM_L_l66_BR3, LM_L_l68_BR3, LM_B_BR3)
Dim BP_BS1: BP_BS1=Array(BM_BS1, LM_FL_F21_BS1, LM_FL_F22_BS1, LM_FL_F25_BS1, LM_FL_F27_BS1, LM_FL_F28_BS1, LM_FL_F30_BS1, LM_GIR_GIR7_BS1, LM_GIW_GIW11_BS1, LM_GIW_GIW7_BS1, LM_L_L74_BS1, LM_L_l22_BS1, LM_L_l66_BS1, LM_L_l67_BS1, LM_B_BS1)
Dim BP_BS2: BP_BS2=Array(BM_BS2, LM_FL_F22_BS2, LM_FL_F25_BS2, LM_FL_F27_BS2, LM_FL_F28_BS2, LM_FL_F30_BS2, LM_FL_F32_BS2, LM_GIB_GIB7_BS2, LM_GIR_GIR7_BS2, LM_GIW_GIW7_BS2, LM_L_L75_BS2, LM_L_l66_BS2, LM_L_l67_BS2, LM_B_BS2)
Dim BP_BS3: BP_BS3=Array(BM_BS3, LM_FL_F22_BS3, LM_FL_F25_BS3, LM_FL_F27_BS3, LM_FL_F28_BS3, LM_FL_F29_BS3, LM_FL_F30_BS3, LM_FL_F32_BS3, LM_GIR_GIR7_BS3, LM_GIWU_GISpotR3_BS3, LM_GIW_GIW11_BS3, LM_GIW_GIW7_BS3, LM_L_L73_BS3, LM_L_l22_BS3, LM_L_l66_BS3, LM_L_l67_BS3, LM_L_l68_BS3, LM_B_BS3)
Dim BP_Cube: BP_Cube=Array(BM_Cube, LM_FL_F32_Cube, LM_GIW_GIW7_Cube)
Dim BP_Hammer: BP_Hammer=Array(BM_Hammer, LM_FL_F26_Hammer, LM_FL_F27_Hammer, LM_FL_F30_Hammer, LM_FL_F31_Hammer, LM_FL_F32_Hammer, LM_GIWU_GISpotR3_Hammer, LM_GIW_GIW11_Hammer, LM_GIW_GIW7_Hammer, LM_L_l21_Hammer, LM_L_l22_Hammer, LM_L_l28_Hammer, LM_L_l29_Hammer, LM_L_l30_Hammer, LM_L_l31_Hammer, LM_L_l32_Hammer, LM_L_l33_Hammer, LM_L_l34_Hammer, LM_L_l35_Hammer, LM_L_l46_Hammer, LM_L_l47_Hammer, LM_L_l48_Hammer, LM_L_l49_Hammer, LM_L_l50_Hammer, LM_L_l65_Hammer, LM_L_l68_Hammer, LM_L_l69_Hammer, LM_L_l70_Hammer, LM_L_l71_Hammer, LM_L_l78_Hammer, LM_B_Hammer)
Dim BP_HammerUp: BP_HammerUp=Array(BM_HammerUp, LM_FL_F19_HammerUp, LM_FL_F26_HammerUp, LM_FL_F27_HammerUp, LM_FL_F28_HammerUp, LM_FL_F30_HammerUp, LM_FL_F31_HammerUp, LM_FL_F32_HammerUp, LM_GIWU_GISpotR3_HammerUp, LM_GIW_GIW11_HammerUp, LM_GIW_GIW7_HammerUp, LM_L_l21_HammerUp, LM_L_l22_HammerUp, LM_L_l28_HammerUp, LM_L_l29_HammerUp, LM_L_l31_HammerUp, LM_L_l32_HammerUp, LM_L_l33_HammerUp, LM_L_l34_HammerUp, LM_L_l35_HammerUp, LM_L_l46_HammerUp, LM_L_l47_HammerUp, LM_L_l48_HammerUp, LM_L_l49_HammerUp, LM_L_l50_HammerUp, LM_L_l65_HammerUp, LM_L_l68_HammerUp, LM_L_l69_HammerUp, LM_L_l70_HammerUp, LM_L_l71_HammerUp, LM_B_HammerUp)
Dim BP_LEMK: BP_LEMK=Array(BM_LEMK, LM_GIB_GIB2_LEMK, LM_GIR_GIR1_LEMK, LM_GIR_GIR2_LEMK, LM_GIWU_GISpotL1_LEMK, LM_GIW_GIW1_LEMK, LM_GIW_GIW2_LEMK, LM_L_l42_LEMK, LM_L_l59_LEMK)
Dim BP_LPost: BP_LPost=Array(BM_LPost, LM_GIB_GIB5_LPost, LM_GIR_GIR2_LPost, LM_GIR_GIR5_LPost, LM_GIWU_GISpotR2_LPost, LM_GIW_GIW5_LPost, LM_L_l42_LPost, LM_L_l43_LPost, LM_L_l44_LPost, LM_L_l45_LPost, LM_L_l63_LPost, LM_L_l64_LPost)
Dim BP_LSling1: BP_LSling1=Array(BM_LSling1, LM_GIB_GIB2_LSling1, LM_GIR_GIR2_LSling1, LM_GIR_GIR5_LSling1, LM_GIWU_GISpotL2_LSling1, LM_GIW_GIW1_LSling1, LM_GIW_GIW2_LSling1, LM_L_l41_LSling1, LM_L_l59_LSling1)
Dim BP_LSling2: BP_LSling2=Array(BM_LSling2, LM_GIR_GIR2_LSling2, LM_GIW_GIW2_LSling2, LM_L_l40_LSling2, LM_L_l41_LSling2, LM_L_l59_LSling2)
Dim BP_Layer1: BP_Layer1=Array(BM_Layer1, LM_L_CN11_Layer1, LM_L_CN19_Layer1, LM_L_CN4_Layer1, LM_FL_F19_Layer1, LM_FL_F21_Layer1, LM_FL_F22_Layer1, LM_FL_F23_Layer1, LM_FL_F25_Layer1, LM_FL_F26_Layer1, LM_FL_F27_Layer1, LM_FL_F28_Layer1, LM_FL_F30_Layer1, LM_FL_F31_Layer1, LM_FL_F32_Layer1, LM_GIB_GIB7_Layer1, LM_GIB_GIB5_Layer1, LM_GIR_GIR5_Layer1, LM_GIR_GIR6_Layer1, LM_GIR_GIR7_Layer1, LM_GIWU_GISpotR3_Layer1, LM_GIW_GIW11_Layer1, LM_GIW_GIW7_Layer1, LM_GIW_GIW5_Layer1, LM_GIW_GIW6_Layer1, LM_L_L73_Layer1, LM_L_l18_Layer1, LM_L_l21_Layer1, LM_L_l22_Layer1, LM_L_l28_Layer1, LM_L_l29_Layer1, LM_L_l31_Layer1, LM_L_l32_Layer1, LM_L_l33_Layer1, LM_L_l34_Layer1, LM_L_l43_Layer1, LM_L_l44_Layer1, LM_L_l45_Layer1, LM_L_l48_Layer1, LM_L_l49_Layer1, LM_L_l50_Layer1, LM_L_l51_Layer1, LM_L_l64_Layer1, LM_L_l65_Layer1, LM_L_l66_Layer1, LM_L_l68_Layer1, LM_L_l69_Layer1, LM_L_l70_Layer1, LM_L_l71_Layer1, LM_L_l77_Layer1, LM_L_l78_Layer1, LM_L_l79_Layer1, LM_L_l80_Layer1, LM_B_Layer1, LM_L_optos_Layer1)
Dim BP_Layer2a: BP_Layer2a=Array(BM_Layer2a, LM_FL_F22_Layer2a, LM_FL_F25_Layer2a, LM_FL_F30_Layer2a, LM_FL_F32_Layer2a, LM_GIWU_GISpotR3_Layer2a, LM_GIW_GIW11_Layer2a, LM_GIW_GIW7_Layer2a, LM_L_L73_Layer2a, LM_L_l22_Layer2a, LM_B_Layer2a)
Dim BP_Layer2b: BP_Layer2b=Array(BM_Layer2b, LM_FL_F22_Layer2b, LM_FL_F25_Layer2b, LM_FL_F28_Layer2b, LM_FL_F30_Layer2b, LM_FL_F32_Layer2b, LM_GIW_GIW7_Layer2b, LM_L_L73_Layer2b, LM_L_L74_Layer2b, LM_L_l22_Layer2b, LM_L_l66_Layer2b, LM_L_l67_Layer2b, LM_B_Layer2b)
Dim BP_Layer3: BP_Layer3=Array(BM_Layer3, LM_L_CN11_Layer3, LM_FL_F19_Layer3, LM_FL_F21_Layer3, LM_FL_F22_Layer3, LM_FL_F23_Layer3, LM_FL_F26_Layer3, LM_FL_F27_Layer3, LM_FL_F28_Layer3, LM_FL_F30_Layer3, LM_FL_F32_Layer3, LM_GIB_GIB7_Layer3, LM_GIR_GIR7_Layer3, LM_GIW_GIW7_Layer3, LM_L_L73_Layer3, LM_L_l21_Layer3, LM_L_l49_Layer3, LM_L_l65_Layer3, LM_L_l66_Layer3, LM_L_l71_Layer3, LM_B_Layer3)
Dim BP_Lflip: BP_Lflip=Array(BM_Lflip, LM_GIR_GIR2_Lflip, LM_GIWU_GISpotL1_Lflip, LM_GIW_GIW1_Lflip, LM_GIW_GIW2_Lflip, LM_GIW_GIW3_Lflip, LM_L_l53_Lflip, LM_L_l55_Lflip, LM_L_l56_Lflip, LM_L_l57_Lflip, LM_L_l58_Lflip, LM_L_l59_Lflip, LM_L_l60_Lflip, LM_L_l61_Lflip)
Dim BP_LflipU: BP_LflipU=Array(BM_LflipU, LM_GIR_GIR1_LflipU, LM_GIR_GIR2_LflipU, LM_GIWU_GISpotL1_LflipU, LM_GIWU_GISpotR1_LflipU, LM_GIW_GIW1_LflipU, LM_GIW_GIW2_LflipU, LM_GIW_GIW4_LflipU, LM_L_l53_LflipU, LM_L_l55_LflipU, LM_L_l56_LflipU, LM_L_l57_LflipU, LM_L_l58_LflipU, LM_L_l59_LflipU, LM_L_l60_LflipU, LM_L_l61_LflipU)
Dim BP_Lspinner: BP_Lspinner=Array(BM_Lspinner, LM_L_CN11_Lspinner, LM_FL_F19_Lspinner, LM_FL_F26_Lspinner, LM_GIW_GIW11_Lspinner, LM_GIW_GIW7_Lspinner, LM_L_l49_Lspinner, LM_L_l50_Lspinner, LM_L_l51_Lspinner, LM_L_l71_Lspinner, LM_B_Lspinner)
Dim BP_Parts: BP_Parts=Array(BM_Parts, LM_L_CN11_Parts, LM_L_CN13_Parts, LM_L_CN19_Parts, LM_L_CN4_Parts, LM_L_CN5_Parts, LM_L_CN9_Parts, LM_FL_F19_Parts, LM_FL_F21_Parts, LM_FL_F22_Parts, LM_FL_F23_Parts, LM_FL_F25_Parts, LM_FL_F26_Parts, LM_FL_F27_Parts, LM_FL_F28_Parts, LM_FL_F29_Parts, LM_FL_F30_Parts, LM_FL_F31_Parts, LM_FL_F32_Parts, LM_GIB_GIB1_Parts, LM_GIB_GIB7_Parts, LM_GIB_GIB2_Parts, LM_GIB_GIB3_Parts, LM_GIB_GIB4_Parts, LM_GIB_GIB5_Parts, LM_GIB_GIB6_Parts, LM_GIR_GIR1_Parts, LM_GIR_GIR2_Parts, LM_GIR_GIR3_Parts, LM_GIR_GIR4_Parts, LM_GIR_GIR5_Parts, LM_GIR_GIR6_Parts, LM_GIR_GIR7_Parts, LM_GIWU_GISpotL1_Parts, LM_GIWU_GISpotL2_Parts, LM_GIWU_GISpotR1_Parts, LM_GIWU_GISpotR2_Parts, LM_GIWU_GISpotR3_Parts, LM_GIW_GIW1_Parts, LM_GIW_GIW11_Parts, LM_GIW_GIW7_Parts, LM_GIW_GIW2_Parts, LM_GIW_GIW3_Parts, LM_GIW_GIW4_Parts, LM_GIW_GIW5_Parts, LM_GIW_GIW6_Parts, LM_L_L73_Parts, LM_L_L74_Parts, LM_L_L75_Parts, LM_L_l17_Parts, LM_L_l18_Parts, LM_L_l19_Parts, LM_L_l21_Parts, LM_L_l22_Parts, LM_L_l23_Parts, _
	LM_L_l24_Parts, LM_L_l25_Parts, LM_L_l26_Parts, LM_L_l27_Parts, LM_L_l28_Parts, LM_L_l29_Parts, LM_L_l30_Parts, LM_L_l31_Parts, LM_L_l32_Parts, LM_L_l33_Parts, LM_L_l34_Parts, LM_L_l35_Parts, LM_L_l37_Parts, LM_L_l38_Parts, LM_L_l39_Parts, LM_L_l40_Parts, LM_L_l41_Parts, LM_L_l42_Parts, LM_L_l43_Parts, LM_L_l44_Parts, LM_L_l45_Parts, LM_L_l46_Parts, LM_L_l47_Parts, LM_L_l48_Parts, LM_L_l49_Parts, LM_L_l50_Parts, LM_L_l51_Parts, LM_L_l53_Parts, LM_L_l55_Parts, LM_L_l56_Parts, LM_L_l57_Parts, LM_L_l58_Parts, LM_L_l59_Parts, LM_L_l60_Parts, LM_L_l61_Parts, LM_L_l62_Parts, LM_L_l63_Parts, LM_L_l64_Parts, LM_L_l65_Parts, LM_L_l66_Parts, LM_L_l67_Parts, LM_L_l68_Parts, LM_L_l69_Parts, LM_L_l70_Parts, LM_L_l71_Parts, LM_L_l77_Parts, LM_L_l78_Parts, LM_L_l79_Parts, LM_L_l80_Parts, LM_B_Parts, LM_L_optos_Parts)
Dim BP_Playfield: BP_Playfield=Array(BM_Playfield, LM_L_CN11_Playfield, LM_L_CN13_Playfield, LM_L_CN19_Playfield, LM_L_CN4_Playfield, LM_L_CN5_Playfield, LM_L_CN9_Playfield, LM_FL_F19_Playfield, LM_FL_F21_Playfield, LM_FL_F22_Playfield, LM_FL_F23_Playfield, LM_FL_F25_Playfield, LM_FL_F26_Playfield, LM_FL_F27_Playfield, LM_FL_F28_Playfield, LM_FL_F29_Playfield, LM_FL_F30_Playfield, LM_FL_F31_Playfield, LM_FL_F32_Playfield, LM_GIB_GIB1_Playfield, LM_GIB_GIB7_Playfield, LM_GIB_GIB2_Playfield, LM_GIB_GIB3_Playfield, LM_GIB_GIB4_Playfield, LM_GIB_GIB5_Playfield, LM_GIB_GIB6_Playfield, LM_GIR_GIR1_Playfield, LM_GIR_GIR2_Playfield, LM_GIR_GIR3_Playfield, LM_GIR_GIR4_Playfield, LM_GIR_GIR5_Playfield, LM_GIR_GIR6_Playfield, LM_GIR_GIR7_Playfield, LM_GIWU_GISpotL1_Playfield, LM_GIWU_GISpotL2_Playfield, LM_GIWU_GISpotR1_Playfield, LM_GIWU_GISpotR2_Playfield, LM_GIWU_GISpotR3_Playfield, LM_GIW_GIW1_Playfield, LM_GIW_GIW11_Playfield, LM_GIW_GIW7_Playfield, LM_GIW_GIW2_Playfield, LM_GIW_GIW3_Playfield, _
	LM_GIW_GIW4_Playfield, LM_GIW_GIW5_Playfield, LM_GIW_GIW6_Playfield, LM_L_l17_Playfield, LM_L_l18_Playfield, LM_L_l19_Playfield, LM_L_l21_Playfield, LM_L_l22_Playfield, LM_L_l23_Playfield, LM_L_l24_Playfield, LM_L_l25_Playfield, LM_L_l26_Playfield, LM_L_l27_Playfield, LM_L_l28_Playfield, LM_L_l29_Playfield, LM_L_l30_Playfield, LM_L_l31_Playfield, LM_L_l32_Playfield, LM_L_l33_Playfield, LM_L_l34_Playfield, LM_L_l35_Playfield, LM_L_l37_Playfield, LM_L_l38_Playfield, LM_L_l39_Playfield, LM_L_l40_Playfield, LM_L_l41_Playfield, LM_L_l42_Playfield, LM_L_l43_Playfield, LM_L_l44_Playfield, LM_L_l45_Playfield, LM_L_l46_Playfield, LM_L_l47_Playfield, LM_L_l48_Playfield, LM_L_l49_Playfield, LM_L_l50_Playfield, LM_L_l51_Playfield, LM_L_l53_Playfield, LM_L_l55_Playfield, LM_L_l56_Playfield, LM_L_l57_Playfield, LM_L_l58_Playfield, LM_L_l59_Playfield, LM_L_l60_Playfield, LM_L_l61_Playfield, LM_L_l62_Playfield, LM_L_l63_Playfield, LM_L_l64_Playfield, LM_L_l65_Playfield, LM_L_l66_Playfield, LM_L_l67_Playfield, _
	LM_L_l68_Playfield, LM_L_l69_Playfield, LM_L_l70_Playfield, LM_L_l71_Playfield, LM_L_l77_Playfield, LM_L_l78_Playfield, LM_L_l79_Playfield, LM_L_l80_Playfield, LM_B_Playfield, LM_L_optos_Playfield)
Dim BP_REMK: BP_REMK=Array(BM_REMK, LM_GIB_GIB3_REMK, LM_GIB_GIB4_REMK, LM_GIR_GIR3_REMK, LM_GIR_GIR4_REMK, LM_GIWU_GISpotR1_REMK, LM_GIW_GIW3_REMK, LM_GIW_GIW4_REMK, LM_L_l37_REMK, LM_L_l62_REMK)
Dim BP_RPost: BP_RPost=Array(BM_RPost, LM_GIB_GIB6_RPost, LM_GIR_GIR4_RPost, LM_GIWU_GISpotL2_RPost, LM_GIW_GIW4_RPost, LM_GIW_GIW6_RPost, LM_L_l26_RPost, LM_L_l27_RPost)
Dim BP_RSling1: BP_RSling1=Array(BM_RSling1, LM_GIB_GIB4_RSling1, LM_GIR_GIR4_RSling1, LM_GIWU_GISpotR1_RSling1, LM_GIW_GIW4_RSling1, LM_GIW_GIW5_RSling1, LM_L_l37_RSling1, LM_L_l38_RSling1, LM_L_l39_RSling1, LM_L_l40_RSling1, LM_L_l62_RSling1)
Dim BP_RSling2: BP_RSling2=Array(BM_RSling2, LM_GIB_GIB4_RSling2, LM_GIR_GIR4_RSling2, LM_GIWU_GISpotR2_RSling2, LM_GIW_GIW3_RSling2, LM_GIW_GIW4_RSling2, LM_L_l37_RSling2, LM_L_l38_RSling2, LM_L_l39_RSling2, LM_L_l40_RSling2, LM_L_l62_RSling2)
Dim BP_Rflip: BP_Rflip=Array(BM_Rflip, LM_GIR_GIR3_Rflip, LM_GIR_GIR4_Rflip, LM_GIWU_GISpotR1_Rflip, LM_GIW_GIW1_Rflip, LM_GIW_GIW3_Rflip, LM_GIW_GIW4_Rflip, LM_L_l53_Rflip, LM_L_l55_Rflip, LM_L_l56_Rflip, LM_L_l57_Rflip, LM_L_l58_Rflip, LM_L_l60_Rflip, LM_L_l61_Rflip, LM_L_l62_Rflip)
Dim BP_RflipU: BP_RflipU=Array(BM_RflipU, LM_GIR_GIR3_RflipU, LM_GIR_GIR4_RflipU, LM_GIWU_GISpotL1_RflipU, LM_GIWU_GISpotR1_RflipU, LM_GIW_GIW1_RflipU, LM_GIW_GIW2_RflipU, LM_GIW_GIW3_RflipU, LM_GIW_GIW4_RflipU, LM_L_l53_RflipU, LM_L_l55_RflipU, LM_L_l56_RflipU, LM_L_l57_RflipU, LM_L_l58_RflipU, LM_L_l60_RflipU, LM_L_l61_RflipU, LM_L_l62_RflipU)
Dim BP_Rod: BP_Rod=Array(BM_Rod, LM_FL_F21_Rod, LM_FL_F27_Rod, LM_FL_F28_Rod, LM_FL_F30_Rod, LM_FL_F32_Rod, LM_GIW_GIW11_Rod, LM_GIW_GIW7_Rod, LM_L_l22_Rod, LM_L_l68_Rod, LM_B_Rod)
Dim BP_Rod_001: BP_Rod_001=Array(BM_Rod_001, LM_FL_F22_Rod_001, LM_FL_F30_Rod_001, LM_FL_F32_Rod_001, LM_GIWU_GISpotL2_Rod_001, LM_GIW_GIW7_Rod_001, LM_B_Rod_001)
Dim BP_Rspinner: BP_Rspinner=Array(BM_Rspinner, LM_L_CN9_Rspinner, LM_FL_F30_Rspinner, LM_GIW_GIW7_Rspinner, LM_L_l17_Rspinner, LM_L_l18_Rspinner, LM_L_l19_Rspinner, LM_L_l25_Rspinner, LM_L_l28_Rspinner, LM_L_l29_Rspinner, LM_L_l31_Rspinner)
Dim BP_SnakeMOD: BP_SnakeMOD=Array(BM_SnakeMOD, LM_FL_F22_SnakeMOD, LM_FL_F25_SnakeMOD, LM_FL_F27_SnakeMOD, LM_FL_F28_SnakeMOD, LM_FL_F30_SnakeMOD, LM_GIW_GIW7_SnakeMOD, LM_B_SnakeMOD)
Dim BP_UpPost: BP_UpPost=Array(BM_UpPost, LM_FL_F21_UpPost, LM_FL_F22_UpPost, LM_FL_F26_UpPost, LM_FL_F27_UpPost, LM_FL_F28_UpPost, LM_GIB_GIB7_UpPost, LM_GIR_GIR7_UpPost, LM_GIW_GIW7_UpPost, LM_B_UpPost)
Dim BP_cemeteryMOD: BP_cemeteryMOD=Array(BM_cemeteryMOD, LM_L_CN11_cemeteryMOD, LM_L_CN19_cemeteryMOD, LM_FL_F19_cemeteryMOD, LM_FL_F21_cemeteryMOD, LM_FL_F23_cemeteryMOD, LM_FL_F26_cemeteryMOD, LM_FL_F28_cemeteryMOD, LM_FL_F30_cemeteryMOD, LM_FL_F32_cemeteryMOD, LM_GIW_GIW11_cemeteryMOD, LM_GIW_GIW7_cemeteryMOD, LM_L_l21_cemeteryMOD, LM_L_l33_cemeteryMOD, LM_L_l34_cemeteryMOD, LM_L_l49_cemeteryMOD, LM_L_l65_cemeteryMOD, LM_L_l71_cemeteryMOD, LM_B_cemeteryMOD)
Dim BP_cross: BP_cross=Array(BM_cross, LM_FL_F19_cross, LM_FL_F26_cross, LM_FL_F27_cross, LM_FL_F28_cross, LM_FL_F32_cross, LM_GIB_GIB7_cross, LM_GIW_GIW11_cross, LM_GIW_GIW7_cross, LM_L_l65_cross, LM_L_l71_cross, LM_B_cross)
Dim BP_crossHolder: BP_crossHolder=Array(BM_crossHolder, LM_FL_F19_crossHolder, LM_FL_F26_crossHolder, LM_FL_F27_crossHolder, LM_GIB_GIB7_crossHolder, LM_GIW_GIW7_crossHolder, LM_B_crossHolder)
Dim BP_lockPost: BP_lockPost=Array(BM_lockPost, LM_FL_F31_lockPost, LM_L_optos_lockPost)
Dim BP_snakeM1: BP_snakeM1=Array(BM_snakeM1, LM_FL_F29_snakeM1, LM_FL_F30_snakeM1, LM_FL_F31_snakeM1, LM_GIWU_GISpotL2_snakeM1, LM_GIWU_GISpotR3_snakeM1, LM_GIW_GIW7_snakeM1)
Dim BP_snakeM2: BP_snakeM2=Array(BM_snakeM2, LM_L_CN5_snakeM2, LM_L_CN9_snakeM2, LM_FL_F29_snakeM2, LM_FL_F30_snakeM2, LM_GIWU_GISpotR3_snakeM2, LM_GIW_GIW7_snakeM2, LM_GIW_GIW6_snakeM2, LM_L_l17_snakeM2, LM_L_l28_snakeM2, LM_L_l29_snakeM2, LM_L_l31_snakeM2, LM_L_l32_snakeM2, LM_L_l34_snakeM2, LM_L_l35_snakeM2, LM_L_l48_snakeM2)
Dim BP_sparkyhead: BP_sparkyhead=Array(BM_sparkyhead, LM_FL_F21_sparkyhead, LM_FL_F22_sparkyhead, LM_FL_F23_sparkyhead, LM_FL_F25_sparkyhead, LM_FL_F26_sparkyhead, LM_FL_F27_sparkyhead, LM_FL_F28_sparkyhead, LM_FL_F30_sparkyhead, LM_FL_F32_sparkyhead, LM_GIB_GIB7_sparkyhead, LM_GIR_GIR7_sparkyhead, LM_GIW_GIW11_sparkyhead, LM_GIW_GIW7_sparkyhead, LM_L_L75_sparkyhead, LM_L_l21_sparkyhead, LM_L_l22_sparkyhead, LM_L_l67_sparkyhead, LM_L_l68_sparkyhead, LM_L_l69_sparkyhead, LM_L_l70_sparkyhead, LM_B_sparkyhead)
Dim BP_sw0709: BP_sw0709=Array(BM_sw0709, LM_GIW_GIW7_sw0709, LM_L_l47_sw0709, LM_L_l48_sw0709, LM_L_l51_sw0709, LM_L_l77_sw0709, LM_L_l78_sw0709, LM_L_l79_sw0709, LM_L_l80_sw0709)
Dim BP_sw1: BP_sw1=Array(BM_sw1, LM_GIB_GIB2_sw1, LM_GIR_GIR1_sw1, LM_GIR_GIR2_sw1, LM_GIR_GIR5_sw1, LM_GIW_GIW2_sw1, LM_GIW_GIW5_sw1, LM_L_l42_sw1, LM_L_l63_sw1, LM_L_l64_sw1)
Dim BP_sw24: BP_sw24=Array(BM_sw24, LM_GIR_GIR1_sw24, LM_GIR_GIR2_sw24, LM_GIW_GIW1_sw24, LM_GIW_GIW2_sw24, LM_GIW_GIW5_sw24, LM_L_l63_sw24)
Dim BP_sw25: BP_sw25=Array(BM_sw25, LM_GIB_GIB2_sw25, LM_GIB_GIB5_sw25, LM_GIR_GIR1_sw25, LM_GIR_GIR2_sw25, LM_GIW_GIW2_sw25, LM_GIW_GIW5_sw25, LM_L_l42_sw25, LM_L_l63_sw25, LM_L_l64_sw25)
Dim BP_sw28: BP_sw28=Array(BM_sw28, LM_GIB_GIB4_sw28, LM_GIR_GIR3_sw28, LM_GIR_GIR4_sw28, LM_GIR_GIR6_sw28, LM_GIW_GIW7_sw28, LM_GIW_GIW4_sw28, LM_GIW_GIW6_sw28, LM_L_l26_sw28, LM_L_l27_sw28, LM_L_l64_sw28)
Dim BP_sw29: BP_sw29=Array(BM_sw29, LM_GIB_GIB4_sw29, LM_GIR_GIR3_sw29, LM_GIR_GIR4_sw29, LM_GIW_GIW1_sw29, LM_GIW_GIW4_sw29, LM_GIW_GIW6_sw29, LM_L_l27_sw29)
Dim BP_sw3: BP_sw3=Array(BM_sw3, LM_GIW_GIW7_sw3, LM_L_l78_sw3)
Dim BP_sw35: BP_sw35=Array(BM_sw35, LM_FL_F26_sw35, LM_FL_F27_sw35, LM_FL_F28_sw35, LM_FL_F32_sw35, LM_GIW_GIW7_sw35, LM_L_l68_sw35, LM_L_l69_sw35, LM_L_l70_sw35)
Dim BP_sw36: BP_sw36=Array(BM_sw36, LM_L_CN13_sw36, LM_GIW_GIW11_sw36, LM_GIW_GIW7_sw36, LM_L_l48_sw36, LM_L_l49_sw36, LM_L_l50_sw36, LM_L_l51_sw36, LM_L_l65_sw36, LM_L_l71_sw36, LM_L_l77_sw36)
Dim BP_sw37: BP_sw37=Array(BM_sw37, LM_L_CN13_sw37, LM_GIW_GIW11_sw37, LM_GIW_GIW7_sw37, LM_L_l33_sw37, LM_L_l34_sw37, LM_L_l35_sw37, LM_L_l48_sw37, LM_L_l51_sw37, LM_L_l65_sw37, LM_L_l71_sw37)
Dim BP_sw4: BP_sw4=Array(BM_sw4, LM_GIW_GIW7_sw4)
Dim BP_sw40: BP_sw40=Array(BM_sw40, LM_L_CN5_sw40, LM_FL_F30_sw40, LM_GIWU_GISpotR3_sw40, LM_GIW_GIW6_sw40, LM_L_l28_sw40, LM_L_l29_sw40, LM_L_l31_sw40, LM_L_l32_sw40)
Dim BP_sw41: BP_sw41=Array(BM_sw41, LM_L_CN5_sw41, LM_L_CN9_sw41, LM_GIW_GIW7_sw41, LM_L_l17_sw41, LM_L_l18_sw41, LM_L_l19_sw41, LM_L_l28_sw41, LM_L_l29_sw41, LM_L_l31_sw41, LM_L_l32_sw41)
Dim BP_sw42: BP_sw42=Array(BM_sw42, LM_FL_F32_sw42, LM_GIW_GIW7_sw42)
Dim BP_sw43: BP_sw43=Array(BM_sw43, LM_FL_F22_sw43, LM_FL_F23_sw43, LM_FL_F26_sw43, LM_FL_F27_sw43, LM_GIB_GIB7_sw43, LM_GIW_GIW7_sw43)
Dim BP_sw44: BP_sw44=Array(BM_sw44, LM_FL_F22_sw44, LM_FL_F27_sw44, LM_GIB_GIB7_sw44, LM_GIR_GIR7_sw44, LM_GIW_GIW7_sw44)
Dim BP_sw45: BP_sw45=Array(BM_sw45, LM_FL_F22_sw45, LM_FL_F27_sw45, LM_GIR_GIR7_sw45, LM_GIW_GIW7_sw45)
Dim BP_sw46: BP_sw46=Array(BM_sw46, LM_FL_F21_sw46, LM_FL_F22_sw46, LM_FL_F28_sw46, LM_GIW_GIW7_sw46)
Dim BP_sw47p: BP_sw47p=Array(BM_sw47p, LM_FL_F22_sw47p, LM_FL_F27_sw47p, LM_FL_F28_sw47p, LM_FL_F30_sw47p, LM_FL_F32_sw47p, LM_GIW_GIW7_sw47p, LM_B_sw47p)
Dim BP_sw50p: BP_sw50p=Array(BM_sw50p, LM_FL_F21_sw50p, LM_FL_F26_sw50p, LM_FL_F27_sw50p, LM_GIW_GIW7_sw50p)
Dim BP_sw60: BP_sw60=Array(BM_sw60, LM_L_CN13_sw60, LM_FL_F26_sw60, LM_FL_F31_sw60, LM_GIW_GIW11_sw60, LM_GIW_GIW7_sw60, LM_L_l34_sw60, LM_L_l35_sw60, LM_L_l48_sw60, LM_L_l49_sw60, LM_L_l50_sw60, LM_L_l51_sw60, LM_L_l65_sw60, LM_L_l71_sw60, LM_L_l77_sw60)
Dim BP_sw61: BP_sw61=Array(BM_sw61, LM_L_CN13_sw61, LM_FL_F26_sw61, LM_FL_F31_sw61, LM_FL_F32_sw61, LM_GIW_GIW11_sw61, LM_GIW_GIW7_sw61, LM_L_l33_sw61, LM_L_l34_sw61, LM_L_l35_sw61, LM_L_l48_sw61, LM_L_l49_sw61, LM_L_l50_sw61, LM_L_l51_sw61, LM_L_l65_sw61, LM_L_l70_sw61, LM_L_l71_sw61, LM_B_sw61)
Dim BP_sw62: BP_sw62=Array(BM_sw62, LM_L_CN13_sw62, LM_FL_F19_sw62, LM_FL_F26_sw62, LM_FL_F32_sw62, LM_GIW_GIW11_sw62, LM_GIW_GIW7_sw62, LM_L_l33_sw62, LM_L_l35_sw62, LM_L_l48_sw62, LM_L_l49_sw62, LM_L_l51_sw62, LM_L_l65_sw62, LM_L_l71_sw62, LM_B_sw62)
Dim BP_swplunger: BP_swplunger=Array(BM_swplunger, LM_GIW_GIW7_swplunger)
Dim BP_underPF: BP_underPF=Array(BM_underPF, LM_L_CN11_underPF, LM_L_CN13_underPF, LM_L_CN19_underPF, LM_L_CN4_underPF, LM_L_CN5_underPF, LM_L_CN9_underPF, LM_FL_F21_underPF, LM_FL_F23_underPF, LM_FL_F25_underPF, LM_FL_F26_underPF, LM_FL_F27_underPF, LM_FL_F28_underPF, LM_FL_F29_underPF, LM_FL_F30_underPF, LM_FL_F31_underPF, LM_FL_F32_underPF, LM_GIB_GIB7_underPF, LM_GIR_GIR5_underPF, LM_GIR_GIR6_underPF, LM_GIR_GIR7_underPF, LM_GIWU_GISpotL2_underPF, LM_GIWU_GISpotR2_underPF, LM_GIWU_GISpotR3_underPF, LM_GIW_GIW11_underPF, LM_GIW_GIW7_underPF, LM_GIW_GIW5_underPF, LM_GIW_GIW6_underPF, LM_L_l17_underPF, LM_L_l18_underPF, LM_L_l19_underPF, LM_L_l21_underPF, LM_L_l23_underPF, LM_L_l24_underPF, LM_L_l25_underPF, LM_L_l26_underPF, LM_L_l27_underPF, LM_L_l28_underPF, LM_L_l29_underPF, LM_L_l30_underPF, LM_L_l31_underPF, LM_L_l32_underPF, LM_L_l33_underPF, LM_L_l34_underPF, LM_L_l35_underPF, LM_L_l37_underPF, LM_L_l38_underPF, LM_L_l39_underPF, LM_L_l40_underPF, LM_L_l41_underPF, LM_L_l42_underPF, LM_L_l43_underPF, _
	LM_L_l44_underPF, LM_L_l45_underPF, LM_L_l46_underPF, LM_L_l47_underPF, LM_L_l48_underPF, LM_L_l49_underPF, LM_L_l50_underPF, LM_L_l51_underPF, LM_L_l53_underPF, LM_L_l55_underPF, LM_L_l56_underPF, LM_L_l57_underPF, LM_L_l58_underPF, LM_L_l59_underPF, LM_L_l60_underPF, LM_L_l61_underPF, LM_L_l62_underPF, LM_L_l63_underPF, LM_L_l64_underPF, LM_L_l65_underPF, LM_L_l66_underPF, LM_L_l67_underPF, LM_L_l68_underPF, LM_L_l69_underPF, LM_L_l70_underPF, LM_L_l71_underPF, LM_L_l77_underPF, LM_L_l78_underPF, LM_L_l79_underPF, LM_L_l80_underPF, LM_B_underPF, LM_L_optos_underPF)
' Arrays per lighting scenario
Dim BL_B: BL_B=Array(LM_B_BR1, LM_B_BR2, LM_B_BR3, LM_B_BS1, LM_B_BS2, LM_B_BS3, LM_B_Hammer, LM_B_HammerUp, LM_B_Layer1, LM_B_Layer2a, LM_B_Layer2b, LM_B_Layer3, LM_B_Lspinner, LM_B_Parts, LM_B_Playfield, LM_B_Rod, LM_B_Rod_001, LM_B_SnakeMOD, LM_B_UpPost, LM_B_cemeteryMOD, LM_B_cross, LM_B_crossHolder, LM_B_sparkyhead, LM_B_sw47p, LM_B_sw61, LM_B_sw62, LM_B_underPF)
Dim BL_FL_F19: BL_FL_F19=Array(LM_FL_F19_HammerUp, LM_FL_F19_Layer1, LM_FL_F19_Layer3, LM_FL_F19_Lspinner, LM_FL_F19_Parts, LM_FL_F19_Playfield, LM_FL_F19_cemeteryMOD, LM_FL_F19_cross, LM_FL_F19_crossHolder, LM_FL_F19_sw62)
Dim BL_FL_F21: BL_FL_F21=Array(LM_FL_F21_BS1, LM_FL_F21_Layer1, LM_FL_F21_Layer3, LM_FL_F21_Parts, LM_FL_F21_Playfield, LM_FL_F21_Rod, LM_FL_F21_UpPost, LM_FL_F21_cemeteryMOD, LM_FL_F21_sparkyhead, LM_FL_F21_sw46, LM_FL_F21_sw50p, LM_FL_F21_underPF)
Dim BL_FL_F22: BL_FL_F22=Array(LM_FL_F22_BR2, LM_FL_F22_BS1, LM_FL_F22_BS2, LM_FL_F22_BS3, LM_FL_F22_Layer1, LM_FL_F22_Layer2a, LM_FL_F22_Layer2b, LM_FL_F22_Layer3, LM_FL_F22_Parts, LM_FL_F22_Playfield, LM_FL_F22_Rod_001, LM_FL_F22_SnakeMOD, LM_FL_F22_UpPost, LM_FL_F22_sparkyhead, LM_FL_F22_sw43, LM_FL_F22_sw44, LM_FL_F22_sw45, LM_FL_F22_sw46, LM_FL_F22_sw47p)
Dim BL_FL_F23: BL_FL_F23=Array(LM_FL_F23_Layer1, LM_FL_F23_Layer3, LM_FL_F23_Parts, LM_FL_F23_Playfield, LM_FL_F23_cemeteryMOD, LM_FL_F23_sparkyhead, LM_FL_F23_sw43, LM_FL_F23_underPF)
Dim BL_FL_F25: BL_FL_F25=Array(LM_FL_F25_BR1, LM_FL_F25_BR2, LM_FL_F25_BR3, LM_FL_F25_BS1, LM_FL_F25_BS2, LM_FL_F25_BS3, LM_FL_F25_Layer1, LM_FL_F25_Layer2a, LM_FL_F25_Layer2b, LM_FL_F25_Parts, LM_FL_F25_Playfield, LM_FL_F25_SnakeMOD, LM_FL_F25_sparkyhead, LM_FL_F25_underPF)
Dim BL_FL_F26: BL_FL_F26=Array(LM_FL_F26_Hammer, LM_FL_F26_HammerUp, LM_FL_F26_Layer1, LM_FL_F26_Layer3, LM_FL_F26_Lspinner, LM_FL_F26_Parts, LM_FL_F26_Playfield, LM_FL_F26_UpPost, LM_FL_F26_cemeteryMOD, LM_FL_F26_cross, LM_FL_F26_crossHolder, LM_FL_F26_sparkyhead, LM_FL_F26_sw35, LM_FL_F26_sw43, LM_FL_F26_sw50p, LM_FL_F26_sw60, LM_FL_F26_sw61, LM_FL_F26_sw62, LM_FL_F26_underPF)
Dim BL_FL_F27: BL_FL_F27=Array(LM_FL_F27_BR1, LM_FL_F27_BR2, LM_FL_F27_BR3, LM_FL_F27_BS1, LM_FL_F27_BS2, LM_FL_F27_BS3, LM_FL_F27_Hammer, LM_FL_F27_HammerUp, LM_FL_F27_Layer1, LM_FL_F27_Layer3, LM_FL_F27_Parts, LM_FL_F27_Playfield, LM_FL_F27_Rod, LM_FL_F27_SnakeMOD, LM_FL_F27_UpPost, LM_FL_F27_cross, LM_FL_F27_crossHolder, LM_FL_F27_sparkyhead, LM_FL_F27_sw35, LM_FL_F27_sw43, LM_FL_F27_sw44, LM_FL_F27_sw45, LM_FL_F27_sw47p, LM_FL_F27_sw50p, LM_FL_F27_underPF)
Dim BL_FL_F28: BL_FL_F28=Array(LM_FL_F28_BR1, LM_FL_F28_BR2, LM_FL_F28_BR3, LM_FL_F28_BS1, LM_FL_F28_BS2, LM_FL_F28_BS3, LM_FL_F28_HammerUp, LM_FL_F28_Layer1, LM_FL_F28_Layer2b, LM_FL_F28_Layer3, LM_FL_F28_Parts, LM_FL_F28_Playfield, LM_FL_F28_Rod, LM_FL_F28_SnakeMOD, LM_FL_F28_UpPost, LM_FL_F28_cemeteryMOD, LM_FL_F28_cross, LM_FL_F28_sparkyhead, LM_FL_F28_sw35, LM_FL_F28_sw46, LM_FL_F28_sw47p, LM_FL_F28_underPF)
Dim BL_FL_F29: BL_FL_F29=Array(LM_FL_F29_BR1, LM_FL_F29_BR3, LM_FL_F29_BS3, LM_FL_F29_Parts, LM_FL_F29_Playfield, LM_FL_F29_snakeM1, LM_FL_F29_snakeM2, LM_FL_F29_underPF)
Dim BL_FL_F30: BL_FL_F30=Array(LM_FL_F30_BR1, LM_FL_F30_BR2, LM_FL_F30_BR3, LM_FL_F30_BS1, LM_FL_F30_BS2, LM_FL_F30_BS3, LM_FL_F30_Hammer, LM_FL_F30_HammerUp, LM_FL_F30_Layer1, LM_FL_F30_Layer2a, LM_FL_F30_Layer2b, LM_FL_F30_Layer3, LM_FL_F30_Parts, LM_FL_F30_Playfield, LM_FL_F30_Rod, LM_FL_F30_Rod_001, LM_FL_F30_Rspinner, LM_FL_F30_SnakeMOD, LM_FL_F30_cemeteryMOD, LM_FL_F30_snakeM1, LM_FL_F30_snakeM2, LM_FL_F30_sparkyhead, LM_FL_F30_sw40, LM_FL_F30_sw47p, LM_FL_F30_underPF)
Dim BL_FL_F31: BL_FL_F31=Array(LM_FL_F31_Hammer, LM_FL_F31_HammerUp, LM_FL_F31_Layer1, LM_FL_F31_Parts, LM_FL_F31_Playfield, LM_FL_F31_lockPost, LM_FL_F31_snakeM1, LM_FL_F31_sw60, LM_FL_F31_sw61, LM_FL_F31_underPF)
Dim BL_FL_F32: BL_FL_F32=Array(LM_FL_F32_BR3, LM_FL_F32_BS2, LM_FL_F32_BS3, LM_FL_F32_Cube, LM_FL_F32_Hammer, LM_FL_F32_HammerUp, LM_FL_F32_Layer1, LM_FL_F32_Layer2a, LM_FL_F32_Layer2b, LM_FL_F32_Layer3, LM_FL_F32_Parts, LM_FL_F32_Playfield, LM_FL_F32_Rod, LM_FL_F32_Rod_001, LM_FL_F32_cemeteryMOD, LM_FL_F32_cross, LM_FL_F32_sparkyhead, LM_FL_F32_sw35, LM_FL_F32_sw42, LM_FL_F32_sw47p, LM_FL_F32_sw61, LM_FL_F32_sw62, LM_FL_F32_underPF)
Dim BL_GIB_GIB1: BL_GIB_GIB1=Array(LM_GIB_GIB1_Parts, LM_GIB_GIB1_Playfield)
Dim BL_GIB_GIB2: BL_GIB_GIB2=Array(LM_GIB_GIB2_LEMK, LM_GIB_GIB2_LSling1, LM_GIB_GIB2_Parts, LM_GIB_GIB2_Playfield, LM_GIB_GIB2_sw1, LM_GIB_GIB2_sw25)
Dim BL_GIB_GIB3: BL_GIB_GIB3=Array(LM_GIB_GIB3_Parts, LM_GIB_GIB3_Playfield, LM_GIB_GIB3_REMK)
Dim BL_GIB_GIB4: BL_GIB_GIB4=Array(LM_GIB_GIB4_Parts, LM_GIB_GIB4_Playfield, LM_GIB_GIB4_REMK, LM_GIB_GIB4_RSling1, LM_GIB_GIB4_RSling2, LM_GIB_GIB4_sw28, LM_GIB_GIB4_sw29)
Dim BL_GIB_GIB5: BL_GIB_GIB5=Array(LM_GIB_GIB5_LPost, LM_GIB_GIB5_Layer1, LM_GIB_GIB5_Parts, LM_GIB_GIB5_Playfield, LM_GIB_GIB5_sw25)
Dim BL_GIB_GIB6: BL_GIB_GIB6=Array(LM_GIB_GIB6_Parts, LM_GIB_GIB6_Playfield, LM_GIB_GIB6_RPost)
Dim BL_GIB_GIB7: BL_GIB_GIB7=Array(LM_GIB_GIB7_BS2, LM_GIB_GIB7_Layer1, LM_GIB_GIB7_Layer3, LM_GIB_GIB7_Parts, LM_GIB_GIB7_Playfield, LM_GIB_GIB7_UpPost, LM_GIB_GIB7_cross, LM_GIB_GIB7_crossHolder, LM_GIB_GIB7_sparkyhead, LM_GIB_GIB7_sw43, LM_GIB_GIB7_sw44, LM_GIB_GIB7_underPF)
Dim BL_GIR_GIR1: BL_GIR_GIR1=Array(LM_GIR_GIR1_LEMK, LM_GIR_GIR1_LflipU, LM_GIR_GIR1_Parts, LM_GIR_GIR1_Playfield, LM_GIR_GIR1_sw1, LM_GIR_GIR1_sw24, LM_GIR_GIR1_sw25)
Dim BL_GIR_GIR2: BL_GIR_GIR2=Array(LM_GIR_GIR2_LEMK, LM_GIR_GIR2_LPost, LM_GIR_GIR2_LSling1, LM_GIR_GIR2_LSling2, LM_GIR_GIR2_Lflip, LM_GIR_GIR2_LflipU, LM_GIR_GIR2_Parts, LM_GIR_GIR2_Playfield, LM_GIR_GIR2_sw1, LM_GIR_GIR2_sw24, LM_GIR_GIR2_sw25)
Dim BL_GIR_GIR3: BL_GIR_GIR3=Array(LM_GIR_GIR3_Parts, LM_GIR_GIR3_Playfield, LM_GIR_GIR3_REMK, LM_GIR_GIR3_Rflip, LM_GIR_GIR3_RflipU, LM_GIR_GIR3_sw28, LM_GIR_GIR3_sw29)
Dim BL_GIR_GIR4: BL_GIR_GIR4=Array(LM_GIR_GIR4_Parts, LM_GIR_GIR4_Playfield, LM_GIR_GIR4_REMK, LM_GIR_GIR4_RPost, LM_GIR_GIR4_RSling1, LM_GIR_GIR4_RSling2, LM_GIR_GIR4_Rflip, LM_GIR_GIR4_RflipU, LM_GIR_GIR4_sw28, LM_GIR_GIR4_sw29)
Dim BL_GIR_GIR5: BL_GIR_GIR5=Array(LM_GIR_GIR5_LPost, LM_GIR_GIR5_LSling1, LM_GIR_GIR5_Layer1, LM_GIR_GIR5_Parts, LM_GIR_GIR5_Playfield, LM_GIR_GIR5_sw1, LM_GIR_GIR5_underPF)
Dim BL_GIR_GIR6: BL_GIR_GIR6=Array(LM_GIR_GIR6_Layer1, LM_GIR_GIR6_Parts, LM_GIR_GIR6_Playfield, LM_GIR_GIR6_sw28, LM_GIR_GIR6_underPF)
Dim BL_GIR_GIR7: BL_GIR_GIR7=Array(LM_GIR_GIR7_BR1, LM_GIR_GIR7_BR2, LM_GIR_GIR7_BS1, LM_GIR_GIR7_BS2, LM_GIR_GIR7_BS3, LM_GIR_GIR7_Layer1, LM_GIR_GIR7_Layer3, LM_GIR_GIR7_Parts, LM_GIR_GIR7_Playfield, LM_GIR_GIR7_UpPost, LM_GIR_GIR7_sparkyhead, LM_GIR_GIR7_sw44, LM_GIR_GIR7_sw45, LM_GIR_GIR7_underPF)
Dim BL_GIW_GIW1: BL_GIW_GIW1=Array(LM_GIW_GIW1_LEMK, LM_GIW_GIW1_LSling1, LM_GIW_GIW1_Lflip, LM_GIW_GIW1_LflipU, LM_GIW_GIW1_Parts, LM_GIW_GIW1_Playfield, LM_GIW_GIW1_Rflip, LM_GIW_GIW1_RflipU, LM_GIW_GIW1_sw24, LM_GIW_GIW1_sw29)
Dim BL_GIW_GIW11: BL_GIW_GIW11=Array(LM_GIW_GIW11_BR1, LM_GIW_GIW11_BR3, LM_GIW_GIW11_BS1, LM_GIW_GIW11_BS3, LM_GIW_GIW11_Hammer, LM_GIW_GIW11_HammerUp, LM_GIW_GIW11_Layer1, LM_GIW_GIW11_Layer2a, LM_GIW_GIW11_Lspinner, LM_GIW_GIW11_Parts, LM_GIW_GIW11_Playfield, LM_GIW_GIW11_Rod, LM_GIW_GIW11_cemeteryMOD, LM_GIW_GIW11_cross, LM_GIW_GIW11_sparkyhead, LM_GIW_GIW11_sw36, LM_GIW_GIW11_sw37, LM_GIW_GIW11_sw60, LM_GIW_GIW11_sw61, LM_GIW_GIW11_sw62, LM_GIW_GIW11_underPF)
Dim BL_GIW_GIW2: BL_GIW_GIW2=Array(LM_GIW_GIW2_LEMK, LM_GIW_GIW2_LSling1, LM_GIW_GIW2_LSling2, LM_GIW_GIW2_Lflip, LM_GIW_GIW2_LflipU, LM_GIW_GIW2_Parts, LM_GIW_GIW2_Playfield, LM_GIW_GIW2_RflipU, LM_GIW_GIW2_sw1, LM_GIW_GIW2_sw24, LM_GIW_GIW2_sw25)
Dim BL_GIW_GIW3: BL_GIW_GIW3=Array(LM_GIW_GIW3_Lflip, LM_GIW_GIW3_Parts, LM_GIW_GIW3_Playfield, LM_GIW_GIW3_REMK, LM_GIW_GIW3_RSling2, LM_GIW_GIW3_Rflip, LM_GIW_GIW3_RflipU)
Dim BL_GIW_GIW4: BL_GIW_GIW4=Array(LM_GIW_GIW4_LflipU, LM_GIW_GIW4_Parts, LM_GIW_GIW4_Playfield, LM_GIW_GIW4_REMK, LM_GIW_GIW4_RPost, LM_GIW_GIW4_RSling1, LM_GIW_GIW4_RSling2, LM_GIW_GIW4_Rflip, LM_GIW_GIW4_RflipU, LM_GIW_GIW4_sw28, LM_GIW_GIW4_sw29)
Dim BL_GIW_GIW5: BL_GIW_GIW5=Array(LM_GIW_GIW5_LPost, LM_GIW_GIW5_Layer1, LM_GIW_GIW5_Parts, LM_GIW_GIW5_Playfield, LM_GIW_GIW5_RSling1, LM_GIW_GIW5_sw1, LM_GIW_GIW5_sw24, LM_GIW_GIW5_sw25, LM_GIW_GIW5_underPF)
Dim BL_GIW_GIW6: BL_GIW_GIW6=Array(LM_GIW_GIW6_Layer1, LM_GIW_GIW6_Parts, LM_GIW_GIW6_Playfield, LM_GIW_GIW6_RPost, LM_GIW_GIW6_snakeM2, LM_GIW_GIW6_sw28, LM_GIW_GIW6_sw29, LM_GIW_GIW6_sw40, LM_GIW_GIW6_underPF)
Dim BL_GIW_GIW7: BL_GIW_GIW7=Array(LM_GIW_GIW7_BR1, LM_GIW_GIW7_BR2, LM_GIW_GIW7_BR3, LM_GIW_GIW7_BS1, LM_GIW_GIW7_BS2, LM_GIW_GIW7_BS3, LM_GIW_GIW7_Cube, LM_GIW_GIW7_Hammer, LM_GIW_GIW7_HammerUp, LM_GIW_GIW7_Layer1, LM_GIW_GIW7_Layer2a, LM_GIW_GIW7_Layer2b, LM_GIW_GIW7_Layer3, LM_GIW_GIW7_Lspinner, LM_GIW_GIW7_Parts, LM_GIW_GIW7_Playfield, LM_GIW_GIW7_Rod, LM_GIW_GIW7_Rod_001, LM_GIW_GIW7_Rspinner, LM_GIW_GIW7_SnakeMOD, LM_GIW_GIW7_UpPost, LM_GIW_GIW7_cemeteryMOD, LM_GIW_GIW7_cross, LM_GIW_GIW7_crossHolder, LM_GIW_GIW7_snakeM1, LM_GIW_GIW7_snakeM2, LM_GIW_GIW7_sparkyhead, LM_GIW_GIW7_sw0709, LM_GIW_GIW7_sw28, LM_GIW_GIW7_sw3, LM_GIW_GIW7_sw35, LM_GIW_GIW7_sw36, LM_GIW_GIW7_sw37, LM_GIW_GIW7_sw4, LM_GIW_GIW7_sw41, LM_GIW_GIW7_sw42, LM_GIW_GIW7_sw43, LM_GIW_GIW7_sw44, LM_GIW_GIW7_sw45, LM_GIW_GIW7_sw46, LM_GIW_GIW7_sw47p, LM_GIW_GIW7_sw50p, LM_GIW_GIW7_sw60, LM_GIW_GIW7_sw61, LM_GIW_GIW7_sw62, LM_GIW_GIW7_swplunger, LM_GIW_GIW7_underPF)
Dim BL_GIWU_GISpotL1: BL_GIWU_GISpotL1=Array(LM_GIWU_GISpotL1_LEMK, LM_GIWU_GISpotL1_Lflip, LM_GIWU_GISpotL1_LflipU, LM_GIWU_GISpotL1_Parts, LM_GIWU_GISpotL1_Playfield, LM_GIWU_GISpotL1_RflipU)
Dim BL_GIWU_GISpotL2: BL_GIWU_GISpotL2=Array(LM_GIWU_GISpotL2_LSling1, LM_GIWU_GISpotL2_Parts, LM_GIWU_GISpotL2_Playfield, LM_GIWU_GISpotL2_RPost, LM_GIWU_GISpotL2_Rod_001, LM_GIWU_GISpotL2_snakeM1, LM_GIWU_GISpotL2_underPF)
Dim BL_GIWU_GISpotR1: BL_GIWU_GISpotR1=Array(LM_GIWU_GISpotR1_LflipU, LM_GIWU_GISpotR1_Parts, LM_GIWU_GISpotR1_Playfield, LM_GIWU_GISpotR1_REMK, LM_GIWU_GISpotR1_RSling1, LM_GIWU_GISpotR1_Rflip, LM_GIWU_GISpotR1_RflipU)
Dim BL_GIWU_GISpotR2: BL_GIWU_GISpotR2=Array(LM_GIWU_GISpotR2_LPost, LM_GIWU_GISpotR2_Parts, LM_GIWU_GISpotR2_Playfield, LM_GIWU_GISpotR2_RSling2, LM_GIWU_GISpotR2_underPF)
Dim BL_GIWU_GISpotR3: BL_GIWU_GISpotR3=Array(LM_GIWU_GISpotR3_BR3, LM_GIWU_GISpotR3_BS3, LM_GIWU_GISpotR3_Hammer, LM_GIWU_GISpotR3_HammerUp, LM_GIWU_GISpotR3_Layer1, LM_GIWU_GISpotR3_Layer2a, LM_GIWU_GISpotR3_Parts, LM_GIWU_GISpotR3_Playfield, LM_GIWU_GISpotR3_snakeM1, LM_GIWU_GISpotR3_snakeM2, LM_GIWU_GISpotR3_sw40, LM_GIWU_GISpotR3_underPF)
Dim BL_L_CN11: BL_L_CN11=Array(LM_L_CN11_Layer1, LM_L_CN11_Layer3, LM_L_CN11_Lspinner, LM_L_CN11_Parts, LM_L_CN11_Playfield, LM_L_CN11_cemeteryMOD, LM_L_CN11_underPF)
Dim BL_L_CN13: BL_L_CN13=Array(LM_L_CN13_Parts, LM_L_CN13_Playfield, LM_L_CN13_sw36, LM_L_CN13_sw37, LM_L_CN13_sw60, LM_L_CN13_sw61, LM_L_CN13_sw62, LM_L_CN13_underPF)
Dim BL_L_CN19: BL_L_CN19=Array(LM_L_CN19_Layer1, LM_L_CN19_Parts, LM_L_CN19_Playfield, LM_L_CN19_cemeteryMOD, LM_L_CN19_underPF)
Dim BL_L_CN4: BL_L_CN4=Array(LM_L_CN4_Layer1, LM_L_CN4_Parts, LM_L_CN4_Playfield, LM_L_CN4_underPF)
Dim BL_L_CN5: BL_L_CN5=Array(LM_L_CN5_Parts, LM_L_CN5_Playfield, LM_L_CN5_snakeM2, LM_L_CN5_sw40, LM_L_CN5_sw41, LM_L_CN5_underPF)
Dim BL_L_CN9: BL_L_CN9=Array(LM_L_CN9_Parts, LM_L_CN9_Playfield, LM_L_CN9_Rspinner, LM_L_CN9_snakeM2, LM_L_CN9_sw41, LM_L_CN9_underPF)
Dim BL_L_L73: BL_L_L73=Array(LM_L_L73_BR3, LM_L_L73_BS3, LM_L_L73_Layer1, LM_L_L73_Layer2a, LM_L_L73_Layer2b, LM_L_L73_Layer3, LM_L_L73_Parts)
Dim BL_L_L74: BL_L_L74=Array(LM_L_L74_BS1, LM_L_L74_Layer2b, LM_L_L74_Parts)
Dim BL_L_L75: BL_L_L75=Array(LM_L_L75_BS2, LM_L_L75_Parts, LM_L_L75_sparkyhead)
Dim BL_L_l17: BL_L_l17=Array(LM_L_l17_Parts, LM_L_l17_Playfield, LM_L_l17_Rspinner, LM_L_l17_snakeM2, LM_L_l17_sw41, LM_L_l17_underPF)
Dim BL_L_l18: BL_L_l18=Array(LM_L_l18_Layer1, LM_L_l18_Parts, LM_L_l18_Playfield, LM_L_l18_Rspinner, LM_L_l18_sw41, LM_L_l18_underPF)
Dim BL_L_l19: BL_L_l19=Array(LM_L_l19_Parts, LM_L_l19_Playfield, LM_L_l19_Rspinner, LM_L_l19_sw41, LM_L_l19_underPF)
Dim BL_L_l21: BL_L_l21=Array(LM_L_l21_Hammer, LM_L_l21_HammerUp, LM_L_l21_Layer1, LM_L_l21_Layer3, LM_L_l21_Parts, LM_L_l21_Playfield, LM_L_l21_cemeteryMOD, LM_L_l21_sparkyhead, LM_L_l21_underPF)
Dim BL_L_l22: BL_L_l22=Array(LM_L_l22_BR2, LM_L_l22_BR3, LM_L_l22_BS1, LM_L_l22_BS3, LM_L_l22_Hammer, LM_L_l22_HammerUp, LM_L_l22_Layer1, LM_L_l22_Layer2a, LM_L_l22_Layer2b, LM_L_l22_Parts, LM_L_l22_Playfield, LM_L_l22_Rod, LM_L_l22_sparkyhead)
Dim BL_L_l23: BL_L_l23=Array(LM_L_l23_Parts, LM_L_l23_Playfield, LM_L_l23_underPF)
Dim BL_L_l24: BL_L_l24=Array(LM_L_l24_Parts, LM_L_l24_Playfield, LM_L_l24_underPF)
Dim BL_L_l25: BL_L_l25=Array(LM_L_l25_Parts, LM_L_l25_Playfield, LM_L_l25_Rspinner, LM_L_l25_underPF)
Dim BL_L_l26: BL_L_l26=Array(LM_L_l26_Parts, LM_L_l26_Playfield, LM_L_l26_RPost, LM_L_l26_sw28, LM_L_l26_underPF)
Dim BL_L_l27: BL_L_l27=Array(LM_L_l27_Parts, LM_L_l27_Playfield, LM_L_l27_RPost, LM_L_l27_sw28, LM_L_l27_sw29, LM_L_l27_underPF)
Dim BL_L_l28: BL_L_l28=Array(LM_L_l28_Hammer, LM_L_l28_HammerUp, LM_L_l28_Layer1, LM_L_l28_Parts, LM_L_l28_Playfield, LM_L_l28_Rspinner, LM_L_l28_snakeM2, LM_L_l28_sw40, LM_L_l28_sw41, LM_L_l28_underPF)
Dim BL_L_l29: BL_L_l29=Array(LM_L_l29_Hammer, LM_L_l29_HammerUp, LM_L_l29_Layer1, LM_L_l29_Parts, LM_L_l29_Playfield, LM_L_l29_Rspinner, LM_L_l29_snakeM2, LM_L_l29_sw40, LM_L_l29_sw41, LM_L_l29_underPF)
Dim BL_L_l30: BL_L_l30=Array(LM_L_l30_Hammer, LM_L_l30_Parts, LM_L_l30_Playfield, LM_L_l30_underPF)
Dim BL_L_l31: BL_L_l31=Array(LM_L_l31_Hammer, LM_L_l31_HammerUp, LM_L_l31_Layer1, LM_L_l31_Parts, LM_L_l31_Playfield, LM_L_l31_Rspinner, LM_L_l31_snakeM2, LM_L_l31_sw40, LM_L_l31_sw41, LM_L_l31_underPF)
Dim BL_L_l32: BL_L_l32=Array(LM_L_l32_Hammer, LM_L_l32_HammerUp, LM_L_l32_Layer1, LM_L_l32_Parts, LM_L_l32_Playfield, LM_L_l32_snakeM2, LM_L_l32_sw40, LM_L_l32_sw41, LM_L_l32_underPF)
Dim BL_L_l33: BL_L_l33=Array(LM_L_l33_Hammer, LM_L_l33_HammerUp, LM_L_l33_Layer1, LM_L_l33_Parts, LM_L_l33_Playfield, LM_L_l33_cemeteryMOD, LM_L_l33_sw37, LM_L_l33_sw61, LM_L_l33_sw62, LM_L_l33_underPF)
Dim BL_L_l34: BL_L_l34=Array(LM_L_l34_Hammer, LM_L_l34_HammerUp, LM_L_l34_Layer1, LM_L_l34_Parts, LM_L_l34_Playfield, LM_L_l34_cemeteryMOD, LM_L_l34_snakeM2, LM_L_l34_sw37, LM_L_l34_sw60, LM_L_l34_sw61, LM_L_l34_underPF)
Dim BL_L_l35: BL_L_l35=Array(LM_L_l35_Hammer, LM_L_l35_HammerUp, LM_L_l35_Parts, LM_L_l35_Playfield, LM_L_l35_snakeM2, LM_L_l35_sw37, LM_L_l35_sw60, LM_L_l35_sw61, LM_L_l35_sw62, LM_L_l35_underPF)
Dim BL_L_l37: BL_L_l37=Array(LM_L_l37_Parts, LM_L_l37_Playfield, LM_L_l37_REMK, LM_L_l37_RSling1, LM_L_l37_RSling2, LM_L_l37_underPF)
Dim BL_L_l38: BL_L_l38=Array(LM_L_l38_Parts, LM_L_l38_Playfield, LM_L_l38_RSling1, LM_L_l38_RSling2, LM_L_l38_underPF)
Dim BL_L_l39: BL_L_l39=Array(LM_L_l39_Parts, LM_L_l39_Playfield, LM_L_l39_RSling1, LM_L_l39_RSling2, LM_L_l39_underPF)
Dim BL_L_l40: BL_L_l40=Array(LM_L_l40_LSling2, LM_L_l40_Parts, LM_L_l40_Playfield, LM_L_l40_RSling1, LM_L_l40_RSling2, LM_L_l40_underPF)
Dim BL_L_l41: BL_L_l41=Array(LM_L_l41_LSling1, LM_L_l41_LSling2, LM_L_l41_Parts, LM_L_l41_Playfield, LM_L_l41_underPF)
Dim BL_L_l42: BL_L_l42=Array(LM_L_l42_LEMK, LM_L_l42_LPost, LM_L_l42_Parts, LM_L_l42_Playfield, LM_L_l42_sw1, LM_L_l42_sw25, LM_L_l42_underPF)
Dim BL_L_l43: BL_L_l43=Array(LM_L_l43_LPost, LM_L_l43_Layer1, LM_L_l43_Parts, LM_L_l43_Playfield, LM_L_l43_underPF)
Dim BL_L_l44: BL_L_l44=Array(LM_L_l44_LPost, LM_L_l44_Layer1, LM_L_l44_Parts, LM_L_l44_Playfield, LM_L_l44_underPF)
Dim BL_L_l45: BL_L_l45=Array(LM_L_l45_LPost, LM_L_l45_Layer1, LM_L_l45_Parts, LM_L_l45_Playfield, LM_L_l45_underPF)
Dim BL_L_l46: BL_L_l46=Array(LM_L_l46_Hammer, LM_L_l46_HammerUp, LM_L_l46_Parts, LM_L_l46_Playfield, LM_L_l46_underPF)
Dim BL_L_l47: BL_L_l47=Array(LM_L_l47_Hammer, LM_L_l47_HammerUp, LM_L_l47_Parts, LM_L_l47_Playfield, LM_L_l47_sw0709, LM_L_l47_underPF)
Dim BL_L_l48: BL_L_l48=Array(LM_L_l48_Hammer, LM_L_l48_HammerUp, LM_L_l48_Layer1, LM_L_l48_Parts, LM_L_l48_Playfield, LM_L_l48_snakeM2, LM_L_l48_sw0709, LM_L_l48_sw36, LM_L_l48_sw37, LM_L_l48_sw60, LM_L_l48_sw61, LM_L_l48_sw62, LM_L_l48_underPF)
Dim BL_L_l49: BL_L_l49=Array(LM_L_l49_Hammer, LM_L_l49_HammerUp, LM_L_l49_Layer1, LM_L_l49_Layer3, LM_L_l49_Lspinner, LM_L_l49_Parts, LM_L_l49_Playfield, LM_L_l49_cemeteryMOD, LM_L_l49_sw36, LM_L_l49_sw60, LM_L_l49_sw61, LM_L_l49_sw62, LM_L_l49_underPF)
Dim BL_L_l50: BL_L_l50=Array(LM_L_l50_Hammer, LM_L_l50_HammerUp, LM_L_l50_Layer1, LM_L_l50_Lspinner, LM_L_l50_Parts, LM_L_l50_Playfield, LM_L_l50_sw36, LM_L_l50_sw60, LM_L_l50_sw61, LM_L_l50_underPF)
Dim BL_L_l51: BL_L_l51=Array(LM_L_l51_Layer1, LM_L_l51_Lspinner, LM_L_l51_Parts, LM_L_l51_Playfield, LM_L_l51_sw0709, LM_L_l51_sw36, LM_L_l51_sw37, LM_L_l51_sw60, LM_L_l51_sw61, LM_L_l51_sw62, LM_L_l51_underPF)
Dim BL_L_l53: BL_L_l53=Array(LM_L_l53_Lflip, LM_L_l53_LflipU, LM_L_l53_Parts, LM_L_l53_Playfield, LM_L_l53_Rflip, LM_L_l53_RflipU, LM_L_l53_underPF)
Dim BL_L_l55: BL_L_l55=Array(LM_L_l55_Lflip, LM_L_l55_LflipU, LM_L_l55_Parts, LM_L_l55_Playfield, LM_L_l55_Rflip, LM_L_l55_RflipU, LM_L_l55_underPF)
Dim BL_L_l56: BL_L_l56=Array(LM_L_l56_Lflip, LM_L_l56_LflipU, LM_L_l56_Parts, LM_L_l56_Playfield, LM_L_l56_Rflip, LM_L_l56_RflipU, LM_L_l56_underPF)
Dim BL_L_l57: BL_L_l57=Array(LM_L_l57_Lflip, LM_L_l57_LflipU, LM_L_l57_Parts, LM_L_l57_Playfield, LM_L_l57_Rflip, LM_L_l57_RflipU, LM_L_l57_underPF)
Dim BL_L_l58: BL_L_l58=Array(LM_L_l58_Lflip, LM_L_l58_LflipU, LM_L_l58_Parts, LM_L_l58_Playfield, LM_L_l58_Rflip, LM_L_l58_RflipU, LM_L_l58_underPF)
Dim BL_L_l59: BL_L_l59=Array(LM_L_l59_LEMK, LM_L_l59_LSling1, LM_L_l59_LSling2, LM_L_l59_Lflip, LM_L_l59_LflipU, LM_L_l59_Parts, LM_L_l59_Playfield, LM_L_l59_underPF)
Dim BL_L_l60: BL_L_l60=Array(LM_L_l60_Lflip, LM_L_l60_LflipU, LM_L_l60_Parts, LM_L_l60_Playfield, LM_L_l60_Rflip, LM_L_l60_RflipU, LM_L_l60_underPF)
Dim BL_L_l61: BL_L_l61=Array(LM_L_l61_Lflip, LM_L_l61_LflipU, LM_L_l61_Parts, LM_L_l61_Playfield, LM_L_l61_Rflip, LM_L_l61_RflipU, LM_L_l61_underPF)
Dim BL_L_l62: BL_L_l62=Array(LM_L_l62_Parts, LM_L_l62_Playfield, LM_L_l62_REMK, LM_L_l62_RSling1, LM_L_l62_RSling2, LM_L_l62_Rflip, LM_L_l62_RflipU, LM_L_l62_underPF)
Dim BL_L_l63: BL_L_l63=Array(LM_L_l63_LPost, LM_L_l63_Parts, LM_L_l63_Playfield, LM_L_l63_sw1, LM_L_l63_sw24, LM_L_l63_sw25, LM_L_l63_underPF)
Dim BL_L_l64: BL_L_l64=Array(LM_L_l64_LPost, LM_L_l64_Layer1, LM_L_l64_Parts, LM_L_l64_Playfield, LM_L_l64_sw1, LM_L_l64_sw25, LM_L_l64_sw28, LM_L_l64_underPF)
Dim BL_L_l65: BL_L_l65=Array(LM_L_l65_Hammer, LM_L_l65_HammerUp, LM_L_l65_Layer1, LM_L_l65_Layer3, LM_L_l65_Parts, LM_L_l65_Playfield, LM_L_l65_cemeteryMOD, LM_L_l65_cross, LM_L_l65_sw36, LM_L_l65_sw37, LM_L_l65_sw60, LM_L_l65_sw61, LM_L_l65_sw62, LM_L_l65_underPF)
Dim BL_L_l66: BL_L_l66=Array(LM_L_l66_BR1, LM_L_l66_BR2, LM_L_l66_BR3, LM_L_l66_BS1, LM_L_l66_BS2, LM_L_l66_BS3, LM_L_l66_Layer1, LM_L_l66_Layer2b, LM_L_l66_Layer3, LM_L_l66_Parts, LM_L_l66_Playfield, LM_L_l66_underPF)
Dim BL_L_l67: BL_L_l67=Array(LM_L_l67_BR1, LM_L_l67_BR2, LM_L_l67_BS1, LM_L_l67_BS2, LM_L_l67_BS3, LM_L_l67_Layer2b, LM_L_l67_Parts, LM_L_l67_Playfield, LM_L_l67_sparkyhead, LM_L_l67_underPF)
Dim BL_L_l68: BL_L_l68=Array(LM_L_l68_BR3, LM_L_l68_BS3, LM_L_l68_Hammer, LM_L_l68_HammerUp, LM_L_l68_Layer1, LM_L_l68_Parts, LM_L_l68_Playfield, LM_L_l68_Rod, LM_L_l68_sparkyhead, LM_L_l68_sw35, LM_L_l68_underPF)
Dim BL_L_l69: BL_L_l69=Array(LM_L_l69_Hammer, LM_L_l69_HammerUp, LM_L_l69_Layer1, LM_L_l69_Parts, LM_L_l69_Playfield, LM_L_l69_sparkyhead, LM_L_l69_sw35, LM_L_l69_underPF)
Dim BL_L_l70: BL_L_l70=Array(LM_L_l70_Hammer, LM_L_l70_HammerUp, LM_L_l70_Layer1, LM_L_l70_Parts, LM_L_l70_Playfield, LM_L_l70_sparkyhead, LM_L_l70_sw35, LM_L_l70_sw61, LM_L_l70_underPF)
Dim BL_L_l71: BL_L_l71=Array(LM_L_l71_Hammer, LM_L_l71_HammerUp, LM_L_l71_Layer1, LM_L_l71_Layer3, LM_L_l71_Lspinner, LM_L_l71_Parts, LM_L_l71_Playfield, LM_L_l71_cemeteryMOD, LM_L_l71_cross, LM_L_l71_sw36, LM_L_l71_sw37, LM_L_l71_sw60, LM_L_l71_sw61, LM_L_l71_sw62, LM_L_l71_underPF)
Dim BL_L_l77: BL_L_l77=Array(LM_L_l77_Layer1, LM_L_l77_Parts, LM_L_l77_Playfield, LM_L_l77_sw0709, LM_L_l77_sw36, LM_L_l77_sw60, LM_L_l77_underPF)
Dim BL_L_l78: BL_L_l78=Array(LM_L_l78_Hammer, LM_L_l78_Layer1, LM_L_l78_Parts, LM_L_l78_Playfield, LM_L_l78_sw0709, LM_L_l78_sw3, LM_L_l78_underPF)
Dim BL_L_l79: BL_L_l79=Array(LM_L_l79_Layer1, LM_L_l79_Parts, LM_L_l79_Playfield, LM_L_l79_sw0709, LM_L_l79_underPF)
Dim BL_L_l80: BL_L_l80=Array(LM_L_l80_Layer1, LM_L_l80_Parts, LM_L_l80_Playfield, LM_L_l80_sw0709, LM_L_l80_underPF)
Dim BL_L_optos: BL_L_optos=Array(LM_L_optos_Layer1, LM_L_optos_Parts, LM_L_optos_Playfield, LM_L_optos_lockPost, LM_L_optos_underPF)
Dim BL_Room: BL_Room=Array(BM_BR1, BM_BR2, BM_BR3, BM_BS1, BM_BS2, BM_BS3, BM_Cube, BM_Hammer, BM_HammerUp, BM_LEMK, BM_LPost, BM_LSling1, BM_LSling2, BM_Layer1, BM_Layer2a, BM_Layer2b, BM_Layer3, BM_Lflip, BM_LflipU, BM_Lspinner, BM_Parts, BM_Playfield, BM_REMK, BM_RPost, BM_RSling1, BM_RSling2, BM_Rflip, BM_RflipU, BM_Rod, BM_Rod_001, BM_Rspinner, BM_SnakeMOD, BM_UpPost, BM_cemeteryMOD, BM_cross, BM_crossHolder, BM_lockPost, BM_snakeM1, BM_snakeM2, BM_sparkyhead, BM_sw0709, BM_sw1, BM_sw24, BM_sw25, BM_sw28, BM_sw29, BM_sw3, BM_sw35, BM_sw36, BM_sw37, BM_sw4, BM_sw40, BM_sw41, BM_sw42, BM_sw43, BM_sw44, BM_sw45, BM_sw46, BM_sw47p, BM_sw50p, BM_sw60, BM_sw61, BM_sw62, BM_swplunger, BM_underPF)
' Global arrays
'Dim BG_Bakemap: BG_Bakemap=Array(BM_BR1, BM_BR2, BM_BR3, BM_BS1, BM_BS2, BM_BS3, BM_Cube, BM_Hammer, BM_HammerUp, BM_LEMK, BM_LPost, BM_LSling1, BM_LSling2, BM_Layer1, BM_Layer2a, BM_Layer2b, BM_Layer3, BM_Lflip, BM_LflipU, BM_Lspinner, BM_Parts, BM_Playfield, BM_REMK, BM_RPost, BM_RSling1, BM_RSling2, BM_Rflip, BM_RflipU, BM_Rod, BM_Rod_001, BM_Rspinner, BM_SnakeMOD, BM_UpPost, BM_cemeteryMOD, BM_cross, BM_crossHolder, BM_lockPost, BM_snakeM1, BM_snakeM2, BM_sparkyhead, BM_sw0709, BM_sw1, BM_sw24, BM_sw25, BM_sw28, BM_sw29, BM_sw3, BM_sw35, BM_sw36, BM_sw37, BM_sw4, BM_sw40, BM_sw41, BM_sw42, BM_sw43, BM_sw44, BM_sw45, BM_sw46, BM_sw47p, BM_sw50p, BM_sw60, BM_sw61, BM_sw62, BM_swplunger, BM_underPF)
'Dim BG_Lightmap: BG_Lightmap=Array(LM_B_BR1, LM_B_BR2, LM_B_BR3, LM_B_BS1, LM_B_BS2, LM_B_BS3, LM_B_Hammer, LM_B_HammerUp, LM_B_Layer1, LM_B_Layer2a, LM_B_Layer2b, LM_B_Layer3, LM_B_Lspinner, LM_B_Parts, LM_B_Playfield, LM_B_Rod, LM_B_Rod_001, LM_B_SnakeMOD, LM_B_UpPost, LM_B_cemeteryMOD, LM_B_cross, LM_B_crossHolder, LM_B_sparkyhead, LM_B_sw47p, LM_B_sw61, LM_B_sw62, LM_B_underPF, LM_FL_F19_HammerUp, LM_FL_F19_Layer1, LM_FL_F19_Layer3, LM_FL_F19_Lspinner, LM_FL_F19_Parts, LM_FL_F19_Playfield, LM_FL_F19_cemeteryMOD, LM_FL_F19_cross, LM_FL_F19_crossHolder, LM_FL_F19_sw62, LM_FL_F21_BS1, LM_FL_F21_Layer1, LM_FL_F21_Layer3, LM_FL_F21_Parts, LM_FL_F21_Playfield, LM_FL_F21_Rod, LM_FL_F21_UpPost, LM_FL_F21_cemeteryMOD, LM_FL_F21_sparkyhead, LM_FL_F21_sw46, LM_FL_F21_sw50p, LM_FL_F21_underPF, LM_FL_F22_BR2, LM_FL_F22_BS1, LM_FL_F22_BS2, LM_FL_F22_BS3, LM_FL_F22_Layer1, LM_FL_F22_Layer2a, LM_FL_F22_Layer2b, LM_FL_F22_Layer3, LM_FL_F22_Parts, LM_FL_F22_Playfield, LM_FL_F22_Rod_001, LM_FL_F22_SnakeMOD, _
'	LM_FL_F22_UpPost, LM_FL_F22_sparkyhead, LM_FL_F22_sw43, LM_FL_F22_sw44, LM_FL_F22_sw45, LM_FL_F22_sw46, LM_FL_F22_sw47p, LM_FL_F23_Layer1, LM_FL_F23_Layer3, LM_FL_F23_Parts, LM_FL_F23_Playfield, LM_FL_F23_cemeteryMOD, LM_FL_F23_sparkyhead, LM_FL_F23_sw43, LM_FL_F23_underPF, LM_FL_F25_BR1, LM_FL_F25_BR2, LM_FL_F25_BR3, LM_FL_F25_BS1, LM_FL_F25_BS2, LM_FL_F25_BS3, LM_FL_F25_Layer1, LM_FL_F25_Layer2a, LM_FL_F25_Layer2b, LM_FL_F25_Parts, LM_FL_F25_Playfield, LM_FL_F25_SnakeMOD, LM_FL_F25_sparkyhead, LM_FL_F25_underPF, LM_FL_F26_Hammer, LM_FL_F26_HammerUp, LM_FL_F26_Layer1, LM_FL_F26_Layer3, LM_FL_F26_Lspinner, LM_FL_F26_Parts, LM_FL_F26_Playfield, LM_FL_F26_UpPost, LM_FL_F26_cemeteryMOD, LM_FL_F26_cross, LM_FL_F26_crossHolder, LM_FL_F26_sparkyhead, LM_FL_F26_sw35, LM_FL_F26_sw43, LM_FL_F26_sw50p, LM_FL_F26_sw60, LM_FL_F26_sw61, LM_FL_F26_sw62, LM_FL_F26_underPF, LM_FL_F27_BR1, LM_FL_F27_BR2, LM_FL_F27_BR3, LM_FL_F27_BS1, LM_FL_F27_BS2, LM_FL_F27_BS3, LM_FL_F27_Hammer, LM_FL_F27_HammerUp, LM_FL_F27_Layer1, _
'	LM_FL_F27_Layer3, LM_FL_F27_Parts, LM_FL_F27_Playfield, LM_FL_F27_Rod, LM_FL_F27_SnakeMOD, LM_FL_F27_UpPost, LM_FL_F27_cross, LM_FL_F27_crossHolder, LM_FL_F27_sparkyhead, LM_FL_F27_sw35, LM_FL_F27_sw43, LM_FL_F27_sw44, LM_FL_F27_sw45, LM_FL_F27_sw47p, LM_FL_F27_sw50p, LM_FL_F27_underPF, LM_FL_F28_BR1, LM_FL_F28_BR2, LM_FL_F28_BR3, LM_FL_F28_BS1, LM_FL_F28_BS2, LM_FL_F28_BS3, LM_FL_F28_HammerUp, LM_FL_F28_Layer1, LM_FL_F28_Layer2b, LM_FL_F28_Layer3, LM_FL_F28_Parts, LM_FL_F28_Playfield, LM_FL_F28_Rod, LM_FL_F28_SnakeMOD, LM_FL_F28_UpPost, LM_FL_F28_cemeteryMOD, LM_FL_F28_cross, LM_FL_F28_sparkyhead, LM_FL_F28_sw35, LM_FL_F28_sw46, LM_FL_F28_sw47p, LM_FL_F28_underPF, LM_FL_F29_BR1, LM_FL_F29_BR3, LM_FL_F29_BS3, LM_FL_F29_Parts, LM_FL_F29_Playfield, LM_FL_F29_snakeM1, LM_FL_F29_snakeM2, LM_FL_F29_underPF, LM_FL_F30_BR1, LM_FL_F30_BR2, LM_FL_F30_BR3, LM_FL_F30_BS1, LM_FL_F30_BS2, LM_FL_F30_BS3, LM_FL_F30_Hammer, LM_FL_F30_HammerUp, LM_FL_F30_Layer1, LM_FL_F30_Layer2a, LM_FL_F30_Layer2b, LM_FL_F30_Layer3, _
'	LM_FL_F30_Parts, LM_FL_F30_Playfield, LM_FL_F30_Rod, LM_FL_F30_Rod_001, LM_FL_F30_Rspinner, LM_FL_F30_SnakeMOD, LM_FL_F30_cemeteryMOD, LM_FL_F30_snakeM1, LM_FL_F30_snakeM2, LM_FL_F30_sparkyhead, LM_FL_F30_sw40, LM_FL_F30_sw47p, LM_FL_F30_underPF, LM_FL_F31_Hammer, LM_FL_F31_HammerUp, LM_FL_F31_Layer1, LM_FL_F31_Parts, LM_FL_F31_Playfield, LM_FL_F31_lockPost, LM_FL_F31_snakeM1, LM_FL_F31_sw60, LM_FL_F31_sw61, LM_FL_F31_underPF, LM_FL_F32_BR3, LM_FL_F32_BS2, LM_FL_F32_BS3, LM_FL_F32_Cube, LM_FL_F32_Hammer, LM_FL_F32_HammerUp, LM_FL_F32_Layer1, LM_FL_F32_Layer2a, LM_FL_F32_Layer2b, LM_FL_F32_Layer3, LM_FL_F32_Parts, LM_FL_F32_Playfield, LM_FL_F32_Rod, LM_FL_F32_Rod_001, LM_FL_F32_cemeteryMOD, LM_FL_F32_cross, LM_FL_F32_sparkyhead, LM_FL_F32_sw35, LM_FL_F32_sw42, LM_FL_F32_sw47p, LM_FL_F32_sw61, LM_FL_F32_sw62, LM_FL_F32_underPF, LM_GIB_GIB1_Parts, LM_GIB_GIB1_Playfield, LM_GIB_GIB2_LEMK, LM_GIB_GIB2_LSling1, LM_GIB_GIB2_Parts, LM_GIB_GIB2_Playfield, LM_GIB_GIB2_sw1, LM_GIB_GIB2_sw25, LM_GIB_GIB3_Parts, _
'	LM_GIB_GIB3_Playfield, LM_GIB_GIB3_REMK, LM_GIB_GIB4_Parts, LM_GIB_GIB4_Playfield, LM_GIB_GIB4_REMK, LM_GIB_GIB4_RSling1, LM_GIB_GIB4_RSling2, LM_GIB_GIB4_sw28, LM_GIB_GIB4_sw29, LM_GIB_GIB5_LPost, LM_GIB_GIB5_Layer1, LM_GIB_GIB5_Parts, LM_GIB_GIB5_Playfield, LM_GIB_GIB5_sw25, LM_GIB_GIB6_Parts, LM_GIB_GIB6_Playfield, LM_GIB_GIB6_RPost, LM_GIB_GIB7_BS2, LM_GIB_GIB7_Layer1, LM_GIB_GIB7_Layer3, LM_GIB_GIB7_Parts, LM_GIB_GIB7_Playfield, LM_GIB_GIB7_UpPost, LM_GIB_GIB7_cross, LM_GIB_GIB7_crossHolder, LM_GIB_GIB7_sparkyhead, LM_GIB_GIB7_sw43, LM_GIB_GIB7_sw44, LM_GIB_GIB7_underPF, LM_GIR_GIR1_LEMK, LM_GIR_GIR1_LflipU, LM_GIR_GIR1_Parts, LM_GIR_GIR1_Playfield, LM_GIR_GIR1_sw1, LM_GIR_GIR1_sw24, LM_GIR_GIR1_sw25, LM_GIR_GIR2_LEMK, LM_GIR_GIR2_LPost, LM_GIR_GIR2_LSling1, LM_GIR_GIR2_LSling2, LM_GIR_GIR2_Lflip, LM_GIR_GIR2_LflipU, LM_GIR_GIR2_Parts, LM_GIR_GIR2_Playfield, LM_GIR_GIR2_sw1, LM_GIR_GIR2_sw24, LM_GIR_GIR2_sw25, LM_GIR_GIR3_Parts, LM_GIR_GIR3_Playfield, LM_GIR_GIR3_REMK, LM_GIR_GIR3_Rflip, _
'	LM_GIR_GIR3_RflipU, LM_GIR_GIR3_sw28, LM_GIR_GIR3_sw29, LM_GIR_GIR4_Parts, LM_GIR_GIR4_Playfield, LM_GIR_GIR4_REMK, LM_GIR_GIR4_RPost, LM_GIR_GIR4_RSling1, LM_GIR_GIR4_RSling2, LM_GIR_GIR4_Rflip, LM_GIR_GIR4_RflipU, LM_GIR_GIR4_sw28, LM_GIR_GIR4_sw29, LM_GIR_GIR5_LPost, LM_GIR_GIR5_LSling1, LM_GIR_GIR5_Layer1, LM_GIR_GIR5_Parts, LM_GIR_GIR5_Playfield, LM_GIR_GIR5_sw1, LM_GIR_GIR5_underPF, LM_GIR_GIR6_Layer1, LM_GIR_GIR6_Parts, LM_GIR_GIR6_Playfield, LM_GIR_GIR6_sw28, LM_GIR_GIR6_underPF, LM_GIR_GIR7_BR1, LM_GIR_GIR7_BR2, LM_GIR_GIR7_BS1, LM_GIR_GIR7_BS2, LM_GIR_GIR7_BS3, LM_GIR_GIR7_Layer1, LM_GIR_GIR7_Layer3, LM_GIR_GIR7_Parts, LM_GIR_GIR7_Playfield, LM_GIR_GIR7_UpPost, LM_GIR_GIR7_sparkyhead, LM_GIR_GIR7_sw44, LM_GIR_GIR7_sw45, LM_GIR_GIR7_underPF, LM_GIW_GIW1_LEMK, LM_GIW_GIW1_LSling1, LM_GIW_GIW1_Lflip, LM_GIW_GIW1_LflipU, LM_GIW_GIW1_Parts, LM_GIW_GIW1_Playfield, LM_GIW_GIW1_Rflip, LM_GIW_GIW1_RflipU, LM_GIW_GIW1_sw24, LM_GIW_GIW1_sw29, LM_GIW_GIW11_BR1, LM_GIW_GIW11_BR3, LM_GIW_GIW11_BS1, _
'	LM_GIW_GIW11_BS3, LM_GIW_GIW11_Hammer, LM_GIW_GIW11_HammerUp, LM_GIW_GIW11_Layer1, LM_GIW_GIW11_Layer2a, LM_GIW_GIW11_Lspinner, LM_GIW_GIW11_Parts, LM_GIW_GIW11_Playfield, LM_GIW_GIW11_Rod, LM_GIW_GIW11_cemeteryMOD, LM_GIW_GIW11_cross, LM_GIW_GIW11_sparkyhead, LM_GIW_GIW11_sw36, LM_GIW_GIW11_sw37, LM_GIW_GIW11_sw60, LM_GIW_GIW11_sw61, LM_GIW_GIW11_sw62, LM_GIW_GIW11_underPF, LM_GIW_GIW2_LEMK, LM_GIW_GIW2_LSling1, LM_GIW_GIW2_LSling2, LM_GIW_GIW2_Lflip, LM_GIW_GIW2_LflipU, LM_GIW_GIW2_Parts, LM_GIW_GIW2_Playfield, LM_GIW_GIW2_RflipU, LM_GIW_GIW2_sw1, LM_GIW_GIW2_sw24, LM_GIW_GIW2_sw25, LM_GIW_GIW3_Lflip, LM_GIW_GIW3_Parts, LM_GIW_GIW3_Playfield, LM_GIW_GIW3_REMK, LM_GIW_GIW3_RSling2, LM_GIW_GIW3_Rflip, LM_GIW_GIW3_RflipU, LM_GIW_GIW4_LflipU, LM_GIW_GIW4_Parts, LM_GIW_GIW4_Playfield, LM_GIW_GIW4_REMK, LM_GIW_GIW4_RPost, LM_GIW_GIW4_RSling1, LM_GIW_GIW4_RSling2, LM_GIW_GIW4_Rflip, LM_GIW_GIW4_RflipU, LM_GIW_GIW4_sw28, LM_GIW_GIW4_sw29, LM_GIW_GIW5_LPost, LM_GIW_GIW5_Layer1, LM_GIW_GIW5_Parts, _
'	LM_GIW_GIW5_Playfield, LM_GIW_GIW5_RSling1, LM_GIW_GIW5_sw1, LM_GIW_GIW5_sw24, LM_GIW_GIW5_sw25, LM_GIW_GIW5_underPF, LM_GIW_GIW6_Layer1, LM_GIW_GIW6_Parts, LM_GIW_GIW6_Playfield, LM_GIW_GIW6_RPost, LM_GIW_GIW6_snakeM2, LM_GIW_GIW6_sw28, LM_GIW_GIW6_sw29, LM_GIW_GIW6_sw40, LM_GIW_GIW6_underPF, LM_GIW_GIW7_BR1, LM_GIW_GIW7_BR2, LM_GIW_GIW7_BR3, LM_GIW_GIW7_BS1, LM_GIW_GIW7_BS2, LM_GIW_GIW7_BS3, LM_GIW_GIW7_Cube, LM_GIW_GIW7_Hammer, LM_GIW_GIW7_HammerUp, LM_GIW_GIW7_Layer1, LM_GIW_GIW7_Layer2a, LM_GIW_GIW7_Layer2b, LM_GIW_GIW7_Layer3, LM_GIW_GIW7_Lspinner, LM_GIW_GIW7_Parts, LM_GIW_GIW7_Playfield, LM_GIW_GIW7_Rod, LM_GIW_GIW7_Rod_001, LM_GIW_GIW7_Rspinner, LM_GIW_GIW7_SnakeMOD, LM_GIW_GIW7_UpPost, LM_GIW_GIW7_cemeteryMOD, LM_GIW_GIW7_cross, LM_GIW_GIW7_crossHolder, LM_GIW_GIW7_snakeM1, LM_GIW_GIW7_snakeM2, LM_GIW_GIW7_sparkyhead, LM_GIW_GIW7_sw0709, LM_GIW_GIW7_sw28, LM_GIW_GIW7_sw3, LM_GIW_GIW7_sw35, LM_GIW_GIW7_sw36, LM_GIW_GIW7_sw37, LM_GIW_GIW7_sw4, LM_GIW_GIW7_sw41, LM_GIW_GIW7_sw42, LM_GIW_GIW7_sw43, _
'	LM_GIW_GIW7_sw44, LM_GIW_GIW7_sw45, LM_GIW_GIW7_sw46, LM_GIW_GIW7_sw47p, LM_GIW_GIW7_sw50p, LM_GIW_GIW7_sw60, LM_GIW_GIW7_sw61, LM_GIW_GIW7_sw62, LM_GIW_GIW7_swplunger, LM_GIW_GIW7_underPF, LM_GIWU_GISpotL1_LEMK, LM_GIWU_GISpotL1_Lflip, LM_GIWU_GISpotL1_LflipU, LM_GIWU_GISpotL1_Parts, LM_GIWU_GISpotL1_Playfield, LM_GIWU_GISpotL1_RflipU, LM_GIWU_GISpotL2_LSling1, LM_GIWU_GISpotL2_Parts, LM_GIWU_GISpotL2_Playfield, LM_GIWU_GISpotL2_RPost, LM_GIWU_GISpotL2_Rod_001, LM_GIWU_GISpotL2_snakeM1, LM_GIWU_GISpotL2_underPF, LM_GIWU_GISpotR1_LflipU, LM_GIWU_GISpotR1_Parts, LM_GIWU_GISpotR1_Playfield, LM_GIWU_GISpotR1_REMK, LM_GIWU_GISpotR1_RSling1, LM_GIWU_GISpotR1_Rflip, LM_GIWU_GISpotR1_RflipU, LM_GIWU_GISpotR2_LPost, LM_GIWU_GISpotR2_Parts, LM_GIWU_GISpotR2_Playfield, LM_GIWU_GISpotR2_RSling2, LM_GIWU_GISpotR2_underPF, LM_GIWU_GISpotR3_BR3, LM_GIWU_GISpotR3_BS3, LM_GIWU_GISpotR3_Hammer, LM_GIWU_GISpotR3_HammerUp, LM_GIWU_GISpotR3_Layer1, LM_GIWU_GISpotR3_Layer2a, LM_GIWU_GISpotR3_Parts, LM_GIWU_GISpotR3_Playfield, _
'	LM_GIWU_GISpotR3_snakeM1, LM_GIWU_GISpotR3_snakeM2, LM_GIWU_GISpotR3_sw40, LM_GIWU_GISpotR3_underPF, LM_L_CN11_Layer1, LM_L_CN11_Layer3, LM_L_CN11_Lspinner, LM_L_CN11_Parts, LM_L_CN11_Playfield, LM_L_CN11_cemeteryMOD, LM_L_CN11_underPF, LM_L_CN13_Parts, LM_L_CN13_Playfield, LM_L_CN13_sw36, LM_L_CN13_sw37, LM_L_CN13_sw60, LM_L_CN13_sw61, LM_L_CN13_sw62, LM_L_CN13_underPF, LM_L_CN19_Layer1, LM_L_CN19_Parts, LM_L_CN19_Playfield, LM_L_CN19_cemeteryMOD, LM_L_CN19_underPF, LM_L_CN4_Layer1, LM_L_CN4_Parts, LM_L_CN4_Playfield, LM_L_CN4_underPF, LM_L_CN5_Parts, LM_L_CN5_Playfield, LM_L_CN5_snakeM2, LM_L_CN5_sw40, LM_L_CN5_sw41, LM_L_CN5_underPF, LM_L_CN9_Parts, LM_L_CN9_Playfield, LM_L_CN9_Rspinner, LM_L_CN9_snakeM2, LM_L_CN9_sw41, LM_L_CN9_underPF, LM_L_L73_BR3, LM_L_L73_BS3, LM_L_L73_Layer1, LM_L_L73_Layer2a, LM_L_L73_Layer2b, LM_L_L73_Layer3, LM_L_L73_Parts, LM_L_L74_BS1, LM_L_L74_Layer2b, LM_L_L74_Parts, LM_L_L75_BS2, LM_L_L75_Parts, LM_L_L75_sparkyhead, LM_L_l17_Parts, LM_L_l17_Playfield, LM_L_l17_Rspinner, _
'	LM_L_l17_snakeM2, LM_L_l17_sw41, LM_L_l17_underPF, LM_L_l18_Layer1, LM_L_l18_Parts, LM_L_l18_Playfield, LM_L_l18_Rspinner, LM_L_l18_sw41, LM_L_l18_underPF, LM_L_l19_Parts, LM_L_l19_Playfield, LM_L_l19_Rspinner, LM_L_l19_sw41, LM_L_l19_underPF, LM_L_l21_Hammer, LM_L_l21_HammerUp, LM_L_l21_Layer1, LM_L_l21_Layer3, LM_L_l21_Parts, LM_L_l21_Playfield, LM_L_l21_cemeteryMOD, LM_L_l21_sparkyhead, LM_L_l21_underPF, LM_L_l22_BR2, LM_L_l22_BR3, LM_L_l22_BS1, LM_L_l22_BS3, LM_L_l22_Hammer, LM_L_l22_HammerUp, LM_L_l22_Layer1, LM_L_l22_Layer2a, LM_L_l22_Layer2b, LM_L_l22_Parts, LM_L_l22_Playfield, LM_L_l22_Rod, LM_L_l22_sparkyhead, LM_L_l23_Parts, LM_L_l23_Playfield, LM_L_l23_underPF, LM_L_l24_Parts, LM_L_l24_Playfield, LM_L_l24_underPF, LM_L_l25_Parts, LM_L_l25_Playfield, LM_L_l25_Rspinner, LM_L_l25_underPF, LM_L_l26_Parts, LM_L_l26_Playfield, LM_L_l26_RPost, LM_L_l26_sw28, LM_L_l26_underPF, LM_L_l27_Parts, LM_L_l27_Playfield, LM_L_l27_RPost, LM_L_l27_sw28, LM_L_l27_sw29, LM_L_l27_underPF, LM_L_l28_Hammer, _
'	LM_L_l28_HammerUp, LM_L_l28_Layer1, LM_L_l28_Parts, LM_L_l28_Playfield, LM_L_l28_Rspinner, LM_L_l28_snakeM2, LM_L_l28_sw40, LM_L_l28_sw41, LM_L_l28_underPF, LM_L_l29_Hammer, LM_L_l29_HammerUp, LM_L_l29_Layer1, LM_L_l29_Parts, LM_L_l29_Playfield, LM_L_l29_Rspinner, LM_L_l29_snakeM2, LM_L_l29_sw40, LM_L_l29_sw41, LM_L_l29_underPF, LM_L_l30_Hammer, LM_L_l30_Parts, LM_L_l30_Playfield, LM_L_l30_underPF, LM_L_l31_Hammer, LM_L_l31_HammerUp, LM_L_l31_Layer1, LM_L_l31_Parts, LM_L_l31_Playfield, LM_L_l31_Rspinner, LM_L_l31_snakeM2, LM_L_l31_sw40, LM_L_l31_sw41, LM_L_l31_underPF, LM_L_l32_Hammer, LM_L_l32_HammerUp, LM_L_l32_Layer1, LM_L_l32_Parts, LM_L_l32_Playfield, LM_L_l32_snakeM2, LM_L_l32_sw40, LM_L_l32_sw41, LM_L_l32_underPF, LM_L_l33_Hammer, LM_L_l33_HammerUp, LM_L_l33_Layer1, LM_L_l33_Parts, LM_L_l33_Playfield, LM_L_l33_cemeteryMOD, LM_L_l33_sw37, LM_L_l33_sw61, LM_L_l33_sw62, LM_L_l33_underPF, LM_L_l34_Hammer, LM_L_l34_HammerUp, LM_L_l34_Layer1, LM_L_l34_Parts, LM_L_l34_Playfield, LM_L_l34_cemeteryMOD, _
'	LM_L_l34_snakeM2, LM_L_l34_sw37, LM_L_l34_sw60, LM_L_l34_sw61, LM_L_l34_underPF, LM_L_l35_Hammer, LM_L_l35_HammerUp, LM_L_l35_Parts, LM_L_l35_Playfield, LM_L_l35_snakeM2, LM_L_l35_sw37, LM_L_l35_sw60, LM_L_l35_sw61, LM_L_l35_sw62, LM_L_l35_underPF, LM_L_l37_Parts, LM_L_l37_Playfield, LM_L_l37_REMK, LM_L_l37_RSling1, LM_L_l37_RSling2, LM_L_l37_underPF, LM_L_l38_Parts, LM_L_l38_Playfield, LM_L_l38_RSling1, LM_L_l38_RSling2, LM_L_l38_underPF, LM_L_l39_Parts, LM_L_l39_Playfield, LM_L_l39_RSling1, LM_L_l39_RSling2, LM_L_l39_underPF, LM_L_l40_LSling2, LM_L_l40_Parts, LM_L_l40_Playfield, LM_L_l40_RSling1, LM_L_l40_RSling2, LM_L_l40_underPF, LM_L_l41_LSling1, LM_L_l41_LSling2, LM_L_l41_Parts, LM_L_l41_Playfield, LM_L_l41_underPF, LM_L_l42_LEMK, LM_L_l42_LPost, LM_L_l42_Parts, LM_L_l42_Playfield, LM_L_l42_sw1, LM_L_l42_sw25, LM_L_l42_underPF, LM_L_l43_LPost, LM_L_l43_Layer1, LM_L_l43_Parts, LM_L_l43_Playfield, LM_L_l43_underPF, LM_L_l44_LPost, LM_L_l44_Layer1, LM_L_l44_Parts, LM_L_l44_Playfield, LM_L_l44_underPF, _
'	LM_L_l45_LPost, LM_L_l45_Layer1, LM_L_l45_Parts, LM_L_l45_Playfield, LM_L_l45_underPF, LM_L_l46_Hammer, LM_L_l46_HammerUp, LM_L_l46_Parts, LM_L_l46_Playfield, LM_L_l46_underPF, LM_L_l47_Hammer, LM_L_l47_HammerUp, LM_L_l47_Parts, LM_L_l47_Playfield, LM_L_l47_sw0709, LM_L_l47_underPF, LM_L_l48_Hammer, LM_L_l48_HammerUp, LM_L_l48_Layer1, LM_L_l48_Parts, LM_L_l48_Playfield, LM_L_l48_snakeM2, LM_L_l48_sw0709, LM_L_l48_sw36, LM_L_l48_sw37, LM_L_l48_sw60, LM_L_l48_sw61, LM_L_l48_sw62, LM_L_l48_underPF, LM_L_l49_Hammer, LM_L_l49_HammerUp, LM_L_l49_Layer1, LM_L_l49_Layer3, LM_L_l49_Lspinner, LM_L_l49_Parts, LM_L_l49_Playfield, LM_L_l49_cemeteryMOD, LM_L_l49_sw36, LM_L_l49_sw60, LM_L_l49_sw61, LM_L_l49_sw62, LM_L_l49_underPF, LM_L_l50_Hammer, LM_L_l50_HammerUp, LM_L_l50_Layer1, LM_L_l50_Lspinner, LM_L_l50_Parts, LM_L_l50_Playfield, LM_L_l50_sw36, LM_L_l50_sw60, LM_L_l50_sw61, LM_L_l50_underPF, LM_L_l51_Layer1, LM_L_l51_Lspinner, LM_L_l51_Parts, LM_L_l51_Playfield, LM_L_l51_sw0709, LM_L_l51_sw36, LM_L_l51_sw37, _
'	LM_L_l51_sw60, LM_L_l51_sw61, LM_L_l51_sw62, LM_L_l51_underPF, LM_L_l53_Lflip, LM_L_l53_LflipU, LM_L_l53_Parts, LM_L_l53_Playfield, LM_L_l53_Rflip, LM_L_l53_RflipU, LM_L_l53_underPF, LM_L_l55_Lflip, LM_L_l55_LflipU, LM_L_l55_Parts, LM_L_l55_Playfield, LM_L_l55_Rflip, LM_L_l55_RflipU, LM_L_l55_underPF, LM_L_l56_Lflip, LM_L_l56_LflipU, LM_L_l56_Parts, LM_L_l56_Playfield, LM_L_l56_Rflip, LM_L_l56_RflipU, LM_L_l56_underPF, LM_L_l57_Lflip, LM_L_l57_LflipU, LM_L_l57_Parts, LM_L_l57_Playfield, LM_L_l57_Rflip, LM_L_l57_RflipU, LM_L_l57_underPF, LM_L_l58_Lflip, LM_L_l58_LflipU, LM_L_l58_Parts, LM_L_l58_Playfield, LM_L_l58_Rflip, LM_L_l58_RflipU, LM_L_l58_underPF, LM_L_l59_LEMK, LM_L_l59_LSling1, LM_L_l59_LSling2, LM_L_l59_Lflip, LM_L_l59_LflipU, LM_L_l59_Parts, LM_L_l59_Playfield, LM_L_l59_underPF, LM_L_l60_Lflip, LM_L_l60_LflipU, LM_L_l60_Parts, LM_L_l60_Playfield, LM_L_l60_Rflip, LM_L_l60_RflipU, LM_L_l60_underPF, LM_L_l61_Lflip, LM_L_l61_LflipU, LM_L_l61_Parts, LM_L_l61_Playfield, LM_L_l61_Rflip, LM_L_l61_RflipU, _
'	LM_L_l61_underPF, LM_L_l62_Parts, LM_L_l62_Playfield, LM_L_l62_REMK, LM_L_l62_RSling1, LM_L_l62_RSling2, LM_L_l62_Rflip, LM_L_l62_RflipU, LM_L_l62_underPF, LM_L_l63_LPost, LM_L_l63_Parts, LM_L_l63_Playfield, LM_L_l63_sw1, LM_L_l63_sw24, LM_L_l63_sw25, LM_L_l63_underPF, LM_L_l64_LPost, LM_L_l64_Layer1, LM_L_l64_Parts, LM_L_l64_Playfield, LM_L_l64_sw1, LM_L_l64_sw25, LM_L_l64_sw28, LM_L_l64_underPF, LM_L_l65_Hammer, LM_L_l65_HammerUp, LM_L_l65_Layer1, LM_L_l65_Layer3, LM_L_l65_Parts, LM_L_l65_Playfield, LM_L_l65_cemeteryMOD, LM_L_l65_cross, LM_L_l65_sw36, LM_L_l65_sw37, LM_L_l65_sw60, LM_L_l65_sw61, LM_L_l65_sw62, LM_L_l65_underPF, LM_L_l66_BR1, LM_L_l66_BR2, LM_L_l66_BR3, LM_L_l66_BS1, LM_L_l66_BS2, LM_L_l66_BS3, LM_L_l66_Layer1, LM_L_l66_Layer2b, LM_L_l66_Layer3, LM_L_l66_Parts, LM_L_l66_Playfield, LM_L_l66_underPF, LM_L_l67_BR1, LM_L_l67_BR2, LM_L_l67_BS1, LM_L_l67_BS2, LM_L_l67_BS3, LM_L_l67_Layer2b, LM_L_l67_Parts, LM_L_l67_Playfield, LM_L_l67_sparkyhead, LM_L_l67_underPF, LM_L_l68_BR3, LM_L_l68_BS3, _
'	LM_L_l68_Hammer, LM_L_l68_HammerUp, LM_L_l68_Layer1, LM_L_l68_Parts, LM_L_l68_Playfield, LM_L_l68_Rod, LM_L_l68_sparkyhead, LM_L_l68_sw35, LM_L_l68_underPF, LM_L_l69_Hammer, LM_L_l69_HammerUp, LM_L_l69_Layer1, LM_L_l69_Parts, LM_L_l69_Playfield, LM_L_l69_sparkyhead, LM_L_l69_sw35, LM_L_l69_underPF, LM_L_l70_Hammer, LM_L_l70_HammerUp, LM_L_l70_Layer1, LM_L_l70_Parts, LM_L_l70_Playfield, LM_L_l70_sparkyhead, LM_L_l70_sw35, LM_L_l70_sw61, LM_L_l70_underPF, LM_L_l71_Hammer, LM_L_l71_HammerUp, LM_L_l71_Layer1, LM_L_l71_Layer3, LM_L_l71_Lspinner, LM_L_l71_Parts, LM_L_l71_Playfield, LM_L_l71_cemeteryMOD, LM_L_l71_cross, LM_L_l71_sw36, LM_L_l71_sw37, LM_L_l71_sw60, LM_L_l71_sw61, LM_L_l71_sw62, LM_L_l71_underPF, LM_L_l77_Layer1, LM_L_l77_Parts, LM_L_l77_Playfield, LM_L_l77_sw0709, LM_L_l77_sw36, LM_L_l77_sw60, LM_L_l77_underPF, LM_L_l78_Hammer, LM_L_l78_Layer1, LM_L_l78_Parts, LM_L_l78_Playfield, LM_L_l78_sw0709, LM_L_l78_sw3, LM_L_l78_underPF, LM_L_l79_Layer1, LM_L_l79_Parts, LM_L_l79_Playfield, LM_L_l79_sw0709, _
'	LM_L_l79_underPF, LM_L_l80_Layer1, LM_L_l80_Parts, LM_L_l80_Playfield, LM_L_l80_sw0709, LM_L_l80_underPF, LM_L_optos_Layer1, LM_L_optos_Parts, LM_L_optos_Playfield, LM_L_optos_lockPost, LM_L_optos_underPF)
'Dim BG_All: BG_All=Array(BM_BR1, BM_BR2, BM_BR3, BM_BS1, BM_BS2, BM_BS3, BM_Cube, BM_Hammer, BM_HammerUp, BM_LEMK, BM_LPost, BM_LSling1, BM_LSling2, BM_Layer1, BM_Layer2a, BM_Layer2b, BM_Layer3, BM_Lflip, BM_LflipU, BM_Lspinner, BM_Parts, BM_Playfield, BM_REMK, BM_RPost, BM_RSling1, BM_RSling2, BM_Rflip, BM_RflipU, BM_Rod, BM_Rod_001, BM_Rspinner, BM_SnakeMOD, BM_UpPost, BM_cemeteryMOD, BM_cross, BM_crossHolder, BM_lockPost, BM_snakeM1, BM_snakeM2, BM_sparkyhead, BM_sw0709, BM_sw1, BM_sw24, BM_sw25, BM_sw28, BM_sw29, BM_sw3, BM_sw35, BM_sw36, BM_sw37, BM_sw4, BM_sw40, BM_sw41, BM_sw42, BM_sw43, BM_sw44, BM_sw45, BM_sw46, BM_sw47p, BM_sw50p, BM_sw60, BM_sw61, BM_sw62, BM_swplunger, BM_underPF, LM_B_BR1, LM_B_BR2, LM_B_BR3, LM_B_BS1, LM_B_BS2, LM_B_BS3, LM_B_Hammer, LM_B_HammerUp, LM_B_Layer1, LM_B_Layer2a, LM_B_Layer2b, LM_B_Layer3, LM_B_Lspinner, LM_B_Parts, LM_B_Playfield, LM_B_Rod, LM_B_Rod_001, LM_B_SnakeMOD, LM_B_UpPost, LM_B_cemeteryMOD, LM_B_cross, LM_B_crossHolder, LM_B_sparkyhead, LM_B_sw47p, _
'	LM_B_sw61, LM_B_sw62, LM_B_underPF, LM_FL_F19_HammerUp, LM_FL_F19_Layer1, LM_FL_F19_Layer3, LM_FL_F19_Lspinner, LM_FL_F19_Parts, LM_FL_F19_Playfield, LM_FL_F19_cemeteryMOD, LM_FL_F19_cross, LM_FL_F19_crossHolder, LM_FL_F19_sw62, LM_FL_F21_BS1, LM_FL_F21_Layer1, LM_FL_F21_Layer3, LM_FL_F21_Parts, LM_FL_F21_Playfield, LM_FL_F21_Rod, LM_FL_F21_UpPost, LM_FL_F21_cemeteryMOD, LM_FL_F21_sparkyhead, LM_FL_F21_sw46, LM_FL_F21_sw50p, LM_FL_F21_underPF, LM_FL_F22_BR2, LM_FL_F22_BS1, LM_FL_F22_BS2, LM_FL_F22_BS3, LM_FL_F22_Layer1, LM_FL_F22_Layer2a, LM_FL_F22_Layer2b, LM_FL_F22_Layer3, LM_FL_F22_Parts, LM_FL_F22_Playfield, LM_FL_F22_Rod_001, LM_FL_F22_SnakeMOD, LM_FL_F22_UpPost, LM_FL_F22_sparkyhead, LM_FL_F22_sw43, LM_FL_F22_sw44, LM_FL_F22_sw45, LM_FL_F22_sw46, LM_FL_F22_sw47p, LM_FL_F23_Layer1, LM_FL_F23_Layer3, LM_FL_F23_Parts, LM_FL_F23_Playfield, LM_FL_F23_cemeteryMOD, LM_FL_F23_sparkyhead, LM_FL_F23_sw43, LM_FL_F23_underPF, LM_FL_F25_BR1, LM_FL_F25_BR2, LM_FL_F25_BR3, LM_FL_F25_BS1, LM_FL_F25_BS2, LM_FL_F25_BS3, _
'	LM_FL_F25_Layer1, LM_FL_F25_Layer2a, LM_FL_F25_Layer2b, LM_FL_F25_Parts, LM_FL_F25_Playfield, LM_FL_F25_SnakeMOD, LM_FL_F25_sparkyhead, LM_FL_F25_underPF, LM_FL_F26_Hammer, LM_FL_F26_HammerUp, LM_FL_F26_Layer1, LM_FL_F26_Layer3, LM_FL_F26_Lspinner, LM_FL_F26_Parts, LM_FL_F26_Playfield, LM_FL_F26_UpPost, LM_FL_F26_cemeteryMOD, LM_FL_F26_cross, LM_FL_F26_crossHolder, LM_FL_F26_sparkyhead, LM_FL_F26_sw35, LM_FL_F26_sw43, LM_FL_F26_sw50p, LM_FL_F26_sw60, LM_FL_F26_sw61, LM_FL_F26_sw62, LM_FL_F26_underPF, LM_FL_F27_BR1, LM_FL_F27_BR2, LM_FL_F27_BR3, LM_FL_F27_BS1, LM_FL_F27_BS2, LM_FL_F27_BS3, LM_FL_F27_Hammer, LM_FL_F27_HammerUp, LM_FL_F27_Layer1, LM_FL_F27_Layer3, LM_FL_F27_Parts, LM_FL_F27_Playfield, LM_FL_F27_Rod, LM_FL_F27_SnakeMOD, LM_FL_F27_UpPost, LM_FL_F27_cross, LM_FL_F27_crossHolder, LM_FL_F27_sparkyhead, LM_FL_F27_sw35, LM_FL_F27_sw43, LM_FL_F27_sw44, LM_FL_F27_sw45, LM_FL_F27_sw47p, LM_FL_F27_sw50p, LM_FL_F27_underPF, LM_FL_F28_BR1, LM_FL_F28_BR2, LM_FL_F28_BR3, LM_FL_F28_BS1, LM_FL_F28_BS2, _
'	LM_FL_F28_BS3, LM_FL_F28_HammerUp, LM_FL_F28_Layer1, LM_FL_F28_Layer2b, LM_FL_F28_Layer3, LM_FL_F28_Parts, LM_FL_F28_Playfield, LM_FL_F28_Rod, LM_FL_F28_SnakeMOD, LM_FL_F28_UpPost, LM_FL_F28_cemeteryMOD, LM_FL_F28_cross, LM_FL_F28_sparkyhead, LM_FL_F28_sw35, LM_FL_F28_sw46, LM_FL_F28_sw47p, LM_FL_F28_underPF, LM_FL_F29_BR1, LM_FL_F29_BR3, LM_FL_F29_BS3, LM_FL_F29_Parts, LM_FL_F29_Playfield, LM_FL_F29_snakeM1, LM_FL_F29_snakeM2, LM_FL_F29_underPF, LM_FL_F30_BR1, LM_FL_F30_BR2, LM_FL_F30_BR3, LM_FL_F30_BS1, LM_FL_F30_BS2, LM_FL_F30_BS3, LM_FL_F30_Hammer, LM_FL_F30_HammerUp, LM_FL_F30_Layer1, LM_FL_F30_Layer2a, LM_FL_F30_Layer2b, LM_FL_F30_Layer3, LM_FL_F30_Parts, LM_FL_F30_Playfield, LM_FL_F30_Rod, LM_FL_F30_Rod_001, LM_FL_F30_Rspinner, LM_FL_F30_SnakeMOD, LM_FL_F30_cemeteryMOD, LM_FL_F30_snakeM1, LM_FL_F30_snakeM2, LM_FL_F30_sparkyhead, LM_FL_F30_sw40, LM_FL_F30_sw47p, LM_FL_F30_underPF, LM_FL_F31_Hammer, LM_FL_F31_HammerUp, LM_FL_F31_Layer1, LM_FL_F31_Parts, LM_FL_F31_Playfield, LM_FL_F31_lockPost, _
'	LM_FL_F31_snakeM1, LM_FL_F31_sw60, LM_FL_F31_sw61, LM_FL_F31_underPF, LM_FL_F32_BR3, LM_FL_F32_BS2, LM_FL_F32_BS3, LM_FL_F32_Cube, LM_FL_F32_Hammer, LM_FL_F32_HammerUp, LM_FL_F32_Layer1, LM_FL_F32_Layer2a, LM_FL_F32_Layer2b, LM_FL_F32_Layer3, LM_FL_F32_Parts, LM_FL_F32_Playfield, LM_FL_F32_Rod, LM_FL_F32_Rod_001, LM_FL_F32_cemeteryMOD, LM_FL_F32_cross, LM_FL_F32_sparkyhead, LM_FL_F32_sw35, LM_FL_F32_sw42, LM_FL_F32_sw47p, LM_FL_F32_sw61, LM_FL_F32_sw62, LM_FL_F32_underPF, LM_GIB_GIB1_Parts, LM_GIB_GIB1_Playfield, LM_GIB_GIB2_LEMK, LM_GIB_GIB2_LSling1, LM_GIB_GIB2_Parts, LM_GIB_GIB2_Playfield, LM_GIB_GIB2_sw1, LM_GIB_GIB2_sw25, LM_GIB_GIB3_Parts, LM_GIB_GIB3_Playfield, LM_GIB_GIB3_REMK, LM_GIB_GIB4_Parts, LM_GIB_GIB4_Playfield, LM_GIB_GIB4_REMK, LM_GIB_GIB4_RSling1, LM_GIB_GIB4_RSling2, LM_GIB_GIB4_sw28, LM_GIB_GIB4_sw29, LM_GIB_GIB5_LPost, LM_GIB_GIB5_Layer1, LM_GIB_GIB5_Parts, LM_GIB_GIB5_Playfield, LM_GIB_GIB5_sw25, LM_GIB_GIB6_Parts, LM_GIB_GIB6_Playfield, LM_GIB_GIB6_RPost, LM_GIB_GIB7_BS2, _
'	LM_GIB_GIB7_Layer1, LM_GIB_GIB7_Layer3, LM_GIB_GIB7_Parts, LM_GIB_GIB7_Playfield, LM_GIB_GIB7_UpPost, LM_GIB_GIB7_cross, LM_GIB_GIB7_crossHolder, LM_GIB_GIB7_sparkyhead, LM_GIB_GIB7_sw43, LM_GIB_GIB7_sw44, LM_GIB_GIB7_underPF, LM_GIR_GIR1_LEMK, LM_GIR_GIR1_LflipU, LM_GIR_GIR1_Parts, LM_GIR_GIR1_Playfield, LM_GIR_GIR1_sw1, LM_GIR_GIR1_sw24, LM_GIR_GIR1_sw25, LM_GIR_GIR2_LEMK, LM_GIR_GIR2_LPost, LM_GIR_GIR2_LSling1, LM_GIR_GIR2_LSling2, LM_GIR_GIR2_Lflip, LM_GIR_GIR2_LflipU, LM_GIR_GIR2_Parts, LM_GIR_GIR2_Playfield, LM_GIR_GIR2_sw1, LM_GIR_GIR2_sw24, LM_GIR_GIR2_sw25, LM_GIR_GIR3_Parts, LM_GIR_GIR3_Playfield, LM_GIR_GIR3_REMK, LM_GIR_GIR3_Rflip, LM_GIR_GIR3_RflipU, LM_GIR_GIR3_sw28, LM_GIR_GIR3_sw29, LM_GIR_GIR4_Parts, LM_GIR_GIR4_Playfield, LM_GIR_GIR4_REMK, LM_GIR_GIR4_RPost, LM_GIR_GIR4_RSling1, LM_GIR_GIR4_RSling2, LM_GIR_GIR4_Rflip, LM_GIR_GIR4_RflipU, LM_GIR_GIR4_sw28, LM_GIR_GIR4_sw29, LM_GIR_GIR5_LPost, LM_GIR_GIR5_LSling1, LM_GIR_GIR5_Layer1, LM_GIR_GIR5_Parts, LM_GIR_GIR5_Playfield, LM_GIR_GIR5_sw1, _
'	LM_GIR_GIR5_underPF, LM_GIR_GIR6_Layer1, LM_GIR_GIR6_Parts, LM_GIR_GIR6_Playfield, LM_GIR_GIR6_sw28, LM_GIR_GIR6_underPF, LM_GIR_GIR7_BR1, LM_GIR_GIR7_BR2, LM_GIR_GIR7_BS1, LM_GIR_GIR7_BS2, LM_GIR_GIR7_BS3, LM_GIR_GIR7_Layer1, LM_GIR_GIR7_Layer3, LM_GIR_GIR7_Parts, LM_GIR_GIR7_Playfield, LM_GIR_GIR7_UpPost, LM_GIR_GIR7_sparkyhead, LM_GIR_GIR7_sw44, LM_GIR_GIR7_sw45, LM_GIR_GIR7_underPF, LM_GIW_GIW1_LEMK, LM_GIW_GIW1_LSling1, LM_GIW_GIW1_Lflip, LM_GIW_GIW1_LflipU, LM_GIW_GIW1_Parts, LM_GIW_GIW1_Playfield, LM_GIW_GIW1_Rflip, LM_GIW_GIW1_RflipU, LM_GIW_GIW1_sw24, LM_GIW_GIW1_sw29, LM_GIW_GIW11_BR1, LM_GIW_GIW11_BR3, LM_GIW_GIW11_BS1, LM_GIW_GIW11_BS3, LM_GIW_GIW11_Hammer, LM_GIW_GIW11_HammerUp, LM_GIW_GIW11_Layer1, LM_GIW_GIW11_Layer2a, LM_GIW_GIW11_Lspinner, LM_GIW_GIW11_Parts, LM_GIW_GIW11_Playfield, LM_GIW_GIW11_Rod, LM_GIW_GIW11_cemeteryMOD, LM_GIW_GIW11_cross, LM_GIW_GIW11_sparkyhead, LM_GIW_GIW11_sw36, LM_GIW_GIW11_sw37, LM_GIW_GIW11_sw60, LM_GIW_GIW11_sw61, LM_GIW_GIW11_sw62, LM_GIW_GIW11_underPF, _
'	LM_GIW_GIW2_LEMK, LM_GIW_GIW2_LSling1, LM_GIW_GIW2_LSling2, LM_GIW_GIW2_Lflip, LM_GIW_GIW2_LflipU, LM_GIW_GIW2_Parts, LM_GIW_GIW2_Playfield, LM_GIW_GIW2_RflipU, LM_GIW_GIW2_sw1, LM_GIW_GIW2_sw24, LM_GIW_GIW2_sw25, LM_GIW_GIW3_Lflip, LM_GIW_GIW3_Parts, LM_GIW_GIW3_Playfield, LM_GIW_GIW3_REMK, LM_GIW_GIW3_RSling2, LM_GIW_GIW3_Rflip, LM_GIW_GIW3_RflipU, LM_GIW_GIW4_LflipU, LM_GIW_GIW4_Parts, LM_GIW_GIW4_Playfield, LM_GIW_GIW4_REMK, LM_GIW_GIW4_RPost, LM_GIW_GIW4_RSling1, LM_GIW_GIW4_RSling2, LM_GIW_GIW4_Rflip, LM_GIW_GIW4_RflipU, LM_GIW_GIW4_sw28, LM_GIW_GIW4_sw29, LM_GIW_GIW5_LPost, LM_GIW_GIW5_Layer1, LM_GIW_GIW5_Parts, LM_GIW_GIW5_Playfield, LM_GIW_GIW5_RSling1, LM_GIW_GIW5_sw1, LM_GIW_GIW5_sw24, LM_GIW_GIW5_sw25, LM_GIW_GIW5_underPF, LM_GIW_GIW6_Layer1, LM_GIW_GIW6_Parts, LM_GIW_GIW6_Playfield, LM_GIW_GIW6_RPost, LM_GIW_GIW6_snakeM2, LM_GIW_GIW6_sw28, LM_GIW_GIW6_sw29, LM_GIW_GIW6_sw40, LM_GIW_GIW6_underPF, LM_GIW_GIW7_BR1, LM_GIW_GIW7_BR2, LM_GIW_GIW7_BR3, LM_GIW_GIW7_BS1, LM_GIW_GIW7_BS2, LM_GIW_GIW7_BS3, _
'	LM_GIW_GIW7_Cube, LM_GIW_GIW7_Hammer, LM_GIW_GIW7_HammerUp, LM_GIW_GIW7_Layer1, LM_GIW_GIW7_Layer2a, LM_GIW_GIW7_Layer2b, LM_GIW_GIW7_Layer3, LM_GIW_GIW7_Lspinner, LM_GIW_GIW7_Parts, LM_GIW_GIW7_Playfield, LM_GIW_GIW7_Rod, LM_GIW_GIW7_Rod_001, LM_GIW_GIW7_Rspinner, LM_GIW_GIW7_SnakeMOD, LM_GIW_GIW7_UpPost, LM_GIW_GIW7_cemeteryMOD, LM_GIW_GIW7_cross, LM_GIW_GIW7_crossHolder, LM_GIW_GIW7_snakeM1, LM_GIW_GIW7_snakeM2, LM_GIW_GIW7_sparkyhead, LM_GIW_GIW7_sw0709, LM_GIW_GIW7_sw28, LM_GIW_GIW7_sw3, LM_GIW_GIW7_sw35, LM_GIW_GIW7_sw36, LM_GIW_GIW7_sw37, LM_GIW_GIW7_sw4, LM_GIW_GIW7_sw41, LM_GIW_GIW7_sw42, LM_GIW_GIW7_sw43, LM_GIW_GIW7_sw44, LM_GIW_GIW7_sw45, LM_GIW_GIW7_sw46, LM_GIW_GIW7_sw47p, LM_GIW_GIW7_sw50p, LM_GIW_GIW7_sw60, LM_GIW_GIW7_sw61, LM_GIW_GIW7_sw62, LM_GIW_GIW7_swplunger, LM_GIW_GIW7_underPF, LM_GIWU_GISpotL1_LEMK, LM_GIWU_GISpotL1_Lflip, LM_GIWU_GISpotL1_LflipU, LM_GIWU_GISpotL1_Parts, LM_GIWU_GISpotL1_Playfield, LM_GIWU_GISpotL1_RflipU, LM_GIWU_GISpotL2_LSling1, LM_GIWU_GISpotL2_Parts, _
'	LM_GIWU_GISpotL2_Playfield, LM_GIWU_GISpotL2_RPost, LM_GIWU_GISpotL2_Rod_001, LM_GIWU_GISpotL2_snakeM1, LM_GIWU_GISpotL2_underPF, LM_GIWU_GISpotR1_LflipU, LM_GIWU_GISpotR1_Parts, LM_GIWU_GISpotR1_Playfield, LM_GIWU_GISpotR1_REMK, LM_GIWU_GISpotR1_RSling1, LM_GIWU_GISpotR1_Rflip, LM_GIWU_GISpotR1_RflipU, LM_GIWU_GISpotR2_LPost, LM_GIWU_GISpotR2_Parts, LM_GIWU_GISpotR2_Playfield, LM_GIWU_GISpotR2_RSling2, LM_GIWU_GISpotR2_underPF, LM_GIWU_GISpotR3_BR3, LM_GIWU_GISpotR3_BS3, LM_GIWU_GISpotR3_Hammer, LM_GIWU_GISpotR3_HammerUp, LM_GIWU_GISpotR3_Layer1, LM_GIWU_GISpotR3_Layer2a, LM_GIWU_GISpotR3_Parts, LM_GIWU_GISpotR3_Playfield, LM_GIWU_GISpotR3_snakeM1, LM_GIWU_GISpotR3_snakeM2, LM_GIWU_GISpotR3_sw40, LM_GIWU_GISpotR3_underPF, LM_L_CN11_Layer1, LM_L_CN11_Layer3, LM_L_CN11_Lspinner, LM_L_CN11_Parts, LM_L_CN11_Playfield, LM_L_CN11_cemeteryMOD, LM_L_CN11_underPF, LM_L_CN13_Parts, LM_L_CN13_Playfield, LM_L_CN13_sw36, LM_L_CN13_sw37, LM_L_CN13_sw60, LM_L_CN13_sw61, LM_L_CN13_sw62, LM_L_CN13_underPF, LM_L_CN19_Layer1, _
'	LM_L_CN19_Parts, LM_L_CN19_Playfield, LM_L_CN19_cemeteryMOD, LM_L_CN19_underPF, LM_L_CN4_Layer1, LM_L_CN4_Parts, LM_L_CN4_Playfield, LM_L_CN4_underPF, LM_L_CN5_Parts, LM_L_CN5_Playfield, LM_L_CN5_snakeM2, LM_L_CN5_sw40, LM_L_CN5_sw41, LM_L_CN5_underPF, LM_L_CN9_Parts, LM_L_CN9_Playfield, LM_L_CN9_Rspinner, LM_L_CN9_snakeM2, LM_L_CN9_sw41, LM_L_CN9_underPF, LM_L_L73_BR3, LM_L_L73_BS3, LM_L_L73_Layer1, LM_L_L73_Layer2a, LM_L_L73_Layer2b, LM_L_L73_Layer3, LM_L_L73_Parts, LM_L_L74_BS1, LM_L_L74_Layer2b, LM_L_L74_Parts, LM_L_L75_BS2, LM_L_L75_Parts, LM_L_L75_sparkyhead, LM_L_l17_Parts, LM_L_l17_Playfield, LM_L_l17_Rspinner, LM_L_l17_snakeM2, LM_L_l17_sw41, LM_L_l17_underPF, LM_L_l18_Layer1, LM_L_l18_Parts, LM_L_l18_Playfield, LM_L_l18_Rspinner, LM_L_l18_sw41, LM_L_l18_underPF, LM_L_l19_Parts, LM_L_l19_Playfield, LM_L_l19_Rspinner, LM_L_l19_sw41, LM_L_l19_underPF, LM_L_l21_Hammer, LM_L_l21_HammerUp, LM_L_l21_Layer1, LM_L_l21_Layer3, LM_L_l21_Parts, LM_L_l21_Playfield, LM_L_l21_cemeteryMOD, LM_L_l21_sparkyhead, _
'	LM_L_l21_underPF, LM_L_l22_BR2, LM_L_l22_BR3, LM_L_l22_BS1, LM_L_l22_BS3, LM_L_l22_Hammer, LM_L_l22_HammerUp, LM_L_l22_Layer1, LM_L_l22_Layer2a, LM_L_l22_Layer2b, LM_L_l22_Parts, LM_L_l22_Playfield, LM_L_l22_Rod, LM_L_l22_sparkyhead, LM_L_l23_Parts, LM_L_l23_Playfield, LM_L_l23_underPF, LM_L_l24_Parts, LM_L_l24_Playfield, LM_L_l24_underPF, LM_L_l25_Parts, LM_L_l25_Playfield, LM_L_l25_Rspinner, LM_L_l25_underPF, LM_L_l26_Parts, LM_L_l26_Playfield, LM_L_l26_RPost, LM_L_l26_sw28, LM_L_l26_underPF, LM_L_l27_Parts, LM_L_l27_Playfield, LM_L_l27_RPost, LM_L_l27_sw28, LM_L_l27_sw29, LM_L_l27_underPF, LM_L_l28_Hammer, LM_L_l28_HammerUp, LM_L_l28_Layer1, LM_L_l28_Parts, LM_L_l28_Playfield, LM_L_l28_Rspinner, LM_L_l28_snakeM2, LM_L_l28_sw40, LM_L_l28_sw41, LM_L_l28_underPF, LM_L_l29_Hammer, LM_L_l29_HammerUp, LM_L_l29_Layer1, LM_L_l29_Parts, LM_L_l29_Playfield, LM_L_l29_Rspinner, LM_L_l29_snakeM2, LM_L_l29_sw40, LM_L_l29_sw41, LM_L_l29_underPF, LM_L_l30_Hammer, LM_L_l30_Parts, LM_L_l30_Playfield, LM_L_l30_underPF, _
'	LM_L_l31_Hammer, LM_L_l31_HammerUp, LM_L_l31_Layer1, LM_L_l31_Parts, LM_L_l31_Playfield, LM_L_l31_Rspinner, LM_L_l31_snakeM2, LM_L_l31_sw40, LM_L_l31_sw41, LM_L_l31_underPF, LM_L_l32_Hammer, LM_L_l32_HammerUp, LM_L_l32_Layer1, LM_L_l32_Parts, LM_L_l32_Playfield, LM_L_l32_snakeM2, LM_L_l32_sw40, LM_L_l32_sw41, LM_L_l32_underPF, LM_L_l33_Hammer, LM_L_l33_HammerUp, LM_L_l33_Layer1, LM_L_l33_Parts, LM_L_l33_Playfield, LM_L_l33_cemeteryMOD, LM_L_l33_sw37, LM_L_l33_sw61, LM_L_l33_sw62, LM_L_l33_underPF, LM_L_l34_Hammer, LM_L_l34_HammerUp, LM_L_l34_Layer1, LM_L_l34_Parts, LM_L_l34_Playfield, LM_L_l34_cemeteryMOD, LM_L_l34_snakeM2, LM_L_l34_sw37, LM_L_l34_sw60, LM_L_l34_sw61, LM_L_l34_underPF, LM_L_l35_Hammer, LM_L_l35_HammerUp, LM_L_l35_Parts, LM_L_l35_Playfield, LM_L_l35_snakeM2, LM_L_l35_sw37, LM_L_l35_sw60, LM_L_l35_sw61, LM_L_l35_sw62, LM_L_l35_underPF, LM_L_l37_Parts, LM_L_l37_Playfield, LM_L_l37_REMK, LM_L_l37_RSling1, LM_L_l37_RSling2, LM_L_l37_underPF, LM_L_l38_Parts, LM_L_l38_Playfield, LM_L_l38_RSling1, _
'	LM_L_l38_RSling2, LM_L_l38_underPF, LM_L_l39_Parts, LM_L_l39_Playfield, LM_L_l39_RSling1, LM_L_l39_RSling2, LM_L_l39_underPF, LM_L_l40_LSling2, LM_L_l40_Parts, LM_L_l40_Playfield, LM_L_l40_RSling1, LM_L_l40_RSling2, LM_L_l40_underPF, LM_L_l41_LSling1, LM_L_l41_LSling2, LM_L_l41_Parts, LM_L_l41_Playfield, LM_L_l41_underPF, LM_L_l42_LEMK, LM_L_l42_LPost, LM_L_l42_Parts, LM_L_l42_Playfield, LM_L_l42_sw1, LM_L_l42_sw25, LM_L_l42_underPF, LM_L_l43_LPost, LM_L_l43_Layer1, LM_L_l43_Parts, LM_L_l43_Playfield, LM_L_l43_underPF, LM_L_l44_LPost, LM_L_l44_Layer1, LM_L_l44_Parts, LM_L_l44_Playfield, LM_L_l44_underPF, LM_L_l45_LPost, LM_L_l45_Layer1, LM_L_l45_Parts, LM_L_l45_Playfield, LM_L_l45_underPF, LM_L_l46_Hammer, LM_L_l46_HammerUp, LM_L_l46_Parts, LM_L_l46_Playfield, LM_L_l46_underPF, LM_L_l47_Hammer, LM_L_l47_HammerUp, LM_L_l47_Parts, LM_L_l47_Playfield, LM_L_l47_sw0709, LM_L_l47_underPF, LM_L_l48_Hammer, LM_L_l48_HammerUp, LM_L_l48_Layer1, LM_L_l48_Parts, LM_L_l48_Playfield, LM_L_l48_snakeM2, LM_L_l48_sw0709, _
'	LM_L_l48_sw36, LM_L_l48_sw37, LM_L_l48_sw60, LM_L_l48_sw61, LM_L_l48_sw62, LM_L_l48_underPF, LM_L_l49_Hammer, LM_L_l49_HammerUp, LM_L_l49_Layer1, LM_L_l49_Layer3, LM_L_l49_Lspinner, LM_L_l49_Parts, LM_L_l49_Playfield, LM_L_l49_cemeteryMOD, LM_L_l49_sw36, LM_L_l49_sw60, LM_L_l49_sw61, LM_L_l49_sw62, LM_L_l49_underPF, LM_L_l50_Hammer, LM_L_l50_HammerUp, LM_L_l50_Layer1, LM_L_l50_Lspinner, LM_L_l50_Parts, LM_L_l50_Playfield, LM_L_l50_sw36, LM_L_l50_sw60, LM_L_l50_sw61, LM_L_l50_underPF, LM_L_l51_Layer1, LM_L_l51_Lspinner, LM_L_l51_Parts, LM_L_l51_Playfield, LM_L_l51_sw0709, LM_L_l51_sw36, LM_L_l51_sw37, LM_L_l51_sw60, LM_L_l51_sw61, LM_L_l51_sw62, LM_L_l51_underPF, LM_L_l53_Lflip, LM_L_l53_LflipU, LM_L_l53_Parts, LM_L_l53_Playfield, LM_L_l53_Rflip, LM_L_l53_RflipU, LM_L_l53_underPF, LM_L_l55_Lflip, LM_L_l55_LflipU, LM_L_l55_Parts, LM_L_l55_Playfield, LM_L_l55_Rflip, LM_L_l55_RflipU, LM_L_l55_underPF, LM_L_l56_Lflip, LM_L_l56_LflipU, LM_L_l56_Parts, LM_L_l56_Playfield, LM_L_l56_Rflip, LM_L_l56_RflipU, _
'	LM_L_l56_underPF, LM_L_l57_Lflip, LM_L_l57_LflipU, LM_L_l57_Parts, LM_L_l57_Playfield, LM_L_l57_Rflip, LM_L_l57_RflipU, LM_L_l57_underPF, LM_L_l58_Lflip, LM_L_l58_LflipU, LM_L_l58_Parts, LM_L_l58_Playfield, LM_L_l58_Rflip, LM_L_l58_RflipU, LM_L_l58_underPF, LM_L_l59_LEMK, LM_L_l59_LSling1, LM_L_l59_LSling2, LM_L_l59_Lflip, LM_L_l59_LflipU, LM_L_l59_Parts, LM_L_l59_Playfield, LM_L_l59_underPF, LM_L_l60_Lflip, LM_L_l60_LflipU, LM_L_l60_Parts, LM_L_l60_Playfield, LM_L_l60_Rflip, LM_L_l60_RflipU, LM_L_l60_underPF, LM_L_l61_Lflip, LM_L_l61_LflipU, LM_L_l61_Parts, LM_L_l61_Playfield, LM_L_l61_Rflip, LM_L_l61_RflipU, LM_L_l61_underPF, LM_L_l62_Parts, LM_L_l62_Playfield, LM_L_l62_REMK, LM_L_l62_RSling1, LM_L_l62_RSling2, LM_L_l62_Rflip, LM_L_l62_RflipU, LM_L_l62_underPF, LM_L_l63_LPost, LM_L_l63_Parts, LM_L_l63_Playfield, LM_L_l63_sw1, LM_L_l63_sw24, LM_L_l63_sw25, LM_L_l63_underPF, LM_L_l64_LPost, LM_L_l64_Layer1, LM_L_l64_Parts, LM_L_l64_Playfield, LM_L_l64_sw1, LM_L_l64_sw25, LM_L_l64_sw28, LM_L_l64_underPF, _
'	LM_L_l65_Hammer, LM_L_l65_HammerUp, LM_L_l65_Layer1, LM_L_l65_Layer3, LM_L_l65_Parts, LM_L_l65_Playfield, LM_L_l65_cemeteryMOD, LM_L_l65_cross, LM_L_l65_sw36, LM_L_l65_sw37, LM_L_l65_sw60, LM_L_l65_sw61, LM_L_l65_sw62, LM_L_l65_underPF, LM_L_l66_BR1, LM_L_l66_BR2, LM_L_l66_BR3, LM_L_l66_BS1, LM_L_l66_BS2, LM_L_l66_BS3, LM_L_l66_Layer1, LM_L_l66_Layer2b, LM_L_l66_Layer3, LM_L_l66_Parts, LM_L_l66_Playfield, LM_L_l66_underPF, LM_L_l67_BR1, LM_L_l67_BR2, LM_L_l67_BS1, LM_L_l67_BS2, LM_L_l67_BS3, LM_L_l67_Layer2b, LM_L_l67_Parts, LM_L_l67_Playfield, LM_L_l67_sparkyhead, LM_L_l67_underPF, LM_L_l68_BR3, LM_L_l68_BS3, LM_L_l68_Hammer, LM_L_l68_HammerUp, LM_L_l68_Layer1, LM_L_l68_Parts, LM_L_l68_Playfield, LM_L_l68_Rod, LM_L_l68_sparkyhead, LM_L_l68_sw35, LM_L_l68_underPF, LM_L_l69_Hammer, LM_L_l69_HammerUp, LM_L_l69_Layer1, LM_L_l69_Parts, LM_L_l69_Playfield, LM_L_l69_sparkyhead, LM_L_l69_sw35, LM_L_l69_underPF, LM_L_l70_Hammer, LM_L_l70_HammerUp, LM_L_l70_Layer1, LM_L_l70_Parts, LM_L_l70_Playfield, _
'	LM_L_l70_sparkyhead, LM_L_l70_sw35, LM_L_l70_sw61, LM_L_l70_underPF, LM_L_l71_Hammer, LM_L_l71_HammerUp, LM_L_l71_Layer1, LM_L_l71_Layer3, LM_L_l71_Lspinner, LM_L_l71_Parts, LM_L_l71_Playfield, LM_L_l71_cemeteryMOD, LM_L_l71_cross, LM_L_l71_sw36, LM_L_l71_sw37, LM_L_l71_sw60, LM_L_l71_sw61, LM_L_l71_sw62, LM_L_l71_underPF, LM_L_l77_Layer1, LM_L_l77_Parts, LM_L_l77_Playfield, LM_L_l77_sw0709, LM_L_l77_sw36, LM_L_l77_sw60, LM_L_l77_underPF, LM_L_l78_Hammer, LM_L_l78_Layer1, LM_L_l78_Parts, LM_L_l78_Playfield, LM_L_l78_sw0709, LM_L_l78_sw3, LM_L_l78_underPF, LM_L_l79_Layer1, LM_L_l79_Parts, LM_L_l79_Playfield, LM_L_l79_sw0709, LM_L_l79_underPF, LM_L_l80_Layer1, LM_L_l80_Parts, LM_L_l80_Playfield, LM_L_l80_sw0709, LM_L_l80_underPF, LM_L_optos_Layer1, LM_L_optos_Parts, LM_L_optos_Playfield, LM_L_optos_lockPost, LM_L_optos_underPF)
' VLM  Arrays - End


'******************************************************
'  ZTIM: Timers
'******************************************************

CorTimer.Interval = 10
CorTimer.Enabled = True
Sub CorTimer_Timer(): Cor.Update: End Sub


Dim FrameTime, InitFrameTime
InitFrameTime = 0

FrameTimer.Interval = -1
FrameTimer.Enabled = True
Sub FrameTimer_Timer() 
	FrameTime = gametime - InitFrameTime 'Calculate FrameTime as some animuations could use this
	InitFrameTime = gametime	'Count frametime
	BSUpdate
	UpdateBallBrightness
	RollingUpdate				'Update rolling sounds
	DoSTAnim					'Standup target animations
	UpdateStandupTargets
	DoDTAnim					'Drop target animations
	UpdateDropTargets
	Sparky
End Sub


sub hundredMS_timer()
	'Sparky magnet effects
	If ELChairMagCounter > 0 Then
		ELChairMagCounter = ELChairMagCounter - 1
'		debug.print "Counter: " & ELChairMagCounter
		if ELChairMagCounter <= 0 Then
			ELChairMagCounter = 0
			RMag.GrabCenter = True
'			debug.print "Grab it"
		end if
	end if

	'Shaker motor effects
	If ShakerCount >= 0 Then
		'debug.print "ShakerCount: "&ShakerCount
		If ShakerCount >= ShakerCountMax Then
			SoundShaker False
			ShakerCount = -1
		Else
			ShakerCount = ShakerCount + 1
		End If
	End If
end sub

'******************************************************
'  ZINI: Table Initialization and Exiting
'******************************************************


Sub Table_Init
	VPMInit Me
	InitVpmFFlipsSAM
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description: Exit Sub
		.SplashInfoLine = "Metallica (Stern 2013) VPW"
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.HandleMechanics = 1
		.Hidden = 0
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
	End With
	On Error Goto 0
	
	vpmMapLights AllLamps 

	' Tilt bob
	vpmNudge.TiltSwitch = -7
	vpmNudge.Sensitivity = 5
	vpmNudge.TiltObj = Array(Bumper1b,Bumper2b,Bumper3b,LeftSlingshot,RightSlingshot)

	Set MBall1 = sw21.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set MBall2 = sw20.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set MBall3 = sw19.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set MBall4 = sw18.CreateSizedballWithMass(Ballsize/2,Ballmass)
	gBOT = Array(MBall1, MBall2, MBall3, MBall4)

	Controller.Switch(21) = 1
	Controller.Switch(20) = 1
	Controller.Switch(19) = 1
	Controller.Switch(18) = 1

	' Captive ball
	Set CapBall = ekicker.createball:ekicker.kick 0,0,0
	CapBall.FrontDecal = "NoScratches"

	' Sparky head ball
	Set cBall = ckicker.createball
	ckicker.Kick 0, 0


	' Auto plunger
	Const IMPowerSetting = 55
	Const IMTime = 0.6
	Set plungerIM = New cvpmImpulseP
	With plungerIM
		.InitImpulseP swplunger, IMPowerSetting, IMTime
		.Random 0.3
		'.InitExitSnd SoundFX("Plunger_Release_Ball" ,DOFContactors), SoundFX("Plunger_Release_No_Ball" ,DOFContactors)
		.CreateEvents "plungerIM"
	End With

	' Magnets
	Set LMag=New cvpmMagnet
	LMag.InitMagnet LMagnet,15
	LMag.GrabCenter = False

	Set RMag=New cvpmMagnet
	RMag.InitMagnet RMagnet,40
	RMag.GrabCenter = False

	Set HMag=New cvpmMagnet
	HMag.InitMagnet HMagnet,20
	HMag.GrabCenter = False
	HMag.CreateEvents "HMag"


	' Misc initializations
	UpPost.Isdropped = True

	LStep = 0 : LeftSlingShot.TimerEnabled = True ' Initialize Step to 0
	RStep = 0 : RightSlingShot.TimerEnabled = True ' Initialize Step to 0

	InitInsertRGB
	PlasticBottoms

End Sub

Sub Table_Paused: Controller.Pause = 1: End Sub
Sub Table_unPaused: Controller.Pause = 0: End Sub
Sub Table_exit:	Controller.Stop: End sub




'*******************************************
'  ZOPT: User Options
'*******************************************

Dim VRStrobes: VRStrobes = 5              		' 0 = Disabled, 1 = Left Light Always On, 2 = Right Light Always On, 3 = Slow Flashes, 4 = Moderate Flashes,  5 = Full Effect
Dim VRRoomChoice: VRRoomChoice = 1              ' 1 = Blacklight room  2 = Ultra Minimal room
Dim LightLevel : LightLevel = 0.25				' Level of room lighting (0 to 1), where 0 is dark and 100 is brightest
Dim ColorLUT : ColorLUT = 1						' Color desaturation LUTs: 1 to 11, where 1 is normal and 11 is black'n'white
Dim VolumeDial : VolumeDial = 0.8           	' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume : BallRollVolume = 0.5   	' Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.5 		' Level of ramp rolling volume. Value between 0 and 1
Dim SnakeMod : SnakeMod		 =	1				' 0 = standard table, 1 = custom snake ramp mod (right ramps and plastic)
Dim CemeteryMod : CemeteryMod = 1			    ' 0 = standard table, 1 = cemetery mod
Dim OutpostDif : OutpostDif = 1					' 1 = Easy, 2 = Normal
Dim ShakerSSF: ShakerSSF = 1					' 0 - Disabled, 1 - Enabled
Dim ShakerIntensity: ShakerIntensity = 1		' 0 - Low, 1- - Normal, 2 - High
Dim VPMDMDVisible: VPMDMDVisible = 1 			' 0 - Not Visible, 1 - Visible
Dim MayoOpt : MayoOpt = 1						' 0 - Disabled, 1 - Enabled
Dim ReflectOpt : ReflectOpt = 1					' 0 - Disabled, 1 - Enabled

' Called when options are tweaked by the player. 
' - 0: game has started, good time to load options and adjust accordingly
' - 1: an option has changed
' - 2: options have been reseted
' - 3: player closed the tweak UI, good time to update staticly prerendered parts
' Table.Option arguments are: 
' - option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional arry of literal strings
Dim dspTriggered : dspTriggered = False
Sub Table_OptionEvent(ByVal eventId)
    If eventId = 1 And Not dspTriggered Then dspTriggered = True : DisableStaticPreRendering = True : End If
	Dim BP

	'Desktop DMD
	VPMDMDVisible = Table.Option("Desktop DMD", 0, 1, 1, 1, 0, Array("NOT VISIBLE", "VISIBLE"))
	If VPMDMDVisible = 1 Then
		TextBox.Visible = 1
	Else
		TextBox.Visible = 0
	End If

	' Outpost Difficulty
	OutpostDif = Table.Option("Outpost Difficulty", 1, 2, 1, 2, 0, Array("EASY", "NORMAL"))
	If OutpostDif = 1 Then
		zCol_Outlane_L_E.collidable = 1
		zCol_Outlane_R_E.collidable = 1
		zCol_Outlane_L_H.collidable = 0
		zCol_Outlane_R_H.collidable = 0
		For each BP in BP_LPost: BP.x=zCol_Outlane_L_E.x: BP.y=zCol_Outlane_L_E.y: Next
		For each BP in BP_RPost: BP.x=zCol_Outlane_R_E.x: BP.y=zCol_Outlane_R_E.y: Next
	Else
		zCol_Outlane_L_E.collidable = 0
		zCol_Outlane_R_E.collidable = 0
		zCol_Outlane_L_H.collidable = 1
		zCol_Outlane_R_H.collidable = 1
		For each BP in BP_LPost: BP.x=zCol_Outlane_L_H.x: BP.y=zCol_Outlane_L_H.y: Next
		For each BP in BP_RPost: BP.x=zCol_Outlane_R_H.x: BP.y=zCol_Outlane_R_H.y: Next
	End If


	' Snake ramp mod
	SnakeMod = Table.Option("Snake Ramp Mod", 0, 1, 1, 0, 0, Array("OFF", "ON"))
	For each BP in BP_SnakeMOD: BP.visible = SnakeMod: Next
	
	If SnakeMod = 1 Then
		For each BP in BP_SnakeMOD: BP.visible = 1: Next
		For each BP in BP_Layer2b: BP.visible = 1: Next
		For each BP in BP_Layer2a: BP.visible = 0: Next
	Else
		For each BP in BP_SnakeMOD: BP.visible = 0: Next
		For each BP in BP_Layer2b: BP.visible = 0: Next
		For each BP in BP_Layer2a: BP.visible = 1: Next
	End If


	' Cemetery mod
	CemeteryMod = Table.Option("Cemetery Mod", 0, 1, 1, 0, 0, Array("OFF", "ON"))
	For each BP in BP_cemeteryMOD: BP.visible = CemeteryMod: Next

    ' Shaker SSF 
    ShakerSSF = Table.Option("Shaker SSF", 0, 1, 1, 1, 0, Array("DISABLED", "ENABLED"))

    ' Shaker SSF Intensity
    ShakerIntensity = Table.Option("Shaker SSF Intensity", 0, 2, 1, 1, 0, Array("LOW", "NORMAL", "HIGH"))

	' Mayo
	MayoOpt = Table.Option("Playfield Glass", 0, 1, 1, 1, 0, Array("CLEAN", "GREASY"))

	' PF Reflections (pfreflects(enabled))
	ReflectOpt = Table.Option("Playfield Reflections", 0, 2, 1, 1, 0, Array("NONE", "BALL AND STATIC", "BALL AND DYNAMIC"))
	Select Case ReflectOpt
		Case 0:
			BM_Playfield.ReflectionProbe = ""
			playfield_mesh.ReflectionProbe = ""
			For each BP in Reflections: BP.ReflectionEnabled=0:Next
		Case 1:
			BM_Playfield.ReflectionProbe = "Playfield Reflections"
			playfield_mesh.ReflectionProbe = "Playfield Reflections"
			For each BP in Reflections: BP.ReflectionEnabled=0:Next
		Case 2:
			BM_Playfield.ReflectionProbe = "Playfield Reflections"
			playfield_mesh.ReflectionProbe = "Playfield Reflections"
			For each BP in Reflections: BP.ReflectionEnabled=1:Next
	End Select

	' VR Room
	VRRoomChoice = Table.Option("VR Room", 1, 2, 1, 1, 0, Array("BLACKLIGHT ROOM", "ULTRA MINIMAL"))
	If RenderingMode=2 OR TestVRonDT=True Then VRRoom = VRRoomChoice: Else VRRoom = 0: End If
	SetupRoom

	' VR Room Stobing
	VRStrobes = Table.Option("VR BL Room Srobes", 0, 5, 1, 5, 0, Array("DISABLED", "LEFT ON", "RIGHT ON", "SLOW FLASHING EFFECT", "MODERATE EFFECT", "FULL EFFECT"))
	If RenderingMode=2 OR TestVRonDT=True Then
		If VRStrobes = 1 Then
			TurnLeftStrobeOn
		ElseIf VRStrobes = 2 Then
			TurnRightStrobeOn
		Else
			TurnStrobesOff
		End If
	End If


	' Color Saturation
    ColorLUT = Table.Option("Color Saturation", 1, 11, 1, 1, 0, _
		Array("Normal", "Desaturated 10%", "Desaturated 20%", "Desaturated 30%", "Desaturated 40%", "Desaturated 50%", _
        "Desaturated 60%", "Desaturated 70%", "Desaturated 80%", "Desaturated 90%", "Black 'n White"))
	if ColorLUT = 1 Then Table.ColorGradeImage = ""
	if ColorLUT = 2 Then Table.ColorGradeImage = "colorgradelut256x16-10"
	if ColorLUT = 3 Then Table.ColorGradeImage = "colorgradelut256x16-20"
	if ColorLUT = 4 Then Table.ColorGradeImage = "colorgradelut256x16-30"
	if ColorLUT = 5 Then Table.ColorGradeImage = "colorgradelut256x16-40"
	if ColorLUT = 6 Then Table.ColorGradeImage = "colorgradelut256x16-50"
	if ColorLUT = 7 Then Table.ColorGradeImage = "colorgradelut256x16-60"
	if ColorLUT = 8 Then Table.ColorGradeImage = "colorgradelut256x16-70"
	if ColorLUT = 9 Then Table.ColorGradeImage = "colorgradelut256x16-80"
	if ColorLUT = 10 Then Table.ColorGradeImage = "colorgradelut256x16-90"
	if ColorLUT = 11 Then Table.ColorGradeImage = "colorgradelut256x16-100"


    ' Sound volumes
    VolumeDial = Table.Option("Mech Volume", 0, 1, 0.01, 0.8, 1)
    BallRollVolume = Table.Option("Ball Roll Volume", 0, 1, 0.01, 0.5, 1)
	RampRollVolume = Table.Option("Ramp Roll Volume", 0, 1, 0.01, 0.5, 1)
	' VPW Optimization: cache combined volume products (rolling sound paths read these per ball per frame).
	BallRollVol = BallRollVolume * VolumeDial
	RampRollVol = RampRollVolume * VolumeDial


	' Room brightness
	LightLevel = NightDay/100
	SetRoomBrightness LightLevel   'Uncomment this line for lightmapped tables.


    If eventId = 3 And dspTriggered Then dspTriggered = False : DisableStaticPreRendering = False : End If
End Sub


'**********************************
' 	ZMAT: General Math Functions
'**********************************
' These get used throughout the script.

Dim PI
PI = 4 * Atn(1)

' --- VPW Optimization: pre-computed math/audio/render constants ---
' Eliminates per-frame divisions and string concatenations from hot paths.
Dim PIover180 : PIover180 = PI / 180
Dim d180overPI : d180overPI = 180 / PI
Dim InvTWHalf, InvTHHalf, TW_d2, BS_d2, BallRollVol, RampRollVol
' tablewidth/tableheight/tnob are defined earlier; BallRollVolume/RampRollVolume/VolumeDial
' are defined later but constants — recompute these in Table_Init too if any change.
InvTWHalf = 2 / tablewidth
InvTHHalf = 2 / tableheight
TW_d2     = tablewidth / 2
BS_d2     = BallSize / 2
' Defaults; recomputed in Table_Init after Table.Option resolves user values.
BallRollVol = 0.5 * 0.8
RampRollVol = 0.5 * 0.8

' Pre-built string IDs to eliminate "Name" & i concatenation in per-frame loops.
Dim BallRollStr(8), RampLoopStr(8), WireLoopStr(8)
Dim vpwInitI
For vpwInitI = 0 To 8
	BallRollStr(vpwInitI) = "BallRoll_" & vpwInitI
	RampLoopStr(vpwInitI) = "RampLoop"  & vpwInitI
	WireLoopStr(vpwInitI) = "wireloop"  & vpwInitI
Next

' Per-ball delta cache for UpdateBallBrightness (.color write guard).
Dim LastBallColor(8)
For vpwInitI = 0 To 8 : LastBallColor(vpwInitI) = -1 : Next

' Per-ball visible cache for BSUpdate (.visible write guard).
Dim LastBSVisible(8)
For vpwInitI = 0 To 8 : LastBSVisible(vpwInitI) = -1 : Next

' Sparky cached rotations (avoid redundant Foreach BP writes).
Dim LastSparkyRX, LastSparkyRY
LastSparkyRX = -99999 : LastSparkyRY = -99999

' Active-target counters for DoSTAnim / DoDTAnim early exits.
Dim STActiveCount, DTActiveCount, STVisActive, DTVisActive
STActiveCount = 0 : DTActiveCount = 0 : STVisActive = 0 : DTVisActive = 0
' --- end VPW Optimization constants ---

Function dSin(degrees)
	dsin = Sin(degrees * PIover180)
End Function

Function dCos(degrees)
	dcos = Cos(degrees * PIover180)
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

Function RotPointLH(x,y,angle)  'Left Handed
	Dim rx, ry
	rx = x * dCos(angle) + y * dSin(angle)
	ry = - x * dSin(angle) + y * dCos(angle)
	RotPointLH = Array(rx,ry)
End Function



'******************************************************
'  ZANI: Misc Animations
'******************************************************

' Flippers
Sub LeftFlipper_Animate
	Dim a: a = LeftFlipper.CurrentAngle 
	FlipperLSh.RotZ = a

	Dim v, BP : 
	v = 255.0 * (122.0 - LeftFlipper.CurrentAngle) / (122.0 - 69.0)

	For Each BP in BP_Lflip
		BP.RotZ = a 
		BP.visible = v < 128.0
	Next
	For Each BP in BP_LflipU
		BP.RotZ = a 
		BP.visible = v >= 128.0
	Next	
End Sub

Sub RightFlipper_Animate
	Dim a: a = RightFlipper.CurrentAngle 
	FlipperRSh.RotZ = a

	Dim v, BP : 
	v = 255.0 * (-122.0 - RightFlipper.CurrentAngle) / (-122.0 + 69.0)

	For Each BP in BP_Rflip
		BP.RotZ = a 
		BP.visible = v < 128.0
	Next
	For Each BP in BP_RflipU
		BP.RotZ = a 
		BP.visible = v >= 128.0
	Next	
End Sub


' Spinners
Sub LSpinner_Animate
	Dim a: a = LSpinner.CurrentAngle 
	Dim BP : For Each BP in BP_LSpinner: BP.RotX = -a: Next
End Sub

Sub RSpinner_Animate
	Dim a: a = RSpinner.CurrentAngle 
	Dim BP : For Each BP in BP_RSpinner: BP.RotX = -a: Next
End Sub


' Gates
Sub sw50g_Animate
	Dim a: a = sw50g.CurrentAngle 
	Dim BP : For Each BP in BP_sw50p: BP.RotX = a: Next
End Sub

Sub sw47g_Animate
	Dim a: a = sw47g.CurrentAngle 
	Dim BP : For Each BP in BP_sw47p: BP.RotX = a: Next
End Sub


' Bumpers
Sub Bumper1b_Animate
	Dim z, BP
	z = Bumper1b.CurrentRingOffset
	For Each BP in BP_BR1 : BP.transz = z: Next
End Sub

Sub Bumper2b_Animate
	Dim z, BP
	z = Bumper2b.CurrentRingOffset
	For Each BP in BP_BR2 : BP.transz = z: Next
End Sub

Sub Bumper3b_Animate
	Dim z, BP
	z = Bumper3b.CurrentRingOffset
	For Each BP in BP_BR2 : BP.transz = z: Next
End Sub

'Dim Bumpers : Bumpers = Array(Bumper1, Bumper2, Bumper3)


' Switches
Sub sw1_Animate
	Dim z : z = sw1.CurrentAnimOffset
	Dim BP : For Each BP in BP_sw1 : BP.transz = z: Next
End Sub

Sub sw3_Animate
	Dim z : z = sw3.CurrentAnimOffset
	Dim BP : For Each BP in BP_sw3 : BP.transz = z: Next
End Sub

Sub sw24_Animate
	Dim z : z = sw24.CurrentAnimOffset
	Dim BP : For Each BP in BP_sw24 : BP.transz = z: Next
End Sub

Sub sw25_Animate
	Dim z : z = sw25.CurrentAnimOffset
	Dim BP : For Each BP in BP_sw25 : BP.transz = z: Next
End Sub

Sub sw28_Animate
	Dim z : z = sw28.CurrentAnimOffset
	Dim BP : For Each BP in BP_sw28 : BP.transz = z: Next
End Sub

Sub sw29_Animate
	Dim z : z = sw29.CurrentAnimOffset
	Dim BP : For Each BP in BP_sw29 : BP.transz = z: Next
End Sub

Sub sw43_Animate
	Dim z : z = sw43.CurrentAnimOffset
	Dim BP : For Each BP in BP_sw43 : BP.transz = z: Next
End Sub

Sub sw44_Animate
	Dim z : z = sw44.CurrentAnimOffset
	Dim BP : For Each BP in BP_sw44 : BP.transz = z: Next
End Sub

Sub sw45_Animate
	Dim z : z = sw45.CurrentAnimOffset
	Dim BP : For Each BP in BP_sw45 : BP.transz = z: Next
End Sub

Sub sw46_Animate
	Dim z : z = sw46.CurrentAnimOffset
	Dim BP : For Each BP in BP_sw46 : BP.transz = z: Next
End Sub




'******************************************************
' 	ZBBR: BALL BRIGHTNESS
'******************************************************

Const BallBrightness =  1      	'Ball brightness - Value between 0 and 1 (0=Dark ... 1=Bright)

' Constants for plunger lane ball darkening.
' You can make a temporary wall in the plunger lane area and use the co-ordinates from the corner control points.
Const PLOffset = 0.5			'Minimum ball brightness scale in plunger lane
Const PLLeft = 850 		    	'X position of punger lane left
Const PLRight = 950 			'X position of punger lane right
Const PLTop = 1000 				'Y position of punger lane top
Const PLBottom = 1900 			'Y position of punger lane bottom
Dim PLGain: PLGain = (1-PLOffset)/(PLTop-PLBottom)

Sub UpdateBallBrightness
	' VPW Optimization: cache gBOT(s).x/y/z (was read up to 6x per ball) + .color write guard.
	Dim s, b_base, d_w, b_r, ubBot, bot_s, bx, by, bz, newCol
	b_base = 120 * BallBrightness + 135
	ubBot = UBound(gBOT)

	For s = 0 To ubBot
		Set bot_s = gBOT(s)
		bx = bot_s.x : by = bot_s.y : bz = bot_s.z
		If InRect(bx, by, 830,1780, 830,1850, 470,2070, 440,2010) Then
			d_w = 30
		Else
			d_w = b_base
			If bz > 30 Then d_w = b_base * (1 - bz/500)
			If bz < 0  Then d_w = b_base * (1 + bz/180)
			If d_w < 30 Then d_w = 30
			If InRect(bx, by, PLLeft,PLBottom, PLLeft,PLTop, PLRight,PLTop, PLRight,PLBottom) Then
				d_w = d_w * (PLOffset + PLGain * (by - PLBottom))
			End If
		End If
		b_r = Int(d_w)
		If b_r > 255 Then b_r = 255
		newCol = b_r + (b_r * 256) + (b_r * 256 * 256)
		If LastBallColor(s) <> newCol Then
			bot_s.color = newCol
			LastBallColor(s) = newCol
		End If
	Next
End Sub


'****************************
' 	ZRBR: Room Brightness
'****************************

'This code only applies to lightmapped tables. It is here for reference. 
'NOTE: Objects bightness will be affected by the Day/Night slider only if their blenddisablelighting property is less than 1.
'      Lightmapped table primitives have their blenddisablelighting equal to 1, therefore we need this SetRoomBrightness sub 
'      to handle updating their effective ambient brighness.

' Update these arrays if you want to change more materials with room light level
Dim RoomBrightnessMtlArray: RoomBrightnessMtlArray = Array("VLM.Bake.Active","VLM.Bake.Solid") 

Sub SetRoomBrightness(lvl)
	If lvl > 1 Then lvl = 1
	If lvl < 0 Then lvl = 0

	' Lighting level
	Dim v: v=(lvl * 245 + 10)/255

	Dim i: For i = 0 to UBound(RoomBrightnessMtlArray)
		ModulateMaterialBaseColor RoomBrightnessMtlArray(i), i, v
	Next
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




'******************************************************
'	ZKEY: Key Press Handling
'******************************************************

Sub table_KeyDown(ByVal keycode)

	If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress
	If keycode = LeftFlipperKey and VRRoom = 1 then VRFlipperLEFT.X = VRFlipperLEFT.X + 5

	If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress 
	If keycode = RightFlipperKey and VRRoom = 1 then VRFlipperRight.X = VRFlipperRight.X - 5

	'if keycode = Leftmagnasave and VRRoom = 1 then TestStrobes

	If Keycode = StartGameKey Then Controller.Switch(16) = 1

	If Keycode = StartGameKey and VRRoom = 1 then 
		VRStartButton.Y = VRStartButton.Y - 5
		VRStartButton2. Y =VRStartButton2.Y - 5
		StrobeAttractTimer.enabled = False ' Is this the best way to do this?
		TurnStrobesOff
	End if

	If keycode = keyFront Then Controller.Switch(15) = 1	
	If keycode = keyFront and VRRoom = 1 Then VRTourneyButton.Y =VRTourneyButton.Y -5	

	If keycode = PlungerKey Then Plunger.Pullback
	If keycode = PlungerKey and VRRoom = 1 then TimerVRPlunger.Enabled = True: TimerVRPlunger2.Enabled = False

	If keycode = LeftTiltKey Then Nudge 90, 2:SoundNudgeLeft
	If keycode = RightTiltKey Then Nudge 270, 2:SoundNudgeRight
	If keycode = CenterTiltKey Then Nudge 0, 3:SoundNudgeCenter

	If keycode = AddCreditKey or keycode = AddCreditKey2 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If

	If vpmKeyDown(keycode) Then Exit Sub
End Sub


Sub table_KeyUp(ByVal keycode)

	If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
	If keycode = LeftFlipperKey and VRRoom = 1 then VRFlipperLEFT.X = VRFlipperLEFT.X - 5

	If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress
	If keycode = RightFlipperKey and VRRoom = 1 then VRFlipperRight.X = VRFlipperRight.X + 5

	If keycode = keyFront Then Controller.Switch(15) = 0	
	If keycode = keyFront and VRRoom = 1  Then VRTourneyButton.Y = VRTourneyButton.Y +5

	If Keycode = StartGameKey Then Controller.Switch(16) = 0
	If Keycode = StartGameKey and VRRoom = 1 then VRStartButton.Y = VRStartButton.Y + 5: VRStartButton2. Y =VRStartButton2.Y + 5

	If keycode = PlungerKey Then
		Plunger.Fire
		SoundPlungerReleaseBall

		If VRRoom = 1 then 
			TimerVRPlunger.Enabled = False 
			TimerVRPlunger2.Enabled = True   ' VR Plunger
			VRPlunger.Y = 2312 '= sitting pos.
			Plungereye.y = VRPlunger.Y + 40
		end if
	End If

	If vpmKeyUp(keycode) Then Exit Sub
End Sub




'******************************************************
'	ZSOL: Solenoids & Flashers
'******************************************************

SolCallback(1) = "solTrough"
SolCallback(2) = "solAutofire"
'SolCallback(3) = "LMag.MagnetOn="
SolCallback(3) = "SolLMag"
SolCallback(4) = "SolRMag"		'electric chair magnet
SolCallback(5) = "SnakeKick"
SolCallback(6) = "ScoopKick"
'SolCallback(7) = "orbitpost"  'premium not used
SolCallback(8) = "SolShaker" ' optional
'SolCallback(9)  = "" 'Bumpers
'SolCallback(10) = ""
'SolCallback(11) = ""
SolCallback(12) = "SnakeJawLatch"  ' premium snake jaw latch
'SolCallback(13) = Left Slingshot
'SolCallback(14) = Right Slingshot
SolCallback(15) = "SolLFlipper"
SolCallback(16) = "SolRFlipper"
'SolCallback(17) = not used
SolCallback(18) = "SolSparkyHead"
SolModCallback(19) = "SolCrossPrim"   'Grave Marker
SolCallback(20) = "CrossMotor"      'premium grave marker motor
SolModCallback(21) = "SolBackPanelLeft"  'Back Panel Left
SolModCallback(22) = "SolBackPanelRight"   'Back Panel Right
SolModCallback(23) = "SolLeftRamp"    '"Flash23" Left Ramp
SolModCallback(25) = "SolPopInsert"  'Pop Bumpers
SolModCallback(26) = "SolGraveMarker" 'Grave Marker x2
SolModCallback(27) = "SolElecChair"   'Electric Chair x2
SolModCallback(28) = "SolSparkyFlash"   'Electric Chair Spot
SolModCallback(29) = "SolRightRamp"   'Right Ramp
SolModCallback(30) = "SolSnakeFlasher"  'premium snake flasher 'captive ball flasher
SolModCallback(31) = "SolCoffinFlasher" 'coffin insert flasher x2
SolmodCallback(32) = "SolElecChairInsert"   'electric chair insert flasher
SolCallback(51) = "CLockRelease"
SolCallback(52) = "CoffinMagDown" 'Premium coffinMagnet Down
SolCallback(53) = "Hammer" 'premium hammer
SolCallback(54) = "ResetDTs" ' premium DTresets
Solcallback(55) = "OrbitPost" ' premium loop post
SolCallback(56) = "SolSnakeJaw" '  premium Snake Jaw
SolCallback(57) = "UpdateCoffinProcessorBoard " ' premium Coffin Magnet Mode (2 bits)
SolCallback(58) = "UpdateCoffinProcessorBoard " ' premium Coffin Magnet Mode (2 bits)


' Flashers 
'*****************


Sub SolCrossPrim(level)
	f19.state = level
end sub

Sub SolBackPanelLeft(level)
	f21.state = level
	' Mayo option
	If MayoOpt = 1 Then Mayo_f21.state = level
	' Strobe options
	If VRroom = 1 and VRStrobes >= 3 then 
		If level > 0.5 and LeftStrobeOn = False Then
			If RightStrobeOn = True Then 
				If VRStrobes = 4 Then FullStrobeShort
				If VRStrobes = 5 Then FullStrobeLong 
			Else 
				If VRStrobes = 3 Then AlternateStrobesOnTimed 300
				If VRStrobes = 4 OR VRStrobes = 5 Then TurnLeftStrobeOn
			End If
			Exit Sub
		End If
		If level < 0.2 and LeftStrobeOn = True Then 
			If VRStrobes = 4 OR VRStrobes = 5 Then TurnStrobesOff
		End If
	End If
end sub

Sub SolBackPanelRight(level)
	f22.state = level
	' Mayo option
	If MayoOpt = 1 Then Mayo_f22.state = level
	' Strobe options
	If VRroom = 1 and VRStrobes >= 3 then 
		If level > 0.5 and RightStrobeOn = False Then 
			If LeftStrobeOn = True Then 
				If VRStrobes = 4 Then FullStrobeShort
				If VRStrobes = 5 Then FullStrobeLong 
			Else 
				If VRStrobes = 3 Then AlternateStrobesOnTimed 300
				If VRStrobes = 4 OR VRStrobes = 5 Then TurnRightStrobeOn
			End If
			Exit Sub
		End If
		If level < 0.2 and RightStrobeOn = True Then 
			If VRStrobes = 4 OR VRStrobes = 5 Then TurnStrobesOff
		End If
	End If
end sub

sub SolLeftRamp(level)   
	f23.state = level
end sub

Sub SolPopInsert(level)
	f25.state = level
End Sub

sub SolGraveMarker(level)
	f26.state = level
	f26a.state = level
end sub

Sub SolElecChair(level)
	f27.state = level
	f27a.state = level
End Sub

sub SolSparkyFlash(level)     
	f28.state = level
	f28a.state = level
	If MayoOpt = 1 Then Mayo_f28.state = level
	If MayoOpt = 1 Then Mayo_f28a.state = level
end sub

sub SolRightRamp(level)
	f29.state = level
end sub

sub SolSnakeFlasher(level)
	f30.state = level
end sub

Sub SolCoffinFlasher(level)
	f31.state = level
End Sub

Sub SolElecChairInsert(level)
	f32.state = level
	If MayoOpt = 1 Then Mayo_F32.state = level
End Sub




' Other Solenoids
'*****************

' Posts

Sub orbitpost(Enabled)
'	Debug.Print "Orbit Post = " & Enabled & " at " & GameTime
	If Enabled Then
		UpPost.Isdropped=0
		FlipperUpPost.RotateToEnd		
		RandomSoundLoopPostSolenoid 1, BM_UpPost
	Else
		UpPost.Isdropped=1
		FlipperUpPost.RotateToStart
		RandomSoundLoopPostSolenoid 0, BM_UpPost
	End If
End Sub

Sub FlipperUpPost_Animate
	Dim a : a = FlipperUpPost.CurrentAngle
	Dim BP : For Each BP in BP_UPPost : BP.transz = a: Next
End Sub


'/////////////////////////////  DISAPPEARING POSTS - LOOP POST - SOLENOID SOUNDS  ////////////////////////////
dim LoopPostSolenoidSoundFactor
LoopPostSolenoidSoundFactor = 3											'volume multiplier; must not be zero
Sub RandomSoundLoopPostSolenoid(toggle, obj)
	Select Case toggle
		Case 1
			Select Case Int(Rnd*2)+1
				Case 1 : PlaySoundAtLevelStatic SoundFX("TOM_LoopPost_UP_3",DOFContactors), 0.75 * LoopPostSolenoidSoundFactor, obj
				Case 2 : PlaySoundAtLevelStatic SoundFX("TOM_LoopPost_UP_4",DOFContactors), 0.75 * LoopPostSolenoidSoundFactor, obj
			End Select
		Case 0
			Select Case Int(Rnd*2)+1
				Case 1 : PlaySoundAtLevelStatic SoundFX("TOM_LoopPost_DOWN_3",DOFContactors), 0.9 * LoopPostSolenoidSoundFactor, obj
				Case 2 : PlaySoundAtLevelStatic SoundFX("TOM_LoopPost_DOWN_4",DOFContactors), 0.9 * LoopPostSolenoidSoundFactor, obj
			End Select
	End Select
End Sub

Sub CLockRelease(Enabled)
	If Enabled Then
		RandomSoundLoopPostSolenoid 0, BM_lockPost
		CoffinStop.isDropped = 1
	Else
		RandomSoundLoopPostSolenoid 1, BM_lockPost
		CoffinStop.isDropped = 0
	End If
End Sub


' Auto plungers
Sub solAutofire(Enabled)
	If Enabled Then
		PlungerIM.AutoFire
	End If
End Sub



' Kicker
Sub ScoopKick(Enabled)
	If Enabled Then
		Kicker3.kick 39.5+rnd,54+rnd*4,0
		SoundSaucerKick 1,Kicker3
	End If
End Sub

Sub kicker3_Hit
	debug.print "kicker3_Hit"
	Controller.Switch(51) = 1
End Sub

Sub kicker3_UnHit
	debug.print "kicker3_UnHit"
	Controller.Switch(51) = 0
End Sub



'******************************************************
'	ZDRN: Drain, Trough, and Ball Release
'******************************************************

Sub sw21_Hit   : Controller.Switch(21) = 1 : UpdateTrough : End Sub
Sub sw21_UnHit : Controller.Switch(21) = 0 : UpdateTrough : End Sub
Sub sw20_Hit   : Controller.Switch(20) = 1 : UpdateTrough : End Sub
Sub sw20_UnHit : Controller.Switch(20) = 0 : UpdateTrough : End Sub
Sub sw19_Hit   : Controller.Switch(19) = 1 : UpdateTrough : End Sub
Sub sw19_UnHit : Controller.Switch(19) = 0 : UpdateTrough : End Sub
Sub sw18_Hit   : Controller.Switch(18) = 1 : UpdateTrough : End Sub
Sub sw18_UnHit : Controller.Switch(18) = 0 : UpdateTrough : End Sub
Sub drain_Hit    : UpdateTrough : RandomSoundDrain drain : End Sub
Sub drain_UnHit  : UpdateTrough : End Sub

Sub UpdateTrough
	UpdateTroughTimer.Interval = 100
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer
	If sw21.BallCntOver = 0 Then sw20.kick 57, 10
	If sw20.BallCntOver = 0 Then sw19.kick 57, 10
	If sw19.BallCntOver = 0 Then sw18.kick 57, 10
	If sw18.BallCntOver = 0 Then drain.kick 57, 10
	Me.Enabled = 0
End Sub


'*****************  DRAIN & RELEASE  ******************

Sub SolTrough(enabled)
	If enabled Then 
		sw21.kick 57, 20
		vpmTimer.PulseSw 22
		RandomSoundBallRelease sw21
	End If
End Sub




'******************************************************
'	ZFLP: FLIPPERS
'******************************************************

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

'Flipper collide subs
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




'*******************************************
'	ZSHA: Shaker Motor
'*******************************************

Const ShakerSoundLevel = 1
Const ShakerCountMax = 5
Dim ShakerCount: ShakerCount = -1


Sub SolShaker(enabled)
	If Enabled Then 
		If ShakerCount < 0 Then SoundShaker True
		ShakerCount = 0
	End If
End Sub


Sub SoundShaker(Enabled)
	'.print "SoundShaker  "&Enabled
	If ShakerSSF = 0 Then Exit Sub
	If Enabled Then
		Select Case ShakerIntensity
			Case 0
				PlaySoundAtLevelExistingStaticLoop ("SY_TNA_REV02_Shaker_Level_01_RampUp_and_Loop"), ShakerSoundLevel, Bumper3b
			Case 1
				PlaySoundAtLevelExistingStaticLoop ("SY_TNA_REV02_Shaker_Level_02_RampUp_and_Loop"), ShakerSoundLevel, Bumper3b
			Case 2
				PlaySoundAtLevelExistingStaticLoop ("SY_TNA_REV02_Shaker_Level_03_RampUp_and_Loop"), ShakerSoundLevel, Bumper3b
		End Select
	Else
		Select Case ShakerIntensity
			Case 0
				PlaySoundAtLevelStatic ("SY_TNA_REV02_Shaker_Level_01_RampDown_Only"), ShakerSoundLevel, Bumper3b
				StopSound "SY_TNA_REV02_Shaker_Level_01_RampUp_and_Loop"
			Case 1
				PlaySoundAtLevelStatic ("SY_TNA_REV02_Shaker_Level_02_RampDown_Only"), ShakerSoundLevel, Bumper3b
				StopSound "SY_TNA_REV02_Shaker_Level_02_RampUp_and_Loop"
			Case 2
				PlaySoundAtLevelStatic ("SY_TNA_REV02_Shaker_Level_03_RampDown_Only"), ShakerSoundLevel, Bumper3b
				StopSound "SY_TNA_REV02_Shaker_Level_03_RampUp_and_Loop"
		End Select
	End If
End Sub




'******************************************************
'	ZSPK: Sparky
'******************************************************

Sub SolSparkyHead(Enabled)
	If Enabled Then
		SparkyShake
		SoundSaucerKick 0,BM_Sparkyhead
	End If
End Sub

Sub SparkyShake
	cball.vely = 10 + 2 * (RND(1) - RND(1) )
End Sub

Sub Sparky
	' VPW Optimization: cache deltas; skip BP_sparkyhead Foreach when nothing changed.
	Dim rx, ry, BP
	rx = (ckicker.y - cball.y)
	ry = (cball.x - ckicker.x)
	If rx = LastSparkyRX And ry = LastSparkyRY Then Exit Sub
	LastSparkyRX = rx : LastSparkyRY = ry
	For Each BP In BP_sparkyhead
		BP.rotx = rx
		BP.roty = ry
	Next
End Sub


' Sparky Magnet
Sub RMagnet_Hit
	RMag.AddBall ActiveBall
	If RMag.MagnetOn=true then
	End If
End Sub

Sub RMagnet_UnHit
	RMag.RemoveBall ActiveBall
End Sub



'******************************************************
'	ZCAP: Captive Ball Cube
'******************************************************

Sub CapballCube_Hit
	dim Vx, Vy, Vel, RotVxVy1, RotVxVy2
	Vel = cor.ballvel(activeball.id)
	Vx = cor.ballvelx(activeball.id)
	Vy = cor.ballvely(activeball.id)
	SoundBallBall activeball, vel
	RotVxVy1 = RotPointLH(Vx,Vy,12)
	RotVxVy2 = RotPointLH(0,0.3*RotVxVy1(1),-12)
	CapBall.velx = RotVxVy2(0)
	CapBall.vely = RotVxVy2(1)
End Sub



'******************************************************
'	ZCOF: Coffin Magnet
'******************************************************
 Const DebugCoffin = False

' See: https://missionpinball.org/mechs/magnets/stern_magnet_pcb/
' There are 4 modes defined by sol 57 / sol 58 states (this can be tested from the service menu):
' 00 - Off
' 11 - Detect balls (can detect more than one ball => sw63 radius should be around 50 VPU)
' 01 - Grab, state that is triggered after detection to turn magnet to full power and grab the ball (from diagnostics, it is limited to a 1s pulse)
' 10 - Hold & detect, hold a previously grabbed ball (lower magnet power) but also detects nearby balls (doesn't seems very reliable though, from diagnostics it is limited to a 10s pulse)
Dim CoffinMagnetMode : CoffinMagnetMode = 0
Sub UpdateCoffinProcessorBoard(unused)
	Dim D0, D1: D0 = Controller.Solenoid(57): D1 = Controller.Solenoid(58)
	If D0 = 0 And D1 = 0 Then 														' Mode = Off (pulsed every 1ms)
		If DebugCoffin Then Debug.Print "Coffin Magnet Off [" & GameTime & "]"
		CoffinMagnetMode = 0
		Controller.Switch(63) = 0
		HMag.MagnetOn = 0
		HMag.GrabCenter = false
	ElseIf D0 <> 0 And D1 = 0 Then 													' Mode = Detect/Hold (one long pulse of 10s): hold the ball while still detecting other balls
		If DebugCoffin Then Debug.Print "Coffin Magnet Detect/Hold [" & GameTime & "]"
		CoffinMagnetMode = 1
		If sw63.BallCntOver > 0 Then Controller.Switch(63) = 1 Else Controller.Switch(63) = 0
		HMag.MagnetOn = 1
		HMag.Strength = 20
		HMag.GrabCenter = false
	ElseIf D0 <> 0 And D1 <> 0 Then 												' Mode = Detect or Detect/Grab (pulsed every 250ms): just detect balls
		If DebugCoffin Then Debug.Print "Coffin Magnet Detect [" & GameTime & "]"
		CoffinMagnetMode = 2
		If sw63.BallCntOver > 0 Then Controller.Switch(63) = 1 Else Controller.Switch(63) = 0
		HMag.MagnetOn = 0
		HMag.GrabCenter = false
	ElseIf D0 = 0 And D1 <> 0 Then 													' Mode = Grab (one pulse of 1s), after Detect/Grab has detected a ball and reported it on switch 63
		If DebugCoffin Then Debug.Print "Coffin Magnet Grab [" & GameTime & "]"
		CoffinMagnetMode = 3
		Controller.Switch(63) = 0
		HMag.MagnetOn = 1
		HMag.Strength = 20
		HMag.GrabCenter = true
	End If
End Sub



Sub CoffinMagDown(enabled)
	If DebugCoffin Then Debug.Print "Coffin down " & enabled & " at " & GameTime
	if Enabled Then
		Controller.Switch(64) = 1
		MagnetHole.IsDropped = 1
		RandomSoundLoopPostSolenoid 1, HMagnet
	Else
		Controller.Switch(64) = 0
		MagnetHole.IsDropped = 0
		RandomSoundLoopPostSolenoid 0, HMagnet
	End If
End Sub


Sub sw63_Hit
	If DebugCoffin Then debug.print "sw63_Hit"
	If (CoffinMagnetMode = 1 Or CoffinMagnetMode = 2) And sw63.BallCntOver > 0 Then
		If DebugCoffin and Controller.Switch(63) = 0 Then Debug.Print "Coffin Magnet Ball detected [" & GameTime & "]"
		Controller.Switch(63) = 1
	Else
		If DebugCoffin and Controller.Switch(63) = 1 Then Debug.Print "Coffin Magnet Ball disappeared [" & GameTime & "]"
		Controller.Switch(63) = 0
	End If
End Sub

Sub sw63_unHit
	If DebugCoffin Then debug.print "sw63_unHit"
	If (CoffinMagnetMode = 1 Or CoffinMagnetMode = 2) And sw63.BallCntOver > 0 Then
		If DebugCoffin and Controller.Switch(63) = 0 Then Debug.Print "Coffin Magnet Ball detected [" & GameTime & "]"
		Controller.Switch(63) = 1
	Else
		If DebugCoffin and Controller.Switch(63) = 1 Then Debug.Print "Coffin Magnet Ball disappeared [" & GameTime & "]"
		Controller.Switch(63) = 0
	End If
End Sub





'******************************************************
'	ZGRV: Grave Magnet
'******************************************************

sub SolLMag(Enabled)
	if (enabled) then
		'debug.print "magnet on"
		LMag.MagnetOn=True
		GraveRelease.enabled = false
		cLMagnetRelease = 0
	Else
		'debug.print "magnet off"
		GraveRelease.interval = 15 'RndInt(14,15)
		GraveRelease.enabled = true
	end if
End sub

dim cLMagnetRelease : cLMagnetRelease = 0

sub GraveRelease_timer
	select Case cLMagnetRelease
		case 12: LMag.MagnetOn=False
		Case 21: LMag.MagnetOn=True
		case 33: LMag.MagnetOn=False
		case 35: LMag.MagnetOn=False : GraveRelease.enabled = false : cLMagnetRelease = 0
	end select
	cLMagnetRelease = cLMagnetRelease + 1
end sub

Sub LMagnet_Hit
	LMag.AddBall ActiveBall
End Sub

Sub LMagnet_UnHit
	LMag.RemoveBall ActiveBall
End Sub


'******************************************************
'	ZECM: EL Chair Magnet
'******************************************************

dim ELChairMagCounter
sub SolRMag(Enabled)
'	debug.print Enabled
	if (enabled) then
'		debug.print "magnet on"
		ELChairMagCounter = 8
		RMag.MagnetOn=True
	Else
'		debug.print "magnet off"
		ELChairMagCounter = 0
		RMag.GrabCenter = False
		RMag.MagnetOn=False
	end if
End sub




'******************************************************
'	ZSNK: Snake
'******************************************************

Sub SnakeKick(enabled)
	If enabled Then
		sw54.timerenabled = true
	End If
End Sub

Sub SnakeJawLatch(enabled)
'	debug.print "--> SnakeJawLatch : " & enabled
	If Enabled Then
		snakejawf.rotatetoend
		JawLatch.isDropped = 0
		Controller.Switch(56) = 0
		Snake_OpenSound
	Else
		Snake_CloseSound
	End If
End Sub

Sub Solsnakejaw(enabled)
'	debug.print "Solsnakejaw : " & enabled
	If Enabled Then
		RandomSoundLoopPostSolenoid 1, BM_snakeM1
		snakejawf.rotatetoStart
		JawLatch.isDropped = 1
		Controller.Switch(56) = 1
	Else
		RandomSoundLoopPostSolenoid 0, BM_snakeM1
	End If
End Sub

Sub sw54_Hit
	Controller.Switch(54) = 1
	SoundSaucerLock
End Sub

Sub sw54_Timer()
	SoundSaucerKick 1,sw54
	sw54.Kick 193+rnd*4, 30+rnd*4
	controller.switch (54) = 0
	sw54.timerenabled = 0
End Sub

Sub SnakeJawF_animate
	Dim BP, a
	a = SnakeJawF.CurrentAngle
	For Each BP in BP_snakeM1: BP.ObjRotX = a: Next
	For Each BP in BP_snakeM2: BP.ObjRotX = a: Next
End Sub

Sub JawLatch_Hit:Controller.Switch(55) = 1:me.TimerEnabled = 1:End Sub
Sub JawLatch_Timer:Controller.Switch(55) = 0:me.timerenabled = 0:End Sub

dim GateFlapSoundLevel, GateCoilHoldSoundLevel
GateFlapSoundLevel = 0.5												'volume level; range [0, 1]
GateCoilHoldSoundLevel = 0.0025											'volume level; range [0, 1]
Const Cartridge_Gates					= "SY_TNA_REV02" 'Spooky Total Nuclear Annihilation Cartridge REV02

Sub Snake_OpenSound()
	PlaySoundAtLevelStatic (Cartridge_Gates & "_Gate_Left_Energized_" & Int(Rnd*2)+1), GateFlapSoundLevel, sw54
	Snake_CoilHoldSound(1)
End Sub

Sub Snake_CloseSound()
	PlaySoundAtLevelStatic (Cartridge_Gates & "_Gate_Left_Deenergized"), GateFlapSoundLevel, sw54
	Snake_CoilHoldSound(0)
End Sub

Sub Snake_CoilHoldSound(toggle)
	Select Case toggle
		Case 1
			PlaySoundAtLevelExistingStaticLoop (Cartridge_Gates & "_Gate_Left_Hold_Loop"), GateCoilHoldSoundLevel, sw54
		Case 0
			StopSound Cartridge_Gates & "_Gate_Left_Hold_Loop"
	End Select
End Sub


'******************************************************
'	ZHMR: Hammer
'******************************************************

Const hammerMax = 22
Const hammerMid = 11
Const hammerMin = 0

Sub HammerF_Animate
	Dim BP, hammerRot
	hammerRot = HammerF.CurrentAngle
	For Each BP in BP_Hammer
		BP.ObjRotX = hammerRot
		BP.Visible = hammerRot < hammerMid
	Next
	For Each BP in BP_HammerUp
		BP.ObjRotX = hammerRot
		BP.Visible = hammerRot >= hammerMid
	Next
	For Each BP in BP_Rod
		BP.Transz = -2*hammerRot
	Next
	For Each BP in BP_Rod_001
		BP.Transz = -2*hammerRot
	Next
End Sub


Sub Hammer(enabled)
	If Enabled Then
		SoundSaucerKick 0,BM_Rod
		HammerF.RotateToEnd
		MagnetHole.IsDropped = 1
	Else
		HammerF.RotateToStart
		RandomSoundLoopPostSolenoid 1, BM_Rod
		MagnetHole.IsDropped = 0
	End If
End Sub



'******************************************************
'	ZCRS: Cross
'******************************************************

Const CrossMax = 0
Const CrossMin = -60
Dim CrossPos: CrossPos = 0

Sub CrossMotor(enabled)
	'debug.print "CrossMotor "&Enabled
	If Enabled Then
		If CrossPos = 0 Then CrossTimerDown.Enabled = 1
		If CrossPos = -60 Then CrossTimerUp.Enabled = 1
		PlaySoundAtLevelStaticLoop "motor", VolumeDial*0.02, LMagnet
	Else
		StopSound "motor"
	End If
End Sub


Sub CrossTimerUp_Timer
	If CrossPos < CrossMax Then
		CrossPos = CrossPos+1
		Controller.Switch(33)=0
		Controller.Switch(34)=0
	End If
	If CrossPos >= CrossMax Then Controller.Switch(34) = 1:CrossTimerUp.Enabled = 0
	Dim BP
	For Each BP in BP_cross: BP.TransZ = CrossPos: Next
	For Each BP in BP_crossHolder: BP.TransZ = CrossPos: Next
End Sub

Sub CrossTimerDown_Timer
	If CrossPos > CrossMin Then
		CrossPos = CrossPos-1
		Controller.Switch(33)=0
		Controller.Switch(34)=0
	End If
	If CrossPos <= CrossMin Then Controller.Switch(33) = 1: CrossTimerDown.Enabled = 0
	Dim BP
	For Each BP in BP_cross: BP.TransZ = CrossPos: Next
	For Each BP in BP_crossHolder: BP.TransZ = CrossPos: Next
End Sub

'Sub F19_Animate  'Lighmap dimming as a function of cross position
'	Dim f: f = F19.GetInPlayIntensity / F19.Intensity
'	Dim op: op = 100*f*abs((CrossPos - CrossMin)/(CrossMax - CrossMin))
'	Dim BL: For Each BL In BL_FL_F19: BL.opacity = op: Next
'	'debug.print "F19_Animate f=" & f & " opacity=" & op & " R=" & abs((CrossPos - CrossMin)/(CrossMax - CrossMin))
'End Sub
'





'************************************************************
'	ZSWI: SWITCHES 
'************************************************************


Sub CapWall_Hit: RandomSoundMetal :End Sub

Sub sw1_Hit:   Controller.Switch(1) = 1:  End Sub
Sub sw1_UnHit: Controller.Switch(1) = 0:  End Sub
Sub sw3_Hit:   Controller.Switch(3) = 1:  End Sub
Sub sw3_UnHit: Controller.Switch(3) = 0:  End Sub
Sub sw23_Hit:  Controller.Switch(23) = 1: End Sub
Sub sw23_UnHit:Controller.Switch(23) = 0: End Sub
Sub sw24_Hit:  Controller.Switch(24) = 1: End Sub
Sub sw24_UnHit:Controller.Switch(24) = 0: End Sub
Sub sw25_Hit:  Controller.Switch(25) = 1: End Sub
Sub sw25_UnHit:Controller.Switch(25) = 0: End Sub
Sub sw28_Hit:  Controller.Switch(28) = 1: End Sub
Sub sw28_UnHit:Controller.Switch(28) = 0: End Sub
Sub sw29_Hit:  Controller.Switch(29) = 1: End Sub
Sub sw29_UnHit:Controller.Switch(29) = 0: End Sub
Sub sw43_Hit:  Controller.Switch(43) = 1: End Sub
Sub sw43_UnHit:Controller.Switch(43) = 0: End Sub
Sub sw44_Hit:  Controller.Switch(44) = 1: End Sub
Sub sw44_UnHit:Controller.Switch(44) = 0: End Sub
Sub sw45_Hit:  Controller.Switch(45) = 1: End Sub
Sub sw45_UnHit:Controller.Switch(45) = 0: End Sub
Sub sw46_Hit:  Controller.Switch(46) = 1: End Sub
Sub sw46_UnHit:Controller.Switch(46) = 0: End Sub
Sub sw47g_hit: vpmTimer.PulseSw 47: End Sub
Sub sw50g_hit: vpmTimer.PulseSw 50: End Sub
Sub sw52_Hit:  Controller.Switch(52) = 1: End Sub
Sub sw52_unHit:Controller.Switch(52) = 0: End Sub
Sub sw53_Hit:  vpmTimer.PulseSw 53: End Sub
Sub sw57_Hit:  Controller.Switch(57) = 1: End Sub
Sub sw57_unHit:Controller.Switch(57) = 0: End Sub
Sub sw58_Hit:  Controller.Switch(58) = 1: End Sub
Sub sw58_unHit:Controller.Switch(58) = 0: End Sub
Sub sw59_Hit:  Controller.Switch(59) = 1: End Sub
Sub sw59_unHit:Controller.Switch(59) = 0: End Sub

Sub Lspinner_Spin():vpmTimer.PulseSW 38:SoundSpinner LSpinner:End Sub
Sub Rspinner_Spin():vpmTimer.PulseSW 39:SoundSpinner RSpinner:End Sub

sub Trigger001_hit
	'reducing speed variation from plunger lane
	if activeball.velx > -18 and activeball.velx < -16 then
		activeball.velx = -17.5
	end if
end sub





'************************************************************
'	ZSLG: Slingshot Animations
'************************************************************

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(activeball)
	vpmTimer.PulseSw(26)
	RandomSoundSlingshotLeft sw1
	LStep = 0 : LeftSlingShot_Timer ' Initialize Step to 0
	LeftSlingShot.TimerInterval = 17
	LeftSlingShot.TimerEnabled = 1

	If VRroom = 1 Then
		If VRStrobes = 3 then LeftStrobesOnTimed 300
		If VRStrobes = 5 then LeftStrobeShort
	End If
End Sub

Sub LeftSlingShot_Timer
	Dim BL
	Dim x1, x2, y: x1 = True:x2 = False:y = 25
    Select Case LStep
        Case 3:x1 = False:x2 = True:y = 15
        Case 4:x1 = False:x2 = False:y = 0:LeftSlingShot.TimerEnabled = 0
    End Select
	
	For Each BL in BP_LSling1 : BL.Visible = x1: Next
	For Each BL in BP_LSling2 : BL.Visible = x2: Next
	For Each BL in BP_LEMK : BL.transx = y: Next

    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(activeball)
	vpmTimer.PulseSw(27)
	RandomSoundSlingshotRight sw28
	RStep = 0 : RightSlingShot_Timer ' Initialize Step to 0
	RightSlingShot.TimerInterval = 17
	RightSlingShot.TimerEnabled = 1

	If VRroom = 1  Then
		If VRStrobes = 3 then RightStrobesOnTimed 300
		If VRStrobes = 5 then RightStrobeShort
	End If
End Sub

Sub RightSlingShot_Timer
	Dim BL
	Dim x1, x2, y: x1 = True:x2 = False:y = 25
    Select Case RStep
        Case 2:x1 = False:x2 = True:y = 15
        Case 3:x1 = False:x2 = False:y = 0:RightSlingShot.TimerEnabled = 0
    End Select

	For Each BL in BP_RSling1 : BL.Visible = x1: Next
	For Each BL in BP_RSling2 : BL.Visible = x2: Next
	For Each BL in BP_REMK : BL.transx = y: Next

    RStep = RStep + 1
End Sub




'******************************************************
'	ZSSC: SLINGSHOT CORRECTION FUNCTIONS by apophis
'******************************************************
' To add these slingshot corrections:
'	 - On the table, add the endpoint primitives that define the two ends of the Slingshot
'	 - Initialize the SlingshotCorrection objects in InitSlingCorrection
'	 - Call the .VelocityCorrect methods from the respective _Slingshot event sub

Dim LS: Set LS = New SlingshotCorrection
Dim RS: Set RS = New SlingshotCorrection

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
'	ZBMP: Bumpers
'******************************************************


Sub Bumper1b_Hit
	vpmTimer.PulseSw 31
	RandomSoundBumperTop(Bumper1b)
End Sub


Sub Bumper2b_Hit
	vpmTimer.PulseSw 30
	RandomSoundBumperMiddle(Bumper2b)
End Sub

Sub Bumper3b_Hit
	vpmTimer.PulseSw 32
	RandomSoundBumperBottom(Bumper3b)
End Sub



'******************************************************
' 	ZLAN:   Light Animation Calls
'******************************************************


Sub l25_animate
	If MayoOpt = 1 Then Mayo_l25.intensityscale = 1 * (l25.GetInPlayIntensity / l25.Intensity)
End Sub

Sub L72_animate
	If VRroom > 0 then 
		VRStartButton.blenddisableLighting = L72.GetInPlayIntensity / L72.Intensity *2
		VRStartButton2.blenddisableLighting = VRStartButton.blenddisableLighting
	End If
End Sub

Sub L76_animate
	If VRroom > 0 then VRTourneyButton.blenddisableLighting = 10*L76.GetInPlayIntensity / L76.Intensity
End Sub



'******************************************************
' 	ZRGB:   RGB Inserts
'******************************************************

' RGB Inserts

Sub CN4R_animate: UpdateCN4RGB: End Sub
Sub CN4G_animate: UpdateCN4RGB: End Sub
Sub CN4B_animate: UpdateCN4RGB: End Sub
Sub UpdateCN4RGB
	dim R: R = CN4R.GetInPlayIntensity / CN4R.Intensity
	dim G: G = CN4G.GetInPlayIntensity / CN4G.Intensity
	dim B: B = CN4B.GetInPlayIntensity / CN4B.Intensity
	SetRGBLamp CN4, BL_L_CN4, R, G, B
'	p4CN.BlendDisableLighting = 50 * max(max(R,G),B)
End Sub

Sub CN5R_animate: UpdateCN5GB: End Sub
Sub CN5G_animate: UpdateCN5GB: End Sub
Sub CN5B_animate: UpdateCN5GB: End Sub
Sub UpdateCN5GB
	dim R: R = CN5R.GetInPlayIntensity / CN5R.Intensity
	dim G: G = CN5G.GetInPlayIntensity / CN5G.Intensity
	dim B: B = CN5B.GetInPlayIntensity / CN5B.Intensity
	SetRGBLamp CN5, BL_L_CN5, R, G, B
'	p5CN.BlendDisableLighting = 50 * max(max(R,G),B)
End Sub

Sub CN9R_animate: UpdateCN9GB: End Sub
Sub CN9G_animate: UpdateCN9GB: End Sub
Sub CN9B_animate: UpdateCN9GB: End Sub
Sub UpdateCN9GB
	dim R: R = CN9R.GetInPlayIntensity / CN9R.Intensity
	dim G: G = CN9G.GetInPlayIntensity / CN9G.Intensity
	dim B: B = CN9B.GetInPlayIntensity / CN9B.Intensity
	SetRGBLamp CN9, BL_L_CN9, R, G, B
'	p9CN.BlendDisableLighting = 50 * max(max(R,G),B)
End Sub

Sub CN11R_animate: UpdateCN11GB: End Sub
Sub CN11G_animate: UpdateCN11GB: End Sub
Sub CN11B_animate: UpdateCN11GB: End Sub
Sub UpdateCN11GB
	dim R: R = CN11R.GetInPlayIntensity / CN11R.Intensity
	dim G: G = CN11G.GetInPlayIntensity / CN11G.Intensity
	dim B: B = CN11B.GetInPlayIntensity / CN11B.Intensity
	SetRGBLamp CN11, BL_L_CN11, R, G, B
'	p11CN.BlendDisableLighting = 50 * max(max(R,G),B)
End Sub

Sub CN13R_animate: UpdateCN13GB: End Sub
Sub CN13G_animate: UpdateCN13GB: End Sub
Sub CN13B_animate: UpdateCN13GB: End Sub
Sub UpdateCN13GB
	dim R: R = CN13R.GetInPlayIntensity / CN13R.Intensity
	dim G: G = CN13G.GetInPlayIntensity / CN13G.Intensity
	dim B: B = CN13B.GetInPlayIntensity / CN13B.Intensity
	SetRGBLamp CN13, BL_L_CN13, R, G, B
'	p13CN.BlendDisableLighting = 50 * max(max(R,G),B)
End Sub


Sub CN19R_animate: UpdateCN19GB: End Sub
Sub CN19G_animate: UpdateCN19GB: End Sub
Sub CN19B_animate: UpdateCN19GB: End Sub
Sub UpdateCN19GB
	dim R: R = CN19R.GetInPlayIntensity / CN19R.Intensity
	dim G: G = CN19G.GetInPlayIntensity / CN19G.Intensity
	dim B: B = CN19B.GetInPlayIntensity / CN19B.Intensity
	SetRGBLamp CN19, BL_L_CN19, R, G, B
'	p19CN.BlendDisableLighting = 50 * max(max(R,G),B)
End Sub


Sub SetRGBLamp(Lamp, BL, R, G, B)
	Dim LM
	For each LM in BL: LM.color = RGB(CInt(R*255), CInt(G*255), CInt(B*255)): Next
	Lamp.Color = RGB(CInt(R*255), CInt(G*255), CInt(B*255))
	Lamp.ColorFull = RGB(CInt(R*255), CInt(G*255), CInt(B*255))
	Lamp.State = max(max(R,G),B)
End Sub



' Configure other insert colors and brightnesses

Sub InitInsertRGB
	Dim LM

	' Fuel gage insert colors and brightness
	For each LM in BL_L_L41: LM.color = RGB(255, 2, 2): Next
	For each LM in BL_L_L40: LM.color = RGB(255, 128, 10): Next
	For each LM in BL_L_L39: LM.color = RGB(255, 128, 10): Next
	For each LM in BL_L_L38: LM.color = RGB(255, 255, 240): Next
	For each LM in BL_L_L37: LM.color = RGB(10, 255, 10): Next

	' Coffin flasher brightness
	For each LM in BL_FL_F31: LM.opacity = 170: Next

'	' RGB insert brightness
'	For each LM in BL_L_CN4: LM.opacity = 200: Next
'	For each LM in BL_L_CN5: LM.opacity = 200: Next
'	For each LM in BL_L_CN9: LM.opacity = 200: Next
'	For each LM in BL_L_CN11: LM.opacity = 200: Next
'	For each LM in BL_L_CN13: LM.opacity = 200: Next
'	For each LM in BL_L_CN19: LM.opacity = 200: Next

End Sub


'******************************************************
' 	ZGIU: GI updates
'******************************************************

' All the GI lights are in the AllLamps collection have their timer intevals set to the appropriate indexes.
' GI is driven by Lamp indexes 130, 132, 134, and 136
'   130  Red GI
'   132  Blue GI
'   134  White GI, Upper
'   136  White GI, PF level



dim RGIlvl, BGIlvl, WGIlvl, BottomLVL
BottomLVL = 80
	
' RED GI
Sub GI130_Animate
	Dim bulb, p
	p = (GI130.GetInPlayIntensity / GI130.Intensity)
	For Each bulb In GIR: bulb.state = p: Next
	'debug.print "  RED GI = "&p
	RGIlvl = p
	PlasticBottoms
End Sub


' BLUE GI
Sub GI132_Animate
	Dim bulb, p
	p = (GI132.GetInPlayIntensity / GI132.Intensity)
	For Each bulb In GIB: bulb.state = p: Next
	'debug.print "  BLUE GI = "&p
	BGIlvl = p
	PlasticBottoms
End Sub

' WHITE UPPER GI
Sub GI134_Animate
	Dim bulb, p
	p = (GI134.GetInPlayIntensity / GI134.Intensity)
	For Each bulb In GIWU: bulb.state = p: Next
	'debug.print "  WHITE UPPER GI = "&p
End Sub


' WHITE GI
Sub GI136_Animate
	Dim bulb, p
	p = (GI136.GetInPlayIntensity / GI136.Intensity)
	For Each bulb In GIW: bulb.state = p: Next
	'debug.print "  WHITE GI = "&p
	WGIlvl = p
	PlasticBottoms
End Sub


'Updating the color of plastic bottom primitives when GI values are updated - iaakki
sub PlasticBottoms()

	dim whiteAVG

	whiteAVG = WGIlvl-( (RGIlvl + BGIlvl) / 4)	'Reducing the level of white on certain cases
	if (whiteAVG < 0) then whiteAVG = 0
	
	' Updating the colors
	PlasticBottoms1.color = RGB((whiteAVG+RGIlvl) * BottomLVL, whiteAVG * BottomLVL, (whiteAVG+BGIlvl) * BottomLVL)
	PlasticBottoms2.color = RGB((whiteAVG+RGIlvl) * BottomLVL, whiteAVG * BottomLVL, (whiteAVG+BGIlvl) * BottomLVL)
end sub

'******************************************************
'	ZNFF:  FLIPPER CORRECTIONS by nFozzy
'******************************************************


Dim LF : Set LF = New FlipperPolarity
Dim RF : Set RF = New FlipperPolarity

InitPolarity


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
		x.AddPt "Polarity", 1, 0.05, - 5.5
		x.AddPt "Polarity", 2, 0.16, - 5.5
		x.AddPt "Polarity", 3, 0.20, - 0.75
		x.AddPt "Polarity", 4, 0.25, - 1.25
		x.AddPt "Polarity", 5, 0.3, - 1.75
		x.AddPt "Polarity", 6, 0.4, - 3.5
		x.AddPt "Polarity", 7, 0.5, - 5.25
		x.AddPt "Polarity", 8, 0.7, - 4.0
		x.AddPt "Polarity", 9, 0.75, - 3.5
		x.AddPt "Polarity", 10, 0.8, - 3.0
		x.AddPt "Polarity", 11, 0.85, - 2.5
		x.AddPt "Polarity", 12, 0.9, - 2.0
		x.AddPt "Polarity", 13, 0.95, - 1.5
		x.AddPt "Polarity", 14, 1, - 1.0
		x.AddPt "Polarity", 15, 1.05, -0.5
		x.AddPt "Polarity", 16, 1.1, 0
		x.AddPt "Polarity", 17, 1.3, 0

		x.AddPt "Velocity", 0, 0, 0.85
		x.AddPt "Velocity", 1, 0.23, 0.85
		x.AddPt "Velocity", 2, 0.35, 0.88
		x.AddPt "Velocity", 3, 0.45, 1
		x.AddPt "Velocity", 4, 0.6, 1 '0.982
		x.AddPt "Velocity", 5, 0.62, 1.0
		x.AddPt "Velocity", 6, 0.702, 0.968
		x.AddPt "Velocity", 7, 0.95,  0.968
		x.AddPt "Velocity", 8, 1.03,  0.945
		x.AddPt "Velocity", 9, 1.5,  0.945

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
	' VPW Optimization: cache COM reads + replace ^2 with multiplication.
	Dim vx, vy, vz : vx = ball.VelX : vy = ball.VelY : vz = ball.VelZ
	BallSpeed = Sqr(vx*vx + vy*vy + vz*vz)
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
'	 - Include FlipperActivate and FlipperDeactivate in the Flipper solenoid subs

RightFlipper.timerinterval = 10  ' VPW Optimization: 1ms -> 10ms (visually identical at 60fps; eliminates ~4500 calls/sec)
Rightflipper.timerenabled = True

Sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	' VPW Optimization: skip FlipperNudge entirely when both flippers are at rest.
	If LFPress = 0 And RFPress = 0 Then Exit Sub
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
End Sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	' VPW Optimization: cache currentangle COM reads.
	Dim b, ubBot, ca1, ca2, bx, by, bot_b
	ca1 = Flipper1.currentangle

	If ca1 = Endangle1 And EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		ca2 = Flipper2.currentangle
		If ca2 = EndAngle2 Then
			ubBot = UBound(gBOT)
			For b = 0 To ubBot
				Set bot_b = gBOT(b)
				bx = bot_b.x : by = bot_b.y
				If FlipperTrigger(bx, by, Flipper1) Then Exit Sub
			Next
			For b = 0 To ubBot
				Set bot_b = gBOT(b)
				bx = bot_b.x : by = bot_b.y
				If FlipperTrigger(bx, by, Flipper2) Then
					bot_b.velx = bot_b.velx / 1.3
					bot_b.vely = bot_b.vely - 0.5
				End If
			Next
		End If
	Else
		If Abs(ca1) > Abs(EndAngle1) + 30 Then EOSNudge1 = 0
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
	' VPW Optimization: ^2 -> multiplication.
	Dim dx, dy : dx = ax - bx : dy = ay - by
	Distance = Sqr(dx*dx + dy*dy)
End Function

Function DistancePL(px,py,ax,ay,bx,by) 'Distance between a point and a line where point Is px,py
	DistancePL = Abs((by - ay) * px - (bx - ax) * py + bx * ay - by * ax) / Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PIover180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax)) * d180overPI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle + 90)) + Flipper.x, Sin(Radians(Flipper.currentangle + 90)) + Flipper.y)
End Function

Function DistanceFromFlipperAngle(ballx, bally, Flipper, Angle)
	DistanceFromFlipperAngle = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Angle + 90)) + Flipper.x, Sin(Radians(angle + 90)) + Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	' VPW Optimization: cache flipper COM reads (x/y/currentangle/Length) and inline DistanceFromFlipper.
	Dim DiffAngle, fx, fy, fca, flen, frad, dx, dy, cx, cy
	fx = Flipper.x : fy = Flipper.y
	fca = Flipper.currentangle
	flen = Flipper.Length
	DiffAngle = Abs(fca - Atn2((bally - fy),(ballx - fx)) * d180overPI - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360

	frad = (fca + 90) * PIover180
	cx = Cos(frad) + fx : cy = Sin(frad) + fy
	dx = ballx - fx : dy = bally - fy
	If DistancePL(ballx, bally, fx, fy, cx, cy) < 48 And DiffAngle <= 90 And Sqr(dx*dx + dy*dy) < flen Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If
End Function

'*************************************************
'  End - Check ball distance from Flipper for Rem
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
		'Dim gBOT: gBOT = GetBalls
		
		For b = 0 To UBound(gBOT)
			If Distance(gBOT(b).x, gBOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If gBOT(b).vely >= - 0.4 Then gBOT(b).vely =  - 0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState)
	' VPW Optimization: cache startangle/currentangle COM reads. Flipper.endangle is NOT cached
	' because state 1 mutates it; subsequent ElseIfs need the live value.
	Dim Dir, sa, absSa, ca
	sa = Flipper.startangle
	Dir = sa / Abs(sa)
	absSa = Abs(sa)
	ca = Abs(Flipper.currentangle)

	If ca > absSa - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup
			Flipper.endangle = FEndAngle - 3 * Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0
			FState = 1
		End If
	ElseIf ca <= Abs(Flipper.endangle) And FlipperPress = 1 Then
		If FCount = 0 Then FCount = GameTime

		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	ElseIf ca > Abs(Flipper.endangle) + 0.01 And FlipperPress = 1 Then
		If FState <> 3 Then
			Flipper.eostorque = EOST
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If
	End If
End Sub

Const LiveDistanceMin = 5  'minimum distance In vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114 'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)
Const BaseDampen = 0.55

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
    Dim Dir, LiveDist
    Dir = Flipper.startangle / Abs(Flipper.startangle)    '-1 for Right Flipper
    Dim LiveCatchBounce   'If live catch is not perfect, it won't freeze ball totally
    Dim CatchTime
    CatchTime = GameTime - FCount
    LiveDist = Abs(Flipper.x - ball.x)

    If CatchTime <= LiveCatch And parm > 3 And LiveDist > LiveDistanceMin And LiveDist < LiveDistanceMax Then
        If CatchTime <= LiveCatch * 0.5 Then   'Perfect catch only when catch time happens in the beginning of the window
            LiveCatchBounce = 0
        Else
            LiveCatchBounce = Abs((LiveCatch / 2) - CatchTime)  'Partial catch when catch happens a bit late
        End If
        
        If LiveCatchBounce = 0 And ball.velx * Dir > 0 And LiveDist > 30 Then ball.velx = 0

        If ball.velx * Dir > 0 And LiveDist < 30 Then
            ball.velx = BaseDampen * ball.velx
            ball.vely = BaseDampen * ball.vely
            ball.angmomx = BaseDampen * ball.angmomx
            ball.angmomy = BaseDampen * ball.angmomy
            ball.angmomz = BaseDampen * ball.angmomz
        Elseif LiveDist > 30 Then
            ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
            ball.angmomx = 0
            ball.angmomy = 0
            ball.angmomz = 0
        End If
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf ActiveBall, parm
    End If
End Sub





'******************************************************
' 	ZDMP:  RUBBER  DAMPENERS
'******************************************************
' These are data mined bounce curves,
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR

' Put all the Post and Pin objects in dPosts collection. Make sure dPosts fires hit events.
Sub dPosts_Hit(idx)
	RubbersD.dampen ActiveBall
	TargetBouncer ActiveBall, 1.1
End Sub

' This collection contains the bottom sling posts. They are not in the dPosts collection so that the TargetBouncer is not applied to them, but they should still have dampening applied
' If you experience airballs with posts or targets, consider adding them to this collection
Sub NoTargetBouncer_Hit
    RubbersD.dampen ActiveBall
End Sub

' Put all the Sleeve objects in dSleeves collection. Make sure dSleeves fires hit events.
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
RubbersD.addpoint 1, 3.77, 0.99
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
		aBall.velz = aBall.velz * coef
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
		' VPW Optimization: pre-size to tnob; gBOT is static at tnob entries.
		ReDim ballvel(tnob)
		ReDim ballvelx(tnob)
		ReDim ballvely(tnob)
	End Sub
	
	Public Sub Update()	'tracks in-ball-velocity
		' VPW Optimization: replaced GetBalls + dynamic ReDim with single pass over static gBOT.
		Dim b, ubBot, bot_b, bid
		ubBot = UBound(gBOT)
		For b = 0 To ubBot
			Set bot_b = gBOT(b)
			bid = bot_b.id
			If bid <= tnob Then
				ballvel(bid) = BallSpeed(bot_b)
				ballvelx(bid) = bot_b.velx
				ballvely(bid) = bot_b.vely
			End If
		Next
	End Sub
End Class





'******************************************************
' 	ZBOU: VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.9 	'Level of bounces. Recommmended value of 0.7-1.0

sub TargetBouncer(aBall,defvalue)
    dim zMultiplier, vel, vratio
    if TargetBouncerEnabled = 1 and aball.z < 30 then
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
	end if
end sub




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
' Tutorial videos by Apophis
' Audio : Adding Fleep Part 1					https://youtu.be/rG35JVHxtx4?si=zdN9W4cZWEyXbOz_
' Audio : Adding Fleep Part 2					https://youtu.be/dk110pWMxGo?si=2iGMImXXZ0SFKVCh
' Audio : Adding Fleep Part 3					https://youtu.be/ESXWGJZY_EI?si=6D20E2nUM-xAw7xy


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
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStaticLoop(playsoundparams, aVol, tableobj)
    PlaySound playsoundparams, -1, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 1, 1, AudioFade(tableobj)
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

Function AudioFade(tableobj) ' VPW Optimization: pre-computed InvTHHalf + ^10 -> multiplication chain.
	Dim tmp, t2, t4, t8, nt
	tmp = tableobj.y * InvTHHalf - 1
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp < -7000 Then
		tmp = -7000
	End If
	If tmp >= 0 Then
		t2 = tmp*tmp : t4 = t2*t2 : t8 = t4*t4
		AudioFade = CSng(t8 * t2)
	Else
		nt = -tmp
		t2 = nt*nt : t4 = t2*t2 : t8 = t4*t4
		AudioFade = CSng(-(t8 * t2))
	End If
End Function

Function AudioPan(tableobj) ' VPW Optimization: pre-computed InvTWHalf + ^10 -> multiplication chain.
	Dim tmp, t2, t4, t8, nt
	tmp = tableobj.x * InvTWHalf - 1
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp < -7000 Then
		tmp = -7000
	End If
	If tmp >= 0 Then
		t2 = tmp*tmp : t4 = t2*t2 : t8 = t4*t4
		AudioPan = CSng(t8 * t2)
	Else
		nt = -tmp
		t2 = nt*nt : t4 = t2*t2 : t8 = t4*t4
		AudioPan = CSng(-(t8 * t2))
	End If
End Function

Function Vol(ball) ' VPW Optimization: cache BallVel + replace ^2.
	Dim bv : bv = BallVel(ball)
	Vol = CSng(bv * bv)
End Function

Function Volz(ball) ' VPW Optimization: cache + replace ^2.
	Dim vz : vz = ball.velz
	Volz = CSng(vz * vz)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) ' VPW Optimization: cache COM + replace ^2.
	Dim vx, vy : vx = ball.VelX : vy = ball.VelY
	BallVel = Int(Sqr(vx*vx + vy*vy))
End Function

Function VolPlayfieldRoll(ball) ' VPW Optimization: cache + replace ^3.
	Dim bv : bv = BallVel(ball)
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * CSng(bv*bv*bv)
End Function

Function PitchPlayfieldRoll(ball) ' VPW Optimization: cache + replace ^2.
	Dim bv : bv = BallVel(ball)
	PitchPlayfieldRoll = bv * bv * 15
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







'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////

Sub OnBallBallCollision(ball1, ball2, velocity)
	FlipperCradleCollision ball1, ball2, velocity
	SoundBallBall ball1, velocity
End Sub

Sub SoundBallBall(ball, velocity)
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
	
	PlaySound (snd), 0, CSng(velocity) ^ 2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball), 0, Pitch(ball), 0, 0, AudioFade(ball)
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
'	ZBRL: BALL ROLLING AND DROP SOUNDS
'******************************************************
'
' Be sure to call RollingUpdate in a timer with a 10ms interval see the GameTimer_Timer() sub

ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Sub InitRolling
	Dim i
	For i = 0 to tnob
		rolling(i) = False
	Next
End Sub

Sub RollingUpdate()
	' VPW Optimization: pre-built BallRollStr() arrays, cache COM properties (ball.x/y/z/Velx/y/z),
	' inline BallVel (eliminates 2 helper calls per ball), use cached BallRollVol.
	Dim b, ubBot, ball, bvx, bvy, bvz, bz, bv, bv2, bv3, dc
	ubBot = UBound(gBOT)

	' stop the sound of deleted balls
	For b = ubBot + 1 To tnob - 1
		rolling(b) = False
		StopSound(BallRollStr(b))
	Next

	If ubBot = -1 Then Exit Sub

	For b = 0 To ubBot
		Set ball = gBOT(b)
		bvx = ball.VelX : bvy = ball.VelY : bvz = ball.VelZ : bz = ball.Z
		bv  = Int(Sqr(bvx*bvx + bvy*bvy))
		If bv > 1 And bz < 30 Then
			rolling(b) = True
			bv2 = bv * bv
			bv3 = bv2 * bv
			PlaySound BallRollStr(b), -1, RollingSoundFactor * 0.0005 * CSng(bv3) * BallRollVol, AudioPan(ball), 0, bv2 * 15, 1, 0, AudioFade(ball)
		Else
			If rolling(b) = True Then
				StopSound(BallRollStr(b))
				rolling(b) = False
			End If
		End If

		' Ball Drop Sounds
		dc = DropCount(b)
		If bvz < -1 And bz < 55 And bz > 27 Then
			If dc >= 5 Then
				DropCount(b) = 0
				If bvz > -7 Then
					RandomSoundBallBouncePlayfieldSoft ball
				Else
					RandomSoundBallBouncePlayfieldHard ball
				End If
			End If
		End If
		If dc < 5 Then DropCount(b) = dc + 1
	Next
End Sub





'******************************************************
' 	ZRRL: RAMP ROLLING SFX
'******************************************************

'Ball tracking ramp SFX 1.0
'   Reqirements:
'          * Import A Sound File for each ball on the table for plastic ramps.  Call It RampLoop<Ball_Number> ex: RampLoop1, RampLoop2, ...
'          * Import a Sound File for each ball on the table for wire ramps. Call it WireLoop<Ball_Number> ex: WireLoop1, WireLoop2, ...
'          * Create a Timer called RampRoll, that is enabled, with a interval of 100
'          * Set RampBAlls and RampType variable to Total Number of Balls
'	Usage:
'          * Setup hit events and call WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'          * To stop tracking ball
'                 * call WireRampOff
'                 * Otherwise, the ball will auto remove if it's below 30 vp units
'

dim RampMinLoops : RampMinLoops = 4

' RampBalls
'      Setup:        Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RammBalls(6,2)
'      Description:  
dim RampBalls(5,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(5)	

Sub WireRampOn(input)  : Waddball ActiveBall, input : RampRollUpdate: End Sub
Sub WireRampOff() : WRemoveBall ActiveBall.ID	: End Sub


' WaddBall (Active Ball, Boolean)
'     Description: This subroutine is called from WireRampOn to Add Balls to the RampBalls Array
Sub Waddball(input, RampInput)	'Add ball
	' This will loop through the RampBalls array checking each element of the array x, position 1
	' To see if the the ball was already added to the array.
	' If the ball is found then exit the subroutine
	dim x : for x = 1 to uBound(RampBalls)	'Check, don't add balls twice
		if RampBalls(x, 1) = input.id then 
			if Not IsEmpty(RampBalls(x,1) ) then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next

	' This will itterate through the RampBalls Array.
	' The first time it comes to a element in the array where the Ball Id (Slot 1) is empty.  It will add the current ball to the array
	' The RampBalls assigns the ActiveBall to element x,0 and ball id of ActiveBall to 0,1
	' The RampType(BallId) is set to RampInput
	' RampBalls in 0,0 is set to True, this will enable the timer and the timer is also turned on
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

' WRemoveBall (BallId)
'    Description: This subroutine is called from the RampRollUpdate subroutine 
'                 and is used to remove and stop the ball rolling sounds
Sub WRemoveBall(ID)		'Remove ball
	'Debug.Print "In WRemoveBall() + Remove ball from loop array"
	dim ballcount : ballcount = 0
	dim x : for x = 1 to Ubound(RampBalls)
		if ID = RampBalls(x, 1) then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound(RampLoopStr(x))
			StopSound(WireLoopStr(x))
		end If
		if not IsEmpty(Rampballs(x,1)) then ballcount = ballcount + 1
	next
	if BallCount = 0 then RampBalls(0,0) = False	'if no balls in queue, disable timer update
End Sub

Sub RampRoll_Timer():RampRollUpdate:End Sub

Sub RampRollUpdate()		'Timer update
	' VPW Optimization: pre-built RampLoopStr/WireLoopStr arrays + cache rb + inline BallVel.
	Dim x, rb, bvx, bvy, bv, bv2, bv3, sName, panV, fadeV, pitchV
	For x = 1 To uBound(RampBalls)
		If Not IsEmpty(RampBalls(x,1)) Then
			Set rb = RampBalls(x,0)
			bvx = rb.VelX : bvy = rb.VelY
			bv = Int(Sqr(bvx*bvx + bvy*bvy))
			If bv > 1 Then
				bv2 = bv * bv : bv3 = bv2 * bv
				panV = AudioPan(rb) : fadeV = AudioFade(rb)
				If RampType(x) Then
					pitchV = pSlope(bv, 1, -4000, 60, 7000)
					PlaySound RampLoopStr(x), -1, RollingSoundFactor * 0.0005 * CSng(bv3) * RampRollVol, panV, 0, pitchV, 1, 0, fadeV
					StopSound(WireLoopStr(x))
				Else
					pitchV = pSlope(bv, 1, -1000, 60, 10000)
					StopSound(RampLoopStr(x))
					PlaySound WireLoopStr(x), -1, RollingSoundFactor * 0.0005 * CSng(bv3) * RampRollVol, panV, 0, pitchV, 1, 0, fadeV
				End If
				RampBalls(x, 2) = RampBalls(x, 2) + 1
			Else
				StopSound(RampLoopStr(x))
				StopSound(WireLoopStr(x))
			End If
			If rb.Z < 30 And RampBalls(x, 2) > RampMinLoops Then
				StopSound(RampLoopStr(x))
				StopSound(WireLoopStr(x))
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound(RampLoopStr(x))
			StopSound(WireLoopStr(x))
		End If
	Next
	If Not RampBalls(0,0) Then RampRoll.enabled = 0
End Sub

' This can be used to debug the Ramp Roll time.  You need to enable the tbWR timer on the TextBox
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


Function BallPitch(ball) ' Calculates the pitch of the sound based on the ball speed
    BallPitch = pSlope(BallVel(ball), 1, -1000, 60, 10000)
End Function

Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
	BallPitchV = pSlope(BallVel(ball), 1, -4000, 60, 7000)
End Function


' Ramp SFX triggers

Sub RRamp_Hit
	If activeball.vely < 1 Then
		WireRampOn False	 'Play Metal Ramp Sound
		'		PlaySoundAtLevelActiveBall "RightRampMetal", Vol(ActiveBall) * MetalImpactSoundFactor
	Else
		WireRampOff
	End If
'	debug.print ActiveBall.VelY
	If (ActiveBall.VelY > 0) Then 
		'ball is traveling down the playfield
'		debug.print "down"
		RandomSoundRampFlapDown()
	ElseIf (ActiveBall.VelY < 0) Then
'		debug.print "up"
		RandomSoundRampFlapUp()

	End If

End Sub

Sub RRampEnd_Hit
	WireRampOff
End Sub

Sub LRamp_Hit
	If activeball.vely < 1 Then
		WireRampOn False	 'Play Metal Ramp Sound
		'		PlaySoundAtLevelActiveBall "LeftRampMetal", Vol(ActiveBall) * MetalImpactSoundFactor
	Else
		WireRampOff
	End If

	If (ActiveBall.VelY > 0) Then 
		'ball is traveling down the playfield
'		debug.print "down"
		RandomSoundRampFlapDown()
	ElseIf (ActiveBall.VelY < 0) Then
		'ball is traveling up the playfield
'		debug.print "up"
		RandomSoundRampFlapUp()
	End If

End Sub

Sub LRampEnd_Hit
	WireRampOff
End Sub


dim FlapSoundLevel
FlapSoundLevel = 0.8													'volume level; range [0, 1]

'/////////////////////////////  RAMPS FLAPS - SOUNDS  ////////////////////////////
Sub RandomSoundRampFlapUp()
	
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelActiveBall ("TOM_Ramp_Flap_Up_1"), FlapSoundLevel
		Case 2 : PlaySoundAtLevelActiveBall ("TOM_Ramp_Flap_Up_2"), FlapSoundLevel
		Case 3 : PlaySoundAtLevelActiveBall ("TOM_Ramp_Flap_Up_3"), FlapSoundLevel
	End Select
End Sub

Sub RandomSoundRampFlapDown()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelActiveBall ("TOM_Ramp_Flap_Down_1"), FlapSoundLevel
		Case 2 : PlaySoundAtLevelActiveBall ("TOM_Ramp_Flap_Down_2"), FlapSoundLevel
		Case 3 : PlaySoundAtLevelActiveBall ("TOM_Ramp_Flap_Down_3"), FlapSoundLevel
	End Select
End Sub


'***************************************************************
'	ZSHA: Ambient ball shadows
'***************************************************************

' For dynamic ball shadows, Check the "Raytraced ball shadows" box for the specific light. 
' Also make sure the light's z position is around 25 (mid ball)

'Ambient (Room light source)
Const AmbientBSFactor = 0.9    '0 To 1, higher is darker
Const AmbientMovement = 1	   '1+ higher means more movement as the ball moves left and right
Const offsetX = 0			   'Offset x position under ball (These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY = 0			   'Offset y position under ball (^^for example 5,5 if the light is in the back left corner)

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!  (will throw errors if there aren't enough objects)
Dim objBallShadow(7)

'Initialization
BSInit

Sub BSInit()
	Dim iii
	'Prepare the shadow objects before play begins
	For iii = 0 To tnob - 1
		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = 3 + iii / 1000
		objBallShadow(iii).visible = 0
	Next
End Sub


Sub BSUpdate
	' VPW Optimization: cache gBOT(s).X/Y/Z; .visible write guard.
	Dim s, ubBot, bot_s, bx, by, bz, newVis
	ubBot = UBound(gBOT)
	For s = lob To ubBot
		Set bot_s = gBOT(s)
		bx = bot_s.X : by = bot_s.Y : bz = bot_s.Z
		If bz > 20 And bz < 30 Then
			newVis = 1
			If LastBSVisible(s) <> 1 Then
				objBallShadow(s).visible = 1
				LastBSVisible(s) = 1
			End If
			objBallShadow(s).X = bx + (bx - TW_d2) / (Ballsize / AmbientMovement) + offsetX
			objBallShadow(s).Y = by + offsetY
		Else
			If LastBSVisible(s) <> 0 Then
				objBallShadow(s).visible = 0
				LastBSVisible(s) = 0
			End If
		End If
	Next
End Sub





'******************************************************
' 	ZRDT:  DROP TARGETS by Rothbauerw
'******************************************************

' This table has raised DT switches = 1 and dropped = 0
controller.switch(60) = 1
controller.switch(61) = 1
controller.switch(62) = 1

Sub sw60_hit: DTHit 60: End Sub
Sub sw61_hit: DTHit 61: End Sub
Sub sw62_hit: DTHit 62: End Sub

Sub ResetDTs(Enabled)
	If Enabled Then
		DTRaise 60
		DTRaise 61
		DTRaise 62
		PlaySoundAt SoundFX(DTResetSound,DOFContactors),LMagnet
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
Dim DT60, DT61, DT62

'Set array with drop target objects
'
'DropTargetvar = Array(primary, secondary, prim, swtich, animate)
' 	primary: 			primary target wall to determine drop
'	secondary:			wall used to simulate the ball striking a bent or offset target after the initial Hit
'	prim:				primitive target used for visuals and animation
'							IMPORTANT!!! 
'							rotz must be used for orientation
'							rotx to bend the target back
'							transz to move it up and down
'							the pivot point should be in the center of the target on the x, y and at or below the playfield (0) on z
'	switch:				ROM switch number
'	animate:			Array slot for handling the animation instrucitons, set to 0
'						Values for animate: 1 - bend target (hit to primary), 2 - drop target (hit to secondary), 3 - brick target (high velocity hit to secondary), -1 - raise target 
'   isDropped:			Boolean which determines whether a drop target is dropped. Set to false if they are initially raised, true if initially dropped.

Set DT60 = (new DropTarget)(sw60, sw60a, BM_sw60, 60, 0, false)  
Set DT61 = (new DropTarget)(sw61, sw61a, BM_sw61, 61, 0, false)
Set DT62 = (new DropTarget)(sw62, sw62a, BM_sw62, 62, 0, false)


Dim DTArray
DTArray = Array(DT60, DT61, DT62)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 80 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 55 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 10 'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 'max degrees primitive rotates when hit
Const DTDropDelay = 20 'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40 'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 30 'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 0 'Set to 0 to disable bricking, 1 to enable bricking
Const DTHitSound = "" 'Drop Target Hit sound
Const DTDropSound = "DropTarget_Down" 'Drop Target Drop sound
Const DTResetSound = "DropTarget_Up" 'Drop Target reset sound

Const DTMass = 0.2 'Mass of the Drop Target (between 0 and 1), higher values provide more resistance


'******************************************************
'  DROP TARGETS FUNCTIONS
'******************************************************

Sub DTHit(switch)
	Dim i, prev
	i = DTArrayID(switch)

	PlayTargetSound
	prev = DTArray(i).animate
	DTArray(i).animate = DTCheckBrick(ActiveBall,DTArray(i).prim)
	If prev = 0 And DTArray(i).animate <> 0 Then DTActiveCount = DTActiveCount + 1
	If DTArray(i).animate = 1 Or DTArray(i).animate = 3 Or DTArray(i).animate = 4 Then
		DTBallPhysics ActiveBall, DTArray(i).prim.rotz, DTMass
	End If
	DTVisActive = 1
	DoDTAnim
End Sub

Sub DTRaise(switch)
	Dim i
	i = DTArrayID(switch)
	If DTArray(i).animate = 0 Then DTActiveCount = DTActiveCount + 1
	DTArray(i).animate =  - 1
	DTVisActive = 1
	DoDTAnim
End Sub

Sub DTDrop(switch)
	Dim i
	i = DTArrayID(switch)
	If DTArray(i).animate = 0 Then DTActiveCount = DTActiveCount + 1
	DTArray(i).animate = 1
	DTVisActive = 1
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
	' VPW Optimization: skip when no targets are animating.
	If DTActiveCount <= 0 Then
		DTActiveCount = 0
		Exit Sub
	End If
	Dim i, prev
	For i = 0 To UBound(DTArray)
		prev = DTArray(i).animate
		If prev <> 0 Then
			DTArray(i).animate = DTAnimate(DTArray(i).primary,DTArray(i).secondary,DTArray(i).prim,DTArray(i).sw,prev)
			If DTArray(i).animate = 0 Then DTActiveCount = DTActiveCount - 1
		End If
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
			controller.Switch(Switchid mod 100) = 0  'This table has dropped DT swtich = 0
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
			'Dim gBOT: gBOT = GetBalls
			
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
		controller.Switch(Switchid mod 100) = 1 'This table has raised DT swtich = 1
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


Sub UpdateDropTargets
	' VPW Optimization: only iterate BP collections while a drop target is animating.
	If DTVisActive = 0 Then Exit Sub
	dim BP, tz, rx, ry

    tz = BM_sw60.transz
	rx = BM_sw60.rotx
	ry = BM_sw60.roty
	For each BP in BP_sw60 : BP.transz = tz: BP.rotx = rx: BP.roty = ry: Next

    tz = BM_sw61.transz
	rx = BM_sw61.rotx
	ry = BM_sw61.roty
	For each BP in BP_sw61 : BP.transz = tz: BP.rotx = rx: BP.roty = ry: Next

    tz = BM_sw62.transz
	rx = BM_sw62.rotx
	ry = BM_sw62.roty
	For each BP in BP_sw62 : BP.transz = tz: BP.rotx = rx: BP.roty = ry: Next

	' Once all targets are at rest again, run one more pass next frame then stop.
	If DTActiveCount <= 0 Then DTVisActive = 0
End Sub



'******************************************************
'	ZRST: STAND-UP TARGETS 
'******************************************************


Sub sw4_Hit
	STHit 4
	if VRroom = 1 and VRStrobes = 5 then LeftStrobeShort
End Sub
Sub sw7_Hit: STHit 7: End Sub
Sub sw9_Hit: STHit 9: End Sub
Sub sw35_Hit: STHit 35: End Sub
Sub sw36_Hit: STHit 36: End Sub
Sub sw37_Hit: STHit 37: End Sub
Sub sw40_Hit: STHit 40: End Sub
Sub sw41_Hit: STHit 41: End Sub

Sub sw42_Hit
	STHit 42
	Dim BP
	For Each BP in BP_HammerUp: BP.ObjRotX = hammerMax-1: Next
	For Each BP in BP_Cube: BP.Transy = -2: BP.Transx = 1: Next
	sw42.HasHitEvent = 0
	sw42.timerenabled = 1
End Sub

sub sw42_timer
	Dim BP
	For Each BP in BP_HammerUp: BP.ObjRotX = hammerMax: Next
	For Each BP in BP_Cube: BP.Transy = 0: BP.Transx = 0: Next
	sw42.HasHitEvent = 1
	sw42.timerenabled = 0
end Sub


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
Dim ST4, ST7, ST9, ST35, ST36, ST37, ST40, ST41, ST42

'Set array with stand-up target objects
'
'StandupTargetvar = Array(primary, prim, swtich)
' 	primary: 			vp target to determine target hit
'	prim:				primitive target used for visuals and animation
'							IMPORTANT!!! 
'							transy must be used to offset the target animation
'	switch:				ROM switch number
'	animate:			Arrary slot for handling the animation instrucitons, set to 0
' 
'You will also need to add a secondary hit object for each stand up (name sw11o, sw12o, and sw13o on the example Table)
'these are inclined primitives to simulate hitting a bent target and should provide so z velocity on high speed impacts


Set ST4 =  (new StandupTarget)(sw4,  BM_sw4,     4,  0)  
Set ST7 =  (new StandupTarget)(sw7,  BM_sw0709,  7,  0)
Set ST9 =  (new StandupTarget)(sw9,  BM_sw0709,  9,  0)
Set ST35 = (new StandupTarget)(sw35, BM_sw35,	35, 0)
Set ST36 = (new StandupTarget)(sw36, BM_sw36,	36, 0)
Set ST37 = (new StandupTarget)(sw37, BM_sw37,	37, 0)
Set ST40 = (new StandupTarget)(sw40, BM_sw40,	40, 0)
Set ST41 = (new StandupTarget)(sw41, BM_sw41,	41, 0)
Set ST42 = (new StandupTarget)(sw42, BM_sw42,	42, 0)


'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
' STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST4, ST7, ST9, ST35, ST36, ST37, ST40, ST41, ST42)

'Configure the behavior of Stand-up Targets
Const STAnimStep =  1.5 		'vpunits per animation step (control return to Start)
Const STMaxOffset = 9 			'max vp units target moves when hit

Const STMass = 0.2				'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance



'******************************************************
'				STAND-UP TARGETS FUNCTIONS
'******************************************************

Sub STHit(switch)
	Dim i, prev
	i = STArrayID(switch)

	PlayTargetSound
	prev = STArray(i).animate
	STArray(i).animate = STCheckHit(ActiveBall,STArray(i).primary)
	If prev = 0 And STArray(i).animate <> 0 Then STActiveCount = STActiveCount + 1

	If STArray(i).animate <> 0 Then
		DTBallPhysics ActiveBall, STArray(i).primary.orientation, STMass
	End If
	STVisActive = 1
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
	' VPW Optimization: skip when no targets are animating.
	If STActiveCount <= 0 Then
		STActiveCount = 0
		Exit Sub
	End If
	Dim i, prev
	For i = 0 To UBound(STArray)
		prev = STArray(i).animate
		If prev <> 0 Then
			STArray(i).animate = STAnimate(STArray(i).primary,STArray(i).prim,STArray(i).sw,prev)
			If STArray(i).animate = 0 Then STActiveCount = STActiveCount - 1
		End If
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
		vpmTimer.PulseSw switch mod 100
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


Sub UpdateStandupTargets
	' VPW Optimization: only iterate BP collections while a standup target is animating.
	If STVisActive = 0 Then Exit Sub
	dim BP, ty

    ty = BM_sw4.transy
	For each BP in BP_sw4 : BP.transy = ty: Next

    ty = BM_sw0709.transy
	For each BP in BP_sw0709 : BP.transy = ty: Next

    ty = BM_sw35.transy
	For each BP in BP_sw35 : BP.transy = ty: Next

    ty = BM_sw36.transy
	For each BP in BP_sw36 : BP.transy = ty: Next

    ty = BM_sw37.transy
	For each BP in BP_sw37 : BP.transy = ty: Next

    ty = BM_sw40.transy
	For each BP in BP_sw40 : BP.transy = ty: Next

    ty = BM_sw41.transy
	For each BP in BP_sw41 : BP.transy = ty: Next

    ty = BM_sw42.transy
	For each BP in BP_sw42 : BP.transy = ty: Next

	If STActiveCount <= 0 Then STVisActive = 0
End Sub



'******************************************************
'   ZVRR: VR Room / VR Cabinet
'******************************************************


'New VRRoom Stuff Sept 2024 Rawd **********************************************************************



Dim FullStrobeCount
Dim LeftStrobeCount
Dim RightStrobeCount
Dim RightStrobeOn
Dim LeftStrobeOn
Dim LastStrobeOn

LeftStrobeOn = False
RightStrobeOn = True
FullStrobeCount = 0
LeftStrobeCount = 0
RightStrobeCount = 0
LastStrobeOn = "Right"


Sub SetupRoom
	Dim VRThings
	If VRRoom = 1 Then ' Blacklight room
		for each VRThings in VRStuff:VRThings.visible = 1:Next
		for each VRThings in VRCabinet:VRThings.visible = 1:Next
		for each VRThings in DesktopRails:VRThings.visible = 0:Next	
		TimerVRPlunger2.enabled = true
		PinCab_Backglass.blenddisablelighting = 15
		'***** Set room lights......
		VR_coininsertsOpen.blenddisablelighting = 60
		Topper007.blenddisablelighting = 60
		Topper005.blenddisablelighting = 100
		Topper2.blenddisablelighting = 100
		Topper4.blenddisablelighting = 100
		Topper002.blenddisablelighting = 100
		Topper001.blenddisablelighting = 100
		DMD.visible = true
		TurnRightStrobeOn
		RightstrobeOn = True
		StrobeAttractTimer.enabled = true
	ElseIf VRRoom = 2 Then ' Ultra Minimal room
		for each VRThings in VRStuff:VRThings.visible = 0:Next
		for each VRThings in VRCabinet:VRThings.visible = 0:Next
		for each VRThings in DesktopRails:VRThings.visible = 0:Next	
		PinCab_Rails.visible = true
		PinCab_Backglass.visible = true
		PinCab_Backglass.blenddisablelighting = 15
		DMD.visible = true
		PinCabThing.visible = true
		PinCab_Backbox.visible = true
		StrobeAttractTimer.enabled = false
	Else
		for each VRThings in VRStuff:VRThings.visible = 0:Next
		for each VRThings in VRCabinet:VRThings.visible = 0:Next
		for each VRThings in DesktopRails:VRThings.visible = 0:Next	
		If DesktopMode=True Then
			for each VRThings in DesktopRails:VRThings.visible = 1:Next	
		End If
		PinCab_Rails.visible = false
		PinCab_Backglass.visible = false
		PinCab_Backglass.blenddisablelighting = 15
		DMD.visible = false
		PinCabThing.visible = false
		PinCab_Backbox.visible = false
		StrobeAttractTimer.enabled = false
	end If
End Sub


'VR Room Light Calls (Use these in table code) ******************************************************
'****************************************************************************************************

' Here is a list of all the subs you can call..

' TurnRightStrobeOn  - Turns the Right strobe light on indefinately
' TurnLeftStrobeOn  - Turns the Left strobe light on indefinately
' TurnStrobesOff - Turns both strobe lights off (use for both)
' FullStrobeLong - Alternates back and forth between left and right strobe 10 times
' FullStrobeShort - Alternates back and forth between left and right strobe 4 times
' LeftStrobeLong - Alternates back and forth between left and NO strobe 10 times
' LeftStrobeShort - Alternates back and forth between left and NO strobe 4 times
' RightStrobeLong - Alternates back and forth between Right and NO strobe 10 times
' RightStrobeShort - Alternates back and forth between Right and NO strobe 4 times
' LeftStrobesOnTimed(duration) -  Turns on left strobe for a fixed duration.
' RightStrobesOnTimed(duration) -  Turns on right strobe for a fixed duration.
' AlternateStrobesOnTimed(duration) - Turns on one strobe for a fixed duration. Alternates back and forth between sides.


Sub TurnRightStrobeOn()
	VRRightWall.image="RightwallRightStrobe"
	VRLeftWall.image="LeftwallRightstrobe1"
	VRCeiling.image="CeilingRightStrobe"
	VRFLOOR.image="floorRightStrobe"
	Poster1.color = RGB(155,155,155)
	Poster2.color = RGB(155,155,155)
	Poster3.color = RGB(68,68,68)
	Poster4.color = RGB(68,68,68)
	VRStrobeRight.blenddisablelighting = 350
	VRStrobeRight.material = "Plastic White"
	VRStrobeLeft.blenddisablelighting = 0
	VRStrobeLeft.material = "Metal Black"
	PinCab_Backbox.image = "BBStrobeRight"
	PinCab_Cabinet.image = "CabRightStrobe"
	CabFrontNew.color = RGB(75, 75, 75)
	CabFrontNew.imageA = "MetFront1"
	CabFrontNew.imageB = "MetFront1"
	PinCab_Rails.image = "RailsRightStrobe"
	RightRailBlade.color = RGB(170, 170, 170)
	LeftRailBlade.color = RGB(6, 7, 8)
	RightBackboxSupport.color = RGB(170, 170, 170)
	LeftBackboxSupport.color = RGB(6, 7, 8)
	PlungerHousing.color = RGB(255, 255, 255)
	VR_LegsBack.image = "BackLegsRightStrobe"
	LegsFront.image = "FrontLegsRightStrobe"
	VR_CoinDoor.color = RGB(255, 255, 255)
	VR_CoinDoorTrim.color = RGB(255, 255, 255)
	KeyHole.color = RGB(200, 250, 200)
	VRFlipperRight.color = RGB(255, 255, 255)
	VRFlipperLeft.color = RGB(55, 55, 55)
	PinCabThing.color = RGB(30, 30, 30)
	TournamentButtonHousing.color = RGB(50, 60, 50)
	startbuttonring.color = RGB(50, 60, 50)
	VRStartButtonRing.color = RGB(50, 60, 50)
	VRPlunger.color = RGB(50, 60, 50)
	VR_LegBoltsFront.color = RGB(50, 60, 50)
	VR_LegBoltsBack.color = RGB(250, 250, 250)
	RightRailscrew1.color = RGB(250, 250, 250)
	RightRailscrew2.color = RGB(250, 250, 250)
	RightRailscrew3.color = RGB(250, 250, 250)
	LeftRailscrew1.color = RGB(30, 40, 30)
	LeftRailscrew2.color = RGB(30, 40, 30)
	LeftRailscrew4.color = RGB(30, 40, 30)
	FlipperButtonRingRight.color = RGB(140, 220, 220)
	FlipperButtonRingLeft.color = RGB(14, 36, 38)
	VRTopper.image = "TopperRightStrobe"
	PlungerEye.image = "EyeballRightStrobe"
	LeftStrobeOn = False
	RightStrobeOn = True
	LastStrobeOn = "Right"
End Sub


Sub TurnLeftStrobeOn()
	VRRightWall.image="RightwallLeftStrobe"
	VRLeftWall.image="LeftwallLeftstrobe"
	VRCeiling.image="CeilingLeftStrobe"
	VRFLOOR.image="floorLeftStrobe"
	Poster1.color = RGB(68,68,68)
	Poster2.color = RGB(68,68,68)
	Poster3.color = RGB(155,155,155)
	Poster4.color = RGB(155,155,155)
	VRStrobeLeft.blenddisablelighting = 350
	VRStrobeLeft.material = "Plastic White"
	VRStrobeRight.blenddisablelighting = 0
	VRStrobeRight.material = "Metal Black"
	PinCab_Backbox.image = "BBStrobeLeft"
	PinCab_Cabinet.image = "CabLeftStrobe"
	PlungerHousing.color = RGB(255, 255, 255)
	CabFrontNew.color = RGB(45, 45, 45)
	CabFrontNew.imageA = "MetFront1"
	CabFrontNew.imageB = "MetFront1"
	PinCab_Rails.image = "RailsLeftStrobe"
	RightRailBlade.color = RGB(5, 9, 10)
	LeftRailBlade.color = RGB(181, 181, 181)
	RightBackboxSupport.color = RGB(5, 9, 10)
	LeftBackboxSupport.color = RGB(181, 181, 181)
	PlungerHousing.color = RGB(200, 200, 200)
	VR_LegsBack.image = "BackLegsLeftStrobe"
	LegsFront.image = "FrontLegsLeftStrobe"
	VR_CoinDoor.color = RGB(200, 200, 200)
	VR_CoinDoorTrim.color = RGB(200, 200, 200)
	KeyHole.color = RGB(160, 160, 160)
	VRFlipperRight.color = RGB(65, 65, 65)
	VRFlipperLeft.color = RGB(255, 255, 255)
	PinCabThing.color = RGB(20, 20, 20)
	TournamentButtonHousing.color = RGB(30, 40, 30)
	startbuttonring.color = RGB(30, 40, 30)
	VRStartButtonRing.color = RGB(30, 40, 30)
	VRPlunger.color = RGB(30, 40, 30)
	VR_LegBoltsFront.color = RGB(30, 40, 30)
	VR_LegBoltsBack.color = RGB(30, 40, 30)
	RightRailscrew1.color = RGB(30, 40, 30)
	RightRailscrew2.color = RGB(30, 40, 30)
	RightRailscrew3.color = RGB(30, 40, 30)
	LeftRailscrew1.color = RGB(250, 250, 250)
	LeftRailscrew2.color = RGB(250, 250, 250)
	LeftRailscrew4.color = RGB(250, 250, 250)
	FlipperButtonRingRight.color = RGB(4, 10, 12)
	FlipperButtonRingLeft.color = RGB(140, 220, 220)
	VRTopper.image = "TopperLeftStrobe"
	PlungerEye.image = "EyeballLeftStrobe"
	LeftStrobeOn = True
	RightStrobeOn = False
	LastStrobeOn = "Left"
End Sub


Sub TurnStrobesOff()
	VRRightWall.image="Rightwall5"
	VRLeftWall.image="Leftwall6"
	VRCeiling.image="Ceiling5"
	VRFLOOR.image="floor2"
	Poster1.color = RGB(155,155,155)
	Poster2.color = RGB(155,155,155)
	Poster3.color = RGB(155,155,155)
	Poster4.color = RGB(155,155,155)
	VRStrobeLeft.blenddisablelighting = 0
	VRStrobeLeft.material = "Metal Black"
	VRStrobeRight.blenddisablelighting = 0
	VRStrobeRight.material = "Metal Black"
	PinCab_Backbox.image = "BBStrobesOff"
	PinCab_Cabinet.image = "CabNoStrobe"
	CabFrontNew.color = RGB(75, 75, 75)
	CabFrontNew.imageA = "CabFrontNoStrobe"
	CabFrontNew.imageB = "CabFrontNoStrobe"
	PinCab_Rails.image = "RailsNoStrobe"
	RightRailBlade.color = RGB(2, 5, 7)
	LeftRailBlade.color = RGB(1, 3, 8)
	RightBackboxSupport.color = RGB(2, 5, 7)
	LeftBackboxSupport.color = RGB(1, 3, 8)
	PlungerHousing.color = RGB(18, 254, 129)
	VR_LegsBack.image = "BackLegsNoStrobe"
	LegsFront.image = "FrontLegsnoStrobe"
	VR_CoinDoor.color = RGB(18, 254, 129)
	VR_CoinDoorTrim.color = RGB(18, 254, 129)
	KeyHole.color = RGB(68, 123, 120)
	VRFlipperRight.color = RGB(35, 35, 35)
	VRFlipperLeft.color = RGB(35, 35, 35)
	PinCabThing.color = RGB(0, 9, 8)
	TournamentButtonHousing.color = RGB(1, 12, 9)
	startbuttonring.color = RGB(1, 12, 9)
	VRStartButtonRing.color = RGB(1, 12, 9)
	VRPlunger.color = RGB(2, 17, 10)
	VR_LegBoltsFront.color = RGB(6, 12, 9)
	VR_LegBoltsBack.color = RGB(6, 12, 9)
	RightRailscrew1.color = RGB(0, 19, 12)
	RightRailscrew2.color = RGB(0, 19, 12)
	RightRailscrew3.color = RGB(0, 19, 12)
	LeftRailscrew1.color = RGB(0, 19, 12)
	LeftRailscrew2.color = RGB(0, 19, 12)
	LeftRailscrew4.color = RGB(0, 19, 12)
	FlipperButtonRingRight.color = RGB(2, 8, 10)
	FlipperButtonRingLeft.color = RGB(4, 10, 12)
	VRTopper.image = "TopperNoStrobes"
	PlungerEye.image = "EyeballNoStrobe"
	LeftStrobeOn = False
	RightStrobeOn = False
End Sub


Sub FullStrobeLong()
	StrobeAttractTimer.enabled = false
	RightStrobeTimer.enabled = false
	LeftStrobeTimer.enabled = false
	FullStrobeTimer.enabled = true	
End Sub

Sub FullStrobeShort()
	StrobeAttractTimer.enabled = false
	RightStrobeTimer.enabled = false
	LeftStrobeTimer.enabled = false
	FullStrobeCount = 6
	FullStrobeTimer.enabled = true
End Sub

Sub LeftStrobeLong()
	StrobeAttractTimer.enabled = false
	RightStrobeTimer.enabled = false
	FullStrobeTimer.enabled = false
	LeftStrobeTimer.enabled = true
End Sub

Sub LeftStrobeShort()
	StrobeAttractTimer.enabled = false
	RightStrobeTimer.enabled = false
	FullStrobeTimer.enabled = false
	LeftStrobeCount = 6
	LeftStrobeTimer.enabled = true
End Sub

Sub RightStrobeLong()
	StrobeAttractTimer.enabled = false
	FullStrobeTimer.enabled = false
	LeftStrobeTimer.enabled = false
	RightStrobeTimer.enabled = true
End Sub

Sub RightStrobeShort()
	StrobeAttractTimer.enabled = false
	FullStrobeTimer.enabled = false
	LeftStrobeTimer.enabled = false
	RightStrobeCount = 6
	RightStrobeTimer.enabled = true
End Sub


Sub LeftStrobesOnTimed(duration)
	StrobeAttractTimer.enabled = false
	StrobeOnTimer.Interval = duration
	StrobeOnTimer.Enabled = False
	StrobeOnTimer.Enabled = True
	TurnLeftStrobeOn
End Sub

Sub RightStrobesOnTimed(duration)
	StrobeAttractTimer.enabled = false
	StrobeOnTimer.Interval = duration
	StrobeOnTimer.Enabled = False
	StrobeOnTimer.Enabled = True
	TurnRightStrobeOn
End Sub

Sub AlternateStrobesOnTimed(duration)
	StrobeAttractTimer.enabled = false
	StrobeOnTimer.Interval = duration
	StrobeOnTimer.Enabled = False
	StrobeOnTimer.Enabled = True
	If LastStrobeOn = "Right" Then
		TurnLeftStrobeOn
	Else
		TurnRightStrobeOn
	End If
End Sub


Sub StrobeOnTimer_Timer()
	StrobeOnTimer.Enabled = False
	TurnStrobesOff
End Sub


Sub LeftStrobeTimer_Timer()
	LeftStrobeCount = LeftStrobeCount + 1

	if LeftStrobeCount < 10 and LeftStrobeOn = true then 
		TurnStrobesOff
		Exit sub
	end if

	if LeftStrobeCount < 10 and LeftStrobeOn = false then 
		TurnLeftStrobeOn
		Exit sub
	end if

	if LeftStrobeCount => 10 Then
		TurnStrobesOff
		LeftStrobeTimer.enabled = false
		LeftStrobeCount = 0
		Exit sub
	end If
End Sub


Sub RightStrobeTimer_Timer()
	RightStrobeCount = RightStrobeCount + 1

	if RightStrobeCount < 10 and RightStrobeOn = true then 
		TurnStrobesOff
		RightStrobeOn = False
		Exit sub
	end if

	if RightStrobeCount < 10 and RightStrobeOn = false then 
		TurnRightStrobeOn
		RightStrobeOn = True
		Exit sub
	end if

	if RightStrobeCount => 10 Then
		TurnStrobesOff
		RightStrobeTimer.enabled = false
		RightStrobeCount = 0
		Exit sub
	end If
End Sub


Sub FullStrobeTimer_Timer()
	FullStrobeCount = FullStrobeCount + 1

	if FullStrobeCount < 10 and RightStrobeOn = true then 
		TurnLeftStrobeOn
		Exit sub
	end if

	if FullStrobeCount < 10 and LeftStrobeOn = true then 
		TurnRightStrobeOn
		Exit sub
	end if

	if FullStrobeCount => 10 Then
		TurnStrobesOff
		FullStrobeTimer.enabled = false
		FullStrobeCount = 0
		Exit sub
	end If
End Sub


' VR plunger code below.
Sub TimerVRPlunger_Timer()
	If VRPlunger.Y < 2437 then    ' Sitting pos + 115
		VRPlunger.Y = VRPlunger.Y + 5
		PlungerEye.Y = PlungerEye.Y + 5
	End If
End Sub

Sub TimerVRPlunger2_Timer()
	VRPlunger.Y = 2312 + (5* Plunger.Position) -20
	PlungerEye.Y = VRPlunger.Y +40
End Sub


Sub TestStrobes
	if RightStrobeOn = True then 
		TurnLeftStrobeOn
		exit sub
	End if
	if LeftStrobeOn = True then 
		TurnStrobesOff
		exit sub
	End if
	if RightStrobeOn = False and LeftStrobeOn = False then 
		TurnRightStrobeOn
		exit sub
	End if
End Sub


Sub StrobeAttractTimer_Timer()
	if RightStrobeOn = True then 
		TurnLeftStrobeOn
		exit sub
	End If
	if LeftStrobeOn = True then 
		TurnStrobesOff
		exit sub
	End if
	if RightStrobeOn = False and LeftStrobeOn = False then 
		TurnRightStrobeOn
		exit sub
	End if
End Sub

'*******  End VR Code  ********************




'******************************************************
'   ZCHA: Change Log
'******************************************************

' 001 - EBisLit - New Playfield
' 002 - EBisLit - insert cutout, added insert image layer
' 003 - iaakki - Added sample flupper 3D inserts and imported requisite materials & images
' 004 - EBisLit - 3D insert creation from Iaakki template, placement and initial intensity tweaks
' 005 - EBisLit - pf artifact cleanup, mech alignments, more 3D insert tweaking (depth, intensity, falloff, frosted Lights)
' 006 = Benji - Added nfozzy physics and flipper sounds from TOM. New (?) script error discovered: "type mismatch Scoopkick"
' 007 - EBisLit - image insert re-alignment, turn on bulb display for center inserts, new center apron decal
' 008 - iaakki - Added 6 RGB inserts, reduced live catch time window to 12, as it was too easy on this table
' 009 - iaakki - Modified inserts L59-L62, Sparky voltage increased via Flash28 sub.
' 010 - EBisLit - New layer for guitar inside spider inserts. Added frosted primitives for guitar pick inserts.
' 011 - Benji - Added playfield shadows on a Flasher named "Playfield Shadow" on Layer 11
' 013 - Benji - Added Fleep sounds
' 014 - iaakki - insert bulbs retuned, GI falloff power reduced in script, RGB inserts reworked, flip tricks enabled, slackyflips disabled, flip trigger areas reworked to 23 units
' 015 - iaakki - PF altered, insert flashers reworked, items on layers 5,9,11 realigned
' 016 - iaakki - combined to sixtoes version
' 017 - iaakki - aligned stuff on layers 1,2,3 to pf. Left altered items unlocked
' 018 - iaakki - aligning continued and reworked and aligned GI on layer 4. Some lights aligned on layer 8.
' 019 - Sixtoe - Aligned more stuff including fittings and cross lights, aligned ramps and rebuilt shooter exit, adjusted captive ball section.
' 021 - iaakki - pegs, posts, rubberbands reworked, sleeves missing.. Updated physics scripts. AO shadow needs rework, snake head depth bias issues,
' 022 - iaakki - swapped nuts with lowpoly versions, PF cleanup and insert adjustments
' 023 - iaakki - pf mesh and all pf flashers realigned, fuel lights adjusted
' 024 - iaakki - Slingshot timers fix, captive ball fixed and magnets tested
' 025 - iaakki - added latest AO shadow from benji, hammer normal map, minor lights adjustments..
' 026 - Sixtoe - Added VR Room and Cabinet Mode, unified timers, removed old floodlights, dropped split slingshot GI height, tweaked a few things.
' 027 - iaakki - fixed captive ball mess, disabled apron hit sounds
' 028 - iaakki - coffin lights improved, hammer shadow added, standup targets realigned so right ramp works properly
' 029 - iaakki - captive balls and fliptriggersreworked one more time, hammer nudge added, fine tunings here and there.
' 030 - iaakki - snake tongue fixed, snake jaw nudge and material adjust, PF shadow fading with GI, New LUT and general lighting adjust, few inserts adjusted
' 031 - iaakki - new flipper bats, captive ball safe guard, snake head polished, hammer shadow z-value fixed
' 032 - iaakki - fixed some fading elements, some new flashers just to test in VR
' 033 - iaakki - moving all solenoid controlled lamps to lamp update code
' 034 - iaakki - sideblade flashers reworked with new flare image, sparky tune
' 035 - sixtoe - various fixes and lighting rework
' 036 - iaakki - lighting tune
' 037 - sixtoe - hooked up missing lights, loads more light tweaks.
' 038 - iaakki - most of the flasher code redone
' 039 - iaakki - solenoid flashers done, coffin reworked again
' 040 - sixtoe - fixed vpx ramps cutting through ramp prims, hooked up back wall coloured GI lights properly
' 041 - iaakki - left ramp entrance fixed to return correctly, right ramp entrance reworked, left orb adjusted, super skillshot fixed, flipper angles altered, elasticity reduced, livecatch tuned
' 043 - iaakki - right ramp entrance adjusted more, laneguide metals tied to GI, hammer dulled
' 044 - iaakki - hammershadow hidden, when sparky flashes
' 045 - Sixtoe - fixed sparkyhead lighting?, completely redid pop bumper lighting!, tweaked shadow layers, tidied up layers.
' 046 - iaakki - laneguide DL adjusted, f125 and f127 fixed and adjusted, hammershadow effect fixed
' 047 - Sixtoe - fixed ballshadow clashes, plunger lane and some lighting work
' 048 - iaakki - LUT changed, super skill shot tweaks, pf shadow control reverted
' 050 - tomate - LUT levels changed a bit, new textures for Dark's Sparky model, new Sparky's chair prim, new wireramps textures
' 051 - Sixtoe - Added DL for chair in line with head and body, replaced wall19 with Prim9, removed old texture switch, bit of tidying
' 052 - iaakki - wall19/prim19 db and z-fighting solved, sparky adjust, hmagnet radius 70>55 and timer change. SW42 hit threshold 3 -> 4, L22 brightness tuned
' RC1 - iaakki - hmagnet size 63, script cleanup
' RC2 - EBisLit - Adding flupper's script which allows LUT switching via Magnasave

' 101 - iaakki - table angle 6.5, flip angles adjusted, plunger adjusted, sling threshold and power changed, grave magnet made smaller, LUT selector tweaked
' 102 - iaakki - flip elasticity restored to 0.83 and left and right sling adjusted
' 103 - tomate - some fixes on the wire ramps prims, new textures, add some clearcoat over flippers prims, back to the previous LUT
' 104 - skitso - -Reworked lower PF GI lighting, Tinkered a bit with the LUT (a tad warmer tones and slight desaturation), Added HDR -ball, decreased ball's PF reflection and added higher resolution ball scratches texture, Lowered global bloom strenght setting.
' 105 - Sixtoe - fixed some playfield odditites, turned down vr cab lighting
' 106 - iaakki/oqqsan - Made lut save working, added 2 more balls to script, improved ball shadows to work on ramps
' 107 - iaakki - Csng overflow bug fixed, Rubberizer and Targetbouncer added, FlipperNudge values adjusted, Wall34 set collidable
' Thal - reduced ball rolling volume from 1.1 to 0.05, misc walls to prevent ball stuck.
' 108 -  Rawd  - New Blacklight VRRoom with Blender lighting - original Artwork credit: Hauntfreaks. New cabinet parts, various tweaks and cleanup.

' 108 - apophis - added slingshot corrections
' 109 - Aubrel - snake and skull coffin mods added. CoffinBottom's "disable lighting" values fixed. Playfield insert CN19 fixed
' 110 - iaakki - all physics code updated, blocker walls added to plastics, gravemarker walls adjusted to match play videos, Target physics fixed, targetbouncer updated, left orb exit fixed 
' 111 - iaakki - nudge reduced, sling corner bouncer removed, left spinner lubed
' 112 - iaakki - LMag reworked
' 1.1 Release - apophis - Cleaned up some script. Automated VRRoom and CabinetMode selections. Default ROM set to mtl_180h
' 1.1.1 - Rawd - Added Blacklight VR Room option (Made by Hauntfreaks and Rawd), VR cabinet metals and scratches options.  Cabinet and room cleanup. 
' 1.1.2 - iaakki - Updated Flippernudge and Flasher6 position
' 1.1.3 - fluffhead35 - changed ballrolling sounds 
' 1.1.4 - Wylte - Flipped RGB values, locked everything down, flipper correction update, lots of missing materials assigned, sound collections made,
'				ballrolling sub replaced and volumedial added (kept Fluff's sound), Ramprolling sounds, metadata updated, boring cleanup stuff
' 1.1.5 - Wylte - Bottom lane dividers given material and hit events, rampdrop triggers and code deleted, LRampEnd placed on Ramp6, updated ballshadow prim/image/code
' 1.1.6 - Niwak - Add Coffin Magnet Processor Board emulation
' 1.1.6b - Niwak - Add support for PinMame 3.6 physic outputs
' 1.1.7 - apophis -
'	Added TOC and reorganized the script
'	Removed old LUT system. New LUTs in option menu.
'	Updated animation methods
'	Added room and ball brightness
'	Added roth drop target and standup target scripts and objects
'	Updated flipper tricks and triggers
'	Updated dampener scripts
'	Updated Fleep scripts and some sounds
'	Updated ball and ramp rolling sounds and scripts
'	Updated ambient shadow script
'	Updated all insert and gi lights for UseLamps=1 and vpmMapLights. 
'	Removed UpdateLamps and replaced with light_animate subs for 3D inserts.
'	Updated all the flashers to work with lightmaps
' 1.1.8 - apophis - Tried to fortify captive balls so they dont fall out. Added more ball shadow objects to prevent errors (didnt really work). Inc target bouncer on posts. 
' 1.1.9 - Sixtoe - New playfield, new dimensions, new physical PF mesh, TABLE DOESN'T WORK
' 1.2.0 - Sixtoe - Loads more alignment, added missing rubbers, cleaned table some more, added physical trough (not hooked up yet), loads of little stuff, new playfield cut out image, new playfield text layer, new blueprint
' 1.2.1 - apophis - Scripting for physical trough. Updated captive ball collider to a cube. Fixed Kicker3. Fixed some blocker walls. Fixed some Fleep things. 
' 1.2.2 - Sixtoe - Realigned most physical layout including walls, physics objects and ramps, fixed subway issue, fixed snake issue, loads of changes and cleanup, turned off blocker walls for now due to misalignment, want to confirm correct before redoing them.
' 1.2.3 - apophis - Adjusted newton cube elasticity. Adjusted LMagnet strength and added WallLMagHelper to slow ball down for magnet.
' 1.2.4 - Sixtoe - Redid blocker walls, added new walls, reinforced several areas, new playfield mesh, tidied assets more, sorted out flasher layer, added apron lights to gi layer, moved old walls to "_old walls".
' 1.2.5 - apophis - Put small chamfers on the Newton cube. Slight geometry tweak on the slingshot walls (near posts).
' 1.2.5b - Tomate - 2k Render - Table Doesn't Work.
' 1.2.6 - Sixtoe - Made VPX / Physics objects invisible, reimported insert_prims and insert images (?!?), something is up with the playfield / inserts / playfield mesh / render.
' 1.2.7 - apophis - Re-imported everything useful into a completely clean empty table (fixes bizarre grey lightmap issue). Did not import VR room stuff (that will be redone anyway). Removed VRRoom related script. Animated flippers, STs, DTs, spinners, gates, bumper rings, switch wires, cross 
' 1.2.8 - apophis - Animated sparky, hammer, rod, snake jaw, slings. Fixed DT physics mtl. Newton cube fortifications. 
' 1.2.8 - Tomate - 4k bake
' 1.2.10 - apophis - Hooked up GI strings correctly. Thanks for the help flux! Smoothed out snake and hammer animations. 
' 1.2.11 - apophis - Set up options (snake ramp, cemetery, outlanes). Fixed ball stuck near pops? Reduced targetbouncer on dPosts. Reduced RMagnet radius a little. Added some coffin opto red lights for effect on ball. 
' 1.2.12 - mcarter78 - Adjust wall to avoid stuck balls near bottom bumper
' 1.2.13 - passion4pins - Add desktop side rails and lockdown bar. Add 3 blackout panels under playfield
' 1.2.13 - passion4pins - made blackout panels not collidable.
' 1.2.15 - iaakki - Magnets reworked to not GrabCenter by default, Added counters that enable it just to remove wiggle, subway height adjusted to match primitives
' 1.2.16 - apophis - Set flipper strength to 2900 to help match passion4pins's machine. Improved post passes. Animate hammer and cube when sw42 is hit. 
' 1.2.17 - tomate - new 4k batch added, lighting imrovements, inserts added, unwrapped textures (flippers need to be fixed)
' 1.2.18 - apophis - Deleted 3D insert prims. Updated scripts to support new bake (no Flupper-style inserts).  Added SSF shaker motor option. Added gear motor sfx for graveyard cross. Colored fuel gage lights in script, and adjusted brightness of RBG insers and coffin flasher in script. Fixed stuck ball near sparky's knee.
' 1.2.19 - apophis - Fixed left ramp reject issue. Fixed SSS. Added sound effect to snake jaw solenoids. 
' 1.2.20 - iaakki - reworked sling physics
' 1.2.21 - tomate - final 4k batch added with unwrapped sidewalls, apron and standup targets, Fixed flipper textures and Snake plastic transparency. BM_Playfield set as "hide parts behind", bloom set at 0.05, Backwall lights tied to  GIW7 light and other minor tweaks (thanks apophis!) 
' 1.2.22 - apophis - Fixed snake plastic option. Added desktop DMD visibility option. Made balls dark in the trough and in the coffin.
' 1.2.23 - iaakki - Added some mayo
' 1.2.24 - apophis - Made mayo optional, default ON. Made snake mod default OFF. Adjusted slingshot strength. Tried making piston lane return a little more dangerous. Adjusted flipper pol curve to tune ramp backhand strength. Adjusted newton cube sensitivity. Fixed position of ramp exits. 
' 1.2.25 - apophis - Fixed slingshot phys material and settings. Changed phys mat on Wall53 to make Borg lanes deadlier. Angled scoop1 to aim kickout towards left flipper tip. Made scoop kicks more random. Mystery mayo light intensity -> 1. Fixed sw64 logic to fix hammer issue.
' 1.2.26 - iaakki - Ramp flap, Top Post, Coffin Mag, Hammer mech and smake mech sounds added. Made right ramp entrance bounce less on return.
' 1.2.27 - iaakki - Added table option to enable PF reflections for various object. Collection name for objects is 'Reflections'
' 1.2.28 - iaakki - Added white plastic bottoms that follow GI color combinations and levels
' 1.2.29 - apophis - Added VR DMD and logic. Added WallSubBack to prevent narnia balls. Removed sw64 kicker (its not needed). 
' 1.2.30 - Sixtoe - Reinforced sparky / sparkys feet area
' 1.2.31 - apophis - Added more playfield reflection options
' 1.2.32 - Rawd -  Blacklight Room and Ultra Minimal room options added.
' 1.2.33 - apophis - Wired up BL room strobes and lit buttons to the table. Updated BL room textures (thanks HauntFreaks). Added VR options to tweak menu.
' 1.2.34 - apophis - Added more strobe options.
' 1.2.35 - Rawd -  Cab image tweaks. Added VR strobe attract mode. 
' 1.2.36 - apophis - Some image optimizations. Table info updates.
' Release 2.0 !!!!
' 2.0.1 - apophis - Fixed flipper dampening issue. Removed hacks for VPM issues now that VPM is fixed (Removed GIScale. RGB insert LM opacity back to normal).
' 2.0.2 - DGrimmReaper - Animated UpPost and added coin-in sounds

