'Genesis - Gottlieb 1986

'0.01 mcarter78    - Add flipper triggers and update code, update keydown subs, add updated fleep sounds, add roth varitarget handling
'0.02 RobbyKingPin - Finished nFozzy/Rothbauerw physics and added Rothbauerw targets. Made Fleep sounds functional. Updated trough that saves the balls
'0.03 mcarter78 - pulse sw75 on right flipper callback to switch top lanes, fix top gates placement, fix flipper angles and triggers, minor physics adjustments
'0.04 mcarter78 - realigned/rescaled all collidable posts and rubbers
'0.05 mcarter78 - fixed alignment for apron, slingshots, physical walls & wire ball guides, add missing peg, fix varitarget stops count
'0.07 MetaTed - 4k batch with GI only
'0.08 mcarter78 - Fix walls & posts alignment & sizing, remove a bunch more unnecessary stuff, add Kicking Target code & animation, set lights to hidden
'0.09 mcarter78 - Rename switches to numbers from manual, fix drop targets alignment, add new physical ramps, fix flipper angles & triggers, new pf mesh
'0.10 mcarter78 - New 2k batch with all lighting and switch numbers fixed and new kicking target models, removed remaining unecessary lights/objects, animated movables, robot animation WIP
'0.11 mcarter78 - New 2k batch with fixed backwall lights, more cylinder copies; Add options for outlane difficulty (physics only atm) and color for GI, inserts, bumpers & flashers; Fix robot lights sequence; Fix bumpers radius
'0.12.0 MetaTed - New playfield material with inserts mask, Updated right ramp model, Updated inserts (integration WIP)
'0.12.1 mcarter78 - New 2k batch with Fixed backwall, added new apron & ramp decals, fixed robot cylinder animation, added outlane posts difficulty option, tweaked top lanes
'0.13 mcarter78 - New 2k batch - fixed inserts, backwall, add inlane post rubbers option, fix plastics light transmission (thanks tomate!)
'0.14 mcarter78 - New 2k batch with many visual fixes.  Tweaks to kicker targets, ramps, top lanes, general physics
'0.15 mcarter78 - New 4k batch with added visual fixes (new flipper models, fixed ramp material, brighter env) and also added visual issues (still fighting with plastics, kicker target reflection) :/ -- Adjust playfield angle and flipper strength
'0.16 mcarter78 - New 4k batch fixing remaining plastics render issues & kicking target reflection.  Added F12 options for slope, flipper strength and flasher brightness. Reduce room brightness, pf friction, pf reflections & refraction roughness
'0.17 RobbyKingPin - Added new displays for desktop. Assets and codes used from Mecha_Enron's Hollywood Heat. Using Frametimer to update the displays and removed DisplayTimer. Added a new backdrop as well
'0.18 mcarter78 - New 4k batch fixing remaining visual issues, tweak kicking targets params, disabled unused timers, set all lamps to incandescent fader until PWM can be used. Fixes: varitarget hit sound, under pf lights, LUT options, spinning ball in shooter lane
'0.19 apophis - Set L14 fader to None. Minor physics adjustments: reduced pf friction, decreased flipper strength, increased default slope, lowered DT mass and increased elasticity, updated sling corrections. Adjusted flipper triggers. Fixed drop target pop up animation.
'0.20 mcarter78 - Add PWM, fix ball shadows & ball brightness, decrease flipper strength, increase pf & ramps friction, decrease plunger strength
'0.21 DGrimmReaper - Add VR. Assets and codes used from Mecha_Enron's Hollywood Heat, Hauntfreaks Backglass Images and 3rdaxis's Car Hop
'0.22 DaRdog - Add VR Mega room
'0.23 mcarter78 - Kicker tweaks
'0.24 mcarter78 - Fix refraction probes for layered plastics, add blocker walls, add ball guide to metals collection
'0.25 mcarter78 - New backdrop collab w RKP, make drain area narrower
'0.26 mcarter78 - Remove old timers, move robot reveal animation to FrameTimer. Open up draain a bit. GI WIP.
'0.27 bthlonewol - added DOF support, mapped to existing DOF config for slings/bumpers
'0.28 mcarter78 - Fix trough for double-drain, increase apron height, reduce varitarget difficulty 
'0.29 mcarter78 - Enable reflections for more objects, add reflections options
'0.30 mcarter78 - Fix varitarget for real this time, set color values for insert lights
'0.31 apophis - Added drop target shadows.
'0.32 mcarter78 - Fix Extra ball/Shoot again lights, fix for stuck ball at flipper base
'RC1  mcarter78 - Fix reflections, update difficulty options verbiage
'RC2  apophis - Updated sfx to be mono. Removed unused sfx.
'RC3  apophis - Setup default DIP switches with EB working by default.
'RC4  apophis - Fix side rails option.

'Release 1.0

Option Explicit
Randomize
SetLocale 1033			'Forces VBS to use english to stop crashes.

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0


' ALL OPTIONS ARE IN THE F12 MENU, EXCEPT FOR THE SetDIPSwitches and VRTest BELOW
	
Const SetDIPSwitches = 1	'If you want to set the dips differently, set to 1, and then hit F6 to launch.
Const VRTest = 0			'If you want to force VR room to be on all the time, set to 1, otherwise keep at 0


' VLM  Arrays - Start
' Arrays per baked part
Dim BP_Bumper1_Ring: BP_Bumper1_Ring=Array(BM_Bumper1_Ring, LM_GI_Split_gi1_Bumper1_Ring, LM_GI_Split_gi3_Bumper1_Ring, LM_GI_Split_gi4_Bumper1_Ring, LM_F_fl2_Bumper1_Ring, LM_F_fl3_Bumper1_Ring, LM_F_fr3_Bumper1_Ring, LM_GI_Bumper1_Ring)
Dim BP_Bumper1_Skirt: BP_Bumper1_Skirt=Array(BM_Bumper1_Skirt, LM_GI_Split_gi1_Bumper1_Skirt, LM_GI_Split_gi2_Bumper1_Skirt, LM_GI_Split_gi3_Bumper1_Skirt, LM_GI_Split_gi4_Bumper1_Skirt, LM_F_fl2_Bumper1_Skirt, LM_F_fl3_Bumper1_Skirt, LM_F_fr2_Bumper1_Skirt, LM_F_fr3_Bumper1_Skirt, LM_GI_Bumper1_Skirt)
Dim BP_Bumper2_Ring: BP_Bumper2_Ring=Array(BM_Bumper2_Ring, LM_GI_Split_gi1_Bumper2_Ring, LM_GI_Split_gi2_Bumper2_Ring, LM_GI_Split_gi3_Bumper2_Ring, LM_GI_Split_gi4_Bumper2_Ring, LM_F_fl3_Bumper2_Ring, LM_F_fr2_Bumper2_Ring, LM_F_fr3_Bumper2_Ring, LM_GI_Bumper2_Ring)
Dim BP_Bumper2_Skirt: BP_Bumper2_Skirt=Array(BM_Bumper2_Skirt, LM_GI_Split_gi1_Bumper2_Skirt, LM_GI_Split_gi2_Bumper2_Skirt, LM_GI_Split_gi3_Bumper2_Skirt, LM_GI_Split_gi4_Bumper2_Skirt, LM_F_fr2_Bumper2_Skirt, LM_F_fr3_Bumper2_Skirt, LM_GI_Bumper2_Skirt)
Dim BP_Bumper3_Ring: BP_Bumper3_Ring=Array(BM_Bumper3_Ring, LM_GI_Split_gi3_Bumper3_Ring, LM_F_fl2_Bumper3_Ring, LM_F_fl3_Bumper3_Ring, LM_F_fr3_Bumper3_Ring, LM_GI_Bumper3_Ring)
Dim BP_Bumper3_Skirt: BP_Bumper3_Skirt=Array(BM_Bumper3_Skirt, LM_GI_Split_gi1_Bumper3_Skirt, LM_GI_Split_gi2_Bumper3_Skirt, LM_GI_Split_gi3_Bumper3_Skirt, LM_GI_Split_gi4_Bumper3_Skirt, LM_F_fl2_Bumper3_Skirt, LM_F_fl3_Bumper3_Skirt, LM_F_fr2_Bumper3_Skirt, LM_F_fr3_Bumper3_Skirt, LM_GI_Bumper3_Skirt)
Dim BP_Bumper4_Ring: BP_Bumper4_Ring=Array(BM_Bumper4_Ring, LM_GI_Split_gi1_Bumper4_Ring, LM_GI_Split_gi3_Bumper4_Ring, LM_GI_Split_gi4_Bumper4_Ring, LM_F_fl2_Bumper4_Ring, LM_F_fr3_Bumper4_Ring, LM_GI_Bumper4_Ring)
Dim BP_Bumper4_Skirt: BP_Bumper4_Skirt=Array(BM_Bumper4_Skirt, LM_GI_Split_gi1_Bumper4_Skirt, LM_GI_Split_gi2_Bumper4_Skirt, LM_GI_Split_gi3_Bumper4_Skirt, LM_GI_Split_gi4_Bumper4_Skirt, LM_F_fl2_Bumper4_Skirt, LM_F_fl3_Bumper4_Skirt, LM_F_fr2_Bumper4_Skirt, LM_F_fr3_Bumper4_Skirt, LM_GI_Bumper4_Skirt)
Dim BP_CapRCons: BP_CapRCons=Array(BM_CapRCons)
Dim BP_CapRLib: BP_CapRLib=Array(BM_CapRLib, LM_F_fl3_CapRLib)
Dim BP_DT_sw41: BP_DT_sw41=Array(BM_DT_sw41, LM_F_fl3_DT_sw41, LM_F_fr3_DT_sw41, LM_GI_DT_sw41, LM_GI_Split_gi007_DT_sw41, LM_F_ll2_DT_sw41, LM_F_ll3_DT_sw41, LM_F_rl3_DT_sw41, LM_F_rl4_DT_sw41)
Dim BP_DT_sw51: BP_DT_sw51=Array(BM_DT_sw51, LM_F_fl3_DT_sw51, LM_F_fr3_DT_sw51, LM_GI_DT_sw51, LM_F_ll4_DT_sw51)
Dim BP_DT_sw61: BP_DT_sw61=Array(BM_DT_sw61, LM_F_fr3_DT_sw61, LM_GI_DT_sw61, LM_L_l21_DT_sw61, LM_F_ll3_DT_sw61, LM_F_ll4_DT_sw61, LM_F_rl4_DT_sw61)
Dim BP_Gate1_Flap: BP_Gate1_Flap=Array(BM_Gate1_Flap, LM_F_fl2_Gate1_Flap, LM_F_fl3_Gate1_Flap, LM_F_fr2_Gate1_Flap, LM_F_fr3_Gate1_Flap, LM_GI_Gate1_Flap, LM_L_l27_Gate1_Flap, LM_LBW_l40_Gate1_Flap)
Dim BP_Gate2_Wire: BP_Gate2_Wire=Array(BM_Gate2_Wire, LM_F_fr3_Gate2_Wire)
Dim BP_Gate5_Wire: BP_Gate5_Wire=Array(BM_Gate5_Wire, LM_F_fl2_Gate5_Wire, LM_F_fl3_Gate5_Wire, LM_F_fr3_Gate5_Wire, LM_GI_Gate5_Wire)
Dim BP_Gate_A_8096___Flap: BP_Gate_A_8096___Flap=Array(BM_Gate_A_8096___Flap, LM_GI_Split_gi2_Gate_A_8096___F, LM_F_fl2_Gate_A_8096___Flap, LM_F_fr2_Gate_A_8096___Flap, LM_GI_Gate_A_8096___Flap, LM_L_l25_Gate_A_8096___Flap, LM_LBW_l31_Gate_A_8096___Flap, LM_LBW_l32_Gate_A_8096___Flap, LM_LBW_l33_Gate_A_8096___Flap)
Dim BP_InlaneRubbers: BP_InlaneRubbers=Array(BM_InlaneRubbers, LM_F_fl3_InlaneRubbers, LM_GI_Split_gi007_InlaneRubbers, LM_GI_Split_gi009_InlaneRubbers, LM_L_l47_InlaneRubbers, LM_L_l51_InlaneRubbers)
Dim BP_KT_sw45: BP_KT_sw45=Array(BM_KT_sw45, LM_GI_Split_gi3_KT_sw45, LM_GI_KT_sw45, LM_GI_Split_gi007_KT_sw45, LM_L_l45_KT_sw45, LM_L_l47_KT_sw45, LM_F_Rl2_KT_sw45, LM_F_rl3_KT_sw45, LM_F_rl4_KT_sw45, LM_F_Rl5_KT_sw45)
Dim BP_KT_sw65: BP_KT_sw65=Array(BM_KT_sw65, LM_F_fr3_KT_sw65, LM_GI_KT_sw65, LM_GI_Split_gi009_KT_sw65, LM_L_l46_KT_sw65, LM_L_l51_KT_sw65, LM_F_ll3_KT_sw65, LM_F_ll4_KT_sw65, LM_F_ll5_KT_sw65)
Dim BP_LF: BP_LF=Array(BM_LF, LM_GI_Split_gi007_LF, LM_GI_Split_gi009_LF, LM_L_l2_LF, LM_F_ll1_LF, LM_F_Rl1_LF, LM_F_Rl2_LF, LM_F_rl3_LF)
Dim BP_LFU: BP_LFU=Array(BM_LFU, LM_GI_Split_gi007_LFU, LM_L_l2_LFU, LM_F_ll1_LFU, LM_F_ll2_LFU, LM_F_Rl1_LFU, LM_F_Rl2_LFU, LM_F_rl3_LFU)
Dim BP_LSling: BP_LSling=Array(BM_LSling, LM_F_fr3_LSling, LM_GI_Split_gi007_LSling, LM_GI_Split_gi009_LSling, LM_F_ll4_LSling, LM_F_Rl1_LSling, LM_F_Rl2_LSling)
Dim BP_LSling1: BP_LSling1=Array(BM_LSling1, LM_GI_Split_gi007_LSling1, LM_GI_Split_gi009_LSling1, LM_F_Rl1_LSling1, LM_F_Rl2_LSling1, LM_F_rl3_LSling1, LM_F_Rl5_LSling1)
Dim BP_LSling2: BP_LSling2=Array(BM_LSling2, LM_GI_Split_gi007_LSling2, LM_GI_Split_gi009_LSling2, LM_F_Rl2_LSling2)
Dim BP_LockDownBar: BP_LockDownBar=Array(BM_LockDownBar)
Dim BP_PFWindow: BP_PFWindow=Array(BM_PFWindow, LM_F_Rl5_PFWindow)
Dim BP_PLCons: BP_PLCons=Array(BM_PLCons, LM_GI_PLCons)
Dim BP_PLLib: BP_PLLib=Array(BM_PLLib, LM_GI_PLLib)
Dim BP_PRCons: BP_PRCons=Array(BM_PRCons, LM_GI_PRCons, LM_F_ll2_PRCons, LM_F_ll4_PRCons)
Dim BP_PRLib: BP_PRLib=Array(BM_PRLib, LM_GI_PRLib, LM_F_ll2_PRLib, LM_F_ll4_PRLib)
Dim BP_Parts: BP_Parts=Array(BM_Parts, LM_GI_Split_gi1_Parts, LM_GI_Split_gi2_Parts, LM_GI_Split_gi3_Parts, LM_GI_Split_gi4_Parts, LM_F_fl2_Parts, LM_F_fl3_Parts, LM_F_fr2_Parts, LM_F_fr3_Parts, LM_GI_Parts, LM_GI_Split_gi007_Parts, LM_GI_Split_gi009_Parts, LM_LUPF_l15_Parts, LM_LUPF_l16_Parts, LM_LUPF_l17_Parts, LM_LUPF_l18_Parts, LM_L_l19_Parts, LM_L_l2_Parts, LM_L_l21_Parts, LM_L_l22_Parts, LM_L_l23_Parts, LM_L_l25_Parts, LM_L_l26_Parts, LM_L_l27_Parts, LM_LBW_l28_Parts, LM_LBW_l29_Parts, LM_L_l3_Parts, LM_LBW_l30_Parts, LM_LBW_l31_Parts, LM_LBW_l32_Parts, LM_LBW_l33_Parts, LM_LBW_l34_Parts, LM_LBW_l35_Parts, LM_LBW_l36_Parts, LM_LBW_l37_Parts, LM_LBW_l38_Parts, LM_LBW_l39_Parts, LM_LBW_l40_Parts, LM_LBW_l41_Parts, LM_LBW_l42_Parts, LM_LBW_l43_Parts, LM_LBW_l44_Parts, LM_L_l45_Parts, LM_L_l46_Parts, LM_L_l47_Parts, LM_L_l51_Parts, LM_L_l6_Parts, LM_L_l7_Parts, LM_L_l8_Parts, LM_F_ll1_Parts, LM_F_ll2_Parts, LM_F_ll3_Parts, LM_F_ll4_Parts, LM_F_ll5_Parts, LM_F_Rl1_Parts, LM_F_Rl2_Parts, LM_F_rl3_Parts, _
	LM_F_rl4_Parts, LM_F_Rl5_Parts)
Dim BP_Plastics_Low: BP_Plastics_Low=Array(BM_Plastics_Low, LM_GI_Split_gi1_Plastics_Low, LM_GI_Split_gi2_Plastics_Low, LM_GI_Split_gi3_Plastics_Low, LM_GI_Split_gi4_Plastics_Low, LM_F_fl2_Plastics_Low, LM_F_fl3_Plastics_Low, LM_F_fr2_Plastics_Low, LM_F_fr3_Plastics_Low, LM_GI_Plastics_Low, LM_GI_Split_gi007_Plastics_Low, LM_GI_Split_gi009_Plastics_Low, LM_LBW_l28_Plastics_Low, LM_LBW_l29_Plastics_Low, LM_LBW_l30_Plastics_Low, LM_LBW_l31_Plastics_Low, LM_LBW_l32_Plastics_Low, LM_LBW_l33_Plastics_Low, LM_LBW_l34_Plastics_Low, LM_LBW_l35_Plastics_Low, LM_LBW_l36_Plastics_Low, LM_LBW_l37_Plastics_Low, LM_LBW_l38_Plastics_Low, LM_LBW_l39_Plastics_Low, LM_LBW_l40_Plastics_Low, LM_LBW_l41_Plastics_Low, LM_LBW_l42_Plastics_Low, LM_LBW_l43_Plastics_Low, LM_LBW_l44_Plastics_Low, LM_L_l46_Plastics_Low, LM_F_ll2_Plastics_Low, LM_F_ll3_Plastics_Low, LM_F_ll4_Plastics_Low, LM_F_Rl2_Plastics_Low, LM_F_rl3_Plastics_Low, LM_F_rl4_Plastics_Low, LM_F_Rl5_Plastics_Low)
Dim BP_Plastics_Mid: BP_Plastics_Mid=Array(BM_Plastics_Mid, LM_GI_Split_gi4_Plastics_Mid, LM_F_fl3_Plastics_Mid, LM_F_fr3_Plastics_Mid, LM_GI_Plastics_Mid, LM_GI_Split_gi007_Plastics_Mid, LM_GI_Split_gi009_Plastics_Mid, LM_L_l46_Plastics_Mid, LM_F_ll1_Plastics_Mid, LM_F_rl4_Plastics_Mid)
Dim BP_Plastics_Top: BP_Plastics_Top=Array(BM_Plastics_Top, LM_F_fl3_Plastics_Top, LM_F_fr3_Plastics_Top, LM_GI_Plastics_Top, LM_GI_Split_gi009_Plastics_Top)
Dim BP_Playfield: BP_Playfield=Array(BM_Playfield, LM_GI_Split_gi1_Playfield, LM_GI_Split_gi2_Playfield, LM_GI_Split_gi3_Playfield, LM_GI_Split_gi4_Playfield, LM_F_fl2_Playfield, LM_F_fl3_Playfield, LM_F_fr2_Playfield, LM_F_fr3_Playfield, LM_GI_Playfield, LM_GI_Split_gi007_Playfield, LM_GI_Split_gi009_Playfield, LM_L_l10_Playfield, LM_L_l11_Playfield, LM_L_l19_Playfield, LM_L_l2_Playfield, LM_L_l20_Playfield, LM_L_l21_Playfield, LM_L_l22_Playfield, LM_L_l23_Playfield, LM_L_l24_Playfield, LM_L_l25_Playfield, LM_L_l26_Playfield, LM_L_l27_Playfield, LM_LBW_l28_Playfield, LM_LBW_l29_Playfield, LM_L_l3_Playfield, LM_LBW_l33_Playfield, LM_LBW_l34_Playfield, LM_LBW_l35_Playfield, LM_LBW_l36_Playfield, LM_LBW_l37_Playfield, LM_LBW_l38_Playfield, LM_LBW_l41_Playfield, LM_LBW_l42_Playfield, LM_LBW_l43_Playfield, LM_LBW_l44_Playfield, LM_L_l45_Playfield, LM_L_l46_Playfield, LM_L_l47_Playfield, LM_L_l5_Playfield, LM_L_l51_Playfield, LM_L_l6_Playfield, LM_L_l7_Playfield, LM_L_l8_Playfield, LM_L_l9_Playfield, _
	LM_F_ll1_Playfield, LM_F_ll2_Playfield, LM_F_ll3_Playfield, LM_F_ll4_Playfield, LM_F_ll5_Playfield, LM_F_Rl1_Playfield, LM_F_Rl2_Playfield, LM_F_rl3_Playfield, LM_F_rl4_Playfield, LM_F_Rl5_Playfield)
Dim BP_RF: BP_RF=Array(BM_RF, LM_GI_Split_gi007_RF, LM_GI_Split_gi009_RF, LM_L_l3_RF, LM_F_ll1_RF, LM_F_ll3_RF, LM_F_Rl1_RF)
Dim BP_RFU: BP_RFU=Array(BM_RFU, LM_GI_Split_gi009_RFU, LM_L_l3_RFU, LM_F_ll1_RFU, LM_F_ll2_RFU, LM_F_ll3_RFU, LM_F_Rl1_RFU, LM_F_Rl2_RFU)
Dim BP_RLCons: BP_RLCons=Array(BM_RLCons, LM_GI_RLCons, LM_GI_Split_gi007_RLCons, LM_L_l45_RLCons)
Dim BP_RLLib: BP_RLLib=Array(BM_RLLib, LM_GI_RLLib, LM_GI_Split_gi007_RLLib, LM_L_l45_RLLib)
Dim BP_RRCons: BP_RRCons=Array(BM_RRCons, LM_F_fl3_RRCons, LM_GI_RRCons, LM_GI_Split_gi009_RRCons, LM_L_l46_RRCons, LM_F_ll2_RRCons)
Dim BP_RRLib: BP_RRLib=Array(BM_RRLib, LM_F_fl3_RRLib, LM_GI_RRLib, LM_GI_Split_gi009_RRLib, LM_L_l46_RRLib)
Dim BP_RSling: BP_RSling=Array(BM_RSling, LM_F_fl3_RSling, LM_F_fr3_RSling, LM_GI_Split_gi007_RSling, LM_GI_Split_gi009_RSling, LM_F_ll1_RSling, LM_F_ll2_RSling, LM_F_ll3_RSling)
Dim BP_RSling1: BP_RSling1=Array(BM_RSling1, LM_F_fl3_RSling1, LM_F_fr3_RSling1, LM_GI_Split_gi007_RSling1, LM_GI_Split_gi009_RSling1, LM_F_ll1_RSling1, LM_F_ll2_RSling1, LM_F_ll3_RSling1)
Dim BP_RSling2: BP_RSling2=Array(BM_RSling2, LM_F_fl3_RSling2, LM_F_fr3_RSling2, LM_GI_Split_gi007_RSling2, LM_GI_Split_gi009_RSling2, LM_F_ll1_RSling2, LM_F_ll2_RSling2, LM_F_ll5_RSling2)
Dim BP_Ramp_Left: BP_Ramp_Left=Array(BM_Ramp_Left, LM_GI_Split_gi1_Ramp_Left, LM_GI_Split_gi2_Ramp_Left, LM_GI_Split_gi3_Ramp_Left, LM_GI_Split_gi4_Ramp_Left, LM_F_fl2_Ramp_Left, LM_F_fl3_Ramp_Left, LM_F_fr2_Ramp_Left, LM_F_fr3_Ramp_Left, LM_GI_Ramp_Left, LM_L_l19_Ramp_Left, LM_L_l22_Ramp_Left, LM_L_l25_Ramp_Left, LM_LBW_l29_Ramp_Left, LM_LBW_l30_Ramp_Left, LM_LBW_l31_Ramp_Left, LM_LBW_l32_Ramp_Left, LM_F_ll2_Ramp_Left, LM_F_ll3_Ramp_Left, LM_F_ll4_Ramp_Left, LM_F_Rl1_Ramp_Left, LM_F_Rl2_Ramp_Left, LM_F_rl3_Ramp_Left, LM_F_rl4_Ramp_Left)
Dim BP_Ramp_Right: BP_Ramp_Right=Array(BM_Ramp_Right, LM_GI_Split_gi1_Ramp_Right, LM_GI_Split_gi2_Ramp_Right, LM_GI_Split_gi3_Ramp_Right, LM_GI_Split_gi4_Ramp_Right, LM_F_fl2_Ramp_Right, LM_F_fl3_Ramp_Right, LM_F_fr2_Ramp_Right, LM_F_fr3_Ramp_Right, LM_GI_Ramp_Right, LM_LBW_l39_Ramp_Right, LM_LBW_l40_Ramp_Right, LM_LBW_l41_Ramp_Right, LM_LBW_l42_Ramp_Right, LM_LBW_l43_Ramp_Right, LM_LBW_l44_Ramp_Right)
Dim BP_RobotTube1: BP_RobotTube1=Array(BM_RobotTube1, LM_LUPF_l15_RobotTube1, LM_LUPF_l16_RobotTube1, LM_LUPF_l17_RobotTube1, LM_LUPF_l18_RobotTube1, LM_F_ll1_RobotTube1, LM_F_ll2_RobotTube1, LM_F_ll3_RobotTube1, LM_F_ll4_RobotTube1, LM_F_ll5_RobotTube1, LM_F_Rl1_RobotTube1, LM_F_Rl2_RobotTube1, LM_F_rl3_RobotTube1, LM_F_rl4_RobotTube1, LM_F_Rl5_RobotTube1)
Dim BP_RobotTube10: BP_RobotTube10=Array(BM_RobotTube10, LM_LUPF_l15_RobotTube10, LM_LUPF_l17_RobotTube10, LM_F_ll1_RobotTube10, LM_F_ll2_RobotTube10, LM_F_ll3_RobotTube10, LM_F_ll4_RobotTube10, LM_F_ll5_RobotTube10, LM_F_Rl5_RobotTube10)
Dim BP_RobotTube11: BP_RobotTube11=Array(BM_RobotTube11, LM_LUPF_l15_RobotTube11, LM_LUPF_l17_RobotTube11, LM_LUPF_l18_RobotTube11, LM_F_ll1_RobotTube11, LM_F_ll2_RobotTube11, LM_F_ll3_RobotTube11, LM_F_ll4_RobotTube11, LM_F_ll5_RobotTube11, LM_F_Rl1_RobotTube11, LM_F_Rl2_RobotTube11, LM_F_rl3_RobotTube11, LM_F_rl4_RobotTube11, LM_F_Rl5_RobotTube11)
Dim BP_RobotTube12: BP_RobotTube12=Array(BM_RobotTube12, LM_LUPF_l15_RobotTube12, LM_LUPF_l16_RobotTube12, LM_LUPF_l17_RobotTube12, LM_LUPF_l18_RobotTube12, LM_F_ll1_RobotTube12, LM_F_ll2_RobotTube12, LM_F_ll3_RobotTube12, LM_F_ll4_RobotTube12, LM_F_ll5_RobotTube12, LM_F_Rl1_RobotTube12, LM_F_Rl2_RobotTube12, LM_F_rl3_RobotTube12, LM_F_rl4_RobotTube12, LM_F_Rl5_RobotTube12)
Dim BP_RobotTube2: BP_RobotTube2=Array(BM_RobotTube2, LM_F_fr3_RobotTube2, LM_LUPF_l16_RobotTube2, LM_LUPF_l17_RobotTube2, LM_LUPF_l18_RobotTube2, LM_F_ll1_RobotTube2, LM_F_ll2_RobotTube2, LM_F_ll3_RobotTube2, LM_F_ll4_RobotTube2, LM_F_ll5_RobotTube2, LM_F_Rl1_RobotTube2, LM_F_Rl2_RobotTube2, LM_F_rl3_RobotTube2, LM_F_rl4_RobotTube2, LM_F_Rl5_RobotTube2)
Dim BP_RobotTube3: BP_RobotTube3=Array(BM_RobotTube3, LM_LUPF_l16_RobotTube3, LM_LUPF_l18_RobotTube3, LM_F_ll1_RobotTube3, LM_F_ll2_RobotTube3, LM_F_ll3_RobotTube3, LM_F_ll4_RobotTube3, LM_F_ll5_RobotTube3, LM_F_Rl1_RobotTube3, LM_F_Rl2_RobotTube3, LM_F_rl3_RobotTube3, LM_F_rl4_RobotTube3, LM_F_Rl5_RobotTube3)
Dim BP_RobotTube4: BP_RobotTube4=Array(BM_RobotTube4, LM_LUPF_l16_RobotTube4, LM_LUPF_l18_RobotTube4, LM_F_Rl1_RobotTube4, LM_F_Rl2_RobotTube4, LM_F_rl3_RobotTube4, LM_F_rl4_RobotTube4, LM_F_Rl5_RobotTube4)
Dim BP_RobotTube5: BP_RobotTube5=Array(BM_RobotTube5, LM_LUPF_l16_RobotTube5, LM_LUPF_l17_RobotTube5, LM_LUPF_l18_RobotTube5, LM_F_ll2_RobotTube5, LM_F_ll3_RobotTube5, LM_F_ll4_RobotTube5, LM_F_ll5_RobotTube5, LM_F_Rl1_RobotTube5, LM_F_Rl2_RobotTube5, LM_F_rl3_RobotTube5, LM_F_rl4_RobotTube5, LM_F_Rl5_RobotTube5)
Dim BP_RobotTube6: BP_RobotTube6=Array(BM_RobotTube6, LM_LUPF_l16_RobotTube6, LM_LUPF_l17_RobotTube6, LM_LUPF_l18_RobotTube6, LM_F_ll1_RobotTube6, LM_F_ll2_RobotTube6, LM_F_ll3_RobotTube6, LM_F_ll4_RobotTube6, LM_F_ll5_RobotTube6, LM_F_Rl1_RobotTube6, LM_F_Rl2_RobotTube6, LM_F_rl3_RobotTube6, LM_F_rl4_RobotTube6, LM_F_Rl5_RobotTube6)
Dim BP_RobotTube7: BP_RobotTube7=Array(BM_RobotTube7, LM_F_ll1_RobotTube7, LM_F_ll2_RobotTube7, LM_F_ll3_RobotTube7, LM_F_ll4_RobotTube7, LM_F_ll5_RobotTube7, LM_F_Rl1_RobotTube7, LM_F_Rl2_RobotTube7, LM_F_rl3_RobotTube7, LM_F_rl4_RobotTube7, LM_F_Rl5_RobotTube7)
Dim BP_RobotTube8: BP_RobotTube8=Array(BM_RobotTube8, LM_LUPF_l15_RobotTube8, LM_LUPF_l17_RobotTube8, LM_LUPF_l18_RobotTube8, LM_F_ll1_RobotTube8, LM_F_ll2_RobotTube8, LM_F_ll3_RobotTube8, LM_F_ll4_RobotTube8, LM_F_ll5_RobotTube8, LM_F_Rl1_RobotTube8, LM_F_Rl2_RobotTube8, LM_F_rl3_RobotTube8, LM_F_rl4_RobotTube8, LM_F_Rl5_RobotTube8)
Dim BP_RobotTube9: BP_RobotTube9=Array(BM_RobotTube9, LM_LUPF_l15_RobotTube9, LM_LUPF_l16_RobotTube9, LM_LUPF_l17_RobotTube9, LM_LUPF_l18_RobotTube9, LM_F_ll1_RobotTube9, LM_F_ll2_RobotTube9, LM_F_ll3_RobotTube9, LM_F_ll4_RobotTube9, LM_F_ll5_RobotTube9, LM_F_Rl2_RobotTube9, LM_F_rl3_RobotTube9, LM_F_rl4_RobotTube9, LM_F_Rl5_RobotTube9)
Dim BP_SideRails: BP_SideRails=Array(BM_SideRails)
Dim BP_SideRails_001: BP_SideRails_001=Array(BM_SideRails_001)
Dim BP_SlingL: BP_SlingL=Array(BM_SlingL, LM_GI_Split_gi007_SlingL, LM_F_ll3_SlingL, LM_F_ll4_SlingL, LM_F_ll5_SlingL, LM_F_Rl2_SlingL, LM_F_Rl5_SlingL)
Dim BP_SlingR: BP_SlingR=Array(BM_SlingR, LM_GI_Split_gi007_SlingR, LM_GI_Split_gi009_SlingR, LM_F_ll2_SlingR, LM_F_ll3_SlingR, LM_F_ll4_SlingR, LM_F_ll5_SlingR, LM_F_rl3_SlingR, LM_F_rl4_SlingR, LM_F_Rl5_SlingR)
Dim BP_SlingT: BP_SlingT=Array(BM_SlingT)
Dim BP_UnderPF: BP_UnderPF=Array(BM_UnderPF, LM_GI_Split_gi1_UnderPF, LM_GI_Split_gi2_UnderPF, LM_GI_Split_gi3_UnderPF, LM_GI_Split_gi4_UnderPF, LM_F_fl2_UnderPF, LM_F_fl3_UnderPF, LM_F_fr2_UnderPF, LM_F_fr3_UnderPF, LM_GI_UnderPF, LM_GI_Split_gi007_UnderPF, LM_GI_Split_gi009_UnderPF, LM_L_l10_UnderPF, LM_L_l11_UnderPF, LM_LUPF_l15_UnderPF, LM_LUPF_l16_UnderPF, LM_LUPF_l17_UnderPF, LM_LUPF_l18_UnderPF, LM_L_l19_UnderPF, LM_L_l2_UnderPF, LM_L_l20_UnderPF, LM_L_l21_UnderPF, LM_L_l22_UnderPF, LM_L_l23_UnderPF, LM_L_l24_UnderPF, LM_L_l25_UnderPF, LM_L_l26_UnderPF, LM_L_l27_UnderPF, LM_L_l3_UnderPF, LM_LBW_l33_UnderPF, LM_LBW_l35_UnderPF, LM_LBW_l40_UnderPF, LM_LBW_l41_UnderPF, LM_LBW_l42_UnderPF, LM_L_l45_UnderPF, LM_L_l46_UnderPF, LM_L_l47_UnderPF, LM_L_l5_UnderPF, LM_L_l51_UnderPF, LM_L_l6_UnderPF, LM_L_l7_UnderPF, LM_L_l8_UnderPF, LM_L_l9_UnderPF, LM_F_ll1_UnderPF, LM_F_ll2_UnderPF, LM_F_ll3_UnderPF, LM_F_ll4_UnderPF, LM_F_ll5_UnderPF, LM_F_Rl1_UnderPF, LM_F_Rl2_UnderPF, LM_F_rl3_UnderPF, LM_F_rl4_UnderPF, _
	LM_F_Rl5_UnderPF)
Dim BP_VaritargetP: BP_VaritargetP=Array(BM_VaritargetP, LM_GI_Split_gi1_VaritargetP, LM_GI_Split_gi3_VaritargetP, LM_F_fl2_VaritargetP, LM_F_fl3_VaritargetP, LM_F_fr3_VaritargetP, LM_GI_VaritargetP)
Dim BP_sw144: BP_sw144=Array(BM_sw144, LM_GI_Split_gi007_sw144, LM_GI_Split_gi009_sw144, LM_F_rl4_sw144)
Dim BP_sw42: BP_sw42=Array(BM_sw42, LM_F_fl2_sw42, LM_GI_sw42)
Dim BP_sw44: BP_sw44=Array(BM_sw44, LM_GI_Split_gi009_sw44)
Dim BP_sw52: BP_sw52=Array(BM_sw52, LM_GI_sw52)
Dim BP_sw53: BP_sw53=Array(BM_sw53, LM_F_fl2_sw53)
Dim BP_sw54: BP_sw54=Array(BM_sw54, LM_GI_Split_gi007_sw54)
Dim BP_sw62: BP_sw62=Array(BM_sw62, LM_F_fr2_sw62, LM_GI_sw62)
Dim BP_sw64: BP_sw64=Array(BM_sw64, LM_GI_Split_gi009_sw64)
' Arrays per lighting scenario
Dim BL_F_Rl1: BL_F_Rl1=Array(LM_F_Rl1_LF, LM_F_Rl1_LFU, LM_F_Rl1_LSling, LM_F_Rl1_LSling1, LM_F_Rl1_Parts, LM_F_Rl1_Playfield, LM_F_Rl1_RF, LM_F_Rl1_RFU, LM_F_Rl1_Ramp_Left, LM_F_Rl1_RobotTube1, LM_F_Rl1_RobotTube11, LM_F_Rl1_RobotTube12, LM_F_Rl1_RobotTube2, LM_F_Rl1_RobotTube3, LM_F_Rl1_RobotTube4, LM_F_Rl1_RobotTube5, LM_F_Rl1_RobotTube6, LM_F_Rl1_RobotTube7, LM_F_Rl1_RobotTube8, LM_F_Rl1_UnderPF)
Dim BL_F_Rl2: BL_F_Rl2=Array(LM_F_Rl2_KT_sw45, LM_F_Rl2_LF, LM_F_Rl2_LFU, LM_F_Rl2_LSling, LM_F_Rl2_LSling1, LM_F_Rl2_LSling2, LM_F_Rl2_Parts, LM_F_Rl2_Plastics_Low, LM_F_Rl2_Playfield, LM_F_Rl2_RFU, LM_F_Rl2_Ramp_Left, LM_F_Rl2_RobotTube1, LM_F_Rl2_RobotTube11, LM_F_Rl2_RobotTube12, LM_F_Rl2_RobotTube2, LM_F_Rl2_RobotTube3, LM_F_Rl2_RobotTube4, LM_F_Rl2_RobotTube5, LM_F_Rl2_RobotTube6, LM_F_Rl2_RobotTube7, LM_F_Rl2_RobotTube8, LM_F_Rl2_RobotTube9, LM_F_Rl2_SlingL, LM_F_Rl2_UnderPF)
Dim BL_F_Rl5: BL_F_Rl5=Array(LM_F_Rl5_KT_sw45, LM_F_Rl5_LSling1, LM_F_Rl5_PFWindow, LM_F_Rl5_Parts, LM_F_Rl5_Plastics_Low, LM_F_Rl5_Playfield, LM_F_Rl5_RobotTube1, LM_F_Rl5_RobotTube10, LM_F_Rl5_RobotTube11, LM_F_Rl5_RobotTube12, LM_F_Rl5_RobotTube2, LM_F_Rl5_RobotTube3, LM_F_Rl5_RobotTube4, LM_F_Rl5_RobotTube5, LM_F_Rl5_RobotTube6, LM_F_Rl5_RobotTube7, LM_F_Rl5_RobotTube8, LM_F_Rl5_RobotTube9, LM_F_Rl5_SlingL, LM_F_Rl5_SlingR, LM_F_Rl5_UnderPF)
Dim BL_F_fl2: BL_F_fl2=Array(LM_F_fl2_Bumper1_Ring, LM_F_fl2_Bumper1_Skirt, LM_F_fl2_Bumper3_Ring, LM_F_fl2_Bumper3_Skirt, LM_F_fl2_Bumper4_Ring, LM_F_fl2_Bumper4_Skirt, LM_F_fl2_Gate_A_8096___Flap, LM_F_fl2_Gate1_Flap, LM_F_fl2_Gate5_Wire, LM_F_fl2_Parts, LM_F_fl2_Plastics_Low, LM_F_fl2_Playfield, LM_F_fl2_Ramp_Left, LM_F_fl2_Ramp_Right, LM_F_fl2_UnderPF, LM_F_fl2_VaritargetP, LM_F_fl2_sw42, LM_F_fl2_sw53)
Dim BL_F_fl3: BL_F_fl3=Array(LM_F_fl3_Bumper1_Ring, LM_F_fl3_Bumper1_Skirt, LM_F_fl3_Bumper2_Ring, LM_F_fl3_Bumper3_Ring, LM_F_fl3_Bumper3_Skirt, LM_F_fl3_Bumper4_Skirt, LM_F_fl3_CapRLib, LM_F_fl3_DT_sw41, LM_F_fl3_DT_sw51, LM_F_fl3_Gate1_Flap, LM_F_fl3_Gate5_Wire, LM_F_fl3_InlaneRubbers, LM_F_fl3_Parts, LM_F_fl3_Plastics_Low, LM_F_fl3_Plastics_Mid, LM_F_fl3_Plastics_Top, LM_F_fl3_Playfield, LM_F_fl3_RRCons, LM_F_fl3_RRLib, LM_F_fl3_RSling, LM_F_fl3_RSling1, LM_F_fl3_RSling2, LM_F_fl3_Ramp_Left, LM_F_fl3_Ramp_Right, LM_F_fl3_UnderPF, LM_F_fl3_VaritargetP)
Dim BL_F_fr2: BL_F_fr2=Array(LM_F_fr2_Bumper1_Skirt, LM_F_fr2_Bumper2_Ring, LM_F_fr2_Bumper2_Skirt, LM_F_fr2_Bumper3_Skirt, LM_F_fr2_Bumper4_Skirt, LM_F_fr2_Gate_A_8096___Flap, LM_F_fr2_Gate1_Flap, LM_F_fr2_Parts, LM_F_fr2_Plastics_Low, LM_F_fr2_Playfield, LM_F_fr2_Ramp_Left, LM_F_fr2_Ramp_Right, LM_F_fr2_UnderPF, LM_F_fr2_sw62)
Dim BL_F_fr3: BL_F_fr3=Array(LM_F_fr3_Bumper1_Ring, LM_F_fr3_Bumper1_Skirt, LM_F_fr3_Bumper2_Ring, LM_F_fr3_Bumper2_Skirt, LM_F_fr3_Bumper3_Ring, LM_F_fr3_Bumper3_Skirt, LM_F_fr3_Bumper4_Ring, LM_F_fr3_Bumper4_Skirt, LM_F_fr3_DT_sw41, LM_F_fr3_DT_sw51, LM_F_fr3_DT_sw61, LM_F_fr3_Gate1_Flap, LM_F_fr3_Gate2_Wire, LM_F_fr3_Gate5_Wire, LM_F_fr3_KT_sw65, LM_F_fr3_LSling, LM_F_fr3_Parts, LM_F_fr3_Plastics_Low, LM_F_fr3_Plastics_Mid, LM_F_fr3_Plastics_Top, LM_F_fr3_Playfield, LM_F_fr3_RSling, LM_F_fr3_RSling1, LM_F_fr3_RSling2, LM_F_fr3_Ramp_Left, LM_F_fr3_Ramp_Right, LM_F_fr3_RobotTube2, LM_F_fr3_UnderPF, LM_F_fr3_VaritargetP)
Dim BL_F_ll1: BL_F_ll1=Array(LM_F_ll1_LF, LM_F_ll1_LFU, LM_F_ll1_Parts, LM_F_ll1_Plastics_Mid, LM_F_ll1_Playfield, LM_F_ll1_RF, LM_F_ll1_RFU, LM_F_ll1_RSling, LM_F_ll1_RSling1, LM_F_ll1_RSling2, LM_F_ll1_RobotTube1, LM_F_ll1_RobotTube10, LM_F_ll1_RobotTube11, LM_F_ll1_RobotTube12, LM_F_ll1_RobotTube2, LM_F_ll1_RobotTube3, LM_F_ll1_RobotTube6, LM_F_ll1_RobotTube7, LM_F_ll1_RobotTube8, LM_F_ll1_RobotTube9, LM_F_ll1_UnderPF)
Dim BL_F_ll2: BL_F_ll2=Array(LM_F_ll2_DT_sw41, LM_F_ll2_LFU, LM_F_ll2_PRCons, LM_F_ll2_PRLib, LM_F_ll2_Parts, LM_F_ll2_Plastics_Low, LM_F_ll2_Playfield, LM_F_ll2_RFU, LM_F_ll2_RRCons, LM_F_ll2_RSling, LM_F_ll2_RSling1, LM_F_ll2_RSling2, LM_F_ll2_Ramp_Left, LM_F_ll2_RobotTube1, LM_F_ll2_RobotTube10, LM_F_ll2_RobotTube11, LM_F_ll2_RobotTube12, LM_F_ll2_RobotTube2, LM_F_ll2_RobotTube3, LM_F_ll2_RobotTube5, LM_F_ll2_RobotTube6, LM_F_ll2_RobotTube7, LM_F_ll2_RobotTube8, LM_F_ll2_RobotTube9, LM_F_ll2_SlingR, LM_F_ll2_UnderPF)
Dim BL_F_ll3: BL_F_ll3=Array(LM_F_ll3_DT_sw41, LM_F_ll3_DT_sw61, LM_F_ll3_KT_sw65, LM_F_ll3_Parts, LM_F_ll3_Plastics_Low, LM_F_ll3_Playfield, LM_F_ll3_RF, LM_F_ll3_RFU, LM_F_ll3_RSling, LM_F_ll3_RSling1, LM_F_ll3_Ramp_Left, LM_F_ll3_RobotTube1, LM_F_ll3_RobotTube10, LM_F_ll3_RobotTube11, LM_F_ll3_RobotTube12, LM_F_ll3_RobotTube2, LM_F_ll3_RobotTube3, LM_F_ll3_RobotTube5, LM_F_ll3_RobotTube6, LM_F_ll3_RobotTube7, LM_F_ll3_RobotTube8, LM_F_ll3_RobotTube9, LM_F_ll3_SlingL, LM_F_ll3_SlingR, LM_F_ll3_UnderPF)
Dim BL_F_ll4: BL_F_ll4=Array(LM_F_ll4_DT_sw51, LM_F_ll4_DT_sw61, LM_F_ll4_KT_sw65, LM_F_ll4_LSling, LM_F_ll4_PRCons, LM_F_ll4_PRLib, LM_F_ll4_Parts, LM_F_ll4_Plastics_Low, LM_F_ll4_Playfield, LM_F_ll4_Ramp_Left, LM_F_ll4_RobotTube1, LM_F_ll4_RobotTube10, LM_F_ll4_RobotTube11, LM_F_ll4_RobotTube12, LM_F_ll4_RobotTube2, LM_F_ll4_RobotTube3, LM_F_ll4_RobotTube5, LM_F_ll4_RobotTube6, LM_F_ll4_RobotTube7, LM_F_ll4_RobotTube8, LM_F_ll4_RobotTube9, LM_F_ll4_SlingL, LM_F_ll4_SlingR, LM_F_ll4_UnderPF)
Dim BL_F_ll5: BL_F_ll5=Array(LM_F_ll5_KT_sw65, LM_F_ll5_Parts, LM_F_ll5_Playfield, LM_F_ll5_RSling2, LM_F_ll5_RobotTube1, LM_F_ll5_RobotTube10, LM_F_ll5_RobotTube11, LM_F_ll5_RobotTube12, LM_F_ll5_RobotTube2, LM_F_ll5_RobotTube3, LM_F_ll5_RobotTube5, LM_F_ll5_RobotTube6, LM_F_ll5_RobotTube7, LM_F_ll5_RobotTube8, LM_F_ll5_RobotTube9, LM_F_ll5_SlingL, LM_F_ll5_SlingR, LM_F_ll5_UnderPF)
Dim BL_F_rl3: BL_F_rl3=Array(LM_F_rl3_DT_sw41, LM_F_rl3_KT_sw45, LM_F_rl3_LF, LM_F_rl3_LFU, LM_F_rl3_LSling1, LM_F_rl3_Parts, LM_F_rl3_Plastics_Low, LM_F_rl3_Playfield, LM_F_rl3_Ramp_Left, LM_F_rl3_RobotTube1, LM_F_rl3_RobotTube11, LM_F_rl3_RobotTube12, LM_F_rl3_RobotTube2, LM_F_rl3_RobotTube3, LM_F_rl3_RobotTube4, LM_F_rl3_RobotTube5, LM_F_rl3_RobotTube6, LM_F_rl3_RobotTube7, LM_F_rl3_RobotTube8, LM_F_rl3_RobotTube9, LM_F_rl3_SlingR, LM_F_rl3_UnderPF)
Dim BL_F_rl4: BL_F_rl4=Array(LM_F_rl4_DT_sw41, LM_F_rl4_DT_sw61, LM_F_rl4_KT_sw45, LM_F_rl4_Parts, LM_F_rl4_Plastics_Low, LM_F_rl4_Plastics_Mid, LM_F_rl4_Playfield, LM_F_rl4_Ramp_Left, LM_F_rl4_RobotTube1, LM_F_rl4_RobotTube11, LM_F_rl4_RobotTube12, LM_F_rl4_RobotTube2, LM_F_rl4_RobotTube3, LM_F_rl4_RobotTube4, LM_F_rl4_RobotTube5, LM_F_rl4_RobotTube6, LM_F_rl4_RobotTube7, LM_F_rl4_RobotTube8, LM_F_rl4_RobotTube9, LM_F_rl4_SlingR, LM_F_rl4_UnderPF, LM_F_rl4_sw144)
Dim BL_GI: BL_GI=Array(LM_GI_Bumper1_Ring, LM_GI_Bumper1_Skirt, LM_GI_Bumper2_Ring, LM_GI_Bumper2_Skirt, LM_GI_Bumper3_Ring, LM_GI_Bumper3_Skirt, LM_GI_Bumper4_Ring, LM_GI_Bumper4_Skirt, LM_GI_DT_sw41, LM_GI_DT_sw51, LM_GI_DT_sw61, LM_GI_Gate_A_8096___Flap, LM_GI_Gate1_Flap, LM_GI_Gate5_Wire, LM_GI_KT_sw45, LM_GI_KT_sw65, LM_GI_PLCons, LM_GI_PLLib, LM_GI_PRCons, LM_GI_PRLib, LM_GI_Parts, LM_GI_Plastics_Low, LM_GI_Plastics_Mid, LM_GI_Plastics_Top, LM_GI_Playfield, LM_GI_RLCons, LM_GI_RLLib, LM_GI_RRCons, LM_GI_RRLib, LM_GI_Ramp_Left, LM_GI_Ramp_Right, LM_GI_UnderPF, LM_GI_VaritargetP, LM_GI_sw42, LM_GI_sw52, LM_GI_sw62)
Dim BL_GI_Split_gi007: BL_GI_Split_gi007=Array(LM_GI_Split_gi007_DT_sw41, LM_GI_Split_gi007_InlaneRubbers, LM_GI_Split_gi007_KT_sw45, LM_GI_Split_gi007_LF, LM_GI_Split_gi007_LFU, LM_GI_Split_gi007_LSling, LM_GI_Split_gi007_LSling1, LM_GI_Split_gi007_LSling2, LM_GI_Split_gi007_Parts, LM_GI_Split_gi007_Plastics_Low, LM_GI_Split_gi007_Plastics_Mid, LM_GI_Split_gi007_Playfield, LM_GI_Split_gi007_RF, LM_GI_Split_gi007_RLCons, LM_GI_Split_gi007_RLLib, LM_GI_Split_gi007_RSling, LM_GI_Split_gi007_RSling1, LM_GI_Split_gi007_RSling2, LM_GI_Split_gi007_SlingL, LM_GI_Split_gi007_SlingR, LM_GI_Split_gi007_UnderPF, LM_GI_Split_gi007_sw144, LM_GI_Split_gi007_sw54)
Dim BL_GI_Split_gi009: BL_GI_Split_gi009=Array(LM_GI_Split_gi009_InlaneRubbers, LM_GI_Split_gi009_KT_sw65, LM_GI_Split_gi009_LF, LM_GI_Split_gi009_LSling, LM_GI_Split_gi009_LSling1, LM_GI_Split_gi009_LSling2, LM_GI_Split_gi009_Parts, LM_GI_Split_gi009_Plastics_Low, LM_GI_Split_gi009_Plastics_Mid, LM_GI_Split_gi009_Plastics_Top, LM_GI_Split_gi009_Playfield, LM_GI_Split_gi009_RF, LM_GI_Split_gi009_RFU, LM_GI_Split_gi009_RRCons, LM_GI_Split_gi009_RRLib, LM_GI_Split_gi009_RSling, LM_GI_Split_gi009_RSling1, LM_GI_Split_gi009_RSling2, LM_GI_Split_gi009_SlingR, LM_GI_Split_gi009_UnderPF, LM_GI_Split_gi009_sw144, LM_GI_Split_gi009_sw44, LM_GI_Split_gi009_sw64)
Dim BL_GI_Split_gi1: BL_GI_Split_gi1=Array(LM_GI_Split_gi1_Bumper1_Ring, LM_GI_Split_gi1_Bumper1_Skirt, LM_GI_Split_gi1_Bumper2_Ring, LM_GI_Split_gi1_Bumper2_Skirt, LM_GI_Split_gi1_Bumper3_Skirt, LM_GI_Split_gi1_Bumper4_Ring, LM_GI_Split_gi1_Bumper4_Skirt, LM_GI_Split_gi1_Parts, LM_GI_Split_gi1_Plastics_Low, LM_GI_Split_gi1_Playfield, LM_GI_Split_gi1_Ramp_Left, LM_GI_Split_gi1_Ramp_Right, LM_GI_Split_gi1_UnderPF, LM_GI_Split_gi1_VaritargetP)
Dim BL_GI_Split_gi2: BL_GI_Split_gi2=Array(LM_GI_Split_gi2_Bumper1_Skirt, LM_GI_Split_gi2_Bumper2_Ring, LM_GI_Split_gi2_Bumper2_Skirt, LM_GI_Split_gi2_Bumper3_Skirt, LM_GI_Split_gi2_Bumper4_Skirt, LM_GI_Split_gi2_Gate_A_8096___F, LM_GI_Split_gi2_Parts, LM_GI_Split_gi2_Plastics_Low, LM_GI_Split_gi2_Playfield, LM_GI_Split_gi2_Ramp_Left, LM_GI_Split_gi2_Ramp_Right, LM_GI_Split_gi2_UnderPF)
Dim BL_GI_Split_gi3: BL_GI_Split_gi3=Array(LM_GI_Split_gi3_Bumper1_Ring, LM_GI_Split_gi3_Bumper1_Skirt, LM_GI_Split_gi3_Bumper2_Ring, LM_GI_Split_gi3_Bumper2_Skirt, LM_GI_Split_gi3_Bumper3_Ring, LM_GI_Split_gi3_Bumper3_Skirt, LM_GI_Split_gi3_Bumper4_Ring, LM_GI_Split_gi3_Bumper4_Skirt, LM_GI_Split_gi3_KT_sw45, LM_GI_Split_gi3_Parts, LM_GI_Split_gi3_Plastics_Low, LM_GI_Split_gi3_Playfield, LM_GI_Split_gi3_Ramp_Left, LM_GI_Split_gi3_Ramp_Right, LM_GI_Split_gi3_UnderPF, LM_GI_Split_gi3_VaritargetP)
Dim BL_GI_Split_gi4: BL_GI_Split_gi4=Array(LM_GI_Split_gi4_Bumper1_Ring, LM_GI_Split_gi4_Bumper1_Skirt, LM_GI_Split_gi4_Bumper2_Ring, LM_GI_Split_gi4_Bumper2_Skirt, LM_GI_Split_gi4_Bumper3_Skirt, LM_GI_Split_gi4_Bumper4_Ring, LM_GI_Split_gi4_Bumper4_Skirt, LM_GI_Split_gi4_Parts, LM_GI_Split_gi4_Plastics_Low, LM_GI_Split_gi4_Plastics_Mid, LM_GI_Split_gi4_Playfield, LM_GI_Split_gi4_Ramp_Left, LM_GI_Split_gi4_Ramp_Right, LM_GI_Split_gi4_UnderPF)
Dim BL_L_l10: BL_L_l10=Array(LM_L_l10_Playfield, LM_L_l10_UnderPF)
Dim BL_L_l11: BL_L_l11=Array(LM_L_l11_Playfield, LM_L_l11_UnderPF)
Dim BL_L_l19: BL_L_l19=Array(LM_L_l19_Parts, LM_L_l19_Playfield, LM_L_l19_Ramp_Left, LM_L_l19_UnderPF)
Dim BL_L_l2: BL_L_l2=Array(LM_L_l2_LF, LM_L_l2_LFU, LM_L_l2_Parts, LM_L_l2_Playfield, LM_L_l2_UnderPF)
Dim BL_L_l20: BL_L_l20=Array(LM_L_l20_Playfield, LM_L_l20_UnderPF)
Dim BL_L_l21: BL_L_l21=Array(LM_L_l21_DT_sw61, LM_L_l21_Parts, LM_L_l21_Playfield, LM_L_l21_UnderPF)
Dim BL_L_l22: BL_L_l22=Array(LM_L_l22_Parts, LM_L_l22_Playfield, LM_L_l22_Ramp_Left, LM_L_l22_UnderPF)
Dim BL_L_l23: BL_L_l23=Array(LM_L_l23_Parts, LM_L_l23_Playfield, LM_L_l23_UnderPF)
Dim BL_L_l24: BL_L_l24=Array(LM_L_l24_Playfield, LM_L_l24_UnderPF)
Dim BL_L_l25: BL_L_l25=Array(LM_L_l25_Gate_A_8096___Flap, LM_L_l25_Parts, LM_L_l25_Playfield, LM_L_l25_Ramp_Left, LM_L_l25_UnderPF)
Dim BL_L_l26: BL_L_l26=Array(LM_L_l26_Parts, LM_L_l26_Playfield, LM_L_l26_UnderPF)
Dim BL_L_l27: BL_L_l27=Array(LM_L_l27_Gate1_Flap, LM_L_l27_Parts, LM_L_l27_Playfield, LM_L_l27_UnderPF)
Dim BL_L_l3: BL_L_l3=Array(LM_L_l3_Parts, LM_L_l3_Playfield, LM_L_l3_RF, LM_L_l3_RFU, LM_L_l3_UnderPF)
Dim BL_L_l45: BL_L_l45=Array(LM_L_l45_KT_sw45, LM_L_l45_Parts, LM_L_l45_Playfield, LM_L_l45_RLCons, LM_L_l45_RLLib, LM_L_l45_UnderPF)
Dim BL_L_l46: BL_L_l46=Array(LM_L_l46_KT_sw65, LM_L_l46_Parts, LM_L_l46_Plastics_Low, LM_L_l46_Plastics_Mid, LM_L_l46_Playfield, LM_L_l46_RRCons, LM_L_l46_RRLib, LM_L_l46_UnderPF)
Dim BL_L_l47: BL_L_l47=Array(LM_L_l47_InlaneRubbers, LM_L_l47_KT_sw45, LM_L_l47_Parts, LM_L_l47_Playfield, LM_L_l47_UnderPF)
Dim BL_L_l5: BL_L_l5=Array(LM_L_l5_Playfield, LM_L_l5_UnderPF)
Dim BL_L_l51: BL_L_l51=Array(LM_L_l51_InlaneRubbers, LM_L_l51_KT_sw65, LM_L_l51_Parts, LM_L_l51_Playfield, LM_L_l51_UnderPF)
Dim BL_L_l6: BL_L_l6=Array(LM_L_l6_Parts, LM_L_l6_Playfield, LM_L_l6_UnderPF)
Dim BL_L_l7: BL_L_l7=Array(LM_L_l7_Parts, LM_L_l7_Playfield, LM_L_l7_UnderPF)
Dim BL_L_l8: BL_L_l8=Array(LM_L_l8_Parts, LM_L_l8_Playfield, LM_L_l8_UnderPF)
Dim BL_L_l9: BL_L_l9=Array(LM_L_l9_Playfield, LM_L_l9_UnderPF)
Dim BL_LBW_l28: BL_LBW_l28=Array(LM_LBW_l28_Parts, LM_LBW_l28_Plastics_Low, LM_LBW_l28_Playfield)
Dim BL_LBW_l29: BL_LBW_l29=Array(LM_LBW_l29_Parts, LM_LBW_l29_Plastics_Low, LM_LBW_l29_Playfield, LM_LBW_l29_Ramp_Left)
Dim BL_LBW_l30: BL_LBW_l30=Array(LM_LBW_l30_Parts, LM_LBW_l30_Plastics_Low, LM_LBW_l30_Ramp_Left)
Dim BL_LBW_l31: BL_LBW_l31=Array(LM_LBW_l31_Gate_A_8096___Flap, LM_LBW_l31_Parts, LM_LBW_l31_Plastics_Low, LM_LBW_l31_Ramp_Left)
Dim BL_LBW_l32: BL_LBW_l32=Array(LM_LBW_l32_Gate_A_8096___Flap, LM_LBW_l32_Parts, LM_LBW_l32_Plastics_Low, LM_LBW_l32_Ramp_Left)
Dim BL_LBW_l33: BL_LBW_l33=Array(LM_LBW_l33_Gate_A_8096___Flap, LM_LBW_l33_Parts, LM_LBW_l33_Plastics_Low, LM_LBW_l33_Playfield, LM_LBW_l33_UnderPF)
Dim BL_LBW_l34: BL_LBW_l34=Array(LM_LBW_l34_Parts, LM_LBW_l34_Plastics_Low, LM_LBW_l34_Playfield)
Dim BL_LBW_l35: BL_LBW_l35=Array(LM_LBW_l35_Parts, LM_LBW_l35_Plastics_Low, LM_LBW_l35_Playfield, LM_LBW_l35_UnderPF)
Dim BL_LBW_l36: BL_LBW_l36=Array(LM_LBW_l36_Parts, LM_LBW_l36_Plastics_Low, LM_LBW_l36_Playfield)
Dim BL_LBW_l37: BL_LBW_l37=Array(LM_LBW_l37_Parts, LM_LBW_l37_Plastics_Low, LM_LBW_l37_Playfield)
Dim BL_LBW_l38: BL_LBW_l38=Array(LM_LBW_l38_Parts, LM_LBW_l38_Plastics_Low, LM_LBW_l38_Playfield)
Dim BL_LBW_l39: BL_LBW_l39=Array(LM_LBW_l39_Parts, LM_LBW_l39_Plastics_Low, LM_LBW_l39_Ramp_Right)
Dim BL_LBW_l40: BL_LBW_l40=Array(LM_LBW_l40_Gate1_Flap, LM_LBW_l40_Parts, LM_LBW_l40_Plastics_Low, LM_LBW_l40_Ramp_Right, LM_LBW_l40_UnderPF)
Dim BL_LBW_l41: BL_LBW_l41=Array(LM_LBW_l41_Parts, LM_LBW_l41_Plastics_Low, LM_LBW_l41_Playfield, LM_LBW_l41_Ramp_Right, LM_LBW_l41_UnderPF)
Dim BL_LBW_l42: BL_LBW_l42=Array(LM_LBW_l42_Parts, LM_LBW_l42_Plastics_Low, LM_LBW_l42_Playfield, LM_LBW_l42_Ramp_Right, LM_LBW_l42_UnderPF)
Dim BL_LBW_l43: BL_LBW_l43=Array(LM_LBW_l43_Parts, LM_LBW_l43_Plastics_Low, LM_LBW_l43_Playfield, LM_LBW_l43_Ramp_Right)
Dim BL_LBW_l44: BL_LBW_l44=Array(LM_LBW_l44_Parts, LM_LBW_l44_Plastics_Low, LM_LBW_l44_Playfield, LM_LBW_l44_Ramp_Right)
Dim BL_LUPF_l15: BL_LUPF_l15=Array(LM_LUPF_l15_Parts, LM_LUPF_l15_RobotTube1, LM_LUPF_l15_RobotTube10, LM_LUPF_l15_RobotTube11, LM_LUPF_l15_RobotTube12, LM_LUPF_l15_RobotTube8, LM_LUPF_l15_RobotTube9, LM_LUPF_l15_UnderPF)
Dim BL_LUPF_l16: BL_LUPF_l16=Array(LM_LUPF_l16_Parts, LM_LUPF_l16_RobotTube1, LM_LUPF_l16_RobotTube12, LM_LUPF_l16_RobotTube2, LM_LUPF_l16_RobotTube3, LM_LUPF_l16_RobotTube4, LM_LUPF_l16_RobotTube5, LM_LUPF_l16_RobotTube6, LM_LUPF_l16_RobotTube9, LM_LUPF_l16_UnderPF)
Dim BL_LUPF_l17: BL_LUPF_l17=Array(LM_LUPF_l17_Parts, LM_LUPF_l17_RobotTube1, LM_LUPF_l17_RobotTube10, LM_LUPF_l17_RobotTube11, LM_LUPF_l17_RobotTube12, LM_LUPF_l17_RobotTube2, LM_LUPF_l17_RobotTube5, LM_LUPF_l17_RobotTube6, LM_LUPF_l17_RobotTube8, LM_LUPF_l17_RobotTube9, LM_LUPF_l17_UnderPF)
Dim BL_LUPF_l18: BL_LUPF_l18=Array(LM_LUPF_l18_Parts, LM_LUPF_l18_RobotTube1, LM_LUPF_l18_RobotTube11, LM_LUPF_l18_RobotTube12, LM_LUPF_l18_RobotTube2, LM_LUPF_l18_RobotTube3, LM_LUPF_l18_RobotTube4, LM_LUPF_l18_RobotTube5, LM_LUPF_l18_RobotTube6, LM_LUPF_l18_RobotTube8, LM_LUPF_l18_RobotTube9, LM_LUPF_l18_UnderPF)
Dim BL_World: BL_World=Array(BM_Bumper1_Ring, BM_Bumper1_Skirt, BM_Bumper2_Ring, BM_Bumper2_Skirt, BM_Bumper3_Ring, BM_Bumper3_Skirt, BM_Bumper4_Ring, BM_Bumper4_Skirt, BM_CapRCons, BM_CapRLib, BM_DT_sw41, BM_DT_sw51, BM_DT_sw61, BM_Gate_A_8096___Flap, BM_Gate1_Flap, BM_Gate2_Wire, BM_Gate5_Wire, BM_InlaneRubbers, BM_KT_sw45, BM_KT_sw65, BM_LF, BM_LFU, BM_LSling, BM_LSling1, BM_LSling2, BM_LockDownBar, BM_PFWindow, BM_PLCons, BM_PLLib, BM_PRCons, BM_PRLib, BM_Parts, BM_Plastics_Low, BM_Plastics_Mid, BM_Plastics_Top, BM_Playfield, BM_RF, BM_RFU, BM_RLCons, BM_RLLib, BM_RRCons, BM_RRLib, BM_RSling, BM_RSling1, BM_RSling2, BM_Ramp_Left, BM_Ramp_Right, BM_RobotTube1, BM_RobotTube10, BM_RobotTube11, BM_RobotTube12, BM_RobotTube2, BM_RobotTube3, BM_RobotTube4, BM_RobotTube5, BM_RobotTube6, BM_RobotTube7, BM_RobotTube8, BM_RobotTube9, BM_SideRails, BM_SideRails_001, BM_SlingL, BM_SlingR, BM_SlingT, BM_UnderPF, BM_VaritargetP, BM_sw144, BM_sw42, BM_sw44, BM_sw52, BM_sw53, BM_sw54, BM_sw62, BM_sw64)
' Global arrays
Dim BG_Bakemap: BG_Bakemap=Array(BM_Bumper1_Ring, BM_Bumper1_Skirt, BM_Bumper2_Ring, BM_Bumper2_Skirt, BM_Bumper3_Ring, BM_Bumper3_Skirt, BM_Bumper4_Ring, BM_Bumper4_Skirt, BM_CapRCons, BM_CapRLib, BM_DT_sw41, BM_DT_sw51, BM_DT_sw61, BM_Gate_A_8096___Flap, BM_Gate1_Flap, BM_Gate2_Wire, BM_Gate5_Wire, BM_InlaneRubbers, BM_KT_sw45, BM_KT_sw65, BM_LF, BM_LFU, BM_LSling, BM_LSling1, BM_LSling2, BM_LockDownBar, BM_PFWindow, BM_PLCons, BM_PLLib, BM_PRCons, BM_PRLib, BM_Parts, BM_Plastics_Low, BM_Plastics_Mid, BM_Plastics_Top, BM_Playfield, BM_RF, BM_RFU, BM_RLCons, BM_RLLib, BM_RRCons, BM_RRLib, BM_RSling, BM_RSling1, BM_RSling2, BM_Ramp_Left, BM_Ramp_Right, BM_RobotTube1, BM_RobotTube10, BM_RobotTube11, BM_RobotTube12, BM_RobotTube2, BM_RobotTube3, BM_RobotTube4, BM_RobotTube5, BM_RobotTube6, BM_RobotTube7, BM_RobotTube8, BM_RobotTube9, BM_SideRails, BM_SideRails_001, BM_SlingL, BM_SlingR, BM_SlingT, BM_UnderPF, BM_VaritargetP, BM_sw144, BM_sw42, BM_sw44, BM_sw52, BM_sw53, BM_sw54, BM_sw62, BM_sw64)
Dim BG_Lightmap: BG_Lightmap=Array(LM_F_Rl1_LF, LM_F_Rl1_LFU, LM_F_Rl1_LSling, LM_F_Rl1_LSling1, LM_F_Rl1_Parts, LM_F_Rl1_Playfield, LM_F_Rl1_RF, LM_F_Rl1_RFU, LM_F_Rl1_Ramp_Left, LM_F_Rl1_RobotTube1, LM_F_Rl1_RobotTube11, LM_F_Rl1_RobotTube12, LM_F_Rl1_RobotTube2, LM_F_Rl1_RobotTube3, LM_F_Rl1_RobotTube4, LM_F_Rl1_RobotTube5, LM_F_Rl1_RobotTube6, LM_F_Rl1_RobotTube7, LM_F_Rl1_RobotTube8, LM_F_Rl1_UnderPF, LM_F_Rl2_KT_sw45, LM_F_Rl2_LF, LM_F_Rl2_LFU, LM_F_Rl2_LSling, LM_F_Rl2_LSling1, LM_F_Rl2_LSling2, LM_F_Rl2_Parts, LM_F_Rl2_Plastics_Low, LM_F_Rl2_Playfield, LM_F_Rl2_RFU, LM_F_Rl2_Ramp_Left, LM_F_Rl2_RobotTube1, LM_F_Rl2_RobotTube11, LM_F_Rl2_RobotTube12, LM_F_Rl2_RobotTube2, LM_F_Rl2_RobotTube3, LM_F_Rl2_RobotTube4, LM_F_Rl2_RobotTube5, LM_F_Rl2_RobotTube6, LM_F_Rl2_RobotTube7, LM_F_Rl2_RobotTube8, LM_F_Rl2_RobotTube9, LM_F_Rl2_SlingL, LM_F_Rl2_UnderPF, LM_F_Rl5_KT_sw45, LM_F_Rl5_LSling1, LM_F_Rl5_PFWindow, LM_F_Rl5_Parts, LM_F_Rl5_Plastics_Low, LM_F_Rl5_Playfield, LM_F_Rl5_RobotTube1, _
	LM_F_Rl5_RobotTube10, LM_F_Rl5_RobotTube11, LM_F_Rl5_RobotTube12, LM_F_Rl5_RobotTube2, LM_F_Rl5_RobotTube3, LM_F_Rl5_RobotTube4, LM_F_Rl5_RobotTube5, LM_F_Rl5_RobotTube6, LM_F_Rl5_RobotTube7, LM_F_Rl5_RobotTube8, LM_F_Rl5_RobotTube9, LM_F_Rl5_SlingL, LM_F_Rl5_SlingR, LM_F_Rl5_UnderPF, LM_F_fl2_Bumper1_Ring, LM_F_fl2_Bumper1_Skirt, LM_F_fl2_Bumper3_Ring, LM_F_fl2_Bumper3_Skirt, LM_F_fl2_Bumper4_Ring, LM_F_fl2_Bumper4_Skirt, LM_F_fl2_Gate_A_8096___Flap, LM_F_fl2_Gate1_Flap, LM_F_fl2_Gate5_Wire, LM_F_fl2_Parts, LM_F_fl2_Plastics_Low, LM_F_fl2_Playfield, LM_F_fl2_Ramp_Left, LM_F_fl2_Ramp_Right, LM_F_fl2_UnderPF, LM_F_fl2_VaritargetP, LM_F_fl2_sw42, LM_F_fl2_sw53, LM_F_fl3_Bumper1_Ring, LM_F_fl3_Bumper1_Skirt, LM_F_fl3_Bumper2_Ring, LM_F_fl3_Bumper3_Ring, LM_F_fl3_Bumper3_Skirt, LM_F_fl3_Bumper4_Skirt, LM_F_fl3_CapRLib, LM_F_fl3_DT_sw41, LM_F_fl3_DT_sw51, LM_F_fl3_Gate1_Flap, LM_F_fl3_Gate5_Wire, LM_F_fl3_InlaneRubbers, LM_F_fl3_Parts, LM_F_fl3_Plastics_Low, LM_F_fl3_Plastics_Mid, LM_F_fl3_Plastics_Top, _
	LM_F_fl3_Playfield, LM_F_fl3_RRCons, LM_F_fl3_RRLib, LM_F_fl3_RSling, LM_F_fl3_RSling1, LM_F_fl3_RSling2, LM_F_fl3_Ramp_Left, LM_F_fl3_Ramp_Right, LM_F_fl3_UnderPF, LM_F_fl3_VaritargetP, LM_F_fr2_Bumper1_Skirt, LM_F_fr2_Bumper2_Ring, LM_F_fr2_Bumper2_Skirt, LM_F_fr2_Bumper3_Skirt, LM_F_fr2_Bumper4_Skirt, LM_F_fr2_Gate_A_8096___Flap, LM_F_fr2_Gate1_Flap, LM_F_fr2_Parts, LM_F_fr2_Plastics_Low, LM_F_fr2_Playfield, LM_F_fr2_Ramp_Left, LM_F_fr2_Ramp_Right, LM_F_fr2_UnderPF, LM_F_fr2_sw62, LM_F_fr3_Bumper1_Ring, LM_F_fr3_Bumper1_Skirt, LM_F_fr3_Bumper2_Ring, LM_F_fr3_Bumper2_Skirt, LM_F_fr3_Bumper3_Ring, LM_F_fr3_Bumper3_Skirt, LM_F_fr3_Bumper4_Ring, LM_F_fr3_Bumper4_Skirt, LM_F_fr3_DT_sw41, LM_F_fr3_DT_sw51, LM_F_fr3_DT_sw61, LM_F_fr3_Gate1_Flap, LM_F_fr3_Gate2_Wire, LM_F_fr3_Gate5_Wire, LM_F_fr3_KT_sw65, LM_F_fr3_LSling, LM_F_fr3_Parts, LM_F_fr3_Plastics_Low, LM_F_fr3_Plastics_Mid, LM_F_fr3_Plastics_Top, LM_F_fr3_Playfield, LM_F_fr3_RSling, LM_F_fr3_RSling1, LM_F_fr3_RSling2, LM_F_fr3_Ramp_Left, _
	LM_F_fr3_Ramp_Right, LM_F_fr3_RobotTube2, LM_F_fr3_UnderPF, LM_F_fr3_VaritargetP, LM_F_ll1_LF, LM_F_ll1_LFU, LM_F_ll1_Parts, LM_F_ll1_Plastics_Mid, LM_F_ll1_Playfield, LM_F_ll1_RF, LM_F_ll1_RFU, LM_F_ll1_RSling, LM_F_ll1_RSling1, LM_F_ll1_RSling2, LM_F_ll1_RobotTube1, LM_F_ll1_RobotTube10, LM_F_ll1_RobotTube11, LM_F_ll1_RobotTube12, LM_F_ll1_RobotTube2, LM_F_ll1_RobotTube3, LM_F_ll1_RobotTube6, LM_F_ll1_RobotTube7, LM_F_ll1_RobotTube8, LM_F_ll1_RobotTube9, LM_F_ll1_UnderPF, LM_F_ll2_DT_sw41, LM_F_ll2_LFU, LM_F_ll2_PRCons, LM_F_ll2_PRLib, LM_F_ll2_Parts, LM_F_ll2_Plastics_Low, LM_F_ll2_Playfield, LM_F_ll2_RFU, LM_F_ll2_RRCons, LM_F_ll2_RSling, LM_F_ll2_RSling1, LM_F_ll2_RSling2, LM_F_ll2_Ramp_Left, LM_F_ll2_RobotTube1, LM_F_ll2_RobotTube10, LM_F_ll2_RobotTube11, LM_F_ll2_RobotTube12, LM_F_ll2_RobotTube2, LM_F_ll2_RobotTube3, LM_F_ll2_RobotTube5, LM_F_ll2_RobotTube6, LM_F_ll2_RobotTube7, LM_F_ll2_RobotTube8, LM_F_ll2_RobotTube9, LM_F_ll2_SlingR, LM_F_ll2_UnderPF, LM_F_ll3_DT_sw41, LM_F_ll3_DT_sw61, _
	LM_F_ll3_KT_sw65, LM_F_ll3_Parts, LM_F_ll3_Plastics_Low, LM_F_ll3_Playfield, LM_F_ll3_RF, LM_F_ll3_RFU, LM_F_ll3_RSling, LM_F_ll3_RSling1, LM_F_ll3_Ramp_Left, LM_F_ll3_RobotTube1, LM_F_ll3_RobotTube10, LM_F_ll3_RobotTube11, LM_F_ll3_RobotTube12, LM_F_ll3_RobotTube2, LM_F_ll3_RobotTube3, LM_F_ll3_RobotTube5, LM_F_ll3_RobotTube6, LM_F_ll3_RobotTube7, LM_F_ll3_RobotTube8, LM_F_ll3_RobotTube9, LM_F_ll3_SlingL, LM_F_ll3_SlingR, LM_F_ll3_UnderPF, LM_F_ll4_DT_sw51, LM_F_ll4_DT_sw61, LM_F_ll4_KT_sw65, LM_F_ll4_LSling, LM_F_ll4_PRCons, LM_F_ll4_PRLib, LM_F_ll4_Parts, LM_F_ll4_Plastics_Low, LM_F_ll4_Playfield, LM_F_ll4_Ramp_Left, LM_F_ll4_RobotTube1, LM_F_ll4_RobotTube10, LM_F_ll4_RobotTube11, LM_F_ll4_RobotTube12, LM_F_ll4_RobotTube2, LM_F_ll4_RobotTube3, LM_F_ll4_RobotTube5, LM_F_ll4_RobotTube6, LM_F_ll4_RobotTube7, LM_F_ll4_RobotTube8, LM_F_ll4_RobotTube9, LM_F_ll4_SlingL, LM_F_ll4_SlingR, LM_F_ll4_UnderPF, LM_F_ll5_KT_sw65, LM_F_ll5_Parts, LM_F_ll5_Playfield, LM_F_ll5_RSling2, LM_F_ll5_RobotTube1, _
	LM_F_ll5_RobotTube10, LM_F_ll5_RobotTube11, LM_F_ll5_RobotTube12, LM_F_ll5_RobotTube2, LM_F_ll5_RobotTube3, LM_F_ll5_RobotTube5, LM_F_ll5_RobotTube6, LM_F_ll5_RobotTube7, LM_F_ll5_RobotTube8, LM_F_ll5_RobotTube9, LM_F_ll5_SlingL, LM_F_ll5_SlingR, LM_F_ll5_UnderPF, LM_F_rl3_DT_sw41, LM_F_rl3_KT_sw45, LM_F_rl3_LF, LM_F_rl3_LFU, LM_F_rl3_LSling1, LM_F_rl3_Parts, LM_F_rl3_Plastics_Low, LM_F_rl3_Playfield, LM_F_rl3_Ramp_Left, LM_F_rl3_RobotTube1, LM_F_rl3_RobotTube11, LM_F_rl3_RobotTube12, LM_F_rl3_RobotTube2, LM_F_rl3_RobotTube3, LM_F_rl3_RobotTube4, LM_F_rl3_RobotTube5, LM_F_rl3_RobotTube6, LM_F_rl3_RobotTube7, LM_F_rl3_RobotTube8, LM_F_rl3_RobotTube9, LM_F_rl3_SlingR, LM_F_rl3_UnderPF, LM_F_rl4_DT_sw41, LM_F_rl4_DT_sw61, LM_F_rl4_KT_sw45, LM_F_rl4_Parts, LM_F_rl4_Plastics_Low, LM_F_rl4_Plastics_Mid, LM_F_rl4_Playfield, LM_F_rl4_Ramp_Left, LM_F_rl4_RobotTube1, LM_F_rl4_RobotTube11, LM_F_rl4_RobotTube12, LM_F_rl4_RobotTube2, LM_F_rl4_RobotTube3, LM_F_rl4_RobotTube4, LM_F_rl4_RobotTube5, LM_F_rl4_RobotTube6, _
	LM_F_rl4_RobotTube7, LM_F_rl4_RobotTube8, LM_F_rl4_RobotTube9, LM_F_rl4_SlingR, LM_F_rl4_UnderPF, LM_F_rl4_sw144, LM_GI_Split_gi007_DT_sw41, LM_GI_Split_gi007_InlaneRubbers, LM_GI_Split_gi007_KT_sw45, LM_GI_Split_gi007_LF, LM_GI_Split_gi007_LFU, LM_GI_Split_gi007_LSling, LM_GI_Split_gi007_LSling1, LM_GI_Split_gi007_LSling2, LM_GI_Split_gi007_Parts, LM_GI_Split_gi007_Plastics_Low, LM_GI_Split_gi007_Plastics_Mid, LM_GI_Split_gi007_Playfield, LM_GI_Split_gi007_RF, LM_GI_Split_gi007_RLCons, LM_GI_Split_gi007_RLLib, LM_GI_Split_gi007_RSling, LM_GI_Split_gi007_RSling1, LM_GI_Split_gi007_RSling2, LM_GI_Split_gi007_SlingL, LM_GI_Split_gi007_SlingR, LM_GI_Split_gi007_UnderPF, LM_GI_Split_gi007_sw144, LM_GI_Split_gi007_sw54, LM_GI_Split_gi009_InlaneRubbers, LM_GI_Split_gi009_KT_sw65, LM_GI_Split_gi009_LF, LM_GI_Split_gi009_LSling, LM_GI_Split_gi009_LSling1, LM_GI_Split_gi009_LSling2, LM_GI_Split_gi009_Parts, LM_GI_Split_gi009_Plastics_Low, LM_GI_Split_gi009_Plastics_Mid, LM_GI_Split_gi009_Plastics_Top, _
	LM_GI_Split_gi009_Playfield, LM_GI_Split_gi009_RF, LM_GI_Split_gi009_RFU, LM_GI_Split_gi009_RRCons, LM_GI_Split_gi009_RRLib, LM_GI_Split_gi009_RSling, LM_GI_Split_gi009_RSling1, LM_GI_Split_gi009_RSling2, LM_GI_Split_gi009_SlingR, LM_GI_Split_gi009_UnderPF, LM_GI_Split_gi009_sw144, LM_GI_Split_gi009_sw44, LM_GI_Split_gi009_sw64, LM_GI_Split_gi1_Bumper1_Ring, LM_GI_Split_gi1_Bumper1_Skirt, LM_GI_Split_gi1_Bumper2_Ring, LM_GI_Split_gi1_Bumper2_Skirt, LM_GI_Split_gi1_Bumper3_Skirt, LM_GI_Split_gi1_Bumper4_Ring, LM_GI_Split_gi1_Bumper4_Skirt, LM_GI_Split_gi1_Parts, LM_GI_Split_gi1_Plastics_Low, LM_GI_Split_gi1_Playfield, LM_GI_Split_gi1_Ramp_Left, LM_GI_Split_gi1_Ramp_Right, LM_GI_Split_gi1_UnderPF, LM_GI_Split_gi1_VaritargetP, LM_GI_Split_gi2_Bumper1_Skirt, LM_GI_Split_gi2_Bumper2_Ring, LM_GI_Split_gi2_Bumper2_Skirt, LM_GI_Split_gi2_Bumper3_Skirt, LM_GI_Split_gi2_Bumper4_Skirt, LM_GI_Split_gi2_Gate_A_8096___F, LM_GI_Split_gi2_Parts, LM_GI_Split_gi2_Plastics_Low, LM_GI_Split_gi2_Playfield, _
	LM_GI_Split_gi2_Ramp_Left, LM_GI_Split_gi2_Ramp_Right, LM_GI_Split_gi2_UnderPF, LM_GI_Split_gi3_Bumper1_Ring, LM_GI_Split_gi3_Bumper1_Skirt, LM_GI_Split_gi3_Bumper2_Ring, LM_GI_Split_gi3_Bumper2_Skirt, LM_GI_Split_gi3_Bumper3_Ring, LM_GI_Split_gi3_Bumper3_Skirt, LM_GI_Split_gi3_Bumper4_Ring, LM_GI_Split_gi3_Bumper4_Skirt, LM_GI_Split_gi3_KT_sw45, LM_GI_Split_gi3_Parts, LM_GI_Split_gi3_Plastics_Low, LM_GI_Split_gi3_Playfield, LM_GI_Split_gi3_Ramp_Left, LM_GI_Split_gi3_Ramp_Right, LM_GI_Split_gi3_UnderPF, LM_GI_Split_gi3_VaritargetP, LM_GI_Split_gi4_Bumper1_Ring, LM_GI_Split_gi4_Bumper1_Skirt, LM_GI_Split_gi4_Bumper2_Ring, LM_GI_Split_gi4_Bumper2_Skirt, LM_GI_Split_gi4_Bumper3_Skirt, LM_GI_Split_gi4_Bumper4_Ring, LM_GI_Split_gi4_Bumper4_Skirt, LM_GI_Split_gi4_Parts, LM_GI_Split_gi4_Plastics_Low, LM_GI_Split_gi4_Plastics_Mid, LM_GI_Split_gi4_Playfield, LM_GI_Split_gi4_Ramp_Left, LM_GI_Split_gi4_Ramp_Right, LM_GI_Split_gi4_UnderPF, LM_GI_Bumper1_Ring, LM_GI_Bumper1_Skirt, LM_GI_Bumper2_Ring, LM_GI_Bumper2_Skirt, _
	LM_GI_Bumper3_Ring, LM_GI_Bumper3_Skirt, LM_GI_Bumper4_Ring, LM_GI_Bumper4_Skirt, LM_GI_DT_sw41, LM_GI_DT_sw51, LM_GI_DT_sw61, LM_GI_Gate_A_8096___Flap, LM_GI_Gate1_Flap, LM_GI_Gate5_Wire, LM_GI_KT_sw45, LM_GI_KT_sw65, LM_GI_PLCons, LM_GI_PLLib, LM_GI_PRCons, LM_GI_PRLib, LM_GI_Parts, LM_GI_Plastics_Low, LM_GI_Plastics_Mid, LM_GI_Plastics_Top, LM_GI_Playfield, LM_GI_RLCons, LM_GI_RLLib, LM_GI_RRCons, LM_GI_RRLib, LM_GI_Ramp_Left, LM_GI_Ramp_Right, LM_GI_UnderPF, LM_GI_VaritargetP, LM_GI_sw42, LM_GI_sw52, LM_GI_sw62, LM_L_l10_Playfield, LM_L_l10_UnderPF, LM_L_l11_Playfield, LM_L_l11_UnderPF, LM_L_l19_Parts, LM_L_l19_Playfield, LM_L_l19_Ramp_Left, LM_L_l19_UnderPF, LM_L_l2_LF, LM_L_l2_LFU, LM_L_l2_Parts, LM_L_l2_Playfield, LM_L_l2_UnderPF, LM_L_l20_Playfield, LM_L_l20_UnderPF, LM_L_l21_DT_sw61, LM_L_l21_Parts, LM_L_l21_Playfield, LM_L_l21_UnderPF, LM_L_l22_Parts, LM_L_l22_Playfield, LM_L_l22_Ramp_Left, LM_L_l22_UnderPF, LM_L_l23_Parts, LM_L_l23_Playfield, LM_L_l23_UnderPF, LM_L_l24_Playfield, LM_L_l24_UnderPF, _
	LM_L_l25_Gate_A_8096___Flap, LM_L_l25_Parts, LM_L_l25_Playfield, LM_L_l25_Ramp_Left, LM_L_l25_UnderPF, LM_L_l26_Parts, LM_L_l26_Playfield, LM_L_l26_UnderPF, LM_L_l27_Gate1_Flap, LM_L_l27_Parts, LM_L_l27_Playfield, LM_L_l27_UnderPF, LM_L_l3_Parts, LM_L_l3_Playfield, LM_L_l3_RF, LM_L_l3_RFU, LM_L_l3_UnderPF, LM_L_l45_KT_sw45, LM_L_l45_Parts, LM_L_l45_Playfield, LM_L_l45_RLCons, LM_L_l45_RLLib, LM_L_l45_UnderPF, LM_L_l46_KT_sw65, LM_L_l46_Parts, LM_L_l46_Plastics_Low, LM_L_l46_Plastics_Mid, LM_L_l46_Playfield, LM_L_l46_RRCons, LM_L_l46_RRLib, LM_L_l46_UnderPF, LM_L_l47_InlaneRubbers, LM_L_l47_KT_sw45, LM_L_l47_Parts, LM_L_l47_Playfield, LM_L_l47_UnderPF, LM_L_l5_Playfield, LM_L_l5_UnderPF, LM_L_l51_InlaneRubbers, LM_L_l51_KT_sw65, LM_L_l51_Parts, LM_L_l51_Playfield, LM_L_l51_UnderPF, LM_L_l6_Parts, LM_L_l6_Playfield, LM_L_l6_UnderPF, LM_L_l7_Parts, LM_L_l7_Playfield, LM_L_l7_UnderPF, LM_L_l8_Parts, LM_L_l8_Playfield, LM_L_l8_UnderPF, LM_L_l9_Playfield, LM_L_l9_UnderPF, LM_LBW_l28_Parts, LM_LBW_l28_Plastics_Low, _
	LM_LBW_l28_Playfield, LM_LBW_l29_Parts, LM_LBW_l29_Plastics_Low, LM_LBW_l29_Playfield, LM_LBW_l29_Ramp_Left, LM_LBW_l30_Parts, LM_LBW_l30_Plastics_Low, LM_LBW_l30_Ramp_Left, LM_LBW_l31_Gate_A_8096___Flap, LM_LBW_l31_Parts, LM_LBW_l31_Plastics_Low, LM_LBW_l31_Ramp_Left, LM_LBW_l32_Gate_A_8096___Flap, LM_LBW_l32_Parts, LM_LBW_l32_Plastics_Low, LM_LBW_l32_Ramp_Left, LM_LBW_l33_Gate_A_8096___Flap, LM_LBW_l33_Parts, LM_LBW_l33_Plastics_Low, LM_LBW_l33_Playfield, LM_LBW_l33_UnderPF, LM_LBW_l34_Parts, LM_LBW_l34_Plastics_Low, LM_LBW_l34_Playfield, LM_LBW_l35_Parts, LM_LBW_l35_Plastics_Low, LM_LBW_l35_Playfield, LM_LBW_l35_UnderPF, LM_LBW_l36_Parts, LM_LBW_l36_Plastics_Low, LM_LBW_l36_Playfield, LM_LBW_l37_Parts, LM_LBW_l37_Plastics_Low, LM_LBW_l37_Playfield, LM_LBW_l38_Parts, LM_LBW_l38_Plastics_Low, LM_LBW_l38_Playfield, LM_LBW_l39_Parts, LM_LBW_l39_Plastics_Low, LM_LBW_l39_Ramp_Right, LM_LBW_l40_Gate1_Flap, LM_LBW_l40_Parts, LM_LBW_l40_Plastics_Low, LM_LBW_l40_Ramp_Right, LM_LBW_l40_UnderPF, LM_LBW_l41_Parts, _
	LM_LBW_l41_Plastics_Low, LM_LBW_l41_Playfield, LM_LBW_l41_Ramp_Right, LM_LBW_l41_UnderPF, LM_LBW_l42_Parts, LM_LBW_l42_Plastics_Low, LM_LBW_l42_Playfield, LM_LBW_l42_Ramp_Right, LM_LBW_l42_UnderPF, LM_LBW_l43_Parts, LM_LBW_l43_Plastics_Low, LM_LBW_l43_Playfield, LM_LBW_l43_Ramp_Right, LM_LBW_l44_Parts, LM_LBW_l44_Plastics_Low, LM_LBW_l44_Playfield, LM_LBW_l44_Ramp_Right, LM_LUPF_l15_Parts, LM_LUPF_l15_RobotTube1, LM_LUPF_l15_RobotTube10, LM_LUPF_l15_RobotTube11, LM_LUPF_l15_RobotTube12, LM_LUPF_l15_RobotTube8, LM_LUPF_l15_RobotTube9, LM_LUPF_l15_UnderPF, LM_LUPF_l16_Parts, LM_LUPF_l16_RobotTube1, LM_LUPF_l16_RobotTube12, LM_LUPF_l16_RobotTube2, LM_LUPF_l16_RobotTube3, LM_LUPF_l16_RobotTube4, LM_LUPF_l16_RobotTube5, LM_LUPF_l16_RobotTube6, LM_LUPF_l16_RobotTube9, LM_LUPF_l16_UnderPF, LM_LUPF_l17_Parts, LM_LUPF_l17_RobotTube1, LM_LUPF_l17_RobotTube10, LM_LUPF_l17_RobotTube11, LM_LUPF_l17_RobotTube12, LM_LUPF_l17_RobotTube2, LM_LUPF_l17_RobotTube5, LM_LUPF_l17_RobotTube6, LM_LUPF_l17_RobotTube8, _
	LM_LUPF_l17_RobotTube9, LM_LUPF_l17_UnderPF, LM_LUPF_l18_Parts, LM_LUPF_l18_RobotTube1, LM_LUPF_l18_RobotTube11, LM_LUPF_l18_RobotTube12, LM_LUPF_l18_RobotTube2, LM_LUPF_l18_RobotTube3, LM_LUPF_l18_RobotTube4, LM_LUPF_l18_RobotTube5, LM_LUPF_l18_RobotTube6, LM_LUPF_l18_RobotTube8, LM_LUPF_l18_RobotTube9, LM_LUPF_l18_UnderPF)
Dim BG_All: BG_All=Array(BM_Bumper1_Ring, BM_Bumper1_Skirt, BM_Bumper2_Ring, BM_Bumper2_Skirt, BM_Bumper3_Ring, BM_Bumper3_Skirt, BM_Bumper4_Ring, BM_Bumper4_Skirt, BM_CapRCons, BM_CapRLib, BM_DT_sw41, BM_DT_sw51, BM_DT_sw61, BM_Gate_A_8096___Flap, BM_Gate1_Flap, BM_Gate2_Wire, BM_Gate5_Wire, BM_InlaneRubbers, BM_KT_sw45, BM_KT_sw65, BM_LF, BM_LFU, BM_LSling, BM_LSling1, BM_LSling2, BM_LockDownBar, BM_PFWindow, BM_PLCons, BM_PLLib, BM_PRCons, BM_PRLib, BM_Parts, BM_Plastics_Low, BM_Plastics_Mid, BM_Plastics_Top, BM_Playfield, BM_RF, BM_RFU, BM_RLCons, BM_RLLib, BM_RRCons, BM_RRLib, BM_RSling, BM_RSling1, BM_RSling2, BM_Ramp_Left, BM_Ramp_Right, BM_RobotTube1, BM_RobotTube10, BM_RobotTube11, BM_RobotTube12, BM_RobotTube2, BM_RobotTube3, BM_RobotTube4, BM_RobotTube5, BM_RobotTube6, BM_RobotTube7, BM_RobotTube8, BM_RobotTube9, BM_SideRails, BM_SideRails_001, BM_SlingL, BM_SlingR, BM_SlingT, BM_UnderPF, BM_VaritargetP, BM_sw144, BM_sw42, BM_sw44, BM_sw52, BM_sw53, BM_sw54, BM_sw62, BM_sw64, LM_F_Rl1_LF, _
	LM_F_Rl1_LFU, LM_F_Rl1_LSling, LM_F_Rl1_LSling1, LM_F_Rl1_Parts, LM_F_Rl1_Playfield, LM_F_Rl1_RF, LM_F_Rl1_RFU, LM_F_Rl1_Ramp_Left, LM_F_Rl1_RobotTube1, LM_F_Rl1_RobotTube11, LM_F_Rl1_RobotTube12, LM_F_Rl1_RobotTube2, LM_F_Rl1_RobotTube3, LM_F_Rl1_RobotTube4, LM_F_Rl1_RobotTube5, LM_F_Rl1_RobotTube6, LM_F_Rl1_RobotTube7, LM_F_Rl1_RobotTube8, LM_F_Rl1_UnderPF, LM_F_Rl2_KT_sw45, LM_F_Rl2_LF, LM_F_Rl2_LFU, LM_F_Rl2_LSling, LM_F_Rl2_LSling1, LM_F_Rl2_LSling2, LM_F_Rl2_Parts, LM_F_Rl2_Plastics_Low, LM_F_Rl2_Playfield, LM_F_Rl2_RFU, LM_F_Rl2_Ramp_Left, LM_F_Rl2_RobotTube1, LM_F_Rl2_RobotTube11, LM_F_Rl2_RobotTube12, LM_F_Rl2_RobotTube2, LM_F_Rl2_RobotTube3, LM_F_Rl2_RobotTube4, LM_F_Rl2_RobotTube5, LM_F_Rl2_RobotTube6, LM_F_Rl2_RobotTube7, LM_F_Rl2_RobotTube8, LM_F_Rl2_RobotTube9, LM_F_Rl2_SlingL, LM_F_Rl2_UnderPF, LM_F_Rl5_KT_sw45, LM_F_Rl5_LSling1, LM_F_Rl5_PFWindow, LM_F_Rl5_Parts, LM_F_Rl5_Plastics_Low, LM_F_Rl5_Playfield, LM_F_Rl5_RobotTube1, LM_F_Rl5_RobotTube10, LM_F_Rl5_RobotTube11, LM_F_Rl5_RobotTube12, _
	LM_F_Rl5_RobotTube2, LM_F_Rl5_RobotTube3, LM_F_Rl5_RobotTube4, LM_F_Rl5_RobotTube5, LM_F_Rl5_RobotTube6, LM_F_Rl5_RobotTube7, LM_F_Rl5_RobotTube8, LM_F_Rl5_RobotTube9, LM_F_Rl5_SlingL, LM_F_Rl5_SlingR, LM_F_Rl5_UnderPF, LM_F_fl2_Bumper1_Ring, LM_F_fl2_Bumper1_Skirt, LM_F_fl2_Bumper3_Ring, LM_F_fl2_Bumper3_Skirt, LM_F_fl2_Bumper4_Ring, LM_F_fl2_Bumper4_Skirt, LM_F_fl2_Gate_A_8096___Flap, LM_F_fl2_Gate1_Flap, LM_F_fl2_Gate5_Wire, LM_F_fl2_Parts, LM_F_fl2_Plastics_Low, LM_F_fl2_Playfield, LM_F_fl2_Ramp_Left, LM_F_fl2_Ramp_Right, LM_F_fl2_UnderPF, LM_F_fl2_VaritargetP, LM_F_fl2_sw42, LM_F_fl2_sw53, LM_F_fl3_Bumper1_Ring, LM_F_fl3_Bumper1_Skirt, LM_F_fl3_Bumper2_Ring, LM_F_fl3_Bumper3_Ring, LM_F_fl3_Bumper3_Skirt, LM_F_fl3_Bumper4_Skirt, LM_F_fl3_CapRLib, LM_F_fl3_DT_sw41, LM_F_fl3_DT_sw51, LM_F_fl3_Gate1_Flap, LM_F_fl3_Gate5_Wire, LM_F_fl3_InlaneRubbers, LM_F_fl3_Parts, LM_F_fl3_Plastics_Low, LM_F_fl3_Plastics_Mid, LM_F_fl3_Plastics_Top, LM_F_fl3_Playfield, LM_F_fl3_RRCons, LM_F_fl3_RRLib, LM_F_fl3_RSling, _
	LM_F_fl3_RSling1, LM_F_fl3_RSling2, LM_F_fl3_Ramp_Left, LM_F_fl3_Ramp_Right, LM_F_fl3_UnderPF, LM_F_fl3_VaritargetP, LM_F_fr2_Bumper1_Skirt, LM_F_fr2_Bumper2_Ring, LM_F_fr2_Bumper2_Skirt, LM_F_fr2_Bumper3_Skirt, LM_F_fr2_Bumper4_Skirt, LM_F_fr2_Gate_A_8096___Flap, LM_F_fr2_Gate1_Flap, LM_F_fr2_Parts, LM_F_fr2_Plastics_Low, LM_F_fr2_Playfield, LM_F_fr2_Ramp_Left, LM_F_fr2_Ramp_Right, LM_F_fr2_UnderPF, LM_F_fr2_sw62, LM_F_fr3_Bumper1_Ring, LM_F_fr3_Bumper1_Skirt, LM_F_fr3_Bumper2_Ring, LM_F_fr3_Bumper2_Skirt, LM_F_fr3_Bumper3_Ring, LM_F_fr3_Bumper3_Skirt, LM_F_fr3_Bumper4_Ring, LM_F_fr3_Bumper4_Skirt, LM_F_fr3_DT_sw41, LM_F_fr3_DT_sw51, LM_F_fr3_DT_sw61, LM_F_fr3_Gate1_Flap, LM_F_fr3_Gate2_Wire, LM_F_fr3_Gate5_Wire, LM_F_fr3_KT_sw65, LM_F_fr3_LSling, LM_F_fr3_Parts, LM_F_fr3_Plastics_Low, LM_F_fr3_Plastics_Mid, LM_F_fr3_Plastics_Top, LM_F_fr3_Playfield, LM_F_fr3_RSling, LM_F_fr3_RSling1, LM_F_fr3_RSling2, LM_F_fr3_Ramp_Left, LM_F_fr3_Ramp_Right, LM_F_fr3_RobotTube2, LM_F_fr3_UnderPF, LM_F_fr3_VaritargetP, _
	LM_F_ll1_LF, LM_F_ll1_LFU, LM_F_ll1_Parts, LM_F_ll1_Plastics_Mid, LM_F_ll1_Playfield, LM_F_ll1_RF, LM_F_ll1_RFU, LM_F_ll1_RSling, LM_F_ll1_RSling1, LM_F_ll1_RSling2, LM_F_ll1_RobotTube1, LM_F_ll1_RobotTube10, LM_F_ll1_RobotTube11, LM_F_ll1_RobotTube12, LM_F_ll1_RobotTube2, LM_F_ll1_RobotTube3, LM_F_ll1_RobotTube6, LM_F_ll1_RobotTube7, LM_F_ll1_RobotTube8, LM_F_ll1_RobotTube9, LM_F_ll1_UnderPF, LM_F_ll2_DT_sw41, LM_F_ll2_LFU, LM_F_ll2_PRCons, LM_F_ll2_PRLib, LM_F_ll2_Parts, LM_F_ll2_Plastics_Low, LM_F_ll2_Playfield, LM_F_ll2_RFU, LM_F_ll2_RRCons, LM_F_ll2_RSling, LM_F_ll2_RSling1, LM_F_ll2_RSling2, LM_F_ll2_Ramp_Left, LM_F_ll2_RobotTube1, LM_F_ll2_RobotTube10, LM_F_ll2_RobotTube11, LM_F_ll2_RobotTube12, LM_F_ll2_RobotTube2, LM_F_ll2_RobotTube3, LM_F_ll2_RobotTube5, LM_F_ll2_RobotTube6, LM_F_ll2_RobotTube7, LM_F_ll2_RobotTube8, LM_F_ll2_RobotTube9, LM_F_ll2_SlingR, LM_F_ll2_UnderPF, LM_F_ll3_DT_sw41, LM_F_ll3_DT_sw61, LM_F_ll3_KT_sw65, LM_F_ll3_Parts, LM_F_ll3_Plastics_Low, LM_F_ll3_Playfield, LM_F_ll3_RF, _
	LM_F_ll3_RFU, LM_F_ll3_RSling, LM_F_ll3_RSling1, LM_F_ll3_Ramp_Left, LM_F_ll3_RobotTube1, LM_F_ll3_RobotTube10, LM_F_ll3_RobotTube11, LM_F_ll3_RobotTube12, LM_F_ll3_RobotTube2, LM_F_ll3_RobotTube3, LM_F_ll3_RobotTube5, LM_F_ll3_RobotTube6, LM_F_ll3_RobotTube7, LM_F_ll3_RobotTube8, LM_F_ll3_RobotTube9, LM_F_ll3_SlingL, LM_F_ll3_SlingR, LM_F_ll3_UnderPF, LM_F_ll4_DT_sw51, LM_F_ll4_DT_sw61, LM_F_ll4_KT_sw65, LM_F_ll4_LSling, LM_F_ll4_PRCons, LM_F_ll4_PRLib, LM_F_ll4_Parts, LM_F_ll4_Plastics_Low, LM_F_ll4_Playfield, LM_F_ll4_Ramp_Left, LM_F_ll4_RobotTube1, LM_F_ll4_RobotTube10, LM_F_ll4_RobotTube11, LM_F_ll4_RobotTube12, LM_F_ll4_RobotTube2, LM_F_ll4_RobotTube3, LM_F_ll4_RobotTube5, LM_F_ll4_RobotTube6, LM_F_ll4_RobotTube7, LM_F_ll4_RobotTube8, LM_F_ll4_RobotTube9, LM_F_ll4_SlingL, LM_F_ll4_SlingR, LM_F_ll4_UnderPF, LM_F_ll5_KT_sw65, LM_F_ll5_Parts, LM_F_ll5_Playfield, LM_F_ll5_RSling2, LM_F_ll5_RobotTube1, LM_F_ll5_RobotTube10, LM_F_ll5_RobotTube11, LM_F_ll5_RobotTube12, LM_F_ll5_RobotTube2, LM_F_ll5_RobotTube3, _
	LM_F_ll5_RobotTube5, LM_F_ll5_RobotTube6, LM_F_ll5_RobotTube7, LM_F_ll5_RobotTube8, LM_F_ll5_RobotTube9, LM_F_ll5_SlingL, LM_F_ll5_SlingR, LM_F_ll5_UnderPF, LM_F_rl3_DT_sw41, LM_F_rl3_KT_sw45, LM_F_rl3_LF, LM_F_rl3_LFU, LM_F_rl3_LSling1, LM_F_rl3_Parts, LM_F_rl3_Plastics_Low, LM_F_rl3_Playfield, LM_F_rl3_Ramp_Left, LM_F_rl3_RobotTube1, LM_F_rl3_RobotTube11, LM_F_rl3_RobotTube12, LM_F_rl3_RobotTube2, LM_F_rl3_RobotTube3, LM_F_rl3_RobotTube4, LM_F_rl3_RobotTube5, LM_F_rl3_RobotTube6, LM_F_rl3_RobotTube7, LM_F_rl3_RobotTube8, LM_F_rl3_RobotTube9, LM_F_rl3_SlingR, LM_F_rl3_UnderPF, LM_F_rl4_DT_sw41, LM_F_rl4_DT_sw61, LM_F_rl4_KT_sw45, LM_F_rl4_Parts, LM_F_rl4_Plastics_Low, LM_F_rl4_Plastics_Mid, LM_F_rl4_Playfield, LM_F_rl4_Ramp_Left, LM_F_rl4_RobotTube1, LM_F_rl4_RobotTube11, LM_F_rl4_RobotTube12, LM_F_rl4_RobotTube2, LM_F_rl4_RobotTube3, LM_F_rl4_RobotTube4, LM_F_rl4_RobotTube5, LM_F_rl4_RobotTube6, LM_F_rl4_RobotTube7, LM_F_rl4_RobotTube8, LM_F_rl4_RobotTube9, LM_F_rl4_SlingR, LM_F_rl4_UnderPF, LM_F_rl4_sw144, _
	LM_GI_Split_gi007_DT_sw41, LM_GI_Split_gi007_InlaneRubbers, LM_GI_Split_gi007_KT_sw45, LM_GI_Split_gi007_LF, LM_GI_Split_gi007_LFU, LM_GI_Split_gi007_LSling, LM_GI_Split_gi007_LSling1, LM_GI_Split_gi007_LSling2, LM_GI_Split_gi007_Parts, LM_GI_Split_gi007_Plastics_Low, LM_GI_Split_gi007_Plastics_Mid, LM_GI_Split_gi007_Playfield, LM_GI_Split_gi007_RF, LM_GI_Split_gi007_RLCons, LM_GI_Split_gi007_RLLib, LM_GI_Split_gi007_RSling, LM_GI_Split_gi007_RSling1, LM_GI_Split_gi007_RSling2, LM_GI_Split_gi007_SlingL, LM_GI_Split_gi007_SlingR, LM_GI_Split_gi007_UnderPF, LM_GI_Split_gi007_sw144, LM_GI_Split_gi007_sw54, LM_GI_Split_gi009_InlaneRubbers, LM_GI_Split_gi009_KT_sw65, LM_GI_Split_gi009_LF, LM_GI_Split_gi009_LSling, LM_GI_Split_gi009_LSling1, LM_GI_Split_gi009_LSling2, LM_GI_Split_gi009_Parts, LM_GI_Split_gi009_Plastics_Low, LM_GI_Split_gi009_Plastics_Mid, LM_GI_Split_gi009_Plastics_Top, LM_GI_Split_gi009_Playfield, LM_GI_Split_gi009_RF, LM_GI_Split_gi009_RFU, LM_GI_Split_gi009_RRCons, LM_GI_Split_gi009_RRLib, _
	LM_GI_Split_gi009_RSling, LM_GI_Split_gi009_RSling1, LM_GI_Split_gi009_RSling2, LM_GI_Split_gi009_SlingR, LM_GI_Split_gi009_UnderPF, LM_GI_Split_gi009_sw144, LM_GI_Split_gi009_sw44, LM_GI_Split_gi009_sw64, LM_GI_Split_gi1_Bumper1_Ring, LM_GI_Split_gi1_Bumper1_Skirt, LM_GI_Split_gi1_Bumper2_Ring, LM_GI_Split_gi1_Bumper2_Skirt, LM_GI_Split_gi1_Bumper3_Skirt, LM_GI_Split_gi1_Bumper4_Ring, LM_GI_Split_gi1_Bumper4_Skirt, LM_GI_Split_gi1_Parts, LM_GI_Split_gi1_Plastics_Low, LM_GI_Split_gi1_Playfield, LM_GI_Split_gi1_Ramp_Left, LM_GI_Split_gi1_Ramp_Right, LM_GI_Split_gi1_UnderPF, LM_GI_Split_gi1_VaritargetP, LM_GI_Split_gi2_Bumper1_Skirt, LM_GI_Split_gi2_Bumper2_Ring, LM_GI_Split_gi2_Bumper2_Skirt, LM_GI_Split_gi2_Bumper3_Skirt, LM_GI_Split_gi2_Bumper4_Skirt, LM_GI_Split_gi2_Gate_A_8096___F, LM_GI_Split_gi2_Parts, LM_GI_Split_gi2_Plastics_Low, LM_GI_Split_gi2_Playfield, LM_GI_Split_gi2_Ramp_Left, LM_GI_Split_gi2_Ramp_Right, LM_GI_Split_gi2_UnderPF, LM_GI_Split_gi3_Bumper1_Ring, LM_GI_Split_gi3_Bumper1_Skirt, _
	LM_GI_Split_gi3_Bumper2_Ring, LM_GI_Split_gi3_Bumper2_Skirt, LM_GI_Split_gi3_Bumper3_Ring, LM_GI_Split_gi3_Bumper3_Skirt, LM_GI_Split_gi3_Bumper4_Ring, LM_GI_Split_gi3_Bumper4_Skirt, LM_GI_Split_gi3_KT_sw45, LM_GI_Split_gi3_Parts, LM_GI_Split_gi3_Plastics_Low, LM_GI_Split_gi3_Playfield, LM_GI_Split_gi3_Ramp_Left, LM_GI_Split_gi3_Ramp_Right, LM_GI_Split_gi3_UnderPF, LM_GI_Split_gi3_VaritargetP, LM_GI_Split_gi4_Bumper1_Ring, LM_GI_Split_gi4_Bumper1_Skirt, LM_GI_Split_gi4_Bumper2_Ring, LM_GI_Split_gi4_Bumper2_Skirt, LM_GI_Split_gi4_Bumper3_Skirt, LM_GI_Split_gi4_Bumper4_Ring, LM_GI_Split_gi4_Bumper4_Skirt, LM_GI_Split_gi4_Parts, LM_GI_Split_gi4_Plastics_Low, LM_GI_Split_gi4_Plastics_Mid, LM_GI_Split_gi4_Playfield, LM_GI_Split_gi4_Ramp_Left, LM_GI_Split_gi4_Ramp_Right, LM_GI_Split_gi4_UnderPF, LM_GI_Bumper1_Ring, LM_GI_Bumper1_Skirt, LM_GI_Bumper2_Ring, LM_GI_Bumper2_Skirt, LM_GI_Bumper3_Ring, LM_GI_Bumper3_Skirt, LM_GI_Bumper4_Ring, LM_GI_Bumper4_Skirt, LM_GI_DT_sw41, LM_GI_DT_sw51, LM_GI_DT_sw61, _
	LM_GI_Gate_A_8096___Flap, LM_GI_Gate1_Flap, LM_GI_Gate5_Wire, LM_GI_KT_sw45, LM_GI_KT_sw65, LM_GI_PLCons, LM_GI_PLLib, LM_GI_PRCons, LM_GI_PRLib, LM_GI_Parts, LM_GI_Plastics_Low, LM_GI_Plastics_Mid, LM_GI_Plastics_Top, LM_GI_Playfield, LM_GI_RLCons, LM_GI_RLLib, LM_GI_RRCons, LM_GI_RRLib, LM_GI_Ramp_Left, LM_GI_Ramp_Right, LM_GI_UnderPF, LM_GI_VaritargetP, LM_GI_sw42, LM_GI_sw52, LM_GI_sw62, LM_L_l10_Playfield, LM_L_l10_UnderPF, LM_L_l11_Playfield, LM_L_l11_UnderPF, LM_L_l19_Parts, LM_L_l19_Playfield, LM_L_l19_Ramp_Left, LM_L_l19_UnderPF, LM_L_l2_LF, LM_L_l2_LFU, LM_L_l2_Parts, LM_L_l2_Playfield, LM_L_l2_UnderPF, LM_L_l20_Playfield, LM_L_l20_UnderPF, LM_L_l21_DT_sw61, LM_L_l21_Parts, LM_L_l21_Playfield, LM_L_l21_UnderPF, LM_L_l22_Parts, LM_L_l22_Playfield, LM_L_l22_Ramp_Left, LM_L_l22_UnderPF, LM_L_l23_Parts, LM_L_l23_Playfield, LM_L_l23_UnderPF, LM_L_l24_Playfield, LM_L_l24_UnderPF, LM_L_l25_Gate_A_8096___Flap, LM_L_l25_Parts, LM_L_l25_Playfield, LM_L_l25_Ramp_Left, LM_L_l25_UnderPF, LM_L_l26_Parts, _
	LM_L_l26_Playfield, LM_L_l26_UnderPF, LM_L_l27_Gate1_Flap, LM_L_l27_Parts, LM_L_l27_Playfield, LM_L_l27_UnderPF, LM_L_l3_Parts, LM_L_l3_Playfield, LM_L_l3_RF, LM_L_l3_RFU, LM_L_l3_UnderPF, LM_L_l45_KT_sw45, LM_L_l45_Parts, LM_L_l45_Playfield, LM_L_l45_RLCons, LM_L_l45_RLLib, LM_L_l45_UnderPF, LM_L_l46_KT_sw65, LM_L_l46_Parts, LM_L_l46_Plastics_Low, LM_L_l46_Plastics_Mid, LM_L_l46_Playfield, LM_L_l46_RRCons, LM_L_l46_RRLib, LM_L_l46_UnderPF, LM_L_l47_InlaneRubbers, LM_L_l47_KT_sw45, LM_L_l47_Parts, LM_L_l47_Playfield, LM_L_l47_UnderPF, LM_L_l5_Playfield, LM_L_l5_UnderPF, LM_L_l51_InlaneRubbers, LM_L_l51_KT_sw65, LM_L_l51_Parts, LM_L_l51_Playfield, LM_L_l51_UnderPF, LM_L_l6_Parts, LM_L_l6_Playfield, LM_L_l6_UnderPF, LM_L_l7_Parts, LM_L_l7_Playfield, LM_L_l7_UnderPF, LM_L_l8_Parts, LM_L_l8_Playfield, LM_L_l8_UnderPF, LM_L_l9_Playfield, LM_L_l9_UnderPF, LM_LBW_l28_Parts, LM_LBW_l28_Plastics_Low, LM_LBW_l28_Playfield, LM_LBW_l29_Parts, LM_LBW_l29_Plastics_Low, LM_LBW_l29_Playfield, LM_LBW_l29_Ramp_Left, _
	LM_LBW_l30_Parts, LM_LBW_l30_Plastics_Low, LM_LBW_l30_Ramp_Left, LM_LBW_l31_Gate_A_8096___Flap, LM_LBW_l31_Parts, LM_LBW_l31_Plastics_Low, LM_LBW_l31_Ramp_Left, LM_LBW_l32_Gate_A_8096___Flap, LM_LBW_l32_Parts, LM_LBW_l32_Plastics_Low, LM_LBW_l32_Ramp_Left, LM_LBW_l33_Gate_A_8096___Flap, LM_LBW_l33_Parts, LM_LBW_l33_Plastics_Low, LM_LBW_l33_Playfield, LM_LBW_l33_UnderPF, LM_LBW_l34_Parts, LM_LBW_l34_Plastics_Low, LM_LBW_l34_Playfield, LM_LBW_l35_Parts, LM_LBW_l35_Plastics_Low, LM_LBW_l35_Playfield, LM_LBW_l35_UnderPF, LM_LBW_l36_Parts, LM_LBW_l36_Plastics_Low, LM_LBW_l36_Playfield, LM_LBW_l37_Parts, LM_LBW_l37_Plastics_Low, LM_LBW_l37_Playfield, LM_LBW_l38_Parts, LM_LBW_l38_Plastics_Low, LM_LBW_l38_Playfield, LM_LBW_l39_Parts, LM_LBW_l39_Plastics_Low, LM_LBW_l39_Ramp_Right, LM_LBW_l40_Gate1_Flap, LM_LBW_l40_Parts, LM_LBW_l40_Plastics_Low, LM_LBW_l40_Ramp_Right, LM_LBW_l40_UnderPF, LM_LBW_l41_Parts, LM_LBW_l41_Plastics_Low, LM_LBW_l41_Playfield, LM_LBW_l41_Ramp_Right, LM_LBW_l41_UnderPF, LM_LBW_l42_Parts, _
	LM_LBW_l42_Plastics_Low, LM_LBW_l42_Playfield, LM_LBW_l42_Ramp_Right, LM_LBW_l42_UnderPF, LM_LBW_l43_Parts, LM_LBW_l43_Plastics_Low, LM_LBW_l43_Playfield, LM_LBW_l43_Ramp_Right, LM_LBW_l44_Parts, LM_LBW_l44_Plastics_Low, LM_LBW_l44_Playfield, LM_LBW_l44_Ramp_Right, LM_LUPF_l15_Parts, LM_LUPF_l15_RobotTube1, LM_LUPF_l15_RobotTube10, LM_LUPF_l15_RobotTube11, LM_LUPF_l15_RobotTube12, LM_LUPF_l15_RobotTube8, LM_LUPF_l15_RobotTube9, LM_LUPF_l15_UnderPF, LM_LUPF_l16_Parts, LM_LUPF_l16_RobotTube1, LM_LUPF_l16_RobotTube12, LM_LUPF_l16_RobotTube2, LM_LUPF_l16_RobotTube3, LM_LUPF_l16_RobotTube4, LM_LUPF_l16_RobotTube5, LM_LUPF_l16_RobotTube6, LM_LUPF_l16_RobotTube9, LM_LUPF_l16_UnderPF, LM_LUPF_l17_Parts, LM_LUPF_l17_RobotTube1, LM_LUPF_l17_RobotTube10, LM_LUPF_l17_RobotTube11, LM_LUPF_l17_RobotTube12, LM_LUPF_l17_RobotTube2, LM_LUPF_l17_RobotTube5, LM_LUPF_l17_RobotTube6, LM_LUPF_l17_RobotTube8, LM_LUPF_l17_RobotTube9, LM_LUPF_l17_UnderPF, LM_LUPF_l18_Parts, LM_LUPF_l18_RobotTube1, LM_LUPF_l18_RobotTube11, _
	LM_LUPF_l18_RobotTube12, LM_LUPF_l18_RobotTube2, LM_LUPF_l18_RobotTube3, LM_LUPF_l18_RobotTube4, LM_LUPF_l18_RobotTube5, LM_LUPF_l18_RobotTube6, LM_LUPF_l18_RobotTube8, LM_LUPF_l18_RobotTube9, LM_LUPF_l18_UnderPF)
' VLM  Arrays - End



Dim DesktopMode:DesktopMode = Table1.ShowDT

dim xx
If DesktopMode then
	For each xx in DisplayDT:xx.visible = 1: Next
end if


'VR Room Selection


Dim VRRoom, cab_mode
DesktopMode = Table1.ShowDT
If RenderingMode = 2 or VRTest = 1 Then VRRoom=1 Else VRRoom=0      'VRRoom set based on RenderingMode starting in version 10.72
if Not DesktopMode and VRRoom=0 Then cab_mode=1 Else cab_mode=0

Dim UseVPMDMD
UseVPMDMD = 1


'/////////////////////
'====================

'Load VPM and scripts

'******************************************************
'  ZVAR: Constants and Global Variables
'******************************************************
Const BallSize = 50		'Ball size must be 50
Const BallMass = 1		'Ball mass must be 1
Const tnob = 2			'Total number of balls on the playfield including captive balls.
Const lob = 0

Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height

Const cGameName = "genesis"
Const UseVPMModSol = 2
Const UseSolenoids = 2			'1 = Normal Flippers, 2 = Fastflips
Const UseLamps = 1				'0 = Custom lamp handling, 1 = Built-in VPX handling (using light number in light timer)
Const UseSync = 0
Const HandleMech = 0
Const SSolenoidOn = ""			'Sound sample used for this, obsolete.
Const SSolenoidOff = ""			' ^
Const SFlipperOn = ""			' ^
Const SFlipperOff = ""			' ^
Const SCoin = ""				' ^

LoadVPM "03070000", "sys80.vbs", 3.02

Dim bsTrough, bsArmsLock, bsLegsLock, dtM', plungerIM
Dim x, Balls', bump1, bump2, bump3, bump4
Dim Last12, Current12, Last13, Current13, Last14, Current14

Dim GBall1, GBall2, gBOT, BIP


'**********
'Table Init
'**********

Sub Table1_Init
    vpminit me

	vpmMapLights AllLamps 

    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Genesis (Gottlieb 1986)" & vbNewLine & "VPW"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 0
        .ShowFrame = 0
        .HandleMechanics = 0
		'.SetDisplayPosition 0,0,GetPlayerHWnd 'if you can't see the DMD then uncomment this line
    End With
    On Error Resume Next
    Controller.Run
    If Err Then MsgBox Err.Description
    On Error Goto 0

	' 'Trough - Creates a ball in the kicker switch and gives that ball used an individual name.
	Set GBall1 = sw74a.CreateSizedballWithMass(Ballsize/2,Ballmass)
    Set GBall2 = sw74b.CreateSizedballWithMass(Ballsize/2,Ballmass)

	' '***Setting up a ball array (collection), must contain all the balls you create on the table.
	gBOT = Array(GBall1, GBall2)

	Controller.Switch(74) = 1
	'Controller.Switch(55) = 1 'Enabling this breaks the game

	' Nudging
	vpmNudge.TiltSwitch = 57 'swTilt
	vpmNudge.Sensitivity = 5
	vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, Bumper4, LeftSlingshot, RightSlingshot)

	'Saucers Declaration
	Set bsArmsLock = New cvpmSaucer
	with bsArmsLock
		.InitKicker sw43, 43, 160, 10, 0	' LeftKickout '160, 5 ,8
		'.InitExitVariance 2, 1
		.createevents "bsArmsLock", sw43
	end with

	Set bsLegsLock = New cvpmSaucer
	with bsLegsLock
		.InitKicker sw73, 73, 0, 25, 0	' RightKickout '320, 16, 20
		'.InitExitVariance 2, 1
		.createevents "bsLegsLock", sw73
	end with

	'Other variables
	Last12 = 0
	Current12 = 0
	Last13 = 0
	Current13 = 0
	Last14 = 0
	Current14 = 0

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

	'display Option
	if not DesktopMode then
		For each xx in DisplayDT:xx.visible = 0: Next
	end If

	'GIOn

	InitSlings
	InitRobot

	SetFlasherOpacity 100
	ApplyVRRoomChoice
	ApplyVRBackglassChoice

	' If user wants to set dip switches themselves it will force them to set it via F6.
	If SetDIPSwitches = 0 Then
		SetDefaultDips
	End If

End Sub

Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub

'******************************************************
'  ZTIM: Timers
'******************************************************

'The FrameTimer interval should be -1, so executes at the display frame rate
'The frame timer should be used to update anything visual, like some animations, shadows, etc.
'However, a lot of animations will be handled in their respective _animate subroutines.

Dim FrameTime, InitFrameTime
InitFrameTime = 0

FrameTimer.Interval = -1
Sub FrameTimer_Timer() 
	FrameTime = gametime - InitFrameTime 'Calculate FrameTime as some animuations could use this
	InitFrameTime = gametime	'Count frametime
	'Add animation stuff here
	BSUpdate
	UpdateBallBrightness
	RollingUpdate				'Update rolling sounds
	DoDTAnim					'Drop target animations
	DoVTAnim
	DoKTAnim
	AnimateDropTargets
	UpdateKickingTargets
	UpdateVaritarget
	UpdateMultipleLamps
    If VRRoom = 0 and cab_mode = 0 Then: DisplayTimer: End If
	If VRRoom=1 Then: VRDisplayTimer: End If
End Sub

'The CorTimer interval should be 10. It's sole purpose is to update the Cor (physics) calculations
CorTimer.Interval = 10
Sub CorTimer_Timer() 
	Cor.Update
End Sub



' keys

Sub Table1_KeyDown(ByVal keycode)
    If Keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress
    If Keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress
	If keycode = PlungerKey Then 
		Plunger.Pullback
		SoundPlungerPull
		TimerVRPlunger.Enabled = True
		TimerVRPlunger1.Enabled = False
		VR_Plunger.Y = 20
	End If
  	If Keycode = LeftFlipperKey Then
		VR_LeftFlipperButton.X = 0 + 6
	End if
  	If Keycode = RightFlipperKey Then
		VR_RightFlipperButton.X = 0 - 6 
	End if
	If Keycode = StartGameKey Then
		SoundStartButton
		VR_StartButton.y=VR_StartButton.y-4
	End if 
	If keycode = LeftTiltKey Then SoundNudgeLeft
	If keycode = RightTiltKey Then SoundNudgeRight
	If keycode = CenterTiltKey Then SoundNudgeCenter
    If keycode = StartGameKey Then SoundStartButton
	If keycode = AddCreditKey or keycode = AddCreditKey2 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If
    If vpmKeyDown(keycode) Then Exit Sub
End Sub


Sub Table1_KeyUp(ByVal keycode)
    If Keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
	If Keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress
	If keycode = PlungerKey Then
		Plunger.Fire
		If BIP = 1 Then
			SoundPlungerReleaseBall()   'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall() 'Plunger release sound when there is no ball in shooter lane
		End If
		TimerVRPlunger.Enabled = False
        TimerVRPlunger1.Enabled = True
		VR_Plunger.Y = 20
	End If
	If Keycode = LeftFlipperKey Then
		VR_LeftFlipperButton.x = 0
	End if
	If keycode = RightFlipperKey Then
		VR_RightFlipperButton.X = 0
	End If
	If Keycode = StartGameKey Then
		SoundStartButton
		VR_StartButton.y=VR_StartButton.y+4
	End if
	If vpmKeyUp(KeyCode) Then Exit Sub
End Sub

' VR Plunger code

Sub TimerVRPlunger_Timer
  If VR_Plunger.Y < 130 then
       VR_Plunger.Y = VR_Plunger.Y + 5
  End If
End Sub

Sub TimerVRPlunger1_Timer
  VR_Plunger.Y = 20 + (5* Plunger.Position) -20
End Sub

Sub swPlunger_Hit:BIP = 1:End Sub

Sub swPlunger_UnHit:BIP = 0:End Sub

'*******************************************
'  ZOPT: User Options
'*******************************************

Dim LightLevel : LightLevel = 0.25				' Level of room lighting (0 to 1), where 0 is dark and 100 is brightest
Dim ColorLUT : ColorLUT = 1						' Color desaturation LUTs: 1 to 11, where 1 is normal and 11 is black'n'white
Dim VolumeDial : VolumeDial = 0.8           	' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume : BallRollVolume = 0.5   	' Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.5 		' Level of ramp rolling volume. Value between 0 and 1
Dim VRRoomChoice : VRRoomChoice = 1   			'0=Minimal Room 1=Mega Room
Dim VRBackglassChoice : VRBackglassChoice = 0	'0=Original 1=Alt


Dim Slope, FlipperStrength
Dim FlasherBlinkCount, FlasherBlinkMaxSteps, newFlasherColorIdx
Dim LastFlasherBrightness : LastFlasherBrightness = -1
Dim newFlasherBrightness


' Called when options are tweaked by the player. 
' - 0: game has started, good time to load options and adjust accordingly
' - 1: an option has changed
' - 2: options have been reseted
' - 3: player closed the tweak UI, good time to update staticly prerendered parts
' Table1.Option arguments are: 
' - option name, minimum value, maximum value, step between valid values, default value, unit (0=None, 1=Percent), an optional arry of literal strings
Dim dspTriggered : dspTriggered = False

Sub Table1_OptionEvent(ByVal eventId)
	Dim v

    If eventId = 1 And Not dspTriggered Then dspTriggered = True : DisableStaticPreRendering = True : End If

	' Color Saturation
    ColorLUT = Table1.Option("Color Saturation", 0, 11, 1, 0, 0, _
		Array("Normal", "Vibrant", "Desaturated 10%", "Desaturated 20%", "Desaturated 30%", "Desaturated 40%", "Desaturated 50%", _
        "Desaturated 60%", "Desaturated 70%", "Desaturated 80%", "Desaturated 90%", "Black 'n White"))
	if ColorLUT = 0 Then Table1.ColorGradeImage = "colorgradelut256x16_1to1"
	if ColorLUT = 1 Then Table1.ColorGradeImage = "ColorGradeLUT256x16_1to1gam0.70vibr50sat100"
	if ColorLUT = 2 Then Table1.ColorGradeImage = "ColorGradeLUT256x16_1to1gam0.70vibr50sat90"
	if ColorLUT = 3 Then Table1.ColorGradeImage = "ColorGradeLUT256x16_1to1gam0.70vibr50sat80"
	if ColorLUT = 4 Then Table1.ColorGradeImage = "ColorGradeLUT256x16_1to1gam0.70vibr50sat70"
	if ColorLUT = 5 Then Table1.ColorGradeImage = "ColorGradeLUT256x16_1to1gam0.70vibr50sat60"
	if ColorLUT = 6 Then Table1.ColorGradeImage = "ColorGradeLUT256x16_1to1gam0.70vibr50sat50"
	if ColorLUT = 7 Then Table1.ColorGradeImage = "ColorGradeLUT256x16_1to1gam0.70vibr50sat40"
	if ColorLUT = 8 Then Table1.ColorGradeImage = "ColorGradeLUT256x16_1to1gam0.70vibr50sat30"
	if ColorLUT = 9 Then Table1.ColorGradeImage = "ColorGradeLUT256x16_1to1gam0.70vibr50sat20"
	if ColorLUT = 10 Then Table1.ColorGradeImage = "ColorGradeLUT256x16_1to1gam0.70vibr50sat10"
	if ColorLUT = 11 Then Table1.ColorGradeImage = "ColorGradeLUT256x16_1to1gam0.70vibr50sat00"

	'GI Lamps Color
	v = Table1.Option("GI Lamps Color", 0, 4, 1, 0, 0, Array("Incandescent (warm white)", "LED (cool white)", "Purple", "Blue", "Red"))
	SetGiColor v

'	'Insert Lamps Color
'	v = Table1.Option("Insert Lamps Color", 0, 1, 1, 0, 0, Array("Incandescent (warm white)", "LED (cool white)"))
'	SetInsertsColor v

	'Bumper Lamps Color
	v = Table1.Option("Bumper Lamps Color", 0, 4, 1, 0, 0, Array("Incandescent (warm white)", "LED (cool white)", "Purple", "Blue", "Red"))
	SetBumpersColor v

	'Flasher Lamps Color
	v = Table1.Option("Flasher Lamps Color", 0, 4, 1, 0, 0, Array("Incandescent (warm white)", "LED (cool white)", "Purple", "Blue", "Red"))
	SetFlashersColor v

	'Flasher Brightness
	newFlasherBrightness = Table1.Option("Flasher Lamps Brightness", 0, 200, 25, 200, 0)
	SetFlashersBrightness newFlasherBrightness
	If LastFlasherBrightness = -1 Then
		LastFlasherBrightness = newFlasherBrightness
	ElseIf newFlasherBrightness <> LastFlasherBrightness Then
		StartFlashersBlink 3, 100        ' 3 flashes, 100 ms toggle
		LastFlasherBrightness = newFlasherBrightness
	End If

	' Playfield Reflections
	v = Table1.Option("Playfield Reflections", 0, 1, 1, 1, 0, Array("Clean", "Rough"))
	Select Case v		
		Case 0: playfield_mesh.ReflectionProbe = "Playfield Reflections": BM_Playfield.ReflectionProbe = "Playfield Reflections"
		Case 1: playfield_mesh.ReflectionProbe = "Playfield Reflections Rough": BM_Playfield.ReflectionProbe = "Playfield Reflections Rough"
	End Select
	
	' Reflections Intensity
	'v = Table1.Option("Reflections Intensity", 0.05, 0.15, 0.1, 5.5, 0)
	'playfield_mesh.Strength = v

   ' Toggle Reflections Scope
	v = Table1.Option("Playfield Reflections Scope", 0, 2, 1, 2, 0, Array("None", "Static only", "Dynamic"))
	ReflectionToggle(v)

	Dim BP
    ' Toggle Rails
	If RenderingMode <> 2 And Not VRTest Then
		v = Table1.Option("Side Rails", 0, 1, 1, 1, 0, Array("Hide", "Show"))
		For Each BP in BP_SideRails : BP.Visible = v: Next
	End If

    ' VR room (only matters in VR)
    VRRoomChoice = Table1.Option("VR Room", 0, 1, 1, VRRoomChoice, 0, Array("Minimal Room", "Mega Room"))
	ApplyVRRoomChoice

    ' eventId = 1 means an option changed -> apply right now
    If eventId = 1 Then ApplyVRRoomChoice
    ' VR room (only matters in VR)

    VRBackglassChoice = Table1.Option("VR Backglass", 0, 1, 1, VRBackglassChoice, 0, Array("Original", "Alternative"))
	ApplyVRBackglassChoice

    ' eventId = 1 means an option changed -> apply right now
    If eventId = 1 Then ApplyVRBackglassChoice

    ' Outlane Difficulty
	v = Table1.Option("Outlanes difficulty", 0, 1, 1, 0, 0, Array("Normal", "Hard"))
	SetOutlaneDifficulty(v)
	
	' Inlane Posts Rubbers
	v = Table1.Option("Inlane Posts Rubbers", 0, 1, 1, 1, 0, Array("Off", "On"))
	SetInlanePostsRubbers(v)

	'Set slope
	Slope = Table1.Option("Playfield Angle (degrees)", 4, 8, 0.5, 6, 0)
	Table1.SlopeMin = Slope
	Table1.SlopeMax = Slope

	'Set flipper strength
	FlipperStrength = Table1.Option("Flipper Strength", 2000, 3500, 100, 2200, 0)
	LeftFlipper.Strength = FlipperStrength
	RightFlipper.Strength = FlipperStrength

    ' Sound volumes
    VolumeDial = Table1.Option("Mech Volume", 0, 1, 0.01, 0.8, 1)
    BallRollVolume = Table1.Option("Ball Roll Volume", 0, 1, 0.01, 0.5, 1)
	RampRollVolume = Table1.Option("Ramp Roll Volume", 0, 1, 0.01, 0.5, 1)

	' Room brightness
'	LightLevel = Table1.Option("Table Brightness (Ambient Light Level)", 0, 1, 0.01, .5, 1)
	LightLevel = NightDay/100
	SetRoomBrightness LightLevel   'Uncomment this line for lightmapped tables.

    If eventId = 3 And dspTriggered Then dspTriggered = False : DisableStaticPreRendering = False : End If
End Sub

Function ReflectionToggle(state)
	Dim BP, v1, v2
	Select Case state
		Case 0: v1 = false: v2 = false  'No reflections
		Case 1: v1 = true: v2 = false   'Only reflect Bakemaps
		Case 2: v1 = true:  v2 = true   'Reflect Bakemaps and Lightmaps
	End Select

	For Each BP in BG_Bakemap
		BP.ReflectionEnabled = v1
	Next

	For Each BP in BG_Lightmap
		BP.ReflectionEnabled = v2
	Next

	BM_Parts.ReflectionEnabled = v2
	BM_Playfield.ReflectionEnabled = False
	playfield_mesh.ReflectionEnabled= False
End Function

Function SetOutlaneDifficulty(d)
	Dim BP, v1, v2

	Select Case d
		Case 0: ' Conservative (Hard)
			v1 = False: v2 =True
		Case 1: ' Liberal (Easy)
			v1 = True:  v2 = False
	End Select

	For Each BP in BP_PLCons : BP.visible = v1 : Next
	For Each BP in BP_PRCons : BP.visible = v1 : Next
	For Each BP in BP_RLCons : BP.visible = v1 : Next
	For Each BP in BP_RRCons : BP.visible = v1 : Next
	For Each BP in BP_CapRCons : BP.visible = v1 : Next
	
	For Each BP in BP_PLLib : BP.visible = v2 : Next
	For Each BP in BP_PRLib : BP.visible = v2 : Next
	For Each BP in BP_RLLib : BP.visible = v2 : Next
	For Each BP in BP_RRLib : BP.visible = v2 : Next
	For Each BP in BP_CapRLib : BP.visible = v2 : Next

	' set the physics
	post_right_conservative.collidable   = v2:  post_left_conservative.collidable  = v2
	rubber_right_conservative.collidable		 = v2:	rubber_left_conservative.collidable		= v2

	post_right_liberal.collidable   = v1:  post_left_liberal.collidable  = v1
	rubber_right_liberal.collidable		 = v1:	rubber_left_liberal.collidable		= v1

End Function

Function SetInlanePostsRubbers(d)
	Dim BP, v1, v2

	Select Case d
		Case 0: ' Off (Hard)
			v1 = False: v2 =True
		Case 1: ' On (Easy)
			v1 = True:  v2 = False
	End Select

	For Each BP in BP_InlaneRubbers : BP.visible = v1 : Next

	' set the physics
	IPRubberOnLeft.collidable   = v1
	IPRubberOnRight.collidable		 = v1

End Function


Sub SetFlasherOpacity(x)
	Dim BP
	For Each BP In BL_F_Rl1: BP.opacity = x: Next
	For Each BP In BL_F_Rl2: BP.opacity = x: Next
	For Each BP In BL_F_Rl3: BP.opacity = x: Next
	For Each BP In BL_F_Rl4: BP.opacity = x: Next
	For Each BP In BL_F_Rl5: BP.opacity = x: Next
	For Each BP In BL_F_Ll1: BP.opacity = x: Next
	For Each BP In BL_F_Ll2: BP.opacity = x: Next
	For Each BP In BL_F_Ll3: BP.opacity = x: Next
	For Each BP In BL_F_Ll4: BP.opacity = x: Next
	For Each BP In BL_F_Ll5: BP.opacity = x: Next
End Sub


'******************************************************
' 	ZBBR: BALL BRIGHTNESS
'******************************************************

Const BallBrightness =  .5      	'Ball brightness - Value between 0 and 1 (0=Dark ... 1=Bright)

' Constants for plunger lane ball darkening.
' You can make a temporary wall in the plunger lane area and use the co-ordinates from the corner control points.
Const PLOffset = 0.5			'Minimum ball brightness scale in plunger lane
Const PLLeft = 1000 			'X position of punger lane left
Const PLRight = 1060 			'X position of punger lane right
Const PLTop = 1225 				'Y position of punger lane top
Const PLBottom = 1900 			'Y position of punger lane bottom
Dim PLGain: PLGain = (1-PLOffset)/(PLTop-PLBottom)

Sub UpdateBallBrightness
	Dim s, b_base, b_r, b_g, b_b, d_w
	b_base = 120 * BallBrightness + 135*gilvl	' orig was 120 and 70

	For s = 0 To UBound(gBOT)
		' Handle z direction
		d_w = b_base*(1 - (gBOT(s).z-25)/500)
		If d_w < 30 Then d_w = 30
		' Handle plunger lane
		If InRect(gBOT(s).x,gBOT(s).y,PLLeft,PLBottom,PLLeft,PLTop,PLRight,PLTop,PLRight,PLBottom) Then  
			d_w = d_w*(PLOffset+PLGain*(gBOT(s).y-PLBottom))
		End If
		' Assign color
		b_r = Int(d_w)
		b_g = Int(d_w)
		b_b = Int(d_w)
		If b_r > 255 Then b_r = 255
		If b_g > 255 Then b_g = 255
		If b_b > 255 Then b_b = 255
		gBOT(s).color = b_r + (b_g * 256) + (b_b * 256 * 256)
		'debug.print "--- ball.color level="&b_r
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
	Dim s: For s = lob To UBound(gBOT)
		' *** Normal "ambient light" ball shadow
		
		'Primitive shadow on playfield, flasher shadow in ramps
		'** If on main and upper pf
		If gBOT(s).Z > 20 And gBOT(s).Z < 30 Then
			objBallShadow(s).visible = 1
			objBallShadow(s).X = gBOT(s).X + (gBOT(s).X - (tablewidth / 2)) / (Ballsize / AmbientMovement) + offsetX
			objBallShadow(s).Y = gBOT(s).Y + offsetY
			'objBallShadow(s).Z = gBOT(s).Z + s/1000 + 1.04 - 25	

		'** No shadow if ball is off the main playfield (this may need to be adjusted per table)
		Else
			objBallShadow(s).visible = 0
		End If
	Next
End Sub





'*******************
'Solenoids Callback
'*******************

SolCallback(1) = "SolVTReset"
'SolCallback(2) = "SolLeftKicker" 'doesn't work
SolCallback(2) = "bsLegsLock.SolOut"
'SolCallback(3) = "SolBallRelease" 'doesn't work by a solenoid, but is controlled by lamp 14
SolModCallback(4) = "SolLeftFlasher"
'SolCallback(5) = "SolRightKicker" 'doesn't work
SolCallback(5) = "bsArmsLock.SolOut"
'SolCallback(6) = "DropDelaysub" 'replaced by Rothbauer Targets
SolCallback(6) = "ResetDrops"
SolModCallback(7) = "SolRightFlasher"
SolCallback(8) = "SolKnocker"
SolCallback(9) = "SolOuthole" 
'SolCallback(10) = "GIUpdate"

'Solenoids Subs

Sub SolLeftFlasher(lvl)
	Fl2.state = lvl
	fl3.state = lvl
	If lvl = 1 Then
		Sound_GI_Relay 1, KnockerPosition
	ElseIf lvl = 0 Then
		Sound_GI_Relay 0, KnockerPosition
	End If
End Sub

Sub SolRightFlasher(lvl)
	fr2.state = lvl
	fr3.State = lvl
	If lvl = 1 Then
		Sound_GI_Relay 1, KnockerPosition
	ElseIf lvl = 0 Then
		Sound_GI_Relay 0, KnockerPosition
	End If
End Sub

Sub SolOuthole(enabled)
	If enabled And sw74b.BallCntOver = 0 Then
		sw55.Kick 60, 20
	End If
End Sub

Sub SolKnocker(enabled)
	If enabled then
		KnockerSolenoid
	End If
End Sub

'Sub SolLeftKicker(enabled)
'	Debug.Print("solleftkicker : " & enabled)
'	If enabled Then
'		sw43.Kick 160, 5
'		SoundSaucerKick 1, sw43
'		Controller.Switch(43) = 0
'	End If
'End Sub

'Sub SolRightKicker(enabled)
'	If enabled Then
'		sw73.Kick 320, 16
'		SoundSaucerKick 1, sw73
'		Controller.Switch(73) = 0
'	End If
'End Sub

'Sub SolBallRelease(enabled)
'	Debug.Print("solballrelease " & enabled)
'	If enabled Then
'		If sw74a.BallCntOver = 0 Then sw74b.kick 60, 8
'		'sw74a.Kick 60, 9
'	End If
'End Sub

'******************************************************
'						TROUGH 
'******************************************************
'4kp fix (tilt): switch 74 now follows the BACK trough slot (sw74b), so it is closed only when
'4kp fix (tilt): both balls are home. It used to follow the front slot (sw74a) and stayed closed
'4kp fix (tilt): during single-ball play, which let the ROM end a tilted ball before it drained.
'4kp original: Sub sw74a_Hit():Controller.Switch(74) = 1:UpdateTrough:End Sub
'4kp original: Sub sw74a_UnHit():Controller.Switch(74) = 0:UpdateTrough:End Sub
'4kp original: Sub sw74b_Hit():UpdateTrough:End Sub
'4kp original: Sub sw74b_UnHit():UpdateTrough:End Sub
Sub sw74a_Hit():UpdateTrough:End Sub	'4kp fix (tilt)
Sub sw74a_UnHit():UpdateTrough:End Sub	'4kp fix (tilt)
Sub sw74b_Hit():Controller.Switch(74) = 1:UpdateTrough:End Sub	'4kp fix (tilt)
Sub sw74b_UnHit():Controller.Switch(74) = 0:UpdateTrough:End Sub	'4kp fix (tilt)
'Sub sw74_Hit():Debug.Print("sw74_Hit"):End Sub
'Sub sw74_UnHit():Debug.Print("sw74_UnHit"):End Sub
Sub sw55_Hit():Controller.Switch(55) = 1 : RandomSoundDrain sw55 : UpdateTrough : End Sub
Sub sw55_UnHit():Controller.Switch(55) = 0 : UpdateTrough : End Sub

Sub UpdateTrough()
	UpdateTroughTimer.Interval = 1300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	If sw74a.BallCntOver = 0 Then sw74b.kick 60, 20
	'If sw74b.BallCntOver = 0 Then sw55.kick 60, 20
	UpdateTroughTimer.Enabled = 0
End Sub

'dim Drop targets 
dim drop1, drop2, drop3
drop1 = sw41.isdropped
drop2 = sw51.isdropped
drop3 = sw61.isdropped

'********************** Drop Targets ************************
Sub Sw41_Hit : DTHit 41 : End Sub
Sub Sw51_Hit : DTHit 51 : End Sub
Sub Sw61_Hit : DTHit 61 : End Sub

Sub ResetDrops(enabled)
	if enabled then
		RandomSoundDropTargetReset BM_DT_sw41
		DTRaise 41
		DTRaise 51
		DTRaise 61
	end if
End Sub

'******************************************************
'****  ZGIU:	GI Control
'******************************************************

'**** Global variable to hold current GI light intensity. Not Mandatory, but may help in other lighting subs and timers
'**** This value is updated always when GI state is being updated
dim gilvl	

Sub GIOn
	GIUpdates 1
End Sub

Set GICallback = GetRef("GIUpdate")
'Set GICallback2  = GetRef("GIUpdates")		'use this for non-modulated GI

Sub GIUpdates(nr, aLvl)	
	'debug.print "GIUpdates value: " & aLvl
	

	' Update the state for each GI light. The state will be a float value between 0 and 1.
	Dim bulb: For Each bulb in GI: bulb.State = aLvl: Next

	' If the GI has an associated Relay sound, this can be played 
	If aLvl >= 0.5 And gilvl < 0.5 Then
		Sound_GI_Relay 1, Bumper1 'Note: Bumper1 is just used for sound positioning. Can be anywhere that makes sense.
	ElseIf aLvl <= 0.4 And gilvl > 0.4 Then
		Sound_GI_Relay 0, Bumper1
	End If

    gilvl = aLvl

End Sub

Sub GIUpdate(nr, Enabled)
	'Debug.Print("GIUpdate :" & Enabled)
	If Enabled Then
		gilvl = 0
	Else
		gilvl = 1
	End If
	Dim bulb: For Each bulb in GI: bulb.State = gilvl: Next

	Sound_GI_Relay gilvl, Bumper1
End Sub


'****************************
'	ZGIC: GI Colors
'****************************

''''''''' GI Color Change options
' reference: https://andi-siess.de/rgb-to-color-temperature/

Dim c2700k: c2700k = rgb(255, 169, 87)
Dim c3000k: c3000k = rgb(255, 180, 107)
Dim c5000k: c5000k = rgb(255, 244, 226)

Dim cRedFull: cRedFull = rgb(255,0,0) 
Dim cRed: cRed= rgb(255,5,5)
Dim cPinkFull: cPinkFull = rgb(255,0,225)
Dim cPink: cPink = rgb(255,5,255)
Dim cWhiteFull: cWhiteFull = rgb(255,255,128) 
Dim cWhite: cWhite = rgb(255,255,255)
Dim cBlueFull: cBlueFull= rgb(0,0,255)
Dim cBlue: cBlue = rgb(5,5,255)
Dim cCyanFull: cCyanFull= rgb(0,255,255)
Dim cCyan : cCyan = rgb(5,128,255)
Dim cYellowFull:cYellowFull  = rgb(255,255,128)
Dim cYellow: cYellow = rgb(255,255,0)
Dim cOrangeFull: cOrangeFull = rgb(255,128,0)
Dim cOrange: cOrange = rgb(255,70,5)
Dim cGreenFull: cGreenFull = rgb(0,255,0)
Dim cGreen: cGreen = rgb(5,255,5)
Dim cPurpleFull: cPurpleFull = rgb(128,0,255)
Dim cPurple: cPurple = rgb(60,5,255)
Dim cAmberFull: cAmberFull = rgb(255,197,143)
Dim cAmber: cAmber = rgb(255,197,143)

Dim cArray
cArray = Array(c2700k,c5000k,cPurple,cBlue,cRed)

sub SetGiColor(c)
	Dim xx, BL
	For each xx in GI
		xx.color = cArray(c)
		xx.colorfull = cArray(c)
		'Select Case c
		'	Case 0: xx.fadespeedup = 120:: xx.fadespeeddown = 120
		'	Case Else: xx.fadespeedup = 0: xx.fadespeeddown = 0
		'End Select
	Next
	For each BL in BL_GI_Split_gi007: BL.color = cArray(c): Next
	For each BL in BL_GI_Split_gi009: BL.color = cArray(c): Next
	For each BL in BL_GI: BL.color = cArray(c): Next
end Sub

'sub SetInsertsColor(c)
'	Dim xx, BL
'	For each xx in AllLamps
'		'xx.color = cArray(c)
'		'xx.colorfull = cArray(c)
'		'Select Case c
'		'	Case 0: xx.fadespeedup = 120:: xx.fadespeeddown = 120
'		'	Case Else: xx.fadespeedup = 0: xx.fadespeeddown = 0
'		'End Select
'	Next
'	'gim001.color = cArray(0): gim001.colorfull = cArray(0)
'	For each BL in BL_L_l10: BL.color = cArray(c): Next
'	For each BL in BL_L_l11: BL.color = cArray(c): Next
'	For each BL in BL_L_l19: BL.color = cArray(c): Next
'	For each BL in BL_L_l20: BL.color = cArray(c): Next
'	For each BL in BL_L_l21: BL.color = cArray(c): Next
'	For each BL in BL_L_l23: BL.color = cArray(c): Next
'	For each BL in BL_L_l24: BL.color = cArray(c): Next
'	For each BL in BL_L_l25: BL.color = cArray(c): Next
'	For each BL in BL_L_l26: BL.color = cArray(c): Next
'	For each BL in BL_L_l27: BL.color = cArray(c): Next
'	For each BL in BL_L_l45: BL.color = cArray(c): Next
'	For each BL in BL_L_l46: BL.color = cArray(c): Next
'	For each BL in BL_L_l47: BL.color = cArray(c): Next
'	For each BL in BL_L_l5: BL.color = cArray(c): Next
'	For each BL in BL_L_l51: BL.color = cArray(c): Next
'	For each BL in BL_L_l6: BL.color = cArray(c): Next
'	For each BL in BL_L_l7: BL.color = cArray(c): Next
'	For each BL in BL_L_l8: BL.color = cArray(c): Next
'	For each BL in BL_L_l9: BL.color = cArray(c): Next
'	
'	For each BL in BL_LBW_l28: BL.color = cArray(c): Next
'	For each BL in BL_LBW_l29: BL.color = cArray(c): Next
'	For each BL in BL_LBW_l30: BL.color = cArray(c): Next
'	For each BL in BL_LBW_l31: BL.color = cArray(c): Next
'	For each BL in BL_LBW_l32: BL.color = cArray(c): Next
'	For each BL in BL_LBW_l33: BL.color = cArray(c): Next
'	For each BL in BL_LBW_l34: BL.color = cArray(c): Next
'	For each BL in BL_LBW_l35: BL.color = cArray(c): Next
'	For each BL in BL_LBW_l36: BL.color = cArray(c): Next
'	For each BL in BL_LBW_l37: BL.color = cArray(c): Next
'	For each BL in BL_LBW_l38: BL.color = cArray(c): Next
'	For each BL in BL_LBW_l39: BL.color = cArray(c): Next
'	For each BL in BL_LBW_l40: BL.color = cArray(c): Next
'	For each BL in BL_LBW_l41: BL.color = cArray(c): Next
'	For each BL in BL_LBW_l42: BL.color = cArray(c): Next
'	For each BL in BL_LBW_l43: BL.color = cArray(c): Next
'	For each BL in BL_LBW_l44: BL.color = cArray(c): Next	
'end Sub

sub SetFlashersColor(c)
	Dim xx, BL
	For each xx in FlasherLamps
		xx.color = cArray(c)
		xx.colorfull = cArray(c)
		'Select Case c
		'	Case 0: xx.fadespeedup = 120:: xx.fadespeeddown = 120
		'	Case Else: xx.fadespeedup = 0: xx.fadespeeddown = 0
		'End Select
	Next
	'gim001.color = cArray(0): gim001.colorfull = cArray(0)
	For each BL in BL_F_Rl1: BL.color = cArray(c): Next
	For each BL in BL_F_Rl2: BL.color = cArray(c): Next
	For each BL in BL_F_rl3: BL.color = cArray(c): Next
	For each BL in BL_F_rl4: BL.color = cArray(c): Next
	For each BL in BL_F_Rl5: BL.color = cArray(c): Next
	For each BL in BL_F_Ll1: BL.color = cArray(c): Next
	For each BL in BL_F_ll2: BL.color = cArray(c): Next
	For each BL in BL_F_ll3: BL.color = cArray(c): Next
	For each BL in BL_F_ll4: BL.color = cArray(c): Next
	For each BL in BL_F_ll5: BL.color = cArray(c): Next
	For each BL in BL_F_fr2: BL.color = cArray(c): Next
	For each BL in BL_F_fr3: BL.color = cArray(c): Next
	For each BL in BL_F_fl2: BL.color = cArray(c): Next
	For each BL in BL_F_fl3: BL.color = cArray(c): Next
end Sub

sub SetBumpersColor(c)
	Dim xx, BL
	For each xx in BumperLamps
		xx.color = cArray(c)
		xx.colorfull = cArray(c)
		'Select Case c
		'	Case 0: xx.fadespeedup = 120:: xx.fadespeeddown = 120
		'	Case Else: xx.fadespeedup = 0: xx.fadespeeddown = 0
		'End Select
	Next
	For each BL in BL_GI_Split_gi1 : BL.color = cArray(c): Next
	For each BL in BL_GI_Split_gi2: BL.color = cArray(c): Next
	For each BL in BL_GI_Split_gi3: BL.color = cArray(c): Next
	For each BL in BL_GI_Split_gi4: BL.color = cArray(c): Next
end Sub

' ========= Blink helper =========
Sub StartFlashersBlink(times, intervalMs)
    FlasherBlinkCount = 0
    FlasherBlinkMaxSteps = times * 2   ' ON+OFF per flash
    TestFlasherTimer.Interval = intervalMs
    TestFlasherTimer.Enabled = True
End Sub

' Timer object on table: Name=TestFlasherTimer, Interval=100, Enabled=False
Sub TestFlasherTimer_Timer()
    Dim xx
    If (FlasherBlinkCount Mod 2) = 0 Then
        For Each xx In FlasherLamps: xx.State = 1: Next
    Else
        For Each xx In FlasherLamps: xx.State = 0: Next
    End If

    FlasherBlinkCount = FlasherBlinkCount + 1
    If FlasherBlinkCount >= FlasherBlinkMaxSteps Then
        TestFlasherTimer.Enabled = False
        For Each xx In FlasherLamps: xx.State = 0: Next
    End If
End Sub

sub SetFlashersBrightness(v)
	Dim BL
	For each BL in BL_F_fl2: BL.opacity = v*2: Next
	For each BL in BL_F_fl3: BL.opacity = v: Next
	For each BL in BL_F_fr2: BL.opacity = v: Next
	For each BL in BL_F_fr3: BL.opacity = v: Next
	For each BL in BL_F_ll1: BL.opacity = v: Next
	For each BL in BL_F_ll2: BL.opacity = v: Next
	For each BL in BL_F_ll3: BL.opacity = v: Next
	For each BL in BL_F_ll4: BL.opacity = v: Next
	For each BL in BL_F_ll5: BL.opacity = v: Next
	For each BL in BL_F_Rl1: BL.opacity = v: Next
	For each BL in BL_F_Rl2: BL.opacity = v: Next
	For each BL in BL_F_rl3: BL.opacity = v: Next
	For each BL in BL_F_rl4: BL.opacity = v: Next
	For each BL in BL_F_Rl5: BL.opacity = v: Next
end Sub



'******************************************************
'****  END GI Control
'******************************************************

Sub UpdateMultipleLamps
	' Robot animation
	Current12 = Controller.Lamp(12)
	If Current12 And Not RobotAnimating Then
		StartRobotAnimation
	End If
	Last12 = Current12

	UpdateRobotLamps
	UpdateRobotSpin

	' Ball Release
	If Controller.Lamp(14) = True And sw74a.BallCntOver = 1 Then
		sw74a.kick 60, 12
		RandomSoundBallRelease sw74a
		UpdateTrough
	End If

End Sub

Sub UpdateRobotLamps
	' Robot lights
	Current13 = Controller.Lamp(13)
	If Current13 Then
		RobotLightAccum = RobotLightAccum + FrameTime
		If RobotLightAccum >= 95 Then
			RobotStep
			RobotLightAccum = RobotLightAccum - 95
		End If
	Else
		StopRobotLights
		RobotLightAccum = 0
	End If
	Last13 = Current13
End Sub


'*************
'Robots Lights
'*************
	
Sub StopRobotLights
	ll1.state=0:rl1.state=0:ll2.state=0:rl2.state=0:ll3.state=0:rl3.state=0:ll4.state=0:rl4.state=0:ll5.state=0:rl5.state=0
End Sub

Dim LightSeqCounter : LightSeqCounter = 0
Dim RobotLightAccum : RobotLightAccum = 0
Sub RobotStep()
	Select Case LightSeqCounter
		Case 0:
			ll1.state=1:rl1.state=1
			ll2.state=0:rl2.state=0
			ll3.state=0:rl3.state=0
			ll4.state=0:rl4.state=0
			ll5.state=0:rl5.state=0
			LightSeqCounter = LightSeqCounter + 1
		Case 1:
			ll1.state=0:rl1.state=0
			ll2.state=1:rl2.state=1
			ll3.state=0:rl3.state=0
			ll4.state=0:rl4.state=0
			ll5.state=0:rl5.state=0
			LightSeqCounter = LightSeqCounter + 1
		Case 2:
			ll1.state=0:rl1.state=0
			ll2.state=0:rl2.state=0
			ll3.state=1:rl3.state=1
			ll4.state=0:rl4.state=0
			ll5.state=0:rl5.state=0
			LightSeqCounter = LightSeqCounter + 1
		Case 3:
			ll1.state=0:rl1.state=0
			ll2.state=0:rl2.state=0
			ll3.state=0:rl3.state=0
			ll4.state=1:rl4.state=1
			ll5.state=0:rl5.state=0
			LightSeqCounter = LightSeqCounter + 1
		Case 4:
			ll1.state=0:rl1.state=0
			ll2.state=0:rl2.state=0
			ll3.state=0:rl3.state=0
			ll4.state=0:rl4.state=0
			ll5.state=1:rl5.state=1
			LightSeqCounter = 0
	End Select
End Sub


'****************
' Robot Animation

'****************

dim StartRotation
dim EndRotation
Dim CurrentRot
Dim RobotAnimating : RobotAnimating = False

StartRotation=0
EndRotation=360

Sub InitRobot
	Dim BP
	For Each BP in BP_RobotTube2 : BP.objroty = 0 : BP.visible = 0 : Next
	For Each BP in BP_RobotTube3 : BP.objroty = 0 : BP.visible = 0 : Next
	For Each BP in BP_RobotTube4 : BP.objroty = 0 : BP.visible = 0 : Next
	For Each BP in BP_RobotTube5 : BP.objroty = 0 : BP.visible = 0 : Next
	For Each BP in BP_RobotTube6 : BP.objroty = 0 : BP.visible = 0 : Next
	For Each BP in BP_RobotTube7 : BP.objroty = 0 : BP.visible = 0 : Next
	For Each BP in BP_RobotTube8 : BP.objroty = 0 : BP.visible = 0 : Next
	For Each BP in BP_RobotTube9 : BP.objroty = 0 : BP.visible = 0 : Next
	For Each BP in BP_RobotTube10 : BP.objroty = 0 : BP.visible = 0 : Next
	For Each BP in BP_RobotTube11 : BP.objroty = 0 : BP.visible = 0 : Next
	For Each BP in BP_RobotTube12 : BP.objroty = 0 : BP.visible = 0 : Next
End Sub

Sub StartRobotAnimation
	RobotAnimating = True
	CurrentRot = 0
End Sub

Sub UpdateRobotSpin
	If Not RobotAnimating Then Exit Sub
	If currentrot >= EndRotation Then
		StopRobotLights
		RobotAnimating = False
		currentrot = StartRotation
		InitRobot
		Exit Sub
	End If
	currentrot = currentrot + (0.014 * FrameTime)

	Dim BP
	For Each BP in BP_RobotTube1
		BP.objroty = currentrot
		BP.visible = currentrot < 30 Or currentrot >= 360
	Next
	For Each BP in BP_RobotTube2
		BP.objroty = currentrot
		BP.visible = currentrot >= 30 And currentrot < 60
	Next
	For Each BP in BP_RobotTube3
		BP.objroty = currentrot
		BP.visible = currentrot >= 60 And currentrot < 90
	Next
	For Each BP in BP_RobotTube4
		BP.objroty = currentrot
		BP.visible = currentrot >= 90 And currentrot < 120
	Next
	For Each BP in BP_RobotTube5
		BP.objroty = currentrot
		BP.visible = currentrot >= 120 And currentrot < 150
	Next
	For Each BP in BP_RobotTube6
		BP.objroty = currentrot
		BP.visible = currentrot >= 150 And currentrot < 180
	Next
	For Each BP in BP_RobotTube7
		BP.objroty = currentrot
		BP.visible = currentrot >= 180 And currentrot < 210
	Next
	For Each BP in BP_RobotTube8
		BP.objroty = currentrot
		BP.visible = currentrot >= 210 And currentrot < 240
	Next
	For Each BP in BP_RobotTube9
		BP.objroty = currentrot
		BP.visible = currentrot >= 240 And currentrot < 270
	Next
	For Each BP in BP_RobotTube10
		BP.objroty = currentrot
		BP.visible = currentrot >= 270 And currentrot < 300
	Next
	For Each BP in BP_RobotTube11
		BP.objroty = currentrot
		BP.visible = currentrot >= 300 And currentrot < 330
	Next
	For Each BP in BP_RobotTube12
		BP.objroty = currentrot
		BP.visible = currentrot >= 330 And currentrot < 360
	Next
End Sub

'**************
' Flipper Subs
'**************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire
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
		vpmTimer.PulseSw 75
		RF.Fire
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

Sub LeftFlipper_Animate
	Dim a: a = LeftFlipper.CurrentAngle  			' store flipper angle in a
	Dim max_angle, min_angle, mid_angle				' min and max angles from the flipper
	max_angle = LeftFlipper.StartAngle									' flipper down angle
	min_angle = LeftFlipper.EndAngle
	mid_angle = (max_angle-min_angle)/2 + min_angle ' bake map switch point angle
	FlipperLSh.RotZ = a								' set flipper shadow angle

	Dim BP : 
	For Each BP in BP_LF
		BP.RotZ = a 								' rotate the maps
		BP.visible = a > mid_angle
	Next
	For Each BP in BP_LFU
		BP.RotZ = a 								' rotate the maps
		BP.visible = a < mid_angle
	Next
End Sub

Sub RightFlipper_Animate
	Dim a: a = RightFlipper.CurrentAngle  			' store flipper angle in a
	Dim max_angle, min_angle, mid_angle				' min and max angles from the flipper
	max_angle = RightFlipper.StartAngle								' flipper down angle
	min_angle = RightFlipper.EndAngle									' flipper up angle
	mid_angle = (max_angle-min_angle)/2 + min_angle ' bake map switch point angle
	FlipperRSh.RotZ = a								' set flipper shadow angle

	Dim BP : 
	For Each BP in BP_RF
		BP.RotZ = a 								' rotate the maps
		BP.visible = a < mid_angle
	Next
	For Each BP in BP_RFU
		BP.visible = a > mid_angle
		BP.RotZ = a 								' rotate the maps
	Next
End Sub


'Triggers

' left orbit
Sub sw53_Hit():Controller.switch(53) = 1:AnimateWire BP_sw53, 1:End Sub
Sub sw53_UnHit():Controller.switch(53) = 0:AnimateWire BP_sw53, 0:End Sub

' top lanes
Sub sw42_Hit():Controller.switch(42) = 1:AnimateWire BP_sw42, 1:End Sub
Sub sw42_UnHit():Controller.switch(42) = 0:AnimateWire BP_sw42, 0:End Sub
Sub sw52_Hit():Controller.switch(52) = 1:AnimateWire BP_sw52, 1:End Sub
Sub sw52_UnHit():Controller.switch(52) = 0:AnimateWire BP_sw52, 0:End Sub
Sub sw62_Hit():Controller.switch(62) = 1:AnimateWire BP_sw62, 1:End Sub
Sub sw62_UnHit():Controller.switch(62) = 0:AnimateWire BP_sw62, 0:End Sub

' inlanes
Sub sw144_Hit():Controller.switch(44) = 1:AnimateWire BP_sw144, 1:leftInlaneSpeedLimit:End Sub
Sub sw144_UnHit():Controller.switch(44) = 0:AnimateWire BP_sw144, 0:End Sub
Sub sw44_Hit():Controller.switch(44) = 1:AnimateWire BP_sw44, 1:rightInlaneSpeedLimit:End Sub
Sub sw44_UnHit():Controller.switch(44) = 0:AnimateWire BP_sw44, 0:End Sub

' outlanes
Sub sw54_Hit():Controller.switch(54) = 1:AnimateWire BP_sw54, 1:End Sub
Sub sw54_UnHit():Controller.switch(54) = 0:AnimateWire BP_sw54, 0:End Sub
Sub sw64_Hit():Controller.switch(64) = 1:AnimateWire BP_sw64, 1:End Sub
Sub sw64_UnHit():Controller.switch(64) = 0:AnimateWire BP_sw64, 0:End Sub

' Kickers
Sub sw43_Hit():Controller.Switch(43) = 1:SoundSaucerLock:End Sub
Sub sw43_UnHit():Controller.Switch(43) = 0:SoundSaucerKick 1,sw43:End Sub
Sub sw73_Hit():Controller.Switch(73) = 1:RandomSoundMetal:End Sub
Sub sw73_UnHit():Controller.Switch(73) = 0:RandomSoundSlingshotRight BM_SlingT:End Sub

' right orbit
Sub Gate2_Hit()
	vpmTimer.pulseSw 63
End Sub

Sub Gate2_Animate()
	Dim a : a = Gate2.currentangle
	Dim BP
	For Each BP in BP_Gate2_Wire : BP.RotX = a : Next
End Sub

Sub AnimateWire(group, action) ' Action = 1 - to drop, 0 to raise)
	Dim BP
	If action = 1 Then
		For Each BP in group : BP.transz = -13 : Next
	Else
		For Each BP in group : BP.transz = 0 : Next
	End If
End Sub

Sub Gate1_Animate()
	Dim a : a = Gate1.currentangle
	Dim BP
	For Each BP in BP_Gate1_Flap : BP.RotX = a : Next
End Sub

Sub Gate4_Animate()
	Dim a : a = Gate4.currentangle
	Dim BP
	For Each BP in BP_Gate_A_8096___Flap : BP.RotX = a : Next
End Sub


'****************************
'	ZINL: Inlane speed limit code
'****************************

Sub leftInlaneSpeedLimit
	'Wylte's implementation
'    debug.print "Spin in: "& activeball.AngMomZ
'    debug.print "Speed in: "& activeball.vely
	If activeball.vely < 0 Then Exit Sub 							'don't affect upwards movement
    activeball.AngMomZ = -abs(activeball.AngMomZ) * RndNum(3,6)
    If abs(activeball.AngMomZ) > 60 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If abs(activeball.AngMomZ) > 80 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If activeball.AngMomZ > 100 Then activeball.AngMomZ = RndNum(80,100)
    If activeball.AngMomZ < -100 Then activeball.AngMomZ = RndNum(-80,-100)

    If abs(activeball.vely) > 5 Then activeball.vely = 0.8 * activeball.vely
    If abs(activeball.vely) > 10 Then activeball.vely = 0.8 * activeball.vely
    If abs(activeball.vely) > 15 Then activeball.vely = 0.8 * activeball.vely
    If activeball.vely > 16 Then activeball.vely = RndNum(14,16)
    If activeball.vely < -16 Then activeball.vely = RndNum(-14,-16)
'    debug.print "Spin out: "& activeball.AngMomZ
'    debug.print "Speed out: "& activeball.vely
End Sub


Sub rightInlaneSpeedLimit
	'Wylte's implementation
'    debug.print "Spin in: "& activeball.AngMomZ
'    debug.print "Speed in: "& activeball.vely
	If activeball.vely < 0 Then Exit Sub 							'don't affect upwards movement

    activeball.AngMomZ = abs(activeball.AngMomZ) * RndNum(2,4)
    If abs(activeball.AngMomZ) > 60 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If abs(activeball.AngMomZ) > 80 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If activeball.AngMomZ > 100 Then activeball.AngMomZ = RndNum(80,100)
    If activeball.AngMomZ < -100 Then activeball.AngMomZ = RndNum(-80,-100)

	If abs(activeball.vely) > 5 Then activeball.vely = 0.8 * activeball.vely
    If abs(activeball.vely) > 10 Then activeball.vely = 0.8 * activeball.vely
    If abs(activeball.vely) > 15 Then activeball.vely = 0.8 * activeball.vely
    If activeball.vely > 16 Then activeball.vely = RndNum(14,16)
    If activeball.vely < -16 Then activeball.vely = RndNum(-14,-16)
'    debug.print "Spin out: "& activeball.AngMomZ
'    debug.print "Speed out: "& activeball.vely
End Sub



'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub InitSlings 'hides extended slings on startup
	Dim BP
	For Each BP in BP_LSling1 : BP.Visible = 0: Next
	For Each BP in BP_LSling2 : BP.Visible = 0: Next
	For Each BP in BP_RSling1 : BP.Visible = 0: Next
	For Each BP in BP_RSling2 : BP.Visible = 0: Next
End Sub

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
	RandomSoundSlingshotLeft BM_SlingL
	vpmTimer.PulseSw(72)
	DOF 112, DOFPulse
	LStep = 0
	LeftSlingShot_Timer
	LeftSlingShot.TimerEnabled = 1
	LeftSlingShot.TimerInterval = 17
End Sub

Sub LeftSlingShot_Timer
	Dim BP
	Dim x1, x2, y: x1 = False:x2 = True:y = 20
    Select Case LStep
        Case 2:x1 = True:x2 = False:y = 10
        Case 3:x1 = False:x2 = False:y = 0:LeftSlingShot.TimerEnabled = 0
    End Select

	For Each BP in BP_LSling1 : BP.Visible = x1: Next
	For Each BP in BP_LSling2 : BP.Visible = x2: Next
	For Each BP in BP_SlingL : BP.transy = -y: Next

    LStep = LStep + 1
End Sub

'Right Slingshot

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
	RandomSoundSlingshotRight BM_SlingR
	vpmTimer.PulseSw(72)
	DOF 113, DOFPulse
	RStep = 0
	RightSlingShot_Timer
	RightSlingShot.TimerEnabled = 1
	RightSlingShot.TimerInterval = 17
End Sub

Sub RightSlingShot_Timer
	Dim BP
	Dim x1, x2, y: x1 = False:x2 = True:y = 20
    Select Case RStep
        Case 2:x1 = True:x2 = False:y = 10
        Case 3:x1 = False:x2 = False:y = 0:RightSlingShot.TimerEnabled = 0
    End Select

	For Each BP in BP_RSling1 : BP.Visible = x1: Next
	For Each BP in BP_RSling2 : BP.Visible = x2: Next
	For Each BP in BP_SlingR : BP.transy = -y: Next

    RStep = RStep + 1
End Sub

'-==============================

Sub Bumper1_Hit()
    vpmTimer.PulseSw(71)
    RandomSoundBumperTop Bumper1
	DOF 103, DOFPulse
    me.timerenabled = 1

	Dim BP
	For Each BP in BP_Bumper1_Skirt
		BP.roty=skirtAY(me,Activeball)
		BP.rotx=skirtAX(me,Activeball)
	Next

	me.timerinterval = 150
	me.timerenabled=1
End Sub

sub Bumper1_timer
	Dim BP
	For Each BP in BP_Bumper1_Skirt
		BP.roty=0
		BP.rotx=0
	Next
	me.timerenabled=0
end sub

Sub Bumper1_Animate
	Dim z: z = Bumper1.CurrentRingOffset

	Dim BP
	For Each BP in BP_Bumper1_Ring : BP.transz = z : Next
End Sub

'-------------------------------

Sub Bumper2_Hit()
    vpmTimer.PulseSw(71)
    RandomSoundBumperTop Bumper2
	DOF 104, DOFPulse

	Dim BP
	For Each BP in BP_Bumper2_Skirt
		BP.roty=skirtAY(me,Activeball)
		BP.rotx=skirtAX(me,Activeball)
	Next

	me.timerinterval = 150
	me.timerenabled=1
End Sub

sub Bumper2_timer
	Dim BP
	For Each BP in BP_Bumper2_Skirt
		BP.roty=0
		BP.rotx=0
	Next
	me.timerenabled=0
end sub

Sub Bumper2_Animate
	Dim z: z = Bumper2.CurrentRingOffset

	Dim BP
	For Each BP in BP_Bumper2_Ring : BP.transz = z : Next
End Sub

'-------------------------------

Sub Bumper3_Hit()
    vpmTimer.PulseSw(71)
    RandomSoundBumperBottom Bumper3
	DOF 105, DOFPulse
    me.timerenabled = 1

	Dim BP
	For Each BP in BP_Bumper3_Skirt
		BP.roty=skirtAY(me,Activeball)
		BP.rotx=skirtAX(me,Activeball)
	Next

	me.timerinterval = 150
	me.timerenabled=1
End Sub

sub Bumper3_timer
	Dim BP
	For Each BP in BP_Bumper3_Skirt
		BP.roty=0
		BP.rotx=0
	Next
	me.timerenabled=0
end sub

Sub Bumper3_Animate
	Dim z: z = Bumper3.CurrentRingOffset

	Dim BP
	For Each BP in BP_Bumper3_Ring : BP.transz = z : Next
End Sub

'-------------------------------

Sub Bumper4_Hit()
    vpmTimer.PulseSw(71)
    RandomSoundBumperBottom Bumper4
	DOF 106, DOFPulse

	Dim BP
	For Each BP in BP_Bumper4_Skirt
		BP.roty=skirtAY(me,Activeball)
		BP.rotx=skirtAX(me,Activeball)
	Next

	me.timerinterval = 150
	me.timerenabled=1
End Sub

sub Bumper4_timer
	Dim BP
	For Each BP in BP_Bumper4_Skirt
		BP.roty=0
		BP.rotx=0
	Next
	me.timerenabled=0
end sub

Sub Bumper4_Animate
	Dim z: z = Bumper4.CurrentRingOffset

	Dim BP
	For Each BP in BP_Bumper4_Ring : BP.transz = z : Next
End Sub



'******************************************************
'			SKIRT ANIMATION FUNCTIONS
'******************************************************
' NOTE: set bumper object timer to around 150-175 in order to be able
' to actually see the animaation, adjust to your liking

'Const PI = 3.1415926
Const SkirtTilt=5		'angle of skirt tilting in degrees

Function SkirtAX(bumper, bumperball)
	skirtAX=cos(skirtA(bumper,bumperball))*(SkirtTilt)		'x component of angle
	if (bumper.y<bumperball.y) then	skirtAX=skirtAX*-1		'adjust for ball hit bottom half
End Function

Function SkirtAY(bumper, bumperball)
	skirtAY=sin(skirtA(bumper,bumperball))*(SkirtTilt)		'y component of angle
	if (bumper.x>bumperball.x) then	skirtAY=skirtAY*-1		'adjust for ball hit left half
End Function

Function SkirtA(bumper, bumperball)
	dim hitx, hity, dx, dy
	hitx=bumperball.x
	hity=bumperball.y

	dy=Abs(hity-bumper.y)					'y offset ball at hit to center of bumper
	if dy=0 then dy=0.0000001
	dx=Abs(hitx-bumper.x)					'x offset ball at hit to center of bumper
	skirtA=(atn(dx/dy)) '/(PI/180)			'angle in radians to ball from center of Bumper1
End Function



'Ramps Top

Sub Ramptrigger01_Hit():WireRampOn True:End Sub 
Sub Ramptrigger02_Hit():WireRampOn True:End Sub 
Sub RHelp1_Hit():WireRampOff:End Sub
Sub RHelp2_Hit():WireRampOff:End Sub




'**********************************************************************************************************
'Digital Display
'**********************************************************************************************************
Dim Digits(40)
Digits(0) = Array(a00, a05, a0c, a0d, a08, a01, a06, a0f, a02, a03, a04, a07, a0b, a0a, a09, a0e)
Digits(1) = Array(a10, a15, a1c, a1d, a18, a11, a16, a1f, a12, a13, a14, a17, a1b, a1a, a19, a1e)
Digits(2) = Array(a20, a25, a2c, a2d, a28, a21, a26, a2f, a22, a23, a24, a27, a2b, a2a, a29, a2e)
Digits(3) = Array(a30, a35, a3c, a3d, a38, a31, a36, a3f, a32, a33, a34, a37, a3b, a3a, a39, a3e)
Digits(4) = Array(a40, a45, a4c, a4d, a48, a41, a46, a4f, a42, a43, a44, a47, a4b, a4a, a49, a4e)
Digits(5) = Array(a50, a55, a5c, a5d, a58, a51, a56, a5f, a52, a53, a54, a57, a5b, a5a, a59, a5e)
Digits(6) = Array(a60, a65, a6c, a6d, a68, a61, a66, a6f, a62, a63, a64, a67, a6b, a6a, a69, a6e)
Digits(7) = Array(a70, a75, a7c, a7d, a78, a71, a76, a7f, a72, a73, a74, a77, a7b, a7a, a79, a7e)
Digits(8) = Array(a80, a85, a8c, a8d, a88, a81, a86, a8f, a82, a83, a84, a87, a8b, a8a, a89, a8e)
Digits(9) = Array(a90, a95, a9c, a9d, a98, a91, a96, a9f, a92, a93, a94, a97, a9b, a9a, a99, a9e)
Digits(10) = Array(aa0, aa5, aac, aad, aa8, aa1, aa6, aaf, aa2, aa3, aa4, aa7, aab, aaa, aa9, aae)
Digits(11) = Array(ab0, ab5, abc, abd, ab8, ab1, ab6, abf, ab2, ab3, ab4, ab7, abb, aba, ab9, abe)
Digits(12) = Array(ac0, ac5, acc, acd, ac8, ac1, ac6, acf, ac2, ac3, ac4, ac7, acb, aca, ac9, ace)
Digits(13) = Array(ad0, ad5, adc, add, ad8, ad1, ad6, adf, ad2, ad3, ad4, ad7, adb, ada, ad9, ade)
Digits(14) = Array(ae0, ae5, aec, aed, ae8, ae1, ae6, aef, ae2, ae3, ae4, ae7, aeb, aea, ae9, aee)
Digits(15) = Array(af0, af5, afc, afd, af8, af1, af6, aff, af2, af3, af4, af7, afb, afa, af9, afe)

Digits(16) = Array(b00, b05, b0c, b0d, b08, b01, b06, b0f, b02, b03, b04, b07, b0b, b0a, b09, b0e)
Digits(17) = Array(b10, b15, b1c, b1d, b18, b11, b16, b1f, b12, b13, b14, b17, b1b, b1a, b19, b1e)
Digits(18) = Array(b20, b25, b2c, b2d, b28, b21, b26, b2f, b22, b23, b24, b27, b2b, b2a, b29, b2e)
Digits(19) = Array(b30, b35, b3c, b3d, b38, b31, b36, b3f, b32, b33, b34, b37, b3b, b3a, b39, b3e)
Digits(20) = Array(b40, b45, b4c, b4d, b48, b41, b46, b4f, b42, b43, b44, b47, b4b, b4a, b49, b4e)
Digits(21) = Array(b50, b55, b5c, b5d, b58, b51, b56, b5f, b52, b53, b54, b57, b5b, b5a, b59, b5e)
Digits(22) = Array(b60, b65, b6c, b6d, b68, b61, b66, b6f, b62, b63, b64, b67, b6b, b6a, b69, b6e)
Digits(23) = Array(b70, b75, b7c, b7d, b78, b71, b76, b7f, b72, b73, b74, b77, b7b, b7a, b79, b7e)
Digits(24) = Array(b80, b85, b8c, b8d, b88, b81, b86, b8f, b82, b83, b84, b87, b8b, b8a, b89, b8e)
Digits(25) = Array(b90, b95, b9c, b9d, b98, b91, b96, b9f, b92, b93, b94, b97, b9b, b9a, b99, b9e)
Digits(26) = Array(ba0, ba5, bac, bad, ba8, ba1, ba6, baf, ba2, ba3, ba4, ba7, bab, baa, ba9, bae)
Digits(27) = Array(bb0, bb5, bbc, bbd, bb8, bb1, bb6, bbf, bb2, bb3, bb4, bb7, bbb, bba, bb9, bbe)
Digits(28) = Array(bc0, bc5, bcc, bcd, bc8, bc1, bc6, bcf, bc2, bc3, bc4, bc7, bcb, bca, bc9, bce)
Digits(29) = Array(bd0, bd5, bdc, bdd, bd8, bd1, bd6, bdf, bd2, bd3, bd4, bd7, bdb, bda, bd9, bde)
Digits(30) = Array(be0, be5, bec, bed, be8, be1, be6, bef, be2, be3, be4, be7, beb, bea, be9, bee)
Digits(31) = Array(bf0, bf5, bfc, bfd, bf8, bf1, bf6, bff, bf2, bf3, bf4, bf7, bfb, bfa, bf9, bfe)

Digits(32) = Array(c00, c05, c0c, c0d, c08, c01, c06, c0f, c02, c03, c04, c07, c0b, c0a, c09, c0e)
Digits(33) = Array(c10, c15, c1c, c1d, c18, c11, c16, c1f, c12, c13, c14, c17, c1b, c1a, c19, c1e)
Digits(34) = Array(c20, c25, c2c, c2d, c28, c21, c26, c2f, c22, c23, c24, c27, c2b, c2a, c29, c2e)
Digits(35) = Array(c30, c35, c3c, c3d, c38, c31, c36, c3f, c32, c33, c34, c37, c3b, c3a, c39, c3e)
Digits(36) = Array(c40, c45, c4c, c4d, c48, c41, c46, c4f, c42, c43, c44, c47, c4b, c4a, c49, c4e)
Digits(37) = Array(c50, c55, c5c, c5d, c58, c51, c56, c5f, c52, c53, c54, c57, c5b, c5a, c59, c5e)
Digits(38) = Array(c60, c65, c6c, c6d, c68, c61, c66, c6f, c62, c63, c64, c67, c6b, c6a, c69, c6e)
Digits(39) = Array(c70, c75, c7c, c7d, c78, c71, c76, c7f, c72, c73, c74, c77, c7b, c7a, c79, c7e)

Sub DisplayTimer
    Dim ChgLED, ii, num, chg, stat, obj
    ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
		If Not IsEmpty(ChgLED)Then
			For ii=0 To UBound(chgLED)
				num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
				For Each obj In Digits(num)
					If chg And 1 Then obj.State=stat And 1
					chg=chg\2 : stat=stat\2
				Next
			Next
		End If
 End Sub

'Gottlieb Genesis
'added by Inkochnito
Sub editDips
	If SetDIPSwitches = 1 Then
	Dim vpmDips:Set vpmDips = New cvpmDips
		With vpmDips
			.AddForm 700, 400, "Genesis - DIP switches"
			.AddFrame 2, 4, 190, "Maximum credits", 49152, Array("8 credits", 0, "10 credits", 32768, "15 credits", &H00004000, "20 credits", 49152)                                                                                  'dip 15&16
			.AddFrame 2, 80, 190, "Coin chute 1 and 2 control", &H00002000, Array("seperate", 0, "same", &H00002000)                                                                                                                  'dip 14
			.AddFrame 2, 126, 190, "Playfield special", &H00200000, Array("replay", 0, "extra ball", &H00200000)                                                                                                                      'dip 22
			.AddFrame 2, 172, 190, "High games to date control", &H00000020, Array("no effect", 0, "reset high games 2-5 on power off", &H00000020)                                                                                   'dip 6
			.AddFrame 2, 218, 190, "Completing drop target sequence", &H00000080, Array("adds a letter to most complete part", 0, "spots a letter to each part", &H00000080)                                                          'dip 8
			.AddFrame 2, 264, 190, "Special lights after", &H40000000, Array("Hitting 'Lifeforce' when flashing", 0, "completing all body parts", &H40000000)                                                                         'dip 31
			.AddFrame 2, 310, 190, "Extra ball after completing", &H80000000, Array("4 body parts during the same ball", 0, "3 body parts during the same ball", &H80000000)                                                          'dip 32
			.AddFrame 205, 4, 190, "High game to date awards", &H00C00000, Array("not displayed and no award", 0, "displayed and no award", &H00800000, "displayed and 2 replays", &H00400000, "displayed and 3 replays", &H00C00000) 'dip 23&24
			.AddFrame 205, 80, 190, "Balls per game", &H01000000, Array("5 balls", 0, "3 balls", &H01000000)                                                                                                                          'dip 25
			.AddFrame 205, 126, 190, "Replay limit", &H04000000, Array("no limit", 0, "one per game", &H04000000)                                                                                                                     'dip 27
			.AddFrame 205, 172, 190, "Novelty", &H08000000, Array("normal", 0, "extra ball and replay scores points", &H08000000)                                                                                                     'dip 28
			.AddFrame 205, 218, 190, "Game mode", &H10000000, Array("replay", 0, "extra ball", &H10000000)                                                                                                                            'dip 29
			.AddFrame 205, 264, 190, "3rd coin chute credits control", &H20000000, Array("no effect", 0, "add 9", &H20000000)                                                                                                         'dip 30
			.AddChk 205, 316, 180, Array("Match feature", &H02000000)                                                                                                                                                                 'dip 26
			.AddChk 205, 331, 190, Array("Attract sound", &H00000040)                                                                                                                                                                 'dip 7
			.AddLabel 50, 360, 300, 20, "After hitting OK, press F3 to reset game with new settings."
			.ViewDips
		End With
	End If
End Sub

Set vpmShowDips = GetRef("editDips")


Sub SetDefaultDips
	If SetDIPSwitches = 0 Then
		Controller.Dip(0) = 2
		Controller.Dip(1) = 128
		Controller.Dip(2) = 192
		Controller.Dip(3) = 247
		Controller.Dip(4) = 52
		Controller.Dip(5) = 0
	End If
End Sub




'******************************************************
'	ZNFF:  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
' There are several steps for taking advantage of nFozzy's flipper solution.  At a high level we'll need the following:
'	1. flippers with specific physics settings
'	2. custom triggers for each flipper (TriggerLF, TriggerRF)
'	3. and, special scripting
'
' TriggerLF and RF should now be 27 vp units from the flippers. In addition, 3 degrees should be added to the end angle
' when creating these triggers.
'
' RF.ReProcessBalls Activeball and LF.ReProcessBalls Activeball must be added the flipper_collide subs.
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
' | EOS Torque         | 0.4            | 0.4                   | 0.375                  | 0.375              |
' | EOS Torque Angle   | 4              | 4                     | 6                      | 6                  |
'

'******************************************************
' Flippers Polarity (Select appropriate sub based on era)
'******************************************************

Dim LF : Set LF = New FlipperPolarity
Dim RF : Set RF = New FlipperPolarity

InitPolarity

'
''*******************************************
'' Mid 80's
'
Sub InitPolarity()
  dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 80
		x.DebugOn=False ' prints some info in debugger

		x.AddPt "Polarity", 0, 0, 0
		x.AddPt "Polarity", 1, 0.05, - 3.7
		x.AddPt "Polarity", 2, 0.16, - 3.7
		x.AddPt "Polarity", 3, 0.22, - 0
		x.AddPt "Polarity", 4, 0.25, - 0
		x.AddPt "Polarity", 5, 0.3, - 2
		x.AddPt "Polarity", 6, 0.4, - 3
		x.AddPt "Polarity", 7, 0.5, - 3.7
		x.AddPt "Polarity", 8, 0.65, - 2.3
		x.AddPt "Polarity", 9, 0.75, - 1.5
		x.AddPt "Polarity", 10, 0.81, - 1
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

'******************************************************
'  FLIPPER TRICKS
'******************************************************
' To add the flipper tricks you must
'	 - Include a call to FlipperCradleCollision from within OnBallBallCollision subroutine
'	 - Include a call the CheckLiveCatch from the LeftFlipper_Collide and RightFlipper_Collide subroutines
'	 - Include FlipperActivate and FlipperDeactivate in the Flipper solenoid subs

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
    'Dim BOT
    'BOT = GetBalls
	
	If Flipper1.currentangle = Endangle1 And EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'   debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then
			For b = 0 To UBound(gBOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
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
Const EOSTnew = 1.5 'EM's to late 80's - new recommendation by rothbauerw (previously 1)
'Const EOSTnew = 1.2 '90's and later - new recommendation by rothbauerw (previously 0.8)
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
		Dim b', BOT
        'BOT = GetBalls
		
		For b = 0 To UBound(gBOT)
			If Distance(gBOT(b).x, gBOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
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
'****  END FLIPPER CORRECTIONS
'******************************************************

'******************************************************
' 	ZDMP:  RUBBER  DAMPENERS
'******************************************************
' These are data mined bounce curves,
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR

' Put all the Post and Pin objects in dPosts collection. Make sure dPosts fires hit events.
Sub dPosts_Hit(idx)
	RubbersD.dampen ActiveBall
	TargetBouncer ActiveBall, 1
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



'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************

'******************************************************
' 	ZBOU: VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1	  '0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.9	 'Level of bounces. Recommmended value of 0.7-1

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
'	ZSSC: SLINGSHOT CORRECTION FUNCTIONS by apophis
'******************************************************
' To add these slingshot corrections:
'	 - On the table, add the endpoint primitives that define the two ends of the Slingshot
'	 - Initialize the SlingshotCorrection objects in InitSlingCorrection
'	 - Call the .VelocityCorrect methods from the respective _Slingshot event sub

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
	AddSlingsPt 0, 0.00, - 2
	AddSlingsPt 1, 0.40, - 5
	AddSlingsPt 2, 0.40, - 11
	AddSlingsPt 3, 0.50,	11
	AddSlingsPt 4, 0.60,	5
	AddSlingsPt 5, 1.00,	2
End Sub

Sub AddSlingsPt(idx, aX, aY)		'debugger wrapper for adjusting flipper script In-game
	Dim a
	a = Array(LS, RS)
	Dim x
	For Each x In a
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
'	dim rx, ry
'	rx = x*dCos(angle) - y*dSin(angle)
'	ry = x*dSin(angle) + y*dCos(angle)
'	RotPoint = Array(rx,ry)
'End Function

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
' 	ZRDT:  DROP TARGETS by Rothbauerw
'******************************************************
' The Stand Up and Drop Target solutions improve the physics for targets to create more realistic behavior. It allows the ball
' to move through the target enabling the ability to score more than one target with a well placed shot.
' It also handles full target animation, switch handling and deflection on hit. For drop targets there is also a slight lift when
' the drop targets raise, bricking, and popping the ball up if it's over the drop target when it raises.
'
' Add a Timers named DTAnim and STAnim to editor to handle drop & standup target animations, or run them off an always-on 10ms timer (GameTimer)
' DTAnim.interval = 10
' DTAnim.enabled = True

' Sub DTAnim_Timer
' 	DoDTAnim
'	DoSTAnim
' End Sub

' For each drop target, we'll use two wall objects for physics calculations and one primitive for visuals and
' animation. We will not use target objects.  Place your drop target primitive the same as you would a VP drop target.
' The primitive should have it's pivot point centered on the x and y axis and at or just below the playfield
' level on the z axis. Orientation needs to be set using Rotz and bending deflection using Rotx. You'll find a hooded
' target mesh in this table's example. It uses the same texture map as the VP drop targets.
'
' For each stand up target we'll use a vp target, a laid back collidable primitive, and one primitive for visuals and animation.
' The visual primitive should should have it's pivot point centered on the x and y axis and the z should be at or just below the playfield.
' The target should animate backwards using transy.
'
' To create visual target primitives that work with the stand up and drop target code, follow the below instructions:
' (Other methods will work as well, but this is easy for even non-blender users to do)
' 1) Open a new blank table. Delete everything off the table in editor.
' 2) Copy and paste the VP target from your table into this blank table.
' 3) Place the target at x = 0, y = 0  (upper left hand corner) with an orientation of 0 (target facing the front of the table)
' 4) Under the file menu, select Export "OBJ Mesh"
' 5) Go to "https://threejs.org/editor/". Here you can modify the exported obj file. When you export, it exports your target and also 
'    the playfield mesh. You need to delete the playfield mesh here. Under the file menu, chose import, and select the obj you exported
'    from VPX. In the right hand panel, find the Playfield object and click on it and delete. Then use the file menu to Export OBJ.
' 6) In VPX, you can add a primitive and use "Import Mesh" to import the exported obj from the previous step. X,Y,Z scale should be 1.
'    The primitive will use the same target texture as the VP target object. 
'
' * Note, each target must have a unique switch number. If they share a same number, add 100 to additional target with that number.
' For example, three targets with switch 32 would use 32, 132, 232 for their switch numbers.
' The 100 and 200 will be removed when setting the switch value for the target.

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
Dim DT41, DT51, DT61

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

Set DT41 = (new DropTarget)(sw41, sw41a, BM_DT_sw41, 41, 0, false)
Set DT51 = (new DropTarget)(sw51, sw51a, BM_DT_sw51, 51, 0, false)
Set DT61 = (new DropTarget)(sw61, sw61a, BM_DT_sw61, 61, 0, false)

Dim DTArray
DTArray = Array(DT41, DT51, DT61)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 80 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 42 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 5 'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 'max degrees primitive rotates when hit
Const DTDropDelay = 20 'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40 'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 30 'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 0 'Set to 0 to disable bricking, 1 to enable bricking
Const DTHitSound = "" 'Drop Target Hit sound
Const DTDropSound = "DropTarget_Down" 'Drop Target Drop sound
Const DTResetSound = "DropTarget_Up" 'Drop Target reset sound

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
	Dim i
	For i = 0 To UBound(DTArray)
		DTArray(i).animate = DTAnimate(DTArray(i).primary,DTArray(i).secondary,DTArray(i).prim,DTArray(i).sw,DTArray(i).animate)
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
			controller.Switch(Switchid mod 100) = 1
			DTAction switchid, 0
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
			'Dim BOT
			'BOT = GetBalls
			
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
		controller.Switch(Switchid mod 100) = 0
		DTAction switchid, 1
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


Sub DTAction(switchid, v)
	Select Case switchid
		Case 41
			DTShadows(0).visible = v
		Case 51
			DTShadows(1).visible = v
		Case 61
			DTShadows(2).visible = v
	End Select
End Sub


Sub AnimateDropTargets
	dim BP, tz, rx, ry

    tz = BM_DT_sw41.transz
	rx = BM_DT_sw41.rotx
	ry = BM_DT_sw41.roty
	For each BP in BP_DT_sw41 : BP.transz = tz: BP.rotx = rx: BP.roty = ry: Next

    tz = BM_DT_sw51.transz
	rx = BM_DT_sw51.rotx
	ry = BM_DT_sw51.roty
	For each BP in BP_DT_sw51 : BP.transz = tz: BP.rotx = rx: BP.roty = ry: Next

    tz = BM_DT_sw61.transz
	rx = BM_DT_sw61.rotx
	ry = BM_DT_sw61.roty
	For each BP in BP_DT_sw61 : BP.transz = tz: BP.rotx = rx: BP.roty = ry: Next

End Sub

'******************************************************
'****  END DROP TARGETS
'******************************************************


'*******************************************
'  ZKTA: KICKING TARGETS
'*******************************************

'Kicking Standups
Sub sw45_Hit: KTHit 45: End Sub		
Sub sw65_Hit: KTHit 65: End Sub		

'Kick Objects
Sub sw45col_hit: KTKick 45: End Sub
Sub sw65col_hit: KTKick 65: End Sub

Class KickingTarget
  Private m_primary, m_prim, m_sw, m_animate
  Public ball

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

'Define a variable for each kicking target
Dim KT45, KT65	'Kicking

'Set array with kicking target objects
'
'KickingTargetvar = Array(primary, prim, swtich)
'   primary:	vp target to determine target hit
'   prim:	   primitive target used for visuals and animation
'				   IMPORTANT!!!
'				   transy must be used to offset the target animation
'   switch:	 ROM switch number
'   animate:	Arrary slot for handling the animation instrucitons, set to 0
'

'TODO: Update with new primitives after toolkit import. Delete old primitives

Set KT45 = (new KickingTarget)(sw45, BM_KT_sw45, 45, 0)
Set KT65 = (new KickingTarget)(sw65, BM_KT_sw65, 65, 0)

'Add all the Kicking Target Arrays to Kicking Target Animation Array
'   KTAnimationArray = Array(KT1, KT2, ....)
Dim KTArray
KTArray = Array(KT45, KT65)

' kSpring 	- strength of the target spring (non-kicking)
' kKickVel 	- velocity imparted on the ball with the target solenoid fires
' kDist 	- distance the target must be displaced before the switch registers and the solenoid fires
' kWidth	- total width of the target
Dim kSpring, kKickVel, kDist, kWidth
kSpring = 30
kKickvel = 25
kDist = 15
kWidth = 66

'''''' KICKING TARGETS FUNCTIONS

Sub KTHit(switch)
	Dim i
	i = KTArrayID(switch)
	
	PlayTargetSound
	KTArray(i).animate = STCheckHit(Activeball,KTArray(i).primary)
	
	Set KTArray(i).ball = Activeball

	Activeball.velx = cor.BallVelx(Activeball.id)
	Activeball.vely = cor.BallVely(Activeball.id)

	DoKTAnim
End Sub

Sub KTKick(switch)
	Dim i
	i = KTArrayID(switch)
	
	KTArray(i).animate = 2
	
	Activeball.velx = cor.BallVelx(Activeball.id)
	Activeball.vely = cor.BallVely(Activeball.id)

	DoKTAnim
End Sub


Function KTArrayID(switch)
	Dim i
	For i = 0 To UBound(KTArray)
		If KTArray(i).sw = switch Then
			KTArrayID = i
			Exit Function
		End If
	Next
End Function

Sub KTAnim_Timer
	DoKTAnim
	dim bangle
End Sub

Sub DoKTAnim()
	Dim i
	For i = 0 To UBound(KTArray)
		KTArray(i).animate = KTAnimate(KTArray(i)) '(KTArray(i).primary,KTArray(i).prim,KTArray(i).sw,KTArray(i).animate,KTArray(i).ball)
	Next
End Sub

Function KTAnimate(arr) '(primary, prim, switch,  animate, aball)
	KTAnimate = arr.animate
	
	If arr.animate = 0  Then
		arr.primary.uservalue = 0
		KTAnimate = 0
		arr.primary.collidable = 1
		Exit Function
	ElseIf arr.primary.uservalue = 0 Then
		arr.primary.uservalue = GameTime
	End If

	Dim animtime, btdist, btwidth, btangle, bangle, angle, nposx, nposy, tdist, kparavel, kballvel
	tdist = 31.31

	angle = arr.primary.orientation
	
	animtime = GameTime - arr.primary.uservalue
	arr.primary.uservalue = GameTime

	nposx = arr.primary.x + arr.prim.transy * dCos(angle + 90)
	nposy = arr.primary.y + arr.prim.transy * dSin(angle + 90)
		
	btwidth = DistancePL(arr.ball.x,arr.ball.y,arr.primary.x,arr.primary.y,arr.primary.x+dcos(angle+90),arr.primary.y+dsin(angle+90))
	btdist = DistancePL(arr.ball.x,arr.ball.y,nposx,nposy,nposx+dcos(angle),nposy+dsin(angle))

	kballvel = sqr(arr.ball.velx^2 + arr.ball.vely^2)
		
	if kballvel <> 0 Then 
		bangle = arcCos(arr.ball.velx/kballvel)*180/PI
	Else
		bangle = 0
	End If
	
	If arr.ball.vely < 0 Then
		bangle = bangle * -1
	End If
		
	btangle = bangle - angle

	arr.prim.transy = arr.prim.transy + btdist - tdist

	'debug.print btangle & " " & btwidth & " " & btdist &  " " & prim.transy
		
	If arr.animate = 1 Then
		arr.primary.collidable = 0

		If btdist < tdist and btwidth < kWidth/2 + 25 Then
			if abs(arr.prim.transy) >= kDist Then
				vpmTimer.PulseSw arr.sw
				'fire Solenoid
				Select Case arr.sw:
					Case 45:
						PlaysoundAtVol "S05_LeftKickingTarget", BM_KT_sw45, Vol(arr.ball)
					Case 65:
						PlaysoundAtVol "S07_RightKickingTarget", BM_KT_sw65, Vol(arr.ball)
				End Select
				kparavel = Sqr(arr.ball.velx^2 + arr.ball.vely^2)*dCos(btangle)
				arr.ball.velx = dcos(angle)*kparavel - (kKickVel * dsin(angle))
				arr.ball.vely = dsin(angle)*kparavel + (kKickVel * dcos(angle))
				KTAnimate = 3
				'debug.print HHBall.velx & " fire " & HHBall.vely & " kpara " & kparavel  
			Else
				arr.ball.velx = arr.ball.velx - (kSpring * dsin(angle) * abs(arr.prim.transy) * animtime/1000)
				arr.ball.vely = arr.ball.vely + (kSpring * dcos(angle) * abs(arr.prim.transy) * animtime/1000)
				'debug.print HHBall.velx & " nofire " & HHBall.vely 
				'debug.print - (kSpring * dsin(angle) * abs(prim.transy) * animtime/1000) & " delta " & + (kSpring * dcos(angle) * abs(prim.transy) * animtime/1000)
			End If
		Elseif btdist > tdist then
			arr.ball.velx = arr.ball.velx - (kSpring * dsin(angle) * abs(arr.prim.transy) * animtime/1000)
			arr.ball.vely = arr.ball.vely + (kSpring * dcos(angle) * abs(arr.prim.transy) * animtime/1000)
			'debug.print HHBall.velx & " nofire " & HHBall.vely 
			'debug.print - (kSpring * dsin(angle) * abs(prim.transy) * animtime/1000) & " delta " & + (kSpring * dcos(angle) * abs(prim.transy) * animtime/1000)
			
			if arr.prim.transy >= 0 Then
				arr.prim.transy = 0 
				arr.ball = Empty 
				KTAnimate = 0
				Exit Function
			end If
		Else
			arr.prim.transy = arr.prim.transy + 1
			if arr.prim.transy >= 0 Then
				arr.prim.transy = 0 
				arr.ball = Empty 
				KTAnimate = 0
				Exit Function
			end If
		End If 
	Elseif arr.animate = 2 Then
		vpmTimer.PulseSw arr.sw
		'fire Solenoid
		RandomSoundSlingshotLeft arr.primary
		kparavel = Sqr(arr.ball.velx^2 + arr.ball.vely^2)*dCos(btangle)
		arr.ball.velx = dcos(angle)*kparavel - (kKickVel * dsin(angle))
		arr.ball.vely = dsin(angle)*kparavel + (kKickVel * dcos(angle))
		KTAnimate = 3
		'debug.print HHBall.velx & " fire2 " & HHBall.vely & " kpara " & kparavel
	Elseif arr.animate = 3 Then
		'debug.print HHBall.velx & " fire3 " & HHBall.vely & " tang " & dCos(btangle) * kballvel 

		if arr.prim.transy >= 0 Then
			arr.prim.transy = 0
			arr.ball = Empty 
			KTAnimate = 0
			Exit Function
		end If
	End If
End Function
Sub UpdateKickingTargets
	dim BP, ty

    ty = BM_KT_sw45.transy
	For each BP in BP_KT_sw45 : BP.transx = ty: Next

    ty = BM_KT_sw65.transy
	For each BP in BP_KT_sw65 : BP.transx = -ty: Next

End Sub

'******************************************************
'*   END KICKING TARGETS
'******************************************************


'Linear Target
Sub LinearTargetStart_Hit: LTHit 19: End Sub		

Class LinearTarget
  Private m_primary, m_secondary, m_prim, m_sw, m_animate

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

  Public default Function init(primary, secondary, prim, sw, animate)
    Set m_primary = primary
	Set m_secondary = secondary
    Set m_prim = prim
    m_sw = sw
    m_animate = animate

    Set Init = Me
  End Function
End Class

'Define a variable for each linear target
Dim LT19

'Set array with linear target objects
'
'LinearTargetvar = Array(primary, prim, swtich)
'   primary:		vp target to determine target hit
'   secondary:		vp target at the end of the linear target path
'   prim:		   	primitive target used for visuals and animation
'				   	IMPORTANT!!!
'				   	transy must be used to offset the target animation
'   switch:	 		ROM switch number
'   animate:		Arrary slot for handling the animation instrucitons, set to 0
'

'TODO: Update with new primitives after toolkit import. Delete old primitives


'Add all the Linear Target Arrays to Linear Target Animation Array
'   LTAnimationArray = Array(LT1, LT2, ....)
Dim LTArray


' lSpring 	- strength of the target spring
' lWidth	- total width of the target
' lReturn	- return speed of target (vp units per second)
Dim lSpring, lWidth, lReturn, lLength
lSpring = 12.5 
lWidth = 48
lReturn = 300
lLength = 120.34

'''''' LINEAR TARGET FUNCTIONS

Sub LTHit(switch)
	Dim i
	i = LTArrayID(switch)
	
	PlayTargetSound
	LTArray(i).animate = STCheckHit(ActiveBall,LTArray(i).primary)
	
	Activeball.velx = cor.BallVelx(Activeball.id)
	Activeball.vely = cor.BallVely(Activeball.id)

	DoLTAnim
End Sub

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

Function LTArrayID(switch)
	Dim i
	For i = 0 To UBound(LTArray)
		If LTArray(i).sw = switch Then
			LTArrayID = i
			Exit Function
		End If
	Next
End Function

Sub LTAnim_Timer
	DoLTAnim
	LM_GI_FishT.transy = BM_FishT.transy
End Sub

Sub DoLTAnim()
	Dim i
	For i = 0 To UBound(LTArray)
		LTArray(i).animate = LTAnimate(LTArray(i).primary,LTArray(i).secondary,LTArray(i).prim,LTArray(i).sw,LTArray(i).animate)
	Next
End Sub

Function LTAnimate(primary, secondary, prim, switch,  animate)
	LTAnimate = animate
	
	If animate = 0  Then
		primary.uservalue = 0
		LTAnimate = 0
		primary.collidable = 1
		Exit Function
	ElseIf primary.uservalue = 0 Then
		primary.uservalue = GameTime
	End If

	Dim animtime, btdist, btdist2, btwidth, btangle, bangle, angle, nposx, nposy, tdist
	tdist = 31.31

	angle = primary.orientation
	
	animtime = GameTime - primary.uservalue
	primary.uservalue = GameTime

	nposx = primary.x + prim.transy * dCos(angle + 90)
	nposy = primary.y + prim.transy * dSin(angle + 90)
	btdist = DistancePL(BCBall1.x,BCBall1.y,nposx,nposy,nposx+dcos(angle),nposy+dsin(angle))

	nposx = secondary.x
	nposy = secondary.y
	btdist2 = DistancePL(BCBall1.x,BCBall1.y,nposx,nposy,nposx+dcos(angle),nposy+dsin(angle))-tdist
		
	btwidth = DistancePL(BCBall1.x,BCBall1.y,primary.x,primary.y,primary.x+dcos(angle+90),primary.y+dsin(angle+90))

	If (btdist2 > 98 and btdist2 < 105) or (btdist2 > 63 and btdist2 < 70) or (btdist2 > 28 and btdist2 < 35) Then
		controller.Switch(Switch) = 1
	Else
		controller.Switch(Switch) = 0
	End If

	'debug.print btdist2 & " " & controller.Switch(19)

	If animate = 1 Then
		primary.collidable = 0

		If lLength + prim.transy => btdist2 and btwidth < lWidth/2 + 25 Then
			BCBall1.velx = BCBall1.velx - (lSpring * dsin(angle) * abs(prim.transy) * animtime/1000)
			BCBall1.vely = BCBall1.vely + (lSpring * dcos(angle) * abs(prim.transy) * animtime/1000)
			prim.transy =  btdist2 - lLength
		Else
			prim.transy = prim.transy + lReturn * animtime/1000
			If prim.transy > btdist2 - lLength Then
				'debug.print "Target Faster " & btdist2 & " " & prim.transy
				If btwidth < lWidth/2 + 25 Then
					BCBall1.velx = BCBall1.velx - (lSpring * dsin(angle) * abs(prim.transy) * animtime/1000)
					BCBall1.vely = BCBall1.vely + (lSpring * dcos(angle) * abs(prim.transy) * animtime/1000)
				End If
				prim.transy = btdist2 - lLength
			End If
			if prim.transy >= 0 Then
				prim.transy = 0 
				LTAnimate = 0
				Exit Function
			end If
		End If
	End If
End Function

'******************************************************
'*   END LINEAR TARGET
'******************************************************


'******************************************************
'*   VARI TARGET
'******************************************************

Sub SolVTReset(Enabled)
    If Enabled Then
        VTReset 1, Enabled
    End If
End Sub

Sub VariTargetStart_Hit: VTHit 1:End Sub

'Define a variable for each vari target
Dim VT1

'Set array with vari target objects
'
'	VariTargetvar = Array(primary, prim, swtich)
'   primary:		vp target to determine target hit
'   secondary:		vp target at the end of the vari target path
'   prim:		   	primitive target used for visuals and animation
'				   	IMPORTANT!!!
'				   	rotx must be used to offset the target animation
'   num:	 		unique number to identify the vari target
'   plength:		length from the pivot point of the primitive to the hit point/center of the target
'   width:		 	width of the vari target
'   kspring:	 	Spring strength constant
'	stops:			Number of notches in the vari target including start position, defines where the target will stop
'   rspeed:			return speed of the target in vp units per second
'   animate:		Arrary slot for handling the animation instrucitons, set to 0
'

dim v1dist: v1dist = Distance2Obj(VariTargetStart, VariTargetStop)

Set VT1 = (new VariTarget)(VariTargetStart, VariTargetStop, VariTargetP, 1, 335, 50, 2, 7, 600, 0)

' Index, distance from seconardy, switch number (the first switch should fire at the first Stop {number of stops - 1})
VT1.addpoint 0, v1dist/6, 40
VT1.addpoint 1, v1dist*2/5, 50
VT1.addpoint 2, v1dist*3/5, 60
VT1.addpoint 3, v1dist*4/5, 70

'Add all the Vari Target Arrays to Vari Target Animation Array
'   VTArray = Array(VT1, VT2, ....)
Dim VTArray
VTArray = Array(VT1)
		
Class VariTarget
	Private m_primary, m_secondary, m_prim, m_num, m_plength, m_width, m_kspring, m_stops, m_rspeed, m_animate
	Public Distances, Switches, Ball

	Public Property Get Primary(): Set Primary = m_primary: End Property
	Public Property Let Primary(input): Set m_primary = input: End Property

	Public Property Get Secondary(): Set Secondary = m_secondary: End Property
	Public Property Let Secondary(input): Set m_secondary = input: End Property

	Public Property Get Prim(): Set Prim = m_prim: End Property
	Public Property Let Prim(input): Set m_prim = input: End Property

	Public Property Get Num(): Num = m_num: End Property
	Public Property Let Num(input): m_num = input: End Property

	Public Property Get PLength(): PLength = m_plength: End Property
	Public Property Let PLength(input): m_plength = input: End Property

	Public Property Get Width(): Width = m_width: End Property
	Public Property Let Width(input): m_width = input: End Property

	Public Property Get KSpring(): KSpring = m_kspring: End Property
	Public Property Let KSpring(input): m_kspring = input: End Property

	Public Property Get Stops(): Stops = m_stops: End Property
	Public Property Let Stops(input): m_stops = input: End Property

	Public Property Get RSpeed(): RSpeed = m_rspeed: End Property
	Public Property Let RSpeed(input): m_rspeed = input: End Property

	Public Property Get Animate(): Animate = m_animate: End Property
	Public Property Let Animate(input): m_animate = input: End Property

	Public default Function init(primary, secondary, prim, num, plength, width, kspring, stops, rspeed, animate)
		Set m_primary = primary
		Set m_secondary = secondary
		Set m_prim = prim
		m_num = num
		m_plength = plength
		m_width = width
		m_kspring = kspring
		m_stops = stops
		m_rspeed = rspeed
		m_animate = animate

		Set Init = Me
		redim Distances(0)
		redim Switches(0)
	End Function

	Public Sub AddPoint(aIdx, dist, sw) 
		ShuffleArrays Distances, Switches, 1 : Distances(aIDX) = dist : Switches(aIDX) = sw : ShuffleArrays Distances, Switches, 0
	End Sub
End Class

'''''' VARI TARGET FUNCTIONS

Sub VTHit(num)
	Dim i
	i = VTArrayID(num)

	If VTArray(i).animate <> 2 Then
		VTArray(i).animate = 1 'STCheckHit(ActiveBall,VTArray(i).primary) 'We don't need STCheckHit because VariTarget geometry should only allow a valid hit
	End If
	
	 Set VTArray(i).ball = Activeball

	Activeball.velx = cor.BallVelx(Activeball.id)
	Activeball.vely = cor.BallVely(Activeball.id)

	DoVTAnim
End Sub

Sub VTReset(num, enabled)
	Dim i
	i = VTArrayID(num)

	If enabled = true then
		VTArray(i).animate = 2
	Else
		VTArray(i).animate = 1
	End If

	DoVTAnim
End Sub

Function VTArrayID(num)
	Dim i
	For i = 0 To UBound(VTArray)
		If VTArray(i).num = num Then
			VTArrayID = i
			Exit Function
		End If
	Next
End Function

Sub VTAnim_Timer
	DoVTAnim
	Cor.Update
End Sub

Sub DoVTAnim()
	Dim i
	For i = 0 To UBound(VTArray)
		VTArray(i).animate = VTAnimate(VTArray(i))
	Next
End Sub

Function VTAnimate(arr)
	VTAnimate = arr.animate

	If arr.animate = 0  Then
		arr.primary.uservalue = 0
		VTAnimate = 0
		arr.primary.collidable = 1
		Exit Function
	ElseIf arr.primary.uservalue = 0 Then
		arr.primary.uservalue = GameTime
	End If

	If arr.animate <> 0 Then 
		Dim animtime, length, btdist, btwidth, angle
		Dim tdist, transP, transPnew, cstop, x
		cstop = 0

		animtime = GameTime - arr.primary.uservalue
		arr.primary.uservalue = GameTime

		length = Distance2Obj(VariTargetStart, VariTargetStop)

		angle = arr.primary.orientation
		transP = dSin(arr.prim.rotx)*arr.plength 'previous distance target has moved from start
		transPnew = transP + arr.rspeed * animtime/1000

		If arr.animate = 1 then
			for x = 0 to (arr.Stops - 1)
				dim d: d = -length * x / (arr.Stops - 1) 'stops at end of path, remove  - 1 to stop short of the end of path
				If transP - 0.01 <= d and transPnew + 0.01 >= d Then
					transPnew = d
					cstop = d
					'debug.print x & " " & d
				End If
			next
		End If

		if not isEmpty(arr.ball) Then 
			arr.primary.collidable = 0
			tdist = 31.31 'distance between ball and target location on hit event

			btdist = DistancePL(arr.ball.x,arr.ball.y,arr.secondary.x,arr.secondary.y,arr.secondary.x+dcos(angle),arr.secondary.y+dsin(angle))-tdist 'distance between the ball and secondary target
			btwidth = DistancePL(arr.ball.x,arr.ball.y,arr.primary.x,arr.primary.y,arr.primary.x+dcos(angle+90),arr.primary.y+dsin(angle+90)) 'distance between the ball and the parallel patch of the target

			If transPnew + length => btdist and btwidth < arr.width/2 + 25 Then
				arr.ball.velx = arr.ball.velx - (arr.kspring * dsin(angle) * abs(transP) * animtime/1000)
				arr.ball.vely = arr.ball.vely + (arr.kspring * dcos(angle) * abs(transP) * animtime/1000)
				transPnew = btdist - length
				If arr.secondary.uservalue <> 1 then:PlayVTargetSound(arr.ball):arr.secondary.uservalue = 1:End If
			End If
			If btdist > length + tdist Then
				arr.ball = Empty
				arr.primary.collidable = 1
				arr.secondary.uservalue = 0
			End If
		End If

		arr.prim.rotx = dArcSin(transPnew/arr.plength)
		VTSwitch arr, transPnew	

		if arr.prim.rotx >= 0 Then
			arr.prim.rotx = 0
			VTSwitch arr, 0 
			VTAnimate = 0
			Exit Function
		elseif cstop = transPnew and isEmpty(arr.ball) and arr.animate <> 2 Then
			VTAnimate = 0
			'debug.print cstop & " " & Controller.Switch(16) & " " & Controller.Switch(26) & " " & Controller.Switch(36) & " " & Controller.Switch(46)
		end If
	End If	
End Function

Sub VTSwitch(arr, transP)
	TransVT = transP
	Dim x, count, sw
	sw = 0
	count = 0
	For each x in arr.distances
		If abs(transP) > x Then 
			sw = arr.switches(Count)
			count = count + 1
		End If
	Next
	For each x in arr.switches
		If x <> 0 Then Controller.Switch(x) = 0
	Next
	If sw <> 0 Then 
		'Debug.Print("Varitarget switch hit: "&sw)
		Controller.Switch(sw) = 1
	End If
End Sub

Sub PlayVTargetSound(ball)
	RandomSoundMetal
End Sub

Dim TransVT : TransVT = 0
Sub UpdateVaritarget
	dim BP, ty

    ty = TransVT
	For each BP in BP_VaritargetP : BP.transy = TransVT: Next
End Sub



'**********************************
' 	ZMAT: General Math Functions
'**********************************
' These get used throughout the script.

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

Function dArcSin(x)
	If X = 1 Then
		dArcSin = 90
	ElseIf x = -1 Then
		dArcSin = -90
	Else
		dArcSin = Atn(X / Sqr(-X * X + 1))*180/PI
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

Function Distance2Obj(obj1, obj2)
	Distance2Obj = SQR((obj1.x - obj2.x)^2 + (obj1.y - obj2.y)^2)
End Function

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
	'Dim BOT
	'BOT = GetBalls
	
	' stop the sound of deleted balls
	For b = UBound(gBOT) + 1 To tnob - 1
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next
	
	' exit the sub if no balls on the table
	If UBound(gBOT) =  - 1 Then Exit Sub
	
	' play the rolling sound for each ball
	For b = 0 To UBound(gBOT)
		If BallVel(gBOT(b)) > 1 And gBOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), - 1, VolPlayfieldRoll(gBOT(b)) * BallRollVolume * VolumeDial, AudioPan(gBOT(b)), 0, PitchPlayfieldRoll(gBOT(b)), 1, 0, AudioFade(gBOT(b))
		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If
		
		' Ball Drop Sounds
		If gBOT(b).VelZ <  - 1 And gBOT(b).z < 55 And gBOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If gBOT(b).velz >  - 7 Then
					RandomSoundBallBouncePlayfieldSoft gBOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard gBOT(b)
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
Const RelayGISoundLevel = 10	  'volume level; range [0, 1];

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
'****  END FLEEP MECHANICAL SOUNDS
'******************************************************

dim DisplayColor, DisplayColorG
DisplayColor =  RGB(1,37,251)

Sub VRDisplayTimer
	Dim ii, jj, obj, b, x
	Dim ChgLED,num, chg, stat
	ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
		If Not IsEmpty(ChgLED) Then
			For ii=0 To UBound(chgLED)
				num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
				For Each obj In VRDigits(num)
					If chg And 1 Then FadeDisplay obj, stat And 1	
					chg=chg\2 : stat=stat\2
				Next
			Next
		End If
End Sub

Sub FadeDisplay(object, onoffstat)
	If OnOffstat = 1 Then
		object.color = DisplayColor
	Else
		Object.Color = RGB(1,1,1)
	End If
End Sub

Dim VRDigits(40)

VRDigits(0)=Array(D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,D12,D13,D14,D15)
VRDigits(1)=Array(D16,D17,D18,D19,D20,D21,D22,D23,D24,D25,D26,D27,D28,D29,D30)
VRDigits(2)=Array(D31,D32,D33,D34,D35,D36,D37,D38,D39,D40,D41,D42,D43,D44,D45)
VRDigits(3)=Array(D46,D47,D48,D49,D50,D51,D52,D53,D54,D55,D56,D57,D58,D59,D60)
VRDigits(4)=Array(D61,D62,D63,D64,D65,D66,D67,D68,D69,D70,D71,D72,D73,D74,D75)
VRDigits(5)=Array(D76,D77,D78,D79,D80,D81,D82,D83,D84,D85,D86,D87,D88,D89,D90)
VRDigits(6)=Array(D91,D92,D93,D94,D95,D96,D97,D98,D99,D100,D101,D102,D103,D104,D105)
VRDigits(7)=Array(D106,D107,D108,D109,D110,D111,D112,D113,D114,D115,D116,D117,D118,D119,D120)
VRDigits(8)=Array(D121,D122,D123,D124,D125,D126,D127,D128,D129,D130,D131,D132,D133,D134,D135)
VRDigits(9)=Array(D136,D137,D138,D139,D140,D141,D142,D143,D144,D145,D146,D147,D148,D149,D150)
VRDigits(10)=Array(D151,D152,D153,D154,D155,D156,D157,D158,D159,D160,D161,D162,D163,D164,D165)
VRDigits(11)=Array(D166,D167,D168,D169,D170,D171,D172,D173,D174,D175,D176,D177,D178,D179,D180)
VRDigits(12)=Array(D181,D182,D183,D184,D185,D186,D187,D188,D189,D190,D191,D192,D193,D194,D195)
VRDigits(13)=Array(D196,D197,D198,D199,D200,D201,D202,D203,D204,D205,D206,D207,D208,D209,D210)
VRDigits(14)=Array(D211,D212,D213,D214,D215,D216,D217,D218,D219,D220,D221,D222,D223,D224,D225)
VRDigits(15)=Array(D226,D227,D228,D229,D230,D231,D232,D233,D234,D235,D236,D237,D238,D239,D240)

VRDigits(16)=Array(D241,D242,D243,D244,D245,D246,D247,D248,D249,D250,D251,D252,D253,D254,D255)
VRDigits(17)=Array(D256,D257,D258,D259,D260,D261,D262,D263,D264,D265,D266,D267,D268,D269,D270)
VRDigits(18)=Array(D271,D272,D273,D274,D275,D276,D277,D278,D279,D280,D281,D282,D283,D284,D285)
VRDigits(19)=Array(D286,D287,D288,D289,D290,D291,D292,D293,D294,D295,D296,D297,D298,D299,D300)
VRDigits(20)=Array(D301,D302,D303,D304,D305,D306,D307,D308,D309,D310,D311,D312,D313,D314,D315)
VRDigits(21)=Array(D316,D317,D318,D319,D320,D321,D322,D323,D324,D325,D326,D327,D328,D329,D330)
VRDigits(22)=Array(D331,D332,D333,D334,D335,D336,D337,D338,D339,D340,D341,D342,D343,D344,D345)
VRDigits(23)=Array(D346,D347,D348,D349,D350,D351,D352,D353,D354,D355,D356,D357,D358,D359,D360)
VRDigits(24)=Array(D361,D362,D363,D364,D365,D366,D367,D368,D369,D370,D371,D372,D373,D374,D375)
VRDigits(25)=Array(D376,D377,D378,D379,D380,D381,D382,D383,D384,D385,D386,D387,D388,D389,D390)
VRDigits(26)=Array(D391,D392,D393,D394,D395,D396,D397,D398,D399,D400,D401,D402,D403,D404,D405)
VRDigits(27)=Array(D406,D407,D408,D409,D410,D411,D412,D413,D414,D415,D416,D417,D418,D419,D420)
VRDigits(28)=Array(D421,D422,D423,D424,D425,D426,D427,D428,D429,D430,D431,D432,D433,D434,D435)
VRDigits(29)=Array(D436,D437,D438,D439,D440,D441,D442,D443,D444,D445,D446,D447,D448,D449,D450)
VRDigits(30)=Array(D451,D452,D453,D454,D455,D456,D457,D458,D459,D460,D461,D462,D463,D464,D465)
VRDigits(31)=Array(D466,D467,D468,D469,D470,D471,D472,D473,D474,D475,D476,D477,D478,D479,D480)

VRDigits(32)=Array(D481,D482,D483,D484,D485,D486,D487,D488,D489,D490,D491,D492,D493,D494,D495)
VRDigits(33)=Array(D496,D497,D498,D499,D500,D501,D502,D503,D504,D505,D506,D507,D508,D509,D510)
VRDigits(34)=Array(D511,D512,D513,D514,D515,D516,D517,D518,D519,D520,D521,D522,D523,D524,D525)
VRDigits(35)=Array(D526,D527,D528,D529,D530,D531,D532,D533,D534,D535,D536,D537,D538,D539,D540)
VRDigits(36)=Array(D541,D542,D543,D544,D545,D546,D547,D548,D549,D550,D551,D552,D553,D554,D555)
VRDigits(37)=Array(D556,D557,D558,D559,D560,D561,D562,D563,D564,D565,D566,D567,D568,D569,D570)
VRDigits(38)=Array(D571,D572,D573,D574,D575,D576,D577,D578,D579,D580,D581,D582,D583,D584,D585)
VRDigits(39)=Array(D586,D587,D588,D589,D590,D591,D592,D593,D594,D595,D596,D597,D598,D599,D600)

Sub InitDigits()
	dim tmp, x, obj
	for x = 0 to uBound(VRDigits)
		if IsArray(VRDigits(x) ) then
			For each obj in VRDigits(x)
				obj.height = obj.height + 18
				FadeDisplay obj, 0
			next
		end If
	Next
End Sub

If VRRoom = 1 Then
	InitDigits
End If

Dim VRThings

' Desktop Mode
if VRRoom = 0 and cab_mode = 0 Then
	for each VRThings in VRCabinet:VRThings.visible = 0:Next
	for each VRThings in VRBackglass:VRThings.visible = 0:Next

' Cabinet Mode
Elseif VRRoom = 0 and cab_mode = 1 Then
	for each VRThings in VRCabinet:VRThings.visible = 0:Next
	for each VRThings in VRBackglass:VRThings.visible = 0:Next
	For each VRThings in VRMinimalRoom: VRThings.Visible = 0: Next

' VR Mode
Else
	For Each VRThings in BP_SideRails : VRThings.Visible = 0: Next
	for each VRThings in VRCabinet:VRThings.visible = 1:Next
	for each VRThings in VRBackglass:VRThings.visible = 1:Next
	If VRRoomChoice = 0 then
		For each VRThings in VRMinimalRoom: VRThings.Visible = 1: Next
		for each VRThings in VRMegaRoom:VRThings.visible = 0:Next
	Else
		For each VRThings in VRMinimalRoom: VRThings.Visible = 0: Next
		for each VRThings in VRMegaRoom:VRThings.visible = 1:Next
	End If
End If

Sub ApplyVRRoomChoice()
    Dim o

    ' Only do anything in VR mode or VRTest
    If VRRoom <> 1 And VRTest <> 1 Then Exit Sub

    If VRRoomChoice = 0 Then
        For Each o In VRMinimalRoom: o.Visible = 1: Next
        For Each o In VRMegaRoom:    o.Visible = 0: Next
    Else
        For Each o In VRMinimalRoom: o.Visible = 0: Next
        For Each o In VRMegaRoom:    o.Visible = 1: Next
    End If
End Sub

Sub ApplyVRBackglassChoice()

    ' Only do anything in VR mode or VRTest
    If VRRoom <> 1 And VRTest <> 1 Then Exit Sub

    If VRBackglassChoice = 0 Then
		VR_Display.image = 		 "BackglassOff"
		VR_DisplayOn.image =	 "BackglassOn"
		VR_DisplayLight1.image = "BackglassLight1"
		VR_DisplayLight2.image = "BackglassLight2"
		VR_DisplayLight3.image = "BackglassLight3" 
    Else
		VR_Display.image = 		 "BackglassOffAlt"
		VR_DisplayOn.image =	 "BackglassOnAlt"
		VR_DisplayLight1.image = "BackglassLight1Alt"
		VR_DisplayLight2.image = "BackglassLight2Alt"
		VR_DisplayLight3.image = "BackglassLight3Alt" 
    End If
End Sub
