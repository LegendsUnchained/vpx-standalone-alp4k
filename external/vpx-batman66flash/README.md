# Batman '66 (luvthecubs re-skin of Flash--Williams 1979)

![Table Preview](../../images/vpx-batman66flash-preview.jpg)

---

## Files
| File Type | Link | Version | Author |
|:---------:|:----:|:-------:|:------:|
| VPX | [VPForums](https://www.vpforums.org/index.php?app=downloads&showfile=13652) | 1.2 | [luvthecubs](https://www.vpforums.org/index.php?showuser=32651) |
| B2S | Included with VPX | N/A | N/A |
| ROM | [VPForums](https://www.vpforums.org/index.php?app=downloads&showfile=758) | flash_l1.zip | Williams |

**Tested by:** [Curt]

---

## Status 
**Minimum VPX Standalone build:** 10.8.0-1989-a764013
| Backglass | DMD | ROM Required | Has Puppack | FPS |
|-----------|-----|-----|-----|-----|
| :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |

---

## Instructions

- Install this table through the Table Manager, using the `Add Table` > `Manual` page
- If you need help, more information can be found on the wiki: [TM - Add Table - Manual](https://github.com/LegendsUnchained/vpx-standalone-alp4k/wiki/%5B04%5D-%F0%9F%A7%A1-TM-%E2%80%90-Other-Features#add-table---manual)
- If the table requires any additional files/steps, click `GO TO TABLE` after adding, and the TM will open to the relevant table folder.

## Notes
- **TO GET ALL CLASSIC "BATMAN" TV MUSIC AND SOUNDS TO PLAY IN VPX STANDALONE**, you will need to modify the table code very slightly. As of this writing, the Table Manager does not have the capability to patch binaries, so you will have to perform this step yourself, away from the 4KP. Do these steps AFTER you have installed Batman using the Table Manager. If you do this before installing the table, the hash of the file will not match, and the Table Manager will not accept the modified .vpx file.

If your computer has VPX version 10.8 or higher (No worry if you don't have a fast processor, high-end GPU, or even an HD display. You will not need to play the table here.):
  - Plug your Legends Unchained USB drive into the computer, open a File Explorer window to `external/vpx-batman66flash`, and open `Batman 66 (Original 2018).vpx` in Visual Pinball X. 
  - Open the Sound Editor in VPX (F2 in Windows).
  - Change the output location of the first 11 sounds in the list from "Table" to "Backglass."
  - Save the table, Quit VPX, eject the USB drive, and plug it back into your 4KP.

If you have a Windows computer that does not have VPX, the process is slightly more complicated:
  - Plug your Legends Unchained USB drive into a computer, and open a File Explorer window to `external/vpx-batman66flash`.
  - Open a second File Explorer window to a convenient temporary workspace (that’s not on your Legends Unchained USB), and place a copy of `Batman 66 (Original 2018).vpx` there.
  - Click on the `vpx-batman66flash.dif` at the top of this page or [here](https://github.com/LegendsUnchained/vpx-standalone-alp4k/blob/main/external/vpx-batman66flash/vpx-batman66flash.dif). From the resulting page, make sure the text next to the Download icon says “Raw,” and download `vpx-batman66flash.dif` to your temporary workspace.
  - Download and run VPUPatcher from [VPUniverse](https://vpuniverse.com/files/file/2581-vpuremix-system-vppatching-system-vpx-only/).
  - Follow the VPUniverse instructions to apply the `vpx-batman66flash.dif` to the `Batman 66 (Original 2018).vpx` file.
  - I chose the simple output name of "patched," and got the full filename you see below. These should be your results:

| File | Size (Windows Explorer)| [Size](https://md5file.com/calculator) | [Hash](https://md5file.com/calculator) |
|-----------|--------|----------|----------|
| `Batman 66 (Original 2018).vpx` | 146,616 kb | 150134784 bytes | MD5: 7f18278326ee54c1528ad9bb07172b40 |
| `patched.vpx_VPUMod.vpx` | 146,704 kb | 150224896 bytes | MD5: 2c91d62446d13e5b60c449680d1fc3e4|

  -Rename `Batman 66 (Original 2018).vpx` to `Batman 66 (Original 2018)_unpatched.vpx`, or some other convenient new name.
  - Rename your patched file to `Batman 66 (Original 2018).vpx`
  - Copy the newly-created `Batman 66 (Original 2018).vpx` to the `vpx-batman66flash` folder on your Legends Unchained USB, overwriting the original there.
  - Eject the USB drive, and plug it back into your 4KP.

I have been told that [IDAPro](https://hex-rays.com/ida-pro) with some plugins or [Binary Ninja](https://binary.ninja/), will allow a Mac or Linux computer to patch Windows executables, but I have not tried either app personally.

If you do not have a computer at all: I’m sorry, you will have to live without BOOM! POW! for a while. But you could let the moderators know that Table Manager patching is a high priority for you.

---
"To the Batcave!"