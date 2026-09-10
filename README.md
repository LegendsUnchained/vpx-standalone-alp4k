# vpx-standalone-alp4k
Settings and configs for VPX Standalone on the AtGames ALP 4K Pinball

## Paying for the Legends Unchained Loader
The Legends Unchained Loader is not for sale. You are not permitted to distribute the software in any way and are not permitted to charge a "service fee", or any other charge for assisting users with obtaining or using the software. The Legends Unchained Launcher and Legends Unchained Table Manager are Copyright (c) 2025 by Jeff Rebeiro.

If you'd like to contribute to the development of the Legends Unchained Launcher, Legends Unchained Table Manager and VPX Standalone on the ALP 4K devices, we ask that you [make a donation to St. Jude](https://www.stjude.org/give.html) in any amount you choose.

## Team Favorites

[`team_favorites.json`](team_favorites.json) backs the **Team Favorites** page in the
Table Manager's Add Tables wizard — a curated "start here" list, shown as one card per
team member. It is read straight from this repo (latest release tag, falling back to
`main`), so an edit here reaches cabinets without a Table Manager release.

The file is a plain array, and **cards are shown in file order** — there is no
sorting on the device, so the order here is the order players see. Add yourself
in the appropriate place rather than always at the end.

`role` is a `/`-separated list of roles, and **anyone holding the `Wizard` role
goes last**, after everyone else. Match whole values, not substrings:
`Wizard / PUP Sorcerer` holds it and belongs at the end, while
`Wizard Wrangler / Community Manager` does not — `Wizard Wrangler` is a role in
its own right.

An entry looks like:

```json
[
  {
    "member": "n-i-x",
    "avatar": "https://cdn.discordapp.com/avatars/<user-id>/<avatar-hash>.png",
    "role": "Lead Developer",
    "tables": ["vpx-deadpool", "vpx-spacecadetge", "vpx-pennantfever"]
  }
]
```

| Field | Required | Description |
|:-----:|:--------:|:-----------:|
| `member` | :white_check_mark: | Display name shown on the card |
| `tables` | :white_check_mark: | Folder names under `tables/`, **not** table display names |
| `role` | :x: | Roles held, `/`-separated, shown under the name — e.g. "Lead Developer" or "Wizard / PUP Sorcerer". Holding `Wizard` decides ordering, see above |
| `avatar` | :x: | Image URL; a person icon is shown when empty or unreachable |

Each table's box art is its own `tables/<name>/launcher.png`, and its display name,
manufacturer and year come from the wizard manifest — so nothing but the folder name
needs repeating here. A table that isn't in the manifest (renamed, or removed) is
silently skipped, and a member left with no resolvable tables drops off the page.

Tables already on the cabinet get an "Installed" badge. An adult-content table is
listed with an "NSFW" badge but can't be clicked through to an install unless the
viewer has enabled NSFW content in the wizard, so it is a fine pick — just expect
most people to see it greyed out.

## Contributing
This repo is public and accepts Pull Requests for new table configs and updates to existing configs.

A table template [README.md](table-template_README.md) file to assist with documentation consistency across all tables.

The repo has specific naming requirements in order to work with the Legends Unchained Table Manager.

Please ensure your files are named:

| File Name | Required | Description |
|:---------:|:--------:|:-----------:|
| launcher.png | :white_check_mark: | The image used by the AtGames Launcher UI (640px x 960px)|
| table.yml | :white_check_mark: | Wizard config YAML |
| alias.txt | :x: | PinMAME ROM set aliases, installed to the table's `pinmame` folder. Maps the ROM set the table script asks for onto one that is actually installed |
| backglass.png | :x: | Backglass image to use during loading |
| buttons.ini | :x: | Custom launcher button labels |
| dmd.png | :x: | DMD image to use during loading and as a static image for tables without a DMD |
| launcher.cfg | :x: | Legacy copy of `buttons.ini` during the launcher rollout |
| nvram.nv | :x: | NVRAM file needed for the table to initialize (should not have high-scores from play) |
| pinmame.ini | :x: | PinMAME per-ROM settings (DMD tint, sound, cheat). Installed to the table's `pinmame/ini` folder renamed to `<romVersion>.ini`, so the table must also have a ROM |
| playfield.png | :x: | Playfield image to use during loading |
| table.dif | :x: | VPUPatch diff. Applied to the downloaded `.vpx` with `vpxtool patch` before install, for tables whose author publishes a patch rather than a whole table |
| table.ini | :x: | VPX settings to overried to use the table |
| table.vbs | :x: | VBS file to use instead of the one built-in to the VPX |
| use_these_pup_files.zip | :x: | Replacement PUP pack files. Extracted over the installed pack's folder under `pupvideos`, so it only applies to tables whose `table.yml` sets `pupRequired` or `pupBundled` |
| VPReg.ini | :x: | Registry emulation file. If high scores are in the file, ensure the following initials are used JSM, CTH, NIX, VPX |

### Validate table.yml before committing

This repository includes a pre-commit hook that runs the same table metadata
validation and YAML linting used by GitHub Actions.

Python 3.11 or newer is required to run the validator locally:

- **Windows:** Install Python using the
  [Python Install Manager](https://docs.python.org/3/using/windows.html#python-install-manager),
  available from python.org or the Microsoft Store. Confirm that `python` works
  in a new terminal.
- **macOS:** Install the current Python 3 universal installer from
  [python.org](https://www.python.org/downloads/macos/). Confirm that `python3`
  works in a new terminal.
- **Linux:** Install Python 3 and its `venv` package using your distribution's
  package manager. Confirm that `python3` works in a terminal.

Then create an isolated environment and install the hook once for this clone.
On Windows, run:

```powershell
python -m venv .venv
.\.venv\Scripts\python.exe -m pip install pre-commit
.\.venv\Scripts\python.exe -m pre_commit install
```

On macOS or Linux, run:

```sh
python3 -m venv .venv
.venv/bin/python -m pip install pre-commit
.venv/bin/python -m pre_commit install
```

In VS Code, after installing Python, you can instead run **Tasks: Run Task** and select
**Table YAML: Set up pre-commit hook**. The
**Table YAML: Validate staged files** task runs the checks without committing.

The hook runs only when a `table.yml` file is staged. The metadata validator
uses VPSDB, so an internet connection is required when it runs. Once installed,
the same hook runs for commits made from the command line, VS Code Source
Control, or GitHub Desktop. Do not bypass the hook after a validation failure;
fix the reported `table.yml` error and commit again.

## Recommended Hardware
The following hardware has been verified to work with the Legends Unchained Loader. Other products may work, but these are known to work well. If you find additional compatible hardware, please let us know and we'll add it to the list!

Links to these products on Amazon are through affiliate links.

#### Flash Drives
- [Amazon Basics 128GB USB Flash Drive](https://amzn.to/4w0KmX0)
- [Amazon Basics 256GB USB Flash Drive](https://amzn.to/4pPivYq)
- [Amazon Basics 512GB USB Flash Drive](https://amzn.to/4bqSrgn)
- [Kingston DataTraveler Max 256GB](https://amzn.to/42Ygu1k)
- [Kingston DataTraveler Max 512GB](https://amzn.to/3ECtzFi)
- [Samsung FIT 256GB Flash Drive](https://amzn.to/3ymA382)
- [Samsung FIT 512GB Flash Drive](https://amzn.to/46uLC9M)

#### Keyboards
- [Rii RK707 Keyboard/Game Controller](https://amzn.to/4fqC1oC)
- [Rii Mini](https://amzn.to/40iwZE7)
- [Logitech K400 Wireless](https://amzn.to/4iPOG6l)

<br>


## Tables

The full table list lives in the Table Manager Wizard catalog, which is kept
up to date automatically and is searchable:

**[vpxtablemanager.com/catalog](https://vpxtablemanager.com/catalog)**
