# vpx-standalone-alp4k
Settings and configs for VPX Standalone on the AtGames ALP 4K Pinball

## Paying for the Legends Unchained Loader
The Legends Unchained Loader is not for sale. You are not permitted to distribute the software in any way and are not permitted to charge a "service fee", or any other charge for assisting users with obtaining or using the software. The Legends Unchained Launcher and Legends Unchained Table Manager are Copyright (c) 2025 by Jeff Rebeiro.

If you'd like to contribute to the development of the Legends Unchained Launcher, Legends Unchained Table Manager and VPX Standalone on the ALP 4K devices, we ask that you [make a donation to St. Jude](https://www.stjude.org/give.html) in any amount you choose.

## Contributing
This repo is public and accepts Pull Requests for new table configs and updates to existing configs.

A table template [README.md](table-template_README.md) file to assist with documentation consistency across all tables.

The repo has specific naming requirements in order to work with the Legends Unchained Table Manager.

Please ensure your files are named:

| File Name | Required | Description |
|:---------:|:--------:|:-----------:|
| launcher.png | :white_check_mark: | The image used by the AtGames Launcher UI (500px x 750px)|
| table.yml | :white_check_mark: | Wizard config YAML |
| backglass.png | :x: | Backglass image to use during loading |
| dmd.png | :x: | DMD image to use during loading and as a static image for tables without a DMD |
| launcher.cfg | :x: | Any launcher configs like button labels |
| nvram.nv | :x: | NVRAM file needed for the table to initialize (should not have high-scores from play) |
| playfield.png | :x: | Playfield image to use during loading |
| table.ini | :x: | VPX settings to overried to use the table |
| table.vbs | :x: | VBS file to use instead of the one built-in to the VPX |
| VPReg.ini | :x: | Registry emulation file. If high scores are in the file, ensure the following initials are used JSM, CTH, NIX, VPX |

## Recommended Hardware
The following hardware has been verified to work with the Legends Unchained Loader. Other products may work, but these are known to work well. If you find additional compatible hardware, please let us know and we'll add it to the list!

Links to these products on Amazon are through affiliate links.

#### Flash Drives
- [Samsung FIT 256GB Flash Drive](https://amzn.to/3ymA382)
- [Samsung FIT 512GB Flash Drive](https://amzn.to/46uLC9M)
- [Silicon Power 1TB DS72](https://amzn.to/3Wws3t6)

#### Keyboards
- [Rii RK707 Keyboard/Game Controller](https://amzn.to/4fqC1oC)
- [Rii Mini](https://amzn.to/40iwZE7)
- [Logitech K400 Wireless](https://amzn.to/4iPOG6l)

## Tested tables
Minimum VPX Standalone build: 10.8.0-1983-b84441e

| Table | Playfield | Controls | Backglass | DMD | ROM Required | FPS |
|:------|:---------:|:--------:|:---------:|:---:|:------------:|:---:|
| [1-2-3 (Talleres de Llobregat 1973)](external/vpx-123) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [4 Aces (Williams 1970)](external/vpx-4aces) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [24 (Stern 2009)](external/vpx-24) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 54 |
| [250cc (Inder 1992)](external/vpx-250cc) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [301 Bullseye (Grand Products 1986)](external/vpx-301bullseye) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 57 |
| [Aaron Spelling (Data East 1992)](external/vpx-aaronspelling) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 55 |
| [Abra Ca Dabra (Gottlieb 1975)](external/vpx-abracadabra) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 49 |
| [Ace High (Gottlieb 1957)](external/vpx-acehigh) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Ace of Speed (Original 2019)](external/vpx-aceofspeed) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 30 |
| [Ace Ventura Pet Detective (Original 2019)](external/vpx-aceventura) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Addams Family, The (Bally 1992)](external/vpx-taf) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Adventures of Rocky and Bullwinkle and Friends (Data East - 1993)](external/vpx-aorab) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Aerosmith (Pro) (Stern/Tribute 2017) ](external/vpx-aerosmith) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 44 |
| [Agents 777 (Game Plan 1984)](external/vpx-agents777) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 55 |
| [Airborne (Capcom 1996)](external/vpx-airborne) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 37 |
| [Airborne Avenger (Atari 1977)](external/vpx-airavenger) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 55 |
| [Airwolf (TBA 2020)](external/vpx-airwolf) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Alaska (Interflip 1978)](external/vpx-alaska) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Algar (Williams 1980)](external/vpx-algar) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 55 |
| [Ali (Stern 1980)](external/vpx-ali) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 55 |
| [Ali, JP's (Stern 1980)](external/vpx-alijp) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Alice in Wonderland (Gottlieb 1948)](external/vpx-alice1948) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Alien Poker (Williams 1980)](external/vpx-alienpoker) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 55 |
| [Alien Star (Gottlieb 1984)](external/vpx-alienstar) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 42 |
| [Aliens (Original 2020)](external/vpx-aliens) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Al's Garage Band Goes on a World Tour (Gottlieb 1992)](external/vpx-alsgarageband) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 38 |
| [Amazing Spiderman - Sinister Six Edition (Gottlieb 1980)](external/vpx-amazingspidermansse) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 48 
| [Amazon Hunt (Gottlieb 1983)](external/vpx-amazonhunt) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [America 1492 (Juegos Populares 1986)](external/vpx-america1492) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 48 |
| [American Graffiti (Original 2024)](external/vpx-agraffiti) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 40 |
| [Andromeda (Game Plan 1985)](external/vpx-andromeda) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 58 |
| [Apache! (Taito do Brasil 1978)](external/vpx-apache) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Apollo 13 (Sega 1995)](external/vpx-apollo13) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 47 |
| [Arena (Gottlieb 1987)](external/vpx-arena1C3S) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Aspen (Brunswick 1979)](external/vpx-aspen) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Asterix The Twelve Tasks (Original 2022)](external/vpx-asterix) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Asteroid Annie (Gottlieb 1980)](external/vpx-astannie) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [A-Team, The (Original 2023)](external/vpx-theateam) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |
| [Atlantis (Bally 1989)](external/vpx-atlantis) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 48 |
| [Atlantis (Gottlieb 1975)](external/vpx-atlantisgottlieb) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 44 |
| [Attack & Revenge from Mars (Original 2015)](external/vpx-attackandrevengefrommars) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 48 |
| [Attack from Mars (Bally 1995)](external/vpx-attackfrommars) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 30 |
| [Attack of the Killer Tomatoes (MusicMOD) (Iceman 2023)](external/vpx-attackofthekillertomatoes) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Attila The Hun (Game Plan 1984)](external/vpx-atilla) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Austin Powers (Stern 2001)](external/vpx-austinpowers) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Avatar (Stern 2012)](external/vpx-avatar) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 42 |
| [Avengers LE, The - JP (Stern 2012)](external/vpx-avengersclassic) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 59 |
| [Back to the Future: The Pinball (Data East 1990)](external/vpx-bttf) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 30 |
| [Bad Cats (Williams 1989)](external/vpx-badcats) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 50 |
| [Bad Girls (Gottlieb 1988)](external/vpx-badgirls) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:x: | :white_check_mark: | 50 |
| [Balls-A-Poppin (Bally 1956)](external/vpx-ballsapoppin) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 52 |
| [Bally Game Show, The (Bally 1990)](external/vpx-ballygameshow) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 33 |
| [Banzai Run (Williams 1988)](external/vpx-banzairun) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [Barb Wire (Gottlieb 1996)](external/vpx-barbwire) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 41 |
| [Barracora (Williams 1981)](external/vpx-barracora) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 32 |
| [Baseball (Gottlieb 1970)](external/vpx-baseball1970) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 46 |
| [Batman (Data East 1991)](external/vpx-batman) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 33 |
| [Batman (The Dark Knight) (Stern 2008)](external/vpx-darkknight) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 55 |
| [Batman '66 (re-skin of Flash--Williams 1979)](external/vpx-batman66flash) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Batman BW Edition (Data East 1991)](external/vpx-batmanbw) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 37 |
| [Batman Forever (Sega 1995)](external/vpx-batmanforever) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Battlestar Galactica (Original 2018)](external/vpx-battlestargalactica) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 47 |
| [Baywatch Sega (1995)](external/vpx-baywatch) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [Beach Bums (Original 2018)](external/vpx-beachbums) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 42 |
| [BeastMaster V2 SoundFX, The (Original 2021)](external/vpx-beastmaster) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:x: | :white_check_mark: | 60 |
| [Beat The Clock (Bally 1985)](external/vpx-beattheclock) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:x: | :white_check_mark: | 34 |
| [Beavis and Butt-head: Pinballed (Original 2024)](external/vpx-bbhpinballed) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 34 |
| [Beetlejuice (Original 2023)](external/vpx-beetlejuice) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 38 |
| [Beetlejuice Movie Pinball (Original 2021)](external/vpx-beetlejuicemovie) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 48 |
| [Beverly Hills Cop (Original 2019)](external/vpx-beverlyhillscop) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 57 |
| [Big Bang Bar (Capcom 1996)](external/vpx-bigbangbar) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 50 |
| [Big Buck Hunter Pro (Stern 2010)](external/vpx-bigbuckhunter) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Big Game (Stern 1980)](external/vpx-biggame) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Big Horse (Maresa 1975)](external/vpx-bighorse) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Big House (Gottlieb 1989)](external/vpx-bighouse) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 32 |
| [Big Indian (Gottlieb 1975)](external/vpx-bigindian) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Big Lebowski, The (2025)](external/vpx-biglebowski) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |
| [Big Star (Williams 1972)](external/vpx-bigstar) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Big Trouble in Little China (Original 2022)](external/vpx-big_trouble) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Biker Mice From Mars (Original 2024)](external/vpx-bikermice) | :white_check_mark: | :white_check_mark: | :x: | :x: | :x: | 47 |
| [Bird Fly Original (2022)](external/vpx-birdfly) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Black Belt (Bally 1986)](external/vpx-blackbeltbally) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 55 |
| [Black Fever (Playmatic 1980)](external/vpx-blackfever) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:x: | :white_check_mark: | 50 |
| [Black Hole (Gottlieb 1981)](external/vpx-blackhole) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 30 |
| [Black Knight (Williams 1980)](external/vpx-blackknight) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 32 |
| [Black Knight 2000 (Williams 1989)](external/vpx-blackknight2000) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 46 |
| [Black Pyramid (Bally 1984)](external/vpx-blackpyramid) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Black Rose (Bally 1992)](external/vpx-blackrose) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 35 |
| [Blackout (Williams 1980)](external/vpx-blackout) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 30 |
| [Blood Machines (VPW Original 2022)](external/vpx-bloodmachines) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 27 |
| [BMX (Bally 1983)](external/vpx-bmx) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 55 |
| [Bobby Orr Power Play (Bally 1977)](external/vpx-bobbyorr) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 40 |
| [Bonanza (Original 2022)](external/vpx-bonanza) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 58 |
| [Bone Busters Inc. (Gottlieb 1989)](external/vpx-bonebusters) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Bounty Hunter (Gottlieb 1985)](external/vpx-bountyhunter) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [Bow and Arrow EM (Bally 1975)](external/vpx-bowandarrow) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 48 |
| [Bram Stoker's Dracula (Williams 1993)](external/vpx-bsdracula) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Brave Team (Inder 1985)](external/vpx-braveteam) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Breaking Bad (Original 2022)](external/vpx-breakingbad) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 58 |
| [Breakshot (Capcom 1996)](external/vpx-breakshot) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 28 |
| [Breakshot - Bigus MOD (Capcom 1996)](external/vpx-breakshotbigus) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Bubba the Redneck Werewolf (Original 2017)](external/vpx-bubbatheredneckwerewolf) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 40 |
| [Buck Rogers Sound MOD (Gottlieb 1980)](external/vpx-buckrogerssound) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [Bueno, el Feo, y el Malo, El (Original 2015)](external/vpx-bueno) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Buffy The Vampire Slayer (Original 2022)](external/vpx-buffy) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 52 |
| [Bugs Bunny Birthday Ball (Bally 1991)](external/vpx-bugs) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Cactus Canyon (Bally 1998)](external/vpx-cactuscanyon) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 55 |
| [Cactus Jack's (Gottlieb 1991)](external/vpx-cactusjacks) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 29 |
| [Capersville (Bally 1966)](external/vpx-capersville) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 43 |
| [Capt. Fantastic and The Brown Dirt Cowboy (Bally 1976)](external/vpx-captfantastic) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 40 |
| [Capt. Fantastic and The Brown Dirt Cowboy, JPs (Bally 1976)](external/vpx-jpcaptfantastic) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Captain Spauldings's Museum Of Monsters and Madmen (Original 2025))](external/vpx-captainspaulding) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 35 |
| [Car Hop (Gottlieb 1991)](external/vpx-carhop) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 40 |
| [Castlevania - Symphony of the Night (Original 2022)](external/vpx-castlevaniasotn) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Cat Burglars (Anthias 2024)](external/vpx-catburglars) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Cenobite (Uncle Willy 2023)](external/vpx-cenobite) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 | 
| [Centaur (Bally 1981)](external/vpx-centaur) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 35 |
| [Centigrade 37 (Gottlieb 1977)](external/vpx-centigrade37) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 43 |
| [Champion Pub, The (Williams 1998)](external/vpx-champpub) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 45 |
| [Charlie Brown Christmas, A (Original 2023)](external/vpx-acharliebrownchristmas) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Charlie's Angels (Gottlieb 1978)](external/vpx-charliesangelsremix) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 57 |
| [Checkpoint (Data East 1991)](external/vpx-checkpoint) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Cheech & Chong: Road-Trip'pin (Bally 2021)](external/vpx-cheechandchong) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 45 |
| [Cheetah (Stern 1980)](external/vpx-cheetah) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 40 |
| [Child's Play (Original 2018)](external/vpx-childsplay) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:x: | :x: | 50 |
| [Chrono Trigger (JPSalas' Pokemon Re-Theme (Original 2022)](external/vpx-chrono) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Circus (Gottlieb 1980)](external/vpx-circus) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 38 |
| [Cirqus Voltaire (Bally 1997)](external/vpx-cirqus) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 37 |
| [City Slicker (Bally 1987)](external/vpx-cityslicker) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 32 |
| [Class of 1812 (Gottlieb 1991)](external/vpx-1812) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 47 |
| [Class of 1984 (Original 2024)](external/vpx-classof1984) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Cleopatra (Gottlieb 1977)](external/vpx-cleopatra) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Clock of Eternal Fog (Original 2024)](external/vpx-clockofetfog) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Comet (Williams 1985)](external/vpx-comet) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 34 |
| [Conan (Rowament 1983)](external/vpx-conan) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 55 |
| [Congo (Williams 1995)](external/vpx-congo) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Corvette (Midway 1994)](external/vpx-corvette) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Cosmic (Taito do Brasil 1980)](external/vpx-cosmic) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Cosmic Gunfight (Williams 1982)](external/vpx-cosmicgunfight) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Cosmic Princess (Stern 1979)](external/vpx-cosmicprincess) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Count-Down (Gottlieb 1979)](external/vpx-countdownjp) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Counterforce (Gottlieb 1980)](external/vpx-counterforce) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 33 |
| [Creature from the Black Lagoon (Bally 1992)](external/vpx-cftbl) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 50 |
| [Creature From The Black Lagoon - BW Edition (1992)](external/vpx-creaturebw) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 40 | 
| [Creepshow (Original 2022)](external/vpx-creepshow) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 42 |
| [Criterium 77 (Taito do Brasil 1977)](external/vpx-criterium77) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [CSI - LED Mod (Stern 2008)](external/vpx-csiled) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 50 | 
| [Cue Ball Wizard (Gottlieb 1992)](external/vpx-cueball) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 33 |
| [Cuphead Original (Original 2019)](external/vpx-cupheadoriginal) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 47 |
| [Cuphead Pro (Original 2020)](external/vpx-cuphead) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 34 |
| [Cybernaut (Bally 1985)](external/vpx-cybernaut) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 45 |
| [Cyclone - JP (Williams 1988)](external/vpx-cyclone) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Daredevil and The Defenders (Original 2024)](external/vpx-daredevil) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Dark Crystal, The (Original 2020)](external/vpx-darkcrystal) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Deadpool - JP (Original 2021)](external/vpx-deadpool) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Death Note (Original 2020)](external/vpx-deathnote) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Death Proof (Original 2021)](external/vpx-deathproof) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Deep Purple (Original 2024)](external/vpx-deeppurple) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Defender (Williams 1982)](external/vpx-defender) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 32 |
| [Demolition Man (Williams 1994)](external/vpx-demoman) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 42 |
| [Demolition Man (Williams 1994) - Limited Cryo Edition](external/vpx-demolitionmancryo) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 42 |
| [Depeche Mode Pinball (Original 2021)](external/vpx-depechemode) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Desert City (Fipermatic 1977)](external/vpx-desertcity) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 52 |
| [Devil Riders (Zaccaria 1984)](external/vpx-devilriders) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Dick Tracy (Original 2024)](external/vpx-dicktracy) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Diner (Williams 1990)](external/vpx-diner) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Dirty Harry (Williams 1995)](external/vpx-dirtyharry) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Django Unchained (Original 2022)](external/vpx-djangounchained) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Doctor Who (Bally 1992)](external/vpx-doctorwho) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 51 |
| [Dolly Parton (Bally 1979)](external/vpx-dolly) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Doodle Bug (Williams 1971)](external/vpx-doodlebug) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 47 |
| [Doom Eternal (Original 2022)](external/vpx-doometernal) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Dr. Dude (Bally 1999)](external/vpx-drdude) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 42 |
| [Dr. Jekyll and Mr. Hyde (Original 2022)](external/vpx-drjekyllandmrhyde) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 45 |
| [Dracula (Stern 1979)](external/vpx-dracula) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 53 |
| [Dragon Ball Z (Original 2018)](external/vpx-dbz) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Dragon Ball Z Budokai (Original 2023)](external/vpx-dbzbudokai) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Drakor (Taito do Brasil 1979)](external/vpx-drakor) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Ducktales - Quest For Money (Original 2020)](external/vpx-ducktales) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Duke Nukem 3D (Original 2020)](external/vpx-dukenukem3d) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [DUNE (Original 2024)](external/vpx-dune) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 40 |
| [Dungeons & Dragons (Bally 1987)](external/vpx-dungeonsndragons) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 48 |
| [Earthshaker (Williams 1989)](external/vpx-earthshaker) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [Eight Ball Champ (Bally 1985)](external/vpx-eightballchamp) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 51 |
| [Eight Ball Deluxe (Bally 1981)](external/vpx-eightballdeluxe) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:x: | :white_check_mark: | 45 |
| [Eight Ball, JPs (Bally 1977)](external/vpx-jpeightball) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [El Dorado (Gottlieb 1975)](external/vpx-eldorado) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 43 |
| [Elvira and the Party Monsters (Bally 1989)](external/vpx-partymonsters) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 33 |
| [Elvira's House of Horrors Remix (Original 2021)](external/vpx-elvirashouseofhorrors) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 38 |
| [Elvis (Stern 2004)](external/vpx-elvis) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [Embryon (Bally 1981)](external/vpx-embryon) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 29 |
| [Escape from the Lost World (Bally 1988)](external/vpx-escapefromthelostworld) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [Evel Knievel (Bally 1977)](external/vpx-evelknievel) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 50 |
| [Evil Dead 2 - Version A (Original 2022)](external/vpx-evildead2a) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Evil Dead 2 - Version B (Original 2022)](external/vpx-evildead2b) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Evil Dead 2 (TBA 2019)](external/vpx-evildead22019) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Evil Dead 3 Army of Darkness (TBA 2020)](external/vpx-armyofdarkness) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 52 |
| [Evil Fight (Playmatic 1980)](external/vpx-evilfight) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 53 |
| [Exorcist, The (Original 2023)](external/vpx-exorcist) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [F-14 Tomcat Hanibals Mod (Williams, 1987)](external/vpx-f14tomcat) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Fallout Vault Edition (Original 2024)](external/vpx-fallout) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Family Guy (Stern 2007)](external/vpx-familyguy) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 28 |
| [Fan-Tas-Tic (Williams 1972)](external/vpx-fantastic) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Far Cry 3 - Blood Dragon (Original 2018)](external/vpx-farcry3) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Farfalla (Zaccaria 1983)](external/vpx-farfalla) | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | :white_check_mark: | 45 |
| [Fast Draw (Gottlieb 1975)](external/vpx-fastdraw) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 51 |
| [Fathom (Bally 1981)](external/vpx-fathom) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 38 |
| [Fifth Element, The (original 2022)](external/vpx-fifthelement) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Fire Action - JP (Taito do Brasil 1980)](external/vpx-jpfireaction) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Fire Action de Luxe - JP (Taito do Brasil 1983)](external/vpx-jpfireactiondeluxe) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Fire! (Williams 1987)](external/vpx-fire) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Fireball (Bally 1972)](external/vpx-fireball) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Fireball II (Bally 1981)](external/vpx-fireballii) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 55 |
| [Firepower (Williams 1980)](external/vpx-firepower) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 56 |
| [Firepower II (Williams 1983)](external/vpx-firepower2) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Fish Tales (Williams 1992)](external/vpx-fishtales) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 32 |
| [Five Nights at Freddy's (Original 2021)](external/vpx-fivenightatfreddys) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 48 |
| [Flash (Williams 1979)](external/vpx-flash) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Flash Gordon (Bally 1981)](external/vpx-flashgordon) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Flash Gordon (Sound Mod) (Bally 1981)](external/vpx-flashgordonsm) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Flashman - JP (Sportmatic 1984)](external/vpx-jpflashman) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Fleetwood Mac (Original 2024)](external/vpx-fleetwood) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 52 |
| [Flight 2000 (Stern 1980)](external/vpx-flight2000) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Flintstones, The (Williams 1994)](external/vpx-flintstones) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 30 |
| [Flipper Football (Capcom 1996)](external/vpx-flipperfootball) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 50 |
| [Fog, The (Original 2022)](external/vpx-thefog) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 52 |
| [Football, JPs (Taito do Brasil 1979)](external/vpx-jpfootball) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Fortnite (Original 2024)](external/vpx-fortnite) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Frank Thomas' Big Hurt (Gottlieb 1995)](external/vpx-frankthomas) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [Frankenstein - Balutito MOD (Original 2022)](external/vpx-frankenstein) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Frankenstein - Black and White Balutito MOD (Original 2022)](external/vpx-frankensteinbw) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Freddy A Nightmare on Elm Street (Gottlieb 1994)](external/vpx-freddy) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 33 |
| [Freddy A Nightmare on Elm Street LED Mod (Gottlieb 1994)](external/vpx-freddyled) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 35 |
| [Freefall (Stern 1981)](external/vpx-freefall) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:x: | :white_check_mark: | 60 |
| [Friday the 13th - JP (Original 2021)](external/vpx-fridaythe13th) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Frontier (Bally 1980)](external/vpx-frontier) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Full Metal Jacket (Original 2022)](external/vpx-fullmetaljacket) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [Funhouse (Williams 1990)](external/vpx-funhouse) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 30 |
| [Futurama (Original 2024)](external/vpx-futurama) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Future Spa (Bally 1979)](external/vpx-futurespa) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 57 |
| [Galáxia (LTD do Brasil 1975)](external/vpx-galaxia) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Galaxy (Stern 1980)](external/vpx-galaxy) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 42 |
| [Gamatron (Pinstar 1985)](external/vpx-gamatron) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Gargamel Park (Orginal 2016)](external/vpx-gargamelpark) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Gemini 2000 (Taito do Brasil 1982)](external/vpx-gemini2000) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 55 |
| [Genie (Gottlieb 1979)](external/vpx-genie) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 40 |
| [George Michael - Faith (Original 2023)](external/vpx-georgemichael) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 60 |
| [Getaway, The - High Speed II (Williams 1992)](external/vpx-getaway) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 42 |
| [Ghostbusters Slimer 5.0.0 - JP (Original 2017)](external/vpx-ghostbustersslimer) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [Gilligan's Island (Bally 1991)](external/vpx-gilligansisland) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Gladiators (Gottlieb 1993)](external/vpx-gladiators) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 45 |
| [Godfather, The (Original 2024)](external/vpx-godfather) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Godzilla (Sega 1998)](external/vpx-godzilla) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 48 |
| [Godzilla LE (Sega - Stern Mashup) (Original 2021)](external/vpx-godzillale) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 39 |
| [Goin' Nuts (Gottlieb 1983)](external/vpx-goinnuts) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 40 |
| [Gold Ball (Bally 1983)](external/vpx-goldball) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 44 |
| [Gold Wing (Original 2017)](external/vpx-goldwing) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Gold Wings (Gottlieb 1986)](external/vpx-goldwings) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Golden Cue (Sega 1998)](external/vpx-goldencue) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 38 |
| [Goldeneye (Sega 1996)](external/vpx-goldeneye) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Goonies, The - Never Say Die (Original 2021)](external/vpx-goonies) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 32 |
| [Gorgar (Williams 1979)](external/vpx-gorgar) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 45 |
| [GORILLAZ (Original 2024)](external/vpx-gorillaz) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 30 |
| [Grand Lizard (Williams 1986)](external/vpx-grandlizard) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Grand Prix - JP (Stern 2005)](external/vpx-grandprix) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 52 |
| [Grand Slam (Gottlieb 1972)](external/vpx-grandslam1972) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 45 |
| [Grease (Original 2020)](external/vpx-grease) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Grease B&W Pro (2021)](external/vpx-greaseblackandwhite) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Great Houdini (Original 2022)](external/vpx-greathoudini) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |
| [Gremlins (Original 2022)](external/vpx-gremlins) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Guardians of the Galaxy Trilogy (Original 2023)](external/vpx-gogtrilogy) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Guns N' Roses (Data East 1994)](external/vpx-gnr) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Guns n' Roses Remix (Original 2021)](external/vpx-gnrjj) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 48 |
| [GUNSHIP PINBALL (Music Mod) (Original 2023)](external/vpx-gunship) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 55 |
| [Hall & Oates (Original 2025)](external/vpx-hallandoates) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 45 |
| [Halloween 1978-1981 (Original 2022)](external/vpx-halloween) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Halloween MM Edition (Original 2023)](external/vpx-halloweenmm) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Hannibal Lecter (Original 2022)](external/vpx-hanniballec) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Hardbody (Bally 1987)](external/vpx-hardbody) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 48 |
| [Harlem Globetrotters On Tour, JPs (Bally 1978)](external/vpx-harlemglobetrotters) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Harley Quinn (Original 2017)](external/vpx-harleyquinn) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [Harley Davidson (Bally 1991)](external/vpx-hdavidson) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [Harley Davidson (Sega 1999)](external/vpx-harleyd) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Heavy Metal [Classic] (Rowamet 1983)](external/vpx-heavymetalclassic) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Hellboy Pinball (Original 2024)](external/vpx-hellboy) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 40 |
| [High Roller Casino (Stern 2001)](external/vpx-highrollercasino) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 | 
| [Highlander (TBA 2019)](external/vpx-highlander) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [HipHop (Original 2024)](external/vpx-hiphop) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 59 |
| [Hollywood Heat (Gottlieb 1986)](external/vpx-hollywoodheat) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Hook (Data East 1992)](external/vpx-hook) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Horrorburg - JP's (Original 2023)](external/vpx-horrorburg) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 57 |
| [Hot Ball - JP (Taito do Brasil 1979)](external/vpx-jphotball) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Humpty Dumpty (Gottlieb 1947)](external/vpx-humptydumpty)  | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Hurricane - Balutito Mod 2.0 (Williams 1991)](external/vpx-hurricane) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 40 |
| [I Dream of Jeannie (Original 2019)](external/vpx-idreamofjeannie) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Ice Fever (Gottlieb 1985)](external/vpx-icefever) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Incredible Hulk (Gottlieb 1979)](external/vpx-incrediblehulk) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Independence Day (Sega 1996)](external/vpx-id4) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 48 |
| [Indiana Jones - JP (Stern 2008)](external/vpx-indianajones) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 57 |
| [Indiana Jones The Pinball Adventure (Williams 1993)](external/vpx-indianajonestpa) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 41 |
| [Indianapolis 500 (Bally 1995)](external/vpx-indy500) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 48 |
| [Inhabiting Mars (original 2023)](external/vpx-inhabitinmars) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 43 |
| [Iron Maiden (Stern 1981)](external/vpx-ironmaiden) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Iron Man 2, JP's (Original 2018)](external/vpx-ironman2jp) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [IT - Pinball Madness - JP (Original 2022)](external/vpx-itpinballmadness) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 59 |
| [Jack Sparrow (Original 2023)](external/vpx-jacksparrow) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [Jack-Bot (Williams 1995)](external/vpx-jackbot) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Jake Mate (Petaco - 1974)](external/vpx-jakemate) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [James Bond 007 (Stern 2022)](external/vpx-jamesbond007) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [James Bond 007 60th Anniversary (Stern 2023)](external/vpx-jb00760th) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Jaws (Balutito MOD) (Original 2013)](external/vpx-jaws) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 45 |
| [Jet Spin (Gottlieb 1977)](external/vpx-jetspin) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 58 |
| [Jimi Hendrix (Original 2021)](external/vpx-jimihendrix) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Joe Bar Team (ZXR 2019)](external/vpx-joebarteam) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 52 |
| [Johnny Mnemonic (Williams 1995)](external/vpx-johnnymnemonic) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 41 |
| [Joker Poker (Gottlieb 1978)](external/vpx-jokerpoker) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 50 |
| [Jokerz! (Williams 1988)](external/vpx-jokerz) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Jolly Roger (Williams 1967)](external/vpx-jollyroger) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 51 |
| [Joust (Williams 1983)](external/vpx-joust) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Judge Dredd (Bally 1993)](external/vpx-judgedredd) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Jumanji (Original 2023)](external/vpx-jumanji) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 48 |
| [Jungle Lord (Williams 1981)](external/vpx-junglelord) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 30 | 
| [Junk Yard (Williams 1996)](external/vpx-junkyard) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Junkyard Cats (Bailey 2012)](external/vpx-junkyardcats) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 54 |
| [Jurassic Park (Data East 1993)](external/vpx-jurassicpark) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 36 |
| [Jurassic Park Pro LE (Stern 2019)](external/vpx-jurassicparkle) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :x: | 50 |
| [Kill Bill (Original 2022)](external/vpx-killbill) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Killer Instinct (Original 2024)](external/vpx-killerinstinct) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 44 |
| [KING Donkey Kong (Original 2023)](external/vpx-kingdonkeykong) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 48 |
| [King Kong (Data East 1990)](external/vpx-kingkong) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [King Tut (Bally 1969)](external/vpx-kingtut) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Kingpin (Capcom 1996)](external/vpx-kingpin) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 42 |
| [KISS (Bally 1979)](external/vpx-kissbally) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [KISS (Stern 2015)](external/vpx-kiss) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 57 |
| [Klondike (Williams 1971)](external/vpx-klondike) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Krull (Gottlieb 1983)](external/vpx-krull) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 57 |
| [Kung Fu Hustle (Original 2024)](external/vpx-kungfuhustle) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Laser Cue (Williams 1984)](external/vpx-lasercue) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 33 |
| [Laser War (Data East 1987)](external/vpx-laserwar) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 29 |
| [Last Action Hero (Balutito Mod) (Data East 1993)](external/vpx-lastactionhero) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [Last Starfighter, The (Taito, 1983)](external/vpx-thelaststarfighter) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 52 |
| [Legend - A Pinball Adventure (Original 2023)](external/vpx-legend) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 54 |
| [Legend of Zelda, The (Original 2015)](external/vpx-legendofzelda) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Leprechaun King, The (Original 2019)](external/vpx-theleprechaunking) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Lethal Weapon 3 (Data East 1992)](external/vpx-lethalweapon3) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [Lightning (Stern 1981)](external/vpx-lightning) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Lights Camera Action! (Gottlieb 1989)](external/vpx-lca) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [Lion King (Original 2020)](external/vpx-lionking) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Lord of the Rings - JP (Original 2017)](external/vpx-lordoftherings) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 49 |
| [Lost in Space Bigus (MOD) 2.0 (Sega 1998)](external/vpx-lostinspace) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Lost World (Bally 1978)](external/vpx-lostworld) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 56 |
| [Lost World Jurassic Park, The - JP (Sega 1997)](external/vpx-thelostworldjp) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 47 |
| [Lucky Seven (Williams 1978)](external/vpx-luckyseven) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Mac's Galaxy (MAC 1986)](external/vpx-macgalaxy) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Mac Jungle (MAC 1987)](external/vpx-macjungle) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Machine, Bride of Pin-Bot, The (Williams 1991)](external/vpx-bop) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 25 |
| [Mad Race - JP (Playmatic 1985)](external/vpx-jpmadrace) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Magic Castle (Zaccaria 1984)](external/vpx-magiccastle) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 52 |
| [Mandalorian, The (Stern 2021)](external/vpx-mandalorian) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 49 |
| [Mario Andretti (Gottlieb 1995)](external/vpx-marioandretti) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 33 |
| [Mars Trek (Sonic 1977)](external/vpx-marstrek) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Mary Shelley's Frankenstein (Sega 1995)](external/vpx-maryshelleyfrank) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Mask (Sound Mod) (Original 2023)](external/vpx-mask) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [Mask, The (Original 2019)](external/vpx-themask) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Masters of the Universe (Original 2018)](external/vpx-mastersoftheuniverse) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 54 |
| [Mata Hari (Bally 1978)](external/vpx-matahari) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Matrix, The (Original 2023)](external/vpx-thematrix) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 48 |
| [Maverick (Data East 1994)](external/vpx-maverick) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Medieval Madness (Williams 1997)](external/vpx-mm) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 40 |
| [Medusa (Bally 1981)](external/vpx-medusa) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 32 |
| [Medusa - JP (Bally 1981)](external/vpx-jpmedusa) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 58 |
| [Megadeth (Original 2023)](external/vpx-megadeth) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 45 |
| [Memory Lane (Stern 1978)](external/vpx-memlane) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Men in Black Trilogy (Original 2024)](external/vpx-meninblack) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Mephisto (Cirsa 1987)](external/vpx-mephisto) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Metallica Pro (JP's) (Stern 2013)](external/vpx-jpmetallica) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 60 |
| [Meteor (Stern 1979)](external/vpx-meteor) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 42 | 
| [Meteor (Taito do Brasil 1979)](external/vpx-meteort) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 58 |
| [Metropolis (Original 2022)](external/vpx-metropolis) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 46 |
| [MF DOOM (GOILL773 2024)](external/vpx-mfdoom) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :x: | 35 |
| [Miami Vice (Original 2020)](external/vpx-miamivice) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 33 |
| [Michael Jordan (Data East 1992)](external/vpx-michaeljordan) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Mickey Mouse in Steamboat Willie (Original 2022)](external/vpx-mickeysbw) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Mighty Morphin Power Rangers (Original 2024)](external/vpx-powerrangers) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 39 |
| [Minions (Original 2017)](external/vpx-minions) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 30 |
| [Miraculous (Original 2019)](external/vpx-miraculous) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Mission Impossible Limited Edition (original 2022)](external/vpx-missionimp) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Monday Night Football (Data East 1989)](external/vpx-mondaynightfootball) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 42 |
| [Monopoly (Stern 2001)](external/vpx-monopoly) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Monster Bash (Williams 1998)](external/vpx-monsterbash) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 33 |
| [Monster Bash (Williams 1998) Balutito Reskin](external/vpx-monsterbashreskin) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 47 |
| [Motordome (Bally 1986)](external/vpx-motordome) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 55 |
| [Mousin' Around! (Bally 1989)](external/vpx-mousin) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 32 |
| [Mr. and Mrs. Pac-Man (Bally 1982)](external/vpx-mrandmrspacman) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 45 |
| [Mr. Black - JP (Taito do Brasil 1984)](external/vpx-jpmrblack) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Mustang (Bullitt LE) (Stern 2014)](external/vpx-mustangbullitt) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 37 |
| [Mustang (Stern 2014)](external/vpx-mustang) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 33 |
| [Mystery Castle (Alvin G 1993)](external/vpx-mysterycastle) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Mystic, JPs (Bally 1980)](external/vpx-jpmystic) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Nascar (Stern 2005)](external/vpx-nascar) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [NBA Mac (MAC 1986)](external/vpx-nbamac) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [NBA Fastbreak (Bally 1997)](external/vpx-nbafastbreak) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Nemesis (Peyper 1986)](external/vpx-nemesis) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 50 |
| [Neverending Story, The (Original 2021)](external/vpx-tnes) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Nevermind The Bollocks (1977)](external/vpx-nmtbollocks) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [NFL (Stern 2001)](external/vpx-nfl) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 42 |
| [Night Moves (International Concepts 1989)](external/vpx-nightmoves) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 47 |
| [Night of the Living Dead '68, Pinvention's (Original 2021)](external/vpx-nightofthelivingdeadpin) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 38 |
| [Night of the Living Dead '68 Grunge Mod (Original 2018)](external/vpx-nightofthelivingdeadgrunge) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 45 |
| [Nightmare Before Christmas (Original 2024)](external/vpx-nbc) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 30 |
| [Night Rider (Bally 1977)](external/vpx-nightrider) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Nine Ball (Stern 1980)](external/vpx-nineball) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 45 |
| [Nip-It (Bally 1973)](external/vpx-nipit) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 54 |
| [Nitro Ground Shaker (Bally 1980)](external/vpx-nitrogs) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [No Fear Dangerous Sports (Williams 1995)](external/vpx-nofear) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [No Good Gofers! (Williams 1997)](external/vpx-nogoodgofers) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 42 |
| [North Star (Gottlieb 1964)](external/vpx-northstar) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 52 |
| [Nosferatu 1922 (Original 2023)](external/vpx-nosferatu) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Nugent (Stern 1978)](external/vpx-nugent) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Odisea Paris-Dakar (Pepyer 1984)](external/vpx-odisea) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 42 |
| [Old Chicago (Bally 1976)](external/vpx-oldchicago) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 58 |
| [Old Coney Island (Game Plan 1979)](external/vpx-oldconeyisland) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Operation Thunder (Gottlieb 1992)](external/vpx-opthunder) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 52 |
| [OXO (Williams 1973)](external/vpx-oxo) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 45 |
| [Pabst Can Crusher, The (Stern 2016)](external/vpx-pabstcancrusher) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 50 |
| [Panthera (Gottlieb 1980)](external/vpx-panthera) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Papa Smurf, JP's (Original 2015)](external/vpx-papasmurf) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Paragon (Bally 1979)](external/vpx-paragon) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:x: | :white_check_mark: | 49 |
| [Party Animal (Bally 1987)](external/vpx-partyanimal) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 42 |
| [Party Zone, The (Bally 1991)](external/vpx-thepartyzone) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Pat Hand (Williams 1978)](external/vpx-pathand) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :x: | 60 |
| [Pennant Fever (Williams 1984)](external/vpx-pennantfever) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | N/A |
| [Pet Sematary (TBA 2019)](external/vpx-petsemetary) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 55 |
| [Phantom of the Opera (Data East 1990)](external/vpx-phantom) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 38 |
| [Pharaoh (Williams 1981)](external/vpx-pharaoh) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 49 |
| [Phoenix (Williams 1978)](external/vpx-phoenix) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Pinball Magic (Capcom 1995)](external/vpx-pinballmagic) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 43 |
| [Pin-Bot (Williams 1986)](external/vpx-pinbot) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 33 |
| [Pink Floyd (Original 2022)](external/vpx-pinkfloyd) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 42 |
| [Pink Panther (SoundMod Hybrid) (Gottlieb 1981)](external/vpx-pinkpanther) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 37 |
| [Pirate Gold (Chicago Coin 1969)](external/vpx-pirategold) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 42 |
| [Pirates Life 2.0 - Under a Black Flag (Original 2024)](external/vpx-pirateslife) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Pirates of the Caribbean (Siggis Mod) (Stern 2006)](external/vpx-potc) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Planet Of The Apes (Original 2021)](external/vpx-planetoftheapes) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 50 |
| [Playboy (Bally 1978)](external/vpx-playboy1978) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Playboy (Stern 2002)](external/vpx-playboy2002) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 48 |
| [Playboy 35th Anniversary (Data East 1989)](external/vpx-playboy35) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 50 |
| [Playboy 35th Anniversary - Marilyn Edition (Data East 1989)](external/vpx-playboy35m) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 50 |
| [Pokemon, JPs (Original 2016)](external/vpx-jps-pokemon) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Polar Explorer - JP (Taito do Brasil 1983)](external/vpx-jppolarexplorer) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Poltergeist (Original 2022)](external/vpx-poltergeist) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Pool Sharks (Bally 1990)](external/vpx-poolsharks) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 55 |
| [Popeye Saves The Earth (Bally 1994)](external/vpx-popeye) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Predator 2 (Original 2019)](external/vpx-predator2) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Primus (Stern 2018)](external/vpx-primus) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 47 |
| [Pulp Fiction (CGC 2023)](external/vpx-pulpfiction) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Punch Out (Original 2025)](external/vpx-punchout) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Q'bert's Quest (Gottlieb 1983)](external/vpx-qbertquest) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 42 |
| [Queen: Show must go on (Original 2022)](external/vpx-queen) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Quicksilver (Stern 1980)](external/vpx-quicksilver) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 49 |
| [Radical! (Bally 1990)](external/vpx-rad) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 43 |
| [Rally - JP (Taito do Brasil 1980)](external/vpx-jprally) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 58 |
| [Rambo First Blood Part II (TBA 2020)](external/vpx-rambo) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 54 |  
| [Rambo: From first to last blood (Original 2025)](external/vpx-ramboflb) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |  
| [Ramones (Original 2021/1972)](external/vpx-ramones) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 50 |
| [Rawhide - JP (Stern 1977)](external/vpx-jprawhide) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Raygun Runner (Original 2024)](external/vpx-raygunrunner) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Real American Hero, A - Operation P.I.N.B.A.L.L. (Original 2017)](external/vpx-gijoe) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Real Ghostbusters, The (JPs GB Mod) (Original 2023)](external/vpx-realghostbusters) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 57 |
| [Re-Animator (Original 2022)](external/vpx-reanimator) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 54 |
| [Red & Ted's Road Show (Williams 1994)](external/vpx-redandteds) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Rescue 911 (Gottlieb 1994)](external/vpx-r911) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Ripley's Believe it or Not! (Stern 2004)](external/vpx-ripleys) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [Road Kings (Williams 1986)](external/vpx-roadkings) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 52 | 
| [RoboCop (Data East 1989)](external/vpx-robocop) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 38 | 
| [RoboCop 3 (Original 2018)](external/vpx-robocop3) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 | 
| [Robot (Zaccaria 1985)](external/vpx-robot) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 41 |
| [Robo-War (Gottlieb 1988)](external/vpx-robowar) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 45|
| [Rock (Gottlieb 1985)](external/vpx-rock) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Rock Encore (Gottlieb 1985)](external/vpx-rockenc) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 58 |
| [Rocky TKO (Original 2021)](external/vpx-rockytko) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 55 |
| [Roller Disco (Gottlieb 1980)](external/vpx-rollerdisco) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 45 |
| [Rollercoaster Tycoon (Stern 2002)](external/vpx-rctycn) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [RollerGames (Williams 1990)](external/vpx-rollergames) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 45 |
| [Rolling Stones - JP (Bally 1980)](external/vpx-jprollingstones) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Rolling Stones - Balutito MOD (Stern 2011)](external/vpx-rollingstonesbalutito) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Roman Victory - JP (Taito do Brasil 1978)](external/vpx-jpromanvictory) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 58 |
| [Royal Flush (Gottlieb 1976)](external/vpx-royalflush) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 57 |
| [Safe Cracker (Bally 1996)](external/vpx-safecracker) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 46 |
| [Samba (LTD do Brasil 1976)](external/vpx-samba) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 47 |
| [Sands of the Aton - JP (Original 2023)](external/vpx-sandsaton) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Scared Stiff (Bally 1996)](external/vpx-scaredstiff) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 47 |
| [Scooby Doo (Original 2022)](external/vpx-scoobydoo) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |
| [Scooby Doo! and KISS Rock n' Roll Mystery (Original 2021)](external/vpx-sdookiss) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Seawitch - JP's (Stern 1980)](external/vpx-seawitch) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Secret Agent (Original 2024)](external/vpx-secretagent) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 30 |
| [Secret Service (Data East 1988)](external/vpx-secretservice) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 45 |
| [Senna Prototype Edition (Culik Pinball 2020)](external/vpx-senna) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Serious Sam (Original 2017)](external/vpx-ssam) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 47 |
| [Sesame Street (Original 2021)](external/vpx-sesamestreet) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 50 |
| [Shadow, The (Bally 1994)](external/vpx-theshadow) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 52 |
| [Shaq Attaq (Gottlieb 1995)](external/vpx-shaq) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Shark (Taito do Brasil 1982](external/vpx-shark) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Sharkey's Shootout (Stern 2000)](external/vpx-shrkysht) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Shooting The Rapids (Zaccaria 1979)](external/vpx-shootingtherapids) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Shovel Knight (Original 2017)](external/vpx-shovel) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 48 |
| [Shrek (Stern 2008)](external/vpx-shrek) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Silverball Mania (Bally 1978)](external/vpx-silverballmania) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 55 |
| [Simpsons, The (Data East 1990)](external/vpx-thesimpsons) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Simpsons Pinball Party, The (Stern 2003)](external/vpx-simpsonspprty) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 37 |
| [Sir Lancelot (Peyper 1994)](external/vpx-sirlancelot) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 45 |
| [Six Million Dollar Man, The (Sound Mod) (Bally 1978)](external/vpx-sixmilliondollarman) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 55
| [Skateball (Bally 1980)](external/vpx-skateball) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Skylab (Williams 1974)](external/vpx-skylab) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 55 |
| [Smurfette, JP's (Original 2015)](external/vpx-smurfette) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Solar City (Gottlieb 1977)](external/vpx-solarcity) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 52 |
| [Soccer Kings (Zaccaria 1982)](external/vpx-socrking) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Sopranos, The (Stern 2005)](external/vpx-sopranos) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Sorcerer (Williams 1985)](external/vpx-sorcerer) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 43 |
| [Sorcerer, JPs (Williams 1985)](external/vpx-jpsorcerer) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [South Park (Sega 1999)](external/vpx-southpark) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 45 |
| [Space Cadet (Maxis 1995)](external/vpx-spacecadet) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Space Cadet: Galaxy Edition (Original 2021)](external/vpx-spacecadetge) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 45 |
| [Space Invaders - JP (Bally 1979)](external/vpx-jpspaceinvaders) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 55 |
| [Space Jam (Sega 1996)](external/vpx-spacejam) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 36 |
| [Space Mission (Williams 1976)](external/vpx-spacemission) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Space Oddity (Original 2022)](external/vpx-spaceoddity) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 37 |
| [Space Patrol - JP (Taito do Brasil 1978)](external/vpx-jpspacepatrol) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Space Rider (Geiger 1980)](external/vpx-spacerider) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Space Shuttle (Taito do Brasil 1982)](external/vpx-spaceshuttletaito) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Space Shuttle (Williams 1984)](external/vpx-spaceshuttle) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 48 |
| [Space Station (Williams 1987)](external/vpx-spacestation) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 38 |
| [Space Train Mac (MAC 1987)](external/vpx-spacetrainmac) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Speed Test - JP (Taito do Brasil 1982)](external/vpx-jpspeedtest) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 55 |
| [Spider-Man - JP (Stern 2007)](external/vpx-spiderman) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Spider-Man - (Vault Edition)](external/vpx-spidermanve) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 40 |
| [Splatterhouse (Original 2023)](external/vpx-splatterhouse) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Split Second (Stern 1981)](external/vpx-splitsecond) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [SpongeBob's Bikini Bottom Pinball](external/vpx-spongebob) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 35 |
| [Spy Hunter (Bally 1984)](external/vpx-spyhunter) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 42 |
| [Squid Game (Original 2024)](external/vpx-squidgame) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 48 |
| [Stampede (Stern 1977)](external/vpx-stampede) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Star Gazer (Stern 1980)](external/vpx-stargazer) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 42 |
| [Star Trek - The Mirror Universe (Zitt 2014)](external/vpx-startrekmu) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 60 |
| [Star Trek - The Next Generation (Williams 1993)](external/vpx-startrektng) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 38 |
| [Star Trek 25th Anniversary (Data East 1991)](external/vpx-startrek25th) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 38 |
| [Star Trek LE (Stern 2013)](external/vpx-startrekle) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Star Trek LE - JPs (Original 2020)](external/vpx-startreklejp) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [Star Wars (Data East 1992)](external/vpx-starwars) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 30 |
| [Star Wars Episode 1 (Original 2023)](external/vpx-starwarsepisode1) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 48 |
| [Star Wars Trilogy (Sega 1997)](external/vpx-starwarstrilogy) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 48 |
| [Stargate (Gottlieb 1995)](external/vpx-stargate) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Stars (Stern 1978)](external/vpx-stars) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 48 |
| [Starship Troopers (Sega 1997)](external/vpx-starship) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 38 |
| [Stingray (Stern 1977)](external/vpx-stingray) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 47 |
| [Strange Science (Bally 1986)](external/vpx-strangescience) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 36 |
| [Street Fighter II - JP (Gottlieb 1993)](external/vpx-sfii) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Street Fighter 2 JP's Felsir Mod (original 2021)](external/vpx-sfiimod) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Strike (Zaccaria 1978)](external/vpx-strike) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Strikes and Spares (Bally 1978)](external/vpx-strikes) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 42 |
| [Student Prince (Williams 1968)](external/vpx-studentprince) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Summer Time (Williams 1972)](external/vpx-summertime) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Super Mario Galaxy. (original 2021)](external/vpx-mariogalaxy) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 58 
| [Super Mario Bros. (Gottlieb 1992)](external/vpx-supermario) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 38 |
| [Super Mario Bros. Mushroom World (Premier 1992)](external/vpx-smbmushroom) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Super Spin (Gottlieb 1977)](external/vpx-superspin) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 48 |
| [Superman (Atari 1979)](external/vpx-superman) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [Superman and The Justice League (Original 2024)](external/vpx-supermanjl) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 58 |
| [Supersonic - JP (Bally 1979)](external/vpx-jpsupersonic) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Surf Champ (Gottlieb 1976)](external/vpx-surfchamp) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 52 |
| [Surf 'N Safari (Gottlieb 1991)](external/vpx-surfnsafari) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 47 |
| [Swamp Thing (Clairvius 2024)](external/vpx-swampthing) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |
| [Swamp Thing 2.0 Bayou Edition (LTek 2024)](external/vpx-swampthingbayou) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 45 |
| [Swords of Fury (Williams 1988)](external/vpx-swordsoffury) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 29 |
| [Tales from the Crypt (Data East 1993)](external/vpx-tftc) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 45 |
| [Tales of the Arabian Nights - JP's (Williams 1996)](external/vpx-totan) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 48 |
| [Tango & Cash (Original 2019)](external/vpx-tangoandcash) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 58 |
| [Target Alpha (Gottlieb 1976)](external/vpx-targetalpha) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 49 |
| [Taxi (Williams 1988)](external/vpx-taxi) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 34 |
| [Taxi Driver (Original 2024)](external/vpx-taxidriver) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Tee'd Off (Gottlieb 1993)](external/vpx-teedoff) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Teenage Mutant Ninja Turtles (Data East 1991) ](external/vpx-tmnt) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 40 |
| [Tenacious D (Original 2025) ](external/vpx-tenaciousd) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:x: | :white_check_mark: | 33 |
| [Terminator 1 (Original 2019)](external/vpx-terminator1) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 58 |
| [Terminator 2 - Judgment Day (Williams 1991)](external/vpx-t2) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 42 |
| [Terminator 2 - JP (Original 2020)](external/vpx-t2jp) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 60 |
| [Terminator 3 - JP (Stern 2003)](external/vpx-terminator3) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 55 |
| [Terminator 3 (Stern 2003)](external/vpx-term3) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [Texas Chainsaw Massacre 1974, The (Original 2020)](external/vpx-texaschainsawmassacre) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Theatre of Magic (Bally 1995)](external/vpx-tom) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Three Angels (Original 2018)](external/vpx-threeangels) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 40 |
| [Thing, The (Balutito MOD) (Original 2019)](external/vpx-thething) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 52 |
| [Tiki Bob's Atomic Beach Party (Original 2021)](external/vpx-tikibob) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 46 |
| [Time Fantasy (Williams 1983)](external/vpx-timefantasy) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 42 |
| [Time Machine (Zacarria 1983)](external/vpx-timemachinezac) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [TimeLord (Luigi Saves the Universe) (Original 2022)](external/vpx-timelord) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 30 |
| [Tom & Jerry (Original 2018)](external/vpx-tomjerry) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 30 |
| [Tornado Rally (Original 2024)](external/vpx-tornadorally) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 40 |
| [Torpedo Alley (Data East 1988)](external/vpx-torpedoalley) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 32 |
| [Trailer Park Boys - Pin-Ballers (Clairvius 2024)](external/vpx-trailerparkboys) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Transformers G1 Generation One (Original 2018)](external/vpx-transformersg1) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Transformers Pro (Stern 2011)](external/vpx-transformers) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Transporter: The Rescue (Bally 1989)](external/vpx-transporter) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 30 |
| [Tri Zone (Williams 1979)](external/vpx-trizone) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 51 |
| [Trick 'r Treat (Original 2023)](external/vpx-trickrtreat) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Triple X (Williams 1973)](external/vpx-triplex) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 45 |
| [Tron Classic (Original 2022)](external/vpx-tron) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 45 |
| [Tron Legacy LE (Stern 2011)](external/vpx-tronlegacy) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Truck Stop (Bally 1988)](external/vpx-truckstop) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 38 |
| [Twilight Zone (Bally 1993)](external/vpx-tz) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 42 |
| [Twisted Metal (Clairvius 2024)](external/vpx-twistedmetal) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Twister (Sega 1996)](external/vpx-twister) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Underwater (Recel 1976)](external/vpx-underwater) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Vampirella (Balutito 2024)](external/vpx-vampirella) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [Vector (Bally 1982)](external/vpx-vector) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 45 |
| [Victory (Gottlieb 1987)](external/vpx-victory) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 35 |
| [Viking (Bally 1980)](external/vpx-viking) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 42 |
| [Viper (Stern 1981)](external/vpx-viper) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Viper Night Drivin' (Sega 1998)](external/vpx-vipernight) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 37 |
| [Volkan Metal and Steel (Original 2023)](external/vpx-volkan) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Volley (Gottlieb 1976)](external/vpx-volley) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 55 |
| [Vortex (Taito do Brasil 1981)](external/vpx-vortex) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Wacky Races (Original 2022)](external/vpx-wackyraces) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 47 |
| [Walking Dead, The - JP (Stern 2014)](external/vpx-thewalkingdead) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Warlok (Williams 1982)](external/vpx-warlok) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Warriors, The (1979) Edition 2023 Reskin (Iceman 2023)](external/vpx-thewarriors) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 52 |
| [Wayne's World (Original 2020)](external/vpx-waynesworld) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 50 |
| [Wheel of Fortune (Stern 2007)](external/vpx-wof) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 37 |
| [Whirlwind (Williams 1990)](external/vpx-wwind) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 32 |
| [White Water (Williams 1993)](external/vpx-whitewater) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Who Dunnit (Bally 1995)](external/vpx-whodunnit) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Whoa Nellie! - JPs (Stern 2015)](external/vpx-whoanellie) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Who's Tommy Pinball Wizard, The (Data East 1994)](external/vpx-tommy) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Wild West (Original 2024)](external/vpx-wildwest) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [Willy Wonka Pro (Original 2020)](external/vpx-wonka) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Willy's Wonderland (Original 2021)](external/vpx-willyswonderland) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 45 |
| [Wipe Out (Gottlieb 1993)](external/vpx-wipeout) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:white_check_mark: | :white_check_mark: | 52 |
| [Witcher, The (Original 2020)](external/vpx-witcher) | :white_check_mark: | :white_check_mark: | :white_check_mark: |:x: | :white_check_mark: | 48 |
| [Wizard! (Bally 1975)](external/vpx-wizard) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Wolf Man (Peyper 1987)](external/vpx-wolfman) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 42 |
| [World Challenge Soccer (Gottlieb 1994)](external/vpx-wcsoccer) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [World Cup Soccer (Bally 1994)](external/vpx-worldcupsoccer) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 32 |
| [World Joker Tour, JP's (Original 2024)](external/vpx-jpwjt) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 48 |
| [World Poker Tour (Stern 2006)](external/vpx-wpt) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 36 |
| [WOW Monopoly, JPs (Original 2015)](external/vpx-wowmonopoly) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Wrath of Olympus - JP (Original 2022)](external/vpx-wrath) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [WWF Royal Rumble (Data East 1994)](external/vpx-royalrumble) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Xenon (Bally 1980)](external/vpx-xenon) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Xenon - JP (Bally 1980)](external/vpx-jpxenon) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [X Files (Sega 1997)](external/vpx-xfiles) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [X-Files Hanibal LED Edition (Sega 1997)](external/vpx-xfileshanibal) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 50 |
| [X-Men LE (Stern 2012)](external/vpx-xmen) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 40 |
| [Yamanobori - (KOMAYA 1981)](external/vpx-yamanobori) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | NA |
| [Yellow Submarine (Original 2021)](external/vpx-yellowsubmarine) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Young Frankenstein (hauntfreaks 2021)](external/vpx-youngfrankenstein) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 35 |
| [Zephy, aka Xenon, aka Zarza (LTD do Brasil 1982)](external/vpx-zephy) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 50 |
| [Zarza - JP (Taito do Brasil 1982)](external/vpx-jpzarza) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Zip-a-Doo (Bally 1970)](external/vpx-zipadoo) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 50 |
| [Zissou - The Life Aquatic (Original 2022)](external/vpx-zissou) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :x: | 46 |
| [Zonderik (Belgian Gaming Company 1980)](external/vpx-zonderik) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
