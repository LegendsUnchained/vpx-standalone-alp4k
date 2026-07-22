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
- [Kingston DataTraveler Max 256GB](https://amzn.to/42Ygu1k)
- [Kingston DataTraveler Max 512GB](https://amzn.to/3ECtzFi)
- [Samsung FIT 256GB Flash Drive](https://amzn.to/3ymA382)
- [Samsung FIT 512GB Flash Drive](https://amzn.to/46uLC9M)

#### Keyboards
- [Rii RK707 Keyboard/Game Controller](https://amzn.to/4fqC1oC)
- [Rii Mini](https://amzn.to/40iwZE7)
- [Logitech K400 Wireless](https://amzn.to/4iPOG6l)

<br>


## Wizard Tables 

| Table | Backglass | DMD | ROM Required | Has Puppack | FPS |
|:------|:---------:|:---:|:------------:|:---:|:---:|
| [1-2-3 (Talleres de Llobregat 1973)](external/vpx-123) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [2001 - A Space Odyssey (Original 2025)](external/vpx-2001) | :x: | :x: | :x: | :white_check_mark: | 55 |
| [24 (Stern 2009)](external/vpx-24) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [250 cc (Inder 1992)](external/vpx-250cc) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [4 Aces (Williams 1970)](external/vpx-4aces) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Aaron Spelling (Data East 1992)](external/vpx-aaronspelling) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 52 |
| [Ace High (Gottlieb 1957)](external/vpx-acehigh) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Addams Family, JP's (Bally 1992)](external/vpx-jptaf) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Adventures of Rocky and Bullwinkle and Friends (Data East 1993)](external/vpx-aorab) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Al's Garage Band Goes on a World Tour (Gottlieb 1992)](external/vpx-alsgarageband) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Aladdin's Castle (Bally 1976)](external/vpx-acastle) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Alaska (Interflip 1978)](external/vpx-alaska) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Ali (JP's, Stern 1980)](external/vpx-alijp) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Alice in Wonderland (Gottlieb 1948)](external/vpx-alice1948) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Amazon Hunt (Gottlieb 1983)](external/vpx-amazonhunt) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Andromeda (Game Plan 1985)](external/vpx-andromeda) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Apache! (Taito do Brasil 1978)](external/vpx-apache) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Apollo 13 (Sega 1995)](external/vpx-apollo13) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Asterix the Twelve Tasks (Original 2022)](external/vpx-asterix) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Asteroid Annie (Gottlieb 1980)](external/vpx-astannie) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Attack from Mars LE, JP's (Chicago Gaming Company 2017)](external/vpx-jpattackfrommarsle) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Attack from Mars, JP's (Bally 1995)](external/vpx-jpattackfrommars) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Austin Powers (Stern 2001)](external/vpx-austinpowers) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Avengers Pro (Stern 2012)](external/vpx-avengers) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Avengers, JP's The (Stern 2012)](external/vpx-jpavengersclassic) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Baby Pac-Man (Bally 1982)](external/vpx-babypacman) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Back to the Future: The Pinball (Data East 1990)](external/vpx-bttf) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |
| [Bad Cats (Williams 1989)](external/vpx-badcats) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Banzai Run (Williams 1988)](external/vpx-banzairun) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Batman '66 (Original 2018)](external/vpx-batman66flash) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Batman (Data East 1991)](external/vpx-batman) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Batman - The Dark Knight (Stern 2008)](external/vpx-darkknight) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Batman Forever (Sega 1995)](external/vpx-batmanforever) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Baywatch (Sega 1995)](external/vpx-baywatch) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Big Bang Bar (Capcom 1996)](external/vpx-bigbangbar) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Big Buck Hunter Pro (Stern 2010)](external/vpx-bigbuckhunter) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Big Shot (Gottlieb 1974)](external/vpx-bigshot) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Big Trouble in Little China (Original 2022)](external/vpx-big_trouble) | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Black Knight (Williams 1980)](external/vpx-blackknight) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Black Knight 2000 (Williams 1989)](external/vpx-blackknight2000) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Black Magic 4 (Recel 1980)](external/vpx-blackmagic4) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Black Pyramid (Bally 1980)](external/vpx-blackpyramid) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Black Rose (Bally 1992)](external/vpx-blackrose) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [Bob Cuspe (Original 2025)](external/vpx-bobcuspe) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Bond 60th (Stern 2022)](external/vpx-jamesbond007) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Bram Stoker's Dracula (Williams 1993)](external/vpx-bsdracula) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Bram Stoker's Dracula - BLOOD EDITION (Original 2025)](external/vpx-dracblood) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Bugs Bunny's Birthday Ball (Bally 1990)](external/vpx-bugs) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 52 |
| [Cactus Canyon (Bally 1998)](external/vpx-cactuscanyon) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Casino (Williams 1958)](external/vpx-casino) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Cat Burglars (Original 2024)](external/vpx-catburglars) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Cenobite (Original 2023)](external/vpx-cenobite) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Centaur (Bally 1981)](external/vpx-centaur) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Centigrade 37 (Gottlieb 1977)](external/vpx-centigrade37) | :white_check_mark: | :x: | :x: | :x: | 58 |
| [Central Park (Gottlieb 1966)](external/vpx-centralpark) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Chrono Trigger (Original 2022)](external/vpx-chrono) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Cirqus Voltaire (Bally 1997)](external/vpx-cirqusvoltaire) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [City Hunter (Original 2025)](external/vpx-cityhunterpup) | :x: | :x: | :x: | :white_check_mark: | 60 |
| [Class of 1812 (Gottlieb 1991)](external/vpx-1812) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Cleopatra (SS) (Gottlieb 1977)](external/vpx-cleopatra) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Clock of Eternal Fog (Original 2024)](external/vpx-clockofetfog) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Congo (Williams 1995)](external/vpx-congo) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [Cosmic (Taito do Brasil 1980)](external/vpx-cosmic) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Count-Down (Gottlieb 1979)](external/vpx-countdownjp) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Cowboy Eight Ball (LTD do Brasil 1981)](external/vpx-cowboy8ball) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Creature from the Black Lagoon (Bally 1992)](external/vpx-cftbl) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Crystal-Ball (Talleres del Llobregat 1970)](external/vpx-crystalball) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [CSI (Stern 2008)](external/vpx-csiled) | :x: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Dark Crystal Pinball, The (Original 2020)](external/vpx-dcrystalpup) | :x: | :x: | :x: | :white_check_mark: | 60 |
| [Deadpool, JPs (Original 2021)](external/vpx-deadpool) | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Demolition Man (Williams 1994)](external/vpx-demoman) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Devil Riders (Zaccaria 1984)](external/vpx-devilriders) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Dexter (Original 2022)](external/vpx-dexter) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Diablo 2 (Original 2026)](external/vpx-diablo2) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Diablo Pinball (Original 2017)](external/vpx-diablo) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Diner (Williams 1990)](external/vpx-diner) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Dirty Harry (Williams 1995)](external/vpx-dirtyharry) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Disney TRON Legacy Limited Edition (Stern 2011)](external/vpx-tronlegacy) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Doctor Who (Bally 1992)](external/vpx-doctorwho) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 58 |
| [Dolly Parton (Bally 1979)](external/vpx-dolly) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Doom Eternal (Original 2022)](external/vpx-doometernal) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Dragon (Interflip 1977)](external/vpx-dragoni) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Dragon (SS) (Gottlieb 1978)](external/vpx-dragong) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Dragon Ball Z Budokai (Original 2023)](external/vpx-dbzbudokai) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Dragon Ball: Super Saiyan Edition (Original 2025)](external/vpx-dbsse) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Drakor (Taito do Brasil 1979)](external/vpx-drakor) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [DUNE (Original 2024)](external/vpx-dune) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Earthshaker (Williams 1989)](external/vpx-earthshaker) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [El Dorado City of Gold (Gottlieb 1975)](external/vpx-eldorado84) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Evil Dead 3 Army of Darkness (Original 2020)](external/vpx-armyofdarkness) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [F-14 Tomcat (Williams 1987)](external/vpx-f14tomcat) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Family Guy (Stern 2007)](external/vpx-familyguy) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Fathom (Bally 1981)](external/vpx-fathom) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 57 |
| [Fifth Element, The (Original 2022)](external/vpx-fifthelement) | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Fireball (Bally 1972)](external/vpx-fireball) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Fireball Classic (Bally 1985)](external/vpx-fbclassic) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Fireball II (Bally 1981)](external/vpx-fireballii) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Firepower (Williams 1980)](external/vpx-firepower) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Firepower II (Williams 1983)](external/vpx-firepower2) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Fireworks Mania (Orignial 2026)](external/vpx-fireworksmania) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Fish Tales (Williams 1992)](external/vpx-fishtales) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 57 |
| [Flight 2000 (Stern 1980)](external/vpx-flight2000) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Flintstones, The (Williams 1994)](external/vpx-flintstones) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [Fortnite (Original 2024)](external/vpx-fortnite) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Frank Thomas' Big Hurt (Gottlieb 1995)](external/vpx-bighurt) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Freddy A Nightmare on Elm Street (Gottlieb 1994)](external/vpx-freddy) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 58 |
| [Freddy's Nightmares (Original 2025)](external/vpx-freddysnm) | :x: | :x: | :x: | :white_check_mark: | 55 |
| [Friday the 13th, JP's (Original 2021)](external/vpx-jpfridaythe13th) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Funhouse (Williams 1990)](external/vpx-funhouse) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 56 |
| [Futurama (Original 2024)](external/vpx-futurama) | :x: | :x: | :x: | :white_check_mark: | 50 |
| [Gargamel Park (Original 2016)](external/vpx-gargamelpark) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Genie (Gottlieb 1979)](external/vpx-genie) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Getaway - High Speed II, The (Williams 1992)](external/vpx-getaway) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Ghostbusters Slimer, JP's (Original 2023)](external/vpx-jpslimer) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Gilligan's Island (Bally 1991)](external/vpx-gilligansisland) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Gladiators (Gottlieb 1993)](external/vpx-gladiators) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Godzilla (Sega 1998)](external/vpx-godzilla) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [Godzilla Remix (Limited Edition)  (Original 2021)](external/vpx-godzilla70th) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Goin' Nuts (Gottlieb 1983)](external/vpx-goinnuts) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Gold Wings (Gottlieb 1986)](external/vpx-goldwings) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Golden Cue (Sega 1998)](external/vpx-goldencue) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Goldeneye (Sega 1996)](external/vpx-goldeneye) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [Gorgar (Williams 1979)](external/vpx-gorgar) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Grand Slam (Gottlieb 1972)](external/vpx-grandslam1972) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Guardians of the Galaxy (Pro) (Stern 2017)](external/vpx-gotg) | :x: | :x: | :x: | :white_check_mark: | 55 |
| [Guardians of the Galaxy trilogy (Original 2023)](external/vpx-gogtrilogy) | :white_check_mark: | :white_check_mark: | :x: | :x: | 55 |
| [Guns N Roses (Data East 1994)](external/vpx-gnr) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [Gunship (Original 2023)](external/vpx-gunship) | :x: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 55 |
| [Hardbody (Bally 1987)](external/vpx-hardbody) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Harley-Davidson (Sega 1999)](external/vpx-harleyd) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 58 |
| [Haunted House (Gottlieb 1982)](external/vpx-hauntedhouse) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [Hellraiser (Original 2022)](external/vpx-hellraiser) | :x: | :x: | :x: | :white_check_mark: | 60 |
| [High Speed (Williams 1986)](external/vpx-highspeed) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Hollywood Heat (Gottlieb 1986)](external/vpx-hollywoodheat) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |
| [Humpty Dumpty (Gottlieb 1947)](external/vpx-humptydumpty) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Incredible Hulk, The (Gottlieb 1979)](external/vpx-incrediblehulk) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Independence Day (SEGA 1996)](external/vpx-id4) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Indiana Jones - The Last Movie (Original 2023)](external/vpx-indylastmovie) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Indiana Jones - The Pinball Adventure (Williams 1993)](external/vpx-indianajonestpa) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Iron Maiden Legacy of the Beast (Stern 2018)](external/vpx-ironmaidenlotb) | :x: | :x: | :x: | :white_check_mark: | 50 |
| [Jacks Open (Gottlieb 1977)](external/vpx-jacksopen) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Jacks to Open (Gottlieb 1984)](external/vpx-jackstoopen) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 58 |
| [James Cameron's Avatar (Stern 2010)](external/vpx-avatar) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 58 |
| [Jaws (Original 2025)](external/vpx-jaws50th) | :white_check_mark: | :x: | :x: | :x: | 55 |
| [John Carpenter's Christine (Original 2019)](external/vpx-christine) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [John Wick (Original 2023)](external/vpx-johnwick) | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Johnny Mnemonic (Williams 1995)](external/vpx-johnnymnemonic) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Judge Dredd (Bally 1993)](external/vpx-judgedredd) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 56 |
| [Jurassic Park (Data East 1993)](external/vpx-jurassicpark) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Jurassic Park Pro LE (Original 2022)](external/vpx-jurassicparkle) | :x: | :x: | :x: | :white_check_mark: | 60 |
| [King Kong (Data East 1990)](external/vpx-kingkong) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 52 |
| [King Of The Hill (Original 2025)](external/vpx-koth) | :x: | :x: | :x: | :white_check_mark: | 60 |
| [KISS (Bally 1979)](external/vpx-kissbally) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Knight Rider (Original 2021)](external/vpx-knightrider) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Last Action Hero (Data East 1993)](external/vpx-lastactionhero) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Legend of Zelda, The (Original 2015)](external/vpx-zelda) | :x: | :x: | :x: | :white_check_mark: | 60 |
| [Lord of the Rings, JP's The (Original 2017)](external/vpx-jplordoftherings) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Lost in Space (Sega 1998)](external/vpx-lostinspace) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Lost World Jurassic Park, JP's The (Sega 1997)](external/vpx-thelostworldjp) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Lost World Jurassic Park, The (Sega 1997)](external/vpx-thelostworld) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Mach 2.0 Two (Spinball S.A.L. 1995)](external/vpx-machtwo) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Machine - Bride of Pin-bot, The (Williams 1991)](external/vpx-bop) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 58 |
| [Mad Max - Fury Road (Original 2021)](external/vpx-madmax) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Mary Shelley's Frankenstein (Sega 1995)](external/vpx-maryshelleyfrank) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Mata Hari (Bally 1978)](external/vpx-matahari) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Medieval Madness (Williams 1997)](external/vpx-mm) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 56 |
| [Megadeth (Original 2023)](external/vpx-megadeth) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Metallica (Premium Monsters) (Stern 2013)](external/vpx-metallicapremium) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Meteor (Taito do Brasil 1979)](external/vpx-meteort) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Mighty Morphin Power Rangers (Original 2024)](external/vpx-powerrangers) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Mission Impossible Limited Edition (Original 2022)](external/vpx-missionimp) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Monster Bash (Williams 1998)](external/vpx-monsterbashreskin) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Mousin' Around! (Bally 1989)](external/vpx-mousin) | :white_check_mark: | :white_check_mark: | :x: | :x: | 55 |
| [Nags (Williams 1960)](external/vpx-nags) | :white_check_mark: | :x: | :x: | :x: | 50 |
| [Nascar (Stern 2005)](external/vpx-nascar) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [NASCAR, JP's (Original 2015)](external/vpx-jpsnascar) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [NBA Fastbreak (Bally 1997)](external/vpx-nbafastbreak) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Neverending Story, The (Original 2021)](external/vpx-tnes) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [No Fear - Dangerous Sports (Williams 1995)](external/vpx-nofear) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [O Gaucho (LTD do Brasil 1975)](external/vpx-ogaucho) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Pennant Fever (Williams 1984)](external/vpx-pennantfever) | :white_check_mark: | :x: | :white_check_mark: | :x: | 57 |
| [Pharaoh - Dead Rise](external/vpx-pharoahdr) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Pin-Bot (Williams 1986)](external/vpx-pinbot) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Pink Floyd (Original 2022)](external/vpx-pinkfloyd) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Pirates of the Caribbean (Stern 2006)](external/vpx-potc) | :white_check_mark: | :white_check_mark: | :x: | :x: | 58 |
| [Pistol Poker (Alvin G. 1993)](external/vpx-pistolpoker) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Playboy (Bally 1978)](external/vpx-playboy1978) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Playboy (Bally 1978) Definitive Edition (Bally 1978)](external/vpx-playboy1978de) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Playboy (Bally 1978) Definitive Edition Nude (Bally 1978)](external/vpx-playboy1978denude) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Playboy (Stern 2002)](external/vpx-playboy2002) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [Playboy 35th Anniversary (Data East 1989)](external/vpx-playboy35) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Playboy 35th Anniversary Marilyn Edition(Data East 1989)](external/vpx-playboy35m) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Police Force (Williams 1989)](external/vpx-policeforce) | :white_check_mark: | :x: | :white_check_mark: | :x: | 55 |
| [Punch-Out (Original 2025)](external/vpx-punchout) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Quicksilver (Stern 1980)](external/vpx-quicksilver) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Rat Fink (Original 2025)](external/vpx-ratfink) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Rescue 911 (Gottlieb 1994)](external/vpx-r911) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Ripley's Believe it or Not! (Stern 2004)](external/vpx-ripleys) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Road Runner (Atari 1979)](external/vpx-roadrunner) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Rollergames (Williams 1990)](external/vpx-rollergames) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Scared Stiff (Bally 1996)](external/vpx-scaredstiff) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Seawitch, JP's (Stern 1980)](external/vpx-jpseawitch) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Seawitch, JP's (Stern 1980)](external/vpx-seawitch) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Secret Service (Data East 1988)](external/vpx-secretservice) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Sexy Girl Nude Mod (Arkon 1980) (Arkon 1980)](external/vpx-sexygirlnude) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Shadow, The (Bally 1994)](external/vpx-theshadow) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Shark (Taito do Brasil 1982)](external/vpx-shark) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Sharkey's Shootout (Stern 2000)](external/vpx-sharkeys) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Shining, The (Original 2025)](external/vpx-theshining) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Skyscraper (Bally 1934)](external/vpx-skyscraper) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Sopranos, The (Stern 2005)](external/vpx-sopranos) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 56 |
| [South Park (Sega 1999)](external/vpx-southpark) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Space Cadet, JP's - Galaxy Edition (Original 2021)](external/vpx-spacecadetge) | :white_check_mark: | :x: | :x: | :white_check_mark: | 60 |
| [Space Invaders (Bally 1980)](external/vpx-spaceinvaders) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Space Jam (Sega 1996)](external/vpx-spacejam) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 56 |
| [Spider-Man (Vault Edition) (Stern 2016)](external/vpx-spidermanve) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Star Trek - The Next Generation (Williams 1993)](external/vpx-startrektng) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Star Trek 25th Anniversary (Data East 1991)](external/vpx-startrek25th) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Star Wars (Data East 1992)](external/vpx-starwars) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 58 |
| [Star Wars (Original 2025)](external/vpx-starwarsburger) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Star Wars - The Bad Batch (Original 2022)](external/vpx-swbadbatch) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Star Wars Trilogy (Sega 1997)](external/vpx-starwarstrilogy) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Starship Troopers (Sega 1997)](external/vpx-starship) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [Starship Troopers VPW - SEGA (1997)](external/vpx-starshipvpw) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 55 |
| [Street Fighter II, JP's (Gottlieb 1993)](external/vpx-sfii) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Tales from the Crypt (Data East 1993)](external/vpx-tftc) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 58 |
| [Tales of the Arabian Nights, JP's (Williams 1996)](external/vpx-totan) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Taxi (Williams 1988)](external/vpx-taxi) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [Tee'd Off (Gottlieb 1993)](external/vpx-teedoff) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Terminator 2 - Judgment Day (Williams 1991)](external/vpx-t2) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 56 |
| [Terminator 2, JP's (Original 2020)](external/vpx-jpt2) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | 60 |
| [The Champion Pub (Williams 1998)](external/vpx-champpub) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [The Simpsons Pinball Party (Stern 2003)](external/vpx-simpsonspprty) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Torpedo Alley (Data East 1988)](external/vpx-torpedoalley) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Total Nuclear Annihilation (Spooky Pinball 2017)](external/vpx-tna) | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 60 |
| [Trailer Park Boys - Pin-Ballers (Original 2024)](external/vpx-trailerparkboys) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Transformers (Pro) [Marcade Mod] (Original 2025)](external/vpx-transformersmod) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 58 |
| [Trident (Stern 1979)](external/vpx-trident) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Twister (Sega 1996)](external/vpx-twister) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Underwater (Recel 1976)](external/vpx-underwater) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Victory (Gottlieb 1987)](external/vpx-victory) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Viper Night Drivin' (Sega 1998)](external/vpx-vipernight) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Volkan Steel and Metal (Original 2023)](external/vpx-volkan) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Vortex (Taito do Brasil 1981)](external/vpx-vortex) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Walking Dead (Limited Edition), The (Stern 2014)](external/vpx-twdle) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 57 |
| [Walkure (Original 2025)](external/vpx-walkure) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Walkyria (Joctronic 1986)](external/vpx-walkyria) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Whirlwind (Williams 1990)](external/vpx-wwind) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [WHO Dunnit (Bally 1995)](external/vpx-whodunnit) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 58 |
| [Who's Tommy Pinball Wizard, The (Data East 1994)](external/vpx-tommy) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Whoa Nellie! Big Juicy Melons (Stern 2015)](external/vpx-whoanellie) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Willow (Original 2025)](external/vpx-willow) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Wipe Out (Gottlieb 1993)](external/vpx-wipeout) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 58 |
| [Wrath of Olympus, JP's (Original 2022)](external/vpx-wrath) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Wuthering Waves (Original 2026)](external/vpx-wutheringwaves) | :x: | :x: | :x: | :white_check_mark: | 57 |
| [WWF Royal Rumble (Data East 1994)](external/vpx-royalrumble) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [X Files (Sega 1997)](external/vpx-xfiles) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [X-Files (Sega 1997)](external/vpx-xfileshanibal) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [X-Men Wolverine LE (Stern 2012)](external/vpx-xmen) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Young Frankenstein (Original 2021)](external/vpx-youngfrankenstein) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Zissou - The Life Aquatic (Original 2022)](external/vpx-zissou) | :white_check_mark: | :x: | :x: | :x: | 60 |

<br>

## Manual Install Tables


| Table | Backglass | DMD | ROM Required | Has Puppack | FPS |
|:------|:---------:|:---:|:------------:|:---:|:---:|
| [301 Bullseye (Grand Products 1986)](external/vpx-301bullseye) | :white_check_mark: | :x: | :white_check_mark: | :x: | 57 |
| [A Real American Hero - Operation P.I.N.B.A.L.L. (Original 2017)](external/vpx-gijoe) | :white_check_mark: | :x: | :white_check_mark: | :x: | 52 |
| [Abra Ca Dabra (Gottlieb 1975)](external/vpx-abracadabra) | :white_check_mark: | :x: | :x: | :x: | 49 |
| [Ace of Speed (Original 2019)](external/vpx-aceofspeed) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 30 |
| [Ace Ventura Pet Detective (TBA 2019)](external/vpx-aceventura) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Addams Family, The (Bally 1992)](external/vpx-taf) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [Aerosmith (Pro) (Stern/Tribute 2017)](external/vpx-aerosmith) | :white_check_mark: | :white_check_mark: | :x: | :x: | 44 |
| [Agents 777 (Game Plan 1984)](external/vpx-agents777) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [Airborne (Capcom 1996)](external/vpx-airborne) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 37 |
| [Airborne Avenger (Atari 1977)](external/vpx-airavenger) | :white_check_mark: | :x: | :white_check_mark: | :x: | 55 |
| [Airwolf (TBA 2020)](external/vpx-airwolf) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Aladdin's Castle (Bally 1976)](external/vpx-aladdincastle) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Algar (Williams 1980)](external/vpx-algar) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [Ali (Stern 1980)](external/vpx-ali) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [Alien Poker (Williams 1980)](external/vpx-alienpoker) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [Alien Star (Gottlieb 1984)](external/vpx-alienstar) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 42 |
| [Aliens (Original 2020)](external/vpx-aliens) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Amazing Spiderman (Gottlieb 1980) Sinister Six Edition](external/vpx-amazingspidermansse) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 48 |
| [America 1492 (Juegos Populares 1986)](external/vpx-america1492) | :white_check_mark: | :x: | :white_check_mark: | :x: | 48 |
| [American Graffiti (Original 2024)](external/vpx-agraffiti) | :white_check_mark: | :white_check_mark: | :x: | :x: | 40 |
| [Arena 1.0c (Gottlieb 1987)](external/vpx-arena1C3S) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 45 |
| [Aspen (Brunswick 1979)](external/vpx-aspen) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Atarians, The (Atari 1976)](external/vpx-atarians) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [Atlantis (Bally 1989)](external/vpx-atlantis) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 48 |
| [Atlantis (Gottlieb 1975)](external/vpx-atlantisgottlieb) | :white_check_mark: | :white_check_mark: | :x: | :x: | 44 |
| [Attack & Revenge from Mars (Original 2015)](external/vpx-attackandrevengefrommars) | :x: | :x: | :x: | :x: | N/A |
| [Attack from Mars (Bally 1995)](external/vpx-attackfrommars) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 30 |
| [Attack of the Killer Tomatoes (MusicMOD) (Iceman 2023)](external/vpx-attackofthekillertomatoes) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Attila The Hun (Game Plan 1984)](external/vpx-atilla) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Bad Girls (Gottlieb 1988)](external/vpx-badgirls) | :white_check_mark: | :x: | :white_check_mark: | :x: | 50 |
| [Balls-A-Poppin (Bally 1956)](external/vpx-ballsapoppin) | :white_check_mark: | :x: | :x: | :x: | 52 |
| [Bally Game Show, The (Bally 1990)](external/vpx-ballygameshow) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 33 |
| [Barb Wire (Gottlieb 1996)](external/vpx-barbwire) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 41 |
| [Barracora (Williams 1981)](external/vpx-barracora) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 32 |
| [Baseball (Gottlieb 1970)](external/vpx-baseball1970) | :white_check_mark: | :x: | :x: | :x: | 45 |
| [Batman '66 Premium (Stern 2016)](external/vpx-batman66premium) | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 50 |
| [Batman BW Edition (Data East 1991)](external/vpx-batmanbw) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 47 |
| [Battlestar Galactica (Original 2018)](external/vpx-battlestargalactica) | :white_check_mark: | :x: | :white_check_mark: | :x: | 47 |
| [Beach Bums (Original 2018)](external/vpx-beachbums) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 42 |
| [Beat The Clock (Bally 1985)](external/vpx-beattheclock) | :white_check_mark: | :x: | :white_check_mark: | :x: | 34 |
| [Beavis and Butt-head: Pinballed (Original 2024)](external/vpx-bbhpinballed) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 34 |
| [Beetlejuice (Original 2023)](external/vpx-beetlejuice) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 38 |
| [BeetleJuice Movie (Original 2021)](external/vpx-beetlejuicemovie) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 48 |
| [Beverly Hills Cop (TBA 2019)](external/vpx-beverlyhillscop) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 57 |
| [Big Deal (Williams 1963)](external/vpx-bigdeal) | :white_check_mark: | :x: | :x: | :x: | 52 |
| [Big Game (Stern 1980)](external/vpx-biggame) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Big Horse (Maresa 1975)](external/vpx-bighorse) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Big House (Gottlieb 1989)](external/vpx-bighouse) | :white_check_mark: | :x: | :white_check_mark: | :x: | 32 |
| [Big Indian (Gottlieb 1974)](external/vpx-bigindian) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Big Lebowski (2025)](external/vpx-biglebowski) | :white_check_mark: | :white_check_mark: | :x: | :x: | 50 |
| [Big Star (Williams 1972)](external/vpx-bigstar) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Biker Mice From Mars](external/vpx-bikermice) | :x: | :x: | :x: | :x: | 47 |
| [Bird Fly (Original 2022)](external/vpx-birdfly) | :white_check_mark: | :x: | :white_check_mark: | :x: | 52 |
| [Black Belt (Bally 1986)](external/vpx-blackbeltbally) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [Black Fever (Playmatic 1980)](external/vpx-blackfever) | :white_check_mark: | :x: | :white_check_mark: | :x: | 50 |
| [Black Hole (Gottlieb 1981)](external/vpx-blackhole) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 30 |
| [Blackout (Williams 1980)](external/vpx-blackout) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Blood Machines (Original 2022)](external/vpx-bloodmachines) | :white_check_mark: | :white_check_mark: | :x: | :x: | 27 |
| [BMX (Radical Rick) (Bally 1983)](external/vpx-bmx) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [Bobby Orr Power Play (Bally 1977)](external/vpx-bobbyorr) | :white_check_mark: | :x: | :white_check_mark: | :x: | 40 |
| [Bonanza (Original 2022)](external/vpx-bonanza) | :white_check_mark: | :x: | :white_check_mark: | :x: | 58 |
| [Bond 60th - Limited Edition (Original 2023)](external/vpx-jb00760th) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Bone Busters Inc. (Gottlieb 1989)](external/vpx-bonebusters) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 35 |
| [Bounty Hunter (Gottlieb 1985)](external/vpx-bountyhunter) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |
| [Bow and Arrow EM (Bally 1975)](external/vpx-bowandarrow) | :white_check_mark: | :x: | :x: | :x: | 48 |
| [Brave Team (Inder 1985)](external/vpx-braveteam) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Breaking Bad (Original 2022)](external/vpx-breakingbad) | :white_check_mark: | :white_check_mark: | :x: | :x: | 58 |
| [Breakshot (capcom 1996)](external/vpx-breakshot) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 28 |
| [Breakshot (Capcom 1996)](external/vpx-breakshotbigus) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 45 |
| [Bubba the Redneck Werewolf (Original 2017)](external/vpx-bubbatheredneckwerewolf) | :white_check_mark: | :x: | :white_check_mark: | :x: | 40 |
| [Buck Rogers (Gottlieb 1980) Sound MOD](external/vpx-buckrogerssound) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |
| [Buffy The Vampire Slayer (Original 2022)](external/vpx-buffy) | :white_check_mark: | :white_check_mark: | :x: | :x: | 52 |
| [Cactus Jack's (Gottlieb 1991)](external/vpx-cactusjacks) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 29 |
| [Capersville (Bally 1966)](external/vpx-capersville) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Capt. Fantastic and The Brown Dirt Cowboy (Bally 1976)](external/vpx-captfantastic) | :white_check_mark: | :x: | :x: | :x: | 45 |
| [Capt. Fantastic and The Brown Dirt Cowboy, JPs (Bally 1976)](external/vpx-jpcaptfantastic) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Captain Spaulding's Museum Of Monsters And Madmen](external/vpx-captainspaulding) | :white_check_mark: | :white_check_mark: | :x: | :x: | 35 |
| [Car Hop (Gottlieb 1991)](external/vpx-carhop) | :white_check_mark: | :x: | :white_check_mark: | :x: | 40 |
| [Carnaval no Rio (LTD do Brasil 1981)](external/vpx-carnaval) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Castlevania - Symphony of the Night (Original 2022)](external/vpx-castlevaniasotn) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Cerberus (Playmatic 1983)](external/vpx-cerberus) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Charlie Brown Christmas, A (Original 2023)](external/vpx-acharliebrownchristmas) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Charlie's Angels (Gottlieb 1978)](external/vpx-charliesangels) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Charlie's Angels (Gottlieb 1978) iDig Complete rEMix](external/vpx-charliesangelsremix) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 57 |
| [Checkpoint (Data East 1991)](external/vpx-checkpoint) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 58 |
| [Cheech & Chong: Road-Trip'pin (Bally 2021)](external/vpx-cheechandchong) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 40 |
| [Cheetah (Stern 1980)](external/vpx-cheetah) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 32 |
| [Child's Play (Original 2018)](external/vpx-childsplay) | :white_check_mark: | :x: | :x: | :white_check_mark: | 52 |
| [Circus (Gottlieb 1980)](external/vpx-circus) | :white_check_mark: | :x: | :white_check_mark: | :x: | 38 |
| [Cirqus Voltaire (Bally 1997)](external/vpx-cirqus) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 35 |
| [City Slicker (Bally 1987)](external/vpx-cityslicker) | :white_check_mark: | :x: | :white_check_mark: | :x: | 32 |
| [Class of 1984 (Original 2024)](external/vpx-classof1984) | :white_check_mark: | :x: | :white_check_mark: | :x: | 52 |
| [Clown PlayMatic 1971](external/vpx-clownplaymatic) | :white_check_mark: | :x: | :x: | :x: | N/A |
| [Comet (Williams 1985)](external/vpx-comet) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 34 |
| [Conan (Rowamet 1983)](external/vpx-conan) | :white_check_mark: | :x: | :white_check_mark: | :x: | 55 |
| [Corvette (Midway 1994)](external/vpx-corvette) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 45 |
| [Cosmic Gunfight (Williams 1982)](external/vpx-cosmicgunfight) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Cosmic Princess (Stern 1979)](external/vpx-cosmicprincess) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Counterforce (Gottlieb 1980)](external/vpx-counterforce) | :white_check_mark: | :x: | :white_check_mark: | :x: | 33 |
| [Creature From The Black Lagoon (1992) - BW Edition](external/vpx-creaturebw) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 40 |
| [Creepshow (Original 2022)](external/vpx-creepshow) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 42 |
| [Criterium 77 (Taito do Brasil 1977)](external/vpx-criterium77) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Cue Ball Wizard (Gottlieb 1992)](external/vpx-cueball) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 33 |
| [Cuphead Original  (D.Goblett & Co 2019)](external/vpx-cupheadoriginal) | :white_check_mark: | :white_check_mark: | :x: | :x: | 47 (without music) |
| [Cuphead Pro (Original 2020)](external/vpx-cuphead) | :white_check_mark: | :x: | :x: | :x: | 34 |
| [Cyber Race (Original 2023)](external/vpx-cyberrace) | :white_check_mark: | :white_check_mark: | :x: | :x: | 40 |
| [Cybernaut (Bally 1985)](external/vpx-cybernaut) | :white_check_mark: | :x: | :white_check_mark: | :x: | 45 |
| [Daredevil and The Defenders (Original 2024)](external/vpx-daredevil) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Dark Crystal, The (Original 2020)](external/vpx-darkcrystal) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [DarkPrincess (original 2020)](external/vpx-darkprincess) | :white_check_mark: | :white_check_mark: | :x: | :x: | 57 |
| [Death Note (Original 2020)](external/vpx-deathnote) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Death Proof (Original 2021)](external/vpx-deathproof) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 41 |
| [Deep Purple (Original 2024)](external/vpx-deeppurple) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 45 |
| [Defender (Williams 1982)](external/vpx-defender) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 38 |
| [Demolition Man (Williams 1994) - Limited Cryo Edition](external/vpx-demolitionmancryo) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 40 |
| [Depeche Mode Pinball (Original 2021)](external/vpx-depechemode) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Desert City (Fipermatic 1977)](external/vpx-desertcity) | :white_check_mark: | :x: | :x: | :x: | 52 |
| [Devil's Dare (Gottlieb 1982)](external/vpx-devilsdare) | :white_check_mark: | :x: | :white_check_mark: | :x: | 58 |
| [Dick Tracy (Original 2024)](external/vpx-dicktracy) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Dimension \/ Galaxie (Gottlieb 1971)](external/vpx-dimension) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Disco Dancing(LTD do Brasil 1979)](external/vpx-discodancing) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Django Unchained (Original 2022)](external/vpx-djangounchained) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 45 |
| [Doodle Bug (Williams 1971)](external/vpx-doodlebug) | :white_check_mark: | :x: | :x: | :x: | 47 |
| [Dr. Dude and his excellent ray (Bally 1999)](external/vpx-drdude) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 42 |
| [Dr. Jekyll and Mr. Hyde (Original 2022)](external/vpx-drjekyllandmrhyde) | :white_check_mark: | :x: | :white_check_mark: | :x: | 45 |
| [Dracula (Stern 1979)](external/vpx-dracula) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 53 |
| [Dragon Ball Z (Original 2018)](external/vpx-dbz) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Ducktales Quest For Money](external/vpx-ducktales) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Duke Nukem 3D (Original 2020)](external/vpx-dukenukem3d) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Dungeons & Dragons (Bally 1987)](external/vpx-dungeonsndragons) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 48 |
| [Duotron (Gottlieb 1974)](external/vpx-duotron) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Eclipse (Gottlieb 1982)](external/vpx-eclipse) | :white_check_mark: | :x: | :white_check_mark: | :x: | 45 |
| [Eight Ball (Bally 1977)](external/vpx-jpeightball) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Eight Ball Champ (Bally 1985)](external/vpx-eightballchamp) | :white_check_mark: | :x: | :white_check_mark: | :x: | 51 |
| [Eight Ball Deluxe (Bally 1981)](external/vpx-eightballdeluxe) | :white_check_mark: | :x: | :white_check_mark: | :x: | 45 |
| [El Bueno, el Feo y el Malo (Original 2015)](external/vpx-bueno) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [El Dorado (Gottlieb 1975)](external/vpx-eldorado) | :white_check_mark: | :x: | :x: | :x: | 43 |
| [Elektra (Bally 1981)](external/vpx-elektrabigus) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Elvira and the Party Monsters (Bally 1989)](external/vpx-partymonsters) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 33 |
| [Elvira's House of Horrors Remix (Original 2021)](external/vpx-elvirashouseofhorrors) | :white_check_mark: | :x: | :white_check_mark: | :x: | 38 |
| [Elvis (Stern 2004)](external/vpx-elvis) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Embryon (Bally 1981)](external/vpx-embryon) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 29 |
| [Escape from the Lost World (Bally 1988)](external/vpx-escapefromthelostworld) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |
| [Evel Knievel (Bally 1977)](external/vpx-evelknievel) | :white_check_mark: | :x: | :white_check_mark: | :x: | 50 |
| [Evil Dead 2 (TBA 2019)](external/vpx-evildead22019) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Evil Dead 2 - Version A (Original 2022)](external/vpx-evildead2a) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Evil Dead 2 - Version B (Original 2022)](external/vpx-evildead2b) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Evil Fight (Playmatic 1980)](external/vpx-evilfight) | :white_check_mark: | :x: | :white_check_mark: | :x: | 53 |
| [Fallout Vault Edition (Original 2024)](external/vpx-fallout) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 45 |
| [Fan-Tas-Tic (Williams 1972)](external/vpx-fantastic) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Far Cry 3 - Blood Dragon (Original 2018)](external/vpx-farcry3) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Farfalla (Zaccaria 1983)](external/vpx-farfalla) | :x: | :white_check_mark: | :white_check_mark: | :x: | 45 |
| [Fast Draw (Gottlieb 1975)](external/vpx-fastdraw) | :white_check_mark: | :x: | :x: | :x: | 51 |
| [Fire Action (Taito do Brasil 1980)](external/vpx-jpfireaction) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Fire Action de Luxe (Taito do Brasil 1983)](external/vpx-jpfireactiondeluxe) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Fire! (Williams 1987)](external/vpx-fire) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 45 |
| [Five Nights at Freddy's Original (2021)](external/vpx-fivenightatfreddys) | :white_check_mark: | :white_check_mark: | :x: | :x: | 48 |
| [Flash (Williams 1979)](external/vpx-flash) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Flash Gordon (Bally 1981) 2.0.2](external/vpx-flashgordon) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 35 |
| [Flash Gordon (Bally, 1981) Sound MOD v2.1](external/vpx-flashgordonsm) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 35 |
| [Flashman (Sportmatic 1984)](external/vpx-jpflashman) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Fleet Jr. (Bally 1934)](external/vpx-fleet) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Fleetwood Mac (Original 2024)](external/vpx-fleetwood) | :white_check_mark: | :x: | :white_check_mark: | :x: | 52 |
| [Flintstones Toon Mod (reskin of The Flintstones, Williams 1994)](external/vpx-flintstonestoon) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 42 |
| [Flipper Football (Capcom 1996)](external/vpx-flipperfootball) | :white_check_mark: | :x: | :white_check_mark: | :x: | 50 |
| [Football (Taito do Brasil 1979)](external/vpx-jpfootball) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Frank Thomas' Big Hurt (Gottlieb 1995)](external/vpx-frankthomas) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |
| [Frankenstein - Balutito MOD (Original 2022)](external/vpx-frankenstein) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 40 |
| [Frankenstein - Black and White Balutito MOD (Original 2022)](external/vpx-frankensteinbw) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 40 |
| [Freddy A Nightmare On Elm Street (Gottlieb 1994) Ejse83 LED Mod](external/vpx-freddyled) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 35 |
| [Freefall (Stern 1981)](external/vpx-freefall) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Frontier (Bally 1980)](external/vpx-frontier) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Full Metal Jacket (Original 2022)](external/vpx-fullmetaljacket) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |
| [Future Spa (Bally 1979)](external/vpx-futurespa) | :white_check_mark: | :x: | :white_check_mark: | :x: | 57 |
| [Galaxy (Sega 1973)](external/vpx-galaxysega) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Galaxy (Stern 1980)](external/vpx-galaxy) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 42 |
| [Galaxy Play (CIC Play 1986)](external/vpx-galaxyplay) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Galáxia (LTD do Brasil 1975)](external/vpx-galaxia) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Gamatron (Pinstar 1985)](external/vpx-gamatron) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Gemini 2000 (Taito do Brasil 1982)](external/vpx-gemini2000) | :white_check_mark: | :x: | :white_check_mark: | :x: | 55 |
| [George Michael - Faith (Original 2023)](external/vpx-georgemichael) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Godzilla LE (Sega - Stern Mashup)](external/vpx-godzillale) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 39 |
| [Gold Ball (Bally 1983)](external/vpx-goldball) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 44 |
| [Gold Wing (Original 2017)](external/vpx-goldwing) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [GORILLAZ (Original 2024)](external/vpx-gorillaz) | :white_check_mark: | :white_check_mark: | :x: | :x: | 30 |
| [Grand Lizard (Williams 1986)](external/vpx-grandlizard) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Granny and the Gators (Bally 1984)](external/vpx-granny) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Grease (Original 2020)](external/vpx-grease) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Grease B&W Pro (2021)](external/vpx-greaseblackandwhite) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Great Houdini (Original 2022)](external/vpx-greathoudini) | :white_check_mark: | :white_check_mark: | :x: | :x: | 50 |
| [Gremlins (Original 2022)](external/vpx-gremlins) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 45 |
| [Guns n' Roses (Remix) (Original 2021)](external/vpx-gunsle) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 48 |
| [Hall & Oates (Original 2025)](external/vpx-hallandoates) | :white_check_mark: | :x: | :white_check_mark: | :x: | 45 |
| [Halloween 1978-1981 (Original 2022)](external/vpx-halloween) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 40 |
| [Halloween MM Edition (Original 2023) Version 3.1](external/vpx-halloweenmm) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Hannibal Lecter (Original 2022)](external/vpx-hanniballec) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Harlem Globetrotters on Tour (Bally 1979) (JP's)](external/vpx-harlemglobetrotters) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Harley Quinn (Original 2017)](external/vpx-harleyquinn) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |
| [Harley-Davidson (Bally 1991)](external/vpx-hdavidson) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Harry Potter and the Goblet of Fire (Original 2020)](external/vpx-hpgof) | :white_check_mark: | :white_check_mark: | :x: | :x: | 35 |
| [Heavy Metal [Classic] (Rowamet 1983)](external/vpx-heavymetalclassic) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Hellboy Pinball](external/vpx-hellboy) | :white_check_mark: | :white_check_mark: | :x: | :x: | 40 |
| [High Roller Casino (Stern 2001)](external/vpx-highrollercasino) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 40 |
| [Highlander (TBA 2019)](external/vpx-highlander) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [HIPHOP](external/vpx-hiphop) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 59 |
| [Hook (Data East 1992)](external/vpx-hook) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 35 |
| [Horrorburg JP_VPX8](external/vpx-horrorburg) | :white_check_mark: | :white_check_mark: | :x: | :x: | 57 |
| [Hot Ball (Taito do Brasil 1979)](external/vpx-jphotball) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Hurricane - Balutito Mod 2.0](external/vpx-hurricane) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 40 |
| [Hustler (LTD do Brasil 1980)](external/vpx-hustler) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [I Dream of Jeannie (Original 2019)](external/vpx-idreamofjeannie) | :white_check_mark: | :x: | :white_check_mark: | :x: | 57 |
| [Ice Fever (Gottlieb 1985)](external/vpx-icefever) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 45 |
| [Indianapolis 500 (Bally 1995)](external/vpx-indy500) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 48 |
| [Inhabiting Mars (Original 2023)](external/vpx-inhabitinmars) | :white_check_mark: | :white_check_mark: | :x: | :x: | 43 |
| [Iron Maiden (Stern 1981)](external/vpx-ironmaiden) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Iron Man 2, JP's (Original 2018)](external/vpx-jpironman2) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Jack In The Box (Gottlieb 1973)](external/vpx-jackinthebox) | :white_check_mark: | :x: | :x: | :x: | 46 |
| [Jack Sparrow (Original 2023)](external/vpx-jacksparrow) | :white_check_mark: | :white_check_mark: | :x: | :x: | 55 |
| [Jack·Bot (Williams 1995)](external/vpx-jackbot) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 35 |
| [Jake Mate, JPSalas DT-FS-VR-MR Ext2k Conversion (Petaco - 1974)](external/vpx-jakemate) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [JAWS (Balutito MOD) (Original 2013)](external/vpx-jaws) | :white_check_mark: | :white_check_mark: | :x: | :x: | 45 |
| [Jaws 50th Anniversary (Original by marty02 2025)](external/vpx-jawsmarty) | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: | 50 |
| [Jet Spin (Gottlieb 1977)](external/vpx-jetspin) | :white_check_mark: | :x: | :x: | :x: | 58 |
| [Jimi Hendrix (Original 2021)](external/vpx-jimihendrix) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Joe Bar Team (ZXR 2019)](external/vpx-joebarteam) | :white_check_mark: | :x: | :white_check_mark: | :x: | 50 |
| [Joker Poker (Gottlieb 1978)](external/vpx-jokerpoker) | :white_check_mark: | :x: | :white_check_mark: | :x: | 50 |
| [Jokerz! (Williams 1988)](external/vpx-jokerz) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 40 |
| [Jolly Roger (Williams 1967)](external/vpx-jollyroger) | :white_check_mark: | :x: | :x: | :x: | 51 |
| [Joust (Williams 1983)](external/vpx-joust) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [JP's Cyclone (Williams 1988)](external/vpx-cyclone) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 45 |
| [JP's Grand Prix](external/vpx-grandprix) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [JP's Indiana Jones (Stern 2008)](external/vpx-indianajones) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 57 |
| [JP's IT - Pinball Madness](external/vpx-jpitpinballmadness) | :white_check_mark: | :white_check_mark: | :x: | :x: | 59 |
| [JP's Mephisto (Cirsa 1987)](external/vpx-mephisto) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [JP's Papa Smurf](external/vpx-papasmurf) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [JP's Sands of the Aton (Original 2023)](external/vpx-sandsaton) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [JP's Smurfette](external/vpx-smurfette) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [JP's Space Cadet (Maxis 1995) (Original)](external/vpx-spacecadet) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [JP's Spider-Man](external/vpx-spiderman) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [JP's Star Trek LE (Original 2020)](external/vpx-jpstartrekle) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [JP's Terminator 3 (Stern 2003)](external/vpx-terminator3) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [JP's The Walking Dead (Stern 2014)](external/vpx-thewalkingdead) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Jumanji (Original 2023)](external/vpx-jumanji) | :white_check_mark: | :white_check_mark: | :x: | :x: | 48 |
| [Jungle Lord (Williams 1981)](external/vpx-junglelord) | :white_check_mark: | :x: | :white_check_mark: | :x: | 30 |
| [Junk Yard (Williams 1996)](external/vpx-junkyard) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 40 |
| [Junkyard Cats (Bailey 2012)](external/vpx-junkyardcats) | :white_check_mark: | :x: | :x: | :x: | 54 |
| [Kill Bill (Original 2022)](external/vpx-killbill) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Killer Instinct](external/vpx-killerinstinct) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 44 |
| [KING Donkey Kong (Original 2023)](external/vpx-kingdonkeykong) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 48 |
| [King Tut (Bally 1969)](external/vpx-kingtut) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Kingpin (Capcom 1996) (1920's Mod)](external/vpx-kingpin) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 42 |
| [KISS (Stern 2015)](external/vpx-kiss) | :white_check_mark: | :white_check_mark: | :x: | :x: | 57 |
| [Klondike (Williams 1971)](external/vpx-klondike) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Krull (Gottlieb 1983)](external/vpx-krull) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 57 |
| [Kung Fu (LTD do Brasil 1975)](external/vpx-kungfu) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Kung Fu Hustle](external/vpx-kungfuhustle) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Laser Cue (Williams 1984)](external/vpx-lasercue) | :white_check_mark: | :x: | :white_check_mark: | :x: | 33 |
| [Laser War (Data East 1987)](external/vpx-laserwar) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 29 |
| [Last Starfighter, The (Taito, 1983)](external/vpx-thelaststarfighter) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Legend - A Pinball Adventure (Original 2023)](external/vpx-legend) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 54 |
| [Lethal Weapon 3 (Data East 1992)](external/vpx-lethalweapon3) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |
| [Lightning (Stern 1981)](external/vpx-lightning) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Lights Camera Action! (Gottlieb 1989)](external/vpx-lca) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |
| [Lion King (Original 2020)](external/vpx-lionking) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Lost World (Bally 1978)](external/vpx-lostworld) | :white_check_mark: | :x: | :white_check_mark: | :x: | 56 |
| [Lucky Seven (Williams 1978)](external/vpx-luckyseven) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Mac Jungle (MAC 1987)](external/vpx-macjungle) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Mac's Galaxy (MAC 1986)](external/vpx-macgalaxy) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Mad Race (Playmatic 1985)](external/vpx-jpmadrace) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Magic Castle (Zaccaria 1984)](external/vpx-magiccastle) | :white_check_mark: | :x: | :white_check_mark: | :x: | 52 |
| [Mario Andretti (Gottlieb 1995)](external/vpx-marioandretti) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 33 |
| [Mars Trek (Sonic 1977)](external/vpx-marstrek) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Mask (Sound Mod) (Original 2023)](external/vpx-mask) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |
| [Masters of the Universe (Original 2018)](external/vpx-mastersoftheuniverse) | :white_check_mark: | :white_check_mark: | :x: | :x: | 54 |
| [Maverick (Data East 1994)](external/vpx-maverick) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 35 |
| [Medusa (Bally 1981)](external/vpx-jpmedusa) | :white_check_mark: | :x: | :white_check_mark: | :x: | 58 |
| [Medusa (Bally 1981)](external/vpx-medusa) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 32 |
| [Memory Lane (Stern 1978)](external/vpx-memlane) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Men in Black Trilogy](external/vpx-meninblack) | :white_check_mark: | :white_check_mark: | :x: | :x: | 50 |
| [Metallica Pro, JPs (Stern 2013)](external/vpx-jpmetallica) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Meteor (Stern 1979)](external/vpx-meteor) | :white_check_mark: | :x: | :white_check_mark: | :x: | 42 |
| [Metropolis (Original 2022)](external/vpx-metropolis) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 46 |
| [MF DOOM (GOILL773 2024)](external/vpx-mfdoom) | :white_check_mark: |:white_check_mark: | :x: | :white_check_mark: | 35 |
| [MF DOOM (GOILL773 2024) FlexDMD version](external/vpx-mfdoomflexdmd) | :white_check_mark: |:white_check_mark: | :x: | :x: | 58 |
| [Miami Vice (Original 2020)](external/vpx-miamivice) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 33 |
| [Michael Jordan (Data East 1992)](external/vpx-michaeljordan) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 45 |
| [Mickey Mouse in Steamboat Willie](external/vpx-mickeysbw) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Middle Earth (Atari 1978)](external/vpx-middleearth) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Minions (Original 2017)](external/vpx-minions) | :white_check_mark: | :white_check_mark: | :x: | :x: | 30 |
| [Miraculous Original (2019)](external/vpx-miraculous) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Monday Night Football (Data East 1989)](external/vpx-mondaynightfootball) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 42 |
| [Monopoly (Stern 2001)](external/vpx-monopoly) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 40 |
| [Monster Bash VPW (Williams 1998)](external/vpx-monsterbash) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 33 |
| [Motordome (Bally 1986)](external/vpx-motordome) | :white_check_mark: | :x: | :white_check_mark: | :x: | 55 |
| [Mr. & Mrs. Pec-Men (LTD do Brasil 1980)](external/vpx-pecmen) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Mr. and Mrs. Pac-Man (Bally 1982)](external/vpx-mrandmrspacman) | :white_check_mark: | :x: | :white_check_mark: | :x: | 45 |
| [Mr. Black (Taito do Brasil 1984)](external/vpx-jpmrblack) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Muppets Tribute (Original 2022)](external/vpx-muppetstribute) | :white_check_mark: | :x: | :white_check_mark: | :x: | 58 |
| [Mustang (Stern 2014)](external/vpx-mustang) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 33 |
| [Mustang Bullitt LE](external/vpx-mustangbullitt) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 37 |
| [Mystery Castle (Alvin G 1993)](external/vpx-mysterycastle) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 45 |
| [Mystic (Bally 1980)](external/vpx-jpmystic) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [NBA Mac (MAC 1986)](external/vpx-nbamac) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Nemesis (Peyper 1986)](external/vpx-nemesis) | :white_check_mark: | :x: | :white_check_mark: | :x: | 49 |
| [Nevermind The Bollocks 1977 (Original 2024)](external/vpx-nmtbollocks) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [NFL (Stern 2001)](external/vpx-nfl) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 42 |
| [Night Moves (International Concepts 1989)](external/vpx-nightmoves) | :white_check_mark: | :x: | :white_check_mark: | :x: | 47 |
| [Night of the Living Dead '68 (Original 2018) Grunge Mod](external/vpx-nightofthelivingdeadgrunge) | :white_check_mark: | :x: | :x: | :x: | 45 |
| [Night of the Living Dead '68, Pinvention's (Original 2021)](external/vpx-nightofthelivingdeadpin) | :white_check_mark: | :x: | :x: | :x: | 38 |
| [Night Rider (Bally 1977)](external/vpx-nightrider) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Nightmare Before Christmas (Original 2024)](external/vpx-nbc) | :white_check_mark: | :white_check_mark: | :x: | :x: | 30 |
| [Nine Ball (Stern 1980)](external/vpx-nineball) | :white_check_mark: | :x: | :white_check_mark: | :x: | 45 |
| [Nip-It (Bally 1973)](external/vpx-nipit) | :white_check_mark: | :white_check_mark: | :x: | :x: | 54 |
| [Nitro Ground Shaker (Bally 1980)](external/vpx-nitrogs) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [No Good Gofers! (Williams 1997)](external/vpx-nogoodgofers) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 42 |
| [North Star (Gottlieb 1964)](external/vpx-northstar) | :white_check_mark: | :x: | :x: | :x: | 52 |
| [Nosferatu 1922 (Original 2023)](external/vpx-nosferatu) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Nugent (Stern 1978)](external/vpx-nugent) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Odisea Paris-Dakar (Peyper 1987)](external/vpx-odisea) | :white_check_mark: | :x: | :white_check_mark: | :x: | 42 |
| [Old Chicago (Bally 1976)](external/vpx-oldchicago) | :white_check_mark: | :x: | :x: | :x: | 58 |
| [Old Coney Island (Game Plan 1979)](external/vpx-oldconeyisland) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Operation Thunder (Gottlieb 1992)](external/vpx-opthunder) | :white_check_mark: | :x: | :white_check_mark: | :x: | 52 |
| [OXO (Williams 1973)](external/vpx-oxo) | :white_check_mark: | :x: | :x: | :x: | 45 |
| [Panthera (Gottlieb 1980)](external/vpx-panthera) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Paragon (Bally 1979)](external/vpx-paragon) | :white_check_mark: | :x: | :white_check_mark: | :x: | 49 |
| [Party Animal (Bally 1987)](external/vpx-partyanimal) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 42 |
| [Pat Hand (Williams 1975)](external/vpx-pathand) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Pet Sematary (TBA 2019)](external/vpx-petsemetary) | :white_check_mark: | :x: | :white_check_mark: | :x: | 55 |
| [Phantom of the Opera (Data East 1990)](external/vpx-phantom) | :white_check_mark: | :x: | :white_check_mark: | :x: | 38 |
| [Pharaoh (Williams 1981)](external/vpx-pharaoh) | :white_check_mark: | :x: | :white_check_mark: | :x: | 49 |
| [Phoenix (Williams 1978)](external/vpx-phoenix) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Pinball Lizard (Game Plan 1980)](external/vpx-pinballlizard) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Pinball Magic (Capcom 1995)](external/vpx-pinballmagic) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 43 |
| [Pink Panther (Gottlieb 1981) SoundMod Hybrid](external/vpx-pinkpanther) | :white_check_mark: | :x: | :white_check_mark: | :x: | 37 |
| [Pirate Gold (Chicago Coin 1969)](external/vpx-pirategold) | :white_check_mark: | :x: | :x: | :x: | 42 |
| [Pirates Life](external/vpx-pirateslife) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 40 |
| [Planet Of The Apes (Original 2021)](external/vpx-planetoftheapes) | :white_check_mark: | :x: | :white_check_mark: | :x: | 50 |
| [Pokémon Pinball, JPs (Original 2016)](external/vpx-jps-pokemon) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Polar Explorer (Taito do Brasil 1983)](external/vpx-jppolarexplorer) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Poltergeist (Original 2022)](external/vpx-poltergeist) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Pool Sharks (Bally/Midway 1990)](external/vpx-poolsharks) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [Popeye Saves the Earth (Bally 1994)](external/vpx-popeye) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 35 |
| [Predator 2 (Original 2019)](external/vpx-predator2) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Primus (Stern 2018)](external/vpx-primus) | :white_check_mark: | :x: | :x: | :x: | 47 |
| [Pulp Fiction (CGC 2023)](external/vpx-pulpfiction) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Punk Park (Original 2025)](external/vpx-punkpark) | :white_check_mark: | :white_check_mark: | :x: | :x: | 43 |
| [Q-Bert's Quest (Gottlieb 1983)](external/vpx-qbertquest) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 42 |
| [Queen: Show must go on (Original 2022)](external/vpx-queen) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Radical (Bally 1990)](external/vpx-rad) | :white_check_mark: | :x: | :white_check_mark: | :x: | 42 |
| [Rally (Taito do Brasil 1980)](external/vpx-jprally) | :white_check_mark: | :x: | :white_check_mark: | :x: | 58 |
| [Rambo First Blood Part II (TBA 2020)](external/vpx-rambo) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 54 |
| [Rambo: From first to last blood (Original 2025)](external/vpx-ramboflb) | :white_check_mark: | :white_check_mark: | :x: | :x: | 50 |
| [Ramones (Original 2021/1972)](external/vpx-ramones) | :white_check_mark: | :x: | :x: | :x: | 50 |
| [Rawhide (Stern 1977)](external/vpx-jprawhide) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Raygun Runner (Original 2024)](external/vpx-raygunrunner) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Re-Animator](external/vpx-reanimator) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 54 |
| [Real Ghostbusters,The (JPs Ghostbusters mod)](external/vpx-realghostbusters) | :white_check_mark: | :white_check_mark: | :x: | :x: | 57 |
| [Red & Ted's Road Show (Williams 1994)](external/vpx-redandteds) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 35 |
| [Ro Go (Bally 1974)](external/vpx-rogo) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Road Kings (Williams 1986)](external/vpx-roadkings) | :white_check_mark: | :x: | :white_check_mark: | :x: | 52 |
| [Robo-War (Gottlieb 1988)](external/vpx-robowar) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [RoboCop (Data East 1989)](external/vpx-robocop) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 38 |
| [RoboCop 3 (Original 2018)](external/vpx-robocop3) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Robot (Zaccaria 1985)](external/vpx-robot) | :white_check_mark: | :x: | :white_check_mark: | :x: | 41 |
| [Rock (Gottlieb 1985)](external/vpx-rock) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Rock Encore (Gottlieb 1985)](external/vpx-rockenc) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 58 |
| [Rocky TKO (Original 2021)](external/vpx-rockytko) | :white_check_mark: | :x: | :white_check_mark: | :x: | 55 |
| [Roller Disco (Gottlieb 1980)](external/vpx-rollerdisco) | :white_check_mark: | :x: | :white_check_mark: | :x: | 45 |
| [Rollercoaster Tycoon (Stern 2002)](external/vpx-rctycn) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |
| [Rolling Stones (Bally 1980)](external/vpx-jprollingstones) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Roman Victory (Taito do Brasil 1978)](external/vpx-jpromanvictory) | :white_check_mark: | :x: | :x: | :x: | 58 |
| [Royal Flush (Gottlieb 1976)](external/vpx-royalflush) | :white_check_mark: | :x: | :x: | :x: | 57 |
| [Safe Cracker (Bally 1996)](external/vpx-safecracker) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 45 |
| [Samba (LTD do Brasil 1976)](external/vpx-samba) | :white_check_mark: | :x: | :x: | :x: | 47 |
| [Scooby Doo (2022)](external/vpx-scoobydoo) | :white_check_mark: | :white_check_mark: | :x: | :x: | 50 |
| [Scooby Doo! and KISS Rock n' Roll Mystery (Original 2021)](external/vpx-sdookiss) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Secret Agent (Original 2024)](external/vpx-secretagent) | :white_check_mark: | :white_check_mark: | :x: | :x: | 30 |
| [Senna Prototype Edition (Culik Pinball 2020) 1.0.1](external/vpx-senna) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Serious Sam (Original 2017)](external/vpx-ssam) | :white_check_mark: | :white_check_mark: | :x: | :x: | 53 |
| [Sesame Street (Original 2021)](external/vpx-sesamestreet) | :white_check_mark: | :x: | :white_check_mark: | :x: | 50 |
| [Sexy Girl (Arkon 1980)](external/vpx-sexygirl) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Shaq Attaq (Gottlieb 1995)](external/vpx-shaq) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 35 |
| [Shooting the Rapids (Zaccaria 1979)](external/vpx-shootingtherapids) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Shovel Knight (Original 2017)](external/vpx-shovel) | :white_check_mark: | :white_check_mark: | :x: | :x: | 48 |
| [Shrek (Stern 2008)](external/vpx-shrek) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 35 |
| [Silverball Mania (Bally 1978)](external/vpx-silverballmania) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 55 |
| [Sinbad (Gottlieb 1978)](external/vpx-sinbad) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Sir Lancelot (Peyper 1994)](external/vpx-sirlancelot) | :white_check_mark: | :x: | :white_check_mark: | :x: | 45 |
| [Six Million Dollar Man, The (Bally 1978) Sound MOD](external/vpx-sixmilliondollarman) | :white_check_mark: | :x: | :white_check_mark: | :x: | 55 |
| [Skate Ball (Bally 1980)](external/vpx-skateball) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Skylab (Williams 1974)](external/vpx-skylab) | :white_check_mark: | :x: | :x: | :x: | 55 |
| [Soccer Kings (Zaccaria 1982)](external/vpx-socrking) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [solar City (Gottlieb 1976)](external/vpx-solarcity) | :white_check_mark: | :x: | :x: | :x: | 52 |
| [Sorcerer (Williams 1985)](external/vpx-jpsorcerer) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Sorcerer (Williams 1985)](external/vpx-sorcerer) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 43 |
| [Sound Stage  Chicago Coin 1976](external/vpx-soundstage) | :white_check_mark: | :x: | :x: | :x: | N/A |
| [Space Mission (Williams 1976)](external/vpx-spacemission) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Space Oddity (Original 2022)](external/vpx-spaceoddity) | :white_check_mark: | :x: | :white_check_mark: | :x: | 37 |
| [Space Patrol (Taito do Brasil 1978)](external/vpx-jpspacepatrol) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Space Rider (Geiger 1980)](external/vpx-spacerider) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Space Riders (Atari 1978)](external/vpx-spaceriders) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 46 |
| [Space Shuttle (Taito do Brasil 1982)](external/vpx-spaceshuttletaito) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Space Shuttle (Williams 1984)](external/vpx-spaceshuttle) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 48 |
| [Space Station (Williams 1987)](external/vpx-spacestation) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 38 |
| [Space Train Mac (MAC 1987)](external/vpx-spacetrainmac) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Speed Test (Taito do Brasil 1982)](external/vpx-jpspeedtest) | :white_check_mark: | :x: | :white_check_mark: | :x: | 55 |
| [Split Second (Stern 1981)](external/vpx-splitsecond) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [SpongeBob's Bikini Bottom Pinball](external/vpx-spongebob) | :white_check_mark: | :white_check_mark: | :x: | :x: | 35 |
| [Spy Hunter (Bally 1984)](external/vpx-spyhunter) | :white_check_mark: | :x: | :white_check_mark: | :x: | 42 |
| [Squid Game (Original 2024)](external/vpx-squidgame) | :white_check_mark: | :white_check_mark: | :x: | :x: | 48 |
| [Stampede (Stern 1977)](external/vpx-stampede) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Star Gazer (Stern 1980)](external/vpx-stargazer) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Star Trek (Enterprise Limited Edition) - Stern (2013)](external/vpx-startrekle) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 35 |
| [Star Trek the Mirror Universe (Zitt 2014)](external/vpx-startrekmu) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Star Wars Episode 1 (Original 2023)](external/vpx-starwarsepisode1) | :white_check_mark: | :x: | :white_check_mark: | :x: | 48 |
| [Stargate (Gottlieb 1995)](external/vpx-stargate) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 40 |
| [Stars (Stern 1978)](external/vpx-stars) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 48 |
| [Stingray (Stern 1977)](external/vpx-stingray) | :white_check_mark: | :x: | :white_check_mark: | :x: | 47 |
| [Strange Science (Bally 1986)](external/vpx-strangescience) | :white_check_mark: | :x: | :white_check_mark: | :x: | 36 |
| [Stranger Things – Stranger Edition (Original 2018)](external/vpx-strangerthings) | :white_check_mark: | :white_check_mark: | :x: | :x: | 45 |
| [Street Fighter 2 JP's Felsir Mod (original 2021)](external/vpx-sfiimod) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Strike (Zaccaria 1978)](external/vpx-strike) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Strikes and Spares (Bally 1978)](external/vpx-strikes) | :white_check_mark: | :x: | :white_check_mark: | :x: | 42 |
| [Student Prince (Williams 1968)](external/vpx-studentprince) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Sultan (Taito do Brasil 1979)](external/vpx-sultan) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Summer Time (Williams 1972)](external/vpx-summertime) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Super Mario Bros. (Gottlieb 1992)](external/vpx-supermario) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 38 |
| [Super Mario Bros. Mushroom World (Premier 1992)](external/vpx-smbmushroom) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 45 |
| [Super Mario Galaxy (Original 2021)](external/vpx-mariogalaxy) | :white_check_mark: | :white_check_mark: | :x: | :x: | 58 |
| [Super Spin (Gottlieb 1977)](external/vpx-superspin) | :white_check_mark: | :x: | :x: | :x: | 48 |
| [Superman (Atari 1979)](external/vpx-superman) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |
| [Superman and The Justice League (Original 2024)](external/vpx-supermanjl) | :white_check_mark: | :white_check_mark: | :x: | :x: | 58 |
| [Supersonic (Bally 1979)](external/vpx-jpsupersonic) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Surf 'N Safari (Gottlieb 1991)](external/vpx-surfnsafari) | :white_check_mark: | :x: | :white_check_mark: | :x: | 47 |
| [Surf Champ (Gottlieb 1976)](external/vpx-surfchamp) | :white_check_mark: | :x: | :x: | :x: | 52 |
| [Swamp Thing (Clairvius 2024)](external/vpx-swampthing) | :white_check_mark: | :white_check_mark: | :x: | :x: | 50 |
| [Swamp Thing 2.0 Bayou Edition (LTek 2024)](external/vpx-swampthingbayou) | :white_check_mark: | :white_check_mark: | :x: | :x: | 45 |
| [Swords of Fury (Williams 1988)](external/vpx-swordsoffury) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 29 |
| [T.M.N.T. - Remix (Original 2024)](external/vpx-tmntle) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |
| [Tango & Cash Pinball (TBA 2019)](external/vpx-tangoandcash) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 58 |
| [Target Alpha (Gottlieb 1976)](external/vpx-targetalpha) | :white_check_mark: | :x: | :x: | :x: | 49 |
| [Taxi Driver (Original 2024)](external/vpx-taxidriver) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Teenage Mutant Ninja Turtles Data East (1991)](external/vpx-tmnt) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 40 |
| [Tenacious D (Original 2025)](external/vpx-tenaciousd) | :white_check_mark: | :x: | :white_check_mark: | :x: | 33 |
| [Terminator 1 (Original 2019)](external/vpx-terminator1) | :white_check_mark: | :x: | :x: | :x: | 58 |
| [Terminator 3 (Stern 2003)](external/vpx-term3) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |
| [Texas Chainsaw Massacre 1974, The (Original 2020)](external/vpx-texaschainsawmassacre) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [The A-Team](external/vpx-theateam) | :white_check_mark: | :white_check_mark: | :x: | :x: | 55 |
| [The BeastMaster V2 SoundFX (Original 2021)](external/vpx-beastmaster) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [The Exorcist (Original 2023)](external/vpx-exorcist) | :white_check_mark: | :white_check_mark: | :x: | :x: | 55 |
| [The Fog (Original 2022)](external/vpx-thefog) | :white_check_mark: | :x: | :x: | :x: | 52 |
| [The Godfather (Original 2024)](external/vpx-godfather) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [The Goonies Never Say Die (Original 2021)](external/vpx-goonies) | :white_check_mark: | :white_check_mark: | :x: | :x: | 32 |
| [The Legend of Zelda (Original 2015)](external/vpx-legendofzelda) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [The Leprechaun King](external/vpx-theleprechaunking) | :white_check_mark: | :white_check_mark: | :x: | :x: | 40 |
| [The Mandalorian (Stern 2021)](external/vpx-mandalorian) | :white_check_mark: | :white_check_mark: | :x: | :x: | 49 |
| [The Mask (TBA 2019)](external/vpx-themask) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [The Matrix (Original 2023)](external/vpx-thematrix) | :white_check_mark: | :white_check_mark: | :x: | :x: | 48 |
| [The Pabst Can Crusher (Stern 2016)](external/vpx-pabstcancrusher) | :white_check_mark: | :x: | :x: | :x: | 50 |
| [The Party Zone (Bally 1991)](external/vpx-thepartyzone) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 45 |
| [The Rolling Stones (Stern 2011) (Balutito MOD)](external/vpx-rollingstonesbalutito) | :white_check_mark: | :x: | :white_check_mark: | :x: | 45 |
| [The Simpsons (Data East 1990)](external/vpx-thesimpsons) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 35 |
| [The Thing (Balutito Reskin)](external/vpx-thething) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 57 |
| [The Witcher (Original 2020)](external/vpx-witcher) | :white_check_mark: | :x: | :white_check_mark: | :x: | 48 |
| [Theatre of Magic (Bally 1995)](external/vpx-tom) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 35 |
| [Three Angels (Original 2018)](external/vpx-threeangels) | :white_check_mark: | :white_check_mark: | :x: | :x: | 40 |
| [Tiki Bob's Atomic Beach Party (Original 2021)](external/vpx-tikibob) | :white_check_mark: | :x: | :white_check_mark: | :x: | 46 |
| [Time 2000 (Atari 1978)](external/vpx-time2000) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Time Fantasy (Williams 1983)](external/vpx-timefantasy) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 42 |
| [Time Machine (Zaccaria 1983)](external/vpx-timemachinezac) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [TimeLord 3.0.10 (Luigi Saves the Universe) OriginalHybrid (Original 2022)](external/vpx-timelord) | :white_check_mark: | :white_check_mark: | :x: | :x: | 30 |
| [Tom & Jerry (Original 2018)](external/vpx-tomjerry) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 30 |
| [Tomb Raider (Original 2025)](external/vpx-tombraider) | :white_check_mark: | :white_check_mark: | :x: | :x: | 50 |
| [Tornado Rally (Original 2024)](external/vpx-tornadorally) | :white_check_mark: | :x: | :x: | :x: | 40 |
| [Transformers G1 Generation One (TBA 2018) 1.7](external/vpx-transformersg1) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Transformers Pro (Stern 2011)](external/vpx-transformers) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 40 |
| [Transporter: The Rescue (Bally 1989)](external/vpx-transporter) | :white_check_mark: | :x: | :white_check_mark: | :x: | 30 |
| [Tri Zone (Williams 1979)](external/vpx-trizone) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 51 |
| [Trick 'r Treat (Original 2023)](external/vpx-trickrtreat) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Trick Shooter (LTD do Brasil 1980)](external/vpx-trickshooter) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Triple X (Williams 1973)](external/vpx-triplex) | :white_check_mark: | :x: | :x: | :x: | 45 |
| [TRON Classic (Original 2022)](external/vpx-tron) | :white_check_mark: | :x: | :white_check_mark: | :x: | 45 |
| [Truck Stop (Bally 1988)](external/vpx-truckstop) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 38 |
| [Twilight Zone (Bally 1993)](external/vpx-tz) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 42 |
| [Twisted Metal (Clairvius 2024)](external/vpx-twistedmetal) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [TX-Sector (Gottlieb 1988)](external/vpx-txsector) | :white_check_mark: | :x: | :white_check_mark: | :x: | 59 |
| [Vampirella (Balutito 2024)](external/vpx-vampirella) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |
| [Vector (Bally 1982)](external/vpx-vector) | :white_check_mark: | :x: | :white_check_mark: | :x: | 45 |
| [Viking (Bally 1980)](external/vpx-viking) | :white_check_mark: | :x: | :white_check_mark: | :x: | 42 |
| [Viper (Stern 1981)](external/vpx-viper) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |
| [Volley (Gottlieb 1976)](external/vpx-volley) | :white_check_mark: | :x: | :x: | :x: | 55 |
| [Wacky Races Pinball Original (2022)](external/vpx-wackyraces) | :white_check_mark: | :white_check_mark: | :x: | :x: | 47 |
| [Warlok (Williams 1982)](external/vpx-warlok) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Warriors, The (1979) Edition 2023 Reskin (Iceman 2023)](external/vpx-thewarriors) | :white_check_mark: | :x: | :white_check_mark: | :x: | 52 |
| [Water World (Gottlieb 1995)](external/vpx-waterworld) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 46 |
| [Wayne's World (Original 2020)](external/vpx-waynesworld) | :white_check_mark: | :x: | :white_check_mark: | :x: | 50 |
| [Wheel of Fortune (Stern 2007)](external/vpx-wof) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 37 |
| [White Water (Williams 1993)](external/vpx-whitewater) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Wild West (Original 2024)](external/vpx-wildwest) | :white_check_mark: | :white_check_mark: | :x: | :x: | 55 |
| [Willy Wonka Pro LE](external/vpx-wonka) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Willy's Wonderland (Original 2021)](external/vpx-willyswonderland) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 45 |
| [Wizard! (Bally 1975)](external/vpx-wizard) | :white_check_mark: | :white_check_mark: | :x: | :x: | 60 |
| [Wolf Man (Peyper 1987)](external/vpx-wolfman) | :white_check_mark: | :x: | :white_check_mark: | :x: | 42 |
| [World Challenge Soccer (Gottlieb 1994)](external/vpx-wcsoccer) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 50 |
| [World Cup Soccer (Bally 1994)](external/vpx-worldcupsoccer) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 32 |
| [World Joker Tour, JP's (Original 2024)](external/vpx-jpwjt) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [World Poker Tour (Stern 2006)](external/vpx-wpt) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 40 |
| [WOW Monopoly, JPs (Original 2015)](external/vpx-wowmonopoly) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 60 |
| [Xenon (Bally 1980)](external/vpx-jpxenon) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Xenon (Bally 1980)](external/vpx-xenon) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x: | 40 |
| [Yamanobori - (KOMAYA 1981)](external/vpx-yamanobori) | :white_check_mark: | :x: | :x: | :x: | NA |
| [Yellow Submarine (Original 2021)](external/vpx-yellowsubmarine) | :white_check_mark: | :x: | :x: | :x: | 60 |
| [Zarza (Taito do Brasil 1982)](external/vpx-jpzarza) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
| [Zephy (LTD do Brasil 1982) [== Xenon (Bally, 1980) == Zarza (Taito, 1980) ]](external/vpx-zephy) | :white_check_mark: | :x: | :white_check_mark: | :x: | 50 |
| [Zip-A-Doo (Bally 1970)](external/vpx-zipadoo) | :white_check_mark: | :x: | :x: | :x: | 50 |
| [Zonderik (Belgian Gaming Company 1980)](external/vpx-zonderik) | :white_check_mark: | :x: | :white_check_mark: | :x: | 60 |
