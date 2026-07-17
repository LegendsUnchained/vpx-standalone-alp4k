"""
Cross-reference external/vpx-*/table.yml vpxChecksum / backglassChecksum
values against the wizard-tables archive scan results, proposing which
archive-scan-results.csv row each checksum should be attached to.

This script only PRODUCES A PROPOSAL (proposed-checksum-crossref.csv) for
manual review. It does not modify archive-scan-results.csv.
"""
import csv
import difflib
import os
import re

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
EXTERNAL_ROOT = os.path.join(REPO_ROOT, "external")
WIZARD_ROOT = r"M:\wizard tables"
CSV_PATH = os.path.join(WIZARD_ROOT, "archive-scan-results.csv")
OUT_PATH = os.path.join(WIZARD_ROOT, "proposed-checksum-crossref.csv")

VPX_CODES = """vpx-123 vpx-1812 vpx-2001 vpx-24 vpx-250cc vpx-4aces vpx-aaronspelling
vpx-acastle vpx-acdcluci vpx-acdcpoweredup vpx-acdcprovault vpx-acehigh vpx-alaska
vpx-alice1948 vpx-alijp vpx-alsgarageband vpx-amazonhunt vpx-andromeda vpx-aorab
vpx-apache vpx-apollo13 vpx-armyofdarkness vpx-astannie vpx-asterix vpx-austinpowers
vpx-avatar vpx-avengers vpx-babypacman vpx-badcats vpx-banzairun vpx-batman
vpx-batman66flash vpx-batmanforever vpx-baywatch vpx-big_trouble vpx-bigbangbar
vpx-bigbuckhunter vpx-bighurt vpx-bigshot vpx-blackknight vpx-blackknight2000
vpx-blackmagic4 vpx-blackpyramid vpx-blackrose vpx-bobcuspe vpx-bop vpx-bsdracula
vpx-bttf vpx-bugs vpx-cactuscanyon vpx-casino vpx-catburglars vpx-caveman
vpx-cenobite vpx-centaur vpx-centigrade37 vpx-centralpark vpx-cftbl vpx-champpub
vpx-christine vpx-chrono vpx-cityhunterpup vpx-cleopatra vpx-clockofetfog vpx-congo
vpx-cosmic vpx-countdownjp vpx-cowboy8ball vpx-crystalball vpx-csiled vpx-cyberrace
vpx-darkknight vpx-dbsse vpx-dbzbudokai vpx-dcrystalpup vpx-deadpool vpx-demoman
vpx-devilriders vpx-dexter vpx-diablo vpx-diner vpx-dirtyharry vpx-doctorwho
vpx-dolly vpx-doometernal vpx-dracblood vpx-dragong vpx-dragoni vpx-drakor vpx-dune
vpx-earthshaker vpx-eldorado84 vpx-f14tomcat vpx-familyguy vpx-fathom vpx-fbclassic
vpx-fifthelement vpx-fireball vpx-fireballii vpx-firepower vpx-firepower2
vpx-fishtales vpx-flight2000 vpx-flintstones vpx-fortnite vpx-freddy vpx-freddysnm
vpx-funhouse vpx-futurama vpx-gargamelpark vpx-genie vpx-getaway vpx-gilligansisland
vpx-gladiators vpx-gnr vpx-godzilla vpx-godzilla70th vpx-gogtrilogy vpx-goinnuts
vpx-goldeneye vpx-goldwings vpx-gorgar vpx-gotg vpx-grandslam1972 vpx-granny
vpx-grease vpx-gunship vpx-hardbody vpx-harleyd vpx-hauntedhouse vpx-hellraiser
vpx-highspeed vpx-hollywoodheat vpx-humptydumpty vpx-id4 vpx-incrediblehulk
vpx-indianajonestpa vpx-indylastmovie vpx-ironmaidenlotb vpx-jacksopen
vpx-jackstoopen vpx-jamesbond007 vpx-jaws50th vpx-johnnymnemonic
vpx-jpattackfrommars vpx-jpattackfrommarsle vpx-jpavengersclassic
vpx-jpfridaythe13th vpx-jplordoftherings vpx-jpseawitch vpx-jpslimer vpx-jpsnascar
vpx-jpt2 vpx-jptaf vpx-judgedredd vpx-jurassicpark vpx-jurassicparkle vpx-kingkong
vpx-kissbally vpx-knightrider vpx-koth vpx-lastactionhero vpx-lostinspace
vpx-machtwo vpx-madmax vpx-maryshelleyfrank vpx-matahari vpx-megadeth vpx-metallica
vpx-metallicapremium vpx-metallicapro vpx-meteort vpx-missionimp vpx-mm
vpx-monkeyisland vpx-monsterbashreskin vpx-mousin vpx-nags vpx-nascar
vpx-nbafastbreak vpx-nofear vpx-ogaucho vpx-pennantfever vpx-pharoahdr vpx-pinbot
vpx-pinkfloyd vpx-pistolpoker vpx-playboy1978 vpx-playboy1978de
vpx-playboy1978denude vpx-playboy2002 vpx-playboy35 vpx-playboy35m vpx-policeforce
vpx-potc vpx-powerrangers vpx-punchout vpx-quicksilver vpx-r911 vpx-ratfink
vpx-ripleys vpx-roadrunner vpx-rollergames vpx-royalrumble vpx-scaredstiff
vpx-seawitch vpx-secretservice vpx-sexygirlnude vpx-sfii vpx-shark vpx-sharkeys
vpx-simpsonspprty vpx-skyscraper vpx-sopranos vpx-southpark vpx-spacecadetge
vpx-spaceinvaders vpx-spacejam vpx-spidermanve vpx-starship vpx-starshipvpw
vpx-startrektng vpx-starwars vpx-starwarsburger vpx-starwarstrilogy vpx-swbadbatch
vpx-t2 vpx-taxi vpx-teedoff vpx-tftc vpx-thelostworld vpx-thelostworldjp
vpx-theshadow vpx-theshining vpx-tna vpx-tnes vpx-tommy vpx-torpedoalley
vpx-trailerparkboys vpx-transformersmod vpx-trident vpx-tronlegacy vpx-twd
vpx-twdle vpx-twdlep vpx-twister vpx-underwater vpx-victory vpx-vipernight
vpx-volkan vpx-vortex vpx-walkure vpx-walkyria vpx-whoanellie vpx-whodunnit
vpx-willow vpx-wipeout vpx-wrath vpx-wutheringwaves vpx-wwind vpx-xfiles
vpx-xfileshanibal vpx-xmen vpx-youngfrankenstein vpx-zelda vpx-zissou""".split()


def parse_table_yml(path):
    """Minimal parser for the flat key/value + simple list subset used here."""
    result = {}
    if not os.path.exists(path):
        return result
    with open(path, "r", encoding="utf-8-sig") as f:
        lines = f.readlines()

    i = 0
    n = len(lines)
    key_re = re.compile(r'^([A-Za-z0-9_]+):\s*(.*)$')
    while i < n:
        raw = lines[i]
        line = raw.rstrip("\n")
        m = key_re.match(line)
        if not m:
            i += 1
            continue
        key, val = m.group(1), m.group(2).strip()
        if val == "":
            # possible list on following lines
            items = []
            j = i + 1
            while j < n and re.match(r'^\s*-\s*(.+)$', lines[j]):
                item_m = re.match(r'^\s*-\s*(.+)$', lines[j])
                item_val = item_m.group(1).strip()
                item_val = item_val.strip('"').strip("'")
                items.append(item_val)
                j += 1
            if items:
                result[key] = items
                i = j
                continue
            else:
                result[key] = ""
                i += 1
                continue
        else:
            # strip surrounding quotes
            if (val.startswith('"') and val.endswith('"')) or (val.startswith("'") and val.endswith("'")):
                val = val[1:-1]
            result[key] = val
            i += 1
    return result


def get_readme_title(path):
    if not os.path.exists(path):
        return None
    with open(path, "r", encoding="utf-8-sig") as f:
        for line in f:
            line = line.strip()
            if line.startswith("#"):
                return line.lstrip("#").strip()
    return None


def normalize(s):
    if not s:
        return ""
    s = s.lower()
    s = s.replace("&#39;", "'")
    s = re.sub(r"[’‘`]", "'", s)
    s = re.sub(r"[^a-z0-9]+", "", s)
    return s


def extract_filename_from_notes(notes):
    if not notes:
        return None
    # quoted filename
    m = re.search(r"['\"]([^'\"]+\.(?:zip|rar|7z|vpx|directb2s))['\"]", notes, re.IGNORECASE)
    if m:
        return m.group(1)
    # bare filename after "Download "
    m = re.search(r"Download\s+([^\s].*\.(?:zip|rar|7z|vpx|directb2s))\s*$", notes, re.IGNORECASE)
    if m:
        return m.group(1).strip()
    return None


def as_list(v):
    if v is None:
        return []
    if isinstance(v, list):
        return v
    return [v]


def core_title(s):
    """Text before the first parenthesis, e.g. 'Ali' from 'Ali (Stern 1980)'.
    Falls back to the full string when there's no parenthesis."""
    if not s:
        return ""
    idx = s.find("(")
    return s[:idx] if idx != -1 else s


def main():
    with open(CSV_PATH, "r", encoding="utf-8-sig", newline="") as f:
        rows = list(csv.DictReader(f))

    for r in rows:
        rel = os.path.relpath(r["Path"], WIZARD_ROOT)
        parts = rel.split(os.sep)
        r["_wizard_folder"] = parts[0]
        r["_basename"] = os.path.basename(r["Path"])

    wizard_folders = sorted(set(r["_wizard_folder"] for r in rows))
    all_wizard_folders = sorted(
        d for d in os.listdir(WIZARD_ROOT)
        if os.path.isdir(os.path.join(WIZARD_ROOT, d))
    )
    norm_core_map = {wf: normalize(core_title(wf)) for wf in all_wizard_folders}
    norm_folder_map = {}
    for wf in all_wizard_folders:
        norm_folder_map.setdefault(normalize(wf), []).append(wf)

    out_rows = []

    for code in VPX_CODES:
        folder = os.path.join(EXTERNAL_ROOT, code)
        yml_path = os.path.join(folder, "table.yml")
        readme_path = os.path.join(folder, "README.md")

        if not os.path.exists(yml_path):
            out_rows.append({
                "VpxCode": code, "ChecksumType": "ERROR", "Checksum": "",
                "Title": "", "MatchedPath": "", "Confidence": "NO_TABLE_YML",
                "Candidates": ""
            })
            continue

        data = parse_table_yml(yml_path)
        title = get_readme_title(readme_path) or ""
        norm_title = normalize(title)

        vpx_checksums = as_list(data.get("vpxChecksum"))
        backglass_checksums = as_list(data.get("backglassChecksum"))
        table_notes = data.get("tableNotes", "")
        backglass_notes = data.get("backglassNotes", "")

        for checksum_type, checksums, notes, match_ext in (
            ("vpx", vpx_checksums, table_notes, ".vpx"),
            ("backglass", backglass_checksums, backglass_notes, ".directb2s"),
        ):
            if not checksums:
                continue

            notes_filename = extract_filename_from_notes(notes)
            candidate_rows = []
            confidence = "NONE"

            if notes_filename:
                nf_norm = notes_filename.lower()
                candidate_rows = [
                    r for r in rows
                    if r["_basename"].lower() == nf_norm and match_ext in r["MatchedFor"]
                ]
                if len(candidate_rows) == 1:
                    confidence = "HIGH_FILENAME"
                elif len(candidate_rows) > 1:
                    confidence = "AMBIGUOUS_FILENAME"

            if not candidate_rows:
                matched_folders = norm_folder_map.get(norm_title, [])
                if len(matched_folders) == 1:
                    folder_rows = [
                        r for r in rows
                        if r["_wizard_folder"] == matched_folders[0] and match_ext in r["MatchedFor"]
                    ]
                    if len(folder_rows) == 1:
                        candidate_rows = folder_rows
                        confidence = "MEDIUM_TITLE"
                    elif len(folder_rows) > 1:
                        candidate_rows = folder_rows
                        confidence = "AMBIGUOUS_TITLE_MULTI_FILE"
                    else:
                        confidence = f"FOLDER_MATCHED_NO_ARCHIVE ({matched_folders[0]})"
                elif len(matched_folders) > 1:
                    confidence = "AMBIGUOUS_TITLE_MULTI_FOLDER"
                    candidate_rows = [
                        r for r in rows
                        if r["_wizard_folder"] in matched_folders and match_ext in r["MatchedFor"]
                    ]

            if not candidate_rows and norm_title and not confidence.startswith("FOLDER_MATCHED_NO_ARCHIVE"):
                # Fuzzy fallback: compare CORE titles (text before the first
                # parenthesis) so that shared "(Manufacturer Year)" tags don't
                # dominate the similarity score for short/unrelated titles.
                norm_core = normalize(core_title(title))
                scored = []
                if norm_core:
                    for wf in all_wizard_folders:
                        ratio = difflib.SequenceMatcher(None, norm_core, norm_core_map[wf]).ratio()
                        scored.append((ratio, wf))
                    scored.sort(key=lambda x: x[0], reverse=True)

                if scored and scored[0][0] >= 0.60:
                    top_ratio = scored[0][0]
                    ties = [wf for ratio, wf in scored if ratio >= top_ratio - 0.05 and ratio >= 0.60]
                    if len(ties) == 1:
                        folder_rows = [
                            r for r in rows
                            if r["_wizard_folder"] == ties[0] and match_ext in r["MatchedFor"]
                        ]
                        if folder_rows:
                            candidate_rows = folder_rows
                            confidence = f"LOW_FUZZY_TITLE ({top_ratio:.2f})"
                        else:
                            confidence = f"FOLDER_MATCHED_NO_ARCHIVE ({ties[0]}, fuzzy {top_ratio:.2f})"
                    else:
                        folder_rows = [
                            r for r in rows
                            if r["_wizard_folder"] in ties and match_ext in r["MatchedFor"]
                        ]
                        if folder_rows:
                            candidate_rows = folder_rows
                            confidence = f"AMBIGUOUS_FUZZY_TITLE ({top_ratio:.2f})"
                        else:
                            confidence = f"FOLDER_MATCHED_NO_ARCHIVE ({'/'.join(ties)}, fuzzy {top_ratio:.2f})"

            matched_path = candidate_rows[0]["Path"] if len(candidate_rows) == 1 else ""
            candidates_str = "; ".join(sorted(set(r["Path"] for r in candidate_rows))) if len(candidate_rows) != 1 else ""

            out_rows.append({
                "VpxCode": code,
                "ChecksumType": checksum_type,
                "Checksum": "; ".join(checksums),
                "Title": title,
                "MatchedPath": matched_path,
                "Confidence": confidence,
                "Candidates": candidates_str,
            })

    with open(OUT_PATH, "w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=[
            "VpxCode", "ChecksumType", "Checksum", "Title", "MatchedPath", "Confidence", "Candidates"
        ])
        writer.writeheader()
        writer.writerows(out_rows)

    # Summary
    from collections import Counter
    conf_counts = Counter(r["Confidence"].split(" (")[0] for r in out_rows)
    print(f"Total checksum entries evaluated: {len(out_rows)}")
    for conf, count in conf_counts.most_common():
        print(f"  {conf}: {count}")
    print(f"Written to {OUT_PATH}")


if __name__ == "__main__":
    main()
