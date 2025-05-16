import os
from pathlib import Path
import re

def remove_common_instructions(content):
    """Remove all common instruction patterns from lines after ## Instructions"""
    try:
        start = content.index("## Instructions")
        # Find the next line after the header
        next_line_start = content.find("\n", start) + 1
        if next_line_start == 0:
            return content
        
        # Split content into lines
        before = content[:next_line_start]
        after = content[next_line_start:]
        lines = after.split('\n')
        
        # Patterns to remove
        patterns = [
            r'(?:Rom file|ROM file).*stays in zip folder.*place zip file in.*pinmame/roms',
            r'Make sure.*\(\.(?:vpx|directb2b|directb2b2|ini|vbs)\).*named the (?:same|"exact" same)',
            r'Place (?:ROM|Rom) (?:zip )?file.*into.*pinmame/roms',
            r'Download the (?:table|vpx) and (?:directb2s|backglass).*extract.*copy.*to external/vpx-',
            r'The ROM zip file gets copied to.*pinmame/roms.*\(Do not unzip\)',
            r'Place.*ROM.*file.*into.*pinmame/roms.*DO NOT UNZIP',
            r'Make sure.*named the "exact" same',
            r'Download the table and directb2s versions listed above and copy them into this folder',
            r'Make sure \(\.vpx\), \(\.directb2s\), and \(\.ini\) files are all named the same',
            r'Place ROM zip file into vpx-[^/]+/pinmame/roms folder\. DO NOT UNZIP!',
            # New pattern for ROM download/copy instructions
            r'Download the ROM (?:listed above|file listed above|file).*copy (?:it|the ROM|the file)? to .+\. DO NOT UNZIP!?',
            r'Download the ROM (?:listed above|file listed above|file).*place (?:it|the ROM|the file)? in .+\. DO NOT UNZIP!?',
            r'Download the ROM (?:listed above|file listed above|file).*DO NOT UNZIP!?',
            # New pattern for bold ROM with backticks
            r'Download the \*\*ROM\*\* version listed above and copy it into `[^`]+` \(DO NOT UNZIP\)',
            # New pattern for VPX and DIRECTB2S download with bold formatting
            r'Download and extract the \*\*VPX\*\* and \*\*DIRECTB2S\*\* files ONLY, and copy them into the \*\*[^*]+\*\* folder',
            # New pattern for table VPX and directb2s download
            r'Download the table vpx file, and the directb2s file and copy to [^\n]+',
            # New pattern for VPX and DIRECTB2S versions with bold formatting
            r'Download and extract the \*\*VPX\*\* and \*\*DIRECTB2S\*\* versions listed above and copy them into the \*\*[^*]+\*\* folder',
            # More flexible pattern for ROM placement with variable ROM name and folder path
            r'Place [^.]+\.zip (?:ROM )?(?:in|into) the [^\n]+ folder[!.]? \*?Do Not unzip\*?',
            # New pattern for table and directb2s download with variable folder path
            r'Download the table and directb2s versions listed above, extract them and copy them into [^\n]+',
            # New pattern for ROM placement with variable name and path, with exclamation mark
            r'Place [^.]+\.zip ROM in the [^\n]+ folder\. \*Do Not unzip\*!',
            # New pattern for table and directb2s download with "this folder"
            r'Download the table and directb2s versions listed above, extract and copy them to this folder',
            # New pattern for VPX and DIRECTB2S download with bold formatting and variable folder
            r'Download and extract the \*\*VPX\*\* and \*\*DIRECTB2S\*\* files and copy it them to the \*\*[^*]+\*\* folder',
            # New pattern for table and directb2s download with backticks for folder name
            r'Download the table and directb2s listed above, extract \(if necessary\) and copy them to `[^`]+`',
            # New pattern for ensuring VPX, directb2s, and VBS files have the same name with backticks
            r'Make sure \(`\.vpx`\), \(`\.directb2s`\), and \(`\.vbs`\) files are all named the same',
            # New pattern for table and backglass download with variable folder path
            r'Download the table & backglass zips above, extract them and copy them to the [^\n]+ folder',
            # New pattern for table and directb2s download with optional parentheses and backticks
            r'Download the table \(\) and directb2s versions listed above, extract \(if necessary\) and copy them into `[^`]+`',
            # New pattern for table and backglass download with variable folder path
            r'Download the table & backglass zips above, extract them \(if neccssary\) and copy them into the [^\n]+ folder',
            # New pattern for table and directb2s download with variable folder name
            r'Download the table, and directb2s, extract them and copy/place them into [^\n]+',
            # New pattern for ROM zip placement with variable ROM name and folder path
            r'Open \(ROM\) Folder, \([^.]+\.zip\) stays in zip folder, place zip file in [^\n]+',
            # New pattern for ROM nv file placement with variable ROM name and folder path
            r'Open \(ROM\) Folder, \([^.]+\.nv\) place \(\.nv\) file in [^\n]+',
            # New pattern for downloading, unzipping and copying vpx and directb2s files
            r'Download table, unzip, and copy the vpx and directb2s into this folder',
            # New pattern for downloading and copying zips with backticks for folder path
            r'Download the table and directb2s zips above, extract them and copy into `[^`]+`',
            # New pattern for ensuring VPX, directb2s, VBS and INI files have the same name
            r'Make sure \(`\.vpx`\) \(`\.direct2b2s`\) \(`\.vbs`\) and \(`\.ini`\) are all named the same',
            # New pattern for downloading and extracting RAR file
            r'Download [^.]+\.rar and extract it\.',
            # New pattern for copying table and directb2s into a folder with backticks
            r'Copy the table and directb2s and copy them into `[^`]+`',
            # New pattern for ROM zip file copy with variable folder path and unzip warning
            r'The ROM zip file gets copied to [^\n]+ \(Do not unzip!\)',
            # New pattern for downloading package and copying vpx and directb2s files with backticks
            r'Download the package listed above, extract \(if necessary\) and copy the \.vpx and \.directb2s into `[^`]+`',
            # New pattern for table and backglass download with variable folder path
            r'Download the table & backglass zips above, extract them \(if neccssary\) and copy them into the [^\n]+ folder',
            # New pattern for downloading, unzipping, opening and copying vpx file
            r'Download table, unzip, open, and copy the vpx into [^\n]+',
            # New pattern for not copying .ini file
            r'Do not copy \.ini use one in repo folder that is already there',
            # New pattern for placing ROM zip file in folder with backticks
            r'Place `[^`]+\.zip` in the `[^`]+` folder \*Do Not unzip\*',
            # New pattern for downloading table and directb2s zips with optional extraction
            r'Download the table and directb2s zips above, extract \(if necessary\) and copy into [^\n]+',
            # New pattern for placing ROM zip file in folder path with unzip warning
            r'Place [^.]+\.zip in the [^\n]+ folder\. Do Not unzip',
            # New pattern for downloading VPX and DIRECTB2S versions with bold formatting
            r'Download the \*\*VPX\*\* and \*\*DIRECTB2S\*\* versions listed above and copy them into the \*\*[^*]+\*\* folder',
            # New pattern for downloading table, ROM and directb2s versions
            r'Download the table, ROM and directb2s versions listed above and copy them into this folder',
            # New pattern for placing ROM file with specific name in folder path
            r'Place ROM file \([^.]+\.zip\) in [^\n]+ - Do NOT unzip!',
            # New pattern for placing ROM nvram file with specific name in folder path
            r'In the table build open the nvram folder and place \([^.]+\.nv\) file in [^\n]+',
            # New pattern for Alt Color instructions
            r'If using the Alt Color, make an altcolor folder in the pinmame folder, then make a [^ ]+ folder and place the [^ ]+\.cRZ inside',
            r'Make sure \(`\.vpx`\), \(`\.directb2s`\), \(`\.ini`\), and \(`\.vbs`\) files are all named the same',
            r'Rom file \([^.]+\.zip\) stays in zip folder, place [^.]+\.zip ---> [^\n]+',
            r'Download the table, ROM and directb2s versions listed above',
            r'Copy the table \(\.vpx\) and backglass \(\.directb2s\) to this folder and remember to rename the vpx and directb2s files to "[^"]+"',
            r'The ROM zip gets copied to pinmame/roms \(do not unzip\)',
            # New pattern for copying .nv files with table download
            r'Copy \.nv files with table download to [^\n]+',
            # New pattern for copying ROM with unzip warning
            r'Copy ROM to [^\n]+\. Do not extract\.',
            # New pattern for downloading table, directb2s, and ROM versions
            r'Download the table, directb2s, and ROM versions listed above and copy them into this folder',
            # More flexible pattern for downloading and copying table and directb2s files
            r'Download the (?:table|vpx) (?:and|&) (?:directb2s|backglass) (?:versions?|files?)? (?:listed above|above)?,? (?:extract|unzip) (?:them|files)? (?:and|,) (?:copy|place) (?:them|files)? (?:to|into|in) (?:the )?[^\n]+',
            # More flexible pattern for ROM placement with parentheses and exclamation mark
            r'Place [^.]+\.zip (?:ROM )?(?:in|into) the [^\n]+ folder \(Do Not unzip!?\)',
            # More flexible pattern for downloading and copying with backticks around folder names
            r'Download the (?:table|vpx) (?:and|&) (?:directb2s|backglass) (?:versions?|files?)? (?:listed above|above)?,? (?:extract|unzip),? (?:and|,)? (?:copy|place) (?:them|files)? (?:to|into|in) `[^`]+`',
            # More flexible pattern for downloading table version and copying specific files
            r'Download the (?:table|vpx) (?:version|file) (?:listed above|above) (?:and|,) (?:copy|place) the (?:vpx|directb2s|b2s) (?:and|&) (?:directb2s|b2s) (?:file|files)? (?:into|to|in) (?:the )?[^\n]+',
            # More flexible pattern for placing ROM files with dash and exclamation
            r'Place (?:ROM|Rom) (?:file|files)? (?:in|into|to) [^\n]+ - \*?DO NOT UNZIP!?\*?',
            # More flexible pattern for ROM placement with parentheses around ROM name
            r'Download the (?:rom|ROM) \(([^.]+\.zip)\) stays in zip folder, place zip file in [^\n]+',
            # Pattern to remove lines starting with specific download instruction
            r'^Download the table, and directb2s.*$',
            # New pattern for table and directb2s download with comma and "versions"
            r'Download the table, and directb2s versions listed above, extract and copy them into this folder',
            # Pattern for table and directb2s listed above, extract (if necessary) and copy to `{folder path}`
            r'Download the table and directb2s listed above, extract \(if necessary\) and copy to `[^`]+`',
            # Pattern for Make sure (`.vpx`), (`.directb2s`), (`.ini`) and (`.vbs`) files are all named the same
            r'Make sure \(`\.vpx`\), \(`\.directb2s`\), \(`\.ini`\) and \(`\.vbs`\) files are all named the same',
            # Pattern for Download the table and directb2s listed above, extract (if necessary) and copy to external/`{folder name}`
            r'Download the table and directb2s listed above, extract \(if necessary\) and copy to external/`[^`]+`',
            # Pattern for Make sure (`.vpx`), (`.directb2s`), and (`.ini`) files are all named the same
            r'Make sure \(`\.vpx`\), \(`\.directb2s`\), and \(`\.ini`\) files are all named the same',
            # Pattern for downloading table zip and copying vpx and b2s files
            r'Download the table zip and copy the vpx and b2s files into this folder',
            # Pattern for downloading table and directb2s and copying all contents
            r'Download the table and directb2s listed above, extract and copy all contents into [^\n]+',
            # Pattern for downloading table and directb2s zips with backticks for folder path
            r'Download the table and directb2s zips above, extract and copy into `[^`]+`',
            # Pattern for ROM zip placement with backticks for ROM name and folder path
            r'Open \(ROM\) Folder, \(`[^`]+\.zip`\) stays in zip folder, place zip file in `[^`]+`',
            # Pattern for downloading table and directb2s files and copying to folder
            r'Download the table and directb2s files and copy them into the [^\n]+ folder',
            # Pattern for colored DMD option instructions
            r'Optional: if you choose to download the colored DMD option, download the cRZ file and place it under [^\n]+ folder',
            # Pattern for table and ROM download instructions
            r'Download the table and ROM versions listed above, extract \(if necessary\) and copy them to [^\n]+ folder',
            # Pattern for nvram file instructions
            r'Open nvram folder and Select/Copy [^.]+\.nv and move to [^\n]+',
            # Pattern for table zip file download and copy instructions
            r'Download the table zip file and copy the vpx and directb2s files into the [^\n]+ folder',
            # Pattern for lines starting with vpx and directb2s download instructions (including variations)
            r'^[-\s]*Download the vpx and directb2s versions.*$',
            r'^[-\s]*Download the vpx and directb2s versions.*copy them into `[^`]+`$',
            # Pattern for ROM placement with quoted ROM name and folder path
            r'Place "[^"]+\.zip" in the "[^"]+" folder \*Do Not unzip\*',
            # Pattern for lines starting with table and directb2s download instructions
            r'^[-\s]*Download the table and directb2s files above.*$',
            # Pattern for ROM download with specific format
            r'Download the rom \([^.]+\.zip\) which stays in zip folder, place zip file in [^\n]+',
            # Pattern for table file download with optional extraction
            r'Download the table file listed above, extract \(if necessary\) and copy them to [^\n]+',
            # Pattern for ROM zip placement with parentheses around ROM name and folder path
            r'Place the ROM zip \([^.]+\.zip\) in the [^\n]+ folder\. \(Do not un-zip!\)',
            # Pattern for table and directb2s download with folder name
            r'Download the table and directb2s versions listed above and copy them into [^\n]+',
            # Pattern for table and directb2s download with optional extraction and folder path
            r'Download the table, and directb2s versions listed above, extract \(if necessary\) and copy them to [^\n]+',
            # Pattern for simple table and directb2s download instruction
            r'Download the table and directb2s versions listed above',
            # Pattern for table and backglass download with optional extraction and folder path
            r'Download the table & backglass files above, extract them \(if necessary\) and copy to the [^\n]+ folder',
            # Pattern for table and backglass download with optional extraction and folder path (alternative format)
            r'Download the table & backglass above, extract \(if necessary\) and copy to [^\n]+',
            # Pattern for table and backglass zips download with extraction and folder path
            r'Download the table & backglass zips above, extract them and copy to the [^\n]+ folder',
            # Pattern for table and backglass zips download with extraction and folder path (alternative format)
            r'Download the table and backglass zips above, extract them and copy them to the [^\n]+ folder',
            # Pattern for zip file placement with folder path and unzip warning
            r'Place \.zip in the [^\n]+ folder \*Do Not unzip\*',
            # Pattern for table and backglass zips download with optional extraction and folder path
            r'Download the table & backglass zips above, extract \(if necessary\) and copy to [^\n]+',
        ]
        
        # Remove all lines matching any pattern
        new_lines = []
        for line in lines:
            if any(re.search(pattern, line, re.IGNORECASE) for pattern in patterns):
                continue
            new_lines.append(line)
        
        return before + '\n'.join(new_lines)
    except ValueError:
        return content

def main():
    # Get all README.md files in External directory
    external_dir = Path("External")
    readme_files = list(external_dir.rglob("README.md"))
    print(f"Found {len(readme_files)} README.md files")
    
    # Process each file
    for readme_path in readme_files:
        try:
            with open(readme_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # Remove common instruction patterns
            new_content = remove_common_instructions(content)
            
            # Write back to file
            with open(readme_path, 'w', encoding='utf-8') as f:
                f.write(new_content)
                
            print(f"Processed {readme_path}")
            
        except Exception as e:
            print(f"Error processing {readme_path}: {e}")

if __name__ == "__main__":
    main() 