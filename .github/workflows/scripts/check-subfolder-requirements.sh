#!/bin/bash

# Check for README.md
test -f "README.md" && echo "README.md found in subfolder" || echo "README.md not found in subfolder"

# Check for launcher files
test -f "launcher.png" && echo "launcher.png found in subfolder" || echo "launcher.png not found in subfolder"
test -f "launcher.xml" && echo "launcher.xml found in subfolder" || echo "launcher.xml not found in subfolder"

# Check for pinmame folders
test -d "pinmame/cfg" && echo "pinmame/cfg found in subfolder" || echo "pinmame/cfg not found in subfolder"
test -d "pinmame/ini" && echo "pinmame/ini found in subfolder" || echo "pinmame/ini not found in subfolder"
test -d "pinmame/nvram" && echo "pinmame/nvram found in subfolder" || echo "pinmame/nvram not found in subfolder"
test -d "pinmame/roms" && echo "pinmame/roms found in subfolder"  || echo "pinmame/roms found in subfolder (check for empty folder)"

# Check for .gitkeep in empty folders
if [[ ! -e "pinmame/roms" ]]; then test -f "pinmame/roms/.gitkeep" && echo ".gitkeep found in empty pinmame/roms in subfolder" || echo ".gitkeep missing in empty pinmame/roms in subfolder"; fi

# Check for forbidden files and extensions
test -f "*.vpx" && echo "VPX files found in subfolder" || echo "No VPX files found in subfolder"
test -f "*.zip" -o -name "*.7z" -o -name "*.rar" && echo "ROM files found in subfolder" || echo "No ROM files found in subfolder"
test -f "*pup" && echo "PUP Media files found in subfolder" || echo "No PUP Media files found in subfolder"

