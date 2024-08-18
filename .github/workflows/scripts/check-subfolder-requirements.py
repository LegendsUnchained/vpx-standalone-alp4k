import os

def check_subfolder_requirements():
    # Check for required files
    required_files = ["README.md", "launcher.png", "launcher.xml"]
    for file in required_files:
        if not os.path.exists(file):
            print(f"Error: {file} not found")

    # Check for pinmame folders
    pinmame_folders = ["cfg", "ini", "nvram", "roms"]
    for folder in pinmame_folders:
        if not os.path.isdir(f"pinmame/{folder}"):
            print(f"Error: pinmame/{folder} not found")

    # ... other checks ...

if __name__ == "__main__":
    check_subfolder_requirements()