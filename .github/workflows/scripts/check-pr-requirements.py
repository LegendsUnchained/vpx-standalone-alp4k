import os

def check_subfolders(external_dir):
    """Recursively checks subfolders for requirements."""
    for subfolder in os.listdir(external_dir):
        subfolder_path = os.path.join(external_dir, subfolder)
        if os.path.isdir(subfolder_path):
            check_subfolder_requirements(subfolder_path)
            # Add your checks here for each subfolder

def check_subfolder_requirements(subfolder_path):
    """Checks specific requirements within a subfolder."""
    # Check for required files
    required_files = ["README.md", "launcher.png", "launcher.xml"]
    for file in required_files:
        if not os.path.exists(os.path.join(subfolder_path, file)):
            print(f"Error: {file} not found in {subfolder_path}")

    # Check for pinmame folders
    pinmame_folders = ["cfg", "ini", "nvram", "roms"]
    for folder in pinmame_folders:
        if not os.path.isdir(os.path.join(subfolder_path, "pinmame", folder)):
            print(f"Error: pinmame/{folder} not found in {subfolder_path}")

    # ... other checks ...

# Main function to start the process
def main():
    check_subfolder_requirements("external")

if __name__ == "__main__":
    main()