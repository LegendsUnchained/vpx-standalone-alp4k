import os
import sys

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

def check_subfolders(external_dir):
    """Recursively checks subfolders for requirements."""
    errors=[]
    for subfolder in sorted(os.listdir(external_dir)):
        subfolder_path = os.path.join(external_dir, subfolder)
        if os.path.isdir(subfolder_path):
            errors.extend(check_subfolder_requirements(subfolder_path))
            # Add your checks here for each subfolder
    return errors

def check_subfolder_requirements(subfolder_path):
    """Checks specific requirements within a subfolder."""
    # Get subfolder name
    subfolder_name = os.path.basename(subfolder_path)
    print(f"{bcolors.HEADER}Checking: {subfolder_name}{bcolors.ENDC}")

    errors = []
    # Check for required files
    required_files = ["README.md", 
                      subfolder_name + ".png", 
                      subfolder_name + ".xml"]
    for file in required_files:
        if not os.path.exists(os.path.join(subfolder_path, file)):
            errors.append(f"     {bcolors.WARNING}Warning: {file} not found in {subfolder_path}{bcolors.ENDC}")

    # Check for forbidden files
    forbidden_extensions = [".b2s", ".vpx", ".zip"]
    for root, dirs, files in os.walk(subfolder_path):
        for file in files:
            if file.endswith(tuple(forbidden_extensions)):
                errors.append(f"     {bcolors.FAIL}Error: Forbidden file {file} found in {subfolder_path}{bcolors.ENDC}")

    # Check for pinmame folders
    pinmame_folders = ["cfg", "ini", "nvram", "roms"]
    for folder in pinmame_folders:
        if not os.path.isdir(os.path.join(subfolder_path, "pinmame", folder)):
            errors.append(f"Error: pinmame/{folder} not found in {subfolder_path}")



    if len(errors) > 0:
        print (f"     {bcolors.WARNING}Errors Found!{bcolors.ENDC}")
    else:
        print (f"     ok.")
    return errors
    # ... other checks ...

# Main function to start the process
def main():
    errors = check_subfolders("external")
    if errors:
        print("Errors encountered:")
        for error in errors:
            print(error)
        sys.exit(1)  # Fail the build

if __name__ == "__main__":
    main()