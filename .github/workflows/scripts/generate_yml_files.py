import os;
import json;
import re;
from git import Repo;
import urllib.parse;

debug_vpx_file_search = False
debug_b2s_file_search = False
debug_special_instructions = False
debug_folder = ''

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    DEBUG = '\033[90m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

# the GitHub Actions runner sets the working directory to 
# the root of the checked-out repository before executing 
# your steps.
project_directory = os.getcwd() + '/'

# open and read the JSON file from VPSDB
vpsdb_file_path = project_directory + '/.github/workflows/vpsdb.json'
with open(vpsdb_file_path, "r") as f:
  vpsdb_file_data = json.load(f)

# helper function to read JSON files
def read_json_file(file_path):
  with open(file_path, 'r') as f:
    data = json.load(f)
  return data

# recursive function to work through the external folders
def find_readme_file_in_subdirs(dir, indent):
  for entry in os.scandir(dir):
    if entry.is_dir() and entry.name.startswith("vpx-"):
      print (f"{bcolors.HEADER}{entry.name}{bcolors.ENDC}")  

      # try to find the .readme file
      found_in_sub = find_readme_file_in_subdirs(entry.path, indent+1)
      if found_in_sub:
        pass
      else:
        print(f"{bcolors.WARNING}     - README.md file NOT found in {entry.name}{bcolors.ENDC}")
    
    elif entry.is_file() and entry.name == "README.md":
      print(f"     - README.md file found in {dir}")
      folder = os.path.basename(dir)
      process_READMEmd_file(entry, folder)
      return True
  return False

def write_yml_header():
  yml_text = "# This configuration file is used to auto-generate README files\n"
  yml_text += "# if you have multiple b2s files or roms, use the - on a new line to denote each one\n\n"
  return yml_text

def parse_and_set_table_name(readme):
  table_name_pattern = r"^\S.*$" # grab first non-blank line
  try:
    match = re.search(table_name_pattern, readme, re.MULTILINE)
    if match:
      table_name = match.group(0).strip().lstrip('# ') 
      return f"name: {table_name}\n" 
    else:
      raise ValueError("Failed to read table name")
  except Exception as e:
    print(f"Error: {e}")

def get_earliest_commit_owner(repo_path, folder_path):
  try:
    repo = Repo(repo_path)
    commit_history = repo.git.log('--pretty=format:%ae', '--follow', '--', folder_path)
    earliest_commit_owner = commit_history.splitlines()[-1]
    username, domain = earliest_commit_owner.split("@")
    parts = username.split("+")
    # Return the part after the # if it exists, otherwise return the entire username
    trimmed_name = parts[1] if len(parts) > 1 else parts[0]
    if trimmed_name:
      return f"tester: {trimmed_name}\n"
    else: 
      return f"tester: ?\n"
  except Exception as e:
    print(f"Error: {e}")
    return None

def get_FPS(readme):
  # Find the index of the "FPS" column
  fps_column_index = None
  fps_row_number = 0
  for line in readme.splitlines():
    if "FPS" in line:
      parts = [part.strip() for part in line.split("|")]  # Trim whitespace
      fps_column_index = parts.index("FPS")
      break
    fps_row_number += 1

  if fps_column_index is None:
    return f"FPS: column not found\n"
  
  # Skip the header row and separator row
  value_line = readme.splitlines()[fps_row_number + 2]
  values = [part.strip() for part in value_line.split("|")] 
  # Find the row containing the FPS value
  return f"FPS: {values[fps_column_index]}\n"
  
def get_special_instructions(readme):
  # get any instructions after the #instructions header (excluding the 3 standard rows)
  instructions_pattern = r"## Instructions\n((?:.|\n)*)"
  match = re.search(instructions_pattern, readme, re.DOTALL)
  yml_text = ""

  if debug_special_instructions:
      print(f"{bcolors.DEBUG}  DEBUG: Special Instructions")
  if match:
    instructions = match.group(1).strip()  # Extract instructions (group 1)
    lines = instructions.splitlines()
    for line in lines:
      if (line.__contains__("Copy the contents of this repo folder to your USB drive")\
        or line.__contains__("Add your personalized launcher.elf and rename it to")\
        or line.__contains__("listed above and copy them into this folder")):
        pass
      else: 
        yml_text += f"     \"{line}\"\n"

  if debug_special_instructions:
    print(yml_text)    
  return f"special_instructions: \n{yml_text}"


def clean_url(url):
  # Remove scheme (http:// or https://)
  url = url.replace("http://", "").replace("https://", "")
  # Remove trailing parenthesis
  url = url.rstrip(")")
  # Remove forums/ path for VPUnivers
  url = url.replace("vpuniverse.com/forums/files/", "vpuniverse.com/files/")
  # Convert to lowercase
  url = url.lower()
  return url

def find_table_in_VPSDB(url):
  for table_data in vpsdb_file_data:
    if find_table_by_url_recursive(url, table_data):
      return (table_data)
  return None

def find_table_by_url_recursive(url, data):
  if isinstance(data, dict):
    for key, value in data.items():
      if key in ["url", "link", "website", "uri"] and clean_url(value) == url:
        return True
      elif isinstance(value, (dict, list)):
        if find_table_by_url_recursive(url, value):
          return True
  elif isinstance(data, list):
    for item in data:
      if find_table_by_url_recursive(url, item):
        return True
  return False

def find_vpx_file_ids(urls, table_data):
  vpx_file_found = False
  vpx_file_id = ""
  for table_file in table_data["tableFiles"]:
    if table_file["tableFormat"] == "VPX":
      for url in urls:
        for file_url in table_file["urls"]:
          if debug_vpx_file_search:
            print(f"{bcolors.DEBUG}  DEBUG: VPX URL Search")
            print(f"     -{clean_url(url)}")
            print(f"     -{clean_url(file_url["url"])}{bcolors.ENDC}")
          if clean_url(url) == clean_url(file_url["url"]):  # Case-insensitive comparison
            vpx_file_found = True
            vpx_file_id = table_file["id"]
            if debug_vpx_file_search:
              print(f"{bcolors.OKGREEN}     -MATCH!{bcolors.DEBUG} vpx id: {vpx_file_id}{bcolors.ENDC}")
            break
          if debug_vpx_file_search:
              print(f"{bcolors.FAIL}     -NO MATCH!{bcolors.ENDC}")
        if vpx_file_found:
          break  # Stop searching once a match is found
      if vpx_file_found:
        break
  if vpx_file_found:
    print("     - VPX file found!")
  else:
    print("     - VPX file NOT found")
  return f"vps_table_id: {vpx_file_id}\n"

def find_directb2s_file_ids(urls, table_data):
  b2s_file_found = False
  b2s_file_ids = []
  for b2s_file in table_data.get("b2sFiles", []):
    if len(urls) > 0:
      for url in urls:
        if "urls" in b2s_file:
          for file_url in b2s_file["urls"]:
            if debug_b2s_file_search:
              print(f"{bcolors.DEBUG}   DEBUG: DirectB2S URL Search")
              print(f"     -{clean_url(url)}")
              print(f"     -{clean_url(file_url["url"])}{bcolors.ENDC}")
            if (clean_url(url)) == clean_url(file_url["url"]):
              b2s_file_found = True
              b2s_file_ids.append(b2s_file["id"])
              if debug_vpx_file_search:
                print(f"{bcolors.OKGREEN}     -MATCH!{bcolors.DEBUG} directb2s id: {b2s_file["id"]}{bcolors.ENDC}")
              break
            if debug_b2s_file_search:
              print(f"{bcolors.FAIL}     -NO MATCH!{bcolors.ENDC}")
  if b2s_file_found:
    print("     - DirectB2S file found!")
  else:
    print("     - DirectB2S file NOT found")
  yml_text = "vps_b2s_ids: \n"
  for b2s_file_id in b2s_file_ids:
    yml_text += "     - " + b2s_file_id + "\n"
  return yml_text

def find_rom_file_info(urls, table_data):
  rom_file_found = False
  rom_file_info = []
  for rom_file in table_data.get("romFiles", []):
    for url in urls:
      for file_url in rom_file["urls"]:
        if clean_url(url) == clean_url(file_url["url"]):
          rom_file_found = True
          rom_file_info.append({
            "id": rom_file["id"],
            "version": rom_file["version"],
            "authors": ", ".join(rom_file.get("authors", []))
          })
          break  # Stop searching once a match is found
  if rom_file_found:
    print(f"     - Rom file found!")
  else:
    print(f"     - Rom file NOT found")
  yml_text = "vps_rom_ids: \n"
  for rom_file in rom_file_info:
    yml_text += "     - version: " + rom_file["version"] + "\n"
    yml_text += "       authors: " + rom_file["authors"] + "\n"
  return yml_text

def parse_download_links(readme):
  url_pattern = r"\(([^()]+)\)"
  urls = re.findall(url_pattern, readme)
  yml_text = ""
  # get the block of JSON for the table my matching a URL
  for url in urls:
    try:
      parsed_url = urllib.parse.urlparse(url)
      if parsed_url.scheme and parsed_url.netloc:
        if "profile/" not in url and "&showuser" not in url:
          url = clean_url(url)
    except ValueError:
      pass  # Ignore invalid URLs

    table_data = find_table_in_VPSDB(url)
    if table_data:
      yml_text = find_vpx_file_ids(urls, table_data)
      yml_text += find_directb2s_file_ids(urls, table_data)
      yml_text += find_rom_file_info(urls, table_data)
      break    
  else:
    print(f"Table ID not found!!")
  return yml_text

# process README.md file and generate yml file
def process_READMEmd_file(entry, dir):
   # open a table-specific JSON file (eventually iterate)
  folder_path = os.path.dirname(entry.path)
  with open(folder_path + '/' "README.md", "r", errors="replace") as f:
  #readme = open(folder_path + '/' "README.md", "r", errors="replace")
    readme = f.read()
    print('     - README.md file data loaded')
 
    yml_text = write_yml_header()
 
    # parse table name from first line of .md
    yml_text += parse_and_set_table_name(readme)

    # get tester from git history
    yml_text += get_earliest_commit_owner(project_directory, folder_path)

    # get FPS
    yml_text += get_FPS(readme)
    yml_text += get_special_instructions(readme)
    yml_text += parse_download_links(readme)

    #write output .yml files to temp folder for now = we can put them in folders later using this...
      #yml = open(folder_path + '/' + entry.name +  '.yml', "w")

    yml_file_path = project_directory + "/.github/workflows/temp_yml/" + dir +  '.yml'
    os.makedirs(os.path.dirname(yml_file_path), exist_ok=True)
    yml_file = open(yml_file_path, "w", encoding="utf-8", errors="ignore")
    yml_file.write(yml_text)
    yml_file.close()

    print(f'{bcolors.OKGREEN}     - {entry.name} yml generated!{bcolors.ENDC}')

# get total table count and display it
root_dir = project_directory + 'external'
external_folder_list = sorted(os.scandir(root_dir), key=lambda e: e.name)
table_count = len(external_folder_list)
print(str(table_count) + " tables in '% s':" % root_dir)


# loop through folders to find and process .alp4k files
find_readme_file_in_subdirs(root_dir, 0)
