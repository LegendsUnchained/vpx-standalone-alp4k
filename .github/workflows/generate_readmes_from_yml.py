import os;
import json;
import yaml;
import datetime;

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

# the GitHub Actions runner sets the working directory to 
# the root of the checked-out repository before executing 
# your steps.
project_directory = os.getcwd() + '/'

# helper function to read JSON files
def read_json_file(file_path):
  with open(file_path, 'r') as f:
    data = json.load(f)
  return data

# helper function to find content from VPSDB by id
def find_vpsdb_item_by_id(data, search_id):
    if isinstance(data, dict):
        for key, value in data.items():
            if key == 'id' and value == search_id:
                return data
            elif isinstance(value, (dict, list)):
                result = find_vpsdb_item_by_id(value, search_id)
                if result:
                    return result
    elif isinstance(data, list):
        for item in data:
            result = find_vpsdb_item_by_id(item, search_id)
            if result:
                return result
    return None 

def find_vpsdb_roms(data, game_id, version, author=None):
  for rom_file in (game['romFiles'] for game in data if game['id'] == game_id):
    if rom_file[0]['version'] == version:
      return rom_file
  return None

# helper function to convert from epoch to readable date time
def convert_to_datetime(epoch):
     dt = datetime.datetime.fromtimestamp(epoch / 1000)
     date_str = dt.strftime('%Y-%m-%d')
     time_str = dt.strftime('%H:%M')
     return date_str + " " + time_str + "\n"   

# generate the actual markdown
def generate_markdown(data):
  markdown_string = "<table><tr><td valign='top' style='width:60%'>\n\n"
  image_string = ""
  for key, value in data.items():
    # if key == "url":
    #  markdown_string += f"<a href='{value['url']}' target='_blank'>\n"
    if key == "urls":
      for url_data in value:
        markdown_string += f"* **URL**: <a href='{url_data['url']}' target='_blank'>{url_data['url']}</a>\n"
    elif key == "imgUrl":
      #markdown_string += f"* **Image**:\n\n"
      #markdown_string += f"![Image]({value})\n"
      image_string += f"<img src='{value}'>\n\n"
    elif key == "createdAt" or key == "updatedAt":
        markdown_string += f"* **{key.capitalize()}**: "
        markdown_string += convert_to_datetime(value)
    elif isinstance(value, list):
      markdown_string += f"* **{key.capitalize()}**:\n"
      for item in value:
        markdown_string += f"  - {item}\n"
    elif isinstance(value, dict):
      markdown_string += f"* **{key.capitalize()}**:\n"
      for subkey, subvalue in value.items():
        markdown_string += f"  - {subkey}: {subvalue}\n"
    else:
      markdown_string += f"* **{key.capitalize()}**: {value}\n"
  markdown_string += "</td><td valign='top'>\n"
  markdown_string += image_string
  markdown_string += "</td></tr></table>\n\n"
  return markdown_string

# recursive function to work through the external folders
def find_alp4k_yml_file_in_subdirs(dir, indent):
  indent_str = "  " * indent
  for entry in os.scandir(dir):
    if entry.is_dir() and entry.name.startswith("vpx-"):
      print (f"{bcolors.HEADER}{entry.name}{bcolors.ENDC}")  

      # try to find the .yml file
      found_in_sub = find_alp4k_yml_file_in_subdirs(entry.path, indent+1)
      if found_in_sub:
        pass
      else:
        print(f"{bcolors.WARNING}     -.yml file NOT found in {entry.name}{bcolors.ENDC}")
    
    elif entry.is_file() and entry.name.endswith(".yml"):
      print(f"     -.yml file found in {dir}")
      process_alp4k_yml_file(entry)
      return True
  return False

# process .alp4k file and generate readme
def process_alp4k_yml_file(entry):
   # open a table-specific JSON file (eventually iterate)
  folder_path = os.path.dirname(entry.path)
  with open(entry.path, 'r') as file:
      table_file_data = yaml.safe_load(file)
  print('     -.yml file data loaded')
  table = table_file_data

  # use table-specific JSON file to set basic table info in readme
  markdown_content = "# " + table["name"] +" \n\n"
  markdown_content += "<table style='width:90%;'><tr><td valign='top' style='width:20%; border-right:1px solid grey;font-size:18px;'>\n\n"
  markdown_content += f"**Tester**: "+ table["tester"] +"\n\n"
  markdown_content += f"**FPS**: "+ str(table["FPS"]) +"\n\n"
  markdown_content += "</td><td valign='top' style='padding-left:20px;'>\n\n"
  markdown_content += "### Instructions: \n" 
  markdown_content += "- Copy the entire contents of this repo folder to your USB drive\n"
  markdown_content += f"- Add your personalized launcher.elf and rename it to '{table["name"]}.elf'\n"
  markdown_content += "- Download the VPX & one of the directb2s files and copy them into this folder.\n"
  markdown_content += "- Download the ROM to pinmame/roms (do not unzip).\n\n"
  markdown_content += f"#### {table["name"]} specific: \n" 
  for instruction in table["special_instructions"]:
    markdown_content += "- " + instruction + "\n"
  markdown_content += "</td></tr></table>\n\n"
  
  # lookup table in VPSDB and generate VPX content for readme
  markdown_content += "## VPX File \n\n"
  table_search_id = table["vps_table_id"]
  result = find_vpsdb_item_by_id(vpsdb_file_data, table_search_id)
  game_id = result['game']['id']
  markdown_content += generate_markdown(result)
  print('     -VPX file processed')

  # get b2s file ids from table-specific JSON file and use them to get content
  #   from VPSDB
  b2s_search_ids = table["vps_b2s_ids"]

  markdown_content += "## DirectB2S \n\n"
  counter = 1
  for b2s_search_id in b2s_search_ids:
      if len(b2s_search_ids) > 1:  
        markdown_content += "### Option: " + str(counter) + " \n\n"
      result = find_vpsdb_item_by_id(vpsdb_file_data, b2s_search_id)
      rom_content = generate_markdown(result)
      markdown_content += rom_content
      counter += 1
  print('     -DirectB2S file processed')

  # get rom version/author from table-specific JSON file and use them to get content
  #   from VPSDB
  rom_search_data = table["vps_rom_ids"]
  markdown_content += "## ROMs \n\n"
  counter = 1
  for item in rom_search_data:
      if len(rom_search_data) > 1:  
         markdown_content += "### Option: " + str(counter) + " \n\n"
      version = item.get('version', "")
      author = item.get('author', None)
      result = find_vpsdb_roms(vpsdb_file_data, game_id, version, author)
      for item in result:
          rom_content = generate_markdown(item)
          markdown_content += rom_content
      data = {"rom_files": result}
      counter += 1
  print('     -ROMs files processed.')

  readme = open(folder_path + '/' + table["name"] +  '_readme.md', "w")
  readme.write(markdown_content)
  readme.close()
  print(f'{bcolors.OKGREEN}     -{table["name"]} readme generated!{bcolors.ENDC}')

# open and read the JSON file from VPSDB
vpsdb_file_path = project_directory + 'vpsdb.json'
vpsdb_file_data = read_json_file(vpsdb_file_path)

# get total table count and display it
root_dir = project_directory + 'external'
external_folder_list = sorted(os.scandir(root_dir), key=lambda e: e.name)
table_count = len(external_folder_list)
print(str(table_count) + " tables in '% s':" % root_dir)

# loop through folders to find and process .alp4k files
find_alp4k_yml_file_in_subdirs(root_dir, 0)


