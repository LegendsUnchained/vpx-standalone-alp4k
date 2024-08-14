import os;
import json;
import datetime;

# folder where root of vpx-standalone-alp4k-main is
project_directory = "C:/users/twsch/code/vpx/vps/"

# helper function to read JSON files
def read_json_file(file_path):
  with open(file_path, 'r') as f:
    data = json.load(f)
  return data

# helper function to find content from VPSDB
def find_by_id(data, search_id):
    if isinstance(data, dict):
        for key, value in data.items():
            if key == 'id' and value == search_id:
                return data
            elif isinstance(value, (dict, list)):
                result = find_by_id(value, search_id)
                if result:
                    return result
    elif isinstance(data, list):
        for item in data:
            result = find_by_id(item, search_id)
            if result:
                return result
    return None 

# helper function to convert from epoch to readable date time
def convert_to_datetime(epoch):
     dt = datetime.datetime.fromtimestamp(epoch / 1000)
     date_str = dt.strftime('%Y-%m-%d')
     time_str = dt.strftime('%H:%M')
     return date_str + " " + time_str + "\n"   

# generate the actual markdown
def generate_markdown(data):
  markdown_string = ""
  for key, value in data.items():
    if key == "url":
      markdown_string += f"![URL]({value})\n"
    if key == "imgUrl":
      markdown_string += f"* **Image**:\n\n"
      markdown_string += f"![Image]({value})\n"
    if key == "createdAt" or key == "updatedAt":
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
  return markdown_string

# open and read the JSON file from VPSDB
vpsdb_file_path = project_directory + 'vpsdb.json'
vpsdb_file_data = read_json_file(vpsdb_file_path)

# open a table-specific JSON file (eventually iterate)
table_file_path = project_directory + 'vpx-24_simple.json'
table_file_data = read_json_file(table_file_path)
table = table_file_data[0]

# use table-specific JSON file to set basic table info in readme
markdown_content = "# " + table["name"] +" \n\n"
markdown_content += f"* **Tester**: "+ table["tester"] +"\n\n"
markdown_content += f"* **FPS**: "+ table["FPS"] +"\n\n"

# lookup table in VPSDB and generate VPX content for readme
table_search_id = table["vps_table_id"]
result = find_by_id(vpsdb_file_data, table_search_id)
markdown_content += generate_markdown(result)

# get b2s file ids from table-specific JSON file and use them to get content
#   from VPSDB
rom_search_ids = table["vps_b2s_ids"]
markdown_content += "## DirectB2S \n\n"
counter = 1
for rom_search_id in rom_search_ids:
    if len(rom_search_ids) > 1:  
       markdown_content += "### Option: " + str(counter) + " \n\n"
    result = find_by_id(vpsdb_file_data, rom_search_id)
    rom_content = generate_markdown(result)
    markdown_content += rom_content
    counter += 1

rom_search_ids = table["vps_rom_ids"]
markdown_content += "## ROMs \n\n"
counter = 1
for rom_search_id in rom_search_ids:
    if len(rom_search_ids) > 1:  
       markdown_content += "### Option: " + str(counter) + " \n\n"
    result = find_by_id(vpsdb_file_data, rom_search_id)
    rom_content = generate_markdown(result)
    markdown_content += rom_content
    counter += 1

readme = open(project_directory + 'readme.md', "w")
readme.write(markdown_content)

