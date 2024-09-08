import os
import re
import datetime
import html
import subprocess

debug_folder_limit = 1000

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

project_directory = os.getcwd() + '/'

def get_earliest_commit(repo_path, file_path):
    """Gets the earliest commit for a file."""
    try:
        result = subprocess.run(
            ['git', 'log', '--pretty=format:%ad', '--date=format:%Y-%m-%d %H:%M:%S', '--follow', '--', file_path],
            cwd=repo_path,
            capture_output=True,
            text=True
        )
        commit_date = result.stdout.strip().splitlines()[0]
        return commit_date
    except subprocess.CalledProcessError:
        return None

def get_latest_commit(repo_path, folder_path):
  """Gets the latest commit for a folder."""
  try:
    result = subprocess.run(
      ['git', 'log', '-n 1', '--pretty=format:%ad', '--date=format:%Y-%m-%d %H:%M:%S', folder_path],
      cwd=repo_path,
      capture_output=True,
      text=True
    )
    commit_date = result.stdout.strip().splitlines()[0]
    return commit_date
  except subprocess.CalledProcessError:
    return None

def generate_html_table(repo_path, folder_path):
  """Generates an HTML table listing subfolders with commit history."""

  html_content = """
  <!DOCTYPE html>
  <html>
  <head>
    <title>Table Listing</title>
    <link rel="stylesheet" href="https://cdn.datatables.net/2.1.5/css/dataTables.dataTables.css" />
    <script src="https://code.jquery.com/jquery-3.7.1.js"></script>
    <script src="https://cdn.datatables.net/2.1.5/js/dataTables.js"></script>
</head>
  <body>
    <table id="myTable" class="display" style="width:100%">
     <thead>
      <tr>
        <th>Table Name</th>
        <th>Image</th>
        <th>Date Added</th>
        <th>Latest Update</th>
      </tr>
     </thead>
  """

  index = 0
  for folder_name in os.listdir(folder_path):
    subfolder_path = os.path.join(folder_path, folder_name)
    print (f"{bcolors.HEADER}{folder_name}{bcolors.ENDC}")  
    if os.path.isdir(subfolder_path):
      xml_files = [f for f in os.listdir(subfolder_path) if f.endswith('.xml')]
      if xml_files:
        first_xml_file = os.path.join(subfolder_path, xml_files[0])
        earliest_xml_commit = get_earliest_commit(repo_path, first_xml_file)
        print(f"{bcolors.DEBUG}  DEBUG: Date Added: {earliest_xml_commit}")
      else:
        earliest_xml_commit = None
      png_files = [f for f in os.listdir(subfolder_path) if f.endswith('.png')]
      if png_files:
         first_png_file = os.path.join(subfolder_path, png_files[0])
      latest_folder_commit = get_latest_commit(repo_path, subfolder_path)
      print(f"{bcolors.DEBUG}  DEBUG: Latest Update: {latest_folder_commit}")
      html_content += f"<tr><td>{html.escape(folder_name)}</td>\n"
      html_content += f"<td><img src='/external/{folder_name}/{folder_name}.png' width='100px'></td>\n"
      html_content += f"<td>{earliest_xml_commit}</td>\n"
      html_content += f"<td>{latest_folder_commit}</td></tr>\n"
    index += 1
    if index == debug_folder_limit:
        break

  html_content += """
    </table>
  <script type="text/javascript">
    $(document).ready( function () {
        $('#myTable').DataTable();
    } );
  </script>
  </body>
  </html>
  """
  return html_content

# Replace 'path/to/your/repo' with the actual path to your repository
external_folder = os.path.join(project_directory, 'external')
print(external_folder)

html_output = generate_html_table(project_directory, external_folder)
print(html_output)
# Write the HTML content to a file
with open('index.html', 'w') as f:
  f.write(html_output)