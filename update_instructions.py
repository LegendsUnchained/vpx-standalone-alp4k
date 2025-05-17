import os
from pathlib import Path

def update_instructions(content):
    """Replace specific instruction lines while preserving all other content"""
    try:
        # Define the replacements
        replacements = {
            '- Make sure to use the Table Manager to install this table.': 
            '- Install this table through the Table Manager, using the `Add Table` > `Manual` page',
            
            '- Instructions can be found on the wiki [Add Table - Manual](https://github.com/LegendsUnchained/vpx-standalone-alp4k/wiki/%5B04%5D-%F0%9F%A7%A1-TM-%E2%80%90-Other-Features#add-table---manual)': 
            '- If you need help, more infomation found on the wiki: [TM - Add Table - Manual](https://github.com/LegendsUnchained/vpx-standalone-alp4k/wiki/%5B04%5D-%F0%9F%A7%A1-TM-%E2%80%90-Other-Features#add-table---manual)'
        }
        
        # Perform the replacements
        for old, new in replacements.items():
            content = content.replace(old, new)
            
        return content
        
    except Exception as e:
        print(f"Error updating instructions: {e}")
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
            
            # Update instructions
            new_content = update_instructions(content)
            
            # Only write if changes were made
            if new_content != content:
                with open(readme_path, 'w', encoding='utf-8') as f:
                    f.write(new_content)
                print(f"Updated {readme_path}")
            
        except Exception as e:
            print(f"Error processing {readme_path}: {e}")

if __name__ == "__main__":
    main() 