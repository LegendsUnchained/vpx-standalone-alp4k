import os
from pathlib import Path

def edit_instructions_section(content):
    """Add standard bullet points to the Instructions section while preserving all other sections"""
    try:
        # Find the Instructions section
        start = content.index("## Instructions")
        # Find the next section header or end of file
        next_section = content.find("## ", start + 1)
        if next_section == -1:
            next_section = len(content)
        
        # Split content into parts
        before = content[:start]
        instructions_section = content[start:next_section]
        after = content[next_section:]
        
        # Process the instructions section
        lines = instructions_section.split('\n')
        header = lines[0]  # "## Instructions"
        
        # Standard bullet points to add
        standard_points = [
            "- Make sure to use the Table Manager to install this table.",
            "- Instructions can be found on the wiki [Add Table - Manual](https://github.com/LegendsUnchained/vpx-standalone-alp4k/wiki/%5B04%5D-%F0%9F%A7%A1-TM-%E2%80%90-Other-Features#add-table---manual)",
            "- If the table requires any additional files/steps, click `GO TO TABLE` after adding, and the TM will open to the relevant table folder."
        ]
        
        # Find any remaining instruction lines (starting with - or >)
        remaining_lines = []
        for line in lines[1:]:  # Skip the header
            line = line.strip()
            if line and (line.startswith('-') or line.startswith('>')):
                remaining_lines.append(line)
        
        # Build the new instructions section
        new_instructions = header + '\n\n' + '\n'.join(standard_points)
        if remaining_lines:
            new_instructions += '\n' + '\n'.join(remaining_lines)
        new_instructions += '\n\n'  # Add extra newline after all instructions
        
        # Combine all parts
        return before + new_instructions + after
        
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
            
            # Edit instructions section
            new_content = edit_instructions_section(content)
            
            # Write back to file
            with open(readme_path, 'w', encoding='utf-8') as f:
                f.write(new_content)
                
            print(f"Processed {readme_path}")
            
        except Exception as e:
            print(f"Error processing {readme_path}: {e}")

if __name__ == "__main__":
    main() 