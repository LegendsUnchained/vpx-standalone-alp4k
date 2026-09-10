# A script to convert stereo sounds in VPX tables to mono. 
param (
    [string]$table = "table.vpx"
)

# Get the directory where the script is located
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Definition

# Define the path to the ffmpeg and vpxtool executables
$ffmpegPath = Join-Path -Path $scriptDir -ChildPath "ffmpeg.exe"
$vpxtoolPath = Join-Path -Path $scriptDir -ChildPath "vpxtool.exe"

# Get the table name from the vpx file (without extension)
$tableName = [System.IO.Path]::GetFileNameWithoutExtension($table)

# Extract the table using vpxtool
Write-Host "Extracting VPX table using vpxtool..."
& $vpxtoolPath extract $table

# Define the parent directory as a folder in the current directory
$parentDirectory = Join-Path -Path $scriptDir -ChildPath $tableName

# Ensure the directory exists before proceeding
if (-Not (Test-Path -Path $parentDirectory)) {
    Write-Host "Error: Parent directory $parentDirectory not found."
    exit
}

# Define the paths for the JSON file and sound files directory
$jsonFilePath = Join-Path -Path $parentDirectory -ChildPath "sounds.json"
$directoryPath = Join-Path -Path $parentDirectory -ChildPath "sounds"

# Check if sounds.json exists
if (-Not (Test-Path -Path $jsonFilePath)) {
    Write-Host "Error: JSON file $jsonFilePath not found."
    exit
}

# Import the JSON file
$soundsData = Get-Content -Path $jsonFilePath | ConvertFrom-Json

# Filter the sounds with output_target set to "table"
$soundsToProcess = $soundsData | Where-Object { $_.output_target -eq "table" }

# Process each sound in the filtered list
foreach ($sound in $soundsToProcess) {
    $filePath = Join-Path -Path $directoryPath -ChildPath ($sound.name + ".wav")
    $tempFilePath = Join-Path -Path $directoryPath -ChildPath ($sound.name + "_mono.wav")

    if (Test-Path $filePath) {
        # Get the audio channel count using ffmpeg
        $channelCount = & $ffmpegPath -i $filePath 2>&1 | Select-String "Audio:"

        if ($channelCount -match "stereo") {
            Write-Host "Converting stereo file to mono: $filePath"

            # Convert the stereo file to mono and output to a temporary file
            & $ffmpegPath -y -i $filePath -ac 1 $tempFilePath

            # Replace the original file with the converted mono file
            Move-Item -Force $tempFilePath $filePath

            Write-Host "File converted and saved as mono: $filePath"
        } else {
            Write-Host "File is already mono: $filePath"
        }
    } else {
        Write-Host "File not found: $filePath"
    }
}

# Rename old VPX file with the extension .original
$originalFilePath = Join-Path -Path $scriptDir -ChildPath $table
$renamedFilePath = Join-Path -Path $scriptDir -ChildPath "$tableName.original"

if (Test-Path $originalFilePath) {
    Rename-Item -Path $originalFilePath -NewName $renamedFilePath
    Write-Host "Renamed original VPX file to $renamedFilePath"
} else {
    Write-Host "Error: Original VPX file not found."
    exit
}

# Reassemble the VPX file using vpxtool (with the original name)
Write-Host "Reassembling VPX file using vpxtool..."
& $vpxtoolPath assemble $tableName

# After reassembling, search for the matching .vpx file (ignoring part of the version numbers)
$baseTableName = $tableName -replace '(\.\d+)$', ''  # Remove only the last numeric part (e.g., from 2.4.41 to 2.4)

$matchingVPXFile = Get-ChildItem -Path $scriptDir -Filter "*.vpx" | Where-Object { $_.Name -like "$baseTableName*" }

# Check if the matching .vpx file was found
if ($matchingVPXFile) {
    $fullVPXName = $matchingVPXFile.Name
    Write-Host "Found reassembled VPX file: $fullVPXName"
    Write-Host "Ensuring file name matches $parentDirectory..."

    # Rename the .vpx file to match the parent directory
    $newVPXFileName = "$tableName.vpx"
    Rename-Item -Path $matchingVPXFile.FullName -NewName $newVPXFileName

    Write-Host "Renamed VPX file to $newVPXFileName"
} else {
    Write-Host "Error: No matching .vpx file found for $baseTableName"
    exit
}

Write-Host "Processing completed."
