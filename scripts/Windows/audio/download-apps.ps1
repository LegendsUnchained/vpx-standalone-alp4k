# A Powershell script to download necessary apps to convert vpx sounds to mono. 
# Function to download and extract a specific file from a ZIP archive
function Download-And-ExtractFile {
    param (
        [string]$url,
        [string]$outputFileName,
        [string]$exeFileName,
        [bool]$extractSpecificFile = $false
    )

    # Get the directory where the script is located
    $scriptDir = $PSScriptRoot

    # Define the destination for the downloaded file
    $zipFilePath = Join-Path -Path $scriptDir -ChildPath "$outputFileName.zip"

    # Download the ZIP file
    Write-Host "Downloading $url..."
    Invoke-WebRequest -Uri $url -OutFile $zipFilePath
    Write-Host "Download complete."

    if ($extractSpecificFile) {
        # Extract only the specified file from the ZIP
        Write-Host "Extracting $exeFileName from $zipFilePath..."
        Add-Type -AssemblyName System.IO.Compression.FileSystem
        $zip = [System.IO.Compression.ZipFile]::OpenRead($zipFilePath)
        $entry = $zip.Entries | Where-Object { $_.Name -eq $exeFileName }

        if ($entry) {
            $destinationPath = Join-Path -Path $scriptDir -ChildPath $exeFileName

            # Extract the file manually
            $fileStream = [System.IO.File]::OpenWrite($destinationPath)
            $entryStream = $entry.Open()
            $entryStream.CopyTo($fileStream)
            $entryStream.Close()
            $fileStream.Close()

            Write-Host "$exeFileName extracted to $scriptDir."
        } else {
            Write-Host "Error: $exeFileName not found in the ZIP archive."
        }

        $zip.Dispose()
    } else {
        # Extract the entire ZIP file
        Write-Host "Extracting $exeFileName to $scriptDir..."
        Add-Type -AssemblyName System.IO.Compression.FileSystem
        [System.IO.Compression.ZipFile]::ExtractToDirectory($zipFilePath, $scriptDir)
    }

    # Clean up: Remove the ZIP file
    Write-Host "Cleaning up..."
    Remove-Item -Path $zipFilePath -Force

    Write-Host "Process completed."
}

# Download and extract vpxtool.exe (extract entire ZIP)
Download-And-ExtractFile -url "https://github.com/francisdb/vpxtool/releases/download/v0.13.11/vpxtool-Windows-x86_64-v0.13.11.zip" -outputFileName "vpxtool" -exeFileName "vpxtool.exe"

# Download and extract only ffmpeg.exe from the ZIP
Download-And-ExtractFile -url "https://www.gyan.dev/ffmpeg/builds/ffmpeg-release-essentials.zip" -outputFileName "ffmpeg" -exeFileName "ffmpeg.exe" -extractSpecificFile $true
