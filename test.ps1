# Script to remove Byte Order Mark (BOM) from README.md files

$logFile = "remove_bom_results.log"
"Script started at $(Get-Date)" | Out-File -FilePath $logFile -Force -Encoding ASCII

# Get all README.md files in the external directory
$readmeFiles = Get-ChildItem -Path "external" -Recurse -Filter "README.md" -File

"Found $($readmeFiles.Count) README.md files to check" | Out-File -FilePath $logFile -Append -Encoding ASCII

$processedCount = 0
$updatedCount = 0

foreach ($readmeFile in $readmeFiles) {
    $processedCount++
    $relativePath = $readmeFile.FullName.Substring((Get-Location).ToString().Length + 1).Replace("\", "/")
    $folderName = Split-Path -Path (Split-Path -Path $readmeFile.FullName -Parent) -Leaf
    
    "Processing [$processedCount/$($readmeFiles.Count)]: $relativePath" | Out-File -FilePath $logFile -Append -Encoding ASCII
    
    try {
        # Read file as bytes to detect BOM
        $fileBytes = [System.IO.File]::ReadAllBytes($readmeFile.FullName)
        $hasBOM = $false
        $startIndex = 0
        
        # Check for UTF-8 BOM (EF BB BF)
        if ($fileBytes.Length -ge 3) {
            if ($fileBytes[0] -eq 0xEF -and $fileBytes[1] -eq 0xBB -and $fileBytes[2] -eq 0xBF) {
                $hasBOM = $true
                $bomType = "UTF-8 BOM (EF BB BF)"
                $startIndex = 3
            }
        }
        
        # Only proceed if we found a BOM
        if ($hasBOM) {
            # Create a new array without the BOM
            $newBytes = New-Object byte[] ($fileBytes.Length - $startIndex)
            [System.Array]::Copy($fileBytes, $startIndex, $newBytes, 0, $newBytes.Length)
            
            # Write the file back without BOM
            [System.IO.File]::WriteAllBytes($readmeFile.FullName, $newBytes)
            
            "Removed $bomType from $folderName README.md" | Out-File -FilePath $logFile -Append -Encoding ASCII
            $updatedCount++
        } else {
            "No BOM found in $folderName README.md" | Out-File -FilePath $logFile -Append -Encoding ASCII
        }
    } catch {
        "Error processing $relativePath`: $_" | Out-File -FilePath $logFile -Append -Encoding ASCII
    }
    
    # Display progress for long-running tasks
    if ($processedCount % 50 -eq 0) {
        Write-Host "Processed $processedCount of $($readmeFiles.Count) files..."
    }
}

# Write summary to log
"" | Out-File -FilePath $logFile -Append -Encoding ASCII
"======================" | Out-File -FilePath $logFile -Append -Encoding ASCII
"Summary:" | Out-File -FilePath $logFile -Append -Encoding ASCII
"======================" | Out-File -FilePath $logFile -Append -Encoding ASCII
"Total README files processed: $processedCount" | Out-File -FilePath $logFile -Append -Encoding ASCII
"README files with BOM removed: $updatedCount" | Out-File -FilePath $logFile -Append -Encoding ASCII
"Script completed at $(Get-Date)" | Out-File -FilePath $logFile -Append -Encoding ASCII

# Display summary in console
Write-Host "Script completed!" -ForegroundColor Green
Write-Host "Total README files processed: $processedCount" -ForegroundColor Cyan
Write-Host "README files with BOM removed: $updatedCount" -ForegroundColor Yellow
Write-Host "Full details in: $logFile" -ForegroundColor White