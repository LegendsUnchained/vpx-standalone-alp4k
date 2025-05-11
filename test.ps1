$content = Get-Content -Path README.md
$result = @()

foreach ($line in $content) {
    # Match table rows ending with FPS number (digits) followed by |
    if ($line -match '^\| \[.*?\].*\| (\d+) \|$') {
        # Insert " :x: |" before the FPS number
        $line = $line -replace '(\|) (\d+) \|$', '$1 :x: | $2 |'
    }
    $result += $line
}

Set-Content -Path README.md -Value $result
Write-Host "Successfully added Has Puppack column to all table rows."