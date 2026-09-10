# Calls converrto-mono.ps1 to batch convert all tables in its folder - Boris
Get-ChildItem -Path "Path\To\Tables\Folder" -File -Filter "*.vpx" | ForEach-Object { .\converrto-mono.ps1 -table $_.Name }
