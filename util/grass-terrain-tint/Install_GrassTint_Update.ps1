#requires -Version 5.1
[CmdletBinding()]
param([string]$GameRoot)
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
if ([string]::IsNullOrWhiteSpace($GameRoot)) { $GameRoot = $PSScriptRoot }
if ([string]::IsNullOrWhiteSpace($GameRoot)) { throw 'Pass -GameRoot with the game folder.' }
$GameRoot = (Resolve-Path -LiteralPath $GameRoot).ProviderPath
$payload = Join-Path $PSScriptRoot '_grass_tint_update_'
$manifest = Get-Content -LiteralPath (Join-Path $payload 'manifest.json') -Raw | ConvertFrom-Json
$binaryDir = $null
foreach ($name in @('bin', 'bin\Release')) {
    $candidate = Join-Path $GameRoot $name
    if (Test-Path -LiteralPath (Join-Path $candidate 'xrEngine.exe') -PathType Leaf) { $binaryDir = $candidate; break }
}
if (!$binaryDir) { throw 'No xrEngine.exe found in bin or bin\Release. Extract this update into the game folder.' }
$renderer = Join-Path $binaryDir 'xrRender_R4.dll'
if (Test-Path -LiteralPath $renderer) {
    try {
        $handle = [IO.File]::Open($renderer, [IO.FileMode]::Open, [IO.FileAccess]::ReadWrite, [IO.FileShare]::None)
        $handle.Dispose()
    } catch { throw 'Close the game before installing the renderer update.' }
}
$copies = @()
foreach ($entry in $manifest.files) {
    $relative = [string]$entry.path
    if ([IO.Path]::IsPathRooted($relative) -or $relative -match '(^|[\\/])\.\.([\\/]|$)') { throw 'Invalid update path.' }
    $source = Join-Path $payload $relative
    if (!(Test-Path -LiteralPath $source -PathType Leaf) -or (Get-FileHash -LiteralPath $source -Algorithm SHA256).Hash -ne $entry.sha256) {
        throw "Incomplete or damaged update: $relative. Extract the complete archive again."
    }
    if ($relative.StartsWith('bin/')) { $destination = Join-Path $binaryDir $relative.Substring(4) }
    elseif ($relative.StartsWith('presets/')) { $destination = Join-Path (Join-Path $GameRoot '_appdata_grass_tint_') $relative.Substring(8) }
    else { $destination = Join-Path $GameRoot $relative }
    $copies += @{ Source = $source; Destination = $destination }
}
$backup = Join-Path $GameRoot ('_grass_tint_backup_v0.2_' + (Get-Date -Format 'yyyyMMdd_HHmmss_fff'))
function Save-Backup([string]$Path) {
    if (Test-Path -LiteralPath $Path -PathType Leaf) {
        $relative = $Path.Substring($GameRoot.Length).TrimStart([char[]]'\/')
        $target = Join-Path $backup $relative
        [IO.Directory]::CreateDirectory([IO.Path]::GetDirectoryName($target)) | Out-Null
        [IO.File]::Copy($Path, $target, $false)
    }
}
foreach ($copy in $copies) {
    Save-Backup $copy.Destination
    [IO.Directory]::CreateDirectory([IO.Path]::GetDirectoryName($copy.Destination)) | Out-Null
    [IO.File]::Copy($copy.Source, $copy.Destination, $true)
}
& (Join-Path $GameRoot 'Prepare_GameFolder.ps1') -GameRoot $GameRoot
$user = Join-Path $GameRoot '_appdata_grass_tint_\user.ltx'
Save-Backup $user
$encoding = [Text.Encoding]::Default
$settings = @([IO.File]::ReadAllLines($user, $encoding) | Where-Object { $_ -notmatch '^\s*(renderer\s|r__grass_tint\S*\s)' })
$settings += 'renderer renderer_r4'
$settings += [IO.File]::ReadAllLines((Join-Path $GameRoot '_appdata_grass_tint_\grass_tint_default.ltx'), $encoding)
[IO.File]::WriteAllLines($user, $settings, $encoding)
Write-Host ('Installed Grass Terrain Tint ' + $manifest.version + ' from ' + $manifest.commit)
Write-Host ('Renderer: ' + $renderer)
Write-Host ('Previous files: ' + $backup)
Write-Host 'New color-transfer defaults are active. Start Run_GrassTint.cmd and load a level.'
Write-Host 'Preset: cfg_load grass_tint_default. Stronger: cfg_load grass_tint_strong. Map details: r__grass_tint_info.'
