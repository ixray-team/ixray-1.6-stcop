#requires -Version 5.1
[CmdletBinding()]
param([string]$GameRoot = $PSScriptRoot)
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$GameRoot = (Resolve-Path $GameRoot).Path
foreach ($name in @('bin\xrEngine.exe', 'gamedata\shaders\d3d11\grass_terrain_tint.hlsli')) {
    if (!(Test-Path (Join-Path $GameRoot $name) -PathType Leaf)) { throw "Extract the complete bin/gamedata package first: missing $name" }
}
$source = $null
foreach ($name in @('fsgame.ltx', 'fs.ltx', 'fsgame_grass_tint.ltx')) {
    $candidate = Join-Path $GameRoot $name
    if (Test-Path $candidate -PathType Leaf) { $source = $candidate; break }
}
if (!$source) { throw 'No filesystem configuration found in the game folder.' }
$encoding = [Text.Encoding]::Default
$lines = @([IO.File]::ReadAllLines($source, $encoding))
$aliases = @{}
foreach ($line in $lines) {
    if ($line -match '^\s*(\$[^$]+\$)\s*=\s*([^;]+)') {
        if ($aliases.ContainsKey($matches[1])) { throw "Duplicate filesystem alias: $($matches[1])" }
        $aliases[$matches[1]] = @($matches[2].Split('|') | ForEach-Object { $_.Trim() })
    }
}
foreach ($name in @('$app_data_root$', '$game_data$', '$game_config$')) {
    if (!$aliases.ContainsKey($name)) { throw "Missing base filesystem alias: $name" }
}
# These aliases are required by this engine branch, including before the main menu.
$required = @(
    '$game_fonts$ = true|false|$game_data$|fonts\',
    '$game_particles$ = true|false|$game_data$|particles\',
    '$game_weathers$ = true|false|$game_config$|environment\weathers',
    '$game_weather_effects$ = true|false|$game_config$|environment\weather_effects',
    '$mod_dir$ = false|false|$fs_root$|mods\',
    '$arch_dir_addons$ = true|true|$fs_root$|ixr_addons\'
)
$added = @()
foreach ($line in $required) {
    $name = $line.Split('=')[0].Trim()
    if (!$aliases.ContainsKey($name)) { $lines += $line; $added += $name }
}
$testFs = Join-Path $GameRoot 'fsgame_grass_tint.ltx'
if ($source -ne $testFs -and $added.Count -gt 0) {
    $backup = $source + '.before-grass-tint-fix.bak'
    if (!(Test-Path $backup)) { [IO.File]::Copy($source, $backup, $false) }
    [IO.File]::WriteAllLines($source, $lines, $encoding)
    Write-Host ('Added filesystem paths: ' + ($added -join ', '))
    Write-Host "Original configuration backup: $backup"
}
# Resolve the original profile before replacing its alias in the test config.
function Resolve-GameAlias([string]$Name, [int]$Depth = 0) {
    if ($Depth -gt 20) { throw 'Circular filesystem aliases.' }
    if ($Name -eq '$fs_root$') { return $GameRoot }
    if (!$aliases.ContainsKey($Name)) { return $null }
    $parts = $aliases[$Name]
    $root = $parts[2]
    if ($root.StartsWith('$')) { $root = Resolve-GameAlias $root ($Depth + 1) }
    elseif (![IO.Path]::IsPathRooted($root)) { $root = Join-Path $GameRoot $root }
    if (!$root) { return $null }
    if ($parts.Length -gt 3 -and $parts[3]) { $root = Join-Path $root $parts[3] }
    return [IO.Path]::GetFullPath($root)
}
$originalProfile = Resolve-GameAlias '$app_data_root$'
$output = @('; IXRay GrassTerrainTint game-folder startup configuration')
foreach ($line in $lines) {
    if ([string]::IsNullOrWhiteSpace($line)) { continue }
    if ($line -match '^\s*\$app_data_root\$\s*=') {
        $output += '$app_data_root$ = true|false|$fs_root$|_appdata_grass_tint_\'
    } else { $output += $line }
}
[IO.File]::WriteAllLines($testFs, $output, $encoding)
$profile = Join-Path $GameRoot '_appdata_grass_tint_'
[IO.Directory]::CreateDirectory($profile) | Out-Null
$user = Join-Path $profile 'user.ltx'
if (!(Test-Path $user)) {
    $settings = @()
    if ($originalProfile -and (Test-Path (Join-Path $originalProfile 'user.ltx'))) {
        $settings = @([IO.File]::ReadAllLines((Join-Path $originalProfile 'user.ltx'), $encoding))
    }
    $settings = @($settings | Where-Object { $_ -notmatch '^\s*(renderer\s|r__grass_tint\S*\s)' })
    $settings += @('renderer renderer_r4', 'r__grass_tint on', 'r__grass_tint_strength 0.2',
        'r__grass_tint_radius 2', 'r__grass_tint_height 1.5', 'r__grass_tint_resolution 1024',
        'r__grass_tint_green_only on', 'r__grass_tint_debug 0')
    [IO.File]::WriteAllLines($user, $settings, $encoding)
}
Write-Host "Startup ready. Logs: $profile\logs"
