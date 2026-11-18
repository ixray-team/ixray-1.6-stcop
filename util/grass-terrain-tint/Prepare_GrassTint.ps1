#requires -Version 5.1
[CmdletBinding()]
param(
    [string]$GameRoot = 'D:\ixray\sdk',
    [string]$TestRoot = 'D:\ixray\IXRay-GrassTint-Test',
    [string]$BinaryDir,
    [string]$ShaderDir,
    [string]$FilesystemConfig,
    [switch]$Start
)
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$artifactRoot = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..\..'))
if (!$BinaryDir) { $BinaryDir = Join-Path $artifactRoot 'build\x64\Engine-Windows\bin\Release' }
if (!$ShaderDir) { $ShaderDir = Join-Path $artifactRoot 'gamedata\shaders' }
$GameRoot = (Resolve-Path $GameRoot).Path.TrimEnd('\', '/')
$TestRoot = [IO.Path]::GetFullPath($TestRoot).TrimEnd('\', '/')
if ($GameRoot.Contains('"') -or $TestRoot.Contains('"') -or $GameRoot.Contains('%') -or $TestRoot.Contains('%')) {
    throw 'Use paths without quotes or percent characters for the CMD launcher.'
}
if ($TestRoot -eq $GameRoot -or $GameRoot.StartsWith($TestRoot + '\', [StringComparison]::OrdinalIgnoreCase)) {
    throw 'TestRoot must be separate from GameRoot and its parent directories.'
}
foreach ($required in @('xrEngine.exe', 'xrEngineCore.dll', 'xrRender_R4.dll', 'xrGame.dll')) {
    if (!(Test-Path (Join-Path $BinaryDir $required))) { throw "Missing compiled binary: $required in $BinaryDir" }
}
if (!(Test-Path (Join-Path $ShaderDir 'd3d11\grass_terrain_tint.hlsli'))) { throw 'The matching feature shaders are missing.' }
if (!$FilesystemConfig) {
    foreach ($candidate in @('fsgame.ltx', 'fs.ltx')) {
        $path = Join-Path $GameRoot $candidate
        if (Test-Path $path) { $FilesystemConfig = $path; break }
    }
}
if (!$FilesystemConfig) { throw 'No fsgame.ltx or fs.ltx found. Pass -GameRoot with the game folder.' }
$FilesystemConfig = (Resolve-Path $FilesystemConfig).Path
if ((Split-Path $FilesystemConfig) -ne $GameRoot) { throw 'FilesystemConfig must be in GameRoot.' }
$encoding = [Text.Encoding]::Default
$sourceLines = [IO.File]::ReadAllLines($FilesystemConfig, $encoding)
$aliases = @{}
foreach ($line in $sourceLines) {
    if ($line -match '^\s*(\$[^$]+\$)\s*=\s*([^;]+)') {
        $aliases[$matches[1]] = @($matches[2].Split('|') | ForEach-Object { $_.Trim() })
    }
}
if (!$aliases.ContainsKey('$app_data_root$') -or !$aliases.ContainsKey('$game_shaders$')) {
    throw 'The selected filesystem configuration does not define app data and shaders.'
}
function Resolve-Alias {
    param([string]$Name, [int]$Depth = 0)
    if ($Depth -gt 20) { throw 'Circular filesystem aliases.' }
    if ($Name -eq '$fs_root$') { return $GameRoot }
    if (!$aliases.ContainsKey($Name)) { return $null }
    $parts = $aliases[$Name]
    $root = $parts[2]
    if ($root.StartsWith('$')) { $root = Resolve-Alias $root ($Depth + 1) }
    elseif (![IO.Path]::IsPathRooted($root)) { $root = Join-Path $GameRoot $root }
    if (!$root) { return $null }
    if ($parts.Length -gt 3 -and $parts[3]) { $root = Join-Path $root $parts[3] }
    return [IO.Path]::GetFullPath($root)
}
$originalUserData = Resolve-Alias '$app_data_root$'
$marker = Join-Path $TestRoot '.grass-terrain-tint-runtime.json'
if ((Test-Path $TestRoot) -and !(Test-Path $marker) -and @(Get-ChildItem $TestRoot -Force).Count -gt 0) {
    throw "TestRoot is not an owned grass-tint test directory: $TestRoot"
}
if (Test-Path $marker) {
    $old = Get-Content $marker -Raw | ConvertFrom-Json
    if ($old.GameRoot -ne $GameRoot) { throw 'This TestRoot was prepared for another game. Choose a new TestRoot.' }
}
$bin = Join-Path $TestRoot 'bin'
$shaders = Join-Path $TestRoot 'shaders'
$userData = Join-Path $TestRoot 'userdata'
foreach ($directory in @($TestRoot, $bin, $shaders, $userData)) { [IO.Directory]::CreateDirectory($directory) | Out-Null }
@{ GameRoot = $GameRoot; BinaryDir = $BinaryDir; ShaderDir = $ShaderDir } | ConvertTo-Json | Set-Content $marker -Encoding UTF8
Get-ChildItem $BinaryDir -File | Where-Object { $_.Extension -in @('.exe', '.dll', '.txt') } | Copy-Item -Destination $bin -Force
Copy-Item (Join-Path $ShaderDir '*') $shaders -Recurse -Force

# A new filesystem file must live next to the original: IX-Ray derives $fs_root$ from its location.
$hashAlgorithm = [Security.Cryptography.SHA256]::Create()
try {
    $tag = ([BitConverter]::ToString($hashAlgorithm.ComputeHash([Text.Encoding]::UTF8.GetBytes($TestRoot.ToLowerInvariant())))).Replace('-', '').Substring(0, 8).ToLowerInvariant()
} finally { $hashAlgorithm.Dispose() }
$fsName = 'fs_grass_tint_' + $tag + '.ltx'
$fsPath = Join-Path $GameRoot $fsName
$signature = '; IXRay GrassTerrainTint isolated test v0.1'
if (Test-Path $fsPath) {
    $existing = [IO.File]::ReadAllLines($fsPath)
    if ($existing.Length -eq 0 -or $existing[0] -ne $signature) { throw "Refusing to replace an unrelated file: $fsPath" }
}
$output = @($signature)
foreach ($line in $sourceLines) {
    if ([string]::IsNullOrWhiteSpace($line)) { continue }
    if ($line -match '^\s*\$app_data_root\$\s*=') { $output += '$app_data_root$ = true|false|' + $userData + '\' }
    elseif ($line -match '^\s*\$game_shaders\$\s*=') { $output += '$game_shaders$ = true|false|' + $shaders + '\' }
    else { $output += $line }
}
[IO.File]::WriteAllLines($fsPath, $output, $encoding)

$userConfig = Join-Path $userData 'user.ltx'
if (!(Test-Path $userConfig)) {
    $originalConfig = if ($originalUserData) { Join-Path $originalUserData 'user.ltx' } else { $null }
    $settings = @()
    if ($originalConfig -and (Test-Path $originalConfig)) { $settings = [IO.File]::ReadAllLines($originalConfig, $encoding) }
    $settings = @($settings | Where-Object { $_ -notmatch '^\s*(renderer\s|r__grass_tint\S*\s)' })
    $settings += @('renderer renderer_r4', 'r__grass_tint 1', 'r__grass_tint_mode 1', 'r__grass_tint_strength 0.9',
        'r__grass_tint_brightness 0.35', 'r__grass_tint_saturation 1.1', 'r__grass_tint_contrast 1', 'r__grass_tint_tips 0.9',
        'r__grass_tint_radius 0.25', 'r__grass_tint_height 1.5', 'r__grass_tint_resolution 0', 'r__grass_tint_resolution_scale 1',
        'r__grass_tint_green_only 0', 'r__grass_tint_debug 0')
    [IO.File]::WriteAllLines($userConfig, $settings, $encoding)
}
$launcher = @('@echo off', 'setlocal DisableDelayedExpansion', ('pushd "' + $GameRoot + '" || exit /b 1'),
    ('"' + (Join-Path $bin 'xrEngine.exe') + '" -r4 -fsltx ' + $fsName + ' %*'), 'set "GAME_RESULT=%ERRORLEVEL%"',
    'popd', 'exit /b %GAME_RESULT%')
[IO.File]::WriteAllLines((Join-Path $TestRoot 'Run_GrassTint.cmd'), $launcher, $encoding)
Write-Host "Test ready: $TestRoot\Run_GrassTint.cmd"
Write-Host "Original filesystem config: $FilesystemConfig"
Write-Host "Separate test filesystem config: $fsPath"
Write-Host 'Load a level, then use r__grass_tint off/on; r__grass_tint_debug 2 displays the captured map.'
if ($Start) { Start-Process -FilePath (Join-Path $TestRoot 'Run_GrassTint.cmd') }
