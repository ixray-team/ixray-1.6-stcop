#requires -Version 5.1
[CmdletBinding()]
param(
    [string]$GameRoot,
    [string]$SourceRoot = 'D:\ixray\IXRay-GrassTint-v02-src',
    [ValidateRange(1, 32)][int]$Jobs = 4
)
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$SourceCommit = '6a600c1e7a0a8a0107abb6693ec05339582f37c1'
$Repository = 'https://github.com/r3tr0-hub/ixray-1.6-stcop.git'
if ([string]::IsNullOrWhiteSpace($GameRoot)) { $GameRoot = $PSScriptRoot }
if ([string]::IsNullOrWhiteSpace($GameRoot)) { throw 'Pass -GameRoot with the game folder.' }
$GameRoot = (Resolve-Path -LiteralPath $GameRoot).ProviderPath
if (!(Test-Path -LiteralPath (Join-Path $GameRoot 'bin\xrEngine.exe')) -and !(Test-Path -LiteralPath (Join-Path $GameRoot 'bin\Release\xrEngine.exe'))) {
    throw 'Extract this build kit into the game folder, next to Run_GrassTint.cmd.'
}
if ([string]::IsNullOrWhiteSpace($SourceRoot)) { throw 'SourceRoot must be a separate source-code folder.' }
$SourceRoot = [IO.Path]::GetFullPath($SourceRoot)
if ($SourceRoot.TrimEnd('\') -eq $GameRoot.TrimEnd('\')) { throw 'SourceRoot must be separate from the game folder.' }
$gitCommand = Get-Command git.exe -ErrorAction SilentlyContinue
if (!$gitCommand) { throw 'Git for Windows was not found. Install Git and reopen this build script.' }
$git = $gitCommand.Source
function Invoke-Checked([string]$Program, [string[]]$Arguments) {
    & $Program @Arguments
    if ($LASTEXITCODE -ne 0) { throw "$Program failed (exit $LASTEXITCODE)." }
}
if (!(Test-Path -LiteralPath $SourceRoot)) {
    $env:GIT_LFS_SKIP_SMUDGE = '1'
    Invoke-Checked $git @('clone', '--config', 'core.autocrlf=false', '--filter=blob:none', '--single-branch', '--branch', 'feature/grass-terrain-tint', '--no-checkout', $Repository, $SourceRoot)
    Invoke-Checked $git @('-C', $SourceRoot, '-c', 'core.autocrlf=false', 'checkout', '--detach', $SourceCommit)
}
if (!(Test-Path -LiteralPath (Join-Path $SourceRoot '.git'))) { throw "SourceRoot is not the dedicated source checkout: $SourceRoot" }
$head = (& $git -C $SourceRoot rev-parse HEAD).Trim()
if ($LASTEXITCODE -ne 0) { throw 'Cannot read source checkout HEAD.' }
$changes = @(& $git -C $SourceRoot status --porcelain)
if ($LASTEXITCODE -ne 0 -or $changes.Count -gt 0) { throw 'SourceRoot contains local changes. Keep your changes and use a separate clean SourceRoot.' }
if ($head -ne $SourceCommit) {
    if ($head -ne '4d005d67968089e4c8148cf953c39d893ca59baf' -and $head -ne '1b18555e5fc8d9116b262bbb1402dfed10018eb2' -and $head -ne '3da2cb5ca7fbf7c08afa30a29bc4876f1525204f' -and $head -ne 'a5960a9e78b1119fbc33c947d985e75233796750') {
        throw 'This is not the clean v0.2 test checkout. Pass a separate -SourceRoot.'
    }
    Invoke-Checked $git @('-C', $SourceRoot, 'fetch', $Repository, 'feature/grass-terrain-tint')
    Invoke-Checked $git @('-C', $SourceRoot, '-c', 'core.autocrlf=false', 'checkout', '--detach', $SourceCommit)
}
$utility = Join-Path $SourceRoot 'util\grass-terrain-tint'
Write-Host ('Building Grass Terrain Tint v0.2 + atlas alpha mask from ' + $SourceCommit)
Write-Host ('Source: ' + $SourceRoot)
& (Join-Path $utility 'Build_GrassTint.ps1') -BuildOnly -Jobs $Jobs
if (!$?) { throw 'The engine build or shader tests failed. The installed game has not been updated.' }
$binaryDir = Join-Path $SourceRoot 'build\x64\Engine-Windows\bin\Release'
foreach ($name in @('xrEngine.exe', 'xrEngineCore.dll', 'xrRender_R4.dll', 'xrGame.dll')) {
    if (!(Test-Path -LiteralPath (Join-Path $binaryDir $name))) { throw "Build output is incomplete: $name" }
}
$rendererBytes = [IO.File]::ReadAllBytes((Join-Path $binaryDir 'xrRender_R4.dll'))
if ($rendererBytes.Length -lt 256 -or $rendererBytes[0] -ne 77 -or $rendererBytes[1] -ne 90 -or ![Text.Encoding]::ASCII.GetString($rendererBytes).Contains('[v0.2, terrain color transfer]') -or ![Text.Encoding]::ASCII.GetString($rendererBytes).Contains('Grass terrain tint: atlas mask')) {
    throw 'The built renderer does not contain the expected v0.2 implementation.'
}
# Assemble a verified update from this build; no Python installation is required.
$stage = Join-Path $SourceRoot 'build\grass-tint-ready-update-mask'
$payload = Join-Path $stage '_grass_tint_update_'
$files = @()
function Stage-File([string]$Source, [string]$Relative) {
    $destination = Join-Path $payload $Relative
    [IO.Directory]::CreateDirectory([IO.Path]::GetDirectoryName($destination)) | Out-Null
    [IO.File]::Copy($Source, $destination, $true)
    $script:files += @{ path = $Relative; sha256 = (Get-FileHash -LiteralPath $destination -Algorithm SHA256).Hash }
}
foreach ($path in Get-ChildItem -LiteralPath $binaryDir -File) {
    if ($path.Extension -in @('.exe', '.dll', '.txt')) { Stage-File $path.FullName ('bin/' + $path.Name) }
}
foreach ($name in @('common_iostructs.hlsli', 'deffer_base.ps.hlsl', 'deffer_detail.vs.hlsl', 'grass_terrain_tint.hlsli',
    'grass_terrain_tint_color.hlsli', 'grass_terrain_tint_capture.vs.hlsl', 'grass_terrain_tint_capture.ps.hlsl', 'grass_terrain_tint_debug.ps.hlsl')) {
    Stage-File (Join-Path (Join-Path $SourceRoot 'gamedata\shaders\d3d11') $name) ('gamedata/shaders/d3d11/' + $name)
}
foreach ($path in Get-ChildItem -LiteralPath $utility -Filter 'grass_tint_*.ltx') { Stage-File $path.FullName ('presets/' + $path.Name) }
foreach ($name in @('Run_GrassTint.cmd', 'Prepare_GameFolder.ps1')) { Stage-File (Join-Path $utility $name) $name }
@{ version = '0.2-alpha-mask'; commit = $SourceCommit; files = $files } | ConvertTo-Json -Depth 4 | Set-Content -LiteralPath (Join-Path $payload 'manifest.json') -Encoding UTF8
[IO.File]::Copy((Join-Path $utility 'Install_GrassTint_Update.ps1'), (Join-Path $stage 'Install_GrassTint_Update.ps1'), $true)
& (Join-Path $stage 'Install_GrassTint_Update.ps1') -GameRoot $GameRoot
if (!$?) { throw 'Compilation finished, but installation did not complete. See the error above.' }
Write-Host ('Ready: ' + (Join-Path $GameRoot 'Run_GrassTint.cmd'))
Write-Host 'Load a level. Default: cfg_load grass_tint_default. Stronger: cfg_load grass_tint_strong.'
