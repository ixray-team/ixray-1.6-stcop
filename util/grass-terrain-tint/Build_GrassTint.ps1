#requires -Version 5.1
[CmdletBinding()]
param(
    [string]$GameRoot = 'D:\ixray\sdk',
    [string]$SourceRoot = 'D:\ixray\IXRay-GrassTint-src',
    [string]$TestRoot = 'D:\ixray\IXRay-GrassTint-Test',
    [string]$FilesystemConfig,
    [ValidateRange(1, 32)][int]$Jobs = 4,
    [switch]$BuildOnly
)
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

function Invoke-Checked {
    param([string]$Program, [string[]]$Arguments)
    & $Program @Arguments
    if ($LASTEXITCODE -ne 0) { throw "$Program failed (exit $LASTEXITCODE)." }
}

function Find-CMake {
    $command = Get-Command cmake.exe -ErrorAction SilentlyContinue
    if ($command) { return $command.Source }
    $vswhere = Join-Path ${env:ProgramFiles(x86)} 'Microsoft Visual Studio\Installer\vswhere.exe'
    if (Test-Path $vswhere) {
        $installation = & $vswhere -latest -products '*' -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath
        if ($installation) {
            $candidate = Join-Path $installation 'Common7\IDE\CommonExtensions\Microsoft\CMake\CMake\bin\cmake.exe'
            if (Test-Path $candidate) { return $candidate }
        }
    }
    throw 'CMake 3.26+ was not found. Install the C++ CMake tools in Visual Studio 2022.'
}

$cmake = Find-CMake
$patch = Join-Path $PSScriptRoot 'grass-terrain-tint.patch'
$repositoryRoot = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..\..'))
if (!(Test-Path $patch) -and (Test-Path (Join-Path $repositoryRoot 'CMakePresets.json'))) {
    # Running directly from the feature branch: no patching is necessary.
    $SourceRoot = $repositoryRoot
} else {
    $git = (Get-Command git.exe -ErrorAction Stop).Source
    $manifest = Get-Content (Join-Path $PSScriptRoot 'manifest.json') -Raw | ConvertFrom-Json
    if ((Get-FileHash $patch -Algorithm SHA256).Hash.ToLowerInvariant() -ne $manifest.patch_sha256) {
        throw 'The patch checksum does not match manifest.json.'
    }
    if (!(Test-Path $SourceRoot)) {
        $env:GIT_LFS_SKIP_SMUDGE = '1'
        Invoke-Checked $git @('clone', '--config', 'core.autocrlf=false', '--filter=blob:none', '--no-checkout', $manifest.repository, $SourceRoot)
        Invoke-Checked $git @('-C', $SourceRoot, '-c', 'core.autocrlf=false', 'checkout', '--detach', $manifest.baseline)
    }
    if (!(Test-Path (Join-Path $SourceRoot '.git'))) { throw "SourceRoot is not a Git checkout: $SourceRoot" }
    $head = (& $git -C $SourceRoot rev-parse HEAD).Trim()
    if ($LASTEXITCODE -ne 0 -or $head -ne $manifest.baseline) {
        throw "SourceRoot must be at the pinned baseline $($manifest.baseline). Use a new separate directory."
    }
    # Compare exact bytes before invoking git apply; never patch modified originals.
    $original = $true
    $patched = $true
    foreach ($entry in $manifest.files) {
        $path = Join-Path $SourceRoot $entry.path
        $hash = $null
        if (Test-Path $path) { $hash = (Get-FileHash $path -Algorithm SHA256).Hash.ToLowerInvariant() }
        if ($hash -ne $entry.original_sha256) { $original = $false }
        if ($hash -ne $entry.patched_sha256) { $patched = $false }
    }
    if ($original) {
        Invoke-Checked $git @('-C', $SourceRoot, '-c', 'core.autocrlf=false', 'apply', '--check', '--whitespace=nowarn', $patch)
        Invoke-Checked $git @('-C', $SourceRoot, '-c', 'core.autocrlf=false', 'apply', '--whitespace=nowarn', $patch)
    } elseif (!$patched) {
        throw 'Source files differ from both the baseline and this package. Use a new separate SourceRoot.'
    }
}

Push-Location $SourceRoot
try {
    Invoke-Checked $cmake @('--preset', 'Engine-x64-Windows', '-DIXRAY_USE_R1=OFF', '-DIXRAY_USE_R2=OFF', '-DIXRAY_PROFILER=OFF')
    Invoke-Checked $cmake @('--build', '--preset', 'Engine-x64-Windows-Release', '--target', 'xrEngine', 'xrGame', 'xrRender_R4', '--parallel', "$Jobs")
    Invoke-Checked $cmake @('-S', 'tests/grass_terrain_tint', '-B', 'build/grass-tint-checks', '-A', 'x64')
    Invoke-Checked $cmake @('--build', 'build/grass-tint-checks', '--config', 'Release')
    $ctest = Join-Path (Split-Path $cmake) 'ctest.exe'
    Invoke-Checked $ctest @('--test-dir', 'build/grass-tint-checks', '-C', 'Release', '--output-on-failure')
} finally { Pop-Location }

$binaryDir = Join-Path $SourceRoot 'build\x64\Engine-Windows\bin\Release'
if ($BuildOnly) {
    Write-Host "Build completed: $binaryDir"
    return
}
$prepareArguments = @{
    GameRoot = $GameRoot
    BinaryDir = $binaryDir
    ShaderDir = (Join-Path $SourceRoot 'gamedata\shaders')
    TestRoot = $TestRoot
}
if ($FilesystemConfig) { $prepareArguments.FilesystemConfig = $FilesystemConfig }
& (Join-Path $PSScriptRoot 'Prepare_GrassTint.ps1') @prepareArguments
Write-Host "Ready. Run: $TestRoot\Run_GrassTint.cmd"
