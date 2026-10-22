#requires -Version 5.1
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$prepare = Join-Path $PSScriptRoot '..\..\util\grass-terrain-tint\Prepare_GrassTint.ps1'
$fixture = Join-Path ([IO.Path]::GetTempPath()) ('grass-tint-check-' + [Guid]::NewGuid().ToString('N'))
function Check([bool]$Condition, [string]$Message) {
    if (!$Condition) { throw "FAIL: $Message" }
}
try {
    $game = Join-Path $fixture 'game with spaces'
    $binary = Join-Path $fixture 'compiled'
    $shader = Join-Path $fixture 'shaders'
    $runtime = Join-Path $fixture 'runtime with spaces'
    foreach ($path in @($game, $binary, (Join-Path $shader 'd3d11'), (Join-Path $game 'userdata'))) {
        [IO.Directory]::CreateDirectory($path) | Out-Null
    }
    foreach ($name in @('xrEngine.exe', 'xrEngineCore.dll', 'xrRender_R4.dll', 'xrGame.dll')) {
        [IO.File]::WriteAllText((Join-Path $binary $name), 'Fixture, never execute')
    }
    [IO.File]::WriteAllText((Join-Path $shader 'd3d11\grass_terrain_tint.hlsli'), '// Fixture')
    $fs = Join-Path $game 'fsgame.ltx'
    $originalUser = Join-Path $game 'userdata\user.ltx'
    [IO.File]::WriteAllLines($fs, @(
        '$app_data_root$ = true|false|$fs_root$|userdata',
        '$game_data$ = true|false|$fs_root$|gamedata',
        '$game_shaders$ = true|false|$game_data$|shaders',
        '$logs$ = true|false|$app_data_root$|logs'
    ))
    [IO.File]::WriteAllLines($originalUser, @('renderer renderer_r2', 'r__grass_tint_strength 0.8', 'custom_setting 123'))
    $fsHash = (Get-FileHash $fs).Hash
    $userHash = (Get-FileHash $originalUser).Hash
    $arguments = @{ GameRoot = $game; BinaryDir = $binary; ShaderDir = $shader; TestRoot = $runtime }
    & $prepare @arguments
    $launcher = [IO.File]::ReadAllLines((Join-Path $runtime 'Run_GrassTint.cmd'))
    Check ($launcher.Length -eq 7) 'Launcher must contain seven complete command lines'
    Check ($launcher[2] -eq ('pushd "' + $game + '" || exit /b 1')) 'Game path with spaces must stay quoted on one line'
    Check ($launcher[3].Contains('xrEngine.exe" -r4 -fsltx fs_grass_tint_')) 'Engine command and filesystem name must be on one line'
    $testFs = @(Get-ChildItem $game -Filter 'fs_grass_tint_*.ltx')
    Check ($testFs.Length -eq 1) 'Exactly one separate filesystem configuration must be created'
    $fsContent = [IO.File]::ReadAllText($testFs[0].FullName)
    Check ($fsContent.Contains((Join-Path $runtime 'userdata'))) 'App data must point to the test directory'
    Check ($fsContent.Contains((Join-Path $runtime 'shaders'))) 'Shaders must point to the test directory'
    Check ($fsContent.Contains('$game_data$ = true|false|$fs_root$|gamedata')) 'Game data aliases must be preserved'
    $testUser = Join-Path $runtime 'userdata\user.ltx'
    $settings = [IO.File]::ReadAllText($testUser)
    Check ($settings.Contains('custom_setting 123') -and $settings.Contains('renderer renderer_r4')) 'Existing settings must be copied and renderer selected'
    Check ($settings.Contains('r__grass_tint_strength 0.2') -and !$settings.Contains('r__grass_tint_strength 0.8')) 'Feature defaults must replace stale settings'
    [IO.File]::AppendAllText($testUser, "`ncustom_test_setting 456`n")
    & $prepare @arguments
    Check ([IO.File]::ReadAllText($testUser).Contains('custom_test_setting 456')) 'Repeated setup must preserve test settings'
    Check ((Get-FileHash $fs).Hash -eq $fsHash -and (Get-FileHash $originalUser).Hash -eq $userHash) 'Original config files must remain byte-identical'
    Check ((Get-FileHash (Join-Path $runtime 'bin\xrRender_R4.dll')).Hash -eq (Get-FileHash (Join-Path $binary 'xrRender_R4.dll')).Hash) 'Compiled DLL must be copied exactly'
    $unowned = Join-Path $fixture 'unowned'
    [IO.Directory]::CreateDirectory($unowned) | Out-Null
    [IO.File]::WriteAllText((Join-Path $unowned 'keep.txt'), 'Keep this')
    $rejected = $false
    try { & $prepare -GameRoot $game -BinaryDir $binary -ShaderDir $shader -TestRoot $unowned }
    catch { $rejected = $true }
    Check $rejected 'An existing unrelated test directory must be rejected'
    Check ([IO.File]::ReadAllText((Join-Path $unowned 'keep.txt')) -eq 'Keep this') 'Unrelated files must remain untouched'
    [IO.File]::WriteAllText($testFs[0].FullName, 'Unrelated file')
    $rejected = $false
    try { & $prepare @arguments } catch { $rejected = $true }
    Check $rejected 'An unrelated filesystem configuration must be rejected'
    Check ([IO.File]::ReadAllText($testFs[0].FullName) -eq 'Unrelated file') 'An unrelated filesystem configuration must remain untouched'
    Write-Host 'PASS: runtime setup, paths with spaces, repeat setup, original files, and ownership guards'
} finally {
    if (Test-Path $fixture) { Remove-Item $fixture -Recurse -Force }
}
