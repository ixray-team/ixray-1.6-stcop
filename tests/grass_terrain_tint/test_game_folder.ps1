#requires -Version 5.1
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$prepare = Join-Path $PSScriptRoot '..\..\util\grass-terrain-tint\Prepare_GameFolder.ps1'
$fixture = Join-Path ([IO.Path]::GetTempPath()) ('grass-tint-game-folder-' + [Guid]::NewGuid().ToString('N'))
function Check([bool]$Condition, [string]$Message) {
    if (!$Condition) { throw "FAIL: $Message" }
}
try {
    $game = Join-Path $fixture 'game with spaces'
    foreach ($path in @('bin', 'gamedata\shaders\d3d11', 'userdata')) {
        [IO.Directory]::CreateDirectory((Join-Path $game $path)) | Out-Null
    }
    [IO.File]::WriteAllText((Join-Path $game 'bin\xrEngine.exe'), 'Fixture, never execute')
    [IO.File]::WriteAllText((Join-Path $game 'bin\xrRender_R4.dll'), 'Fixture renderer')
    [IO.File]::WriteAllText((Join-Path $game 'gamedata\shaders\d3d11\grass_terrain_tint.hlsli'), '// Fixture')
    $fs = Join-Path $game 'fsgame.ltx'
    [IO.File]::WriteAllLines($fs, @(
        '$app_data_root$ = true|false|$fs_root$|userdata',
        '$arch_dir$ = true|false|$fs_root$|custom_database',
        '$game_data$ = true|false|$fs_root$|gamedata',
        '$game_config$ = true|false|$game_data$|configs',
        '$game_fonts$ = true|false|$game_data$|custom_fonts',
        '$logs$ = true|false|$app_data_root$|logs'
    ))
    $originalHash = (Get-FileHash $fs).Hash
    $originalUser = Join-Path $game 'userdata\user.ltx'
    [IO.File]::WriteAllLines($originalUser, @('renderer renderer_r2', 'r__grass_tint_strength 0.8', 'custom_setting 123'))
    $originalUserHash = (Get-FileHash $originalUser).Hash
    & $prepare -GameRoot $game
    $backup = $fs + '.before-grass-tint-fix.bak'
    Check ((Get-FileHash $backup).Hash -eq $originalHash) 'Backup must match the original filesystem bytes'
    $fixed = [IO.File]::ReadAllText($fs)
    Check ($fixed.Contains('$game_particles$ = true|false|$game_data$|particles\')) 'Missing particle path must be added'
    Check ($fixed.Contains('$game_fonts$ = true|false|$game_data$|custom_fonts')) 'Custom aliases must remain unchanged'
    $testFs = Join-Path $game 'fsgame_grass_tint.ltx'
    $testContent = [IO.File]::ReadAllText($testFs)
    Check ($testContent.Contains('custom_database') -and $testContent.Contains('custom_fonts')) 'Test startup must keep custom archive and asset paths'
    Check ($testContent.Contains('$app_data_root$ = true|false|$fs_root$|_appdata_grass_tint_\')) 'Test profile must use the explicit root alias'
    $seen = @{}
    foreach ($line in [IO.File]::ReadAllLines($testFs)) {
        Check (![string]::IsNullOrWhiteSpace($line)) 'Engine filesystem parser must not receive empty lines'
        if ($line.StartsWith(';')) { continue }
        $pieces = $line.Split('=')
        $alias = $pieces[0].Trim()
        $parts = $pieces[1].Split('|')
        Check ($parts.Count -ge 3) 'Each alias must contain the required parser fields'
        $root = $parts[2].Trim()
        Check (!$seen.ContainsKey($alias)) 'No duplicate filesystem aliases'
        Check (!$root.StartsWith('$') -or $root -eq '$fs_root$' -or $seen.ContainsKey($root)) 'Alias parents must be defined before use'
        $seen[$alias] = $true
    }
    $user = Join-Path $game '_appdata_grass_tint_\user.ltx'
    $settings = [IO.File]::ReadAllText($user)
    Check ($settings.Contains('custom_setting 123') -and $settings.Contains('renderer renderer_r4')) 'Existing settings must be copied with R4 selected'
    Check ($settings.Contains('r__grass_tint 1') -and $settings.Contains('r__grass_tint_green_only 0')) 'Boolean settings must use tokens accepted by CCC_Boolean'
    Check ($settings.Contains('r__grass_tint_strength 0.9') -and !$settings.Contains('r__grass_tint_strength 0.8')) 'New test must use the feature defaults'
    Check ($settings.Contains('r__grass_tint_mode 1') -and $settings.Contains('r__grass_tint_resolution 0')) 'New profiles must use color transfer and source-sized auto resolution'
    [IO.File]::AppendAllText($user, "`ncustom_test_setting 456`n")
    $fixedHash = (Get-FileHash $fs).Hash
    & $prepare -GameRoot $game
    Check ((Get-FileHash $fs).Hash -eq $fixedHash) 'Repeated setup must not duplicate paths or rewrite the base config'
    Check ((Get-FileHash $backup).Hash -eq $originalHash) 'Repeated setup must preserve the original backup'
    Check ([IO.File]::ReadAllText($user).Contains('custom_test_setting 456')) 'Repeated setup must keep test settings'
    Check ((Get-FileHash $originalUser).Hash -eq $originalUserHash) 'Original settings must remain byte-identical'
    Remove-Item $user
    Remove-Item $originalUser
    & $prepare -GameRoot $game
    Check ([IO.File]::ReadAllText($user).Contains('renderer renderer_r4')) 'First install without user.ltx must create a valid test profile'
    # Reproduce a downloaded Actions artifact with bin\Release and no explicit root.
    $releaseGame = Join-Path $fixture 'game [release] with spaces'
    foreach ($path in @('bin\Release', 'gamedata\shaders\d3d11')) {
        [IO.Directory]::CreateDirectory((Join-Path $releaseGame $path)) | Out-Null
    }
    [IO.File]::WriteAllText((Join-Path $releaseGame 'bin\Release\xrEngine.exe'), 'Fixture, never execute')
    [IO.File]::WriteAllText((Join-Path $releaseGame 'bin\Release\xrRender_R4.dll'), 'Fixture renderer')
    [IO.File]::WriteAllText((Join-Path $releaseGame 'gamedata\shaders\d3d11\grass_terrain_tint.hlsli'), '// Fixture')
    $releaseFs = Join-Path $releaseGame 'fsgame.ltx'
    [IO.File]::WriteAllLines($releaseFs, @(
        '$app_data_root$ = true|false||',
        '$game_data$ = true|false|$fs_root$|gamedata',
        '$game_config$ = true|false|$game_data$|configs'
    ))
    $installedPrepare = Join-Path $releaseGame 'Prepare_GameFolder.ps1'
    [IO.File]::Copy($prepare, $installedPrepare)
    Push-Location ([IO.Path]::GetTempPath())
    try {
        & $installedPrepare
        $releaseUser = Join-Path $releaseGame '_appdata_grass_tint_\user.ltx'
        Check ([IO.File]::ReadAllText($releaseUser).Contains('renderer renderer_r4')) 'Default root and bin\Release must work from another working directory'
        Check (!(Test-Path -LiteralPath (Join-Path $releaseGame 'bin\xrEngine.exe'))) 'Setup must keep the compiled files in their existing directory'
        & $installedPrepare -GameRoot ''
        Check ([IO.File]::ReadAllText($releaseUser).Contains('r__grass_tint 1')) 'An explicitly empty root must safely use the script directory'
    } finally { Pop-Location }
    # Empty roots reached through another alias must also avoid Join-Path errors.
    [IO.File]::WriteAllLines($releaseFs, @(
        '$empty_profile$ = true|false||',
        '$app_data_root$ = true|false|$empty_profile$|userdata',
        '$game_data$ = true|false|$fs_root$|gamedata',
        '$game_config$ = true|false|$game_data$|configs'
    ))
    & $installedPrepare -GameRoot $releaseGame
    Check ([IO.File]::ReadAllText($releaseUser).Contains('renderer renderer_r4')) 'An empty original profile must not block test startup'
    if ($env:OS -eq 'Windows_NT') {
        # Run the real CMD launcher with a harmless CMD stub instead of a game binary.
        $run = Join-Path $PSScriptRoot '..\..\util\grass-terrain-tint\Run_GrassTint.cmd'
        $commandText = [IO.File]::ReadAllText($run).Replace('xrEngine.exe', 'xrEngine.cmd')
        [IO.File]::WriteAllText((Join-Path $releaseGame 'Run_GrassTint.cmd'), $commandText)
        [IO.File]::WriteAllLines((Join-Path $releaseGame 'bin\Release\xrEngine.cmd'), @(
            '@echo off',
            '>engine-args.txt echo %*',
            'exit /b 0'
        ))
        $cmdArguments = '/d /c ""' + (Join-Path $releaseGame 'Run_GrassTint.cmd') + '" -launcher_probe"'
        $process = Start-Process -FilePath $env:ComSpec -ArgumentList $cmdArguments -WorkingDirectory ([IO.Path]::GetTempPath()) -Wait -PassThru
        Check ($process.ExitCode -eq 0) 'CMD launcher must complete successfully with bin\Release'
        $engineArgs = [IO.File]::ReadAllText((Join-Path $releaseGame 'engine-args.txt'))
        Check ($engineArgs.Trim() -eq '-r4 -fsltx fsgame_grass_tint.ltx -launcher_probe') 'CMD must use the game root and preserve engine arguments'
    }
    Write-Host 'PASS: legacy repair, backups, repeat setup, default/empty roots, empty profile aliases, bin\Release and CMD startup'
} finally {
    if (Test-Path $fixture) { Remove-Item $fixture -Recurse -Force }
}
