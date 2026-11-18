#requires -Version 5.1
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$utility = Join-Path $PSScriptRoot '..\..\util\grass-terrain-tint'
$fixture = Join-Path ([IO.Path]::GetTempPath()) ('grass-tint-install-' + [Guid]::NewGuid().ToString('N'))
function Check([bool]$Condition, [string]$Message) { if (!$Condition) { throw "FAIL: $Message" } }
try {
    foreach ($layout in @('bin', 'bin\Release')) {
        $game = Join-Path $fixture ($layout.Replace('\', '-') + ' game [test] with spaces')
        $binary = Join-Path $game $layout
        $payload = Join-Path $game '_grass_tint_update_'
        foreach ($path in @($binary, (Join-Path $payload 'bin'), (Join-Path $payload 'presets'), (Join-Path $payload 'gamedata\shaders\d3d11'), (Join-Path $game '_appdata_grass_tint_'))) {
            [IO.Directory]::CreateDirectory($path) | Out-Null
        }
        [IO.File]::WriteAllText((Join-Path $binary 'xrEngine.exe'), 'Fixture, never execute')
        [IO.File]::WriteAllText((Join-Path $binary 'xrRender_R4.dll'), 'Old renderer fixture')
        [IO.File]::WriteAllText((Join-Path $payload 'bin\xrRender_R4.dll'), 'New renderer fixture')
        [IO.File]::WriteAllText((Join-Path $payload 'gamedata\shaders\d3d11\grass_terrain_tint.hlsli'), '// New fixture shader')
        [IO.File]::WriteAllLines((Join-Path $game 'fsgame.ltx'), @(
            '$app_data_root$ = true|false|$fs_root$|userdata',
            '$game_data$ = true|false|$fs_root$|gamedata',
            '$game_config$ = true|false|$game_data$|configs'
        ))
        $user = Join-Path $game '_appdata_grass_tint_\user.ltx'
        [IO.File]::WriteAllLines($user, @('renderer renderer_r2', 'r__grass_tint_strength 0.2', 'vid_mode 1920x1080', 'custom_setting 123'))
        $userHash = (Get-FileHash -LiteralPath $user).Hash
        foreach ($path in Get-ChildItem -LiteralPath $utility -Filter 'grass_tint_*.ltx') {
            [IO.File]::Copy($path.FullName, (Join-Path (Join-Path $payload 'presets') $path.Name))
        }
        foreach ($name in @('Run_GrassTint.cmd', 'Prepare_GameFolder.ps1')) { [IO.File]::Copy((Join-Path $utility $name), (Join-Path $payload $name)) }
        [IO.File]::Copy((Join-Path $utility 'Install_GrassTint_Update.ps1'), (Join-Path $game 'Install_GrassTint_Update.ps1'))
        $entries = @('bin/xrRender_R4.dll', 'gamedata/shaders/d3d11/grass_terrain_tint.hlsli', 'Run_GrassTint.cmd', 'Prepare_GameFolder.ps1')
        $entries += @(Get-ChildItem -LiteralPath (Join-Path $payload 'presets') | ForEach-Object { 'presets/' + $_.Name })
        $files = @($entries | ForEach-Object { @{ path = $_; sha256 = (Get-FileHash -LiteralPath (Join-Path $payload $_) -Algorithm SHA256).Hash } })
        @{ version = '0.2-test'; commit = 'fixture'; files = $files } | ConvertTo-Json -Depth 4 | Set-Content -LiteralPath (Join-Path $payload 'manifest.json') -Encoding UTF8
        $installer = Join-Path $game 'Install_GrassTint_Update.ps1'
        # Invalid bytes must be rejected before the first game file changes.
        [IO.File]::AppendAllText((Join-Path $payload 'bin\xrRender_R4.dll'), 'corruption')
        $rejected = $false
        try { & $installer -GameRoot $game } catch { $rejected = $_.Exception.Message.Contains('damaged update') }
        Check $rejected 'Corrupted archives must produce a clear error'
        Check ([IO.File]::ReadAllText((Join-Path $binary 'xrRender_R4.dll')) -eq 'Old renderer fixture') 'Corruption must leave the installed renderer unchanged'
        [IO.File]::WriteAllText((Join-Path $payload 'bin\xrRender_R4.dll'), 'New renderer fixture')
        & $installer -GameRoot $game
        Check ([IO.File]::ReadAllText((Join-Path $binary 'xrRender_R4.dll')) -eq 'New renderer fixture') 'Installer must use the actual engine directory'
        $backups = @(Get-ChildItem -LiteralPath $game -Directory -Filter '_grass_tint_backup*')
        Check ($backups.Count -eq 1) 'An update must retain previous files in one backup'
        Check ((Get-FileHash -LiteralPath (Join-Path $backups[0].FullName '_appdata_grass_tint_\user.ltx')).Hash -eq $userHash) 'Settings backup must match the original bytes'
        Check ([IO.File]::ReadAllText((Join-Path $backups[0].FullName ($layout + '\xrRender_R4.dll'))) -eq 'Old renderer fixture') 'Renderer backup must preserve the previous binary'
        $settings = [IO.File]::ReadAllText($user)
        Check ($settings.Contains('vid_mode 1920x1080') -and $settings.Contains('custom_setting 123')) 'Updating tint defaults must preserve video and game settings'
        Check ($settings.Contains('r__grass_tint_strength 0.9') -and $settings.Contains('r__grass_tint_resolution 0')) 'Updated defaults must replace the weak old profile values'
        Check ([IO.File]::ReadAllText((Join-Path $game '_appdata_grass_tint_\grass_tint_strong.ltx')).Contains('r__grass_tint_strength 1')) 'Strong preset must be installed in the cfg_load profile'
    }
    Write-Host 'PASS: update installation for bin and bin\Release, SHA validation, binary/settings backups, settings migration and presets'
} finally { if (Test-Path -LiteralPath $fixture) { Remove-Item -LiteralPath $fixture -Recurse -Force } }
