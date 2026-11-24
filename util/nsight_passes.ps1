param(
    [string]$Save,
    [string]$Game = $env:IXRAY_GAME_DIR,
    [string]$Exe = $env:IXRAY_ENGINE_EXE,
    [string]$Ngfx = $env:NSIGHT_NGFX,
    [string]$Passes = "SceneVisibility,DEFER_PART0_SPLIT,DEFER_SUN,DEFER_LIGHT_COMBINE",
    [string]$Commands = "",
    [string]$Architecture = "",
    [int]$StartMs = 75000,
    [int]$Runs = 3,
    [switch]$All
)
# Headless Nsight GPU Trace of one frame of a saved game, repeated. The table keeps each pass's fastest
# run, since other GPU work on the machine sometimes lands inside a trace.
#   -Save      name of the saved game to load (required)
#   -Game      game folder, the one with fsgame.ltx; or the IXRAY_GAME_DIR variable
#   -Exe       xrEngine.exe; or IXRAY_ENGINE_EXE; else the newest one under the repository's build folder
#   -Ngfx      ngfx.exe of Nsight Graphics; or NSIGHT_NGFX; else PATH, else the newest installed one
#   -Passes    GPU event names, or paths such as DEFER_SUN/RenderSunCascade, joined by commas
#   -Commands  console commands run before the save is loaded, each ending in ';'
#   -Architecture  Nsight's name of the GPU architecture; guessed from the adapter when empty
#   -StartMs   when the trace starts; has to be after the level has loaded
$ErrorActionPreference = "Stop"

if (-not $Save)
{
    throw "Name the saved game to load: -Save <name>"
}

if (-not $Game -or -not (Test-Path (Join-Path $Game "fsgame.ltx")))
{
    throw "Give the game folder (the one with fsgame.ltx): -Game <path> or the IXRAY_GAME_DIR variable"
}

if (-not $Exe)
{
    $Found = Get-ChildItem (Join-Path $PSScriptRoot "..\build") -Recurse -Filter xrEngine.exe -ErrorAction SilentlyContinue | Sort-Object LastWriteTime -Descending | Select-Object -First 1
    $Exe = if ($Found) { $Found.FullName } else { "" }
}

if (-not $Exe -or -not (Test-Path $Exe))
{
    throw "Give the engine: -Exe <path to xrEngine.exe> or the IXRAY_ENGINE_EXE variable"
}

if (-not $Ngfx)
{
    $OnPath = Get-Command ngfx.exe -ErrorAction SilentlyContinue
    $Installed = Get-ChildItem "$env:ProgramFiles\NVIDIA Corporation\Nsight Graphics *\host\windows-desktop-nomad-x64\ngfx.exe" -ErrorAction SilentlyContinue | Sort-Object LastWriteTime -Descending | Select-Object -First 1
    $Ngfx = if ($OnPath) { $OnPath.Source } elseif ($Installed) { $Installed.FullName } else { "" }
}

if (-not $Ngfx -or -not (Test-Path $Ngfx))
{
    throw "Nsight Graphics was not found: -Ngfx <path to ngfx.exe> or the NSIGHT_NGFX variable"
}

if (-not (Get-Command python -ErrorAction SilentlyContinue))
{
    throw "python is not on the PATH"
}

if (-not $Architecture)
{
    $Adapter = (Get-CimInstance Win32_VideoController | Where-Object { $_.Name -match "NVIDIA" } | Select-Object -First 1).Name

    $Known = [ordered]@{ "RTX 50" = "Blackwell GB20x"; "RTX 40" = "Ada"; "RTX 30" = "Ampere GA10x"; "RTX 20|GTX 16" = "Turing" }

    foreach ($Series in $Known.Keys)
    {
        if (-not $Architecture -and $Adapter -match $Series)
        {
            $Architecture = $Known[$Series]
        }
    }
}

$Process = [IO.Path]::GetFileNameWithoutExtension($Exe)
$Arguments = "-autotest -autotest_frames 1000000 -autotest_warmup 300 -autotest_timeout 600 -autotest_cmd $Commands start server($Save/single/alife/load) client(localhost)"
$Extra = @()

# Without a known architecture Nsight takes its own default metrics; the table then has times only
if ($Architecture)
{
    $Extra = @("--architecture", $Architecture, "--metric-set-id", "0", "--real-time-shader-profiler")
}

$Dirs = @()

foreach ($Run in 1..$Runs)
{
    $Out = Join-Path $env:TEMP "ixray_ngfx$Run"
    Remove-Item $Out -Recurse -Force -ErrorAction SilentlyContinue
    New-Item -ItemType Directory $Out | Out-Null
    Get-Process $Process -ErrorAction SilentlyContinue | Stop-Process -Force
    & $Ngfx --activity "GPU Trace Profiler" --exe $Exe --dir $Game --args $Arguments --start-after-ms $StartMs --limit-to-frames 1 @Extra --auto-export --output-dir $Out *> (Join-Path $Out "ngfx.log")
    Get-Process $Process -ErrorAction SilentlyContinue | Stop-Process -Force
    $Dirs += (Join-Path $Out "BASE")
}

python (Join-Path $PSScriptRoot "nsight_passes.py") ($Dirs -join ";") $Passes $(if ($All) { "all" } else { "" })
