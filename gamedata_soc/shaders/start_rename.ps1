# Define the directories
$directories = @("r1", "r2", "r3", "editor", "shared")

# Define the extension mappings
$extensionMappings = @{
    ".h"  = ".hlsli"
    ".s"  = ".lua"
    ".s_"  = ".lua_"
    ".ps" = ".ps.hlsl"
    ".vs" = ".vs.hlsl"
    ".ds" = ".ds.hlsl"
    ".hs" = ".hs.hlsl"
    ".gs" = ".gs.hlsl"
    ".cs" = ".cs.hlsl"
}

# Loop through each directory
foreach ($dir in $directories) {
    # Loop through each extension mapping
    foreach ($ext in $extensionMappings.Keys) {
        # Get all files with the current extension recursively in the current directory
        $files = Get-ChildItem -Path $dir -Filter "*$ext" -Recurse

        # Loop through each file and rename it
        foreach ($file in $files) {
            $newName = [System.IO.Path]::ChangeExtension($file.FullName, $extensionMappings[$ext])
            Rename-Item -Path $file.FullName -NewName $newName
        }
    }
}
