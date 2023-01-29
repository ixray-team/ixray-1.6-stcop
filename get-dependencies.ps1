# Test and set path to 7-Zip
$globalPaths = $env:Path -split ';'
ForEach ($line in $globalPaths) {
    If (!(Test-Path ($line + "\7z.exe"))) {
        $path = Join-Path -Path ${env:ProgramFiles} `
                          -ChildPath "7-Zip\7z.exe"
    }
}

# Getting DirectX SDK March 2009 from archive
If (!(Test-Path "sdk\dxsdk_mar2009")) {
    $uri = "https://github.com/ixray-team/ixray-1.6-stcop/" + `
           "releases/download/r0.2/sdk-directxsdk-mar2009.7z"
    Invoke-WebRequest -Uri $uri `
                      -OutFile "directxsdk-mar2009.7z"
    Start-Process -FilePath $path `
                  -ArgumentList "x directxsdk-mar2009.7z" `
                  -NoNewWindow -Wait
    Remove-Item "directxsdk-mar2009.7z"
}

# Getting DirectX SDK June 2010 from archive
If (!(Test-Path "sdk\dxsdk_jun2010")) {
    $uri = "https://github.com/ixray-team/ixray-1.6-stcop/" + `
           "releases/download/r0.2/sdk-directxsdk-jun2010.7z"
    Invoke-WebRequest -Uri $uri `
                      -OutFile "directxsdk-jun2010.7z"
    Start-Process -FilePath $path `
                  -ArgumentList "x directxsdk-jun2010.7z" `
                  -NoNewWindow -Wait
    Remove-Item "directxsdk-jun2010.7z"
}

# Getting another dependencies from Git
git clone --branch jul2022 --depth 1 `
    https://github.com/microsoft/DirectXTex.git `
    dep/DirectXTex
git clone --branch jul2022 --depth 1 `
    https://github.com/microsoft/DirectXMesh.git `
    dep/DirectXMesh
git clone --branch Release_3.1.0 --depth 1 `
    https://github.com/dockpanelsuite/dockpanelsuite.git `
    dep/dockpanelsuite
