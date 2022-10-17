# Set title of the window
$Host.UI.RawUI.WindowTitle = "IX-Ray"

# Set path to 7-Zip
$path = Join-Path -Path ${env:ProgramFiles} -ChildPath "7-Zip\7z.exe"

# Getting DirectX SDK March 2009 from archive
If (!(Test-Path "sdk\dxsdk_mar2009")) {
    Invoke-WebRequest -Uri "https://github.com/ixray-team/ixray-1.6-stcop/releases/download/r0.2/sdk-directxsdk-mar2009.7z" `
                      -OutFile "directxsdk-mar2009.7z"
    Start-Process -FilePath $path `
                  -ArgumentList "x directxsdk-mar2009.7z" `
                  -NoNewWindow -Wait
    Remove-Item "directxsdk-mar2009.7z"
}

# Getting DirectX SDK June 2010 from archive
If (!(Test-Path "sdk\dxsdk_jun2010")) {
    Invoke-WebRequest -Uri "https://github.com/ixray-team/ixray-1.6-stcop/releases/download/r0.2/sdk-directxsdk-jun2010.7z" `
                      -OutFile "directxsdk-jun2010.7z"
    Start-Process -FilePath $path `
                  -ArgumentList "x directxsdk-jun2010.7z" `
                  -NoNewWindow -Wait
    Remove-Item "directxsdk-jun2010.7z"
}

# Getting another dependencies from Git
git clone --branch jul2022 --depth 1 https://github.com/microsoft/DirectXTex.git dep/DirectXTex
git clone --branch jul2022 --depth 1 https://github.com/microsoft/DirectXMesh.git dep/DirectXMesh
git clone --branch Release_3.1.0 --depth 1 https://github.com/dockpanelsuite/dockpanelsuite.git dep/dockpanelsuite

# Pause
Write-Host "Press any key to continue..."
$Host.UI.RawUI.ReadKey("NoEcho, IncludeKeyDown") | Out-Null
