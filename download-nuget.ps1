# Download Nuget
$uri = "https://dist.nuget.org/win-x86-commandline/latest/nuget.exe"
Invoke-WebRequest -Uri $uri `
                  -OutFile "dep\nuget\nuget.exe"
