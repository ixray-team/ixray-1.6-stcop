@echo off

title IX-Ray

git.exe clone --branch aug2021 --depth 1 https://github.com/microsoft/DirectXTex.git dep/DirectXTex
git.exe clone --branch Release_3.1.0 --depth 1 https://github.com/dockpanelsuite/dockpanelsuite.git dep/dockpanelsuite

pause
