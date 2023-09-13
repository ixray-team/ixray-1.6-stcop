@echo off

if exist "build" rd /q /s "build"

if exist "_Build" rd /q /s "_Build"
if exist "_Compiler" rd /q /s "_Compiler"
if exist "_NRI_SDK" rd /q /s "_NRI_SDK"
if exist "External/AGS" rd /q /s "External/AGS"
if exist "External/NVAPI" rd /q /s "External/NVAPI"
