#pragma once
#include "../xrEngine/stdafx.h"

#ifdef IXRAY_ENABLE_STEAM_OVERLAY
#include "SteamOverlay.h"
#endif
#include "resource.h"

class CApplication
{
public:
	CApplication(ENTRY_ARGS);
	~CApplication() = default;

	int Run();

	void InitEngine();

private:
	int BeginPlay();
	void EndPlay();

	void InitDebugTools();
	void EnumerateDisplayModes();
	void MigrateToGameWindow();
	void LoadCustomSettings();
	void ConfigureRenderer();

#if defined(IXR_WINDOWS)
	Platform::CMutexHandle CheckSingleInstance();
#endif

private:
	xr_string CommandLine;
#ifdef IXRAY_ENABLE_STEAM_OVERLAY
	CSteamOverlay SteamWorks;
#endif
};
