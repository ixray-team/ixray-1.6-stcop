#include "resource.h"

#include "../xrEngine/stdafx.h"
#include "../xrEngine/x_ray.h"

#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/xr_ioc_cmd.h"
#include "../xrEngine/string_table.h"

#include <SDL3/SDL.h>
#include "Splash.h"

#include "../xrCore/git_version.h"
#include "UIEditorMain.h"

void EnumerateDisplayModes()
{
	PROF_EVENT("EnumerateDisplayModes");
	SDL_DisplayID primaryDisplay = SDL_GetPrimaryDisplay();
	if (!primaryDisplay)
	{
		return;
	}
	bool isHigherResolutionFound = false;

	const char* name = SDL_GetDisplayName(primaryDisplay);
	SDL_Log("Enumerating for display %" SDL_PRIu32 ": %s\n", primaryDisplay, name ? name : "Unknown");

	const SDL_DisplayMode* pDisplayMode = SDL_GetDesktopDisplayMode(primaryDisplay);
	if (!pDisplayMode)
	{
		SDL_Log("Failed to get display mode, using defaults ...");
		psCurrentVidMode[0] = 1024;
		psCurrentVidMode[1] = 768;
		return;
	}

	if (isHigherResolutionFound && psCurrentVidMode[0] < pDisplayMode->w && psCurrentVidMode[1] < pDisplayMode->h)
	{
		psCurrentVidMode[0] = pDisplayMode->w;
		psCurrentVidMode[1] = pDisplayMode->h;
	}
	else if (!isHigherResolutionFound)
	{
		psCurrentVidMode[0] = pDisplayMode->w;
		psCurrentVidMode[1] = pDisplayMode->h;
		isHigherResolutionFound = true;
	}
}


void MigrateToGameWindow()
{
	PROF_EVENT("MigrateToGameWindow");
	SDL_ShowWindow(g_AppInfo.Window);
	SDL_SetWindowTitle(g_AppInfo.Window, "IX-Ray Engine");
		
	Console->Execute("vid_restart");
	SDL_GetWindowSizeInPixels(g_AppInfo.Window, &Device.Width, &Device.Height);
	SDL_GetWindowPosition(g_AppInfo.Window, &Device.PosX, &Device.PosY);
}

static void LoadCustomSettings()
{
	PROF_EVENT("LoadCustomSettings");
	FS_FileSet settingsFiles = {};
	FS.file_list(settingsFiles, _game_config_, FS_ListFiles, "ixray_settings\\default_settings*.ltx");

	for (auto& fsFile : settingsFiles)
	{
		string_path defaultSettings = {};
		FS.update_path(defaultSettings, _game_config_, fsFile.name.c_str());
		Console->ExecuteScript(defaultSettings);
	}
}

#ifdef IXR_WINDOWS
int APIENTRY WinMain
(
	HINSTANCE hInstance,
	HINSTANCE hPrevInstance,
	char* lpCmdLine,
	int nCmdShow
)
#else
int main(int argc, char* argv[])
#endif
{
#ifndef IXR_WINDOWS
	std::string cmd_line;

	for (int i = 0; i < argc; ++i) {
		cmd_line += argv[i];
		cmd_line += " ";
	}
#endif

	{
	PROF_EVENT("START_ENGINE");
	if (!SDL_Init(SDL_INIT_AUDIO | SDL_INIT_VIDEO | SDL_INIT_GAMEPAD | SDL_INIT_EVENTS))
	{
		return -1;
	}

	//std::jthread s(splash::Show); //

	splash::SetProgressStatus(5, "Initializing debugger");
	Debug._initialize(false);

	// Check for another instance
#if defined(NO_MULTI_INSTANCES) && defined(IXR_WINDOWS)
#define STALKER_PRESENCE_MUTEX TEXT("Local\\STALKER-COP")

	HANDLE hCheckPresenceMutex = INVALID_HANDLE_VALUE;
	hCheckPresenceMutex = OpenMutex(READ_CONTROL, false, STALKER_PRESENCE_MUTEX);
	if (hCheckPresenceMutex == nullptr) {
		// New mutex
		hCheckPresenceMutex = CreateMutex(nullptr, false, STALKER_PRESENCE_MUTEX);
		if (hCheckPresenceMutex == nullptr)
			// Shit happens
			return 2;
	}
	else {
		// Already running
		CloseHandle(hCheckPresenceMutex);
		return 1;
	}
#endif
	splash::SetProgressStatus(10, "Calculating display modes");
	EnumerateDisplayModes();

	g_AppInfo.Window = SDL_CreateWindow("IX-Ray Engine", 0, 0, 0);
	//SDL_HideWindow(g_AppInfo.Window);

	splash::SetProgressStatus(20, "Initializing xrCore");
#ifdef IXR_WINDOWS
	EngineLoadStage1(lpCmdLine);
#else
	EngineLoadStage1(cmd_line.data());
#endif
	//plat
	std::jthread s(splash::Show);
#ifdef DEBUG_DRAW
	xrLogger::EnableFastDebugLog();
#endif
	splash::SetProgressStatus(30, "Initializing engine");
	EngineLoadStage2();

	splash::SetProgressStatus(40, "Calculating renderer list");
	Engine.External.CreateRendererList();

		{
			PROF_EVENT("Console::Create");
	Console = new CConsole();
		}
	splash::SetProgressStatus(50, "Reading user settings");
	EngineLoadStage3();

		{
			PROF_EVENT("Select Render");
	if (Core.ParamsData.test(ECoreParams::r4)) {
		Console->Execute("renderer renderer_r4");
	}
	else if (Core.ParamsData.test(ECoreParams::r2)) {
		Console->Execute("renderer renderer_r2");
			}
			else {
		CCC_LoadCFG_custom* pTmp = new CCC_LoadCFG_custom("renderer ");
		pTmp->Execute(Console->ConfigFile);
		xr_delete(pTmp);
		// В любом случае надо вызывать команду CCC_R2
		Console->Execute((std::string("renderer ") + Console->GetToken("renderer")).c_str());
	}
		}

	splash::SetProgressStatus(60, "Initializing engine external");
	Engine.External.Initialize();

	//Console->Execute("stat_memory");
	Msg("IX-Ray %s %s build info: hash[%s] branch[%s] commit author[%s]", EngineExternal().GetCurrentPlatformFullName(), _VER, _HASH, _BRANCH, _AUTHOR);

	splash::SetProgressStatus(70, "Creating device");
	EngineLoadStage4();

	splash::SetProgressStatus(80, "Loading custom settings");
	LoadCustomSettings();
	
#ifdef DEBUG_DRAW
	RenderUI();
	EditorLuaInit();
	ECSViewDraw();
#endif

	splash::SetProgressStatus(90, "Loading menu");

	EngineLoadStage5();
	splash::Close();

	MigrateToGameWindow();
	EngineLoadStage6();

	xr_delete(g_pStringTable);

	Core._destroy();
	s.join();
#ifdef NO_MULTI_INSTANCES		
	// Delete application presence mutex
	CloseHandle(hCheckPresenceMutex);
#endif
	}

	return (0);
}
