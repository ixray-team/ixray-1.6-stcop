//-----------------------------------------------------------------------------
// File: x_ray.cpp
//
// Programmers:
//	Oles		- Oles Shishkovtsov
//	AlexMX		- Alexander Maksimchuk
//-----------------------------------------------------------------------------
#include "stdafx.h"


#include "xr_input.h"
#include "XR_IOConsole.h"
#include "x_ray.h"
#include "GameFont.h"
#include "LightAnimLibrary.h"
#include "../xrCore/Collision/ISpatial.h"
#include <luabind/luabind.hpp>
#include <luabind/luabind_memory.h>
#include "string_table.h"
#include "../xrCore/discord/discord.h"
#include "std_classes.h"
#include "IGame_Persistent.h"
#include "IGame_Level.h"
#include "FPSCounter.h"
#include "GamepadService.h"
#include "Autotest.h"
#include "BuildId.h"

//---------------------------------------------------------------------
static bool IntroFinished = false;
ENGINE_API CInifile* pGameIni = nullptr;
ENGINE_API bool g_dedicated_server = false;
extern void Intro(void* fn);
extern void Intro_DSHOW(void* fn);

//////////////////////////////////////////////////////////////////////////
// global variables
ENGINE_API CEngineApp* pApp = nullptr;

ENGINE_API string512 g_sLaunchOnExit_params;
ENGINE_API string512 g_sLaunchOnExit_app;
ENGINE_API string_path g_sLaunchWorkingFolder;

void InitSettings()
{
	PROF_EVENT("InitSettings");
	string_path fname;
	FS.update_path(fname, _game_config_, "system.ltx");
#ifdef DEBUG
	Msg("Updated path to system.ltx is %s", fname);
#endif // #ifdef DEBUG
	pSettings = new CInifile(fname, true);
	CHECK_OR_EXIT(0 != pSettings->section_count(), make_string<const char*>("Cannot find file %s.\nReinstalling application may fix this problem.", fname));

	FS.update_path(fname, _game_config_, "game.ltx");
	pGameIni = new CInifile(fname, true);
	CHECK_OR_EXIT(0 != pGameIni->section_count(), make_string<const char*>("Cannot find file %s.\nReinstalling application may fix this problem.", fname));
}

void destroySettings()
{
	CInifile** s = (CInifile**)(&pSettings);
	xr_delete(*s);
	xr_delete(pGameIni);
}

void destroyConsole()
{
	Console->Execute("cfg_save");
	Console->Destroy();
	xr_delete(Console);
}

void execUserScript()
{
	PROF_EVENT("execUserScript");
	Console->Execute("default_controls");
	Console->ExecuteScript(Console->ConfigFile);
}

ENGINE_API void EngineLoadStage4()
{
	PROF_EVENT("EngineLoadStage4");
	CSound_manager_interface::_create(0);

	execUserScript();
	if (Autotest::Active())
	{
		psDeviceFlags.set(rsFullscreen, FALSE);
	}

	CSound_manager_interface::_create(1);

	// ...command line for auto start
	{
		const char* pStartup = strstr(Core.Params, "-start ");
		if (pStartup)
		{
			Console->Execute(pStartup + 1);
		}
	}
	{
		const char* pStartup = strstr(Core.Params, "-load ");
		if (pStartup)
		{
			Console->Execute(pStartup + 1);
		}
	}

	// Initialize APP
	g_FontManager = new CFontManager();
	bool ValidRenderDeviceInit = Device.InitRenderDevice(Engine.External.GetAPI());
	R_ASSERT(ValidRenderDeviceInit);
	CRenderDevice::SetStartupStatus(72, "Creating render device");

	Device.Create();
	g_FontManager->InitializeFonts();
	pFPSCounter = new XRay::Hardware::FPSCounter();
	CRenderDevice::SetStartupStatus(78, "Render device created");
	CRenderDevice::PumpStartupFrames(4);
}

ENGINE_API void EngineLoadStage5()
{
	PROF_EVENT("EngineLoadStage5");
	LALib.OnCreate();
	pApp = new CEngineApp();

	CRenderDevice::StartupFrameWaitTask
	(
		[]()
		{
			g_pGamePersistent = (IGame_Persistent*)NEW_INSTANCE(CLSID_GAME_PERSISTANT); 
		},
		90,
		"Loading persistent objects"
	);

	g_SpatialSpace = new ISpatial_DB();
	g_SpatialSpacePhysic = new ISpatial_DB();

	if (Device.m_pRender)
	{
		Device.m_pRender->PostCreate();
	}

	CRenderDevice::SetStartupStatus(91, "Initializing spatial database");
	CRenderDevice::PumpStartupFrames(4);
}

ENGINE_API void EngineLoopAndDestroy()
{
	PROF_EVENT("EngineLoopAndDestroy");

	Device.Run();

	// Destroy level if still loaded (e.g. quit from in-game via window close or console)
	if (g_pGameLevel)
	{
		g_pGameLevel->net_Stop();
		DEL_INSTANCE(g_pGameLevel);
	}

	// Destroy APP
	xr_delete(g_SpatialSpacePhysic);
	xr_delete(g_SpatialSpace);
	DEL_INSTANCE(g_pGamePersistent);

	xr_delete(pApp);
	g_pEventManager->Event.Dump();

	xr_delete(pFPSCounter);

	// Destroying
	xr_delete(pInput);

	destroySettings();
	LALib.OnDestroy();

	destroyConsole();

	// очищение памяти таблицы строк
	CStringTable::Destroy();

	CSound_manager_interface::_destroy();
	Device.Destroy();

	// This should prevent empty log file in some cases
	xrLogger::FlushLog();

	Engine.Destroy();
	xr_delete(DevicePtr);
}

ENGINE_API void EngineLoadStage1(char* lpCmdLine)
{
	PROF_EVENT("EngineLoadStage1");
	// AVI
	IntroFinished = true;

	g_sLaunchOnExit_app[0] = 0;
	g_sLaunchOnExit_params[0] = 0;

	const char* fsgame_ltx_name = "-fsltx ";
	string_path fsgame = "";

	if (strstr(lpCmdLine, fsgame_ltx_name))
	{
		int sz = xr_strlen(fsgame_ltx_name);
		sscanf(strstr(lpCmdLine, fsgame_ltx_name) + sz, "%519[^ ] ", fsgame);
	}

	ComputeBuildID();
#ifndef IXR_APPLE_SERIES
	// HACK: filewatch's FSEvents backend asserts and aborts the game ~1 min in on macOS
	// (its notify() runs on the FSEvents thread). Hot-reload of gamedata is off there.
	CFilewatcher::instance().SetFilewatcherActive(true);
#endif
	Core._initialize("IXRay", nullptr, true, fsgame[0] ? fsgame : nullptr);

	InitSettings();

	// Adjust player & computer name for Asian
	if (pSettings->line_exist("string_table", "no_native_input"))
	{
		xr_strcpy(Core.UserName, sizeof(Core.UserName), "Player");
		xr_strcpy(Core.CompName, sizeof(Core.CompName), "Computer");
	}

	EngineExternal();
}

ENGINE_API const char* GetClipboardErrorMessage()
{
	VERIFY(g_pStringTable);
	return g_pStringTable->translate("st_clipboard_error_message").c_str();
}

ENGINE_API void EngineLoadStage2()
{
	PROF_EVENT("EngineLoadStage2");

	PROF_EVENT("InitEngine");
	if (DevicePtr == nullptr)
	{
		DevicePtr = new CRenderDevice();
	}

	Engine.Initialize();
	while (!IntroFinished)
	{
		Sleep(100);
	}
	Device.Initialize();

	pInput = new CInput();

	g_pStringTable = new CStringTable();
	ClipboardMessageCallback::instance().SetFunc(&GetClipboardErrorMessage);
}

ENGINE_API void EngineLoadStage3()
{
	PROF_EVENT("EngineLoadStage3");
	Console->Initialize();

	shared_str UserName = "user.ltx";
	if (g_dedicated_server)
	{
		UserName = "user_dedicated.ltx";
	}

	xr_strcpy(Console->ConfigFile, *UserName);

	if (strstr(Core.Params, "-ltx "))
	{
		string64 c_name = {};
		sscanf(strstr(Core.Params, "-ltx ") + 5, "%63[^ ] ", c_name);
		xr_strcpy(Console->ConfigFile, c_name);
	}
}