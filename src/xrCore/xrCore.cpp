// xrCore.cpp : Defines the entry point for the DLL application.
//
#include "stdafx.h"
#include "FormatParsers/XML/Expression.h"
#include "compression/ppmd/compression_ppmd_stream.h"

#ifdef IXR_WINDOWS
#	include <mmsystem.h>
#	include <objbase.h>
#endif

#include "xrCore.h"
#include "discord/discord.h"
#include "stack_string.h"
#include "ECS/EntityManager.h"

XRCORE_API xrCore	Core;
XRCORE_API u32		build_id;
XRCORE_API const char*	build_date;
XRCORE_API bool ignore_error_window = false;
namespace CPU
{
	extern void Detect();
};

static u32	init_counter	= 0;

char g_application_path[256];

//. extern xr_vector<shared_str>*	LogFile;

extern xr_hash_map<xr_string, CInifile*>* cached_ini_map;

void xrCore::_initialize	(const char* _ApplicationName, xrLogger::LogCallback cb, bool init_fs, const char* fs_fname)
{
	GECSManager = new CECSManager;

	cached_ini_map = new xr_hash_map<xr_string, CInifile*>();

	PROF_EVENT("xrCore::_initialize");
	xr_strcpy					(ApplicationName,_ApplicationName);
	if (0==init_counter) 
	{
		// Init COM so we can use CoCreateInstance
#ifdef IXR_WINDOWS
        CoInitializeEx	(nullptr, COINIT_MULTITHREADED);

		xr_strcpy			(Params,sizeof(Params),GetCommandLineA());
		_strlwr_s			(Params,sizeof(Params));

		LoadParams();
#endif

		// application path
		std::string ApplicationPath = Platform::GetBinaryFolderPath().string();
		std::string WorkingPath = std::filesystem::current_path().string();

		xr_strcpy(g_application_path, sizeof(g_application_path), ApplicationPath.c_str());

		// User/Comp Name
		std::string user_name = Platform::GetUsrName();
		std::string comp_name = Platform::GetCompName();

		xr_strcpy(UserName, sizeof(UserName), user_name.c_str());
		xr_strcpy(CompName, sizeof(CompName), comp_name.c_str());

		// Mathematics & PSI detection
		CPU::Detect			();
		
		Memory._initialize();

		xrLogger::InitLog();
		_initialize_cpu		();

		rtc_initialize		();

		xr_FS				= new CLocatorAPI	();

		xr_EFS				= new EFS_Utils		();
		g_uiExpressionMgr = new CExpressionManager();

		g_Discord.Init();
	}

	if (init_fs)
	{
		u32 flags = 0;
		if (Core.ParamsData.test(ECoreParams::build))	
			flags |= CLocatorAPI::flBuildCopy;

		if (Core.ParamsData.test(ECoreParams::ebuild))
			flags |= CLocatorAPI::flBuildCopy|CLocatorAPI::flEBuildCopy;

		flags |= CLocatorAPI::flScanAppRoot;

		FS._initialize		(flags,nullptr,fs_fname);
		BuildId             = build_id;
		Msg					("'%s' build %d, %s\n","xrCore",build_id, build_date);
		EFS._initialize		();
#if defined(DEBUG) && defined(IXR_WINDOWS)
		Msg					("CRT heap 0x%08x",_get_heap_handle());
		Msg					("Process heap 0x%08x",GetProcessHeap());
#endif // DEBUG
	}
	xrLogger::AddLogCallback(cb);
	init_counter++;
}

extern compression::ppmd::stream	*trained_model;

void xrCore::_destroy()
{
	--init_counter;
	if (0 == init_counter)
	{
		FS._destroy();
		EFS._destroy();
		xr_delete(xr_FS);
		xr_delete(xr_EFS);

		xr_delete(GECSManager);

		if (trained_model) {
			void* buffer = trained_model->buffer();
			xr_free(buffer);
			xr_delete(trained_model);
		}
		xr_delete(cached_ini_map);
		Memory._destroy();
	}
}

#ifdef IXR_WINDOWS
BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD ul_reason_for_call, LPVOID lpvReserved)
{
	switch (ul_reason_for_call)
	{
	case DLL_PROCESS_ATTACH:
		{
			_clear87		();
			_control87		( _PC_53,   MCW_PC );
			_control87		( _RC_CHOP, MCW_RC );
			_control87		( _RC_NEAR, MCW_RC );
			_control87		( _MCW_EM,  MCW_EM );
		}
//.		LogFile.reserve		(256);
		break;
	case DLL_THREAD_ATTACH:
		timeBeginPeriod	(1);
		break;
	case DLL_THREAD_DETACH:
		break;
	case DLL_PROCESS_DETACH:
		break;
	}
    return true;
}
#endif