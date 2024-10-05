// xrCore.cpp : Defines the entry point for the DLL application.
//
#include "stdafx.h"
#include "XmlParser/Expression.h"
#pragma hdrstop

#ifdef IXR_WINDOWS
#include <mmsystem.h>
#include <objbase.h>
#endif
#include "xrCore.h"
#include "discord/discord.h"

#ifdef DEBUG
#	include	<malloc.h>
#endif // DEBUG

XRCORE_API		xrCore	Core;
XRCORE_API		u32		build_id;
XRCORE_API		LPCSTR	build_date;

namespace CPU
{
	extern	void			Detect	();
};

static u32	init_counter	= 0;

char g_application_path[256];

//. extern xr_vector<shared_str>*	LogFile;

void xrCore::_initialize	(LPCSTR _ApplicationName, xrLogger::LogCallback cb, BOOL init_fs, LPCSTR fs_fname)
{
	xr_strcpy					(ApplicationName,_ApplicationName);
	if (0==init_counter) 
	{
#ifdef XRCORE_STATIC	
		_clear87	();
		_control87	( _PC_53,   MCW_PC );
		_control87	( _RC_CHOP, MCW_RC );
		_control87	( _RC_NEAR, MCW_RC );
		_control87	( _MCW_EM,  MCW_EM );
#endif
		// Init COM so we can use CoCreateInstance
#ifdef IXR_WINDOWS
        CoInitializeEx	(nullptr, COINIT_MULTITHREADED);

		xr_strcpy			(Params,sizeof(Params),GetCommandLineA());
		_strlwr_s			(Params,sizeof(Params));

		LoadParams();
#endif

		string_path		fn,dr,di;

		// application path
#ifdef IXR_WINDOWS
        GetModuleFileNameA(GetModuleHandleA(MODULE_NAME),fn,sizeof(fn));
        _splitpath		(fn,dr,di,0,0);
		xr_strconcat(ApplicationPath,dr,di);

		GetCurrentDirectoryA(sizeof(WorkingPath),WorkingPath);
#else
        xr_strcpy(ApplicationPath, SDL_GetBasePath());
        xr_strcpy(WorkingPath, SDL_GetBasePath());
#endif

		xr_strcpy(g_application_path,sizeof(g_application_path),ApplicationPath);

		// User/Comp Name
#ifdef IXR_WINDOWS
        DWORD sz_user = sizeof(UserName);
		GetUserNameA(UserName,&sz_user);

		DWORD sz_comp = sizeof(CompName);
		GetComputerNameA(CompName,&sz_comp);
#endif

		// Mathematics & PSI detection
		CPU::Detect			();
		
		Memory._initialize	(Core.ParamsData.test(ECoreParams::mem_debug));

		xrLogger::InitLog();
		_initialize_cpu		();

//		Debug._initialize	();

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

		FS._initialize		(flags,0,fs_fname);
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

#include "compression_ppmd_stream.h"
extern compression::ppmd::stream	*trained_model;

void xrCore::_destroy		()
{
	--init_counter;
	if (0==init_counter){
		FS._destroy			();
		EFS._destroy		();
		xr_delete			(xr_FS);
		xr_delete			(xr_EFS);

		if (trained_model) {
			void			*buffer = trained_model->buffer();
			xr_free			(buffer);
			xr_delete		(trained_model);
		}

		Memory._destroy		();
	}
}

#ifndef XRCORE_STATIC

//. why ??? 
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
    return TRUE;
}
#endif
#endif