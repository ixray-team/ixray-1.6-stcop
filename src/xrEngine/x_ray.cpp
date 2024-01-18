//-----------------------------------------------------------------------------
// File: x_ray.cpp
//
// Programmers:
//	Oles		- Oles Shishkovtsov
//	AlexMX		- Alexander Maksimchuk
//-----------------------------------------------------------------------------
#include "stdafx.h"

#include "../xrNetServer/NET_AuthCheck.h"

#include "xr_input.h"
#include "xr_ioconsole.h"
#include "x_ray.h"
#include "GameFont.h"
#include "LightAnimLibrary.h"
#include "../xrCDB/ispatial.h"
#include "Text_Console.h"
#include <luabind/luabind.hpp>
#include <luabind/memory_allocator.hpp>
#include "string_table.h"
#include "../xrCore/discord/discord.h"
#include "std_classes.h"
#include "IGame_Persistent.h"

//---------------------------------------------------------------------
ENGINE_API CInifile* pGameIni		= NULL;
BOOL	g_bIntroFinished			= FALSE;
extern	void	Intro				( void* fn );
extern	void	Intro_DSHOW			( void* fn );
extern	int PASCAL IntroDSHOW_wnd	(HINSTANCE hInstC, HINSTANCE hInstP, LPSTR lpCmdLine, int nCmdShow);
//int		max_load_stage = 0;

// computing build id
XRCORE_API	LPCSTR	build_date;
XRCORE_API	u32		build_id;

#ifdef MASTER_GOLD
#	define NO_MULTI_INSTANCES
#endif // #ifdef MASTER_GOLD


static LPCSTR month_id[12] =
{
	"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"
};

static int days_in_month[12] = 
{
	31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
};

static int start_day	= 31;	// 31
static int start_month	= 1;	// January
static int start_year	= 1999;	// 1999

void compute_build_id()
{
	build_date = __DATE__;

	int days;
	int months = 0;
	int years;

	string16 month;

	string256 buffer;
	xr_strcpy(buffer, __DATE__);

	sscanf(buffer, "%s %d %d", month, &days, &years);

	for (int i = 0; i < 12; i++)
	{
		if (_stricmp(month_id[i], month))
			continue;

		months = i;
		break;
	}

	build_id = (years - start_year) * 365 + days - start_day;

	for (int i = 0; i < months; ++i)
		build_id += days_in_month[i];

	for (int i = 0; i < start_month - 1; ++i)
		build_id -= days_in_month[i];
}

//////////////////////////////////////////////////////////////////////////
// global variables
ENGINE_API	CApplication*	pApp			= NULL;

ENGINE_API	string512		g_sLaunchOnExit_params;
ENGINE_API	string512		g_sLaunchOnExit_app;
ENGINE_API	string_path		g_sLaunchWorkingFolder;
// -------------------------------------------
// startup point
void InitEngine		()
{
	DevicePtr = xr_make_unique<CRenderDevice>();

	Engine.Initialize			( );
	while (!g_bIntroFinished)	Sleep	(100);
	Device.Initialize			( );
}

struct path_excluder_predicate
{
	explicit path_excluder_predicate(xr_auth_strings_t const * ignore) :
		m_ignore(ignore)
	{
	}
	bool xr_stdcall is_allow_include(LPCSTR path)
	{
		if (!m_ignore)
			return true;
		
		return allow_to_include_path(*m_ignore, path);
	}
	xr_auth_strings_t const *	m_ignore;
};

void InitSettings	()
{
	string_path					fname; 
	FS.update_path				(fname,"$game_config$","system.ltx");
#ifdef DEBUG
	Msg							("Updated path to system.ltx is %s", fname);
#endif // #ifdef DEBUG
	pSettings					= xr_new<CInifile>	(fname,TRUE);
	CHECK_OR_EXIT				(0!=pSettings->section_count(), make_string<const char*>("Cannot find file %s.\nReinstalling application may fix this problem.",fname));

	xr_auth_strings_t			tmp_ignore_pathes;
	xr_auth_strings_t			tmp_check_pathes;
	fill_auth_check_params		(tmp_ignore_pathes, tmp_check_pathes);
	
	path_excluder_predicate			tmp_excluder(&tmp_ignore_pathes);
	CInifile::allow_include_func_t	tmp_functor;
	tmp_functor.bind(&tmp_excluder, &path_excluder_predicate::is_allow_include);

	FS.update_path				(fname,"$game_config$","game.ltx");
	pGameIni					= xr_new<CInifile>	(fname,TRUE);
	CHECK_OR_EXIT				(0!=pGameIni->section_count(), make_string<const char*>("Cannot find file %s.\nReinstalling application may fix this problem.",fname));
}

void InitInput		()
{
	BOOL bCaptureInput			= !strstr(Core.Params,"-i");

	pInput						= xr_new<CInput>		();
}
void destroyInput	()
{
	xr_delete					( pInput		);
}

void InitSound1		()
{
	CSound_manager_interface::_create				(0);
}

void InitSound2		()
{
	CSound_manager_interface::_create				(1);
}

void destroySound	()
{
	CSound_manager_interface::_destroy				( );
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

void destroyEngine()
{
	Device.Destroy();

	// This should prevent empty log file in some cases
	xrLogger::FlushLog();

	Engine.Destroy();
	DevicePtr.release();
}

void execUserScript()
{
	Console->Execute("default_controls");
	Console->ExecuteScript(Console->ConfigFile);
}

ENGINE_API void EngineLoadStage4()
{
	InitSound1();
	execUserScript();
	InitSound2();

	// ...command line for auto start
	{
		LPCSTR	pStartup = strstr(Core.Params, "-start ");
		if (pStartup)				Console->Execute(pStartup + 1);
	}
	{
		LPCSTR	pStartup = strstr(Core.Params, "-load ");
		if (pStartup)				Console->Execute(pStartup + 1);
	}

	// Initialize APP
	g_FontManager = new CFontManager();
	bool ValidRenderDeviceInit = Device.InitRenderDevice(Engine.External.GetAPI());
	R_ASSERT(ValidRenderDeviceInit);

	Device.Create();
	g_FontManager->InitializeFonts();
}

ENGINE_API void EngineLoadStage5()
{
	LALib.OnCreate();
	pApp = xr_new<CApplication>();
	g_pGamePersistent = (IGame_Persistent*)NEW_INSTANCE(CLSID_GAME_PERSISTANT);
	g_SpatialSpace = xr_new<ISpatial_DB>();
	g_SpatialSpacePhysic = xr_new<ISpatial_DB>();

	// Main cycle
	Memory.mem_usage();
	Device.Run					( );


	// Destroy APP
	xr_delete					( g_SpatialSpacePhysic	);
	xr_delete					( g_SpatialSpace		);
	DEL_INSTANCE				( g_pGamePersistent		);
	xr_delete					( pApp					);
	g_pEventManager->Event.Dump			( );

	// Destroying
//.	destroySound();
	destroyInput();

	destroySettings();

	LALib.OnDestroy				( );
	
	destroyConsole();

	destroySound();

	destroyEngine();
}


#define dwStickyKeysStructSize sizeof( STICKYKEYS )
#define dwFilterKeysStructSize sizeof( FILTERKEYS )
#define dwToggleKeysStructSize sizeof( TOGGLEKEYS )

struct damn_keys_filter {
	BOOL bScreenSaverState;

	// Sticky & Filter & Toggle keys

	STICKYKEYS StickyKeysStruct;
	FILTERKEYS FilterKeysStruct;
	TOGGLEKEYS ToggleKeysStruct;

	DWORD dwStickyKeysFlags;
	DWORD dwFilterKeysFlags;
	DWORD dwToggleKeysFlags;

	damn_keys_filter	()
	{
		// Screen saver stuff

		bScreenSaverState = FALSE;

		// Saveing current state
		SystemParametersInfo( SPI_GETSCREENSAVEACTIVE , 0 , ( PVOID ) &bScreenSaverState , 0 );

		if ( bScreenSaverState )
			// Disable screensaver
			SystemParametersInfo( SPI_SETSCREENSAVEACTIVE , FALSE , NULL , 0 );

		dwStickyKeysFlags = 0;
		dwFilterKeysFlags = 0;
		dwToggleKeysFlags = 0;


		ZeroMemory( &StickyKeysStruct , dwStickyKeysStructSize );
		ZeroMemory( &FilterKeysStruct , dwFilterKeysStructSize );
		ZeroMemory( &ToggleKeysStruct , dwToggleKeysStructSize );

		StickyKeysStruct.cbSize = dwStickyKeysStructSize;
		FilterKeysStruct.cbSize = dwFilterKeysStructSize;
		ToggleKeysStruct.cbSize = dwToggleKeysStructSize;

		// Saving current state
		SystemParametersInfo( SPI_GETSTICKYKEYS , dwStickyKeysStructSize , ( PVOID ) &StickyKeysStruct , 0 );
		SystemParametersInfo( SPI_GETFILTERKEYS , dwFilterKeysStructSize , ( PVOID ) &FilterKeysStruct , 0 );
		SystemParametersInfo( SPI_GETTOGGLEKEYS , dwToggleKeysStructSize , ( PVOID ) &ToggleKeysStruct , 0 );

		if ( StickyKeysStruct.dwFlags & SKF_AVAILABLE ) {
			// Disable StickyKeys feature
			dwStickyKeysFlags = StickyKeysStruct.dwFlags;
			StickyKeysStruct.dwFlags = 0;
			SystemParametersInfo( SPI_SETSTICKYKEYS , dwStickyKeysStructSize , ( PVOID ) &StickyKeysStruct , 0 );
		}

		if ( FilterKeysStruct.dwFlags & FKF_AVAILABLE ) {
			// Disable FilterKeys feature
			dwFilterKeysFlags = FilterKeysStruct.dwFlags;
			FilterKeysStruct.dwFlags = 0;
			SystemParametersInfo( SPI_SETFILTERKEYS , dwFilterKeysStructSize , ( PVOID ) &FilterKeysStruct , 0 );
		}

		if ( ToggleKeysStruct.dwFlags & TKF_AVAILABLE ) {
			// Disable FilterKeys feature
			dwToggleKeysFlags = ToggleKeysStruct.dwFlags;
			ToggleKeysStruct.dwFlags = 0;
			SystemParametersInfo( SPI_SETTOGGLEKEYS , dwToggleKeysStructSize , ( PVOID ) &ToggleKeysStruct , 0 );
		}
	}

	~damn_keys_filter	()
	{
		if ( bScreenSaverState )
			// Restoring screen saver
			SystemParametersInfo( SPI_SETSCREENSAVEACTIVE , TRUE , NULL , 0 );

		if ( dwStickyKeysFlags) {
			// Restore StickyKeys feature
			StickyKeysStruct.dwFlags = dwStickyKeysFlags;
			SystemParametersInfo( SPI_SETSTICKYKEYS , dwStickyKeysStructSize , ( PVOID ) &StickyKeysStruct , 0 );
		}

		if ( dwFilterKeysFlags ) {
			// Restore FilterKeys feature
			FilterKeysStruct.dwFlags = dwFilterKeysFlags;
			SystemParametersInfo( SPI_SETFILTERKEYS , dwFilterKeysStructSize , ( PVOID ) &FilterKeysStruct , 0 );
		}

		if ( dwToggleKeysFlags ) {
			// Restore FilterKeys feature
			ToggleKeysStruct.dwFlags = dwToggleKeysFlags;
			SystemParametersInfo( SPI_SETTOGGLEKEYS , dwToggleKeysStructSize , ( PVOID ) &ToggleKeysStruct , 0 );
		}

	}
};

#undef dwStickyKeysStructSize
#undef dwFilterKeysStructSize
#undef dwToggleKeysStructSize

static void* __cdecl luabind_allocator(void* context, const void* pointer, size_t const size)
{
	if (!size)
	{
		void* non_const_pointer = const_cast<LPVOID>(pointer);
		xr_free(non_const_pointer);
		return nullptr;
	}
	if (!pointer)
	{
		return xr_malloc(size);
	}
	void* non_const_pointer = const_cast<LPVOID>(pointer);
	return xr_realloc(non_const_pointer, size);
}

ENGINE_API	bool g_dedicated_server	= false;

ENGINE_API void EngineLoadStage1(char* lpCmdLine)
{
	// AVI
	g_bIntroFinished = TRUE;

	g_sLaunchOnExit_app[0] = NULL;
	g_sLaunchOnExit_params[0] = NULL;

	LPCSTR fsgame_ltx_name = "-fsltx ";
	string_path fsgame = "";

	if (strstr(lpCmdLine, fsgame_ltx_name)) {
		int						sz = xr_strlen(fsgame_ltx_name);
		sscanf					(strstr(lpCmdLine,fsgame_ltx_name)+sz,"%[^ ] ",fsgame);
	}

	compute_build_id			();
	Core._initialize			("xray",NULL, TRUE, fsgame[0] ? fsgame : NULL);

	InitSettings				();

	// Adjust player & computer name for Asian
	if ( pSettings->line_exist( "string_table" , "no_native_input" ) ) {
			xr_strcpy( Core.UserName , sizeof( Core.UserName ) , "Player" );
			xr_strcpy( Core.CompName , sizeof( Core.CompName ) , "Computer" );
	}

	EngineExternal();
}

ENGINE_API void EngineLoadStage2()
{
	damn_keys_filter filter;
	(void)filter;

	luabind::allocator = &luabind_allocator;
	luabind::allocator_context = nullptr;

	FPU::m24r();
	InitEngine();

	InitInput();

	g_pStringTable = new CStringTable();
}

ENGINE_API void EngineLoadStage3()
{
	Console->Initialize();

	xr_strcpy(Console->ConfigFile, "user.ltx");

	if (strstr(Core.Params, "-ltx ")) {
		string64 c_name;
		sscanf(strstr(Core.Params, "-ltx ") + 5, "%[^ ] ", c_name);
		xr_strcpy(Console->ConfigFile, c_name);
	}
}

int ENGINE_API WinMain_impl(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     char *    lpCmdLine,
                     int       nCmdShow)
{

	return					(0);
}