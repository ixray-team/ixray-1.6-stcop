// EngineAPI.cpp: implementation of the CEngineAPI class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "EngineAPI.h"
#include "../xrCore/Collision/xrCDB.h"

#include <filesystem>

extern xr_token* vid_quality_token;

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

void __cdecl dummy		(void)	{
};
CEngineAPI::CEngineAPI	()
{
	hGame			= 0;
	hRender			= 0;
	pCreate			= 0;
	pDestroy		= 0;
	hGameSpy		= 0;
}

CEngineAPI::~CEngineAPI()
{
	// destroy quality token here
	if (vid_quality_token)
	{
		for( int i=0; vid_quality_token[i].name; i++ )
		{
			xr_free					(vid_quality_token[i].name);
		}
		xr_free						(vid_quality_token);
		vid_quality_token			= nullptr;
	}
}

extern u32 renderer_value; //con cmd
ENGINE_API int g_current_renderer = 0;
ENGINE_API ELightingMode g_lighting_mode = ELightingMode::Static;
ENGINE_API ELightingMode g_lighting_mode_cfg = ELightingMode::Static;
ENGINE_API bool g_lighting_mode_locked = false;

ENGINE_API bool LightingModeParseToken(const char* name, ELightingMode& mode)
{
	if (!name)
		return false;
	if (!_stricmp(name, "renderer_r1") || !_stricmp(name, "renderer_r4_static"))
	{
		mode = ELightingMode::Static;
		return true;
	}
	if (!_stricmp(name, "renderer_r2") || !_stricmp(name, "renderer_r4"))
	{
		mode = ELightingMode::Dynamic;
		return true;
	}
	return false;
}

ENGINE_API const char* LightingModeCanonicalToken(ELightingMode mode)
{
	return mode == ELightingMode::Static ? "renderer_r4_static" : "renderer_r4";
}

ENGINE_API void LightingModeApply(ELightingMode mode, bool commit_active)
{
	g_lighting_mode_cfg = mode;
	if (!commit_active)
		return;

	g_lighting_mode = mode;
	psDeviceFlags.set(rsR4, true);
	psDeviceFlags.set(rsR2, mode == ELightingMode::Dynamic);
	g_current_renderer = (mode == ELightingMode::Static) ? 1 : 2;
}

ENGINE_API void LightingModeLockActive()
{
	LightingModeApply(g_lighting_mode_cfg, true);
	g_lighting_mode_locked = true;
}

void CEngineAPI::InitializeNotDedicated()
{
	const char* r4_name = "xrRender_R4";

	LightingModeLockActive();

	Msg("Loading DLL: %s [%s]", r4_name, LightingModeCanonicalToken(g_lighting_mode));
	hRender = Platform::LoadLibrary(r4_name);
	if (0 == hRender)
	{
		Msg("! Failed to load %s", r4_name);
		R_CHK(GetLastError());
	}
}

void CEngineAPI::InitializeDedicated()
{
	const char* r1_name	= "xrRender_DS0";
	psDeviceFlags.set	(rsR4,false);
	psDeviceFlags.set	(rsR2,false);
	renderer_value		= 0; //con cmd

	Msg("Loading DLL: %s",	r1_name);
	hRender			= Platform::LoadLibrary(r1_name);
	if (0==hRender)	R_CHK				(GetLastError());
	//R_ASSERT		(hRender);
	g_current_renderer	= 0;
}

void __cdecl Null_Factory_Destroy(DLL_Pure* O)
{
}

DLL_Pure* __cdecl Null_Factory_Create(CLASS_ID CLS_ID)
{
	return nullptr;
}

void CEngineAPI::Initialize(void)
{
	PROF_EVENT("CEngineAPI::Initialize");
	//////////////////////////////////////////////////////////////////////////
	// render
	if (!g_dedicated_server)
		InitializeNotDedicated();
	else
		InitializeDedicated();

	if (0==hRender && !g_dedicated_server)
	{
		Msg("! xrRender_R4 is required for client rendering");
		R_CHK(GetLastError());
	}

	Device.ConnectToRender();

	// game
	{
		const char* g_name	= "xrGame";

		Msg("Loading DLL: %s",g_name);
		hGame = Platform::LoadLibrary(g_name);
		if (0==hGame)	R_CHK			(GetLastError());
		R_ASSERT2		(hGame,"Game DLL raised exception during loading or there is no game DLL at all");

		if (hGame == nullptr)
		{
			pCreate = Null_Factory_Create;
			pDestroy = Null_Factory_Destroy;
		}
		else
		{
			pCreate = (Factory_Create*)Platform::GetAddress(hGame, "xrFactory_Create");		R_ASSERT(pCreate);
			pDestroy = (Factory_Destroy*)Platform::GetAddress(hGame, "xrFactory_Destroy");	R_ASSERT(pDestroy);

			using xrGameInitialize = void();
			xrGameInitialize* pxrGameInitialize = (xrGameInitialize*)Platform::GetAddress(hGame, "xrGameInitialize");
			R_ASSERT(pxrGameInitialize);

			pxrGameInitialize();
		}
	}

	// GameSpy
	{
		const char* g_name = "xrGameSpy";
		hGameSpy = Platform::LoadLibrary(g_name);

		if (hGameSpy != 0)
		{
			Msg("Found %s DLL! Enable MP subsystem!", g_name);
		}
	}
}

void CEngineAPI::Destroy(void)
{
	if (hGame)				
	{
		using callback_t = void();
		callback_t* pShutdownCallback = (callback_t*)Platform::GetAddress(hGame, "xrGameShutdown");

		if (pShutdownCallback)
		{
			pShutdownCallback();
		}

		Platform::FreeLibrary(hGame);
		hGame	= 0;
	}
	if (hRender)			{ Platform::FreeLibrary(hRender); hRender = 0; }
	if (hGameSpy)			{ Platform::FreeLibrary(hGameSpy); hGameSpy = 0; }

	pCreate					= 0;
	pDestroy				= 0;
	g_pEventManager->Event._destroy	();
}

void CEngineAPI::CreateRendererList()
{
	PROF_EVENT("CreateRendererList");
	if (g_dedicated_server)
	{
		vid_quality_token = xr_alloc<xr_token>(2);

		vid_quality_token[0].id = 0;
		vid_quality_token[0].name = xr_strdup("renderer_r1");

		vid_quality_token[1].id = -1;
		vid_quality_token[1].name = nullptr;
	} 
	else
	{
		if(vid_quality_token != nullptr) 
			return;

#ifdef IXR_WINDOWS
		const char* r4_name	= "xrRender_R4.dll";
#else
		const char* r4_name	= "libxrRender_R4.so";
#endif

		bool bSupports_r4 = Core.ParamsData.test(ECoreParams::perfhud_hack);
		if (!bSupports_r4)
		{
			auto dir = std::filesystem::weakly_canonical(Platform::GetBinaryFolderPath());
			bSupports_r4 = std::filesystem::exists(dir / r4_name);
		}

		hRender = 0;

		xr_vector<const char*> _tmp;
		if (bSupports_r4)
		{
			_tmp.push_back(xr_strdup("renderer_r4_static"));
			_tmp.push_back(xr_strdup("renderer_r4"));
		}

		u32 _cnt = (u32) _tmp.size() + 1;
		vid_quality_token = xr_alloc<xr_token>(_cnt);

		vid_quality_token[_cnt - 1].id = -1;
		vid_quality_token[_cnt - 1].name = nullptr;

#ifdef DEBUG
		Msg("Available render modes[%d]:",_tmp.size());
#endif // DEBUG
		for(u32 i=0; i<_tmp.size();++i)
		{
			vid_quality_token[i].id				= i;
			vid_quality_token[i].name			= _tmp[i];
#ifdef DEBUG
			Msg							("[%s]",_tmp[i]);
#endif // DEBUG
		}
	}
}

ERHI_API_LAYER CEngineAPI::GetAPI()
{
	return ERHI_API_LAYER::D3D11;
}

thread_local int SkinningMode = -1;

int CEngineAPI::GetSkinningMode() const
{
	return SkinningMode;
}

void CEngineAPI::SetSkinningMode(int Mode)
{
	SkinningMode = Mode;
}
