// EngineAPI.cpp: implementation of the CEngineAPI class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "EngineAPI.h"
#include "../xrcdb/xrXRC.h"

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

void CEngineAPI::InitializeNotDedicated()
{
	LPCSTR			r2_name	= "xrRender_R2.dll";
	LPCSTR			r4_name	= "xrRender_R4.dll";

	if (psDeviceFlags.test(rsR4)) {
		// try to initialize R4
		Msg("Loading DLL: %s",	r4_name);
		hRender			= LoadLibraryA		(r4_name);
		if (0==hRender) {
			// try to load R1
			Msg			("! ...Failed - incompatible hardware/pre-Vista OS.");
			psDeviceFlags.set	(rsR2,TRUE);
		}
	}

	if (psDeviceFlags.test(rsR2)) {
		// try to initialize R2
		psDeviceFlags.set	(rsR4,FALSE);
		Msg("Loading DLL: %s",	r2_name);
		hRender			= LoadLibraryA		(r2_name);
		if (0==hRender) {
			// try to load R1
			Msg			("! ...Failed - incompatible hardware.");
		} else {
			g_current_renderer = 2;
		}
	}
}

extern ENGINE_API bool g_dedicated_server;

void __cdecl Null_Factory_Destroy(DLL_Pure* O)
{
}

DLL_Pure* __cdecl Null_Factory_Create(CLASS_ID CLS_ID)
{
	return nullptr;
}

void CEngineAPI::Initialize(void)
{
	//////////////////////////////////////////////////////////////////////////
	// render
	LPCSTR			r1_name	= "xrRender_R1.dll";

	if (!g_dedicated_server)
		InitializeNotDedicated();

	if (0==hRender)		
	{
		// try to load R1
		psDeviceFlags.set	(rsR4,FALSE);
		psDeviceFlags.set	(rsR2,FALSE);
		renderer_value		= 0; //con cmd

		Msg("Loading DLL: %s",	r1_name);
		hRender			= LoadLibraryA		(r1_name);
		if (0==hRender)	R_CHK				(GetLastError());
		//R_ASSERT		(hRender);
		g_current_renderer	= 1;
	}

	Device.ConnectToRender();

	// game
	{
		LPCSTR			g_name	= "xrGame.dll";
		Msg("Loading DLL: %s",g_name);
		hGame			= LoadLibraryA	(g_name);
		if (0==hGame)	R_CHK			(GetLastError());
		R_ASSERT2		(hGame,"Game DLL raised exception during loading or there is no game DLL at all");

		if (hGame == nullptr)
		{
			pCreate = Null_Factory_Create;
			pDestroy = Null_Factory_Destroy;
		}
		else
		{
			pCreate = (Factory_Create*)GetProcAddress(hGame, "xrFactory_Create");		R_ASSERT(pCreate);
			pDestroy = (Factory_Destroy*)GetProcAddress(hGame, "xrFactory_Destroy");	R_ASSERT(pDestroy);
		}
	}

	hRenderRHI = LoadLibraryA("xrRenderInterface.dll");

	// GameSpy
	{
		LPCSTR g_name = "xrGameSpy.dll";
		hGameSpy = LoadLibraryA(g_name);

		if (hGameSpy != 0)
		{
			Msg("Found %s DLL! Enable MP subsystem!", g_name);
		}
	}
}

void CEngineAPI::Destroy(void)
{
	if (hGame)				{ FreeLibrary(hGame);	hGame	= 0; }
	if (hRender)			{ FreeLibrary(hRender); hRender = 0; }

	pCreate					= 0;
	pDestroy				= 0;
	g_pEventManager->Event._destroy	();
	XRC.r_clear_compact		();
}

void CEngineAPI::CreateRendererList()
{
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
		//	TODO: ask renderers if they are supported!
		if(vid_quality_token != nullptr) 
			return;
		
		bool bSupports_r1 = false;
		bool bSupports_r2 = false;
		bool bSupports_r4 = false;

		LPCSTR r1_name	= "xrRender_R1.dll";
		LPCSTR r2_name	= "xrRender_R2.dll";
		LPCSTR r4_name	= "xrRender_R4.dll";

		if (Core.ParamsData.test(ECoreParams::perfhud_hack))
		{
			bSupports_r1 = true;
			bSupports_r2 = true;
			bSupports_r4 = true;
		}
		else
		{
			char fullPath[MAX_PATH]{};
			GetModuleFileNameA(nullptr, fullPath, MAX_PATH);
			auto dir = std::filesystem::weakly_canonical(fullPath).parent_path();
			bSupports_r1 = std::filesystem::exists(dir / r1_name);
			bSupports_r2 = std::filesystem::exists(dir / r2_name);
			bSupports_r4 = std::filesystem::exists(dir / r4_name);
		}

		hRender = 0;

		xr_vector<LPCSTR> _tmp;
		if (bSupports_r1)
		{
			_tmp.push_back(xr_strdup("renderer_r1"));
		}
		if (bSupports_r2)
		{
			_tmp.push_back(xr_strdup("renderer_r2"));
		}
		if (bSupports_r4)
		{
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

APILevel CEngineAPI::GetAPI()
{
	if (psDeviceFlags.test(rsR4))
	{
		return APILevel::DX11;
	}

	return APILevel::DX9;
}