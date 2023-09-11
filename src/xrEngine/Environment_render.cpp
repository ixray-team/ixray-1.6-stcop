#include "stdafx.h"
#pragma hdrstop

#include "Environment.h"
#ifndef _EDITOR
    #include "render.h"
#endif
#include "xr_efflensflare.h"
#include "rain.h"
#include "thunderbolt.h"

#ifndef _EDITOR
#	include "igame_level.h"
#endif
/*
//////////////////////////////////////////////////////////////////////////
// half box def
static	Fvector3	hbox_verts[24]	=
{
	{-1.f,	-1.f,	-1.f}, {-1.f,	-1.01f,	-1.f},	// down
	{ 1.f,	-1.f,	-1.f}, { 1.f,	-1.01f,	-1.f},	// down
	{-1.f,	-1.f,	 1.f}, {-1.f,	-1.01f,	 1.f},	// down
	{ 1.f,	-1.f,	 1.f}, { 1.f,	-1.01f,	 1.f},	// down
	{-1.f,	 1.f,	-1.f}, {-1.f,	 1.f,	-1.f},
	{ 1.f,	 1.f,	-1.f}, { 1.f,	 1.f,	-1.f},
	{-1.f,	 1.f,	 1.f}, {-1.f,	 1.f,	 1.f},
	{ 1.f,	 1.f,	 1.f}, { 1.f,	 1.f,	 1.f},
	{-1.f,	 0.f,	-1.f}, {-1.f,	-1.f,	-1.f},	// half
	{ 1.f,	 0.f,	-1.f}, { 1.f,	-1.f,	-1.f},	// half
	{ 1.f,	 0.f,	 1.f}, { 1.f,	-1.f,	 1.f},	// half
	{-1.f,	 0.f,	 1.f}, {-1.f,	-1.f,	 1.f}	// half
};
static	u16			hbox_faces[20*3]	=
{
	0,	 2,	 3,
	3,	 1,	 0,
	4,	 5,	 7,
	7,	 6,	 4,
	0,	 1,	 9,
	9,	 8,	 0,
	8,	 9,	 5,
	5,	 4,	 8,
	1,	 3,	10,
	10,	 9,	 1,
	9,	10,	 7,
	7,	 5,	 9,
	3,	 2,	11,
	11,	10,	 3,
	10,	11,	 6,
	6,	 7,	10,
	2,	 0,	 8,
	8,	11,	 2,
	11,	 8,	 4,
	4,	 6,	11
};

#pragma pack(push,1)
struct v_skybox				{
	Fvector3	p;
	u32			color;
	Fvector3	uv	[2];

	void		set			(Fvector3& _p, u32 _c, Fvector3& _tc)
	{
		p					= _p;
		color				= _c;
		uv[0]				= _tc;
		uv[1]				= _tc;
	}
};
const	u32 v_skybox_fvf	= D3DFVF_XYZ | D3DFVF_DIFFUSE | D3DFVF_TEX2 | D3DFVF_TEXCOORDSIZE3(0) | D3DFVF_TEXCOORDSIZE3(1);
struct v_clouds				{
	Fvector3	p;
	u32			color;
	u32			intensity;
	void		set			(Fvector3& _p, u32 _c, u32 _i)
	{
		p					= _p;
		color				= _c;
		intensity			= _i;
	}
};
const	u32 v_clouds_fvf	= D3DFVF_XYZ | D3DFVF_DIFFUSE | D3DFVF_SPECULAR;
#pragma pack(pop)
*/

//-----------------------------------------------------------------------------
// Environment render
//-----------------------------------------------------------------------------
extern ENGINE_API float psHUD_FOV;
//BOOL bNeed_re_create_env = FALSE;
void CEnvironment::RenderSky		()
{
#ifndef _EDITOR
	if (0==g_pGameLevel)		return;
#endif

	m_pRender->RenderSky(*this);
}

void CEnvironment::RenderClouds			()
{
#ifndef _EDITOR
	if (0==g_pGameLevel)		return	;
#endif
	if (fis_zero(CurrentEnv->clouds_color.w,EPS_L))	return;
	m_pRender->RenderClouds(*this);
}

void CEnvironment::RenderFlares		()
{
#ifndef _EDITOR
	if (0==g_pGameLevel)			return	;
#endif
	// 1
	eff_LensFlare->Render			(FALSE,TRUE,TRUE);
}

void CEnvironment::RenderLast		()
{
#ifndef _EDITOR
	if (0==g_pGameLevel)			return	;
#endif
	// 2
	eff_Rain->Render				();
	eff_Thunderbolt->Render			();
}

void CEnvironment::OnDeviceCreate()
{
	m_pRender->OnDeviceCreate();

	// weathers
	{
		EnvsMapIt _I,_E;
		_I		= WeatherCycles.begin();
		_E		= WeatherCycles.end();
		for (; _I!=_E; _I++)
			for (EnvIt it=_I->second.begin(); it!=_I->second.end(); it++)
				(*it)->on_device_create();
	}
	// effects
	{
		EnvsMapIt _I,_E;
		_I		= WeatherFXs.begin();
		_E		= WeatherFXs.end();
		for (; _I!=_E; _I++)
			for (EnvIt it=_I->second.begin(); it!=_I->second.end(); it++)
				(*it)->on_device_create();
	}


	Invalidate	();
	OnFrame		();
}

void CEnvironment::OnDeviceDestroy()
{
	m_pRender->OnDeviceDestroy();

	// weathers
	{
		EnvsMapIt _I,_E;
		_I		= WeatherCycles.begin();
		_E		= WeatherCycles.end();
		for (; _I!=_E; _I++)
			for (EnvIt it=_I->second.begin(); it!=_I->second.end(); it++)
				(*it)->on_device_destroy();
	}
	// effects
	{
		EnvsMapIt _I,_E;
		_I		= WeatherFXs.begin();
		_E		= WeatherFXs.end();
		for (; _I!=_E; _I++)
			for (EnvIt it=_I->second.begin(); it!=_I->second.end(); it++)
				(*it)->on_device_destroy();
	}
	CurrentEnv->destroy();

}

#ifdef _EDITOR
void CEnvironment::ED_Reload()
{
	OnDeviceDestroy			();
	OnDeviceCreate			();
}
#endif

