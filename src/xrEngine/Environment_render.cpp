#include "stdafx.h"
#pragma hdrstop

#include "Environment.h"
#ifndef _EDITOR
#include "Render.h"
#endif
#include "xr_efflensflare.h"
#include "Rain.h"
#include "thunderbolt.h"

#ifndef _EDITOR
#	include "igame_level.h"
#endif

//-----------------------------------------------------------------------------
// Environment render
//-----------------------------------------------------------------------------
extern ENGINE_API float psHUD_FOV;
//BOOL bNeed_re_create_env = FALSE;
void CEnvironment::RenderSky		()
{
	if (0==g_pGameLevel&&!Device.IsEditorMode())
		return;

	m_pRender->RenderSky(*this);
}

void CEnvironment::RenderClouds()
{
	if (0 == g_pGameLevel && !Device.IsEditorMode())
		return;

	// draw clouds
	if (fis_zero(CurrentEnv->clouds_color.w, EPS_L))
		return;

	m_pRender->RenderClouds(*this);
}

void CEnvironment::RenderFlares()
{
	if (0 == g_pGameLevel && !Device.IsEditorMode())
		return;

	// 1
	eff_LensFlare->Render(FALSE, TRUE, TRUE);
}

void CEnvironment::RenderLast()
{
	if (0 == g_pGameLevel && !Device.IsEditorMode())
		return;

	// 2
	eff_Rain->Render();
	eff_Thunderbolt->Render();
}

void CEnvironment::OnDeviceCreate()
{
	m_pRender->OnDeviceCreate();

	// weathers
	{
		EnvsMapIt _I, _E;
		_I = WeatherCycles.begin();
		_E = WeatherCycles.end();
		for (; _I != _E; _I++)
			for (EnvIt it = _I->second.begin(); it != _I->second.end(); it++)
				(*it)->on_device_create();
	}
	// effects
	{
		EnvsMapIt _I, _E;
		_I = WeatherFXs.begin();
		_E = WeatherFXs.end();
		for (; _I != _E; _I++)
			for (EnvIt it = _I->second.begin(); it != _I->second.end(); it++)
				(*it)->on_device_create();
	}


	Invalidate();
	OnFrame();
}

void CEnvironment::OnDeviceDestroy()
{
	m_pRender->OnDeviceDestroy();

	// weathers
	{
		EnvsMapIt _I, _E;
		_I = WeatherCycles.begin();
		_E = WeatherCycles.end();
		for (; _I != _E; _I++)
			for (EnvIt it = _I->second.begin(); it != _I->second.end(); it++)
				(*it)->on_device_destroy();
	}
	// effects
	{
		EnvsMapIt _I, _E;
		_I = WeatherFXs.begin();
		_E = WeatherFXs.end();
		for (; _I != _E; _I++)
			for (EnvIt it = _I->second.begin(); it != _I->second.end(); it++)
				(*it)->on_device_destroy();
	}

	if (CurrentEnv != nullptr)
		CurrentEnv->destroy();
}

void CEnvironment::ED_Reload()
{
	OnDeviceDestroy();
	OnDeviceCreate();
}