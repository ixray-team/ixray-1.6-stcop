/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "stdafx.h"
#include "../../xrEngine/Environment.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Rain.h"

RenderFrame RFrame;

void RenderFrame::Sync()
{
	vCameraPosition = Device.vCameraPosition;
	vCameraDirection = Device.vCameraDirection;
	vCameraTop = Device.vCameraTop;
	vCameraRight = Device.vCameraRight;
	mView = Device.mView;
	mProject = Device.mProject;
	mFullTransform = Device.mFullTransform;
	fFOV = Device.fFOV;
	fASPECT = Device.fASPECT;
	fViewportNear = Device.fViewportNear;
	mView_old = Device.mView_old;
	mProject_old = Device.mProject_old;
	mFullTransform_old = Device.mFullTransform_old;
	mView_hud = Device.mView_hud;
	mProject_hud = Device.mProject_hud;
	mFullTransform_hud = Device.mFullTransform_hud;
	mView_hud_old = Device.mView_hud_old;
	mProject_hud_old = Device.mProject_hud_old;
	mFullTransform_hud_old = Device.mFullTransform_hud_old;
	mView_saved = Device.mView_saved;
	mProject_saved = Device.mProject_saved;
	mFullTransform_saved = Device.mFullTransform_saved;
	vCameraPosition_saved = Device.vCameraPosition_saved;
	vCameraDirection_saved = Device.vCameraDirection_saved;
	vCameraRight_saved = Device.vCameraRight_saved;
	vCameraTop_saved = Device.vCameraTop_saved;
	IsCut = Device.IsCameraCut();
	dwFrame = Device.dwFrame;
	fTimeGlobal = Device.fTimeGlobal;
	fTimeDelta = Device.fTimeDelta;
	dwTimeGlobal = Device.dwTimeGlobal;
	dwTimeDelta = Device.dwTimeDelta;
	dwTimeContinual = Device.dwTimeContinual;
	dwPrecacheFrame = Device.dwPrecacheFrame;
}

static const CEnvDescriptorMixer* LiveWeather()
{
	return g_pGamePersistent && g_pGamePersistent->pEnvironment ? g_pGamePersistent->pEnvironment->CurrentEnv : nullptr;
}

bool RenderFrame::HasEnv() const
{
	return Weather || LiveWeather();
}

const CEnvDescriptorMixer& RenderFrame::Env() const
{
	return Weather ? *Weather : *LiveWeather();
}

void RenderFrame::SyncWeather()
{
	const CEnvDescriptorMixer* Live = LiveWeather();

	if (!Live)
	{
		return;
	}

	if (!Weather)
	{
		Weather = new CEnvDescriptorMixer("render_frame");
	}

	*Weather = *Live;
	const CEnvironment& Environment = *g_pGamePersistent->pEnvironment;
	Wetness = Environment.wetness_factor;
	WindStrength = Environment.wind_strength_factor;
	WindBlast = Environment.wind_blast_direction;
	HudRainTime = Environment.eff_Rain ? Environment.eff_Rain->HudDropsTime() : 0.f;
	HudRainAmount = Environment.eff_Rain ? Environment.eff_Rain->HudDropsAmount() : 0.f;
}

// The copy holds the sky's textures: it goes before the resources do
void RenderFrame::Release()
{
	xr_delete(Weather);
}
