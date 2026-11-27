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
#pragma once

// The camera the renderer draws a frame with: a copy of the device's, taken when the frame begins and when a view of
// it is calculated. The renderer reads this and never the device's own, which the game goes on writing; what is
// drawn with a camera of its own (the HUD, an icon, the wallmarks) changes this copy for as long as it draws.
// The time and the frame counter are copied with it; code that runs in the game's frame or the pre-render stage
// still reads the device's: the render thread plan's stage 6
class CEnvDescriptorMixer;

struct RenderFrame
{
	Fvector vCameraPosition;
	Fvector vCameraDirection;
	Fvector vCameraTop;
	Fvector vCameraRight;
	Fvector vCameraPosition_saved;
	Fvector vCameraDirection_saved;
	Fvector vCameraRight_saved;
	Fvector vCameraTop_saved;
	Fmatrix mView;
	Fmatrix mProject;
	Fmatrix mFullTransform;
	Fmatrix mView_old;
	Fmatrix mProject_old;
	Fmatrix mFullTransform_old;
	Fmatrix mView_hud;
	Fmatrix mProject_hud;
	Fmatrix mFullTransform_hud;
	Fmatrix mView_hud_old;
	Fmatrix mProject_hud_old;
	Fmatrix mFullTransform_hud_old;
	Fmatrix mView_saved;
	Fmatrix mProject_saved;
	Fmatrix mFullTransform_saved;
	float fFOV = 0.0f;
	float fASPECT = 0.0f;
	float fViewportNear = 0.0f;
	bool IsCut = false;
	u32 dwFrame = 0;
	float fTimeGlobal = 0.0f;
	float fTimeDelta = 0.0f;
	u32 dwTimeGlobal = 0;
	u32 dwTimeDelta = 0;
	u32 dwTimeContinual = 0;
	u32 dwPrecacheFrame = 0;

	// The weather as the game had blended it when the frame began (SyncWeather): the renderer reads this copy and never
	// the environment's own mixer, which the game's frame blends on. Before the first frame it is the environment's
	float Wetness = 0.0f;
	float WindStrength = 0.0f;
	Fvector WindBlast = {};
	float HudRainTime = 0.0f;
	float HudRainAmount = 0.0f;
	CEnvDescriptorMixer* Weather = nullptr;

	bool IsCameraCut() const { return IsCut; }
	bool HasEnv() const;
	const CEnvDescriptorMixer& Env() const;
	void Sync();
	void SyncWeather();
	void Release();
};

extern RenderFrame RFrame;
