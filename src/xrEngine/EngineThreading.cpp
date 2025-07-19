#include "stdafx.h"
#include "EngineThreading.h"
#include "CustomHUD.h"
#include "IGame_Persistent.h"
#include "IGame_Level.h"
#include "Rain.h"
#include "../../xrCore/Collision/ISpatial.h"
#include "../../xrCore/Collision/Frustum.h"
#include "../xrCore/discord/discord.h"
#include "Render.h"
#include "Irenderable.h"
#include "../../Include/xrRender/Kinematics.h"

void XRay::Engine::PreRenderThread()
{
	Platform::SetThreadName("X-Ray Pre-Render");
	PROF_THREAD("X-Ray Pre-Render");
	{
		PROF_EVENT("Discord Sync");
		g_Discord.Update();
	}

	{
		PROF_EVENT("seqParallelRender");
		for (auto& it : Device.seqParallelRender)
			it();
	}

	if (g_pGamePersistent && g_pGamePersistent->pEnvironment && g_pGamePersistent->pEnvironment->eff_Rain)
	{
		g_pGamePersistent->pEnvironment->eff_Rain->UpdateItems();
	}

	if (Device.ParticleWorkerCallback)
	{
		PROF_EVENT("Process Particles");
		Device.ParticleWorkerCallback();
	}

	Platform::SetThreadName("X-Ray Empty Task");
}

void XRay::Engine::CalculateBonesThread()
{
	Platform::SetThreadName("X-Ray CalculateBones");
	PROF_THREAD("Secondary Task 3");

	PROF_EVENT("CalculateBones");

	if (!g_SpatialSpace) return;
	if (Device.Paused()) return;
	if (!psDeviceFlags.test(rsDrawDynamic)) return;
	if (!g_pGameLevel || !g_pGameLevel->bReady) return;
	if (!g_pGameLevel->CurrentEntity()) return;

	static CFrustum ViewBase;
	ViewBase.CreateFromMatrix(Device.mFullTransform_saved, FRUSTUM_P_LRTB | FRUSTUM_P_FAR);
	Fvector& cam_pos = Device.vCameraPosition_saved;
	static xr_vector<ISpatialShared> spatials = {};
	g_SpatialSpace->q_sphere(spatials, ISpatial_DB::O_ORDERED, STYPE_RENDERABLE + STYPE_RENDERABLESHADOW + STYPE_PARTICLE + STYPE_LIGHTSOURCE, cam_pos, g_pGamePersistent->Environment().CurrentEnv->fog_distance);
	spatials.erase(std::remove_if(spatials.begin(), spatials.end(), [&cam_pos](ISpatialShared& S)
	{
		ISpatial* spatial = S.get();
		if (!spatial) return true;
		if (!ViewBase.testSphere_dirty(spatial->spatial.sphere.P, spatial->spatial.sphere.R))
		{
			if (cam_pos.distance_to_sqr(spatial->spatial.sphere.P) > 62500.f)//250 m
				return true;
		}

		spatial->spatial_updatesector();

		return false;
	}), spatials.end());

	std::sort(spatials.begin(), spatials.end(), [&cam_pos](ISpatialShared& _1, ISpatialShared& _2)
	{
		if (!_1.get() || !_2.get()) return false;

		return _1->spatial.sphere.P.distance_to_sqr(cam_pos) < _2->spatial.sphere.P.distance_to_sqr(cam_pos);
	});

	for (ISpatialShared& SSH : spatials)
	{
		ISpatial* spatial = SSH.get();
		if (!spatial) continue;

		if ((spatial->spatial.type & STYPE_RENDERABLE) || (spatial->spatial.type & STYPE_RENDERABLESHADOW))
		{
			if (IRenderable* renderable = spatial->dcast_Renderable())
			{
				if (!renderable->renderable.visual) continue;
				if (!renderable->renderable.visual->dcast_PKinematics()) continue;

				if (IKinematics* pKin = renderable->renderable.visual->dcast_PKinematics())
				{
					pKin->CalculateBones(TRUE);
				}
			}
		}
	}
	Platform::SetThreadName("X-Ray Empty Task");
}

void XRay::Engine::GameThread()
{
	Platform::SetThreadName("X-Ray Game Thread");

	PROF_THREAD("X-Ray Game Thread")
		// we has granted permission to execute
	{
		PROF_EVENT("g_hud OnFrameMT")
			if (g_hud)
				g_hud->OnFrameMT();
	}

	{
		PROF_EVENT("SoundEvent_Dispatch")
			if (g_pGameLevel && g_pGameLevel->bReady)
				g_pGameLevel->SoundEvent_Dispatch();
	}

	{
		PROF_EVENT("Sheduler")
			if (!Device.Paused())
				::Engine.Sheduler.Update();
	}

	{
		PROF_EVENT("seqParallel")
			for (u32 pit = 0; pit < Device.seqParallel.size(); pit++)
				Device.seqParallel[pit]();
		Device.seqParallel.resize(0);
	}

	{
		PROF_EVENT("seqFrameMT")
			Device.seqFrameMT.Process(rp_Frame);
	}

	Platform::SetThreadName("X-Ray Empty Task");
}