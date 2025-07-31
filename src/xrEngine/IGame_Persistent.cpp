#include "stdafx.h"
#pragma hdrstop

#include "IGame_Persistent.h"

#include "Environment.h"
#include "x_ray.h"
#include "IGame_Level.h"
#include "XR_IOConsole.h"
#include "Render.h"
#include "PS_instance.h"
#include "CustomHUD.h"

ENGINE_API	IGame_Persistent* g_pGamePersistent = nullptr;

bool IsMainMenuActive() { return  g_pGamePersistent && g_pGamePersistent->m_pMainMenu && g_pGamePersistent->m_pMainMenu->IsActive(); }

IGame_Persistent::IGame_Persistent	()
{
	RDEVICE.seqAppStart.Add			(this);
	RDEVICE.seqAppEnd.Add			(this);
	RDEVICE.seqFrame.Add			(this,REG_PRIORITY_HIGH+1);
	RDEVICE.seqAppActivate.Add		(this);
	RDEVICE.seqAppDeactivate.Add	(this);

	m_pMainMenu						= nullptr;

	pEnvironment					= new CEnvironment();
}

IGame_Persistent::~IGame_Persistent	()
{
	RDEVICE.seqFrame.Remove			(this);
	RDEVICE.seqAppStart.Remove		(this);
	RDEVICE.seqAppEnd.Remove			(this);
	RDEVICE.seqAppActivate.Remove	(this);
	RDEVICE.seqAppDeactivate.Remove	(this);

	xr_delete						(pEnvironment);
}

void IGame_Persistent::OnAppActivate()
{
}

void IGame_Persistent::OnAppDeactivate()
{
}

void IGame_Persistent::OnAppStart()
{
	Environment().load();
}

void IGame_Persistent::OnAppEnd()
{
	Environment().unload();
	OnGameEnd();

	DEL_INSTANCE(g_hud);
}


void IGame_Persistent::PreStart(LPCSTR op)
{
	string256 prev_type;
	params new_game_params;
	xr_strcpy(prev_type, m_game_params.m_game_type);
	new_game_params.parse_cmd_line(op);

	// change game type
	if (0 != xr_strcmp(prev_type, new_game_params.m_game_type))
	{
		OnGameEnd();
	}
}

void IGame_Persistent::Start(LPCSTR op)
{
	string256						prev_type;
	xr_strcpy(prev_type, m_game_params.m_game_type);
	m_game_params.parse_cmd_line(op);
	// change game type
	if ((0 != xr_strcmp(prev_type, m_game_params.m_game_type)))
	{
		if (*m_game_params.m_game_type)
			OnGameStart();

		if (g_hud)
			DEL_INSTANCE(g_hud);
	}
	else
	{
		UpdateGameType();
	}
}

void IGame_Persistent::Disconnect()
{
	// clear "need to play" particles
	destroy_particles(true);

	if (g_hud)
		DEL_INSTANCE(g_hud);

	// Kill object - save memory
	ObjectPool.clear();
	Render->models_Clear(TRUE);
}

void IGame_Persistent::OnGameStart()
{
	loading_save_timer.Start();
	loading_save_timer_started = true;
	Msg("* Game Loading Timer: Started!");
	SetLoadStageTitle("st_prefetching_objects");
	LoadTitle();

	if (!Core.ParamsData.test(ECoreParams::noprefetch))
		Prefetch();
}

void IGame_Persistent::Prefetch()
{
	PROF_EVENT("Prefetch");
	{
		// prefetch game objects & models
		PROF_EVENT("Loading objects");
		Log("Loading objects...");
		ObjectPool.prefetch();
	}
	{
		PROF_EVENT("Prefetch Loading models");
		Log("Loading models...");
		Render->models_Prefetch();
	}
	{
		PROF_EVENT("Loading textures");
		Log("Loading textures...");
		Device.m_pRender->ResourcesDeferredUpload();
	}
}

void IGame_Persistent::OnGameEnd()
{
	ObjectPool.clear();
	Render->models_Clear(TRUE);
}

void IGame_Persistent::OnFrame()
{
	if (!Device.Paused() || Device.dwPrecacheFrame)
		Environment().OnFrame();
}

void IGame_Persistent::UpdateParticles()
{
	// Play req particle systems
	while (!ps_needtoplay.empty())
	{
		xr_shared_ptr<CPS_Instance> pInstance = ps_needtoplay.back();
		ps_needtoplay.pop_back();
		pInstance->Play(false);
	}

	if (!ps_active_deffer.empty())
	{
		ps_active.reserve(ps_active.size() + ps_active_deffer.size());

		for (xr_shared_ptr<CPS_Instance>& Part : ps_active_deffer)
		{
			ps_active.push_back(Part);
		}
		ps_active_deffer.clear();
	}

	ps_active.erase(std::remove_if
	(
		ps_active.begin(), ps_active.end(),
		[](const xr_shared_ptr<CPS_Instance>& Obj)->bool
		{
			return Obj->m_NeedDestroy;
		}
	), ps_active.end());
}

void IGame_Persistent::destroy_particles(const bool& all_particles)
{
	ps_needtoplay.clear();

	// delete active particles
	if (all_particles)
	{
		ps_active.clear();
	}
	else
	{
		ps_active.erase(std::remove_if
		(
			ps_active.begin(), ps_active.end(),
			[](const xr_shared_ptr<CPS_Instance>& Obj)->bool
			{
				return Obj->destroy_on_game_load();
			}
		), ps_active.end());
	}

	VERIFY(ps_needtoplay.empty() && (!all_particles || ps_active.empty()));
}

void IGame_Persistent::OnAssetsChanged()
{
	Device.m_pRender->OnAssetsChanged();
}