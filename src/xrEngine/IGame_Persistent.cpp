#include "stdafx.h"


#include "IGame_Persistent.h"
#include "../xrCore/IniReadAhead.h"

#include "Environment.h"
#include "x_ray.h"
#include "IGame_Level.h"
#include "XR_IOConsole.h"
#include "Render.h"
#include "CustomHUD.h"

ENGINE_API IGame_Persistent* g_pGamePersistent = nullptr;

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
	g_pGamePersistent->ixr_framework_onframe.reset();
	Environment().unload();
	OnGameEnd();

	DEL_INSTANCE(g_hud);
}


void IGame_Persistent::PreStart(const char* op)
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

void IGame_Persistent::Start(const char* op)
{
	string256						prev_type;
	xr_strcpy(prev_type, m_game_params.m_game_type);
	m_game_params.parse_cmd_line(op);
	// change game type
	if ((0 != xr_strcmp(prev_type, m_game_params.m_game_type)))
	{
		if (g_hud)
			DEL_INSTANCE(g_hud);

		if (*m_game_params.m_game_type)
			OnGameStart();
	}
	else
	{
		UpdateGameType();
	}
}

void IGame_Persistent::Disconnect()
{
	g_IniReadAhead->Stop();
	CInifile::ClearMemoryCache();
	VisorRain.Reset();
	ShaderParams = {};
	ShaderParams.HelmetCondition = -1.f;

	// clear "need to play" particles
	destroy_particles(true);

	if (g_hud)
		DEL_INSTANCE(g_hud);

	// Kill object - save memory
	ObjectPool.clear();
	Render->models_Clear(true);
}

bool g_MtIniPreload = true;

namespace
{
	const char* const DefaultIniReadAhead[] =
	{
		"$game_config$\\scripts\\*.ltx",
		"$game_config$\\misc\\*.ltx",
		"$game_config$\\script.ltx",
		"$game_config$\\game_levels.ltx",
		"$game_config$\\game_maps_single.ltx",
	};

	void AddIniReadAheadFiles(const char* Line, xr_vector<xr_string>& Paths)
	{
		const char* Slash = strchr(Line, '\\');
		if (Slash == nullptr)
		{
			return;
		}

		const xr_string Alias(Line, size_t(Slash - Line));
		const char* Rest = Slash + 1;
		const char* Star = strchr(Rest, '*');
		string_path FullPath;
		if (Star == nullptr)
		{
			FS.update_path(FullPath, Alias.c_str(), Rest);
			Paths.emplace_back(FullPath);
			return;
		}

		const xr_string Folder(Rest, size_t(Star - Rest));
		const char* Extension = strext(Star);
		xr_vector<char*>* List = FS.file_list_open(Alias.c_str(), Folder.c_str(), FS_ListFiles);
		if (List == nullptr)
		{
			return;
		}

		for (char* Name : *List)
		{
			const char* NameExtension = strext(Name);
			if (Extension != nullptr && (NameExtension == nullptr || 0 != _stricmp(NameExtension, Extension)))
			{
				continue;
			}

			const char* FileName = strrchr(Name, '\\');
			FileName = FileName ? FileName + 1 : Name;
			if (0 == _strnicmp(FileName, "mod_", 4))
			{
				continue;
			}

			const xr_string Relative = Folder + Name;
			FS.update_path(FullPath, Alias.c_str(), Relative.c_str());
			Paths.emplace_back(FullPath);
		}
		FS.file_list_close(List);
	}

	void StartIniReadAhead()
	{
		PROF_EVENT("StartIniReadAhead");
		xr_vector<xr_string> Paths;
		if (pSettings->section_exist("ini_read_ahead"))
		{
			for (const CInifile::Item& Item : pSettings->r_section("ini_read_ahead").Data)
			{
				AddIniReadAheadFiles(Item.first.c_str(), Paths);
			}
		}
		else
		{
			for (const char* Line : DefaultIniReadAhead)
			{
				AddIniReadAheadFiles(Line, Paths);
			}
		}
		g_IniReadAhead->Start(std::move(Paths));
	}
}

void IGame_Persistent::OnGameStart()
{
	loading_save_timer.Start();
	loading_save_timer_started = true;
	Msg("* Game Loading Timer: Started!");
	SetLoadStageTitle("st_prefetching_objects");
	LoadTitle();

	if (g_MtIniPreload)
	{
		StartIniReadAhead();
	}

	if (!Core.ParamsData.test(ECoreParams::noprefetch))
		Prefetch();
}

xr_string IGame_Persistent::GetPrefetchSection(const char* Prefix) const
{
	xr_string section = Prefix;
	section += m_game_params.m_game_type;

	if (!pSettings->section_exist(section.c_str()))
	{
		const xr_string Base = Prefix;
		if (section.compare(Base + "dm"))
			section = Base + "deathmatch";
		if (section.compare(Base + "tdm"))
			section = Base + "teamdeathmatch";
		if (section.compare(Base + "ah"))
			section = Base + "artefacthunt";
	}
	return section;
}

void IGame_Persistent::Prefetch()
{
	PROF_EVENT("Prefetch");
	{
		PROF_EVENT("Prefetch requests");
		for (const CInifile::Item& Item : pSettings->r_section(GetPrefetchSection("prefetch_objects_").c_str()).Data)
		{
			const char* Section = Item.first.c_str();
			if (pSettings->section_exist(Section) && pSettings->line_exist(Section, "visual"))
				Render->model_Request(pSettings->r_string(Section, "visual"), EModelRequestPriority::Normal);
		}

		for (const CInifile::Item& Item : pSettings->r_section(GetPrefetchSection("prefetch_visuals_").c_str()).Data)
			Render->model_Request(Item.first.c_str(), EModelRequestPriority::Low);
	}
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
}

void IGame_Persistent::OnGameEnd()
{
	g_IniReadAhead->Stop();
	CInifile::ClearMemoryCache();
	ObjectPool.clear();
	Render->models_Clear(true);
}

void IGame_Persistent::OnFrame()
{
	if (!Device.Paused() || Device.dwPrecacheFrame)
		Environment().OnFrame();
}

void IGame_Persistent::OnFrameMT()
{
	if (ixr_framework_onframe.is_valid())
	{
		ixr_framework_onframe();
	}
}

void IGame_Persistent::OnAssetsChanged()
{
	Device.m_pRender->OnAssetsChanged();
}
