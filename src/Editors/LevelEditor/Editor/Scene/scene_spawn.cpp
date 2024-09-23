#include "stdafx.h"
#include "AI/game_spawn_constructor.h"

bool EScene::BuildSpawn()
{
	if (m_cfrom_builder.empty())
	{
		if (!BuildCForm())
		{
			ELog.DlgMsg(mtError, mbOK, g_pStringTable->translate("ed_st_cform_empty").c_str());
			return false;
		}
	}
	if (m_level_graph.empty())
	{
		if (!BuildAIMap())
		{
			ELog.DlgMsg(mtError, mbOK, g_pStringTable->translate("ed_st_ai_map_empty").c_str());
			return false;
		}
	}
	if (m_game_graph.empty())
	{
		if (!BuildGameGraph())
		{
			ELog.DlgMsg(mtError, mbOK, g_pStringTable->translate("ed_st_game_graph_empty").c_str());
			return false;
		}
	}
	UI->ShowConsole();
	m_spawn_data.clear();
	CGameSpawnConstructor SpawnConstructor;
	if (!SpawnConstructor.build(Scene->m_LevelOp.m_FNLevelPath.c_str(), m_spawn_data, Scene->m_LevelOp.m_FNLevelPath.c_str(), true))
	{
		ELog.DlgMsg(mtError, mbOK, g_pStringTable->translate("ed_st_spawn_failed").c_str());
		UI->CloseConsole();
		return false;
	}
	UI->CloseConsole();
	return true;
}


bool EScene::BuildForPCPlay()
{
	string_path FileName;
	xr_strcpy(FileName, Scene->m_LevelOp.m_FNLevelPath.c_str());

	string_path	N1, N2, N3, N4;
	xr_strconcat(N1, FileName, "\\level");
	xr_strconcat(N2, FileName, "\\level.ltx");
	xr_strconcat(N3, FileName, "\\level.geom");
	xr_strconcat(N4, FileName, "\\level.cform");
	if (
		!FS.exist("$game_levels$", N1) ||
		!FS.exist("$game_levels$", N2) ||
		!FS.exist("$game_levels$", N3) ||
		!FS.exist("$game_levels$", N4)
		)
	{
		ELog.DlgMsg(mtError, mbOK, g_pStringTable->translate("ed_st_level_build_failed").c_str());
		return false;
	}

	if (m_cfrom_builder.empty())
	{
		if (!BuildCForm())
		{
			ELog.DlgMsg(mtError, mbOK, g_pStringTable->translate("ed_st_cform_empty").c_str());
			return false;
		}
	}
	if (m_level_graph.empty())
	{
		if (!BuildAIMap())
		{
			ELog.DlgMsg(mtError, mbOK, g_pStringTable->translate("ed_st_ai_map_empty").c_str());
			return false;
		}
	}
	if (m_game_graph.empty())
	{
		if (!BuildGameGraph())
		{
			ELog.DlgMsg(mtError, mbOK, g_pStringTable->translate("ed_st_game_graph_empty").c_str());
			return false;
		}
	}
	UI->ShowConsole();
	if (!m_level_graph.save_temp())
	{
		ELog.DlgMsg(mtError, mbOK, g_pStringTable->translate("ed_st_level_ai_temp_cant_save").c_str());
		return false;
	}

	CGameSpawnConstructor SpawnConstructor;
	if (!SpawnConstructor.build(Scene->m_LevelOp.m_FNLevelPath.c_str(),"editor", Scene->m_LevelOp.m_FNLevelPath.c_str(), true))
	{
		ELog.DlgMsg(mtError, mbOK, g_pStringTable->translate("ed_st_spawn_failed").c_str());
		UI->CloseConsole();
		return false;
	}
	UI->CloseConsole();
	
	return true;
}