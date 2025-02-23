#include "stdafx.h"
#include "AI/game_spawn_constructor.h"

bool EScene::BuildSpawn()
{
	if (m_cfrom_builder.empty())
	{
		if (!BuildCForm())
		{
			ELog.DlgMsg(mtError, mbOK, "CForm is empty!\Check log.");
			return false;
		}
	}
	if (m_level_graph.empty())
	{
		if (!BuildAIMap())
		{
			ELog.DlgMsg(mtError, mbOK, "AI-Map is empty!\Check log.");
			return false;
		}
	}
	if (m_game_graph.empty())
	{
		if (!BuildGameGraph())
		{
			ELog.DlgMsg(mtError, mbOK, "Game Graph is empty!\Check log.");
			return false;
		}
	}

	m_spawn_data.clear();
	CGameSpawnConstructor SpawnConstructor;
	if (!SpawnConstructor.build(Scene->m_LevelOp.m_FNLevelPath.c_str(), m_spawn_data, Scene->m_LevelOp.m_FNLevelPath.c_str(), true))
	{
		ELog.DlgMsg(mtError, mbOK, "! Failed build spawn! \nSee log.");
		return false;
	}

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
		ELog.DlgMsg(mtError, mbOK, "Level is not builded!");
		return false;
	}

	if (m_cfrom_builder.empty())
	{
		if (!BuildCForm())
		{
			ELog.DlgMsg(mtError, mbOK, "CForm is empty!\Check log.");
			return false;
		}
	}
	if (m_level_graph.empty())
	{
		if (!BuildAIMap())
		{
			ELog.DlgMsg(mtError, mbOK, "AI-Map is empty!\Check log.");
			return false;
		}
	}
	if (m_game_graph.empty())
	{
		if (!BuildGameGraph())
		{
			ELog.DlgMsg(mtError, mbOK, "Game Graph is empty!\Check log.");
			return false;
		}
	}

	if (!m_level_graph.save_temp())
	{
		ELog.DlgMsg(mtError, mbOK, "level.ai.temp can't be saved!\nSee log.");
		return false;
	}

	CGameSpawnConstructor SpawnConstructor;
	if (!SpawnConstructor.build(Scene->m_LevelOp.m_FNLevelPath.c_str(),"editor", Scene->m_LevelOp.m_FNLevelPath.c_str(), true))
	{
		ELog.DlgMsg(mtError, mbOK, "! Failed build spawn! \nSee log.");
		return false;
	}
	
	return true;
}