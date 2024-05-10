#include "stdafx.h"
#include "../AI/game_spawn_constructor.h"

bool EScene::BuildSpawn()
{
	if (m_cfrom_builder.empty())
	{
		if (!BuildCForm())
		{
			ELog.DlgMsg(mtError, mbOK, "CFrom is empty!\nSee log.");
			return false;
		}
	}
	if (m_level_graph.empty())
	{
		if (!BuildAIMap())
		{
			ELog.DlgMsg(mtError, mbOK, "AIMap is empty!\nSee log.");
			return false;
		}
	}
	if (m_game_graph.empty())
	{
		if (!BuildGameGraph())
		{
			ELog.DlgMsg(mtError, mbOK, "GameGraph is empty!\nSee log.");
			return false;
		}
	}
	UI->ShowConsole();
	m_spawn_data.clear();
	CGameSpawnConstructor SpawnConstructor;
	if (!SpawnConstructor.build(Scene->m_LevelOp.m_FNLevelPath.c_str(), m_spawn_data, Scene->m_LevelOp.m_FNLevelPath.c_str(), true))
	{
		ELog.DlgMsg(mtError, mbOK, "! Failed build spawn! \nSee log.");
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
	strconcat(sizeof(N1), N1, FileName, "\\level");
	strconcat(sizeof(N2), N2, FileName, "\\level.ltx");
	strconcat(sizeof(N3), N3, FileName, "\\level.geom");
	strconcat(sizeof(N4), N4, FileName, "\\level.cform");
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
			ELog.DlgMsg(mtError, mbOK, "CFrom is empty!\nSee log.");
			return false;
		}
	}
	if (m_level_graph.empty())
	{
		if (!BuildAIMap())
		{
			ELog.DlgMsg(mtError, mbOK, "AIMap is empty!\nSee log.");
			return false;
		}
	}
	if (m_game_graph.empty())
	{
		if (!BuildGameGraph())
		{
			ELog.DlgMsg(mtError, mbOK, "GameGraph is empty!\nSee log.");
			return false;
		}
	}
	UI->ShowConsole();
	if (!m_level_graph.save_temp())
	{
		ELog.DlgMsg(mtError, mbOK, "level.ai.temp can't save!\nSee log.");
		return false;
	}

	CGameSpawnConstructor SpawnConstructor;
	if (!SpawnConstructor.build(Scene->m_LevelOp.m_FNLevelPath.c_str(),"editor", Scene->m_LevelOp.m_FNLevelPath.c_str(), true))
	{
		ELog.DlgMsg(mtError, mbOK, "! Failed build spawn! \nSee log.");
		UI->CloseConsole();
		return false;
	}
	UI->CloseConsole();
	
	return true;
}