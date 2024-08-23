#include "StdAfx.h"
#include "script_xr_logic.h"

CScriptXRLogicManager::CScriptXRLogicManager() : m_pLevel{} {}

CScriptXRLogicManager::~CScriptXRLogicManager() {}

void CScriptXRLogicManager::initialize(CLevel* pLevelManager) 
{
	m_pLevel = pLevelManager;
}

void CScriptXRLogicManager::destroy() {}
