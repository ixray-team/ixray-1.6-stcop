#include "Stdafx.h"
#include "script_xr_conditions.h"

CScriptXRConditionsStorage::CScriptXRConditionsStorage() : m_pLevel{}
{
}

CScriptXRConditionsStorage::~CScriptXRConditionsStorage()
{
}

void CScriptXRConditionsStorage::initialize(CLevel* pLevelManager)
{
	assert(pLevelManager && "you have to have a valid pointer of LevelManager! early calling?");

	m_pLevel = pLevelManager;
}

void CScriptXRConditionsStorage::destroy()
{
}
