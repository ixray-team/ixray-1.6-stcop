#include "StdAfx.h"
#include "script_xr_logic.h"
#include "script_xr_conditions.h"
#include "script_xr_effects.h"

CScriptXRLogicManager::CScriptXRLogicManager() :
	m_pLevel{}, m_pXRConditions{}, m_pXREffects{}
{
}

CScriptXRLogicManager::~CScriptXRLogicManager() {}

void CScriptXRLogicManager::initialize(CLevel* pLevelManager, CScriptXRConditionsStorage* pStorageXRConditions, CScriptXREffectsStorage* pStorageXREffects)
{
	m_pLevel = pLevelManager;
	m_pXRConditions = pStorageXRConditions;
	m_pXREffects = pStorageXREffects;
}

void CScriptXRLogicManager::destroy() 
{
	m_pLevel = nullptr;
	m_pXRConditions = nullptr;
	m_pXREffects = nullptr;
}
