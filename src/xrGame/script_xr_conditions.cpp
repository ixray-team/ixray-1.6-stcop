#include "Stdafx.h"
#include "script_xr_conditions.h"
#include "script_xr_conditions_functions.cpp"

#define REGISTER_FUNCTION_TO_XR_CONDITIONS(myfunction) this->registerFunction(#myfunction, CAnyCallable<std::function<decltype(myfunction)>::result_type>(myfunction)); 

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
