#include "Stdafx.h"
#include "script_xr_conditions.h"
#include "script_xr_conditions_functions.cpp"

// set breakpoint on these functions for 'understanding' the sense of usage and the purpose of CAnyCallable
//bool test_adiahfuirhgarughargha(int a, int b) { return false; }
//bool test2_Afjreauhgaeughauegaoegheog(int a, int b, const xr_vector<int>& c) { return false; }

CScriptXRConditionsStorage::CScriptXRConditionsStorage() : m_pLevel{}
{
	/* for test
	REGISTER_FUNCTION_TO_XR_CONDITIONS(test_adiahfuirhgarughargha);
	REGISTER_FUNCTION_TO_XR_CONDITIONS(test2_Afjreauhgaeughauegaoegheog);

	this->getRegisteredFunctionByName("test_adiahfuirhgarughargha")(1,2);
	this->getRegisteredFunctionByName("test2_Afjreauhgaeughauegaoegheog")(1, 2, xr_vector<int>{});
	*/
}

CScriptXRConditionsStorage::~CScriptXRConditionsStorage()
{
}

void CScriptXRConditionsStorage::initialize(CLevel* pLevelManager)
{
	R_ASSERT2(pLevelManager, "you have to have a valid pointer of LevelManager! early calling?");

	m_pLevel = pLevelManager;

	REGISTER_FUNCTION_TO_SCRIPT(is_fighting_dist_ge);
}

void CScriptXRConditionsStorage::destroy()
{
}
