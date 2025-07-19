////////////////////////////////////////////////////////////////////////////
//	Module 		: script_xr_conditions.h
//	Created 	: 16.08.2024
//  Modified 	: 16.08.2024
//	Author		: wh1t3lord
//	Description : Implementation of xr_conditions.script
////////////////////////////////////////////////////////////////////////////
#pragma once


#include "script_utility.h"

class CLevel;

class CScriptXRConditionsStorage
{
public:
	CScriptXRConditionsStorage();
	~CScriptXRConditionsStorage();

	void initialize(CLevel* pLevelManager);
	void destroy();

	IXRAY_LUA_TO_CPP_REGISTER_STORAGE_FOR_RETURN_TYPE(bool);

private:
	CLevel* m_pLevel;
};