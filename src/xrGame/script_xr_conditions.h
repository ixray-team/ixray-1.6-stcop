////////////////////////////////////////////////////////////////////////////
//	Module 		: script_xr_conditions.h
//	Created 	: 16.08.2024
//  Modified 	: 16.08.2024
//	Author		: wh1t3lord
//	Description : Implementation of xr_conditions.script
////////////////////////////////////////////////////////////////////////////
#pragma once

class CLevel;

class CScriptXRConditionsStorage
{
public:
	CScriptXRConditionsStorage();
	~CScriptXRConditionsStorage();

	void initialize(CLevel* pLevelManager);
	void destroy();

private:
	CLevel* m_pLevel;
};