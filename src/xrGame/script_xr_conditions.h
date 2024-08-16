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

#define REGISTER_STORAGE_FOR_RETURN_TYPE(return_type_of_function) private: xr_hash_map<xr_string, CAnyCallable<return_type_of_function>> m_mStorageOf##return_type_of_function; private: void registerFunction(const xr_string& function_name, const CAnyCallable<return_type_of_function>& func) { m_mStorageOf##return_type_of_function[function_name] = func; } 

class CScriptXRConditionsStorage
{
public:
	CScriptXRConditionsStorage();
	~CScriptXRConditionsStorage();

	void initialize(CLevel* pLevelManager);
	void destroy();

	REGISTER_STORAGE_FOR_RETURN_TYPE(void);

private:
	CLevel* m_pLevel;
};