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

#define REGISTER_STORAGE_FOR_RETURN_TYPE(return_type_of_function) private: xr_hash_map<xr_string, CAnyCallable<return_type_of_function>> m_mStorageOf##return_type_of_function; private: void registerFunction(const xr_string& function_name, const CAnyCallable<return_type_of_function>& func) { m_mStorageOf##return_type_of_function[function_name] = func; } public: const CAnyCallable<return_type_of_function>& getRegisteredFunctionByName(const xr_string& function_name) { if (m_mStorageOf##return_type_of_function.find(function_name) != m_mStorageOf##return_type_of_function.end()) return m_mStorageOf##return_type_of_function.at(function_name); return CAnyCallable<return_type_of_function>([]()->return_type_of_function{return return_type_of_function{};}); }

class CScriptXRConditionsStorage
{
public:
	CScriptXRConditionsStorage();
	~CScriptXRConditionsStorage();

	void initialize(CLevel* pLevelManager);
	void destroy();

	REGISTER_STORAGE_FOR_RETURN_TYPE(bool);

private:
	CLevel* m_pLevel;
};