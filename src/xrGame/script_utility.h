////////////////////////////////////////////////////////////////////////////
//	Module 		: script_utility.h
//	Created 	: 16.08.2024
//  Modified 	: 16.08.2024
//	Author		: wh1t3lord
//	Description : Implementation of condlist and other stuff based on xr_logic.script
////////////////////////////////////////////////////////////////////////////

#pragma once

// when you use this you need to provide two versions of your function, one is client that means it accepts only CScriptGameObject* instances and server version that accepts a server instance
// when it is called it binds client and server version of  your function
// myfunction = main word without postfix
#define REGISTER_FUNCTION_TO_SCRIPT(myfunction) this->registerClientFunction(#myfunction, CAnyCallable<std::function<decltype(##myfunction##_client)>::result_type>(##myfunction##_client)); this->registerServerFunction(#myfunction, CAnyCallable<std::function<decltype(##myfunction##_server)>::result_type>(##myfunction##_server)); 

// declares two hash maps for two version of function client and server;
// also implements all methods for accessing these methods:
//	getRegisteredClientFunctionByName = returns callable (functor) that you can call but you should provide a valid arguments otherwise you will get an runtime error with invalid by type arguments
//	getRegisteredServerFunctionByName = returns callable (functor) that you can call but you should provide a valid arguments otherwise you will get an runtime error with invalid by type arguments
//  hasRegisteredClientFunctionByName = checks if you registered your function in storage, false - means you didn't register or you passed an empty string; true - means it was successfully registered to storage;
//  hasRegisteredServerFunctionByName = checks if you registered your function in storage, false - means you didn't register or you passed an empty string; true - means it was successfully regsitered to storage;
#define REGISTER_STORAGE_FOR_RETURN_TYPE(return_type_of_function) private: xr_hash_map<xr_string, CAnyCallable<return_type_of_function>> m_mClientStorageOf##return_type_of_function; private: xr_hash_map<xr_string, CAnyCallable<return_type_of_function>> m_mServerStorageOf##return_type_of_function; private: void registerClientFunction(const xr_string& function_name, const CAnyCallable<return_type_of_function>& func) { m_mClientStorageOf##return_type_of_function[function_name] = func; } private: void registerServerFunction(const xr_string& function_name, const CAnyCallable<return_type_of_function>& func) { m_mServerStorageOf##return_type_of_function[function_name] = func; } public: const CAnyCallable<return_type_of_function>& getRegisteredClientFunctionByName(const xr_string& function_name) { if (m_mClientStorageOf##return_type_of_function.find(function_name) != m_mClientStorageOf##return_type_of_function.end()) return m_mClientStorageOf##return_type_of_function.at(function_name); return CAnyCallable<return_type_of_function>([]()->return_type_of_function{return return_type_of_function{};}); } const CAnyCallable<return_type_of_function>& getRegisteredServerFunctionByName(const xr_string& function_name) { if (m_mServerStorageOf##return_type_of_function.find(function_name) != m_mServerStorageOf##return_type_of_function.end()) return m_mServerStorageOf##return_type_of_function.at(function_name); return CAnyCallable<return_type_of_function>([]()->return_type_of_function{return return_type_of_function{};}); } bool hasRegisteredClientFunction(const xr_string& function_name) { if (function_name.empty()) return false; return m_mClientStorageOf##return_type_of_function.find(function_name) != m_mClientStorageOf##return_type_of_function.end(); } bool hasRegisteredServerFunction(const xr_string& function_name) { if (function_name.empty()) return false; return m_mServerStorageOf##return_type_of_function.find(function_name) != m_mServerStorageOf##return_type_of_function.end(); }

class CScriptGameObject;

class CConfigInfoportion
{
public:
	CConfigInfoportion();
	~CConfigInfoportion();

private:
	float m_fProbability;
};

template <typename ReturnType>
struct CAnyCallable
{
	CAnyCallable(void) : m_pFunction{} {}
	~CAnyCallable(void) {}

	CAnyCallable(const CAnyCallable<ReturnType>& inst) : m_pFunction{ inst.m_pFunction } {}

	template <typename F>
	CAnyCallable(F&& func) : m_pFunction{ func }
	{
	}

	template <typename... Args>
	ReturnType operator()(Args... arguments) const
	{
		std::function<ReturnType(Args...)> myfunction;

		auto function = (ReturnType(*)(Args...))m_pFunction;
		myfunction = function;

		return std::invoke(myfunction, std::forward<Args>(arguments)...);
	}

private:
	void* m_pFunction;
};

class CRandomManager
{
public:
	CRandomManager();
	~CRandomManager();

	int randomBetween(int from, int to);
	float randomBetween(float from, float to);
	double randomBetween(double from, double to);
};

namespace ixray
{
	bool is_weapon(CScriptGameObject* pObject);
	bool has_alife_info(LPCSTR str);
	int get_script_clsid(LPCSTR str);
}