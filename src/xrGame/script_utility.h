////////////////////////////////////////////////////////////////////////////
//	Module 		: script_utility.h
//	Created 	: 16.08.2024
//  Modified 	: 16.08.2024
//	Author		: wh1t3lord
//	Description : Implementation of condlist and other stuff based on xr_logic.script
////////////////////////////////////////////////////////////////////////////

#pragma once

#define REGISTER_FUNCTION_TO_SCRIPT(myfunction) this->registerFunction(#myfunction, CAnyCallable<std::function<decltype(myfunction)>::result_type>(myfunction)); 

#define REGISTER_STORAGE_FOR_RETURN_TYPE(return_type_of_function) private: xr_hash_map<xr_string, CAnyCallable<return_type_of_function>> m_mStorageOf##return_type_of_function; private: void registerFunction(const xr_string& function_name, const CAnyCallable<return_type_of_function>& func) { m_mStorageOf##return_type_of_function[function_name] = func; } public: const CAnyCallable<return_type_of_function>& getRegisteredFunctionByName(const xr_string& function_name) { if (m_mStorageOf##return_type_of_function.find(function_name) != m_mStorageOf##return_type_of_function.end()) return m_mStorageOf##return_type_of_function.at(function_name); return CAnyCallable<return_type_of_function>([]()->return_type_of_function{return return_type_of_function{};}); }

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