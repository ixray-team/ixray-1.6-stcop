////////////////////////////////////////////////////////////////////////////
//	Module 		: script_utility.h
//	Created 	: 16.08.2024
//  Modified 	: 16.08.2024
//	Author		: wh1t3lord
//	Description : Implementation of condlist and other stuff based on
// xr_logic.script
////////////////////////////////////////////////////////////////////////////

#pragma once

// when you use this you need to provide two versions of your function, one is
// client that means it accepts only CScriptGameObject* instances and server
// version that accepts a server instance when it is called it binds client and
// server version of  your function myfunction = main word without postfix
#define IXRAY_LUA_TO_CPP_REGISTER_FUNCTION_TO_SCRIPT(myfunction)          \
	this->registerClientFunction(#myfunction,                             \
		CAnyCallable<                                                     \
			std::function<decltype(##myfunction##_client)>::result_type>( \
			##myfunction##_client));                                      \
	this->registerServerFunction(#myfunction,                             \
		CAnyCallable<                                                     \
			std::function<decltype(##myfunction##_server)>::result_type>( \
			##myfunction##_server));

// declares two hash maps for two version of function client and server;
// also implements all methods for accessing these fields:
//	getRegisteredClientFunctionByName = returns callable (functor) that you can
// call but you should provide a valid arguments otherwise you will get an
// runtime error with invalid by type arguments
// getRegisteredServerFunctionByName = returns callable (functor) that you can
// call but you should provide a valid arguments otherwise you will get an
// runtime error with invalid by type arguments
//  hasRegisteredClientFunctionByName = checks if you registered your function
//  in storage, false - means you didn't register or you passed an empty string;
//  true - means it was successfully registered to storage;
//  hasRegisteredServerFunctionByName = checks if you registered your function
//  in storage, false - means you didn't register or you passed an empty string;
//  true - means it was successfully regsitered to storage;
#define IXRAY_LUA_TO_CPP_REGISTER_STORAGE_FOR_RETURN_TYPE(                     \
	return_type_of_function)                                                   \
private:                                                                       \
	xr_hash_map<xr_string, CAnyCallable<return_type_of_function>>              \
		m_mClientStorageOf##return_type_of_function;                           \
                                                                               \
private:                                                                       \
	xr_hash_map<xr_string, CAnyCallable<return_type_of_function>>              \
		m_mServerStorageOf##return_type_of_function;                           \
                                                                               \
private:                                                                       \
	void registerClientFunction(const xr_string& function_name,                \
		const CAnyCallable<return_type_of_function>& func)                     \
	{                                                                          \
		R_ASSERT2(                                                             \
			m_mClientStorageOf##return_type_of_function.find(function_name) == \
				m_mClientStorageOf##return_type_of_function.end(),             \
			"you try to register client same function twice, because it is "   \
			"already "                                                         \
			"registered!");                                                    \
		m_mClientStorageOf##return_type_of_function[function_name] = func;     \
	}                                                                          \
                                                                               \
private:                                                                       \
	void registerServerFunction(const xr_string& function_name,                \
		const CAnyCallable<return_type_of_function>& func)                     \
	{                                                                          \
		R_ASSERT2(                                                             \
			m_mServerStorageOf##return_type_of_function.find(function_name) == \
				m_mServerStorageOf##return_type_of_function.end(),             \
			"you try to register server same function twice, because it is "   \
			"already "                                                         \
			"registered!");                                                    \
		m_mServerStorageOf##return_type_of_function[function_name] = func;     \
	}                                                                          \
                                                                               \
public:                                                                        \
	const CAnyCallable<return_type_of_function>&                               \
	getRegisteredClientFunctionByName(const xr_string& function_name)          \
	{                                                                          \
		if (m_mClientStorageOf##return_type_of_function.find(function_name) != \
			m_mClientStorageOf##return_type_of_function.end())                 \
			return m_mClientStorageOf##return_type_of_function.at(             \
				function_name);                                                \
		return CAnyCallable<return_type_of_function>(                          \
			[]() -> return_type_of_function                                    \
			{ return return_type_of_function{}; });                            \
	}                                                                          \
	const CAnyCallable<return_type_of_function>&                               \
	getRegisteredServerFunctionByName(const xr_string& function_name)          \
	{                                                                          \
		if (m_mServerStorageOf##return_type_of_function.find(function_name) != \
			m_mServerStorageOf##return_type_of_function.end())                 \
			return m_mServerStorageOf##return_type_of_function.at(             \
				function_name);                                                \
		return CAnyCallable<return_type_of_function>(                          \
			[]() -> return_type_of_function                                    \
			{ return return_type_of_function{}; });                            \
	}                                                                          \
	bool hasRegisteredClientFunction(const xr_string& function_name)           \
	{                                                                          \
		if (function_name.empty())                                             \
			return false;                                                      \
		return m_mClientStorageOf##return_type_of_function.find(               \
				   function_name) !=                                           \
			m_mClientStorageOf##return_type_of_function.end();                 \
	}                                                                          \
	bool hasRegisteredServerFunction(const xr_string& function_name)           \
	{                                                                          \
		if (function_name.empty())                                             \
			return false;                                                      \
		return m_mServerStorageOf##return_type_of_function.find(               \
				   function_name) !=                                           \
			m_mServerStorageOf##return_type_of_function.end();                 \
	}

// TODO: ForserX->Discussion(); I suggest to use these variants for preprocessor
// and make possible to use vanila GSC's variant for lua like to disable cpp
// implementations of lua scripts; and for users who wants to re-write
// everything on cpp also I suggest to defined which variant to use through
// CMake

// if user wants vanila lua backend (no cpp at all)
// #define IXRAY_USE_LUA_ONLY_IMPLEMENTATION

// if user wants to use cpp backend (no lua at all)
// #define IXRAY_USE_CPP_ONLY_IMPLEMENTATION

// if user wants to use cpp backend but mixed with lua callings (some stuff is
// in pure lua, some stuff was re-written to cpp)
#define IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION

// we use functions from lua that defined in xr_effects.script and
// xr_conditions.script files (original behaviour)
#define IXRAY_XR_PARSER_USE_LUA_BACKEND

#pragma todo( \
	"wh1t3lord to ForserX: need to implement support of CMake configuration of these preprocessors, read comments carefully, so they need to be generated in separated header file like script_utility_preprocessors.h (or your variant)!")

// this preprocessor uses the third up preprocessors e.g.
// IXRAY_USE_LUA_AND_CPP_IMPLEMENTATION, IXRAY_USE_CPP_ONLY_IMPLEMENTATION,
// IXRAY_USE_LUA_ONLY_IMPLEMENTATION
// in comparison to IXRAY_XR_PARSER_USE_LUA_BACKEND it will used
// CScriptXREffectsStorage and CScriptXRConditionsStorage implementations and
// binded functions #define IXRAY_XR_PARSER_USE_CPP_BACKEND

class CScriptGameObject;

namespace ixray
{
	// the length of string of check condition e.g. {...} <= between brackets string must be equal or less 128 or section_name or %...% <= between percents string must be equal or less 128 symbols 
	constexpr size_t kCondlistInfoStringSize = 128;
	// the length of function name or section name or infoportion
	// for example we have a such condlist "{-infoportion_name} section_name %=function_name%" so
	// this field applies for infoportion_name length and section_name and function_name thus 
	// length of all three are equal to each other (there's no point to separate lengths for section_name, function_name and infoportion)
	constexpr size_t kCondlistDataStringSize = 128;
	// length of number that represents in game uses from 1 to 100, but for future considerations was chosen a such size
	constexpr size_t kCondlistProbabilityStringSize = 6;
	// amount of ',' symbol that represent alternative condlist in one condlist field
	// for example "on_info = {-info} section_name, {-info +info2} section_name %=function%, ..., ..."
	constexpr size_t kCondlistEmbeddedDataSize = 32;
	
	// how many arguments we could have
	constexpr size_t kXRParserParamsBufferSize = 10;

	// char paramName[kXRParserParamBufferSize]; lol but 32 length must be
	// enough
	constexpr size_t kXRParserParamBufferSize = 64;
	constexpr size_t kXRParserFunctionNameBufferSize = 64;
	constexpr size_t kLTXFunctionArgumentDelimiterCharacter = ':';

	constexpr const char* kReservedWordNever = "never";

	bool is_weapon(CScriptGameObject* pObject);
	bool has_alife_info(LPCSTR str);	
	int get_script_clsid(LPCSTR str);
} // namespace ixray

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

	CAnyCallable(const CAnyCallable<ReturnType>& inst) :
		m_pFunction{inst.m_pFunction}
	{
	}

	template <typename F>
	CAnyCallable(F&& func) : m_pFunction{func}
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

class CCondlistData
{
public:
	CCondlistData();
	~CCondlistData();

	bool getRequired(void) const;
	void setRequired(bool bValue);

	bool getExpected(void) const;
	void setExpected(bool bValue);

	const char* getProbability(void) const;
	void setProbability(const char* pString);

	const char* getFunctionName(void) const;
	void setFunctionName(const char* pString);

	const char* getInfoPortionName(void) const;
	void setInfoPortionName(const char* pString);

	const char* getParams(void) const;
	void setParams(const char* pString);

private:
	bool m_bRequired;
	bool m_bExpected;
	// because in some cases it is not initialized field and make it obvious
	// using raw string much better than storing invalid number value yk~~
	char m_probability[ixray::kCondlistProbabilityStringSize];
	// don't make it as std::string because it is already stored in
	// filesystem...
	char m_functionname[ixray::kCondlistDataStringSize];
	char m_infoportionname[ixray::kCondlistDataStringSize];
	char m_params[ixray::kCondlistDataStringSize];
};

class CCondlist
{
public:
	CCondlist();
	~CCondlist();

	const xr_hash_map<u32, CCondlistData>& getInfoPortionCheck(void) const;
	xr_hash_map<u32, CCondlistData>& getInfoPortionCheck(void);

	void addInfoPortionCheck(u32 nID, const CCondlistData& data);
	void addInfoPortionCheck(const std::pair<u32, CCondlistData>& pair);

	const xr_hash_map<u32, CCondlistData>& getInfoPortionSet(void) const;
	xr_hash_map<u32, CCondlistData>& getInfoPortionSet(void);

	void addInfoPortionSet(u32 nID, const CCondlistData& data);
	void addInfoPortionSet(const std::pair<u32, CCondlistData>& pair);

	const char* getSectionName(void) const;
	void setSectionName(const char* pString);

private:
	char m_sectionname[ixray::kCondlistInfoStringSize];
	xr_hash_map<u32, CCondlistData> m_mInfoPortionCheck;
	xr_hash_map<u32, CCondlistData> m_mInfoPortionSet;
};

// stack memory
class CCondlistEmbedded
{
public:
	using xr_condlistdata =
		xr_array<CCondlistData, ixray::kCondlistEmbeddedDataSize>;

public:
	CCondlistEmbedded();
	~CCondlistEmbedded();

	const xr_condlistdata& getInfoPortionSet(void) const;
	xr_condlistdata& getInfoPortionSet(void);

	const xr_condlistdata& getInfoPortionCheck(void) const;
	xr_condlistdata& getInfoPortionCheck(void);

	void addInfoPortionSet(u32 nID, const CCondlistData& data);
	void addInfoPortionCheck(u32 nID, const CCondlistData& data);

	const char* getSectionName(void) const;
	void setSectionName(const char* pString);

	int getArrayCheckSize(void) const;
	void setArrayCheckSize(int nSize);

	int getArraySetSize(void) const;
	void setArraySetSize(int nSize);

private:
	int m_nArrayCheckSize;
	int m_nArraySetSize;
	char m_sectionname[ixray::kCondlistInfoStringSize];
	xr_condlistdata m_aInfoPortionCheck;
	xr_condlistdata m_aInfoPortionSet;
};

class CCondlistInfo
{
public:
	CCondlistInfo();
	~CCondlistInfo();

	void setInfoCheck(const char* pBuffer, size_t nStringLength);
	void setInfoSet(const char* pBuffer, size_t nStringLength);
	void setText(const char* pBuffer, size_t nStringLength);

	void clearInfoCheck();
	void clearInfoSet();
	void clearText();
	void clear();

	const char* getInfoCheckName(void);
	const char* getTextName(void);
	const char* getInfoSetName(void);

private:
	char m_infocheck_name[ixray::kCondlistInfoStringSize];
	char m_infoset_name[ixray::kCondlistInfoStringSize];
	char m_text_name[ixray::kCondlistInfoStringSize];
};