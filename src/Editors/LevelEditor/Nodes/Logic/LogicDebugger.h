#pragma once

struct lua_State;

// Bridge to the scripts of Play In Editor. The calls go from pureFrame: Lua is not running in the game thread then
// The conditions are checked by the C++ parser (CScriptXRParser) through its Lua binding, without the effects
class CLogicDebugger
{
public:
	struct SObject
	{
		int Id = -1;
		xr_string Name;
		// "scripts\\zat_b103_surge_door.ltx", as st.ini_filename of xr_logic
		xr_string IniFile;
		xr_string LogicSection;
		xr_string ActiveSection;
	};

	enum class EResult : s8
	{
		Error = -2,
		NoObject = -1,
		Failed = 0,
		Passed = 1
	};

	static bool IsAvailable();

	// The objects that run xr_logic
	bool GetObjects(xr_vector<SObject>& Objects);
	// Empty when the object has no active section; false when it is gone
	bool GetActiveSection(int Id, xr_string& Section);
	// The conditions of one branch: "+info =function(a:b) !negated"
	EResult Check(int Id, const xr_string& Conditions, bool UseCpp);
	bool SwitchTo(int Id, const xr_string& Section);

	const xr_string& GetLastError() const { return LastError; }

	// The C++ parser asserts on the symbols out of its mask
	static bool IsCppParsable(const xr_string& Condlist);

private:
	lua_State* Prepare();
	bool CallFunction(lua_State* L, const char* Name, int Arguments, int Results);

	lua_State* LoadedState = nullptr;
	xr_string LastError;
};
