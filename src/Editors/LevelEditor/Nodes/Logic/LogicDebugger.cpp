#include "stdafx.h"
#include "LogicDebugger.h"
#include "../../../../xrScripts/linker.h"
#include "../../../../xrScripts/script_engine.h"

// Loaded into the scripts of the level: the scripts of the game stay as they are
static const char* DebuggerChunk = R"lua(
ixr_logic_debug = {}
local D = ixr_logic_debug

local function object_of(id)
	local st = db.storage[id]
	return (st and st.object) or (level and level.object_by_id(id))
end

function D.objects()
	local out = {}
	for id, st in pairs(db.storage) do
		if type(st) == "table" and st.ini_filename ~= nil then
			local obj = object_of(id)
			local name = obj and obj:name() or tostring(id)
			out[#out + 1] = string.format("%d\t%s\t%s\t%s\t%s", id, name, tostring(st.ini_filename), tostring(st.section_logic or ""), tostring(st.active_section or ""))
		end
	end
	return table.concat(out, "\n")
end

function D.active(id)
	local st = db.storage[id]
	if st == nil or object_of(id) == nil then
		return nil
	end
	return tostring(st.active_section or "")
end

-- 1 passed, 0 failed, -1 no object. Only the conditions: the branch has no effects
function D.check(id, conditions, use_cpp)
	local obj = object_of(id)
	if obj == nil or db.actor == nil then
		return -1
	end
	local src = "{" .. conditions .. "} debug_ok"
	local result
	if use_cpp and get_xr_parser_manager ~= nil then
		result = get_xr_parser_manager():pick_section_from_condlist(db.actor, obj, src)
	else
		result = xr_logic.pick_section_from_condlist(db.actor, obj, xr_logic.parse_condlist(obj, "logic_debug", "check", src))
	end
	return (result ~= nil) and 1 or 0
end

function D.switch(id, section)
	local st = db.storage[id]
	local obj = object_of(id)
	if st == nil or obj == nil or st.ini == nil or not st.ini:section_exist(section) then
		return false
	end
	xr_logic.switch_to_section(obj, st.ini, section)
	return true
end
)lua";

bool CLogicDebugger::IsAvailable()
{
	return g_pGameLevel != nullptr && g_pScriptEngine != nullptr && g_pScriptEngine->lua() != nullptr;
}

bool CLogicDebugger::IsCppParsable(const xr_string& Condlist)
{
	// CScriptXRParser::isSymbolValidForParsing
	static const char* Mask = "%{}.,qwertyuioplkjhgfdsamnbvcxz1234567890@-+=~!_(): ";
	for (char Char : Condlist)
	{
		if (strchr(Mask, Char) == nullptr)
		{
			return false;
		}
	}
	return true;
}

lua_State* CLogicDebugger::Prepare()
{
	if (!IsAvailable())
	{
		return nullptr;
	}

	lua_State* L = g_pScriptEngine->lua();

	// A new level has new scripts: the table is loaded again
	lua_getglobal(L, "ixr_logic_debug");
	const bool Loaded = lua_istable(L, -1);
	lua_pop(L, 1);
	if (Loaded)
	{
		return L;
	}

	lua_getglobal(L, "db");
	const bool HasDb = lua_istable(L, -1);
	lua_pop(L, 1);
	if (!HasDb)
	{
		LastError = "the scripts of the level are not loaded yet";
		return nullptr;
	}

	if (luaL_loadbuffer(L, DebuggerChunk, xr_strlen(DebuggerChunk), "@ixr_logic_debug") != 0 || lua_pcall(L, 0, 0, 0) != 0)
	{
		LastError = lua_isstring(L, -1) ? lua_tostring(L, -1) : "can't load the debugger";
		lua_pop(L, 1);
		return nullptr;
	}
	LoadedState = L;
	return L;
}

bool CLogicDebugger::CallFunction(lua_State* L, const char* Name, int Arguments, int Results)
{
	// The arguments are on the stack: the function goes below them
	lua_getglobal(L, "ixr_logic_debug");
	lua_getfield(L, -1, Name);
	lua_remove(L, -2);
	lua_insert(L, -(Arguments + 1));
	if (lua_pcall(L, Arguments, Results, 0) != 0)
	{
		LastError = lua_isstring(L, -1) ? lua_tostring(L, -1) : "script error";
		lua_pop(L, 1);
		return false;
	}
	return true;
}

bool CLogicDebugger::GetObjects(xr_vector<SObject>& Objects)
{
	Objects.clear();
	lua_State* L = Prepare();
	if (L == nullptr || !CallFunction(L, "objects", 0, 1))
	{
		return false;
	}

	const xr_string Text = lua_isstring(L, -1) ? lua_tostring(L, -1) : "";
	lua_pop(L, 1);

	size_t Begin = 0;
	while (Begin < Text.size())
	{
		size_t End = Text.find('\n', Begin);
		if (End == xr_string::npos)
		{
			End = Text.size();
		}
		const xr_string Line = Text.substr(Begin, End - Begin);
		Begin = End + 1;

		xr_vector<xr_string> Fields;
		size_t FieldBegin = 0;
		while (FieldBegin <= Line.size())
		{
			size_t FieldEnd = Line.find('\t', FieldBegin);
			if (FieldEnd == xr_string::npos)
			{
				FieldEnd = Line.size();
			}
			Fields.push_back(Line.substr(FieldBegin, FieldEnd - FieldBegin));
			FieldBegin = FieldEnd + 1;
		}
		if (Fields.size() < 5)
		{
			continue;
		}

		SObject Object;
		Object.Id = atoi(Fields[0].c_str());
		Object.Name = Fields[1];
		Object.IniFile = Fields[2];
		Object.LogicSection = Fields[3];
		Object.ActiveSection = Fields[4];
		Objects.push_back(Object);
	}

	std::sort(Objects.begin(), Objects.end(), [](const SObject& A, const SObject& B) { return A.Name < B.Name; });
	return true;
}

bool CLogicDebugger::GetActiveSection(int Id, xr_string& Section)
{
	lua_State* L = Prepare();
	if (L == nullptr)
	{
		return false;
	}
	lua_pushinteger(L, Id);
	if (!CallFunction(L, "active", 1, 1))
	{
		return false;
	}
	const bool Exists = lua_isstring(L, -1);
	Section = Exists ? lua_tostring(L, -1) : "";
	lua_pop(L, 1);
	return Exists;
}

CLogicDebugger::EResult CLogicDebugger::Check(int Id, const xr_string& Conditions, bool UseCpp)
{
	lua_State* L = Prepare();
	if (L == nullptr)
	{
		return EResult::Error;
	}
	lua_pushinteger(L, Id);
	lua_pushstring(L, Conditions.c_str());
	lua_pushboolean(L, UseCpp && IsCppParsable(Conditions) ? 1 : 0);
	if (!CallFunction(L, "check", 3, 1))
	{
		return EResult::Error;
	}
	const int Result = (int)lua_tointeger(L, -1);
	lua_pop(L, 1);
	return Result > 0 ? EResult::Passed : Result == 0 ? EResult::Failed : EResult::NoObject;
}

bool CLogicDebugger::SwitchTo(int Id, const xr_string& Section)
{
	lua_State* L = Prepare();
	if (L == nullptr)
	{
		return false;
	}
	lua_pushinteger(L, Id);
	lua_pushstring(L, Section.c_str());
	if (!CallFunction(L, "switch", 2, 1))
	{
		return false;
	}
	const bool Result = lua_toboolean(L, -1) != 0;
	lua_pop(L, 1);
	return Result;
}
