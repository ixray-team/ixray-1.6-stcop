////////////////////////////////////////////////////////////////////////////
//	Module 		: script_engine_script.cpp
//	Created 	: 25.12.2002
//  Modified 	: 13.05.2004
//	Author		: Dmitriy Iassenev
//	Description : ALife Simulator script engine export
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"
#include "script_engine.h"
#include "lua_ext.h"

using namespace luabind;

XREFLECT(LuaExport, Name = "log", Condition = "DevicePtr != nullptr && Device.IsEditorMode()")
void LuaLog(const char* caMessage)
{
#ifndef MASTER_GOLD
	g_pScriptEngine->script_log(ScriptStorage::eLuaMessageTypeMessage, "%s", caMessage);
#endif // #ifndef MASTER_GOLD
}

XREFLECT(LuaExport, Name = "error_log")
void ErrorLog(const char* caMessage)
{
	g_pScriptEngine->error_log("%s", caMessage);
	g_pScriptEngine->print_stack();

	R_ASSERT2(0, caMessage);
}

XREFLECT(LuaExport, Name = "flush")
void FlushLogs()
{
#ifdef DEBUG
	xrLogger::FlushLog();
	g_pScriptEngine->flush_log();
#endif // DEBUG
}

XREFLECT(LuaExport)
void verify_if_thread_is_running()
{
	VERIFY2(g_pScriptEngine->current_thread(), "coroutine.yield() is called outside the LUA thread!");
}

XREFLECT(LuaExport, Name = "editor")
bool is_editor()
{
#ifdef XRGAME_EXPORTS
	return (false);
#else
	return (true);
#endif
}

#ifdef XRGAME_EXPORTS
XREFLECT(LuaExport, Name = "device")
CRenderDevice* get_device()
{
	return (&Device);
}

XREFLECT(LuaExport)
void trigger_assert(const char* pStringFromLua)
{
	R_ASSERT(false && "catch the thing!");
}

XREFLECT(LuaExport)
void trigger_vs_log(const char* pStringFromLua)
{
#ifdef IXR_WINDOWS
	OutputDebugStringA(pStringFromLua);
#endif
}

XREFLECT(LuaExport, Name = "load_script_to_global")
void LoadScriptToGlobal_script(const char* script_name)
{
	LoadScriptToGlobal(g_pScriptEngine->lua(), script_name, false);
}
#endif

XREFLECT(LuaExport)
const char* user_name()
{
	return (Core.UserName);
}

XREFLECT(LuaExport, Name = "prefetch")
void prefetch_module(const char* file_name)
{
	g_pScriptEngine->process_file(file_name);
}

XREFLECT(LuaExport, Name = "profile_timer", LuaToString)
struct profile_timer_script
{
	CTimer measure;
	u64 m_accumulator;
	u64 m_count;
	int m_recurse_mark;

	XPROPERTY(LuaConstructor)
	IC profile_timer_script()
	{
		m_accumulator = 0;
		m_count = 0;
		m_recurse_mark = 0;
	}

	XPROPERTY(LuaConstructor)
	IC profile_timer_script(const profile_timer_script& profile_timer)
	{
		*this = profile_timer;
	}

	IC profile_timer_script& operator=(const profile_timer_script& profile_timer)
	{
		measure = profile_timer.measure;
		m_accumulator = profile_timer.m_accumulator;
		m_count = profile_timer.m_count;
		m_recurse_mark = profile_timer.m_recurse_mark;
		return (*this);
	}

	XPROPERTY(LuaOperator)
	IC bool operator<(const profile_timer_script& profile_timer) const
	{
		return (m_accumulator < profile_timer.m_accumulator);
	}

	XPROPERTY(LuaCallable)
	IC void start()
	{
		if (m_recurse_mark)
		{
			++m_recurse_mark;
			return;
		}

		++m_recurse_mark;
		++m_count;
		measure.Start();
	}

	XPROPERTY(LuaCallable)
	IC void stop()
	{
		if (!m_recurse_mark)
		{
			return;
		}

		--m_recurse_mark;

		if (m_recurse_mark)
		{
			return;
		}

		m_accumulator += measure.GetElapsed_mcs();
	}

	XPROPERTY(LuaCallable)
	IC float time() const
	{
		float result = float(double(m_accumulator));
		return (result);
	}

	XPROPERTY(LuaOperator)
	IC profile_timer_script operator+(const profile_timer_script& portion) const
	{
		profile_timer_script result;
		result.m_accumulator = m_accumulator + portion.m_accumulator;
		result.m_count = m_count + portion.m_count;
		return (result);
	}
};

std::ostream& operator<<(std::ostream& os, const profile_timer_script& pt)
{
	return os << pt.time();
}

XREFLECT(LuaExport, Name = "TinyLog")
void MyLog(const char* A)
{
	Log(A);
}

// Log is overloaded, Lua gets the const char* one.
XREFLECT(LuaExport, Name = "log1")
void LuaLog1(const char* Message)
{
	Log(Message);
}

#ifdef XRGAME_EXPORTS
XREFLECT(LuaExport, Name = "time_global")
ICF u32 script_time_global()
{
	return Device.dwTimeGlobal;
}
XREFLECT(LuaExport, Name = "time_global_async")
ICF u32 script_time_global_async()
{
	return Device.TimerAsync_MMT();
}
#else
XREFLECT(LuaExport, Name = "time_global")
ICF u32 script_time_global()
{
	return 0;
}
XREFLECT(LuaExport, Name = "time_global_async")
ICF u32 script_time_global_async()
{
	return 0;
}
#endif

XREFLECT(LuaExport, Name = "IsSupportMP")
bool CheckMP()
{
#ifdef XR_MP_BUILD
	return true;
#else
	return false;
#endif
}

XREFLECT(LuaExport, Name = "IsEditor")
bool IsEditorMode()
{
	if (DevicePtr == nullptr)
	{
		return false;
	}

	return Device.IsEditorMode();
}

XREFLECT(LuaExport)
void SemiLog(const char* message)
{
	Msg(message);
}

namespace ixray::save
{
xr_string CurrentSaveStage = "";
XREFLECT(LuaExport, Name = "save.call_error")
void SaveError()
{
	R_ASSERT2(!"Save file is big. Chunk: ", CurrentSaveStage.c_str());
}

XREFLECT(LuaExport, Name = "save.set_stage")
void SaveStage(const char* Name)
{
	CurrentSaveStage = Name;
}
} // namespace ixray::save

XREFLECT(LuaExport, Name = "try_load_file")
void TryLoadFile(const char* FileName)
{
	FS.TryLoad(FileName);
}

XREFLECT(LuaExport, Name = "debug_print")
void lua_debug_print(const char* str)
{
	if (!xr_strlen(str))
	{
		return;
	}

	Msg("$ DBG:[%lu] %s", Device.dwTimeGlobal, str);
}

XREFLECT(LuaExport, Name = "help")
void DumpLuaExports()
{
	lua_State* L = g_pScriptEngine->lua();
	using namespace luabind::detail;

	class_registry* Registry = class_registry::get_registry(L);
	auto GetArgTypeName = [](const auto info) -> xr_string
	{
		if (!info)
		{
			return "unknown";
		}

		xr_string Result = (*info).name();

		const xr_vector<xr_string> Need2Remove = {"class ", "struct ", " __ptr64"};
		for (const auto& Str : Need2Remove)
		{
			size_t Pos = 0;
			while ((Pos = Result.find(Str, Pos)) != xr_string::npos)
			{
				Result.erase(Pos, Str.length());
			}
		}

		return Result;
	};

	Registry->iterate_classes(L, [&](lua_State* st, class_rep* crep)
							  {
		if (!crep->bases().empty())
		{
			xr_string OutStr = "C++ class ";
			OutStr += crep->name();
			OutStr += " : ";

			bool IsFirstStep = true;
			for (const class_rep::base_info& BaseClassInfo : crep->bases())
			{
				if (!IsFirstStep)
				{
					OutStr += ", ";
				}
				
				OutStr += BaseClassInfo.base->name();
				IsFirstStep = false;
			}

			Msg("C++ class %s", OutStr.c_str());
		}
		else
		{
			Msg("C++ class %s", crep->name());
		}

		Msg("{");

		for (const auto&[Name, _] : crep->m_getters)
		{
			Msg("\tproperty %s;", Name);
		}

		for (const auto& [Name, Value] : crep->static_constants())
		{
			Msg("\tconst %s = %d;", Name, Value);
		}

		for (const auto& Method : crep->m_methods)
		{
			for (const auto& Overload : Method.overloads())
			{
				int ArgsCount = Overload.m_arity;
				xr_string args;
				bool IsPrinted = false;
				for (int i = 0; i < ArgsCount; ++i)
				{
					if (Overload.m_params_.size() > i)
					{
						if (IsPrinted)
						{
							args += ", ";
						}

						args += GetArgTypeName(Overload.m_params_[i]);
						IsPrinted = true;
					}
				}
				Msg("\tfunction %s(%s);", Method.name, args.c_str());
			}
		}

		Msg("};\n"); });
}

static xr_string NormalizeTypeName(const char* Name)
{
	xr_string Result = Name;
	for (const char* Token : { "class ", "struct ", "enum ", "const ", " const", "__ptr64", "volatile " })
	{
		for (size_t Pos = Result.find(Token); Pos != xr_string::npos; Pos = Result.find(Token))
		{
			Result.erase(Pos, xr_strlen(Token));
		}
	}
	std::erase_if(Result, [](char Char) { return Char == '*' || Char == '&' || Char == ' '; });
	return Result;
}

static xr_string GetLuaTypeName(const std::type_info* Info, const xr_hash_map<xr_string, xr_string>& Classes)
{
	if (Info == nullptr)
	{
		return "any";
	}

	const xr_string Name = NormalizeTypeName(Info->name());
	auto Class = Classes.find(Name);
	if (Class != Classes.end())
	{
		return Class->second;
	}

	if (Name == "bool")
	{
		return "boolean";
	}
	if (Name == "char" || Name.ends_with("string") || Name == "shared_str" || Name == "luabind::internal_string")
	{
		return "string";
	}
	if (Name.find("luabind::") != xr_string::npos)
	{
		return Name.find("functor") != xr_string::npos ? "function" : "any";
	}

	static const char* Numbers[] = { "int", "unsignedint", "float", "double", "long", "unsignedlong", "short", "unsignedshort", "unsignedchar", "signedchar", "__int64", "unsigned__int64", "longlong", "unsignedlonglong" };
	for (const char* Number : Numbers)
	{
		if (Name == Number)
		{
			return "number";
		}
	}

	// Enums and unregistered types keep the C++ name without the namespaces
	const size_t Scope = Name.rfind("::");
	return Scope != xr_string::npos ? xr_string(Name.substr(Scope + 2).c_str()) : Name;
}

template <typename TParams>
static xr_string JoinParams(const TParams& Params, size_t First, const xr_hash_map<xr_string, xr_string>& Classes)
{
	xr_string Result;
	for (size_t Index = First; Index < Params.size(); ++Index)
	{
		if (!Result.empty())
		{
			Result += ", ";
		}
		Result += GetLuaTypeName(Params[Index], Classes);
	}
	return Result;
}

static const luabind::detail::free_functions::function_rep* GetFunctionRep(lua_State* L, int Index)
{
	const luabind::detail::free_functions::function_rep* Rep = nullptr;
	if (!lua_iscfunction(L, Index))
	{
		return nullptr;
	}

	if (lua_getupvalue(L, Index, 2) != nullptr)
	{
		// luabind marks its closures with this number
		if (lua_touserdata(L, -1) == (void*)0x1337 && lua_getupvalue(L, Index, 1) != nullptr)
		{
			Rep = static_cast<const luabind::detail::free_functions::function_rep*>(lua_touserdata(L, -1));
			lua_pop(L, 1);
		}
		lua_pop(L, 1);
	}
	return Rep;
}

static xr_string GetScriptParams(lua_State* L, bool IsMethod)
{
	xr_string Result;
	// With no activation record lua_getlocal names the parameters of the function at the top
	for (int Index = 1; ; ++Index)
	{
		const char* Name = lua_getlocal(L, nullptr, Index);
		if (Name == nullptr)
		{
			break;
		}
		if (IsMethod && Index == 1 && xr_strcmp(Name, "self") == 0)
		{
			continue;
		}

		Result += Result.empty() ? "" : ", ";
		Result += Name;
	}
	return Result;
}

static void BakeFunctions(lua_State* L, const char* Namespace, const xr_hash_map<xr_string, xr_string>& Classes, IWriter& Writer)
{
	const int Table = lua_gettop(L);
	lua_pushnil(L);
	while (lua_next(L, Table) != 0)
	{
		if (lua_type(L, -2) == LUA_TSTRING && lua_type(L, -1) == LUA_TFUNCTION)
		{
			const char* Name = lua_tostring(L, -2);
			if (const luabind::detail::free_functions::function_rep* Rep = GetFunctionRep(L, lua_gettop(L)))
			{
				for (const auto& Overload : Rep->overloads())
				{
					Writer.w_string((xr_string("F\t") + Namespace + "\t" + Name + "\t" + JoinParams(Overload.m_params_, 0, Classes)).c_str());
				}
			}
			else if (!lua_iscfunction(L, -1))
			{
				Writer.w_string((xr_string("F\t") + Namespace + "\t" + Name + "\t" + GetScriptParams(L, false)).c_str());
			}
		}
		// Fields: db.storage, db.heli_enemy_count. The global tables are the namespaces
		else if (lua_type(L, -2) == LUA_TSTRING && (Namespace[0] != 0 || lua_type(L, -1) != LUA_TTABLE))
		{
			const char* Name = lua_tostring(L, -2);
			Writer.w_string((xr_string("V\t") + Namespace + "\t" + Name + "\t" + lua_typename(L, lua_type(L, -1))).c_str());
		}
		lua_pop(L, 1);
	}
}

static void BakeScriptMethods(lua_State* L, const char* Class, IWriter& Writer)
{
	const int Table = lua_gettop(L);
	lua_pushnil(L);
	while (lua_next(L, Table) != 0)
	{
		if (lua_type(L, -2) == LUA_TSTRING && lua_type(L, -1) == LUA_TFUNCTION && !lua_iscfunction(L, -1))
		{
			const char* Name = lua_tostring(L, -2);
			Writer.w_string((xr_string("M\t") + Class + "\t" + Name + "\t" + GetScriptParams(L, true)).c_str());
		}
		lua_pop(L, 1);
	}
}

static xr_string TrimText(const xr_string& Text)
{
	size_t Begin = 0;
	size_t End = Text.size();
	while (Begin < End && isspace((u8)Text[Begin]))
	{
		++Begin;
	}
	while (End > Begin && isspace((u8)Text[End - 1]))
	{
		--End;
	}
	return Text.substr(Begin, End - Begin).c_str();
}

static xr_string ReadIdentifier(const xr_string& Text, size_t& Pos)
{
	const size_t Start = Pos;
	while (Pos < Text.size() && (isalnum((u8)Text[Pos]) || Text[Pos] == '_'))
	{
		++Pos;
	}
	return Text.substr(Start, Pos - Start).c_str();
}

// class "name" (base) or class "name": Base is empty without one, it keeps a namespace prefix
static bool ParseClassLine(const xr_string& Line, xr_string& Class, xr_string& Base)
{
	if (!Line.starts_with("class"))
	{
		return false;
	}

	const size_t Open = Line.find('"');
	const size_t Close = Open != xr_string::npos ? Line.find('"', Open + 1) : xr_string::npos;
	if (Close == xr_string::npos)
	{
		return false;
	}

	Class = Line.substr(Open + 1, Close - Open - 1).c_str();
	Base.clear();
	const size_t BaseOpen = Line.find('(', Close);
	const size_t BaseClose = BaseOpen != xr_string::npos ? Line.find(')', BaseOpen) : xr_string::npos;
	if (BaseClose != xr_string::npos)
	{
		Base = TrimText(Line.substr(BaseOpen + 1, BaseClose - BaseOpen - 1).c_str());
	}
	return true;
}

static xr_vector<xr_string> ReadScriptLines(const char* Path, bool Trim = true)
{
	xr_vector<xr_string> Lines;
	IReader* Reader = FS.r_open(Path);
	if (Reader == nullptr)
	{
		return Lines;
	}

	string4096 Buffer;
	while (!Reader->eof())
	{
		Reader->r_string(Buffer, sizeof(Buffer));
		xr_string Line = Buffer;
		const size_t Comment = Line.find("--");
		if (Comment != xr_string::npos)
		{
			Line.resize(Comment);
		}
		Lines.push_back(Trim ? TrimText(Line) : Line);
	}
	FS.r_close(Reader);
	return Lines;
}

// Top level "name = nil" is not in the tables
static void BakeScriptFields(const char* Namespace, const xr_vector<xr_string>& RawLines, IWriter& Writer)
{
	static const char* Keywords[] = { "local", "function", "if", "for", "while", "return", "end", "repeat", "until", "do", "else", "elseif", "class", "then", "break" };

	for (const xr_string& Line : RawLines)
	{
		// Column 0 only: the indented assignments are inside the functions
		if (Line.empty() || !(isalpha((u8)Line[0]) || Line[0] == '_'))
		{
			continue;
		}

		size_t Pos = 0;
		const xr_string Name = ReadIdentifier(Line, Pos);
		while (Pos < Line.size() && isspace((u8)Line[Pos]))
		{
			++Pos;
		}

		const bool IsKeyword = std::any_of(std::begin(Keywords), std::end(Keywords), [&Name](const char* Keyword) { return Name == Keyword; });
		if (IsKeyword || Pos >= Line.size() || Line[Pos] != '=' || (Pos + 1 < Line.size() && Line[Pos + 1] == '='))
		{
			continue;
		}
		Writer.w_string((xr_string("V\t") + Namespace + "\t" + Name + "\tany").c_str());
	}
}

// Fallback for a script that can't be loaded
static void BakeScriptText(const char* Namespace, const char* Path, IWriter& Writer)
{
	const xr_vector<xr_string> Lines = ReadScriptLines(Path);
	if (Namespace[0] != 0)
	{
		Writer.w_string((xr_string("N	") + Namespace).c_str());
	}

	for (const xr_string& Line : Lines)
	{
		xr_string Class;
		xr_string Base;
		if (ParseClassLine(Line, Class, Base))
		{
			// ns.base: classes are globals
			const size_t Dot = Base.rfind('.');
			if (Dot != xr_string::npos)
			{
				Base.erase(0, Dot + 1);
			}
			Writer.w_string(("C	" + Class + "	" + Base).c_str());
			continue;
		}

		if (!Line.starts_with("function") || Line.size() <= 8 || !isspace((u8)Line[8]))
		{
			continue;
		}

		size_t Pos = 8;
		while (Pos < Line.size() && isspace((u8)Line[Pos]))
		{
			++Pos;
		}
		const xr_string First = ReadIdentifier(Line, Pos);
		xr_string Second;
		char Separator = 0;
		if (Pos < Line.size() && (Line[Pos] == '.' || Line[Pos] == ':'))
		{
			Separator = Line[Pos++];
			Second = ReadIdentifier(Line, Pos);
		}

		const size_t ArgsOpen = Line.find('(', Pos);
		const size_t ArgsClose = ArgsOpen != xr_string::npos ? Line.find(')', ArgsOpen) : xr_string::npos;
		if (First.empty() || ArgsClose == xr_string::npos || (Separator != 0 && Second.empty()))
		{
			continue;
		}
		const xr_string Args = TrimText(Line.substr(ArgsOpen + 1, ArgsClose - ArgsOpen - 1).c_str());

		if (Separator == 0)
		{
			Writer.w_string((xr_string("F\t") + Namespace + "\t" + First + "\t" + Args).c_str());
		}
		else if (Separator == ':')
		{
			Writer.w_string(("M\t" + First + "\t" + Second + "\t" + Args).c_str());
		}
		else
		{
			Writer.w_string(("F\t" + First + "\t" + Second + "\t" + Args).c_str());
		}
	}
}

bool CScriptEngine::BakeApi(const char* FilePath)
{
	using namespace luabind::detail;
	lua_State* L = lua();

	// Loading runs the top level of the scripts, like the game does
	xr_vector<std::pair<xr_string, xr_string>> Failed;
	const script_list_type Scripts = xray_scripts;

	// luabind derives from a nil base without a check: the bases load first
	xr_hash_map<xr_string, xr_vector<xr_string>> ScriptBases;
	xr_hash_map<xr_string, xr_hash_set<xr_string>> ScriptClasses;
	xr_hash_map<xr_string, xr_string> ClassOwners;
	for (const auto& [Name, Path] : Scripts)
	{
		for (const xr_string& Line : ReadScriptLines(Path.c_str()))
		{
			xr_string Class;
			xr_string Base;
			if (ParseClassLine(Line, Class, Base))
			{
				ScriptClasses[Name].insert(Class);
				ClassOwners.emplace(Class, Name);
				if (!Base.empty())
				{
					ScriptBases[Name].push_back(Base);
				}
			}
		}
	}

	auto IsClassValue = [L](const xr_string& Expression)
	{
		const int Top = lua_gettop(L);
		bool Result = false;
		const xr_string Code = "return " + Expression;
		if (luaL_loadbuffer(L, Code.c_str(), Code.size(), "=lua_api") == 0 && lua_pcall(L, 0, 1, 0) == 0 && lua_type(L, -1) == LUA_TUSERDATA)
		{
			const void* Value = lua_touserdata(L, -1);
			class_registry::get_registry(L)->iterate_classes(L, [&](lua_State*, class_rep* Class)
			{
				Result |= Class == Value;
			});
		}
		lua_settop(L, Top);
		return Result;
	};

	xr_hash_map<xr_string, int> States;
	std::function<bool(const xr_string&)> LoadScript = [&](const xr_string& Name) -> bool
	{
		int& State = States[Name];
		if (State != 0)
		{
			// A cycle is left to the scripts themselves
			return State != 3;
		}
		State = 1;

		bool Ok = true;
		for (const xr_string& Base : ScriptBases[Name])
		{
			if (ScriptClasses[Name].contains(Base))
			{
				continue;
			}

			const size_t Dot = Base.find('.');
			auto Owner = ClassOwners.find(Base);
			const xr_string Dependency = Dot != xr_string::npos ? xr_string(Base.substr(0, Dot).c_str()) : Owner != ClassOwners.end() ? Owner->second : xr_string();
			if (!Dependency.empty() && Dependency != Name && Scripts.contains(Dependency) && !LoadScript(Dependency))
			{
				Ok = false;
				break;
			}

			if (!IsClassValue(Base))
			{
				Msg("! [Lua API] %s.script: the base class %s is not available", Name.c_str(), Base.c_str());
				Ok = false;
				break;
			}
		}

		const xr_string Namespace = Name == "_g" ? xr_string("_G") : Name;
		if (Ok && !namespace_loaded(Namespace.c_str()))
		{
			Ok = load_file_into_namespace(Scripts.at(Name).c_str(), Namespace.c_str());
		}

		States[Name] = Ok ? 2 : 3;
		if (!Ok)
		{
			Failed.emplace_back(Name, Scripts.at(Name));
		}
		return Ok;
	};

	m_quiet_errors = true;
	for (const auto& [Name, Path] : Scripts)
	{
		// The help is a list of the exports, not a script
		if (Name != "lua_help")
		{
			LoadScript(Name);
		}
	}
	m_quiet_errors = false;

	IWriter* Writer = FS.w_open(FilePath);
	if (Writer == nullptr)
	{
		return false;
	}

	for (const auto& [Name, Path] : Failed)
	{
		Msg("! [Lua API] %s.script failed to load, its functions are read from the text", Name.c_str());
		BakeScriptText(Name == "_g" ? "" : Name.c_str(), Path.c_str(), *Writer);
	}

	// Fields assigned nil on the top level are not in the tables: db.actor
	for (const auto& [Name, Path] : Scripts)
	{
		if (Name != "lua_help")
		{
			BakeScriptFields(Name == "_g" ? "" : Name.c_str(), ReadScriptLines(Path.c_str(), false), *Writer);
		}
	}

	xr_hash_map<xr_string, xr_string> Classes;
	class_registry* Registry = class_registry::get_registry(L);
	Registry->iterate_classes(L, [&](lua_State*, class_rep* Class)
	{
		if (Class->type() != nullptr)
		{
			Classes[NormalizeTypeName(Class->type()->name())] = Class->name();
		}
	});

	Writer->w_string("# IX-Ray Lua API: C class, M method, P property, K constant, N namespace, F function");
	Registry->iterate_classes(L, [&](lua_State*, class_rep* Class)
	{
		if (Class->get_class_type() != class_rep::cpp_class)
		{
			// class "name" (base) of the scripts: its methods are script functions in its table
			xr_string Bases;
			for (const class_rep::base_info& Base : Class->bases())
			{
				Bases += Bases.empty() ? "" : ",";
				Bases += Base.base->name();
			}
			Writer->w_string((xr_string("C\t") + Class->name() + "\t" + Bases).c_str());

			Class->get_table(L);
			if (lua_istable(L, -1))
			{
				BakeScriptMethods(L, Class->name(), *Writer);
			}
			lua_pop(L, 1);
			return;
		}

		xr_string Bases;
		xr_hash_set<xr_string> Self = { Class->name() };
		for (const class_rep::base_info& Base : Class->bases())
		{
			Bases += Bases.empty() ? "" : ",";
			Bases += Base.base->name();
			Self.insert(Base.base->name());
		}
		Writer->w_string((xr_string("C\t") + Class->name() + "\t" + Bases).c_str());

		for (const auto& [Name, Getter] : Class->m_getters)
		{
			Writer->w_string((xr_string("P\t") + Class->name() + "\t" + Name).c_str());
		}

		for (const auto& [Name, Value] : Class->static_constants())
		{
			Writer->w_string((xr_string("K\t") + Class->name() + "\t" + Name).c_str());
		}

		for (const method_rep& Method : Class->m_methods)
		{
			for (const auto& Overload : Method.overloads())
			{
				const bool HasSelf = !Overload.m_params_.empty() && Self.contains(GetLuaTypeName(Overload.m_params_[0], Classes));
				Writer->w_string((xr_string("M\t") + Class->name() + "\t" + Method.name + "\t" + JoinParams(Overload.m_params_, HasSelf ? 1 : 0, Classes)).c_str());
			}
		}
	});

	lua_pushvalue(L, LUA_GLOBALSINDEX);
	BakeFunctions(L, "", Classes, *Writer);

	const int Globals = lua_gettop(L);
	lua_pushnil(L);
	while (lua_next(L, Globals) != 0)
	{
		if (lua_type(L, -2) == LUA_TSTRING && lua_type(L, -1) == LUA_TTABLE)
		{
			const xr_string Namespace = lua_tostring(L, -2);
			if (Namespace != "_G" && Namespace != "package" && Namespace != "_LOADED" && !Namespace.starts_with("__"))
			{
				Writer->w_string(("N\t" + Namespace).c_str());
				BakeFunctions(L, Namespace.c_str(), Classes, *Writer);
			}
		}
		lua_pop(L, 1);
	}
	lua_pop(L, 1);

	FS.w_close(Writer);
	return true;
}

XREFLECT(LuaExport)
bool IsTracyConnected()
{
#ifdef IXRAY_PROFILER_TRACY
	return tracy::GetProfiler().IsConnected();
#endif
	return false;
}

void XReflectRegister(lua_State* L);

void CScriptEngine::script_register(lua_State* L)
{
	XReflectRegister(L);
}

#include "script_engine_script.reflect.inl"
