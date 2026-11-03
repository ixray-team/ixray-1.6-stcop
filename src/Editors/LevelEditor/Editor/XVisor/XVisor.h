#pragma once
#include "plugins/TextEditor.h"

// The script and config analyzer of the editors: one instance, EContext.Visor.
// Lua: the API baked into $app_data_root$\lua_api.txt and the functions of the game scripts.
// ltx: the schema of the XREFLECT(IniRead) classes, baked into $app_data_root$\ltx_schema.txt, and the sections of configs.
// The Coder, the Layout, Logic and Dialog editors read it, their completion lists keep only the state of a view
class CXVisor
{
public:
	using FieldResolver = xr_delegate<xr_string(const xr_string&)>;

	struct SLuaFunction
	{
		xr_vector<xr_string> Overloads;
	};

	struct SLuaClass
	{
		xr_vector<xr_string> Bases;
		xr_map<xr_string, SLuaFunction> Methods;
		xr_vector<xr_string> Properties;
		xr_vector<xr_string> Constants;
	};

	using LuaFunctionMap = xr_map<xr_string, SLuaFunction>;

	struct SLtxSection
	{
		xr_vector<xr_string> Parents;
		xr_hash_map<xr_string, xr_string> Keys;
		int Line = 0;
		xr_string Path;
		bool Override = false;
	};

	using LtxSectionMap = xr_hash_map<xr_string, SLtxSection>;

	// A key of the schemas of a section, the prefix of the nested schemas included
	struct SLtxKey
	{
		xr_string Key;
		xr_string Type;
		bool Required = false;
		xr_string If;
		xr_string Schema;
	};

	static xr_string GetApiPath();
	static xr_string GetSchemaPath();

	// Loads every script namespace into the game state: call it while the game thread is idle
	bool Bake(xr_string& Error);
	// Syntax check of a script in a state of its own, nothing runs. Line is 0 when the error has none
	static bool Compile(const char* ChunkName, const xr_string& Source, xr_string& Error, int& Line);

	// Reads the API and the schema after a bake, cheap when nothing changed
	bool Load();
	bool IsApiLoaded() const { return !Classes.empty(); }
	bool IsSchemaLoaded() const { return !Schemas.empty(); }
	// Changes with every load: the languages built from the data are rebuilt
	u32 GetGeneration() const { return LoadedGeneration; }

	// Lua
	// Lua with the API names: classes and namespaces as known identifiers, calls as functions, hover declarations
	void BuildLuaLanguage(TextEditor::LanguageDefinition& Out) const;
	u32 GetClassCount() const { return (u32)Classes.size(); }
	const xr_hash_map<xr_string, SLuaClass>& GetClasses() const { return Classes; }
	// "" holds the globals
	const xr_hash_map<xr_string, LuaFunctionMap>& GetNamespaces() const { return Namespaces; }
	// Namespace -> field -> Lua type, "any" when only the text knows it
	const xr_hash_map<xr_string, xr_map<xr_string, xr_string>>& GetNamespaceFields() const { return NamespaceFields; }

	// InitStatic -> CUIStatic
	xr_string GetInitClass(const xr_string& Function) const;
	const SLuaFunction* FindMethod(const xr_string& Class, const xr_string& Name, int Depth = 0) const;
	// Class of an expression before '.' or ':', empty when unknown
	xr_string ResolveType(const xr_string& Expression, const xr_string& Script, const FieldResolver& Fields) const;
	// Overloads of the function called at the end of Text, nullptr outside of a call
	const SLuaFunction* FindCalledFunction(const xr_string& Text, const xr_string& Script, const FieldResolver& Fields, xr_string& Name) const;
	// The functions with their parameters and the locals of a script text
	static void CollectScriptNames(const xr_string& Script, xr_hash_map<xr_string, xr_string>& Functions, xr_hash_set<xr_string>& Variables);

	// The functions of $game_scripts$\<Namespace>.script and of the baked namespace, sorted
	const xr_vector<xr_string>& GetScriptFunctions(const xr_string& Namespace);
	// The names of the $game_scripts$\*.script files, sorted
	const xr_vector<xr_string>& GetScriptNamespaces();
	// The functions of xr_conditions.script and xr_effects.script for the condlists
	const xr_vector<xr_string>& GetConditions() { return GetScriptFunctions("xr_conditions"); }
	const xr_vector<xr_string>& GetEffects() { return GetScriptFunctions("xr_effects"); }
	// Hints for a "namespace.function" field: the namespaces, then the functions of the typed one
	void GetFunctionHints(const xr_string& Before, xr_vector<xr_string>& Out);
	// A script was saved: its functions are read again
	void InvalidateScripts();

	// ltx
	// Ini with the hover declarations of the schema keys
	void BuildIniLanguage(TextEditor::LanguageDefinition& Out) const;
	// Counts of the schema and the index, shown in the Coder status
	xr_string GetLtxStatus() const;
	const xr_hash_map<xr_string, xr_vector<xr_string>>& GetClassIds() const { return ClassIds; }

	// Lines, when set, gets the section of every line
	static void ParseLtx(const xr_string& Text, LtxSectionMap& Out, xr_vector<xr_string>* Lines = nullptr);
	// Local is the parsed edited file: its sections override the indexed ones of configs
	const SLtxSection* FindSection(const xr_string& Name, const LtxSectionMap* Local = nullptr);
	xr_string GetClassId(const xr_string& Section, const LtxSectionMap* Local = nullptr, int Depth = 0);
	bool HasKey(const xr_string& Section, const xr_string& Key, const LtxSectionMap* Local = nullptr, int Depth = 0);
	xr_vector<SLtxKey> GetKeys(const xr_string& Section, const LtxSectionMap* Local = nullptr);
	// The section in configs
	bool FindDefinition(const xr_string& Name, xr_string& Path, int& Line);

private:
	void LoadApi();
	void LoadSchema();
	xr_string ResolveCall(const xr_string& Callee, const xr_string& Script, const FieldResolver& Fields) const;
	void ExpandSchema(const xr_string& Name, const xr_string& Prefix, xr_vector<SLtxKey>& Out, int Depth = 0) const;
	void IndexConfigs();

	struct SLtxField
	{
		xr_string Key;
		xr_string Type;
		bool Required = false;
		xr_string If;
		bool Nested = false;
	};

	struct SLtxSchema
	{
		xr_vector<xr_string> Bases;
		xr_vector<SLtxField> Fields;
	};

	// A bake changes it, the data is read again on the next Load
	u32 BakeGeneration = 1;
	u32 LoadedGeneration = 0;

	xr_hash_map<xr_string, SLuaClass> Classes;
	xr_hash_map<xr_string, LuaFunctionMap> Namespaces;
	xr_hash_map<xr_string, xr_map<xr_string, xr_string>> NamespaceFields;

	xr_hash_map<xr_string, xr_vector<xr_string>> ScriptFunctions;
	xr_vector<xr_string> ScriptNamespaces;
	bool ScriptNamespacesListed = false;

	xr_hash_map<xr_string, SLtxSchema> Schemas;
	// Class id of a section -> its schemas
	xr_hash_map<xr_string, xr_vector<xr_string>> ClassIds;
	// Every section of configs
	bool Indexed = false;
	LtxSectionMap Sections;
};
