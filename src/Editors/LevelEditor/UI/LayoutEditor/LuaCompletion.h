#pragma once
#include "plugins/TextEditor.h"

// Lua code completion, the API is baked into $app_data_root$\lua_api.txt
class CLuaCompletion
{
public:
	using FieldResolver = xr_delegate<xr_string(const xr_string&)>;

	// Loads every script namespace into the game state: call it while the game thread is idle
	bool Bake(xr_string& Error);
	static xr_string GetApiPath();

	// Syntax check of a script in a state of its own, nothing runs. Line is 0 when the error has none
	static bool Compile(const char* ChunkName, const xr_string& Source, xr_string& Error, int& Line);
	bool Load();
	bool IsLoaded() const { return Loaded; }
	// Changes with every load of the database: the languages built from it are rebuilt
	u32 GetGeneration() const { return LoadedGeneration; }
	static u32 GetBakeGeneration() { return BakeGeneration; }
	// Lua with the API names: classes and namespaces as known identifiers, calls as functions, hover declarations
	void BuildLanguage(TextEditor::LanguageDefinition& Out) const;
	u32 GetClassCount() const { return (u32)Classes.size(); }

	void BeforeRender(TextEditor& View);
	void AfterRender(TextEditor& View, const xr_string& Script, const FieldResolver& Fields);

	// InitStatic -> CUIStatic
	xr_string GetInitClass(const xr_string& Function) const;

private:
	struct SFunction
	{
		xr_vector<xr_string> Overloads;
	};

	struct SClass
	{
		xr_vector<xr_string> Bases;
		xr_map<xr_string, SFunction> Methods;
		xr_vector<xr_string> Properties;
		xr_vector<xr_string> Constants;
	};

	enum class EItemKind : u8
	{
		Method,
		Property,
		Constant,
		Function,
		Field,
		Class,
		Namespace
	};

	enum class EFilter : u8
	{
		Members,
		Functions,
		Types,
		Namespaces,
		Count
	};

	struct SItem
	{
		xr_string Name;
		xr_string Detail;
		EItemKind Kind = EItemKind::Field;
	};

	static EFilter GetFilter(EItemKind Kind);
	static bool IsCallable(EItemKind Kind);
	int DrawItem(const SItem& Item, bool Selected) const;
	bool DrawFilters(const u32 (&Counts)[(int)EFilter::Count]);
	float GetFiltersWidth(const u32 (&Counts)[(int)EFilter::Count]) const;

	void LoadFile(const xr_string& Path);

	void CollectMembers(const xr_string& Class, xr_vector<SItem>& Out, xr_hash_set<xr_string>& Seen, int Depth = 0) const;
	const SFunction* FindMethod(const xr_string& Class, const xr_string& Name, int Depth = 0) const;
	// Class of an expression before '.' or ':', empty when unknown
	xr_string ResolveType(const xr_string& Expression, const xr_string& Script, const FieldResolver& Fields) const;
	xr_string ResolveCall(const xr_string& Callee, const xr_string& Script, const FieldResolver& Fields) const;
	// Overloads of the function called at the cursor, nullptr outside of a call
	const SFunction* FindCalledFunction(const xr_string& Line, const xr_string& Script, const FieldResolver& Fields, xr_string& Name) const;
	void Accept(TextEditor& View);

	bool Loaded = false;
	// A bake in one completion reloads the others
	static u32 BakeGeneration;
	u32 LoadedGeneration = 0;
	xr_hash_map<xr_string, SClass> Classes;
	// "" holds the globals
	xr_hash_map<xr_string, xr_map<xr_string, SFunction>> Namespaces;
	// Namespace -> field -> Lua type, "any" when only the text knows it
	xr_hash_map<xr_string, xr_map<xr_string, xr_string>> NamespaceFields;

	bool Open = false;
	bool ForceOpen = false;
	xr_vector<SItem> Items;
	int Current = 0;
	// The typed part of the name, replaced on accept
	xr_string Word;
	int Line = -1;

	// Filters toggled by Alt+M, Alt+F, Alt+T, Alt+N while the list is open
	bool Filters[(int)EFilter::Count] = { true, true, true, true };
	u32 FilterCounts[(int)EFilter::Count] = {};

	struct ImGuiWindow* EditorWindow = nullptr;

	int Move = 0;
	bool AcceptRequest = false;
	bool CloseRequest = false;
	bool KeysTaken = false;
};
