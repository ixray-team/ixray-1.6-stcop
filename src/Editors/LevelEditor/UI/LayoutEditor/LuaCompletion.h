#pragma once
#include "plugins/TextEditor.h"
#include "../../Editor/XVisor/XVisor.h"

// Lua code completion of a text view: the API comes from EContext.Visor
class CLuaCompletion
{
public:
	using FieldResolver = CXVisor::FieldResolver;

	void BeforeRender(TextEditor& View);
	void AfterRender(TextEditor& View, const xr_string& Script, const FieldResolver& Fields);

private:
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

	void CollectMembers(const xr_string& Class, xr_vector<SItem>& Out, xr_hash_set<xr_string>& Seen, int Depth = 0) const;
	void Accept(TextEditor& View);

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
