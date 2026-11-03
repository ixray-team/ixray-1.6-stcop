#pragma once
#include "plugins/TextEditor.h"
#include "../../Editor/XVisor/XVisor.h"

// ltx completion of a text view: the schema and the sections of configs come from EContext.Visor.
// The class of a section comes from its "class" key, through its parents in every configs file
class CLtxCompletion
{
public:
	// What the text is: a whole ltx file, or a list of condlist tokens without keys
	enum class EText
	{
		Ltx,
		Conditions,
		Effects
	};
	void SetText(EText Value) { TextKind = Value; }
	bool IsOpen() const { return Open; }

	// Text is the whole file: its sections override the indexed ones of the same file
	void BeforeRender(TextEditor& View);
	void AfterRender(TextEditor& View, const xr_string& Path, const xr_string& Text);
	// Marks the sections without their required keys
	void UpdateMarkers(TextEditor& View, const xr_string& Path, const xr_string& Text);
	bool FindDefinition(const xr_string& Name, xr_string& Path, int& Line);

private:
	struct SItem
	{
		xr_string Name;
		xr_string Detail;
		bool IsKey = true;
	};

	void Accept(TextEditor& View);

	// The sections of the edited file
	CXVisor::LtxSectionMap Local;
	xr_vector<xr_string> LocalLines;
	xr_string LocalPath;

	EText TextKind = EText::Ltx;

	bool Open = false;
	bool ForceOpen = false;
	xr_vector<SItem> Items;
	int Current = 0;
	xr_string Word;
	int Line = -1;
	bool AppendAssign = false;

	int Move = 0;
	bool AcceptRequest = false;
	bool CloseRequest = false;
	bool KeysTaken = false;
	// The text view that had the focus: a click on the list gives it back
	struct ImGuiWindow* EditorWindow = nullptr;
};
