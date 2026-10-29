#pragma once
#include "NodesMode.h"

class CContentView;

enum class ENodesMode : u8
{
	Logic,
	Dialogs,
	Macros,

	Count
};

// The Nodes tab: the node editors share the docks, the Content Browser and the top bar
class UINodesEditorForm :
	public IEditorWnd
{
public:
	UINodesEditorForm();
	virtual ~UINodesEditorForm();

	virtual void Draw() override;
	virtual void Serialize(nlohmann::json& Data, bool IsSaving) override;

	static void Create(int Tab);
	static void Destroy();

	static bool IsActive();
	static bool IsActive(ENodesMode Mode);
	static bool IsModified();
	// Shows the tab with the mode
	static void Activate(ENodesMode Mode);
	static int GetTab();
	static CContentView* GetBrowser();
	static const char* GetBrowserWindow();

	static bool Save();
	static bool Reload();
	static bool Undo(bool Redo);
	static void RevealInBrowser();
	static void DrawTopBar(float ButtonSize);
	// The mode switch
	static void DrawModeSwitch(float ButtonSize);

private:
	INodesMode* Current() const;
	void BuildLayout(INodesMode& Mode);

	static UINodesEditorForm* Form;

	xr_unique_ptr<INodesMode> Modes[(size_t)ENodesMode::Count];
	ENodesMode Mode = ENodesMode::Logic;
	CContentView* Browser = nullptr;

	// The docks made by the first layout, kept for the modes opened later
	ImGuiID Docks[(size_t)ENodesDock::Count] = {};
	bool LayoutChecked[(size_t)ENodesMode::Count] = {};
};
